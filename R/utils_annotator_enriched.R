# =============================================================================
# utils_annotator_enriched.R
#
# Internal helpers for annotator_enriched_server(). Not exported.
#
# Responsibilities:
#   - Produce and merge the enrichments data frame (state helpers).
#   - Build reactable colDef objects for each supported enrichment type
#     (select, text, date, number).
#   - Dispatch a spec to its builder via make_enriched_col_def().
#
# Gating design:
#   Each enrichment input is wrapped in <div class="enrichment-gate">.
#   The correct open/closed inline style is chosen at render time on the
#   server using gate_initial_style(): rows whose ID appears in
#   selected_ids_snap receive .gate_style_open; all others receive
#   .gate_style_closed. This bakes the correct initial state directly into
#   the server-rendered HTML, so no JS timing dependency is involved for
#   the initial paint.
#
#   After the initial render, live selection changes (user clicks, reset
#   button, reset_to) are handled by the enrichmentGateSync custom message
#   handler registered in annotator_enriched_ui(), which toggles
#   gate.style.opacity and gate.style.pointerEvents via data-row-id lookup.
#
# Relationship to utils_annotator.R:
#   This file is intentionally separate. The annotator_table helpers carry
#   concepts (is_touched, gate_is_open, text_checkbox, radio) that do not
#   apply here. Mixing them would add noise and coupling. make_options() from
#   utils_flexible.R is reused since select rendering is identical.
# =============================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x


# -----------------------------------------------------------------------
# Enrichment data frame helpers
# -----------------------------------------------------------------------

#' Return the untouched default value for one enrichment col_spec
#'
#' @param spec `list`. A single enrichment spec. `type` must be one of
#'   `"select"`, `"text"`, `"date"`, or `"number"`.
#'
#' @return A scalar of the appropriate R type and NA value.
#'
#' @noRd
default_enrichment_value <- function(spec) {
  switch(
    spec$type,
    select = NA_character_,
    text = NA_character_,
    date = NA_character_,
    number = NA_real_,
    stop(sprintf("Unknown enrichment type: '%s'", spec$type))
  )
}


#' Build a blank enrichments data frame for the given source data
#'
#' Creates one row per row in `source_data`, keyed by `row_id` (stored as
#' character), with one column per enrichment spec initialised to its
#' untouched default value.
#'
#' Storing the ID as character is essential for consistent set operations
#' (union, intersect, setdiff) against the character vector maintained by
#' `selected_ids`.
#'
#' @param source_data `data.frame`. The current source data frame.
#' @param row_id `character(1)`. Name of the ID column.
#' @param enrich_specs `list`. Enrichment column specifications.
#'
#' @return A `data.frame` with columns: `row_id` column (character), then
#'   one column per enrichment spec.
#'
#' @importFrom purrr map map_chr
#'
#' @noRd
initial_enrichments_blank <- function(source_data, row_id, enrich_specs) {
  id_values <- as.character(source_data[[row_id]])

  if (length(id_values) == 0) {
    col_names <- c(row_id, purrr::map_chr(enrich_specs, "name"))
    empty <- as.data.frame(
      matrix(nrow = 0, ncol = length(col_names)),
      stringsAsFactors = FALSE
    )
    names(empty) <- col_names
    return(empty)
  }

  default_cols <- purrr::map(enrich_specs, default_enrichment_value)
  names(default_cols) <- purrr::map_chr(enrich_specs, "name")

  id_col <- list(id_values)
  names(id_col) <- row_id

  as.data.frame(c(id_col, default_cols), stringsAsFactors = FALSE)
}


#' Merge preserved enrichments with an updated source data frame
#'
#' When `source_data` changes reactively, this function produces a new
#' enrichments frame that:
#'   - preserves values for rows that remain in `source_data`,
#'   - assigns untouched defaults to rows that are new,
#'   - silently retains rows that left `source_data` inside `existing`
#'     (they are not in the result but survive in the reactiveVal for
#'     restoration if those rows reappear).
#'
#' @param source_data `data.frame`. The updated source data frame.
#' @param row_id `character(1)`. Name of the ID column.
#' @param enrich_specs `list`. Enrichment column specifications.
#' @param existing `data.frame`. The current enrichments data frame, as
#'   returned by a previous call to [initial_enrichments_blank()] or
#'   [merge_enrichments()].
#'
#' @return A `data.frame` with the same structure as [initial_enrichments_blank()],
#'   covering exactly the rows currently in `source_data`.
#'
#' @importFrom dplyr left_join
#' @importFrom purrr walk
#'
#' @noRd
merge_enrichments <- function(source_data, row_id, enrich_specs, existing) {
  blank <- initial_enrichments_blank(source_data, row_id, enrich_specs)
  merged <- dplyr::left_join(
    blank[, row_id, drop = FALSE],
    existing,
    by = row_id
  )

  # Replace NAs introduced by the left join with per-type defaults.
  purrr::walk(enrich_specs, function(spec) {
    col <- spec$name
    missing_rows <- is.na(merged[[col]])
    if (any(missing_rows)) {
      merged[[col]][missing_rows] <<- default_enrichment_value(spec)
    }
  })

  merged
}


# -----------------------------------------------------------------------
# Gate style helpers
#
# The correct style is chosen at server render time so no JS timing is
# needed for the initial paint of pre-populated tables. Live changes after
# the initial render are handled by the enrichmentGateSync message handler.
# -----------------------------------------------------------------------

# Inline style constants for gate divs.
.gate_style_open <- "opacity: 1; pointer-events: auto; transition: opacity 0.12s ease;"
.gate_style_closed <- "opacity: 0.35; pointer-events: none; transition: opacity 0.12s ease;"

#' Choose the correct initial gate style for one row
#'
#' Returns the open style when `id_value` is present in `selected_ids_snap`,
#' and the closed style otherwise. Called once per cell at server render time
#' so that pre-selected rows have their fields immediately active without
#' depending on JS executing after the DOM is ready.
#'
#' @param id_value The row's unique identifier value (will be coerced to
#'   character for comparison).
#' @param selected_ids_snap `character`. Snapshot of currently selected row
#'   IDs, captured via `isolate(selected_ids())` in `renderReactable`.
#'
#' @return `character(1)`. A CSS inline style string.
#'
#' @noRd
gate_initial_style <- function(id_value, selected_ids_snap) {
  if (as.character(id_value) %in% selected_ids_snap) {
    .gate_style_open
  } else {
    .gate_style_closed
  }
}


# -----------------------------------------------------------------------
# Cell rendering
#
# WHY THESE BUILDERS RETURN A STRING AND SET html = TRUE.
#
# A colDef `cell` function that returns an htmltools tag hands reactable a tag
# TREE, and reactable does not render it — it walks the tree at serialisation
# time and converts every node into a React element, parsing each style
# attribute as it goes. That cost is paid once per node, per cell, per render.
#
# A catalogue table makes that expensive fast. The developmental milestones
# section is 157 rows x 3 select enrichments, each select carrying up to 13
# <option> tags: ~10,000 nodes, which measured at 3.9 seconds of R and a 540 KB
# widget payload every time the table re-rendered — and it re-renders on every
# filter change, not just on open.
#
# Rendering to HTML here instead and declaring html = TRUE skips the conversion
# entirely; reactable passes the string straight through. Same markup, one
# order of magnitude less work.
#
# The second half is options_renderer(). Every row in a select column offers the
# identical option list and differs only in which entry is `selected`, so the
# <option> block is rendered once per distinct current value and reused. That is
# what takes the milestones table from 3.9s to 0.6s; html = TRUE alone would
# still re-render 157 identical option lists.
# -----------------------------------------------------------------------

#' Wrap one enrichment control in its gate and render it to HTML
#'
#' The gate is the div that greys out and disables an enrichment control while
#' its row is unselected. Its initial style is baked in server-side (see
#' [gate_initial_style()]); `data-row-id` is what the enrichmentGateSync handler
#' looks the gate up by afterwards.
#'
#' @param id_value The row's unique identifier value.
#' @param selected_ids_snap `character`. Frozen snapshot of selected row IDs.
#' @param control A tag (or pre-rendered HTML) for the input itself.
#'
#' @return `character(1)`. HTML, for a colDef with `html = TRUE`.
#'
#' @noRd
gated_cell_html <- function(id_value, selected_ids_snap, control) {
  as.character(shiny::tags$div(
    class = "enrichment-gate",
    style = gate_initial_style(id_value, selected_ids_snap),
    `data-row-id` = as.character(id_value),
    onclick = "event.stopPropagation();",
    control
  ))
}


#' Build a memoised renderer for one select column's <option> list
#'
#' Returns a function of the row's current value that yields the rendered
#' `<option>` block, computing each distinct value at most once. A column with
#' four choices renders at most five distinct blocks no matter how many rows it
#' has.
#'
#' @param choices Named character vector, as in a select spec's `choices`.
#'
#' @return `function(current)` returning [htmltools::HTML()].
#'
#' @noRd
options_renderer <- function(choices) {
  cache <- new.env(parent = emptyenv())

  function(current) {
    # "v." prefix: the placeholder's value is "", and an empty string is a
    # zero-length variable name, which cannot key an environment.
    key <- paste0("v.", current)
    hit <- cache[[key]]
    if (!is.null(hit)) {
      return(hit)
    }
    rendered <- htmltools::HTML(as.character(
      htmltools::tagList(make_options(choices, current))
    ))
    cache[[key]] <- rendered
    rendered
  }
}


# -----------------------------------------------------------------------
# Column definition builders
#
# All builders follow the same pattern:
#   - Accept (spec, enrich_snap, row_id, ns, selected_ids_snap).
#   - enrich_snap is a frozen data frame snapshot captured via
#     isolate(enrichments()) at renderReactable time. Closing over a snapshot
#     (rather than the live reactive) means selection changes do not
#     invalidate renderReactable, keeping the table stable between renders.
#   - selected_ids_snap is a frozen character vector captured via
#     isolate(selected_ids()) at renderReactable time. Used by
#     gate_initial_style() to set the correct open/closed style per row
#     directly in the server-rendered HTML.
#   - The cell function wraps its input in <div class="enrichment-gate">
#     using gate_initial_style() so selected rows start open and unselected
#     rows start closed — no JS timing dependency for the initial paint.
#   - Input IDs follow the pattern ns(paste0(spec$name, "_", id_value)),
#     matching the sync observer in annotator_enriched_server().
# -----------------------------------------------------------------------

#' Build a reactable colDef for a select enrichment column
#'
#' @param spec `list`. A single enrichment spec with `type = "select"`.
#'   Requires `choices`: a named character vector whose names are display
#'   labels and whose values are stored values. Include a placeholder entry
#'   like `c("Select..." = "")` as the first element if you want a visual
#'   "nothing chosen" state. Never use `c("" = "")` — a blank key produces
#'   a zero-length variable name.
#' @param enrich_snap `data.frame`. Frozen enrichments snapshot.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. Module namespace function.
#' @param selected_ids_snap `character`. Frozen snapshot of selected row IDs.
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_select_col_def <- function(
  spec,
  enrich_snap,
  row_id,
  ns,
  selected_ids_snap
) {
  render_options <- options_renderer(spec$choices)

  reactable::colDef(
    name = spec$label %||% spec$name,
    width = spec$width,
    html = TRUE,
    cell = function(value, index) {
      id_value <- enrich_snap[[row_id]][index]
      input_id <- ns(paste0(spec$name, "_", id_value))
      current <- enrich_snap[[spec$name]][index]

      is_blank <- is.na(current) || !nzchar(current)
      current_for_options <- if (is.na(current)) "" else current

      gated_cell_html(
        id_value,
        selected_ids_snap,
        shiny::tags$select(
          id = input_id,
          class = "form-control form-control-sm",
          style = if (is_blank) "color: #6c757d;" else "",
          onchange = sprintf(
            "this.style.color = this.value === '' ? '#6c757d' : '';
             Shiny.setInputValue('%s', this.value, {priority: 'event'});",
            input_id
          ),
          render_options(current_for_options)
        )
      )
    }
  )
}


#' Build a reactable colDef for a text enrichment column
#'
#' @param spec `list`. A single enrichment spec with `type = "text"`.
#'   Accepts an optional `placeholder` field.
#' @param enrich_snap `data.frame`. Frozen enrichments snapshot.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. Module namespace function.
#' @param selected_ids_snap `character`. Frozen snapshot of selected row IDs.
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_text_col_def <- function(
  spec,
  enrich_snap,
  row_id,
  ns,
  selected_ids_snap
) {
  reactable::colDef(
    name = spec$label %||% spec$name,
    width = spec$width,
    html = TRUE,
    cell = function(value, index) {
      id_value <- enrich_snap[[row_id]][index]
      input_id <- ns(paste0(spec$name, "_", id_value))
      current <- enrich_snap[[spec$name]][index]

      gated_cell_html(
        id_value,
        selected_ids_snap,
        shiny::tags$input(
          type = "text",
          id = input_id,
          class = "form-control form-control-sm",
          value = if (!is.na(current)) current else NULL,
          placeholder = spec$placeholder %||% "",
          onchange = sprintf(
            "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
            input_id
          )
        )
      )
    }
  )
}


#' Build a reactable colDef for a date enrichment column
#'
#' Values are stored as `"YYYY-MM-DD"` character strings, which is what
#' `as.Date()` expects. Optional `min` and `max` fields constrain the
#' browser's native date picker.
#'
#' @param spec `list`. A single enrichment spec with `type = "date"`.
#'   Accepts optional `min` and `max` in `"YYYY-MM-DD"` format.
#' @param enrich_snap `data.frame`. Frozen enrichments snapshot.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. Module namespace function.
#' @param selected_ids_snap `character`. Frozen snapshot of selected row IDs.
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_date_col_def <- function(
  spec,
  enrich_snap,
  row_id,
  ns,
  selected_ids_snap
) {
  reactable::colDef(
    name = spec$label %||% spec$name,
    width = spec$width,
    html = TRUE,
    cell = function(value, index) {
      id_value <- enrich_snap[[row_id]][index]
      input_id <- ns(paste0(spec$name, "_", id_value))
      current <- enrich_snap[[spec$name]][index]

      gated_cell_html(
        id_value,
        selected_ids_snap,
        shiny::tags$input(
          type = "date",
          id = input_id,
          class = "form-control form-control-sm",
          value = if (!is.na(current) && nzchar(current)) current else NULL,
          min = spec$min %||% NULL,
          max = spec$max %||% NULL,
          onchange = sprintf(
            "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
            input_id
          )
        )
      )
    }
  )
}


#' Build a reactable colDef for a number enrichment column
#'
#' @param spec `list`. A single enrichment spec with `type = "number"`.
#'   Accepts optional `min` and `max` numeric bounds.
#' @param enrich_snap `data.frame`. Frozen enrichments snapshot.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. Module namespace function.
#' @param selected_ids_snap `character`. Frozen snapshot of selected row IDs.
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_number_col_def <- function(
  spec,
  enrich_snap,
  row_id,
  ns,
  selected_ids_snap
) {
  reactable::colDef(
    name = spec$label %||% spec$name,
    width = spec$width,
    html = TRUE,
    cell = function(value, index) {
      id_value <- enrich_snap[[row_id]][index]
      input_id <- ns(paste0(spec$name, "_", id_value))
      current <- enrich_snap[[spec$name]][index]

      gated_cell_html(
        id_value,
        selected_ids_snap,
        shiny::tags$input(
          type = "number",
          id = input_id,
          class = "form-control form-control-sm",
          value = if (!is.na(current)) current else NULL,
          min = spec$min %||% NULL,
          max = spec$max %||% NULL,
          onchange = sprintf(
            "Shiny.setInputValue('%s', parseFloat(this.value), {priority: 'event'})",
            input_id
          )
        )
      )
    }
  )
}


#' Dispatch an enrichment spec to its colDef builder
#'
#' Routes a single enrichment column specification to the correct
#' `make_enriched_*_col_def()` function based on its `type` field.
#'
#' Supported types: `"select"`, `"text"`, `"date"`, `"number"`.
#'
#' @param spec `list`. A single enrichment column specification.
#' @param enrich_snap `data.frame`. Frozen enrichments snapshot from
#'   `isolate(enrichments())` captured at `renderReactable` time.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. Module namespace function (`session$ns`).
#' @param selected_ids_snap `character`. Frozen snapshot of selected row IDs
#'   from `isolate(selected_ids())` captured at `renderReactable` time.
#'   Passed through to each builder so the correct initial gate style is baked
#'   into the server-rendered HTML.
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_col_def <- function(
  spec,
  enrich_snap,
  row_id,
  ns,
  selected_ids_snap
) {
  switch(
    spec$type,
    select = make_enriched_select_col_def(
      spec,
      enrich_snap,
      row_id,
      ns,
      selected_ids_snap
    ),
    text = make_enriched_text_col_def(
      spec,
      enrich_snap,
      row_id,
      ns,
      selected_ids_snap
    ),
    date = make_enriched_date_col_def(
      spec,
      enrich_snap,
      row_id,
      ns,
      selected_ids_snap
    ),
    number = make_enriched_number_col_def(
      spec,
      enrich_snap,
      row_id,
      ns,
      selected_ids_snap
    ),
    stop(sprintf("Unknown enrichment type: '%s'", spec$type))
  )
}
