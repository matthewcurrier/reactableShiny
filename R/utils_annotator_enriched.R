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
# Gating design note:
#   Visual gating (greyed-out inputs on unselected rows) is handled entirely
#   in custom.css via the selector:
#
#       .rt-tr[aria-selected="true"] .enrichment-gate { ... }
#
#   reactable sets aria-selected="true" on selected row <div role="row">
#   elements. Because the gating is CSS-driven, cell HTML is identical for
#   selected and unselected rows — React's reconciliation will not re-render
#   inputs on selection change, preserving any values the user has typed.
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
  # This handles rows that were in source_data before but not in existing,
  # as well as any type mismatches that could arise from the join.
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
# Column definition builders
#
# All builders follow the same pattern:
#   - Accept (spec, enrich_snap, row_id, ns).
#   - enrich_snap is a frozen data frame snapshot captured via
#     isolate(enrichments()) at renderReactable time. Closing over a snapshot
#     (rather than the live reactive) means selection changes do not
#     invalidate renderReactable, keeping the table stable between renders.
#   - The cell function wraps its input in <div class="enrichment-gate">
#     with .gate_style_closed applied as an INLINE style. The default
#     greyed/non-interactive state is therefore baked into the rendered HTML
#     at the server side — no CSS file, no singleton, no timing issues.
#     The MutationObserver in annotator_enriched_ui() then toggles
#     gate.style.opacity and gate.style.pointerEvents when the row's
#     checkbox re-renders after a click.
#   - Input IDs follow the pattern ns(paste0(spec$name, "_", id_value)),
#     matching the sync observer in annotator_enriched_server().
# -----------------------------------------------------------------------

# Inline style applied to every gate div at render time. Sets the default
# closed (non-interactive) state without relying on any external stylesheet.
# The JS MutationObserver in annotator_enriched_ui() removes these constraints
# when a row is selected and restores them when it is deselected.
.gate_style_closed <- "opacity: 0.35; pointer-events: none; transition: opacity 0.12s ease;"

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
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_select_col_def <- function(spec, enrich_snap, row_id, ns) {
  reactable::colDef(
    name = spec$label %||% spec$name,
    width = spec$width,
    cell = function(value, index) {
      id_value <- enrich_snap[[row_id]][index]
      input_id <- ns(paste0(spec$name, "_", id_value))
      current <- enrich_snap[[spec$name]][index]

      is_blank <- is.na(current) || !nzchar(current)
      current_for_options <- if (is.na(current)) "" else current

      shiny::tags$div(
        class = "enrichment-gate",
        style = .gate_style_closed,
        `data-row-id` = as.character(id_value),
        onclick = "event.stopPropagation();",
        shiny::tags$select(
          id = input_id,
          class = "form-control form-control-sm",
          style = if (is_blank) "color: #6c757d;" else "",
          onchange = sprintf(
            "this.style.color = this.value === '' ? '#6c757d' : '';
             Shiny.setInputValue('%s', this.value, {priority: 'event'});",
            input_id
          ),
          make_options(spec$choices, current_for_options)
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
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_text_col_def <- function(spec, enrich_snap, row_id, ns) {
  reactable::colDef(
    name = spec$label %||% spec$name,
    width = spec$width,
    cell = function(value, index) {
      id_value <- enrich_snap[[row_id]][index]
      input_id <- ns(paste0(spec$name, "_", id_value))
      current <- enrich_snap[[spec$name]][index]

      shiny::tags$div(
        class = "enrichment-gate",
        style = .gate_style_closed,
        `data-row-id` = as.character(id_value),
        onclick = "event.stopPropagation();",
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
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_date_col_def <- function(spec, enrich_snap, row_id, ns) {
  reactable::colDef(
    name = spec$label %||% spec$name,
    width = spec$width,
    cell = function(value, index) {
      id_value <- enrich_snap[[row_id]][index]
      input_id <- ns(paste0(spec$name, "_", id_value))
      current <- enrich_snap[[spec$name]][index]

      shiny::tags$div(
        class = "enrichment-gate",
        style = .gate_style_closed,
        `data-row-id` = as.character(id_value),
        onclick = "event.stopPropagation();",
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
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_number_col_def <- function(spec, enrich_snap, row_id, ns) {
  reactable::colDef(
    name = spec$label %||% spec$name,
    width = spec$width,
    cell = function(value, index) {
      id_value <- enrich_snap[[row_id]][index]
      input_id <- ns(paste0(spec$name, "_", id_value))
      current <- enrich_snap[[spec$name]][index]

      shiny::tags$div(
        class = "enrichment-gate",
        style = .gate_style_closed,
        `data-row-id` = as.character(id_value),
        onclick = "event.stopPropagation();",
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
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_enriched_col_def <- function(spec, enrich_snap, row_id, ns) {
  switch(
    spec$type,
    select = make_enriched_select_col_def(spec, enrich_snap, row_id, ns),
    text = make_enriched_text_col_def(spec, enrich_snap, row_id, ns),
    date = make_enriched_date_col_def(spec, enrich_snap, row_id, ns),
    number = make_enriched_number_col_def(spec, enrich_snap, row_id, ns),
    stop(sprintf("Unknown enrichment type: '%s'", spec$type))
  )
}
