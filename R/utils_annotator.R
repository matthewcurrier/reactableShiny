# =============================================================================
# Internal helpers for the annotator_table module.
# None of these are exported — they are implementation details of
# annotator_table_server() and its rendering pipeline.
#
# NOTE: `cell` cannot be passed via `col_def_options` on input columns —
# the cell renderer is authoritative and owned by the module. Passing `cell`
# in `col_def_options` will cause do.call() to error with a duplicate
# argument, which is intentional: it surfaces the mistake immediately rather
# than silently discarding the custom renderer.
#
# EXCEPTION: `cell` MAY be passed via `col_def_options` on plain `display`
# columns only, since make_display_col_def does not set a cell renderer.
# This is the supported escape hatch for custom display rendering (e.g.
# adding icons). It is not available on `clickable_display` columns, which
# own their cell renderer like all input types.
# =============================================================================

# -----------------------------------------------------------------------
# Annotation data frame helpers
# -----------------------------------------------------------------------

#' Return the untouched default value for one input col_spec
#'
#' Used to initialise the annotations data frame and to detect whether
#' a cell has been touched by the user.
#'
#' @param spec `list`. A single input column spec (type must not be "display").
#'
#' @return A scalar of the appropriate type and default value:
#'   `NA_character_` for select/text/radio, `FALSE` for checkbox/text_checkbox,
#'   `NA_real_` for number.
#'
#' @noRd
default_annotation_value <- function(spec) {
  switch(
    spec$type,
    select = NA_character_,
    text = NA_character_,
    radio = NA_character_,
    checkbox = FALSE,
    text_checkbox = FALSE,
    number = NA_real_,
    stop(sprintf("Unknown input column type: '%s'", spec$type))
  )
}


#' Build a blank annotations data frame for the given source data
#'
#' Creates one row per row in `source_data`, keyed by `row_id`, with one
#' column per input col_spec initialised to its untouched default value.
#' Display col_specs are ignored — only input columns produce annotation
#' columns.
#'
#' @param source_data `data.frame`. The current source data frame.
#' @param row_id `character(1)`. Name of the ID column in `source_data`.
#' @param input_specs `list`. The subset of col_specs where type != "display".
#'
#' @return A `data.frame` with columns: `row_id` column, then one column per
#'   input spec.
#'
#' @importFrom purrr map map_chr
#'
#' @noRd
initial_annotations <- function(source_data, row_id, input_specs) {
  id_values <- source_data[[row_id]]
  default_cols <- map(input_specs, default_annotation_value)
  names(default_cols) <- map_chr(input_specs, "name")

  # Name the ID column using the actual value of row_id (e.g. "car"),
  # not the literal string "row_id" — otherwise joins and subsetting break
  id_col <- list(id_values)
  names(id_col) <- row_id

  # as.data.frame recycles scalar defaults to match id length, but cannot
  # recycle to length 0 — handle the empty source_data case explicitly.
  # Using a zero-row matrix avoids the NULL-column issue where
  # as.data.frame(list(col = NULL)) silently drops columns.
  if (length(id_values) == 0) {
    col_names <- c(row_id, map_chr(input_specs, "name"))
    empty <- as.data.frame(
      matrix(nrow = 0, ncol = length(col_names)),
      stringsAsFactors = FALSE
    )
    names(empty) <- col_names
    return(empty)
  }

  as.data.frame(
    c(id_col, default_cols),
    stringsAsFactors = FALSE
  )
}


#' Merge preserved annotations with a new source data frame
#'
#' When source_data changes reactively, existing annotations are preserved
#' and joined back to the new data. Rows in the new source data that have
#' no prior annotation receive blank defaults. Rows that were previously
#' annotated but are no longer in the new source data are silently retained
#' in the annotations reactive so they are not lost if those rows reappear.
#'
#' @param source_data `data.frame`. The updated source data frame.
#' @param row_id `character(1)`. Name of the ID column in `source_data`.
#' @param input_specs `list`. The subset of col_specs where type != "display".
#' @param existing `data.frame`. The current annotations data frame.
#'
#' @return A `data.frame` in the same structure as [initial_annotations()],
#'   covering all rows in `source_data`, with preserved values where available.
#'
#' @importFrom dplyr left_join
#'
#' @noRd
merge_annotations <- function(source_data, row_id, input_specs, existing) {
  blank <- initial_annotations(source_data, row_id, input_specs)

  # Left join blank -> existing so that rows with prior annotations get their
  # values restored. Rows with no prior annotation keep the blank defaults.
  # The direction of the join ensures the result always has exactly the rows
  # in the current source_data.
  # [, row_id, drop = FALSE] selects the column — without the comma R would
  # attempt to select a row named row_id, returning 0 rows
  merged <- left_join(blank[, row_id, drop = FALSE], existing, by = row_id)

  # left_join produces NA for any column that had no matching row in existing.
  # Replace those NAs with the appropriate untouched default for each type.
  for (spec in input_specs) {
    col <- spec$name
    missing_rows <- is.na(merged[[col]])
    if (any(missing_rows)) {
      merged[[col]][missing_rows] <- default_annotation_value(spec)
    }
  }

  merged
}


# -----------------------------------------------------------------------
# "Touched" detection
# -----------------------------------------------------------------------

#' Determine whether a single annotation value has been touched by the user
#'
#' The definition of "touched" varies by input type:
#' \itemize{
#'   \item **select / text / radio** — any non-NA, non-empty string.
#'   \item **checkbox / text_checkbox** — only `TRUE` (unchecked = untouched).
#'   \item **number** — any non-NA, non-zero value.
#' }
#'
#' @param value The annotation value to test.
#' @param type `character(1)`. The input column type.
#'
#' @return `logical(1)`.
#'
#' @noRd
is_touched <- function(value, type) {
  switch(
    type,
    select = !is.null(value) && !is.na(value) && nzchar(value),
    text = !is.null(value) && !is.na(value) && nzchar(value),
    radio = !is.null(value) && !is.na(value) && nzchar(value),
    checkbox = isTRUE(value),
    text_checkbox = isTRUE(value),
    number = !is.null(value) && !is.na(value) && value != 0,
    stop(sprintf("Unknown input column type: '%s'", type))
  )
}


#' Return TRUE for each row where at least one input column is touched
#'
#' @param annotations `data.frame`. The current annotations data frame
#'   (including the row_id column).
#' @param input_specs `list`. The subset of col_specs where type != "display".
#'
#' @return `logical` vector of length `nrow(annotations)`.
#'
#' @importFrom purrr map_lgl
#'
#' @noRd
any_touched <- function(annotations, input_specs) {
  # Iterate by row index rather than apply() — apply() coerces the data frame
  # to a character matrix, which breaks isTRUE() on checkboxes and numeric
  # comparisons on number columns.
  map_lgl(seq_len(nrow(annotations)), function(i) {
    any(map_lgl(input_specs, function(spec) {
      is_touched(annotations[[spec$name]][i], spec$type)
    }))
  })
}


# -----------------------------------------------------------------------
# Gating helpers
#
# Row gating allows a checkbox to control whether other inputs in the same
# row are interactive. A gated input declares which checkbox controls it via
# a `gates` field in its col_spec. When the gate is closed (checkbox FALSE),
# the input is greyed out and non-interactive. Values are preserved — they
# reappear when the gate reopens.
#
# Gating is implemented entirely client-side in JavaScript so that toggling
# the checkbox feels instant with no Shiny round-trip.
# -----------------------------------------------------------------------

#' Return TRUE if this cell's gate checkbox is open (or if there is no gate)
#'
#' @param spec `list`. A single col_spec, optionally containing a `gates`
#'   field naming the checkbox annotation column that controls this input.
#' @param ann `data.frame`. The current annotations snapshot.
#' @param index `numeric(1)`. Row index within `ann`.
#'
#' @return `logical(1)`.
#'
#' @noRd
gate_is_open <- function(spec, ann, index) {
  if (is.null(spec$gates)) {
    return(TRUE)
  }
  gate_value <- ann[[spec$gates]][index]
  if (!is.null(spec$gates_values)) {
    return(
      !is.null(gate_value) &&
        !is.na(gate_value) &&
        gate_value %in% spec$gates_values
    )
  }
  isTRUE(gate_value) # existing checkbox behaviour unchanged
}


#' Return the namespaced checkbox input ID that gates this input, or NULL
#'
#' Used to populate the `data-gated-by` HTML attribute. The JavaScript gating
#' handler uses `querySelectorAll('[data-gated-by="<id>"]')` to find all
#' inputs controlled by a given checkbox and toggle their state.
#'
#' @param spec `list`. A single col_spec, optionally containing a `gates`
#'   field.
#' @param ns `function`. The module namespace function.
#' @param id_value The row's unique identifier value.
#'
#' @return `character(1)` or `NULL`.
#'
#' @noRd
gated_by_id <- function(spec, ns, id_value) {
  if (is.null(spec$gates)) {
    return(NULL)
  }
  ns(paste0(spec$gates, "_", id_value))
}


#' Build the inline CSS for a gated element based on gate state
#'
#' Returns a CSS string that greys out and blocks interaction when the gate
#' is closed, or NULL when the gate is open.
#'
#' @param is_open `logical(1)`. Whether the gate is currently open.
#'
#' @return `character(1)` or `NULL`.
#'
#' @noRd
gated_style <- function(is_open) {
  if (is_open) NULL else "opacity: 0.4; pointer-events: none;"
}


#' Build the JavaScript onchange handler for a gating checkbox
#'
#' When the checkbox changes, this handler reports the new value to Shiny
#' and then finds all elements in the DOM with `data-gated-by` pointing to
#' this checkbox, toggling their opacity and pointer-events accordingly.
#' Values in gated inputs are preserved — only their interactivity changes.
#'
#' @param input_id `character(1)`. The namespaced Shiny input ID of the
#'   checkbox that acts as the gate.
#'
#' @return `character(1)`. A JavaScript expression for use as an `onchange`
#'   attribute value.
#'
#' @noRd
gating_checkbox_onchange <- function(input_id) {
  sprintf(
    "var checked = this.checked;
     Shiny.setInputValue('%s', checked, {priority: 'event'});
     document.querySelectorAll('[data-gated-by=\"%s\"]').forEach(function(el) {
       el.style.opacity       = checked ? '' : '0.4';
       el.style.pointerEvents = checked ? '' : 'none';
     });",
    input_id,
    input_id
  )
}


# -----------------------------------------------------------------------
# Column definition builders
#
# Every builder follows the same pattern:
#
#   do.call(
#     colDef,
#     c(
#       list(
#         name  = ...,   # always from spec
#         width = ...,   # always from spec
#         cell  = ...    # input builders only; owned by the module
#       ),
#       spec$col_def_options %||% list()
#     )
#   )
#
# Fixed fields (name, width, cell) go in the inner list(). Any additional
# reactable::colDef() arguments — minWidth, maxWidth, align, headerClass,
# etc. — are passed by the caller via spec$col_def_options and merged in
# after. Because do.call() errors on duplicate arguments, passing `cell`
# inside col_def_options will fail loudly rather than silently discarding
# the custom renderer.
#
# EXCEPTION: `cell` may be passed via col_def_options for plain `display`
# columns only, since make_display_col_def does not set a cell renderer.
# -----------------------------------------------------------------------

#' Build a reactable colDef for a display column
#'
#' Display columns are read-only — they show values from the source data
#' frame with no interactive cell renderer.
#'
#' Because no `cell` is set here, callers may pass one via `col_def_options`
#' as an escape hatch for custom display rendering (e.g. icon badges). This
#' is the only column type where passing `cell` in `col_def_options` is
#' supported.
#'
#' @param spec `list`. A single col_spec with `type = "display"`. Accepts an
#'   optional `col_def_options` list of additional [reactable::colDef()]
#'   arguments (e.g. `minWidth`, `maxWidth`, `align`, `cell`).
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#'
#' @noRd
make_display_col_def <- function(spec) {
  do.call(
    colDef,
    c(
      list(
        name = spec$label %||% spec$name,
        width = spec$width
      ),
      spec$col_def_options %||% list()
    )
  )
}


#' Build a reactable colDef for a clickable display column
#'
#' Like `make_display_col_def`, but wraps the cell value in a clickable
#' `<div>` that toggles a named checkbox column in the same row when
#' clicked. This gives users a larger click target than the checkbox alone.
#'
#' The col_spec must include a `toggles` field naming the checkbox column
#' to toggle (e.g. `toggles = "met"`). The pattern mirrors the `gates`
#' field used by input columns: `gates` controls interactivity; `toggles`
#' controls the checked state on click.
#'
#' The `onclick` handler:
#' \enumerate{
#'   \item Finds the target checkbox by its namespaced DOM ID.
#'   \item Inverts its checked state.
#'   \item Reports the new value to Shiny via `Shiny.setInputValue()` with
#'     `priority: 'event'` so Shiny always processes the update even if
#'     the value did not change numerically.
#' }
#'
#' Note: if the target checkbox drives gated inputs (i.e. it has a `gates`
#' field on other columns), clicking the display cell will toggle the
#' checkbox but will NOT trigger the gating CSS update — only the
#' checkbox's own `onchange` handler does that. If gating + clickable
#' display are combined, extend the `onclick` JS here to also run the
#' gating logic.
#'
#' @param spec `list`. A single col_spec with `type = "clickable_display"`.
#'   Must include `toggles`: the `name` of the checkbox col_spec to toggle.
#'   Accepts an optional `col_def_options` list of additional
#'   [reactable::colDef()] arguments. `cell` may not be included in
#'   `col_def_options`.
#' @param annotations `reactive`. The annotations reactiveVal from the
#'   parent module server. Used to look up the row ID at render time.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
# Around line 310 in utils_annotator.R
make_clickable_display_col_def <- function(spec, annotations, row_id, ns) {
  do.call(
    colDef,
    c(
      list(
        name = spec$label %||% spec$name,
        width = spec$width,
        html = TRUE, # <-- add this
        cell = function(value, index) {
          ann <- annotations()
          id_value <- ann[[row_id]][index]
          check_id <- ns(paste0(spec$toggles, "_", id_value))

          content <- if (!is.null(spec$render)) spec$render(value) else value

          as.character(
            # <-- wrap in as.character()
            tags$div(
              content,
              style = "cursor: pointer;",
              onclick = sprintf(
                "var cb = document.getElementById('%s');
                 cb.checked = !cb.checked;
                 Shiny.setInputValue('%s', cb.checked, {priority: 'event'});",
                check_id,
                check_id
              )
            )
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}


#' Build a reactable colDef for a text_checkbox column
#'
#' Renders a source data column's text as a clickable `<label>` linked to a
#' checkbox. Clicking anywhere on the text or the checkbox toggles the checked
#' state. The annotation value stored is `TRUE`/`FALSE` like a regular
#' checkbox. Because the label and checkbox are linked via HTML, no JavaScript
#' toggle logic is needed — the browser handles it natively.
#'
#' `text_checkbox` also acts as a gate for other columns in the same row
#' when those columns declare `gates = spec$name`.
#'
#' @param spec `list`. A single col_spec with `type = "text_checkbox"`. Must
#'   include a `display_col` field naming the source data column whose text
#'   to show as the clickable label. Accepts an optional `col_def_options`
#'   list of additional [reactable::colDef()] arguments. `cell` may not be
#'   included in `col_def_options`.
#' @param annotations `reactive`. The annotations reactiveVal from the parent
#'   module server.
#' @param source_data `reactive`. The source data reactive, used to look up
#'   the display text by row ID.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
make_text_checkbox_col_def <- function(
  spec,
  annotations,
  source_data,
  row_id,
  ns
) {
  do.call(
    colDef,
    c(
      list(
        name = spec$label %||% spec$name,
        width = spec$width,
        cell = function(value, index) {
          ann <- annotations()
          id_value <- ann[[row_id]][index]
          input_id <- ns(paste0(spec$name, "_", id_value))
          is_checked <- isTRUE(ann[[spec$name]][index])

          # Look up display text from source data by matching row ID
          src <- source_data()
          display_text <- src[[spec$display_col]][src[[row_id]] == id_value]

          # text_checkbox can itself be gated by another checkbox
          open <- gate_is_open(spec, ann, index)
          gb_id <- gated_by_id(spec, ns, id_value)

          # Wrap checkbox inside a label — clicking the text toggles the checkbox
          # natively without extra JavaScript. The label is the full clickable area.
          tags$label(
            `data-gated-by` = gb_id,
            style = paste0(
              "cursor: pointer; margin: 0; display: flex; align-items: center; gap: 6px;",
              if (!open) " opacity: 0.4; pointer-events: none;" else ""
            ),
            tags$input(
              type = "checkbox",
              id = input_id,
              checked = if (is_checked) "checked" else NULL,
              onchange = gating_checkbox_onchange(input_id)
            ),
            display_text
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}


#' Build a reactable colDef for a checkbox input column
#'
#' When other columns in the same row declare `gates = spec$name`, this
#' checkbox controls their interactive state. The `onchange` handler both
#' reports to Shiny and drives the gating logic in the browser.
#'
#' @param spec `list`. A single col_spec with `type = "checkbox"`. Optionally
#'   includes a `gates` field if this checkbox is itself gated by another.
#'   Accepts an optional `col_def_options` list of additional
#'   [reactable::colDef()] arguments. `cell` may not be included in
#'   `col_def_options`.
#' @param annotations `reactive`. The annotations reactiveVal from the parent
#'   module server.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
make_checkbox_col_def <- function(spec, annotations, row_id, ns) {
  do.call(
    colDef,
    c(
      list(
        name = spec$label %||% spec$name,
        width = spec$width,
        cell = function(value, index) {
          ann <- annotations()
          id_value <- ann[[row_id]][index]
          input_id <- ns(paste0(spec$name, "_", id_value))
          is_checked <- isTRUE(ann[[spec$name]][index])
          open <- gate_is_open(spec, ann, index)
          gb_id <- gated_by_id(spec, ns, id_value)

          tags$input(
            type = "checkbox",
            id = input_id,
            checked = if (is_checked) "checked" else NULL,
            style = gated_style(open),
            `data-gated-by` = gb_id,
            # onchange reports to Shiny and drives any downstream gated inputs
            onchange = gating_checkbox_onchange(input_id)
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}


#' Build a reactable colDef for a select dropdown input column
#'
#' Supports optional row gating via `spec$gates`.
#'
#' @param spec `list`. A single col_spec with `type = "select"`. Requires
#'   `choices`. Optionally includes `gates`. Accepts an optional
#'   `col_def_options` list of additional [reactable::colDef()] arguments.
#'   `cell` may not be included in `col_def_options`.
#' @param annotations `reactive`. The annotations reactiveVal from the parent
#'   module server.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
make_select_col_def <- function(spec, annotations, row_id, ns) {
  do.call(
    colDef,
    c(
      list(
        name = spec$label %||% spec$name,
        width = spec$width,
        cell = function(value, index) {
          ann <- annotations()
          id_value <- ann[[row_id]][index]
          input_id <- ns(paste0(spec$name, "_", id_value))
          current <- ann[[spec$name]][index]
          open <- gate_is_open(spec, ann, index)
          gb_id <- gated_by_id(spec, ns, id_value)

          # Treat NA as untouched — pass "" to make_options so no option is
          # pre-selected, and apply the muted placeholder colour
          is_blank <- is.na(current) || !nzchar(current)
          current_for_options <- if (is.na(current)) "" else current
          placeholder_style <- if (is_blank) "color: #6c757d;" else ""
          gate_style <- if (!open) "opacity: 0.4; pointer-events: none;" else ""

          tags$select(
            id = input_id,
            class = "form-control",
            style = paste0(placeholder_style, gate_style),
            `data-gated-by` = gb_id,
            `data-gates-values` = if (!is.null(spec$gates_values)) {
              # <-- new
              paste(spec$gates_values, collapse = ",")
            } else {
              NULL
            },
            onchange = sprintf(
              "var v = this.value;
     this.style.color = v === '' ? '#6c757d' : '';
     Shiny.setInputValue('%s', v, {priority: 'event'});
     var myId = '%s';
     document.querySelectorAll('[data-gated-by=\"' + myId + '\"]').forEach(function(el) {
       var raw = el.dataset.gatesValues;
       var open = raw ? raw.split(',').indexOf(v) >= 0 : v !== '';
       el.style.opacity       = open ? '' : '0.4';
       el.style.pointerEvents = open ? '' : 'none';
     });",
              input_id,
              input_id
            ),
            make_options(spec$choices, current_for_options)
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}


#' Build a reactable colDef for a text input column
#'
#' Supports optional row gating via `spec$gates`.
#'
#' @param spec `list`. A single col_spec with `type = "text"`. Optionally
#'   includes `placeholder` and `gates`. Accepts an optional `col_def_options`
#'   list of additional [reactable::colDef()] arguments. `cell` may not be
#'   included in `col_def_options`.
#' @param annotations `reactive`. The annotations reactiveVal from the parent
#'   module server.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
make_text_col_def <- function(spec, annotations, row_id, ns) {
  do.call(
    colDef,
    c(
      list(
        name = spec$label %||% spec$name,
        width = spec$width,
        cell = function(value, index) {
          ann <- annotations()
          id_value <- ann[[row_id]][index]
          input_id <- ns(paste0(spec$name, "_", id_value))
          current <- ann[[spec$name]][index]
          open <- gate_is_open(spec, ann, index)
          gb_id <- gated_by_id(spec, ns, id_value)

          tags$input(
            type = "text",
            id = input_id,
            class = "form-control",
            # Pass NULL rather than NA — HTML renders NA as the literal string "NA"
            value = if (!is.na(current)) current else NULL,
            placeholder = spec$placeholder %||% "",
            style = gated_style(open),
            `data-gated-by` = gb_id,
            onchange = sprintf(
              "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
              input_id
            )
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}


#' Build a reactable colDef for a number input column
#'
#' Supports optional row gating via `spec$gates`.
#'
#' @param spec `list`. A single col_spec with `type = "number"`. Optionally
#'   includes `min`, `max`, and `gates`. Accepts an optional `col_def_options`
#'   list of additional [reactable::colDef()] arguments. `cell` may not be
#'   included in `col_def_options`.
#' @param annotations `reactive`. The annotations reactiveVal from the parent
#'   module server.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
make_number_col_def <- function(spec, annotations, row_id, ns) {
  do.call(
    colDef,
    c(
      list(
        name = spec$label %||% spec$name,
        width = spec$width,
        cell = function(value, index) {
          ann <- annotations()
          id_value <- ann[[row_id]][index]
          input_id <- ns(paste0(spec$name, "_", id_value))
          current <- ann[[spec$name]][index]
          open <- gate_is_open(spec, ann, index)
          gb_id <- gated_by_id(spec, ns, id_value)

          tags$input(
            type = "number",
            id = input_id,
            class = "form-control",
            value = if (!is.na(current)) current else NULL,
            min = spec$min %||% NULL,
            max = spec$max %||% NULL,
            style = gated_style(open),
            `data-gated-by` = gb_id,
            onchange = sprintf(
              "Shiny.setInputValue('%s', parseFloat(this.value), {priority: 'event'})",
              input_id
            )
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}


#' Build a reactable colDef for a radio button input column
#'
#' Radio buttons are rendered as a vertical group within the cell. The entire
#' group is gated as a unit via its wrapping `<div>`.
#'
#' @param spec `list`. A single col_spec with `type = "radio"`. Requires
#'   `choices`: a named character vector. Optionally includes `gates`. Accepts
#'   an optional `col_def_options` list of additional [reactable::colDef()]
#'   arguments. `cell` may not be included in `col_def_options`.
#' @param annotations `reactive`. The annotations reactiveVal from the parent
#'   module server.
#' @param row_id `character(1)`. Name of the ID column.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#' @importFrom purrr map
#'
#' @noRd
make_radio_col_def <- function(spec, annotations, row_id, ns) {
  do.call(
    colDef,
    c(
      list(
        name = spec$label %||% spec$name,
        width = spec$width,
        cell = function(value, index) {
          ann <- annotations()
          id_value <- ann[[row_id]][index]
          group_id <- ns(paste0(spec$name, "_", id_value))
          current <- ann[[spec$name]][index]
          open <- gate_is_open(spec, ann, index)
          gb_id <- gated_by_id(spec, ns, id_value)

          radio_buttons <- map(names(spec$choices), function(label) {
            val <- spec$choices[[label]]
            btn_id <- paste0(group_id, "_", val)
            tags$label(
              style = "display: block; font-weight: normal;",
              tags$input(
                type = "radio",
                id = btn_id,
                name = group_id,
                value = val,
                checked = if (identical(current, val)) "checked" else NULL,
                onchange = sprintf(
                  "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
                  group_id
                )
              ),
              paste0(" ", label)
            )
          })

          # Gate the entire group as a unit via the wrapping div — simpler than
          # disabling each individual radio input
          tags$div(
            style = gated_style(open),
            `data-gated-by` = gb_id,
            radio_buttons
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}


#' Dispatch a col_spec to its appropriate colDef builder
#'
#' Routes each column specification to the correct `make_*_col_def()`
#' function based on its `type` field.
#'
#' Supported types:
#' \describe{
#'   \item{`display`}{Read-only cell sourced from source_data. `cell` may
#'     be passed via `col_def_options` as a custom renderer.}
#'   \item{`clickable_display`}{Like `display`, but the cell is wrapped in
#'     a clickable `<div>` that toggles the checkbox named in `toggles`.}
#'   \item{`select`}{Dropdown input.}
#'   \item{`checkbox`}{Checkbox input. May act as a gate for other columns.}
#'   \item{`text`}{Free-text input.}
#'   \item{`number`}{Numeric input.}
#'   \item{`radio`}{Radio button group.}
#'   \item{`text_checkbox`}{Combined clickable-text + checkbox, sourced from
#'     a display column named in `display_col`.}
#' }
#'
#' @param spec `list`. A single column specification.
#' @param annotations `reactive`. The annotations reactiveVal. Ignored for
#'   plain display columns.
#' @param row_id `character(1)`. Name of the ID column. Ignored for plain
#'   display columns.
#' @param ns `function`. The module namespace function. Ignored for plain
#'   display columns.
#' @param source_data `reactive` or `NULL`. Required only for
#'   `type = "text_checkbox"`, which needs it to look up the display text.
#'   Safely ignored for all other types.
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_input_col_def <- function(
  spec,
  annotations,
  row_id,
  ns,
  source_data = NULL
) {
  switch(
    spec$type,
    display = make_display_col_def(spec),
    clickable_display = make_clickable_display_col_def(
      spec,
      annotations,
      row_id,
      ns
    ),
    select = make_select_col_def(spec, annotations, row_id, ns),
    checkbox = make_checkbox_col_def(spec, annotations, row_id, ns),
    text = make_text_col_def(spec, annotations, row_id, ns),
    number = make_number_col_def(spec, annotations, row_id, ns),
    radio = make_radio_col_def(spec, annotations, row_id, ns),
    text_checkbox = make_text_checkbox_col_def(
      spec,
      annotations,
      source_data,
      row_id,
      ns
    ),
    stop(sprintf("Unknown column type: '%s'", spec$type))
  )
}
