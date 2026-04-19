# =============================================================================
# Internal helpers for the flexible_table module.
# None of these are exported — they are implementation details of
# flexible_table_server() and its rendering pipeline.
# =============================================================================

#' Build a single blank row data frame
#'
#' Creates a one-row data frame with internal `id` and `delete` helper columns
#' alongside one column per entry in `col_specs`, pre-filled with each
#' column's default value (or `""` if none is defined).
#'
#' @param row_id `numeric(1)`. A unique integer identifier for this row. Used
#'   to construct Shiny input IDs that are stable for the lifetime of the row.
#' @param col_specs `list`. The column specification list passed to
#'   [flexible_table_server()]. See that function for the full field reference.
#'
#' @return A one-row `data.frame` with columns `id`, one column per
#'   `col_specs` entry, and `delete`.
#'
#' @seealso [load_data()], [flexible_table_server()]
#'
#' @importFrom purrr map map_chr
#'
#' @noRd
make_row <- function(row_id, col_specs) {
  default_value <- function(spec) {
    if (is.null(spec$default)) "" else spec$default
  }

  values <- map(col_specs, default_value)
  names(values) <- map_chr(col_specs, "name")

  as.data.frame(
    c(list(id = row_id), values, list(delete = "")),
    stringsAsFactors = FALSE
  )
}


#' Convert a data frame of existing values into table rows
#'
#' Maps each row of `df` to a new internal table row created by [make_row()],
#' then overwrites the blank default values with the actual values from `df`.
#' Fresh IDs are assigned starting from `start_id` so that stale Shiny input
#' names from previously rendered rows are never reused.
#'
#' This is a pure function: it does not update `next_id` or any other reactive.
#' The caller is responsible for advancing `next_id` after calling this.
#'
#' @param df `data.frame`. Pre-existing data to load. Column names must match
#'   the `name` fields in `col_specs`. All values are coerced to `character`.
#' @param start_id `numeric(1)`. The first row ID to assign. Typically the
#'   current value of the `next_id` reactive.
#' @param col_specs `list`. The column specification list passed to
#'   [flexible_table_server()].
#'
#' @return A `data.frame` with the same structure as [make_row()], with
#'   `nrow(df)` rows and IDs running from `start_id` to
#'   `start_id + nrow(df) - 1`.
#'
#' @seealso [make_row()]
#'
#' @importFrom purrr map_dfr
#'
#' @noRd
load_data <- function(df, start_id, col_specs) {
  map_dfr(seq_len(nrow(df)), function(i) {
    row <- make_row(start_id + i - 1, col_specs)
    for (col in names(df)) {
      row[[col]] <- as.character(df[[col]][i])
    }
    row
  })
}


#' Build a single `<option>` tag
#'
#' @param value `character(1)`. The option's `value` attribute.
#' @param label `character(1)`. The visible text shown in the dropdown.
#' @param current_value `character(1)`. The currently selected value in this
#'   cell. Used to set the `selected` attribute on the matching option.
#'
#' @return An [shiny::tags]`$option` HTML tag object.
#'
#' @seealso [make_optgroup()], [make_options()]
#'
#' @importFrom shiny tags
#'
#' @noRd
make_option <- function(value, label, current_value) {
  is_selected <- if (current_value == value) "selected" else NULL
  style <- if (!nzchar(value)) "color: #6c757d;" else NULL
  tags$option(value = value, selected = is_selected, style = style, label)
}


#' Build a single `<optgroup>` tag with its `<option>` children
#'
#' @param group_name `character(1)`. The label attribute of the `<optgroup>`.
#' @param group_vals Named `character` vector. Names become option labels;
#'   values become option values.
#' @param current_value `character(1)`. The currently selected value in this
#'   cell, forwarded to each [make_option()] call.
#'
#' @return An [shiny::tags]`$optgroup` HTML tag object containing one
#'   `<option>` per element of `group_vals`.
#'
#' @seealso [make_option()], [make_options()]
#'
#' @importFrom purrr map2
#' @importFrom shiny tags
#'
#' @noRd
make_optgroup <- function(group_name, group_vals, current_value) {
  options <- map2(
    as.character(group_vals),
    names(group_vals),
    make_option,
    current_value
  )
  tags$optgroup(label = group_name, options)
}


#' Build `<option>` or `<optgroup>` tags from a choices vector or list
#'
#' Handles both flat and grouped choice formats:
#' \itemize{
#'   \item **Flat vector** — produces a list of `<option>` tags.
#'   \item **Named list** — produces `<optgroup>` tags for groups with more
#'     than one item, and plain `<option>` tags for single-item groups (the
#'     common pattern for "Select…" placeholder entries).
#' }
#'
#' @param choices Named `character` vector or named `list` of named `character`
#'   vectors. The same format accepted by the `choices` field in `col_specs`.
#' @param current_value `character(1)`. The currently selected value in this
#'   cell. Forwarded to [make_option()] and [make_optgroup()] to mark the
#'   correct option as selected.
#'
#' @return A `list` of HTML tag objects suitable for passing as children of a
#'   `<select>` element.
#'
#' @seealso [make_option()], [make_optgroup()]
#'
#' @importFrom purrr map map2
#'
#' @noRd
make_options <- function(choices, current_value) {
  is_grouped <- is.list(choices) && !is.null(names(choices))

  if (is_grouped) {
    map(names(choices), function(group) {
      group_vals <- choices[[group]]
      if (length(group_vals) == 1) {
        make_option(as.character(group_vals), group, current_value)
      } else {
        make_optgroup(group, group_vals, current_value)
      }
    })
  } else {
    labels <- names(choices) %||% as.character(choices)
    map2(as.character(choices), labels, make_option, current_value)
  }
}


#' Resolve the choices for a dropdown cell
#'
#' Returns the appropriate choices for a cell, taking column dependencies into
#' account. If the column spec declares a `depends_on` column and provides a
#' `get_filtered_choices` function, the choices are filtered based on the
#' current value of the parent column in the same row. Otherwise the static
#' `choices` from the spec are returned unchanged.
#'
#' @param spec `list`. A single column specification from `col_specs`. See
#'   [flexible_table_server()] for the full field reference.
#' @param data `data.frame`. The current snapshot of the full table, as
#'   returned by `table_data()`.
#' @param index `numeric(1)`. The 1-based row index within `data` for the cell
#'   being rendered.
#'
#' @return A named `character` vector or named `list` of named `character`
#'   vectors — the same format as the `choices` field in a column spec.
#'
#' @seealso [make_flexible_col_def()]
#'
#' @noRd
resolve_choices <- function(spec, data, index) {
  has_dependency <- !is.null(spec$depends_on) &&
    !is.null(spec$get_filtered_choices)

  if (has_dependency) {
    spec$get_filtered_choices(data[[spec$depends_on]][index])
  } else {
    spec$choices
  }
}


#' Build a reactable column definition for a select col_spec entry
#'
#' Produces a [reactable::colDef()] whose `cell` renderer draws an HTML
#' `<select>` dropdown. The dropdown is wired to Shiny via an `onchange`
#' handler that calls `Shiny.setInputValue()`, making its value available as
#' `input[[paste0(spec$name, "_", row_id)]]` in the module server.
#'
#' @param spec `list`. A single column specification from `col_specs`. See
#'   [flexible_table_server()] for the full field reference.
#' @param table_data `reactive`. The `table_data` reactive from the parent
#'   module server, used inside the `cell` closure to look up the current row
#'   ID and dependency values at render time.
#' @param ns `function`. The module namespace function (`session$ns`) from the
#'   parent server, used to scope Shiny input IDs correctly.
#'
#' @return A [reactable::colDef()] object.
#'
#' @seealso [resolve_choices()], [make_options()], [flexible_table_server()]
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
make_flexible_select_col_def <- function(spec, table_data, ns) {
  colDef(
    name = spec$label,
    width = spec$width,
    cell = function(value, index) {
      data <- table_data()
      row_id <- data$id[index]
      input_id <- ns(paste0(spec$name, "_", row_id))
      choices <- resolve_choices(spec, data, index)

      tags$select(
        id = input_id,
        class = "form-control",
        style = if (!nzchar(value)) "color: #6c757d;" else "color: #000000;",
        onchange = sprintf(
          "this.style.color = this.value === '' ? '#6c757d' : '#000000';
   Shiny.setInputValue('%s', this.value)",
          input_id
        ),
        make_options(choices, value)
      )
    }
  )
}


#' Build a reactable column definition for a text input col_spec
#'
#' Renders a free-text `<input type="text">` in each cell. The value is
#' reported to Shiny via an `onchange` handler using `{priority: 'event'}`
#' so every keystroke-then-blur is captured reliably.
#'
#' @param spec `list`. A single col_spec with `type = "text"`. Accepts an
#'   optional `placeholder` field. `width` is forwarded to [reactable::colDef()].
#' @param table_data `reactive`. The `table_data` reactive from the parent
#'   module server.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
make_flexible_text_col_def <- function(spec, table_data, ns) {
  colDef(
    name = spec$label,
    width = spec$width,
    cell = function(value, index) {
      data <- table_data()
      row_id <- data$id[index]
      input_id <- ns(paste0(spec$name, "_", row_id))

      tags$input(
        type = "text",
        id = input_id,
        class = "form-control ft-text-input",
        value = if (!is.na(value) && nzchar(value)) value else NULL,
        placeholder = spec$placeholder %||% "",
        onchange = sprintf(
          "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
          input_id
        )
      )
    }
  )
}


#' Build a reactable column definition for a date input col_spec
#'
#' Renders a native `<input type="date">` in each cell. The value is always
#' a character string in `"YYYY-MM-DD"` format when non-empty, which is what
#' `as.Date()` expects. Optional `min` and `max` fields in the spec constrain
#' the browser's date picker.
#'
#' @param spec `list`. A single col_spec with `type = "date"`. Accepts
#'   optional `min` and `max` fields (character strings in `"YYYY-MM-DD"`
#'   format). `width` is forwarded to [reactable::colDef()].
#' @param table_data `reactive`. The `table_data` reactive from the parent
#'   module server.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @importFrom reactable colDef
#' @importFrom shiny tags
#'
#' @noRd
make_flexible_date_col_def <- function(spec, table_data, ns) {
  colDef(
    name = spec$label,
    width = spec$width,
    cell = function(value, index) {
      data <- table_data()
      row_id <- data$id[index]
      input_id <- ns(paste0(spec$name, "_", row_id))

      tags$input(
        type = "date",
        id = input_id,
        class = "form-control ft-date-input",
        value = if (!is.na(value) && nzchar(value)) value else NULL,
        min = spec$min %||% NULL,
        max = spec$max %||% NULL,
        onchange = sprintf(
          "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
          input_id
        )
      )
    }
  )
}


#' Dispatch a col_spec to its appropriate colDef builder
#'
#' Routes each column specification to the correct `make_flexible_*_col_def()`
#' function based on its `type` field. Defaults to `"select"` when `type`
#' is absent so existing col_specs require no migration.
#'
#' Supported types:
#' \describe{
#'   \item{`"select"`}{Dropdown — see [make_flexible_select_col_def()].}
#'   \item{`"text"`}{Free-text input — see [make_flexible_text_col_def()].}
#'   \item{`"date"`}{Date picker — see [make_flexible_date_col_def()].}
#' }
#'
#' @param spec `list`. A single column specification from `col_specs`.
#' @param table_data `reactive`. The `table_data` reactive from the parent
#'   module server.
#' @param ns `function`. The module namespace function.
#'
#' @return A [reactable::colDef()] object.
#'
#' @noRd
make_flexible_col_def <- function(spec, table_data, ns) {
  type <- spec$type %||% "select"
  switch(
    type,
    select = make_flexible_select_col_def(spec, table_data, ns),
    text = make_flexible_text_col_def(spec, table_data, ns),
    date = make_flexible_date_col_def(spec, table_data, ns),
    stop(sprintf(
      "Unknown column type: '%s'. Must be one of: 'select', 'text', 'date'.",
      type
    ))
  )
}
