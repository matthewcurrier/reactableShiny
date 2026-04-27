# Annotator Table

The annotator table is a Shiny module that layers interactive input
columns on top of a read-only data frame. It is designed for
human-in-the-loop workflows where you want reviewers to tag, categorise,
or score every row in a dataset — without being able to edit the
underlying data.

## How it works

You provide a source data frame and a list of **column specifications**
(`col_specs`). Each spec declares whether the column should be a
read-only display column or an interactive input — `select`, `checkbox`,
`text`, `number`, or `radio`. The module renders everything in a single
[reactable](https://glin.github.io/reactable/) table and returns a
reactive data frame of annotations for rows that have been touched by
the user.

A few things worth knowing:

- **Annotations survive filtering.** If `source_data` is a reactive
  expression that can change (e.g. driven by a search box), annotations
  for rows that temporarily disappear are preserved and restored when
  those rows return.
- **Only touched rows are returned.** The module’s return value excludes
  rows where every input is still at its default, so you don’t have to
  filter out blanks yourself.
- **Gating is built in.** Any input column can declare
  `gates = "my_checkbox"` to have its interactivity controlled by a
  checkbox in the same row. When the checkbox is unchecked the gated
  inputs grey out instantly — no Shiny round-trip needed.

## Basic usage

Attach the UI placeholder with
[`annotator_table_ui()`](https://matthewcurrier.github.io/reactableShiny/reference/annotator_table_ui.md)
and start the server logic with
[`annotator_table_server()`](https://matthewcurrier.github.io/reactableShiny/reference/annotator_table_server.md).
The server function returns a reactive that you can observe or pass
downstream.

``` r
library(shiny)
library(bslib)
library(reactableShiny)

col_specs <- list(
  list(name = "car",      type = "display",  label = "Car"),
  list(name = "mpg",      type = "display",  label = "MPG"),
  list(name = "cyl",      type = "display",  label = "Cylinders"),
  list(name = "category", type = "select",   label = "Category",
       choices = c("Economy" = "economy", "Sport" = "sport", "Truck" = "truck")),
  list(name = "approved", type = "checkbox", label = "Approved?"),
  list(name = "notes",    type = "text",     label = "Notes",
       placeholder = "Optional comment...")
)

ui <- page_fluid(
  h4("Review cars"),
  annotator_table_ui("cars"),
  hr(),
  h4("Annotations so far"),
  tableOutput("result")
)

server <- function(input, output, session) {
  cars_df <- tibble::rownames_to_column(mtcars, var = "car")
  source  <- reactive(cars_df)

  annotations <- annotator_table_server(
    id          = "cars",
    source_data = source,
    row_id      = "car",
    col_specs   = col_specs
  )

  output$result <- renderTable(annotations())
}

shinyApp(ui, server)
```

Running the app produces a table with the `car`, `mpg`, and `cyl`
columns shown as plain text, and a **Category** dropdown, **Approved?**
checkbox, and **Notes** field in each row. `output$result` updates in
real time to show only the rows where at least one input has been filled
in.

## Column types

| Type       | Returns       | Key fields                         |
|------------|---------------|------------------------------------|
| `display`  | — (read-only) | `name` must exist in `source_data` |
| `select`   | `character`   | `choices` (named character vector) |
| `checkbox` | `logical`     | —                                  |
| `text`     | `character`   | `placeholder` (optional)           |
| `number`   | `numeric`     | `min`, `max` (optional)            |
| `radio`    | `character`   | `choices` (named character vector) |

Every column type also accepts an optional `label` (shown as the column
header; defaults to `name`) and `width` in pixels.

## Gating example

Use `gates` to make one checkbox control whether other inputs in the
same row are interactive. This is useful when a “flag this row” checkbox
should unlock additional detail fields.

``` r
col_specs <- list(
  list(name = "car",    type = "display",  label = "Car"),
  list(name = "flag",   type = "checkbox", label = "Flag for review?"),
  list(name = "reason", type = "select",   label = "Reason",
       choices = c("Price" = "price", "Safety" = "safety"),
       gates   = "flag"),
  list(name = "notes",  type = "text",     label = "Notes",
       placeholder = "Details...",
       gates   = "flag")
)
```

Here, **Reason** and **Notes** are greyed out until **Flag for review?**
is checked. Unchecking it again restores the previous values — they are
never cleared.

## Interactive demo

The app below demonstrates the full `annotator_table` module using a
representative set of developmental milestone rows. Check **Met?** to
mark a milestone as achieved, and add a short note — the annotation
panel at the bottom updates in real time to show only rows you have
touched.

``` shinylive-r
#| '!! shinylive warning !!': |
#|   shinylive does not work in self-contained HTML documents.
#|   Please set `embed-resources: false` in your metadata.
#| standalone: true
#| viewerHeight: 750

library(shiny)
library(bslib)
library(reactable)
library(purrr)
library(dplyr)

# ── Null-coalescing operator ───────────────────────────────────────────────────
`%||%` <- function(x, y) if (is.null(x)) y else x

# ── Theme ──────────────────────────────────────────────────────────────────────
theme_bare <- reactable::reactableTheme(
  borderColor     = "transparent",
  headerStyle     = list(borderBottom = "none", fontWeight = "normal"),
  backgroundColor = "transparent"
)

# ── Option builders (from utils_flexible.R) ────────────────────────────────────

make_option <- function(value, label, current_value) {
  is_selected <- if (current_value == value) "selected" else NULL
  style <- if (!nzchar(value)) "color: #6c757d;" else NULL
  tags$option(value = value, selected = is_selected, style = style, label)
}

make_optgroup <- function(group_name, group_vals, current_value) {
  options <- map2(
    as.character(group_vals),
    names(group_vals),
    make_option,
    current_value
  )
  tags$optgroup(label = group_name, options)
}

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

# ── Annotation data frame helpers (from utils_annotator.R) ────────────────────

default_annotation_value <- function(spec) {
  switch(
    spec$type,
    select        = NA_character_,
    text          = NA_character_,
    radio         = NA_character_,
    checkbox      = FALSE,
    text_checkbox = FALSE,
    number        = NA_real_,
    stop(sprintf("Unknown input column type: '%s'", spec$type))
  )
}

initial_annotations <- function(source_data, row_id, input_specs) {
  id_values    <- source_data[[row_id]]
  default_cols <- map(input_specs, default_annotation_value)
  names(default_cols) <- map_chr(input_specs, "name")

  id_col <- list(id_values)
  names(id_col) <- row_id

  if (length(id_values) == 0) {
    col_names <- c(row_id, map_chr(input_specs, "name"))
    empty <- as.data.frame(
      matrix(nrow = 0, ncol = length(col_names)),
      stringsAsFactors = FALSE
    )
    names(empty) <- col_names
    return(empty)
  }

  as.data.frame(c(id_col, default_cols), stringsAsFactors = FALSE)
}

merge_annotations <- function(source_data, row_id, input_specs, existing) {
  blank  <- initial_annotations(source_data, row_id, input_specs)
  merged <- left_join(blank[, row_id, drop = FALSE], existing, by = row_id)

  for (spec in input_specs) {
    col          <- spec$name
    missing_rows <- is.na(merged[[col]])
    if (any(missing_rows)) {
      merged[[col]][missing_rows] <- default_annotation_value(spec)
    }
  }
  merged
}

is_touched <- function(value, type) {
  switch(
    type,
    select        = !is.null(value) && !is.na(value) && nzchar(value),
    text          = !is.null(value) && !is.na(value) && nzchar(value),
    radio         = !is.null(value) && !is.na(value) && nzchar(value),
    checkbox      = isTRUE(value),
    text_checkbox = isTRUE(value),
    number        = !is.null(value) && !is.na(value) && value != 0,
    stop(sprintf("Unknown input column type: '%s'", type))
  )
}

any_touched <- function(annotations, input_specs) {
  map_lgl(seq_len(nrow(annotations)), function(i) {
    any(map_lgl(input_specs, function(spec) {
      is_touched(annotations[[spec$name]][i], spec$type)
    }))
  })
}

# ── Gating helpers ─────────────────────────────────────────────────────────────

gate_is_open <- function(spec, ann, index) {
  if (is.null(spec$gates)) return(TRUE)
  gate_value <- ann[[spec$gates]][index]
  if (!is.null(spec$gates_values)) {
    return(!is.null(gate_value) && !is.na(gate_value) &&
             gate_value %in% spec$gates_values)
  }
  isTRUE(gate_value)
}

gated_by_id <- function(spec, ns, id_value) {
  if (is.null(spec$gates)) return(NULL)
  ns(paste0(spec$gates, "_", id_value))
}

gated_style <- function(is_open) {
  if (is_open) NULL else "opacity: 0.4; pointer-events: none;"
}

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

# ── Column definition builders ─────────────────────────────────────────────────

make_display_col_def <- function(spec) {
  base <- list(name = spec$label %||% spec$name, width = spec$width)
  do.call(colDef, modifyList(base, spec$col_def_options %||% list()))
}

make_checkbox_col_def <- function(spec, annotations, row_id, ns) {
  do.call(
    colDef,
    c(
      list(
        name  = spec$label %||% spec$name,
        width = spec$width,
        cell  = function(value, index) {
          ann        <- annotations()
          id_value   <- ann[[row_id]][index]
          input_id   <- ns(paste0(spec$name, "_", id_value))
          is_checked <- isTRUE(ann[[spec$name]][index])
          open       <- gate_is_open(spec, ann, index)
          gb_id      <- gated_by_id(spec, ns, id_value)

          tags$input(
            type            = "checkbox",
            id              = input_id,
            checked         = if (is_checked) "checked" else NULL,
            style           = gated_style(open),
            `data-gated-by` = gb_id,
            onchange        = gating_checkbox_onchange(input_id)
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}

make_select_col_def <- function(spec, annotations, row_id, ns) {
  force(ns)
  do.call(
    colDef,
    c(
      list(
        name  = spec$label %||% spec$name,
        width = spec$width,
        cell  = function(value, index) {
          ann                <- annotations()
          id_value           <- ann[[row_id]][index]
          input_id           <- ns(paste0(spec$name, "_", id_value))
          current            <- ann[[spec$name]][index]
          open               <- gate_is_open(spec, ann, index)
          gb_id              <- gated_by_id(spec, ns, id_value)
          is_blank           <- is.na(current) || !nzchar(current)
          current_for_options <- if (is.na(current)) "" else current
          placeholder_style  <- if (is_blank) "color: #6c757d;" else ""
          gate_style         <- if (!open) "opacity: 0.4; pointer-events: none;" else ""

          tags$select(
            id              = input_id,
            class           = "form-control",
            style           = paste0(placeholder_style, gate_style),
            `data-gated-by` = gb_id,
            onchange        = sprintf(
              "var v = this.value;
     this.style.color = v === '' ? '#6c757d' : '';
     Shiny.setInputValue('%s', v, {priority: 'event'});
     var myId = '%s';
     document.querySelectorAll('[data-gated-by=\"' + myId + '\"]').forEach(function(el) {
       el.style.opacity       = v !== '' ? '' : '0.4';
       el.style.pointerEvents = v !== '' ? '' : 'none';
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

make_text_col_def <- function(spec, annotations, row_id, ns) {
  force(ns)
  do.call(
    colDef,
    c(
      list(
        name  = spec$label %||% spec$name,
        width = spec$width,
        cell  = function(value, index) {
          ann      <- annotations()
          id_value <- ann[[row_id]][index]
          input_id <- ns(paste0(spec$name, "_", id_value))
          current  <- ann[[spec$name]][index]
          open     <- gate_is_open(spec, ann, index)
          gb_id    <- gated_by_id(spec, ns, id_value)

          tags$input(
            type            = "text",
            id              = input_id,
            class           = "form-control",
            value           = if (!is.na(current)) current else NULL,
            placeholder     = spec$placeholder %||% "",
            style           = gated_style(open),
            `data-gated-by` = gb_id,
            onchange        = sprintf(
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

make_number_col_def <- function(spec, annotations, row_id, ns) {
  force(ns)
  do.call(
    colDef,
    c(
      list(
        name  = spec$label %||% spec$name,
        width = spec$width,
        cell  = function(value, index) {
          ann      <- annotations()
          id_value <- ann[[row_id]][index]
          input_id <- ns(paste0(spec$name, "_", id_value))
          current  <- ann[[spec$name]][index]
          open     <- gate_is_open(spec, ann, index)
          gb_id    <- gated_by_id(spec, ns, id_value)

          tags$input(
            type            = "number",
            id              = input_id,
            class           = "form-control",
            value           = if (!is.na(current)) current else NULL,
            min             = spec$min %||% NULL,
            max             = spec$max %||% NULL,
            style           = gated_style(open),
            `data-gated-by` = gb_id,
            onchange        = sprintf(
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

make_radio_col_def <- function(spec, annotations, row_id, ns) {
  force(ns)
  do.call(
    colDef,
    c(
      list(
        name  = spec$label %||% spec$name,
        width = spec$width,
        cell  = function(value, index) {
          ann      <- annotations()
          id_value <- ann[[row_id]][index]
          group_id <- ns(paste0(spec$name, "_", id_value))
          current  <- ann[[spec$name]][index]
          open     <- gate_is_open(spec, ann, index)
          gb_id    <- gated_by_id(spec, ns, id_value)

          radio_buttons <- map(names(spec$choices), function(label) {
            val    <- spec$choices[[label]]
            btn_id <- paste0(group_id, "_", val)
            tags$label(
              style = "display: block; font-weight: normal;",
              tags$input(
                type     = "radio",
                id       = btn_id,
                name     = group_id,
                value    = val,
                checked  = if (identical(current, val)) "checked" else NULL,
                onchange = sprintf(
                  "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
                  group_id
                )
              ),
              paste0(" ", label)
            )
          })

          tags$div(
            style           = gated_style(open),
            `data-gated-by` = gb_id,
            radio_buttons
          )
        }
      ),
      spec$col_def_options %||% list()
    )
  )
}

make_input_col_def <- function(spec, annotations, row_id, ns, source_data = NULL) {
  force(ns)
  switch(
    spec$type,
    display  = make_display_col_def(spec),
    select   = make_select_col_def(spec, annotations, row_id, ns),
    checkbox = make_checkbox_col_def(spec, annotations, row_id, ns),
    text     = make_text_col_def(spec, annotations, row_id, ns),
    number   = make_number_col_def(spec, annotations, row_id, ns),
    radio    = make_radio_col_def(spec, annotations, row_id, ns),
    stop(sprintf("Unknown column type: '%s'", spec$type))
  )
}

# ── Module ─────────────────────────────────────────────────────────────────────

annotator_table_ui <- function(id) {
  reactableOutput(shiny::NS(id, "table"))
}

annotator_table_server <- function(
  id,
  source_data,
  row_id,
  col_specs,
  reactable_theme   = theme_bare,
  reactable_options = list(),
  initial_values    = reactive(NULL)
) {
  if (is.null(col_specs) || length(col_specs) == 0) {
    stop("`col_specs` must be a non-empty list of column specifications.")
  }

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    input_specs   <- keep(col_specs, ~ !.x$type %in% c("display", "clickable_display"))
    display_specs <- keep(col_specs, ~  .x$type %in% c("display", "clickable_display"))

    starting_vals <- isolate(initial_values())

    annotations <- reactiveVal(
      if (!is.null(starting_vals) && nrow(starting_vals) > 0) {
        merge_annotations(isolate(source_data()), row_id, input_specs, starting_vals)
      } else {
        initial_annotations(isolate(source_data()), row_id, input_specs)
      }
    )

    reactable_remount_trigger <- reactiveVal(0)

    observeEvent(source_data(), {
      annotations(merge_annotations(source_data(), row_id, input_specs, annotations()))
      reactable_remount_trigger(reactable_remount_trigger() + 1)
    })

    observe({
      data    <- source_data()
      current <- annotations()
      updated <- current

      walk(seq_len(nrow(data)), function(i) {
        id_value <- data[[row_id]][i]
        ann_row  <- which(updated[[row_id]] == id_value)

        walk(input_specs, function(spec) {
          input_id <- paste0(spec$name, "_", id_value)
          val      <- input[[input_id]]
          if (!is.null(val) && !identical(val, updated[[spec$name]][ann_row])) {
            updated[[spec$name]][ann_row] <<- val
          }
        })
      })

      if (!identical(updated, current)) annotations(updated)
    })

    output$table <- renderReactable({
      force(reactable_remount_trigger())

      data      <- source_data()
      ann       <- isolate(annotations())
      ann_snap  <- function() ann
      data_snap <- function() data

      spec_names  <- map_chr(col_specs, "name")
      render_data <- left_join(data, ann, by = row_id) |>
        select(all_of(c(row_id, spec_names)))

      col_defs <- map(col_specs, \(spec) {
        make_input_col_def(spec, ann_snap, row_id, ns, data_snap)
      }) |>
        set_names(map_chr(col_specs, "name"))

      do.call(
        reactable,
        c(list(data = render_data, columns = col_defs, theme = reactable_theme),
          reactable_options)
      )
    })

    reactive({
      data        <- source_data()
      ann         <- annotations()
      visible_ids <- data[[row_id]]
      visible_ann <- ann[ann[[row_id]] %in% visible_ids, ]
      touched     <- any_touched(visible_ann, input_specs)
      visible_ann[touched, ]
    })
  })
}

# ── Hardcoded demo data ────────────────────────────────────────────────────────

milestones <- data.frame(
  id        = 1:10,
  age       = c("6 months",  "6 months",  "12 months", "12 months",
                "18 months", "18 months", "24 months", "24 months",
                "36 months", "36 months"),
  category  = c("Motor", "Language", "Motor", "Language",
                "Motor", "Language", "Motor", "Language",
                "Motor", "Language"),
  milestone = c(
    "Sits with support",          "Babbles consonants",
    "Pulls to stand",             "Says mama/dada with meaning",
    "Walks alone",                "Points to objects of interest",
    "Runs without falling",       "Uses 2-word phrases",
    "Jumps with both feet",       "Speaks in 3-word sentences"
  ),
  stringsAsFactors = FALSE
)

# ── Column specs ───────────────────────────────────────────────────────────────

col_specs <- list(
  list(name = "id",        type = "display",  label = "ID",        width = 50),
  list(name = "age",       type = "display",  label = "Age",       width = 110),
  list(name = "category",  type = "display",  label = "Category",  width = 120),
  list(name = "milestone", type = "display",  label = "Milestone", width = 340),
  list(name = "met",       type = "checkbox", label = "Met?",      width = 60),
  list(
    name        = "notes",
    type        = "text",
    label       = "Notes",
    placeholder = "Short note...",
    width       = 200
  )
)

# ── App ────────────────────────────────────────────────────────────────────────

ui <- page_fluid(
  theme = bs_theme(version = 5),
  card(
    card_header("Developmental Milestones"),
    card_body(annotator_table_ui("milestones"))
  ),
  card(
    card_header("Annotations so far"),
    card_body(tableOutput("annotations_out"))
  )
)

server <- function(input, output, session) {
  source_data <- reactive(milestones)

  annotations <- annotator_table_server(
    id          = "milestones",
    source_data = source_data,
    row_id      = "id",
    col_specs   = col_specs
  )

  output$annotations_out <- renderTable({
    ann <- annotations()

    if (nrow(ann) == 0) {
      return(data.frame(Message = "No milestones annotated yet."))
    }

    purrr::reduce(
      list(ann, milestones),
      ~ left_join(.x, .y, by = "id")
    ) |>
      select(id, age, category, milestone, met, notes)
  })
}

shinyApp(ui, server)
```
