# =============================================================================
# annotator_table module
#
# Renders a reactable table combining read-only display columns from a source
# data frame with interactive input columns (select, checkbox, text, number,
# radio) defined by col_specs. Returns a reactive data frame of row IDs and
# annotation values for rows where at least one input has been touched.
# =============================================================================

#' Annotator table — UI
#'
#' Returns the UI element for the annotator table module — a single
#' [reactable::reactableOutput()] placeholder. Unlike [flexible_table_ui()],
#' there are no add/reset buttons because rows are fixed to the source data
#' frame.
#'
#' @param id `character(1)`. The Shiny module namespace ID. Must match the
#'   `id` passed to [annotator_table_server()].
#'
#' @return A [reactable::reactableOutput()] UI element.
#'
#' @seealso [annotator_table_server()]
#'
#' @importFrom reactable reactableOutput
#'
#' @examples
#' \dontrun{
#' ui <- bslib::page_fluid(
#'   annotator_table_ui("my_annotator")
#' )
#' }
#'
#' @export
annotator_table_ui <- function(id) {
  reactableOutput(shiny::NS(id, "table"))
}


#' Annotator table — server
#'
#' Server-side logic for a Shiny module that renders a [reactable::reactable()]
#' table combining read-only display columns from a source data frame with
#' interactive input columns. The module returns a reactive data frame
#' containing the row ID and annotation values for every row where at least
#' one input has been touched by the user.
#'
#' Annotations are preserved across reactive changes to `source_data` — if the
#' source data is filtered and a previously annotated row disappears, its
#' annotation is retained and restored if that row reappears.
#'
#' @param id `character(1)`. The Shiny module namespace ID. Must match the
#'   `id` passed to [annotator_table_ui()].
#'
#' @param source_data `reactive`. A reactive expression returning the source
#'   data frame. Column names must include the column named by `row_id` and
#'   all columns referenced by display col_specs.
#'
#' @param row_id `character(1)`. The name of the column in `source_data` that
#'   uniquely identifies each row (e.g. `"id"` or `"car_name"`).
#'
#' @param col_specs `list`. A list of column specification lists describing
#'   every column to show in the table. Each entry must have a `name` and
#'   `type`. Supported types and their additional fields:
#'   \describe{
#'     \item{`"display"`}{Read-only column sourced from `source_data`. `name`
#'       must match a column in the data frame.}
#'     \item{`"select"`}{Dropdown. Requires `choices`: a named character vector.}
#'     \item{`"checkbox"`}{Checkbox. No additional fields required.}
#'     \item{`"text"`}{Free-text input. Optionally accepts `placeholder`.}
#'     \item{`"number"`}{Numeric input. Optionally accepts `min` and `max`.}
#'     \item{`"radio"`}{Radio button group. Requires `choices`: a named
#'       character vector.}
#'   }
#'   All types accept an optional `label` (defaults to `name`) and optional
#'   `width` in pixels.
#'
#' @param reactable_theme A [reactable::reactableTheme()] object applied to
#'   the rendered table. Defaults to `theme_bare`.
#' @param reactable_options `list`. Additional arguments passed to
#'   [reactable::reactable()] via [base::do.call()].
#'
#' @param initial_values `reactive`. A reactive returning a data frame of
#'   pre-existing annotation values to seed the module. Defaults to
#'   `reactive(NULL)`.
#'
#' @return A [shiny::reactive()] returning a data frame with the `row_id`
#'   column and all input annotation columns, filtered to rows where at least
#'   one input has been touched. Display columns are not included in the
#'   return value.
#'
#' @seealso [annotator_table_ui()]
#'
#' @importFrom shiny moduleServer NS reactive reactiveVal observe observeEvent
#' @importFrom reactable reactable colDef renderReactable
#' @importFrom purrr map map_chr keep walk
#' @importFrom dplyr left_join
#'
#' @examples
#' \dontrun{
#' col_specs <- list(
#'   list(name = "car",      type = "display",  label = "Car"),
#'   list(name = "mpg",      type = "display",  label = "MPG"),
#'   list(name = "category", type = "select",   label = "Category",
#'        choices = c("Cheap" = "cheap", "Expensive" = "expensive")),
#'   list(name = "approved",  type = "checkbox", label = "Approved?"),
#'   list(name = "notes",     type = "text",     label = "Notes")
#' )
#'
#' ui <- bslib::page_fluid(
#'   annotator_table_ui("cars")
#' )
#'
#' server <- function(input, output, session) {
#'   source <- reactive(mtcars)
#'
#'   result <- annotator_table_server(
#'     id          = "cars",
#'     source_data = source,
#'     row_id      = "car_name",
#'     col_specs   = col_specs
#'   )
#'
#'   observe(print(result()))
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
annotator_table_server <- function(
  id,
  source_data,
  row_id,
  col_specs,
  reactable_theme = theme_bare,
  reactable_options = list(),
  initial_values = reactive(NULL)
) {
  if (is.null(col_specs) || length(col_specs) == 0) {
    stop("`col_specs` must be a non-empty list of column specifications.")
  }

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # After:
    input_specs <- keep(
      col_specs,
      ~ !.x$type %in% c("display", "clickable_display")
    )
    display_specs <- keep(
      col_specs,
      ~ .x$type %in% c("display", "clickable_display")
    )

    # -------------------------------------------------------------------------
    # Reactive state
    #
    # annotations stores all user input keyed by row_id, including rows
    # that may no longer be visible in the current source_data. This means
    # annotations survive reactive source_data changes — if a row disappears
    # due to a filter and then reappears, its values are restored.
    # -------------------------------------------------------------------------

    starting_vals <- isolate(initial_values())

    annotations <- reactiveVal(
      if (!is.null(starting_vals) && nrow(starting_vals) > 0) {
        merge_annotations(
          isolate(source_data()),
          row_id,
          input_specs,
          starting_vals
        )
      } else {
        initial_annotations(isolate(source_data()), row_id, input_specs)
      }
    )

    # -------------------------------------------------------------------------
    # Respond to source_data changes.
    #
    # Merge preserves existing annotations for rows that remain, assigns blank
    # defaults to new rows, and retains annotations for rows that have left
    # the visible set (they stay in the reactiveVal silently).
    #
    # We also bump reactable_remount_trigger so the table fully re-renders
    # with the new rows — this is the only time a full re-render is needed.
    # -------------------------------------------------------------------------

    reactable_remount_trigger <- reactiveVal(0)

    observeEvent(source_data(), {
      annotations(
        merge_annotations(source_data(), row_id, input_specs, annotations())
      )
      reactable_remount_trigger(reactable_remount_trigger() + 1)
    })

    # -------------------------------------------------------------------------
    # Sync input values back into annotations.
    #
    # For each visible row and each input column, we check whether Shiny has
    # received a value for that cell's input ID. If it has, and it differs
    # from what we have stored, we update the annotations reactiveVal.
    #
    # We write back only when something changed to avoid unnecessary reactive
    # cycles. Crucially, writing to annotations() does NOT trigger a table
    # re-render because renderReactable uses isolate(annotations()) — the
    # table only re-renders when source_data() changes or the trigger fires.
    # -------------------------------------------------------------------------

    observe({
      data <- source_data()
      current <- annotations()
      updated <- current

      walk(seq_len(nrow(data)), function(i) {
        id_value <- data[[row_id]][i]
        ann_row <- which(updated[[row_id]] == id_value)

        walk(input_specs, function(spec) {
          input_id <- paste0(spec$name, "_", id_value)
          val <- input[[input_id]]

          if (!is.null(val) && !identical(val, updated[[spec$name]][ann_row])) {
            updated[[spec$name]][ann_row] <<- val
          }
        })
      })

      if (!identical(updated, current)) {
        annotations(updated)
      }
    })

    output$table <- renderReactable({
      force(reactable_remount_trigger())

      data <- source_data()
      ann <- isolate(annotations())
      ann_snap <- function() ann
      data_snap <- function() data

      spec_names <- map_chr(col_specs, "name")
      render_data <- left_join(data, ann, by = row_id) |>
        dplyr::select(dplyr::all_of(c(row_id, spec_names)))

      col_defs <- map(col_specs, \(spec) {
        make_input_col_def(spec, ann_snap, row_id, ns, data_snap)
      }) |>
        set_names(map_chr(col_specs, "name"))

      do.call(
        reactable,
        c(
          list(
            data = render_data,
            columns = col_defs,
            theme = reactable_theme
          ),
          reactable_options
        )
      )
    })

    # bslib::navset_pill tab panes can prevent Shiny from detecting
    # visibility changes on nested htmlwidget outputs. Force the render
    # to fire regardless of container visibility so the table appears
    # the moment its data is ready.
    outputOptions(output, "table", suspendWhenHidden = FALSE)

    # -------------------------------------------------------------------------
    # Return value
    #
    # The annotations reactiveVal covers all rows ever seen, including those
    # currently filtered out of source_data. We return only rows that are
    # currently in source_data AND have at least one touched input, so the
    # caller always gets a result that corresponds to visible data.
    # -------------------------------------------------------------------------

    reactive({
      data <- source_data()
      ann <- annotations()

      # Restrict to rows currently visible in source_data
      visible_ids <- data[[row_id]]
      visible_ann <- ann[ann[[row_id]] %in% visible_ids, ]

      # Filter to rows where at least one input has been touched
      touched <- any_touched(visible_ann, input_specs)
      visible_ann[touched, ]
    })
  })
}
