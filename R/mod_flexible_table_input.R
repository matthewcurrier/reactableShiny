#' Flexible editable table — UI
#'
#' Returns a named list of UI elements for the flexible table module. The caller
#' is responsible for placing each element in their layout. Separating the
#' elements this way lets the caller position the table and buttons
#' independently (e.g. buttons in a sidebar, table in the main panel).
#'
#' @param id `character(1)`. The Shiny module namespace ID. Must match the `id`
#'   passed to [flexible_table_server()].
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{`table`}{A [reactable::reactableOutput()] placeholder.}
#'     \item{`add_row_button`}{An [shiny::actionButton()] that appends a blank row.}
#'     \item{`reset_button`}{An [shiny::actionButton()] that clears the table to
#'       a single blank row.}
#'   }
#'
#' @seealso [flexible_table_server()]
#'
#' @importFrom shiny NS tagList actionButton
#' @importFrom reactable reactableOutput
#' @importFrom bsicons bs_icon
#'
#' @examples
#' \dontrun{
#' # In your UI definition:
#' ui <- bslib::page_fluid(
#'   flexible_table_ui("fruit_picker")$add_row_button,
#'   flexible_table_ui("fruit_picker")$reset_button,
#'   flexible_table_ui("fruit_picker")$table
#' )
#' }
#'
#' @export
flexible_table_ui <- function(id) {
  ns <- NS(id)

  list(
    table = reactableOutput(ns("table")),
    add_row_button = actionButton(
      ns("add_row"),
      label = tagList(bsicons::bs_icon("plus-circle"), " Add Row"),
      class = "btn-primary"
    ),
    reset_button = actionButton(
      ns("reset"),
      label = tagList(bsicons::bs_icon("arrow-counterclockwise"), " Reset"),
      class = "btn-secondary"
    )
  )
}


#' Flexible editable table — server
#'
#' Server-side logic for a Shiny module that renders an editable
#' [reactable::reactable()] table. Each row contains one dropdown per column,
#' built from the `col_specs` you provide. Rows can be added, deleted, and
#' reset. Columns can optionally depend on each other: selecting a value in a
#' parent column filters the choices in a child column and clears any stale
#' selection.
#'
#' @param id `character(1)`. The Shiny module namespace ID. Must match the `id`
#'   passed to [flexible_table_ui()].
#'
#' @param col_specs `list`. A list of column specification lists. Each element
#'   describes one column and may contain:
#'   \describe{
#'     \item{`name`}{`character(1)`. Column variable name (used as the data
#'       frame column name and to construct Shiny input IDs).}
#'     \item{`label`}{`character(1)`. Header label displayed in the table.}
#'     \item{`choices`}{Named character vector, or named list of named vectors
#'       for option-group dropdowns.}
#'     \item{`default`}{*(optional)* `character(1)`. Pre-selected value for new
#'       rows. Defaults to `""` (blank placeholder).}
#'     \item{`width`}{*(optional)* `numeric(1)`. Column width in pixels.}
#'     \item{`depends_on`}{*(optional)* `character(1)`. The `name` of another
#'       column whose selected value controls the choices in this column.}
#'     \item{`get_filtered_choices`}{*(optional)* `function(dependency_value)`.
#'       Called with the current value of the `depends_on` column; must return
#'       a choices vector/list in the same format as `choices`.}
#'   }
#'
#' @param initial_data `reactive` or `NULL`. A reactive expression returning a
#'   data frame used to pre-populate the table. Column names must match the
#'   `name` fields in `col_specs`. Re-fires whenever the reactive changes,
#'   replacing the current table contents. An empty data frame (`nrow == 0`) is
#'   treated as a reset signal. Defaults to `NULL` (table starts with one blank
#'   row).
#'
#' @param reactable_theme A [reactable::reactableTheme()] object applied to the
#'   rendered table. Defaults to `theme_bare`, a minimal transparent theme
#'   defined in `themes.R`.
#'
#' @param duplicates_allowed `logical(1)`. If `FALSE`, duplicate rows are
#'   removed from the returned reactive data frame. Defaults to `TRUE`.
#'
#' @return A [shiny::reactive()] that returns a data frame of the current table
#'   contents. Internal helper columns (`id`, `delete`) are excluded. Rows
#'   where any cell is blank, `NA`, or `NULL` are also excluded.
#'
#' @seealso [flexible_table_ui()]
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent observe reactive
#' @importFrom reactable reactable colDef renderReactable
#' @importFrom dplyr filter select distinct if_all everything
#' @importFrom purrr map map_chr walk set_names
#'
#' @examples
#' \dontrun{
#' col_specs <- list(
#'   list(
#'     name    = "fruit",
#'     label   = "Fruit",
#'     choices = c("" = "", "Apple" = "apple", "Banana" = "banana")
#'   ),
#'   list(
#'     name    = "colour",
#'     label   = "Colour",
#'     choices = c("" = "", "Red" = "red", "Yellow" = "yellow")
#'   )
#' )
#'
#' ui <- bslib::page_fluid(
#'   do.call(tagList, flexible_table_ui("fruit_picker"))
#' )
#'
#' server <- function(input, output, session) {
#'   selected <- flexible_table_server("fruit_picker", col_specs)
#'
#'   observe({
#'     print(selected())  # data frame of complete, non-blank rows
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
flexible_table_server <- function(
  id,
  col_specs,
  initial_data = NULL,
  reactable_theme = theme_bare,
  duplicates_allowed = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Reactive state
    # -------------------------------------------------------------------------

    table_data <- reactiveVal(make_row(1, col_specs))
    next_id <- reactiveVal(2)
    reactable_remount_trigger <- reactiveVal(0)

    # -------------------------------------------------------------------------
    # Helpers that close over local reactive state
    # -------------------------------------------------------------------------

    bump_id <- function() next_id(next_id() + 1)

    force_table_remount <- function() {
      reactable_remount_trigger(reactable_remount_trigger() + 1)
    }

    append_blank_row <- function() {
      new_row <- make_row(next_id(), col_specs)
      table_data(rbind(table_data(), new_row))
      bump_id()
    }

    reset_to_blank_row <- function() {
      table_data(make_row(next_id(), col_specs))
      bump_id()
    }

    # -------------------------------------------------------------------------
    # Load initial_data whenever the reactive fires.
    # The observer is only registered when initial_data is actually provided —
    # calling NULL() would throw an error, so we guard with if (!is.null(...)).
    # ignoreInit = FALSE so the table populates immediately on app startup.
    # -------------------------------------------------------------------------

    if (!is.null(initial_data)) {
      observeEvent(
        initial_data(),
        {
          df <- initial_data()

          if (is.null(df)) {
            return()
          }

          if (nrow(df) == 0) {
            # Empty data frame is a reset signal (e.g. switching to add mode)
            reset_to_blank_row()
            return()
          }

          new_rows <- load_data(df, next_id(), col_specs)
          next_id(next_id() + nrow(df))
          table_data(new_rows)
        },
        ignoreNULL = TRUE,
        ignoreInit = FALSE
      )
    }

    # -------------------------------------------------------------------------
    # Add row
    # -------------------------------------------------------------------------

    observeEvent(input$add_row, {
      append_blank_row()
    })

    # -------------------------------------------------------------------------
    # Reset
    # -------------------------------------------------------------------------

    observeEvent(input$reset, {
      reset_to_blank_row()
      force_table_remount()
    })

    # -------------------------------------------------------------------------
    # Delete a row.
    # If deleting the last row, replace it with a fresh blank row rather than
    # leaving the table empty.
    # -------------------------------------------------------------------------

    register_delete_handler <- function(row_id) {
      observeEvent(
        input[[paste0("delete_", row_id)]],
        {
          remaining <- filter(table_data(), id != row_id)
          if (nrow(remaining) == 0) {
            remaining <- make_row(next_id(), col_specs)
          }
          table_data(remaining)
        },
        ignoreInit = TRUE # must be TRUE to prevent phantom deletions on startup
      )
    }

    observe({
      walk(table_data()$id, register_delete_handler)
    })

    # -------------------------------------------------------------------------
    # Sync dropdown inputs back into table_data
    # -------------------------------------------------------------------------

    observe({
      current <- table_data()
      updated <- current

      walk(seq_len(nrow(current)), function(i) {
        row_id <- current$id[i]

        walk(col_specs, function(spec) {
          val <- input[[paste0(spec$name, "_", row_id)]]

          if (!is.null(val)) {
            old_val <- updated[[spec$name]][i]
            updated[[spec$name]][i] <<- val

            # If this column changed, clear any columns that depend on it
            if (!is.null(old_val) && old_val != val) {
              walk(col_specs, function(dep) {
                if (!is.null(dep$depends_on) && dep$depends_on == spec$name) {
                  updated[[dep$name]][i] <<- ""
                }
              })
            }
          }
        })
      })

      # Only write back when something actually changed — avoids a redundant
      # reactive cycle that would cause reactable to reconcile in place rather
      # than remounting cleanly after a reset.
      if (!identical(updated, current)) {
        table_data(updated)
      }
    })

    # -------------------------------------------------------------------------
    # Render table
    # -------------------------------------------------------------------------

    make_delete_button <- function(index) {
      row_id <- table_data()$id[index]
      btn_id <- ns(paste0("delete_", row_id))
      tags$button(
        id = btn_id,
        class = "btn btn-danger btn-sm",
        onclick = sprintf("Shiny.setInputValue('%s', Math.random())", btn_id),
        icon("trash"),
        " Delete"
      )
    }

    output$table <- renderReactable({
      force(reactable_remount_trigger()) # take dependency so reset triggers a clean full render
      data <- table_data()

      spec_cols <- map(col_specs, make_col_def, table_data, ns) |>
        set_names(map_chr(col_specs, "name"))

      all_cols <- c(
        list(id = colDef(show = FALSE)),
        spec_cols,
        list(delete = colDef(name = "", width = 100, cell = make_delete_button))
      )

      reactable(data, columns = all_cols, theme = reactable_theme)
    })

    # -------------------------------------------------------------------------
    # Return clean data (no id / delete helper columns).
    # Rows with any blank/NA value are dropped.
    # If duplicates_allowed = FALSE, duplicate rows are also removed.
    # -------------------------------------------------------------------------

    reactive({
      data <- table_data() |>
        select(-id, -delete) |>
        filter(if_all(everything(), ~ !is.null(.x) & !is.na(.x) & .x != ""))

      if (duplicates_allowed) data else distinct(data)
    })
  })
}


#' Register a delete button handler for one row
#'
#' Creates an [shiny::observeEvent()] that listens for clicks on the delete
#' button for the given row and removes that row from `table_data`. If the
#' deleted row was the last one, a fresh blank row is substituted so the table
#' is never left empty.
#'
#' **Note:** This function closes over `input`, `table_data`, `next_id`, and
#' `col_specs` from the parent [flexible_table_server()] call. It is not a
#' pure function and must be called from within the module server.
#'
#' `ignoreInit = TRUE` is required to prevent the observer from firing
#' immediately on registration, which would cause phantom deletions on startup.
#'
#' @param row_id `numeric(1)`. The unique row identifier whose delete button
#'   this handler is bound to.
#'
#' @return Called for its side effect (registering an observer). Returns the
#'   observer handle invisibly.
#'
#' @seealso [flexible_table_server()]
#'
#' @importFrom shiny observeEvent
#' @importFrom dplyr filter
#'
#' @noRd
register_delete_handler <- function(row_id) {
  observeEvent(
    input[[paste0("delete_", row_id)]],
    {
      remaining <- filter(table_data(), id != row_id)
      if (nrow(remaining) == 0) {
        remaining <- make_row(next_id(), col_specs)
      }
      table_data(remaining)
    },
    ignoreInit = TRUE
  )
}
