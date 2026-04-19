# =============================================================================
# milestone_tracker module
#
# Two-card layout:
#   Card 1 — grouped, selectable reactable for picking items from a dataset.
#   Card 2 — annotator_table showing only the selected items, with
#             user-defined input columns (e.g. dropdowns, checkboxes).
#
# Card 1 uses a flat reactable with groupBy + native selection so that
# getReactableState() works reliably — nested sub-table reactables cannot be
# queried by getReactableState() in Shiny.
#
# Dependencies: reactable, bslib, shiny, htmltools, purrr, dplyr
# Also requires: utils_annotator.R, mod_annotator_table.R
# =============================================================================

#' Milestone Tracker — UI
#'
#' Returns a two-column bslib card layout. Card 1 holds the grouped selection
#' table (with a per-group selection summary above it); Card 2 holds the
#' annotation table (or an empty-state prompt).
#'
#' @param id `character(1)`. Shiny module namespace ID.
#'
#' @return A [bslib::layout_columns()] UI element.
#' @importFrom shiny textOutput uiOutput
#' @export
milestone_tracker_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header(textOutput(ns("card1_title"), inline = TRUE)),
      uiOutput(ns("selection_summary")),
      reactableOutput(ns("selection_table"))
    ),
    bslib::card(
      bslib::card_header(textOutput(ns("card2_title"), inline = TRUE)),
      uiOutput(ns("annotation_area"))
    )
  )
}


#' Milestone Tracker — Server
#'
#' @param id `character(1)`. Shiny module namespace ID.
#'
#' @param data `reactive`. A reactive returning the source data frame. Must
#'   contain `row_id`, `group_col`, `item_col`, and optionally `badge_col`.
#'
#' @param row_id `character(1)`. Name of the unique ID column in `data`
#'   (e.g. `"id"`).
#'
#' @param group_col `character(1)`. Column to group rows by in Card 1
#'   (e.g. `"age"`). Clicking a group header expands its rows.
#'
#' @param group_sort_col `character(1)` or `NULL`. Column used to sort the
#'   groups (e.g. `"age_months"`). When `NULL`, groups sort by `group_col`.
#'
#' @param item_col `character(1)`. Column containing the item text shown in
#'   each row (e.g. `"milestone"`).
#'
#' @param badge_col `character(1)` or `NULL`. Column rendered as a coloured
#'   badge in Card 1 (e.g. `"category"`). `NULL` disables badge rendering.
#'
#' @param badge_colors Named list or `NULL`. Maps badge values to
#'   `list(bg = "#hex", text = "#hex")` colour specs. Required when
#'   `badge_col` is not `NULL`.
#'
#' @param annotation_col_specs `list`. Full col_specs list passed to
#'   [annotator_table_server()] for Card 2. Include display col_specs for
#'   columns you want to show from the selected data, followed by input
#'   col_specs for annotation fields (select, checkbox, text, etc.).
#'
#' @param card1_title `character(1)`. Header text for Card 1.
#' @param card2_title `character(1)`. Header text for Card 2.
#' @param group_label `character(1)` or `NULL`. Display label for the group
#'   column. Defaults to `group_col`.
#' @param item_label `character(1)` or `NULL`. Display label for the item
#'   column. Defaults to `item_col`.
#' @param count_label `character(1)`. Suffix used in the aggregate row count
#'   (e.g. `"milestones"` → `"5 milestones"`).
#'
#' @return A [shiny::reactive()] returning a data frame of row IDs and
#'   annotation values for rows where at least one input has been touched.
#'   Passes through the return value of [annotator_table_server()].
#'
#' @importFrom shiny moduleServer NS reactive renderText renderUI debounce
#' @importFrom reactable reactable colDef reactableTheme renderReactable
#'   reactableOutput getReactableState JS
#' @importFrom htmltools tags
#' @importFrom shiny textOutput uiOutput
#' @importFrom bslib layout_columns card card_header
#'
#' @export
milestone_tracker_server <- function(
  id,
  data,
  row_id,
  group_col,
  group_sort_col = NULL,
  item_col,
  badge_col = NULL,
  badge_colors = NULL,
  annotation_col_specs,
  card1_title = "Select Items",
  card2_title = "Annotations",
  group_label = NULL,
  item_label = NULL,
  count_label = "items"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$card1_title <- renderText(card1_title)
    output$card2_title <- renderText(card2_title)

    # -----------------------------------------------------------------------
    # Helpers
    # -----------------------------------------------------------------------

    `%||%` <- function(x, y) if (is.null(x)) y else x

    make_badge <- if (!is.null(badge_col) && !is.null(badge_colors)) {
      function(value) {
        color <- badge_colors[[value]] %||%
          list(bg = "#F1EFE8", text = "#444441")
        tags$span(
          style = paste0(
            "background:",
            color$bg,
            ";",
            "color:",
            color$text,
            ";",
            "padding: 2px 8px;",
            "border-radius: 4px;",
            "font-size: 12px;",
            "font-weight: 500;"
          ),
          value
        )
      }
    } else {
      NULL
    }

    # -----------------------------------------------------------------------
    # Prepare display data for Card 1
    #
    # Keeps only the columns Card 1 needs, deduplicates on row_id, and
    # sorts by group_sort_col so groups appear in the intended order.
    # -----------------------------------------------------------------------

    display_data <- reactive({
      df <- data()
      sc <- group_sort_col %||% group_col

      keep <- unique(c(row_id, sc, group_col, badge_col, item_col))
      keep <- keep[keep %in% names(df)]

      out <- df[, keep, drop = FALSE]
      out <- out[!duplicated(out[[row_id]]), ]
      out <- out[order(out[[sc]]), ]
      out
    })

    # -----------------------------------------------------------------------
    # Card 1: grouped selection table
    # -----------------------------------------------------------------------

    output$selection_table <- renderReactable({
      df <- display_data()

      col_defs <- list()

      col_defs[[".selection"]] <- colDef(
        name = "",
        width = 45,
        align = "center"
      )

      col_defs[[row_id]] <- colDef(show = FALSE)

      if (!is.null(group_sort_col) && group_sort_col != group_col) {
        col_defs[[group_sort_col]] <- colDef(show = FALSE)
      }

      col_defs[[group_col]] <- colDef(
        name = group_label %||% group_col,
        minWidth = 130,
        aggregate = "unique"
      )

      if (!is.null(badge_col)) {
        col_defs[[badge_col]] <- colDef(
          name = badge_col,
          minWidth = 140,
          cell = make_badge,
          aggregate = JS(
            "function(values) { return [...new Set(values)].join(', '); }"
          )
        )
      }

      col_defs[[item_col]] <- colDef(
        name = item_label %||% item_col,
        minWidth = 300,
        aggregate = JS(
          sprintf(
            "function(values) { return values.length + ' %s'; }",
            count_label
          )
        )
      )

      reactable(
        df,
        groupBy = group_col,
        selection = "multiple",
        onClick = "select",
        columns = col_defs,
        sortable = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        defaultPageSize = 20,
        theme = reactableTheme(
          headerStyle = list(fontWeight = 500),
          rowSelectedStyle = list(
            backgroundColor = "#EAF3DE",
            boxShadow = "inset 2px 0 0 0 #1D9E75"
          ),
          rowStyle = list(cursor = "pointer")
        )
      )
    })

    # -----------------------------------------------------------------------
    # Derive selected data from Card 1
    #
    # getReactableState() returns 1-based row indices into display_data().
    # Debounced to prevent rapid re-renders of the annotator table when
    # selection events fire in quick succession.
    # -----------------------------------------------------------------------

    selected_data_raw <- reactive({
      sel <- getReactableState("selection_table", "selected", session = session)
      df <- display_data()
      if (is.null(sel) || length(sel) == 0) {
        return(df[0L, ])
      }
      result <- df[sel, , drop = FALSE]
      result[!duplicated(result[[row_id]]), , drop = FALSE]
    })

    selected_data <- selected_data_raw |> debounce(300)

    # -----------------------------------------------------------------------
    # Card 1 summary: per-group selection counts shown above the table
    # -----------------------------------------------------------------------

    output$selection_summary <- renderUI({
      sel <- selected_data()
      if (nrow(sel) == 0) {
        return(NULL)
      }

      counts <- sort(table(sel[[group_col]]))
      badges <- lapply(names(counts), function(g) {
        tags$span(
          style = paste0(
            "display:inline-block; margin:2px 4px;",
            "background:#EAF3DE; color:#27500A;",
            "padding:2px 8px; border-radius:4px;",
            "font-size:12px; font-weight:500;"
          ),
          paste0(g, ": ", counts[[g]], " selected")
        )
      })

      tags$div(style = "padding:4px 8px 8px 8px;", badges)
    })

    # -----------------------------------------------------------------------
    # Card 2: annotation table or empty-state prompt
    #
    # annotator_table_server is always initialised (it handles zero-row
    # source_data gracefully). The UI swaps between an empty-state message
    # and the actual table output based on selection count.
    # -----------------------------------------------------------------------

    output$annotation_area <- renderUI({
      if (nrow(selected_data()) == 0L) {
        tags$div(
          style = paste0(
            "padding: 2rem;",
            "text-align: center;",
            "color: #6c757d;",
            "font-size: 14px;"
          ),
          "Select items from the left panel to begin annotating."
        )
      } else {
        annotator_table_ui(ns("annotator"))
      }
    })

    annotations <- annotator_table_server(
      id = "annotator",
      source_data = selected_data,
      row_id = row_id,
      col_specs = annotation_col_specs
    )

    annotations
  })
}
