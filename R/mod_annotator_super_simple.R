# =============================================================================
# mod_annotator_super_simple.R
#
# A lightweight Shiny module for row selection using reactable's built-in
# selection mechanism. Unlike annotator_table, this module has no per-cell
# input columns — the user simply clicks a row (or its checkbox) to select
# it. The module returns a reactive character vector of selected row IDs.
#
# Key design decisions:
#   - Selection state is persisted in a reactiveVal (selected_ids) so it
#     survives table re-renders, e.g. when source_data changes reactively.
#   - defaultSelected maps persisted IDs back to row indices on each render
#     so the visual checkbox state is always consistent with the stored state.
#   - Reset uses updateReactable("table", selected = NA) to clear the JS
#     selection state directly. This fires getReactableState with NULL, which
#     the sync observer handles by clearing selected_ids. This avoids the race
#     condition that would occur if we cleared selected_ids first and then
#     re-rendered — getReactableState would fire with the old indices before
#     the re-render completed and repopulate selected_ids with stale values.
#   - onClick = "select" makes the entire row clickable, not just the checkbox.
#
# UI  : annotator_super_simple_ui(id)
# Server: annotator_super_simple_server(id, source_data, row_id,
#                                       display_cols, ...)
#
# Returns a reactive character vector of selected row IDs.
# =============================================================================

#' Super simple annotator — UI
#'
#' Returns a named list of UI elements. The caller is responsible for placing
#' each element in their layout, allowing the table and button to be
#' positioned independently (e.g. button in a sidebar, table in main panel).
#'
#' @param id `character(1)`. The Shiny module namespace ID. Must match the
#'   `id` passed to [annotator_super_simple_server()].
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`table`}{A [reactable::reactableOutput()] placeholder.}
#'     \item{`reset_button`}{An [shiny::actionButton()] that clears all
#'       selections without re-rendering the table.}
#'   }
#'
#' @seealso [annotator_super_simple_server()]
#'
#' @importFrom shiny NS tagList actionButton
#' @importFrom reactable reactableOutput
#' @importFrom bsicons bs_icon
#'
#' @export
annotator_super_simple_ui <- function(id) {
  ns <- NS(id)

  list(
    table = reactableOutput(ns("table")),
    reset_button = actionButton(
      ns("reset"),
      label = tagList(bsicons::bs_icon("arrow-counterclockwise"), " Clear All"),
      class = "btn-secondary btn-sm"
    )
  )
}


#' Super simple annotator — server
#'
#' Server-side logic for a lightweight row-selection module. Renders a
#' [reactable::reactable()] table where clicking a row (or its checkbox)
#' toggles its selection state. Selection persists across re-renders of the
#' table — if `source_data` changes reactively and the table re-renders,
#' previously selected rows are restored via `defaultSelected`.
#'
#' @param id `character(1)`. The Shiny module namespace ID. Must match the
#'   `id` passed to [annotator_super_simple_ui()].
#'
#' @param source_data `reactive`. A reactive expression returning the source
#'   data frame. Must contain the column named by `row_id` and all columns
#'   named in `display_cols`.
#'
#' @param row_id `character(1)`. The name of the column in `source_data`
#'   that uniquely identifies each row. Values are coerced to character for
#'   stable comparison across re-renders.
#'
#' @param display_cols `character`. A character vector of column names from
#'   `source_data` to show as read-only columns in the table. Column order
#'   matches the order of this vector.
#'
#' @param selection `character(1)`. Either `"multiple"` (default) or
#'   `"single"`. Controls whether the user can select one or many rows.
#'
#' @param reactable_theme A [reactable::reactableTheme()] object applied to
#'   the rendered table. Defaults to `theme_bare`.
#'
#' @param reactable_options `list`. A named list of additional arguments
#'   passed directly to [reactable::reactable()] via [base::do.call()].
#'   Useful for `sortable`, `searchable`, `striped`, `groupBy`, etc.
#'   Do not include `data`, `selection`, `onClick`, `rowStyle`, `columns`,
#'   `defaultSelected`, or `theme` — these are owned by the module and will
#'   cause a duplicate argument error if included here.
#'
#' @return A [shiny::reactive()] returning a character vector of selected
#'   row IDs (values of the `row_id` column for currently selected rows).
#'   Returns `character(0)` when nothing is selected.
#'
#' @seealso [annotator_super_simple_ui()]
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent isolate
#' @importFrom reactable reactable colDef renderReactable getReactableState
#'   updateReactable JS
#' @importFrom purrr map set_names
#'
#' @export
annotator_super_simple_server <- function(
    id,
    source_data,
    row_id,
    display_cols,
    selection = "multiple",
    reactable_theme = theme_bare,
    reactable_options = list()
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Persistent selection state
    #
    # selected_ids stores the row_id values of all selected rows, including
    # rows that may not be currently visible in source_data. This means
    # selections survive reactive source_data changes — if a row disappears
    # due to a filter and reappears, it stays selected.
    # -------------------------------------------------------------------------

    selected_ids <- reactiveVal(character(0))

    # -------------------------------------------------------------------------
    # Reset button
    #
    # Clears selected_ids() unconditionally (visible and hidden rows), then
    # calls updateReactable to clear the JS selection state in the browser.
    #
    # selected_ids() is cleared first and directly rather than relying on the
    # sync observer's NULL branch, because that branch only removes visible-row
    # IDs (via setdiff) — it intentionally preserves hidden-row IDs as part of
    # the normal persistence logic. Reset must bypass that and clear everything.
    #
    # The race condition described in earlier versions of this comment (where
    # clearing selected_ids before a re-render caused stale indices to
    # repopulate it) does not apply here: reset never triggers a re-render, so
    # there is nothing to race against. When the sync observer's NULL branch
    # fires afterward, setdiff(character(0), visible_ids) is a no-op.
    # -------------------------------------------------------------------------

    observeEvent(
      input$reset,
      {
        selected_ids(character(0))
        reactable::updateReactable("table", selected = NA, session = session)
      },
      ignoreInit = TRUE
    )

    # -------------------------------------------------------------------------
    # Sync reactable selection → selected_ids
    #
    # getReactableState returns 1-based indices of selected rows in the
    # currently rendered table. We map these to row_id values and merge with
    # the persisted state, replacing only the visible rows' portion so that
    # selections for currently hidden rows are preserved.
    #
    # This observer also fires when defaultSelected restores selections after
    # a re-render (e.g. source_data changes). In that case the computation is
    # idempotent — the same IDs are already in selected_ids(), so no change
    # is written.
    #
    # It also fires after updateReactable clears selections (reset path).
    # In that case indices is NULL and selected_ids is cleared correctly.
    #
    # ignoreNULL = FALSE: handle NULL (nothing selected) to clear visible-row
    # selections when the user deselects all or after a reset.
    # ignoreInit = TRUE: prevents a spurious sync before the table renders.
    # -------------------------------------------------------------------------

    observeEvent(
      reactable::getReactableState("table", "selected", session),
      {
        indices <- reactable::getReactableState("table", "selected", session)
        data <- isolate(source_data())
        visible_ids <- as.character(data[[row_id]])
        current <- selected_ids()

        if (is.null(indices)) {
          # Nothing selected — remove visible IDs from persisted state but
          # keep selections for rows not currently in source_data
          selected_ids(setdiff(current, visible_ids))
        } else {
          new_ids <- visible_ids[indices]
          # Keep non-visible selections + replace visible portion with current
          updated <- union(setdiff(current, visible_ids), new_ids)
          selected_ids(updated)
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # -------------------------------------------------------------------------
    # Render table
    #
    # Takes a natural reactive dependency on source_data() only — no
    # render_trigger needed since reset is handled by updateReactable.
    #
    # selected_ids() is read via isolate() to compute defaultSelected without
    # registering a reactive dependency — otherwise every selection click would
    # re-render the entire table, causing flicker and losing scroll position.
    # -------------------------------------------------------------------------

    output$table <- renderReactable({
      data <- source_data()
      visible_ids <- as.character(data[[row_id]])
      current_selected <- isolate(selected_ids())

      # Map persisted IDs back to row indices for defaultSelected
      default_selected <- which(visible_ids %in% current_selected)
      if (length(default_selected) == 0) {
        default_selected <- NULL
      }

      # Build render data: synthetic Select column + requested display columns.
      # The Select column carries no data — its checkbox is rendered entirely
      # by the JS cell function using cellInfo.selected from reactable's state.
      render_data <- cbind(
        data.frame(Select = NA_character_, stringsAsFactors = FALSE),
        data[, display_cols, drop = FALSE]
      )

      # Column definitions: hide the internal .selection column, render
      # Select as a checkbox via JS, leave display columns as plain defaults.
      col_defs <- c(
        list(
          .selection = colDef(show = FALSE),
          Select = colDef(
            name = "",
            width = 45,
            html = TRUE,
            cell = reactable::JS(
              "function(cellInfo) {
                 const checked = cellInfo.selected ? 'checked' : '';
                 return '<input type=\"checkbox\" ' + checked +
                        ' aria-label=\"Select row\"' +
                        ' style=\"width:16px;height:16px;cursor:pointer;' +
                        'accent-color:#0d6efd;\">';
               }"
            )
          )
        ),
        purrr::set_names(
          purrr::map(display_cols, ~ colDef()),
          display_cols
        )
      )

      do.call(
        reactable,
        c(
          list(
            data = render_data,
            selection = selection,
            onClick = "select",
            rowStyle = list(cursor = "pointer"),
            columns = col_defs,
            defaultSelected = default_selected,
            theme = reactable_theme
          ),
          reactable_options
        )
      )
    })

    # -------------------------------------------------------------------------
    # Return value
    #
    # A reactive character vector of selected row IDs. Only IDs that are
    # currently present in source_data are returned — IDs for rows that have
    # been filtered out are retained in selected_ids() internally but excluded
    # from the return value so the caller always sees a result consistent with
    # the visible data.
    # -------------------------------------------------------------------------

    reactive({
      ids <- selected_ids()
      data <- source_data()
      visible_ids <- as.character(data[[row_id]])
      intersect(ids, visible_ids)
    })
  })
}
