# =============================================================================
# mod_annotator_enriched.R
#
# A Shiny module that combines row selection (click anywhere on a row) with
# per-row enrichment fields (select, text, date, number). Enrichment inputs
# are always visible but greyed-out and non-interactive for unselected rows;
# selecting a row activates its inputs immediately — no Shiny round-trip and
# no table re-render on every click.
#
# Gating mechanism:
#   Each enrichment input is wrapped in <div class="enrichment-gate">.
#   CSS sets all gates to greyed/non-interactive by default. A JavaScript
#   MutationObserver watches the reactable table for cell re-renders — when
#   reactable re-renders the "Select" checkbox cell (which happens on every
#   row selection or deselection), the observer fires syncGatesForRow() for
#   that row, reading the checkbox's .checked state and toggling opacity and
#   pointer-events on every .enrichment-gate in the same row.
#
#   This approach is robust regardless of how reactable marks selected rows
#   in the DOM (aria-selected, inline styles, CSS classes, etc.), and does
#   not require any external CSS file — the module is fully self-contained.
#
# State model:
#   selected_ids   — reactiveVal(character vector) of selected row IDs.
#                    Mirrors annotator_super_simple exactly; survives
#                    source_data re-renders via defaultSelected.
#   enrichments    — reactiveVal(data.frame) of all enrichment values keyed
#                    by row_id. Values are preserved for rows not currently
#                    visible in source_data (same pattern as annotator_table).
#
# Return value:
#   A reactive data frame containing only the rows that are both currently
#   selected and currently visible in source_data, with their enrichment
#   values. Display columns are excluded.
#
# Dependencies: reactable, bslib, shiny, htmltools, purrr, dplyr
# Also requires: utils_annotator_enriched.R, utils_flexible.R (make_options),
#                themes.R (theme_bare)
# =============================================================================

# Inline JS — MutationObserver that syncs .enrichment-gate state whenever
# reactable re-renders a checkbox cell due to a selection change. Injected
# once per page via htmltools::singleton().
#
# The gate sync is driven by Shiny's own messaging infrastructure rather
# than any client-side DOM observation. When selected_ids() changes on the
# server, an observeEvent sends a custom message listing which row IDs are
# open. The handler below finds every .enrichment-gate by its data-row-id
# attribute and applies the correct inline styles.
#
# This is reliable because:
#   - selected_ids() is already the authoritative source of selection truth.
#   - session$sendCustomMessage() is standard Shiny — no DOM timing involved.
#   - data-row-id is baked into the HTML at render time by R, so the handler
#     never needs to parse input IDs or inspect checkbox state.
.enriched_gate_js <- htmltools::singleton(
  htmltools::tags$head(
    htmltools::tags$script(htmltools::HTML(
      "
      Shiny.addCustomMessageHandler('enrichmentGateSync', function(msg) {
        var container = document.getElementById(msg.tableId);
        if (!container) return;

        /* Build a lookup set of open row IDs for O(1) membership test */
        var openIds = {};
        msg.openIds.forEach(function(id) { openIds[String(id)] = true; });

        container.querySelectorAll('.enrichment-gate').forEach(function(gate) {
          var rowId = gate.dataset.rowId;
          var open  = !!openIds[String(rowId)];
          gate.style.opacity       = open ? '1'    : '0.35';
          gate.style.pointerEvents = open ? 'auto' : 'none';
        });
      });
    "
    ))
  )
)


#' Enriched annotator — UI
#'
#' Returns a named list of UI elements. The caller places each element
#' independently in their layout, so the table and reset button can live in
#' different panels.
#'
#' The module is fully self-contained: the CSS and JavaScript required for
#' enrichment gate gating are embedded inline and injected once per page via
#' [htmltools::singleton()]. No external CSS file is needed.
#'
#' @param id `character(1)`. The Shiny module namespace ID. Must match the
#'   `id` passed to [annotator_enriched_server()].
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`table`}{A [htmltools::tagList()] containing the CSS/JS assets
#'       and a [reactable::reactableOutput()] placeholder.}
#'     \item{`reset_button`}{An [shiny::actionButton()] that clears all
#'       selections without re-rendering the table.}
#'   }
#'
#' @seealso [annotator_enriched_server()]
#'
#' @importFrom shiny NS tagList actionButton
#' @importFrom reactable reactableOutput
#' @importFrom htmltools singleton tags tagList
#' @importFrom bsicons bs_icon
#'
#' @export
annotator_enriched_ui <- function(id) {
  ns <- shiny::NS(id)

  list(
    table = htmltools::tagList(
      .enriched_gate_js,
      reactable::reactableOutput(ns("table"))
    ),
    reset_button = shiny::actionButton(
      ns("reset"),
      label = shiny::tagList(
        bsicons::bs_icon("arrow-counterclockwise"),
        " Clear All"
      ),
      class = "btn-secondary btn-sm"
    )
  )
}


#' Enriched annotator — server
#'
#' Server-side logic for a row-selection module with per-row enrichment
#' fields. Clicking anywhere on a row selects or deselects it. Enrichment
#' inputs (dropdowns, text boxes, date pickers, number inputs) are always
#' rendered but are greyed-out and non-interactive for unselected rows.
#' Values entered in enrichment fields are preserved even if the row is
#' subsequently deselected or if `source_data` changes reactively.
#'
#' @param id `character(1)`. The Shiny module namespace ID.
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
#'   `source_data` to show as read-only columns. Column order follows the
#'   order of this vector.
#'
#' @param enrich_specs `list`. A non-empty list of enrichment column
#'   specifications. Each element is a named list with at minimum:
#'   \describe{
#'     \item{`name`}{`character(1)`. Variable name and Shiny input ID stem.}
#'     \item{`type`}{`character(1)`. One of `"select"`, `"text"`, `"date"`,
#'       `"number"`.}
#'     \item{`label`}{`character(1)`. Column header label.}
#'   }
#'   Additional optional fields by type:
#'   \describe{
#'     \item{`choices` (select)}{Named character vector of options. The name
#'       is the display label; the value is stored. To include a placeholder,
#'       use `c("Select..." = "")` — never `c("" = "")`.}
#'     \item{`placeholder` (text)}{Placeholder string shown in the empty input.}
#'     \item{`min`, `max` (date)}{Strings in `"YYYY-MM-DD"` format.}
#'     \item{`min`, `max` (number)}{Numeric bounds for the number input.}
#'     \item{`width` (all)}{Column width in pixels, forwarded to
#'       [reactable::colDef()].}
#'   }
#'
#' @param selection `character(1)`. Either `"multiple"` (default) or
#'   `"single"`.
#'
#' @param reactable_theme A [reactable::reactableTheme()] applied to the
#'   table. Defaults to `theme_bare`.
#'
#' @param reactable_options `list`. Additional arguments passed to
#'   [reactable::reactable()] via [base::do.call()]. Do not include `data`,
#'   `selection`, `onClick`, `rowStyle`, `columns`, `defaultSelected`,
#'   `elementId`, or `theme` — these are owned by the module.
#'
#' @return A [shiny::reactive()] returning a data frame with the `row_id`
#'   column and all enrichment columns, restricted to rows that are currently
#'   selected and currently visible in `source_data`. Display columns are
#'   not included. Returns a zero-row data frame when nothing is selected.
#'
#' @seealso [annotator_enriched_ui()]
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent observe
#'   isolate
#' @importFrom reactable reactable colDef renderReactable getReactableState
#'   updateReactable JS
#' @importFrom purrr map map_chr set_names walk
#' @importFrom dplyr left_join
#'
#' @export
annotator_enriched_server <- function(
  id,
  source_data,
  row_id,
  display_cols,
  enrich_specs,
  selection = "multiple",
  reactable_theme = theme_bare,
  reactable_options = list()
) {
  if (is.null(enrich_specs) || length(enrich_specs) == 0) {
    stop(
      "`enrich_specs` must be a non-empty list of enrichment column specifications."
    )
  }

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    element_id <- ns("table")

    enrich_names <- purrr::map_chr(enrich_specs, "name")

    # -------------------------------------------------------------------------
    # Reactive state
    # -------------------------------------------------------------------------

    selected_ids <- shiny::reactiveVal(character(0))

    enrichments <- shiny::reactiveVal(
      initial_enrichments(shiny::isolate(source_data()), row_id, enrich_specs)
    )

    # Bumped when source_data changes — causes a full table re-render so new
    # rows appear. Not bumped on selection or enrichment changes.
    reactable_remount_trigger <- shiny::reactiveVal(0)

    # -------------------------------------------------------------------------
    # Respond to source_data changes
    #
    # merge_enrichments preserves all existing values, assigns defaults to new
    # rows, and restricts the result to the current source_data. Rows that
    # leave source_data silently remain inside the enrichments reactiveVal
    # so they are restored if those rows reappear.
    # -------------------------------------------------------------------------

    shiny::observeEvent(source_data(), {
      enrichments(
        merge_enrichments(source_data(), row_id, enrich_specs, enrichments())
      )
      reactable_remount_trigger(reactable_remount_trigger() + 1)
    })

    # -------------------------------------------------------------------------
    # Reset button
    #
    # updateReactable clears the JS selection state directly, which fires
    # getReactableState with NULL. The sync observer below handles NULL by
    # clearing selected_ids. This avoids the race condition that would arise
    # if we cleared selected_ids first and then re-rendered.
    # -------------------------------------------------------------------------

    shiny::observeEvent(
      input$reset,
      {
        reactable::updateReactable("table", selected = NA, session = session)
      },
      ignoreInit = TRUE
    )

    # -------------------------------------------------------------------------
    # Sync reactable selection → selected_ids
    #
    # getReactableState returns 1-based row indices into the currently rendered
    # table. We map them to row_id values and merge with the persisted state,
    # preserving selections for rows not currently visible in source_data.
    #
    # ignoreNULL = FALSE: handle NULL (nothing selected) so deselecting all
    # rows correctly clears the visible portion of selected_ids.
    # ignoreInit = TRUE: prevents a spurious sync before the table renders.
    # -------------------------------------------------------------------------

    shiny::observeEvent(
      reactable::getReactableState("table", "selected", session),
      {
        indices <- reactable::getReactableState("table", "selected", session)
        data <- shiny::isolate(source_data())
        visible_ids <- as.character(data[[row_id]])
        current <- selected_ids()

        if (is.null(indices)) {
          selected_ids(setdiff(current, visible_ids))
        } else {
          new_ids <- visible_ids[indices]
          updated <- union(setdiff(current, visible_ids), new_ids)
          selected_ids(updated)
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # -------------------------------------------------------------------------
    # Push gate state to the client whenever selection changes
    #
    # selected_ids() is the authoritative source of selection truth. Whenever
    # it changes we send a custom message listing the open row IDs. The JS
    # handler in .enriched_gate_js uses data-row-id attributes (baked into
    # the HTML by R at render time) to find the correct gate divs and apply
    # inline styles — no DOM timing or React internals involved.
    #
    # ignoreInit = FALSE so gates are synced on first render (handles the
    # case where defaultSelected pre-selects rows).
    # -------------------------------------------------------------------------

    shiny::observeEvent(
      selected_ids(),
      {
        session$sendCustomMessage(
          "enrichmentGateSync",
          list(
            tableId = element_id,
            openIds = as.list(selected_ids())
          )
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )

    # -------------------------------------------------------------------------
    # Sync enrichment inputs → enrichments
    #
    # Walks every visible row and every enrich_spec, comparing the current
    # Shiny input value against the stored enrichment. Writes back only when
    # something has changed to avoid unnecessary reactive cycles.
    #
    # This observer fires on every user interaction with any enrichment input.
    # Writing to enrichments() does NOT trigger a table re-render because
    # renderReactable reads enrichments() via isolate() — the table only
    # re-renders when reactable_remount_trigger() changes.
    # -------------------------------------------------------------------------

    shiny::observe({
      data <- source_data()
      current <- enrichments()
      updated <- current

      purrr::walk(seq_len(nrow(data)), function(i) {
        id_value <- as.character(data[[row_id]][i])
        ann_row <- which(updated[[row_id]] == id_value)

        if (length(ann_row) == 0) {
          return(NULL)
        }

        purrr::walk(enrich_specs, function(spec) {
          input_id <- paste0(spec$name, "_", id_value)
          val <- input[[input_id]]

          if (!is.null(val) && !identical(val, updated[[spec$name]][ann_row])) {
            updated[[spec$name]][ann_row] <<- val
          }
        })
      })

      if (!identical(updated, current)) {
        enrichments(updated)
      }
    })

    # -------------------------------------------------------------------------
    # Render table
    #
    # Takes a reactive dependency on source_data() via reactable_remount_trigger()
    # only — selection and enrichment changes do not trigger a re-render.
    #
    # selected_ids() and enrichments() are both read via isolate() to avoid
    # registering reactive dependencies that would cause unwanted re-renders.
    #
    # render_data structure:
    #   - Select    : NA_character_ placeholder (checkbox rendered by JS)
    #   - display_cols: read-only values from source_data()
    #   - enrich_names: current enrichment values (from snapshot)
    #
    # The enrichment snapshot (enr) is passed to make_enriched_col_def() so
    # cell functions close over a stable value rather than a live reactive.
    # -------------------------------------------------------------------------

    output$table <- reactable::renderReactable({
      force(reactable_remount_trigger())

      data <- source_data()
      visible_ids <- as.character(data[[row_id]])

      # Restore visual checkbox state after a re-render
      current_selected <- shiny::isolate(selected_ids())
      default_selected <- which(visible_ids %in% current_selected)
      if (length(default_selected) == 0) {
        default_selected <- NULL
      }

      # Frozen enrichment snapshot for this render
      enr <- shiny::isolate(enrichments())
      enr_ordered <- enr[
        match(visible_ids, enr[[row_id]]),
        enrich_names,
        drop = FALSE
      ]

      render_data <- cbind(
        data.frame(Select = NA_character_, stringsAsFactors = FALSE),
        data[, display_cols, drop = FALSE],
        enr_ordered
      )

      # Column definitions
      display_col_defs <- purrr::set_names(
        purrr::map(display_cols, ~ reactable::colDef()),
        display_cols
      )

      enrich_col_defs <- purrr::set_names(
        purrr::map(enrich_specs, make_enriched_col_def, enr, row_id, ns),
        enrich_names
      )

      col_defs <- c(
        list(
          .selection = reactable::colDef(show = FALSE),
          Select = reactable::colDef(
            name = "",
            width = 45,
            html = TRUE,
            cell = reactable::JS(
              "function(cellInfo) {
                 var checked = cellInfo.selected ? 'checked' : '';
                 return '<input type=\"checkbox\" ' + checked +
                        ' aria-label=\"Select row\"' +
                        ' style=\"width:16px;height:16px;cursor:pointer;' +
                        'accent-color:#0d6efd;\">';
               }"
            )
          )
        ),
        display_col_defs,
        enrich_col_defs
      )

      do.call(
        reactable::reactable,
        c(
          list(
            data = render_data,
            selection = selection,
            onClick = "select",
            rowStyle = list(cursor = "pointer"),
            columns = col_defs,
            defaultSelected = default_selected,
            elementId = element_id,
            theme = reactable_theme
          ),
          reactable_options
        )
      )
    })

    # -------------------------------------------------------------------------
    # Return value
    #
    # Filters enrichments() to rows that are both currently selected and
    # currently visible in source_data. IDs for rows that have been filtered
    # out of source_data are retained in selected_ids() internally but are
    # excluded here so the caller always receives a result consistent with the
    # visible data.
    # -------------------------------------------------------------------------

    shiny::reactive({
      sel_ids <- selected_ids()
      enr <- enrichments()
      data <- source_data()
      visible_ids <- as.character(data[[row_id]])
      visible_selected <- intersect(sel_ids, visible_ids)

      if (length(visible_selected) == 0) {
        return(enr[0L, ])
      }

      enr[enr[[row_id]] %in% visible_selected, ]
    })
  })
}
