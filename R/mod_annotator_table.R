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
#' Returns the UI elements for the annotator table module: a
#' [reactable::reactableOutput()] placeholder plus a JavaScript snippet
#' that initialises selectize.js on any `"selectize"` columns after the
#' table mounts. The script is idempotent — including multiple annotator
#' tables on the same page does not duplicate event handlers.
#'
#' @param id `character(1)`. The Shiny module namespace ID. Must match the
#'   `id` passed to [annotator_table_server()].
#'
#' @return A [shiny::tagList()] containing the reactable output and the
#'   selectize initialiser script.
#'
#' @seealso [annotator_table_server()]
#'
#' @importFrom reactable reactableOutput
#' @importFrom shiny NS tagList tags selectizeInput
#' @importFrom htmltools findDependencies attachDependencies
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
  # Extract selectize.js dependency from Shiny's bundled selectizeInput.
  # This ensures the selectize library (JS + CSS) is loaded on the page
  # without rendering a visible input element.
  selectize_deps <- findDependencies(
    selectizeInput("__ann_dep__", label = NULL, choices = NULL)
  )

  ui <- tagList(
    reactableOutput(shiny::NS(id, "table")),
    tags$script(shiny::HTML(
      "
      (function() {
        if (window._annSelectizeInitBound) return;
        window._annSelectizeInitBound = true;

        // Global store for pending async search callbacks
        window._annSelectizePending = {};

        // Register message handler immediately — Shiny JS is loaded
        // synchronously in <head>, so it is available by the time
        // inline body scripts execute.
        function registerResultsHandler() {
          if (window._annSelectizeHandlerReady) return;
          if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) {
            console.warn('[selectize] Shiny not ready — retrying in 200ms');
            setTimeout(registerResultsHandler, 200);
            return;
          }
          window._annSelectizeHandlerReady = true;
          console.log('[selectize] Registering message handler');
          Shiny.addCustomMessageHandler(
            'ann-selectize-results',
            function(msg) {
              console.log('[selectize] Received results for',
                          msg.request_id, ':',
                          msg.results ? msg.results.length : 0, 'items');
              var cb = window._annSelectizePending[msg.request_id];
              if (cb) {
                cb(msg.results);
                delete window._annSelectizePending[msg.request_id];
              } else {
                console.warn('[selectize] No pending callback for',
                             msg.request_id);
              }
            }
          );
        }
        registerResultsHandler();

        function initAnnSelectize() {
          var els = document.querySelectorAll(
            'select.ann-selectize-create:not(.selectized)'
          );
          if (els.length === 0) return;
          console.log('[selectize] Initialising',
                      els.length, 'selectize inputs');

          els.forEach(function(el) {
            var inputId = el.id;
            var placeholder =
              el.getAttribute('data-placeholder') || 'Select or type...';
            var isServerSearch =
              el.getAttribute('data-server-search') === 'true';

            console.log('[selectize]', inputId,
                        '| server-search:', isServerSearch);

            var config = {
              create: true,
              createOnBlur: true,
              placeholder: placeholder,
              onChange: function(value) {
                Shiny.setInputValue(
                  inputId, value || '', {priority: 'event'}
                );
              }
            };

            if (isServerSearch) {
              var searchInputId =
                el.getAttribute('data-search-input-id') || '';
              var columnName =
                el.getAttribute('data-column-name') || '';
              var minChars =
                parseInt(el.getAttribute('data-min-chars') || '2', 10);

              console.log('[selectize]', inputId,
                          '| searchInputId:', searchInputId,
                          '| column:', columnName,
                          '| minChars:', minChars);

              config.valueField  = 'value';
              config.labelField  = 'label';
              config.searchField = ['label'];
              config.loadThrottle = 300;

              config.load = function(query, callback) {
                if (query.length < minChars) return callback();
                var requestId = inputId + '_' + Date.now();
                console.log('[selectize] Searching:', query,
                            '| requestId:', requestId);
                window._annSelectizePending[requestId] = callback;
                Shiny.setInputValue(searchInputId, {
                  query: query,
                  column: columnName,
                  request_id: requestId
                }, {priority: 'event'});
              };
            }

            $(el).selectize(config);

            // Move the dropdown to <body> so it escapes any
            // overflow: hidden set by reactable on ancestor elements.
            // Then override positionDropdown to use document-relative
            // coordinates since the dropdown is now a child of <body>.
            var ctrl = el.selectize;
            if (ctrl && ctrl.$dropdown) {
              ctrl.$dropdown.detach().appendTo(document.body);
              ctrl.positionDropdown = function() {
                var $c = this.$control;
                var offset = $c.offset();
                this.$dropdown.css({
                  position : 'absolute',
                  top      : offset.top + $c.outerHeight(true),
                  left     : offset.left,
                  width    : $c.outerWidth()
                });
              };
            }
          });
        }

        $(document).on('shiny:value', function() {
          setTimeout(initAnnSelectize, 50);
        });
      })();
    "
    ))
  )

  attachDependencies(ui, selectize_deps, append = TRUE)
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
#'     \item{`"selectize"`}{Searchable dropdown with free-create via
#'       selectize.js. Requires `choices`: a named character vector. Do
#'       **not** include a blank placeholder in `choices`; instead provide
#'       an optional `placeholder` string (defaults to
#'       `"Select or type..."`). User-created values are row-local — they
#'       are not propagated to other rows and reset when `source_data`
#'       changes.}
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
#'   list(name = "notes",     type = "text",     label = "Notes"),
#'   list(name = "tag",       type = "selectize", label = "Tag",
#'        choices = c("Economy" = "economy", "Luxury" = "luxury"),
#'        placeholder = "Pick or create...")
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

    # -------------------------------------------------------------------------
    # Server-side search for selectize columns.
    #
    # When a selectize col_spec includes a `server_search` function, the JS
    # initialiser configures an async `load` callback. Each keystroke (after
    # the min_chars threshold) sends a search request here via
    # Shiny.setInputValue('selectize_search', {query, column, request_id}).
    #
    # The observer calls the user-provided `server_search(query)` function,
    # normalises the result to a list of list(value=, label=) records, and
    # sends them back via session$sendCustomMessage('ann-selectize-results').
    # The JS handler routes the response to the correct selectize instance
    # by matching request_id.
    # -------------------------------------------------------------------------

    server_search_specs <- keep(input_specs, ~ !is.null(.x$server_search))

    if (length(server_search_specs) > 0) {
      message(
        "[selectize] Server search enabled for columns: ",
        paste(map_chr(server_search_specs, "name"), collapse = ", ")
      )

      observeEvent(input$selectize_search, {
        req <- input$selectize_search
        message(
          "[selectize] Search request — column: ",
          req$column,
          " | query: '",
          req$query,
          "'",
          " | request_id: ",
          req$request_id
        )

        spec <- purrr::detect(
          server_search_specs,
          ~ .x$name == req$column
        )
        if (is.null(spec)) {
          message("[selectize] No matching spec for column: ", req$column)
          return()
        }

        raw <- spec$server_search(req$query)
        message("[selectize] server_search returned ", nrow(raw), " rows")

        # Normalise to a list of list(value, label) for JSON serialisation.
        # Accepts either a data.frame with value + label columns or a named
        # character vector (names = labels, values = values).
        results <- if (is.data.frame(raw)) {
          purrr::pmap(
            raw[, c("value", "label"), drop = FALSE],
            function(value, label, ...) list(value = value, label = label)
          )
        } else {
          purrr::imap(raw, function(val, lab) {
            list(value = val, label = lab)
          }) |>
            unname()
        }

        message(
          "[selectize] Sending ",
          length(results),
          " results for request_id: ",
          req$request_id
        )
        session$sendCustomMessage(
          "ann-selectize-results",
          list(
            request_id = req$request_id,
            results = results
          )
        )
      })
    }

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
