# Enriched annotator — server

Server-side logic for a row-selection module with per-row enrichment
fields. Clicking anywhere on a row selects or deselects it. Enrichment
inputs (dropdowns, text boxes, date pickers, number inputs) are always
rendered but are greyed-out and non-interactive for unselected rows.
Values entered in enrichment fields are preserved even if the row is
subsequently deselected or if \`source_data\` changes reactively.

## Usage

``` r
annotator_enriched_server(
  id,
  source_data,
  row_id,
  display_cols,
  enrich_specs,
  initial_enrichments = NULL,
  initial_selected = character(0),
  reset_to = NULL,
  selection = "multiple",
  reactable_theme = theme_bare,
  reactable_options = list()
)
```

## Arguments

- id:

  \`character(1)\`. The Shiny module namespace ID.

- source_data:

  \`reactive\`. A reactive expression returning the source data frame.
  Must contain the column named by \`row_id\` and all columns named in
  \`display_cols\`.

- row_id:

  \`character(1)\`. The name of the column in \`source_data\` that
  uniquely identifies each row. Values are coerced to character for
  stable comparison across re-renders.

- display_cols:

  \`character\`. A character vector of column names from \`source_data\`
  to show as read-only columns. Column order follows the order of this
  vector.

- enrich_specs:

  \`list\`. A non-empty list of enrichment column specifications. Each
  element is a named list with at minimum:

  \`name\`

  :   \`character(1)\`. Variable name and Shiny input ID stem.

  \`type\`

  :   \`character(1)\`. One of \`"select"\`, \`"text"\`, \`"date"\`,
      \`"number"\`.

  \`label\`

  :   \`character(1)\`. Column header label.

  Additional optional fields by type:

  \`choices\` (select)

  :   Named character vector of options. The name is the display label;
      the value is stored. To include a placeholder, use \`c("Select..."
      = "")\` — never \`c("" = "")\`.

  \`placeholder\` (text)

  :   Placeholder string shown in the empty input.

  \`min\`, \`max\` (date)

  :   Strings in \`"YYYY-MM-DD"\` format.

  \`min\`, \`max\` (number)

  :   Numeric bounds for the number input.

  \`width\` (all)

  :   Column width in pixels, forwarded to \[reactable::colDef()\].

- initial_enrichments:

  \`data.frame\` or \`NULL\`. A pre-populated enrichments frame to seed
  the module's internal state on first render. Must have the same
  structure as the frame produced by \[initial_enrichments_blank()\]: a
  \`row_id\` column (character) followed by one column per enrichment
  spec. Rows absent from \`source_data\` are silently ignored; rows in
  \`source_data\` but absent here receive their untouched defaults. Pass
  \`NULL\` (the default) to start with a fully blank state.

- initial_selected:

  \`character\`. A character vector of \`row_id\` values that should
  appear as selected on first render. Defaults to \`character(0)\`
  (nothing pre-selected). Values not present in \`source_data\` are
  silently dropped. Used to restore checkbox state when re-opening an
  existing report.

- reset_to:

  \`reactive\` or \`NULL\`. An optional reactive expression returning a
  named list with two elements:

  \`selected\`

  :   A character vector of \`row_id\` values to restore as the new
      selection. Pass \`character(0)\` or \`NULL\` to clear all
      selections.

  \`enrichments\`

  :   A data frame of pre-populated enrichment values in the same
      structure as \`initial_enrichments\`, or \`NULL\` to start with a
      fully blank enrichment frame.

  When this reactive fires, the module replaces its internal
  \`selected_ids\` and \`enrichments\` state with the supplied seeds and
  forces a full table re-render. This is the correct mechanism for
  resetting the module when the same Shiny session switches between
  different data contexts (e.g. opening Report B after Report A) without
  a page refresh. Defaults to \`NULL\` (no mid-session reset). The
  recommended calling pattern is to pass a \`reactiveVal(NULL)\` that
  starts as \`NULL\` and is only set when a genuine context switch
  occurs:

        reset_signal <- shiny::reactiveVal(NULL)
        shiny::observeEvent(data_r(), {
          reset_signal(list(
            selected    = build_initial_selected(data_r()),
            enrichments = build_initial_enrichments(data_r())
          ))
        }, ignoreInit = TRUE)

- selection:

  \`character(1)\`. Either \`"multiple"\` (default) or \`"single"\`.

- reactable_theme:

  A \[reactable::reactableTheme()\] applied to the table. Defaults to
  \`theme_bare\`.

- reactable_options:

  \`list\`. Additional arguments passed to \[reactable::reactable()\]
  via \[base::do.call()\]. Do not include \`data\`, \`selection\`,
  \`onClick\`, \`rowStyle\`, \`columns\`, \`defaultSelected\`,
  \`elementId\`, or \`theme\` — these are owned by the module.

## Value

A \[shiny::reactive()\] returning a data frame with the \`row_id\`
column and all enrichment columns, restricted to rows that are currently
selected and currently visible in \`source_data\`. Display columns are
not included. Returns a zero-row data frame when nothing is selected.

## See also

\[annotator_enriched_ui()\]
