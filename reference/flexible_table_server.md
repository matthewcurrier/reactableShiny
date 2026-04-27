# Flexible editable table — server

Server-side logic for a Shiny module that renders an editable
\[reactable::reactable()\] table. Each row contains one dropdown per
column, built from the \`col_specs\` you provide. Rows can be added,
deleted, and reset. Columns can optionally depend on each other:
selecting a value in a parent column filters the choices in a child
column and clears any stale selection.

## Usage

``` r
flexible_table_server(
  id,
  col_specs,
  initial_data = NULL,
  reactable_theme = theme_bare,
  duplicates_allowed = TRUE
)
```

## Arguments

- id:

  \`character(1)\`. The Shiny module namespace ID. Must match the \`id\`
  passed to \[flexible_table_ui()\].

- col_specs:

  \`list\`. A list of column specification lists. Each element describes
  one column and may contain:

  \`name\`

  :   \`character(1)\`. Column variable name (used as the data frame
      column name and to construct Shiny input IDs).

  \`label\`

  :   \`character(1)\`. Header label displayed in the table.

  \`choices\`

  :   Named character vector, or named list of named vectors for
      option-group dropdowns.

  \`default\`

  :   \*(optional)\* \`character(1)\`. Pre-selected value for new rows.
      Defaults to \`""\` (blank placeholder).

  \`width\`

  :   \*(optional)\* \`numeric(1)\`. Column width in pixels.

  \`depends_on\`

  :   \*(optional)\* \`character(1)\`. The \`name\` of another column
      whose selected value controls the choices in this column.

  \`get_filtered_choices\`

  :   \*(optional)\* \`function(dependency_value)\`. Called with the
      current value of the \`depends_on\` column; must return a choices
      vector/list in the same format as \`choices\`.

- initial_data:

  \`reactive\` or \`NULL\`. A reactive expression returning a data frame
  used to pre-populate the table. Column names must match the \`name\`
  fields in \`col_specs\`. Re-fires whenever the reactive changes,
  replacing the current table contents. An empty data frame (\`nrow ==
  0\`) is treated as a reset signal. Defaults to \`NULL\` (table starts
  with one blank row).

- reactable_theme:

  A \[reactable::reactableTheme()\] object applied to the rendered
  table. Defaults to \`theme_bare\`, a minimal transparent theme defined
  in \`themes.R\`.

- duplicates_allowed:

  \`logical(1)\`. If \`FALSE\`, duplicate rows are removed from the
  returned reactive data frame. Defaults to \`TRUE\`.

## Value

A \[shiny::reactive()\] that returns a data frame of the current table
contents. Internal helper columns (\`id\`, \`delete\`) are excluded.
Rows where any cell is blank, \`NA\`, or \`NULL\` are also excluded.

## See also

\[flexible_table_ui()\]

## Examples

``` r
if (FALSE) { # \dontrun{
col_specs <- list(
  list(
    name    = "fruit",
    label   = "Fruit",
    choices = c("None" = "", "Apple" = "apple", "Banana" = "banana")
  ),
  list(
    name    = "colour",
    label   = "Colour",
    choices = c("None" = "", "Red" = "red", "Yellow" = "yellow")
  )
)

ui <- bslib::page_fluid(
  do.call(tagList, flexible_table_ui("fruit_picker"))
)

server <- function(input, output, session) {
  selected <- flexible_table_server("fruit_picker", col_specs)

  observe({
    print(selected())  # data frame of complete, non-blank rows
  })
}

shinyApp(ui, server)
} # }
```
