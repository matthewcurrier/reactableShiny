# Flexible editable table — UI

Returns a named list of UI elements for the flexible table module. The
caller is responsible for placing each element in their layout.
Separating the elements this way lets the caller position the table and
buttons independently (e.g. buttons in a sidebar, table in the main
panel).

## Usage

``` r
flexible_table_ui(id)
```

## Arguments

- id:

  \`character(1)\`. The Shiny module namespace ID. Must match the \`id\`
  passed to \[flexible_table_server()\].

## Value

A named list with three elements:

- \`table\`:

  A \[reactable::reactableOutput()\] placeholder.

- \`add_row_button\`:

  An \[shiny::actionButton()\] that appends a blank row.

- \`reset_button\`:

  An \[shiny::actionButton()\] that clears the table to a single blank
  row.

## See also

\[flexible_table_server()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# In your UI definition:
ui <- bslib::page_fluid(
  flexible_table_ui("fruit_picker")$add_row_button,
  flexible_table_ui("fruit_picker")$reset_button,
  flexible_table_ui("fruit_picker")$table
)
} # }
```
