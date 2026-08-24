# Annotator table — UI

Returns the UI elements for the annotator table module: a
\[reactable::reactableOutput()\] placeholder plus a JavaScript snippet
that initialises selectize.js on any \`"selectize"\` columns after the
table mounts. The script is idempotent — including multiple annotator
tables on the same page does not duplicate event handlers.

## Usage

``` r
annotator_table_ui(id)
```

## Arguments

- id:

  \`character(1)\`. The Shiny module namespace ID. Must match the \`id\`
  passed to \[annotator_table_server()\].

## Value

A \[shiny::tagList()\] containing the reactable output and the selectize
initialiser script.

## See also

\[annotator_table_server()\]

## Examples

``` r
if (FALSE) { # \dontrun{
ui <- bslib::page_fluid(
  annotator_table_ui("my_annotator")
)
} # }
```
