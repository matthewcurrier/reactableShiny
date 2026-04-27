# Annotator table — UI

Returns the UI element for the annotator table module — a single
\[reactable::reactableOutput()\] placeholder. Unlike
\[flexible_table_ui()\], there are no add/reset buttons because rows are
fixed to the source data frame.

## Usage

``` r
annotator_table_ui(id)
```

## Arguments

- id:

  \`character(1)\`. The Shiny module namespace ID. Must match the \`id\`
  passed to \[annotator_table_server()\].

## Value

A \[reactable::reactableOutput()\] UI element.

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
