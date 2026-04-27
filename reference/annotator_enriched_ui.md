# Enriched annotator — UI

Returns a named list of UI elements. The caller places each element
independently in their layout, so the table and reset button can live in
different panels.

## Usage

``` r
annotator_enriched_ui(id)
```

## Arguments

- id:

  \`character(1)\`. The Shiny module namespace ID. Must match the \`id\`
  passed to \[annotator_enriched_server()\].

## Value

A named list with two elements:

- \`table\`:

  A \[htmltools::tagList()\] containing the JS asset and a
  \[reactable::reactableOutput()\] placeholder.

- \`reset_button\`:

  An \[shiny::actionButton()\] that clears all selections without
  re-rendering the table.

## Details

The module is fully self-contained: the JavaScript required for live
gate toggling is embedded inline and injected once per page via
\[htmltools::singleton()\]. No external CSS file is needed.

## See also

\[annotator_enriched_server()\]
