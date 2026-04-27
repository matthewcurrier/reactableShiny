# Super simple annotator — UI

Returns a named list of UI elements. The caller is responsible for
placing each element in their layout, allowing the table and button to
be positioned independently (e.g. button in a sidebar, table in main
panel).

## Usage

``` r
annotator_super_simple_ui(id)
```

## Arguments

- id:

  \`character(1)\`. The Shiny module namespace ID. Must match the \`id\`
  passed to \[annotator_super_simple_server()\].

## Value

A named list with two elements:

- \`table\`:

  A \[reactable::reactableOutput()\] placeholder.

- \`reset_button\`:

  An \[shiny::actionButton()\] that clears all selections without
  re-rendering the table.

## See also

\[annotator_super_simple_server()\]
