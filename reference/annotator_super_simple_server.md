# Super simple annotator — server

Server-side logic for a lightweight row-selection module. Renders a
\[reactable::reactable()\] table where clicking a row (or its checkbox)
toggles its selection state. Selection persists across re-renders of the
table — if \`source_data\` changes reactively and the table re-renders,
previously selected rows are restored via \`defaultSelected\`.

## Usage

``` r
annotator_super_simple_server(
  id,
  source_data,
  row_id,
  display_cols,
  selection = "multiple",
  reactable_theme = theme_bare,
  reactable_options = list()
)
```

## Arguments

- id:

  \`character(1)\`. The Shiny module namespace ID. Must match the \`id\`
  passed to \[annotator_super_simple_ui()\].

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
  to show as read-only columns in the table. Column order matches the
  order of this vector.

- selection:

  \`character(1)\`. Either \`"multiple"\` (default) or \`"single"\`.
  Controls whether the user can select one or many rows.

- reactable_theme:

  A \[reactable::reactableTheme()\] object applied to the rendered
  table. Defaults to \`theme_bare\`.

- reactable_options:

  \`list\`. A named list of additional arguments passed directly to
  \[reactable::reactable()\] via \[base::do.call()\]. Useful for
  \`sortable\`, \`searchable\`, \`striped\`, \`groupBy\`, etc. Do not
  include \`data\`, \`selection\`, \`onClick\`, \`rowStyle\`,
  \`columns\`, \`defaultSelected\`, or \`theme\` — these are owned by
  the module and will cause a duplicate argument error if included here.

## Value

A \[shiny::reactive()\] returning a character vector of selected row IDs
(values of the \`row_id\` column for currently selected rows). Returns
\`character(0)\` when nothing is selected.

## See also

\[annotator_super_simple_ui()\]
