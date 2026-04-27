# Milestone Tracker — UI

Returns a two-column bslib card layout. Card 1 holds the grouped
selection table (with a per-group selection summary above it); Card 2
holds the annotation table (or an empty-state prompt).

## Usage

``` r
milestone_tracker_ui(id, col_widths = c(6, 6))
```

## Arguments

- id:

  \`character(1)\`. Shiny module namespace ID.

- col_widths:

  \`numeric(2)\`. Bootstrap column widths for the two cards, passed
  directly to \[bslib::layout_columns()\]. Defaults to \`c(6, 6)\` for
  an equal split. Use e.g. \`c(4, 8)\` when Card 2 has many columns.

## Value

A \[bslib::layout_columns()\] UI element.
