# Create a badge cell renderer for reactable column definitions

Returns a \`function(value)\` suitable for use as the \`cell\` argument
of \[reactable::colDef()\], or as \`cell\` inside a \`col_def_options\`
list passed to \[annotator_table_server()\].

## Usage

``` r
make_badge_renderer(
  badge_colors,
  fallback = list(bg = "#F1EFE8", text = "#444441")
)
```

## Arguments

- badge_colors:

  Named list. Each name is a possible cell value; each element is a
  \`list(bg = "#hex", text = "#hex")\` colour spec.

- fallback:

  \`list\`. Colour spec used when a value is absent from
  \`badge_colors\`. Defaults to \`list(bg = "#F1EFE8", text =
  "#444441")\`.

## Value

A \`function(value)\` that returns a \[shiny::tags\]\`\$span\` tag.

## Details

The returned function wraps each cell value in a \`\<span\>\` with the
\`.rs-badge\` CSS class (for layout constants) and inline \`background\`
/ \`color\` styles drawn from \`badge_colors\`. Separating structural
styles (padding, border-radius, font-size) into CSS from dynamic colours
(kept inline) means callers can restyle badges via \`custom.css\`
without touching R code.

\[milestone_tracker_server()\] calls this function internally for
Card 1. Callers who want the same appearance in their
\`annotation_col_specs\` for Card 2 can call it once and pass the
result:

“\`r renderer \<- make_badge_renderer(badge_colors)

list( name = "category", type = "display", col_def_options = list(cell =
renderer) ) “\`

## Examples

``` r
badge_colors <- list(
  Cognitive     = list(bg = "#E6F1FB", text = "#0C447C"),
  Communication = list(bg = "#EAF3DE", text = "#27500A")
)

renderer <- make_badge_renderer(badge_colors)
renderer("Cognitive")  # <span class="rs-badge" style="background:#E6F1FB; color:#0C447C;">Cognitive</span>
#> <span class="rs-badge" style="background:#E6F1FB;color:#0C447C;">Cognitive</span>
```
