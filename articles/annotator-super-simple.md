# Annotator Super Simple

## Simple Row Select with Annotator Table

This module is a thin wrapper around the
[`reactable()`](https://glin.github.io/reactable/reference/reactable.html)
function from the reactable package. The `annotator_super_simple_ui` and
`annotator_super_simple_server` functions do just a few things.

- Easier Row Selection - Any field with an individual row will active
  selection or deselection with just a mouse click
- It provides a ‘reset all’ button via the ui portion of the module.
- It makes server side filtering easier.
- If you filter rows of the table and make a selections those selected
  rows persist.

The basic pattern is straightforward — pass a reactive data frame, name
the row-ID column, and list the columns to display:

``` r

library(shiny)
library(reactable)
library(reactableShiny)

mtcars_data      <- mtcars
mtcars_data$car  <- rownames(mtcars)
rownames(mtcars_data) <- NULL

display_cols <- c("car", "mpg", "cyl", "hp", "wt")

ui_els <- annotator_super_simple_ui("demo")

ui <- fluidPage(
  titlePanel("mtcars Explorer"),
  fluidRow(
    column(2, ui_els$reset_button),
    column(10)
  ),
  br(),
  ui_els$table,
  br(),
  strong("Selected cars:"),
  verbatimTextOutput("selected_out")
)

server <- function(input, output, session) {
  selected <- annotator_super_simple_server(
    id           = "demo",
    source_data  = reactive(mtcars_data),
    row_id       = "car",
    display_cols = display_cols
  )

  output$selected_out <- renderPrint({
    ids <- selected()
    if (length(ids) == 0) cat("None") else cat(ids, sep = "\n")
  })
}

shinyApp(ui, server)
```

## Filtering with selection persistence

Because `source_data` is a reactive expression, you can drive it from
any filter control. Rows that are selected and then temporarily hidden
by a filter remain selected when they reappear — the module reconciles
visible row indices against the stored ID vector on every render.

``` r

library(shiny)
library(reactable)
library(reactableShiny)

mtcars_data      <- mtcars
mtcars_data$car  <- rownames(mtcars)
rownames(mtcars_data) <- NULL

display_cols <- c("car", "mpg", "cyl", "hp", "wt")

ui_els <- annotator_super_simple_ui("demo")

ui <- fluidPage(
  titlePanel("mtcars Explorer"),
  fluidRow(
    column(3,
      selectInput(
        "cyl_filter",
        "Filter by cylinders",
        choices  = c("All" = "all", "4" = "4", "6" = "6", "8" = "8"),
        selected = "all"
      )
    ),
    column(3, br(), ui_els$reset_button),
    column(6)
  ),
  ui_els$table,
  br(),
  strong("Selected cars:"),
  verbatimTextOutput("selected_out")
)

server <- function(input, output, session) {

  # source_data is now reactive — it changes when the filter changes.
  # This is what exercises the module's persistence logic.
  filtered_data <- reactive({
    if (input$cyl_filter == "all") {
      mtcars_data
    } else {
      mtcars_data[mtcars_data$cyl == as.integer(input$cyl_filter), ]
    }
  })

  selected <- annotator_super_simple_server(
    id           = "demo",
    source_data  = filtered_data,
    row_id       = "car",
    display_cols = display_cols
  )

  output$selected_out <- renderPrint({
    ids <- selected()
    if (length(ids) == 0) cat("None") else cat(ids, sep = "\n")
  })
}

shinyApp(ui, server)
```
