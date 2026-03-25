# app_annotator_test.R
#
# Test app for the annotator_table module.
# Exercises all input types including text_checkbox and row gating.
#
# The "Car" column is a text_checkbox — clicking the car name or the
# checkbox selects the row. The category, notes, score, and priority
# columns are gated by "selected" and remain greyed out until the row
# is selected.
#
# Run with: shiny::runApp("inst/testapp_annotator")

library(shiny)
library(bslib)
library(reactable)
library(reactable.shiny)


# -----------------------------------------------------------------------
# Column specs
# -----------------------------------------------------------------------

col_specs <- list(
  # Clicking the car name checks the row — stored as TRUE/FALSE in "selected"
  list(
    name = "selected",
    type = "text_checkbox",
    label = "Car",
    display_col = "car", # text comes from this source data column
    width = 200
  ),
  list(
    name = "mpg",
    type = "display",
    label = "MPG",
    width = 80
  ),
  list(
    name = "cyl",
    type = "display",
    label = "Cyl",
    width = 60
  ),
  # All inputs below are gated by "selected" — greyed out until car is checked
  list(
    name = "category",
    type = "select",
    label = "Category",
    width = 150,
    choices = setNames(
      c("", "cheap", "mid_range", "expensive"),
      c("Select…", "Cheap", "Mid-range", "Expensive")
    ),
    gates = "selected"
  ),
  list(
    name = "notes",
    type = "text",
    label = "Notes",
    width = 180,
    placeholder = "Add a note…",
    gates = "selected"
  ),
  list(
    name = "score",
    type = "number",
    label = "Score",
    width = 80,
    min = 0,
    max = 10,
    gates = "selected"
  ),
  list(
    name = "priority",
    type = "radio",
    label = "Priority",
    width = 100,
    choices = c("High" = "high", "Low" = "low"),
    gates = "selected"
  )
)


# -----------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------

ui <- page_fluid(
  title = "annotator_table test — text_checkbox + gating",

  card(
    card_header("Filter"),
    p(
      "Filter the table to test that annotations on hidden rows are
      preserved when they reappear."
    ),
    checkboxGroupInput(
      "cyl_filter",
      label = "Show cylinders:",
      choices = c("4" = 4, "6" = 6, "8" = 8),
      selected = c(4, 6, 8),
      inline = TRUE
    )
  ),

  card(
    card_header("Cars"),
    p("Click a car name to select it. Other inputs in that row become active."),
    annotator_table_ui("cars")
  ),

  card(
    card_header("Returned data"),
    p(
      "Only rows where at least one input is touched are shown. Rows hidden
      by the filter are excluded even if annotated."
    ),
    verbatimTextOutput("returned_data")
  )
)


# -----------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------

server <- function(input, output, session) {
  filtered_cars <- reactive({
    df <- mtcars[
      mtcars$cyl %in% as.numeric(input$cyl_filter),
      c("mpg", "cyl", "hp")
    ]
    df$car <- rownames(df)
    rownames(df) <- NULL
    df[, c("car", "mpg", "cyl", "hp")]
  })

  result <- annotator_table_server(
    id = "cars",
    source_data = filtered_cars,
    row_id = "car",
    col_specs = col_specs
  )

  output$returned_data <- renderPrint({
    result()
  })
}

shinyApp(ui, server)
