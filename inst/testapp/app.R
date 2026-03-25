# app_test.R
#
# A minimal app for manually testing the flexible_table module.
# Demonstrates basic usage: two simple dropdowns, add/delete/reset.
# The reactive data frame returned by the module is printed below the
# table so you can verify what gets returned at any point.
#
# Run with: shiny::runApp("app_test.R")

library(shiny)
library(bslib)
library(reactable)
library(reactable.shiny)
library(here)

source(here("R", "utils.R"))
source(here("R", "themes.R"))
source(here("R", "mod_flexible_table_input.R"))


# -----------------------------------------------------------------------
# Column specs
# -----------------------------------------------------------------------

col_specs <- list(
  list(
    name = "fruit",
    label = "Fruit",
    choices = setNames(
      c("", "apple", "banana", "cherry"),
      c("", "Apple", "Banana", "Cherry")
    )
  ),
  list(
    name = "quantity",
    label = "Quantity",
    choices = setNames(
      c("", "1", "2", "3"),
      c("", "One", "Two", "Three")
    )
  ),
  list(
    name = "urgency",
    label = "Urgency",
    choices = setNames(
      c("High", "Low"),
      c("High", "Low")
    )
  )
)


# -----------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------

ui <- page_sidebar(
  title = "flexible_table test",
  card(
    min_height = 300,
    card_header("Fruit picker"),
    card_body(
      flexible_table_ui("test")$table
    ),
    card_footer(
      div(
        flexible_table_ui("test")$add_row_button,
        br(),
        flexible_table_ui("test")$reset_button
      )
    )
  ),
  card(
    card_header("Returned data"),
    p(
      "Rows with any blank value are excluded. ",
      "This updates live as you change the table above."
    ),
    verbatimTextOutput("returned_data")
  )
)


# -----------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------

server <- function(input, output, session) {
  selected <- flexible_table_server(
    id = "test",
    col_specs = col_specs
  )

  output$returned_data <- renderPrint({
    selected()
  })
}

shinyApp(ui, server)
