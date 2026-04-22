# =============================================================================
# app.R — annotator_enriched pre-population demo
#
# Simulates re-opening a saved report: three conditions arrive pre-checked
# with severity, notes, and diagnosis date already filled in, exactly as
# they would be when a clinician re-opens an existing student report.
#
# To run:
#   Place this file alongside mod_annotator_enriched.R and
#   utils_annotator_enriched.R, then call shiny::runApp().
#
# External stubs:
#   make_options() and theme_bare live in utils_flexible.R / themes.R in
#   the main application. Minimal versions are provided here so the demo
#   is self-contained.
# =============================================================================

library(shiny)
library(bslib)
library(reactable)
library(dplyr)
library(purrr)
library(here)

source(here("R", "utils_annotator_enriched.R"))
source(here("R", "mod_annotator_enriched.R"))


# ── Stubs for helpers defined elsewhere in the main app ───────────────────────

#' Produce a list of <option> tags for a <select> element.
#' Mirrors utils_flexible.R::make_options().
make_options <- function(choices, selected = "") {
  values <- unname(choices)
  labels <- if (!is.null(names(choices))) names(choices) else choices

  make_one <- function(label, value) {
    shiny::tags$option(
      value = value,
      selected = if (identical(value, selected)) "selected" else NULL,
      label
    )
  }

  purrr::map2(labels, values, make_one)
}

theme_bare <- reactable::reactableTheme(
  borderColor = "#dee2e6",
  cellPadding = "6px 12px"
)


# ── Demo data ─────────────────────────────────────────────────────────────────

conditions_df <- data.frame(
  id = as.character(1:10),
  condition = c(
    "Major Depressive Disorder",
    "Generalized Anxiety Disorder",
    "Post-Traumatic Stress Disorder",
    "ADHD - Combined Presentation",
    "Bipolar I Disorder",
    "Obsessive-Compulsive Disorder",
    "Traumatic Brain Injury",
    "Autism Spectrum Disorder",
    "Panic Disorder",
    "Social Anxiety Disorder"
  ),
  category = c(
    "Mood",
    "Anxiety",
    "Trauma & Stressor",
    "Neurodevelopmental",
    "Mood",
    "Anxiety",
    "Neurological",
    "Neurodevelopmental",
    "Anxiety",
    "Anxiety"
  ),
  stringsAsFactors = FALSE
)

# Simulates what db_get_report_ailments() returns when a saved report is opened.
# Three rows have been previously recorded with varying completeness:
#   - Row 1: severity + notes + date
#   - Row 3: severity + notes only
#   - Row 4: severity + date only (notes left blank)
saved_enrichments <- data.frame(
  id = c("1", "3", "4"),
  severity = c("moderate", "severe", "mild"),
  notes = c(
    "Onset age 14; two prior hospitalisations",
    "Combat-related; active nightmares reported",
    NA_character_
  ),
  diagnosis_date = c("2019-06-10", "2021-03-22", "2020-08-05"),
  stringsAsFactors = FALSE
)

saved_selected <- c("1", "3", "4")

enrich_specs <- list(
  list(
    name = "severity",
    type = "select",
    label = "Severity",
    choices = c(
      "Select..." = "",
      "Mild" = "mild",
      "Moderate" = "moderate",
      "Severe" = "severe"
    ),
    width = 145
  ),
  list(
    name = "notes",
    type = "text",
    label = "Clinical Notes",
    placeholder = "Enter notes...",
    width = 260
  ),
  list(
    name = "diagnosis_date",
    type = "date",
    label = "Diagnosis Date",
    width = 160
  )
)


# ── UI ────────────────────────────────────────────────────────────────────────

demo_ui <- annotator_enriched_ui("demo")

ui <- bslib::page_sidebar(
  title = "annotator_enriched — Pre-population Demo",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  lang = "en",

  sidebar = bslib::sidebar(
    title = "Demo Controls",
    width = 340,

    bslib::card(
      bslib::card_header("Seed data (simulated DB fetch)"),
      bslib::card_body(
        class = "p-2",
        p(
          class = "text-muted small mb-2",
          "These rows were passed as ",
          code("initial_enrichments"),
          " and ",
          code("initial_selected"),
          " when the module initialised — ",
          "mimicking a clinician re-opening a saved report."
        ),
        reactable::reactableOutput("seed_table")
      )
    ),

    bslib::card(
      bslib::card_header("Live module output"),
      bslib::card_body(
        class = "p-2",
        p(
          class = "text-muted small mb-2",
          "The data frame returned by ",
          code("get_data()"),
          ". ",
          "Updates on every selection or enrichment change."
        ),
        reactable::reactableOutput("result_table")
      )
    )
  ),

  bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      "Mental Health Conditions",
      demo_ui$reset_button
    ),
    bslib::card_body(demo_ui$table)
  )
)


# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  get_data <- annotator_enriched_server(
    id = "demo",
    source_data = shiny::reactive(conditions_df),
    row_id = "id",
    display_cols = c("condition", "category"),
    enrich_specs = enrich_specs,
    initial_enrichments = saved_enrichments,
    initial_selected = saved_selected
  )

  # ── Sidebar: static seed display ─────────────────────────────────────────

  output$seed_table <- reactable::renderReactable({
    reactable::reactable(
      saved_enrichments,
      columns = list(
        id = reactable::colDef(name = "ID", width = 40),
        severity = reactable::colDef(name = "Severity", width = 90),
        notes = reactable::colDef(name = "Notes"),
        diagnosis_date = reactable::colDef(name = "Date", width = 90)
      ),
      compact = TRUE,
      bordered = TRUE
    )
  })

  # ── Sidebar: live module output ───────────────────────────────────────────

  output$result_table <- reactable::renderReactable({
    reactable::reactable(
      get_data(),
      columns = list(
        id = reactable::colDef(name = "ID", width = 40),
        severity = reactable::colDef(name = "Severity", width = 90),
        notes = reactable::colDef(name = "Notes"),
        diagnosis_date = reactable::colDef(name = "Date", width = 90)
      ),
      compact = TRUE,
      bordered = TRUE
    )
  })
}

shinyApp(ui, server)
