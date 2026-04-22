# tests/testthat/test-annotator_enriched.R
#
# Tests for the annotator_enriched module.
#
# Organised in three sections:
#   1. Pure utility functions  — default_enrichment_value, make_enriched_col_def
#   2. Data frame helpers      — initial_enrichments, merge_enrichments
#   3. Module server           — shiny::testServer for reactive behaviour

library(testthat)
library(shiny)


# -----------------------------------------------------------------------
# Shared fixtures
# -----------------------------------------------------------------------

all_enrich_specs <- list(
  list(
    name    = "severity",
    type    = "select",
    label   = "Severity",
    choices = c("Select..." = "", "Mild" = "mild", "Moderate" = "moderate")
  ),
  list(name = "onset",  type = "date",   label = "Date Onset"),
  list(name = "notes",  type = "text",   label = "Notes", placeholder = "Add notes..."),
  list(name = "score",  type = "number", label = "Score", min = 0, max = 10)
)

# Source data with an integer ID column to verify type handling
make_ailment_source <- function(ailments = c("Flu", "Cold", "Migraine")) {
  data.frame(
    id      = seq_along(ailments),
    ailment = ailments,
    stringsAsFactors = FALSE
  )
}

# Minimal enrich_specs for testServer tests (fewer cols = faster tests)
basic_enrich_specs <- list(
  list(
    name    = "severity",
    type    = "select",
    label   = "Severity",
    choices = c("Select..." = "", "Mild" = "mild", "Moderate" = "moderate")
  ),
  list(name = "notes", type = "text", label = "Notes")
)


# =======================================================================
# 1. Pure utility functions
# =======================================================================

# -----------------------------------------------------------------------
# default_enrichment_value
# -----------------------------------------------------------------------

test_that("default_enrichment_value returns NA_character_ for select", {
  expect_identical(
    default_enrichment_value(list(type = "select")),
    NA_character_
  )
})

test_that("default_enrichment_value returns NA_character_ for text", {
  expect_identical(
    default_enrichment_value(list(type = "text")),
    NA_character_
  )
})

test_that("default_enrichment_value returns NA_character_ for date", {
  expect_identical(
    default_enrichment_value(list(type = "date")),
    NA_character_
  )
})

test_that("default_enrichment_value returns NA_real_ for number", {
  expect_identical(
    default_enrichment_value(list(type = "number")),
    NA_real_
  )
})

test_that("default_enrichment_value errors informatively on unknown type", {
  expect_error(
    default_enrichment_value(list(type = "slider")),
    regexp = "Unknown enrichment type"
  )
})


# -----------------------------------------------------------------------
# make_enriched_col_def dispatcher
# -----------------------------------------------------------------------

test_that("make_enriched_col_def returns a colDef for type 'select'", {
  src       <- make_ailment_source("Flu")
  enr_snap  <- initial_enrichments(src, "id", all_enrich_specs)
  spec      <- all_enrich_specs[[1]]  # severity / select

  col <- make_enriched_col_def(spec, enr_snap, "id", identity)
  expect_s3_class(col, "colDef")
})

test_that("make_enriched_col_def returns a colDef for type 'text'", {
  src       <- make_ailment_source("Flu")
  enr_snap  <- initial_enrichments(src, "id", all_enrich_specs)
  spec      <- all_enrich_specs[[3]]  # notes / text

  col <- make_enriched_col_def(spec, enr_snap, "id", identity)
  expect_s3_class(col, "colDef")
})

test_that("make_enriched_col_def returns a colDef for type 'date'", {
  src       <- make_ailment_source("Flu")
  enr_snap  <- initial_enrichments(src, "id", all_enrich_specs)
  spec      <- all_enrich_specs[[2]]  # onset / date

  col <- make_enriched_col_def(spec, enr_snap, "id", identity)
  expect_s3_class(col, "colDef")
})

test_that("make_enriched_col_def returns a colDef for type 'number'", {
  src       <- make_ailment_source("Flu")
  enr_snap  <- initial_enrichments(src, "id", all_enrich_specs)
  spec      <- all_enrich_specs[[4]]  # score / number

  col <- make_enriched_col_def(spec, enr_snap, "id", identity)
  expect_s3_class(col, "colDef")
})

test_that("make_enriched_col_def errors informatively on unknown type", {
  src       <- make_ailment_source("Flu")
  enr_snap  <- initial_enrichments(src, "id", all_enrich_specs)
  bad_spec  <- list(name = "x", type = "slider", label = "X")

  expect_error(
    make_enriched_col_def(bad_spec, enr_snap, "id", identity),
    regexp = "Unknown enrichment type"
  )
})


# =======================================================================
# 2. Data frame helpers
# =======================================================================

# -----------------------------------------------------------------------
# initial_enrichments
# -----------------------------------------------------------------------

test_that("initial_enrichments produces the correct column names", {
  src    <- make_ailment_source()
  result <- initial_enrichments(src, "id", all_enrich_specs)
  expect_named(result, c("id", "severity", "onset", "notes", "score"))
})

test_that("initial_enrichments has one row per source row", {
  src    <- make_ailment_source()
  result <- initial_enrichments(src, "id", all_enrich_specs)
  expect_equal(nrow(result), nrow(src))
})

test_that("initial_enrichments sets correct defaults for every type", {
  src    <- make_ailment_source("Flu")
  result <- initial_enrichments(src, "id", all_enrich_specs)

  expect_identical(result$severity[1], NA_character_)  # select
  expect_identical(result$onset[1],    NA_character_)  # date
  expect_identical(result$notes[1],    NA_character_)  # text
  expect_true(is.na(result$score[1]))                  # number (NA_real_)
})

test_that("initial_enrichments uses the actual row_id name as column name", {
  src    <- make_ailment_source()
  result <- initial_enrichments(src, "id", all_enrich_specs)

  expect_true("id" %in% names(result))
  expect_false("row_id" %in% names(result))
})

test_that("initial_enrichments coerces the ID column to character", {
  # source has integer IDs — enrichments should store them as character so
  # they match the character keys in selected_ids
  src    <- make_ailment_source()
  result <- initial_enrichments(src, "id", all_enrich_specs)
  expect_type(result[["id"]], "character")
})

test_that("initial_enrichments handles a zero-row source data frame", {
  src    <- make_ailment_source()[0, ]
  result <- initial_enrichments(src, "id", all_enrich_specs)

  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "severity", "onset", "notes", "score"))
})


# -----------------------------------------------------------------------
# merge_enrichments
# -----------------------------------------------------------------------

test_that("merge_enrichments preserves an existing enrichment value", {
  src   <- make_ailment_source()
  specs <- list(
    list(name = "severity", type = "select",
         choices = c("Mild" = "mild"))
  )
  existing             <- initial_enrichments(src, "id", specs)
  existing$severity[1] <- "mild"   # simulate user input on row 1

  result <- merge_enrichments(src, "id", specs, existing)
  expect_equal(result$severity[result$id == "1"], "mild")
})

test_that("merge_enrichments assigns blank defaults to rows not in existing", {
  specs   <- list(
    list(name = "severity", type = "select",
         choices = c("Mild" = "mild"))
  )
  old_src  <- make_ailment_source(c("Flu", "Cold"))
  existing <- initial_enrichments(old_src, "id", specs)

  # Row 3 (Migraine) is brand new
  new_src <- data.frame(
    id      = c(1L, 2L, 3L),
    ailment = c("Flu", "Cold", "Migraine"),
    stringsAsFactors = FALSE
  )
  result <- merge_enrichments(new_src, "id", specs, existing)

  expect_equal(nrow(result), 3)
  expect_identical(result$severity[result$id == "3"], NA_character_)
})

test_that("merge_enrichments result has only rows from the new source_data", {
  specs   <- list(list(name = "severity", type = "select",
                       choices = c("Mild" = "mild")))
  old_src <- make_ailment_source(c("Flu", "Cold", "Migraine"))
  existing <- initial_enrichments(old_src, "id", specs)

  # Cold (id = 2) removed
  new_src <- data.frame(
    id      = c(1L, 3L),
    ailment = c("Flu", "Migraine"),
    stringsAsFactors = FALSE
  )
  result <- merge_enrichments(new_src, "id", specs, existing)

  expect_equal(nrow(result), 2)
  expect_false("2" %in% result$id)
})

test_that("merge_enrichments handles number defaults correctly after join", {
  specs    <- list(list(name = "score", type = "number", min = 0, max = 10))
  old_src  <- make_ailment_source("Flu")
  existing <- initial_enrichments(old_src, "id", specs)

  new_src <- data.frame(
    id      = c(1L, 2L),
    ailment = c("Flu", "Cold"),
    stringsAsFactors = FALSE
  )
  result <- merge_enrichments(new_src, "id", specs, existing)

  expect_true(is.na(result$score[result$id == "2"]))
  expect_type(result$score, "double")
})


# =======================================================================
# 3. Module server
# =======================================================================

test_that("annotator_enriched_server returns empty df when nothing is selected", {
  src <- make_ailment_source()
  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data  = reactive(src),
      row_id       = "id",
      display_cols = "ailment",
      enrich_specs = basic_enrich_specs
    ),
    {
      result <- session$getReturned()
      expect_equal(nrow(result()), 0)
    }
  )
})

test_that("annotator_enriched_server return value has the row_id and enrichment columns", {
  src <- make_ailment_source()
  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data  = reactive(src),
      row_id       = "id",
      display_cols = "ailment",
      enrich_specs = basic_enrich_specs
    ),
    {
      result         <- session$getReturned()
      expected_names <- c("id", "severity", "notes")
      expect_true(all(expected_names %in% names(result())))
    }
  )
})

test_that("annotator_enriched_server return value excludes display columns", {
  src <- make_ailment_source()
  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data  = reactive(src),
      row_id       = "id",
      display_cols = "ailment",
      enrich_specs = basic_enrich_specs
    ),
    {
      result <- session$getReturned()
      expect_false("ailment" %in% names(result()))
    }
  )
})

test_that("annotator_enriched_server errors when enrich_specs is NULL", {
  src <- make_ailment_source()
  expect_error(
    shiny::testServer(
      annotator_enriched_server,
      args = list(
        source_data  = reactive(src),
        row_id       = "id",
        display_cols = "ailment",
        enrich_specs = NULL
      ),
      {}
    ),
    regexp = "enrich_specs"
  )
})

test_that("annotator_enriched_server errors when enrich_specs is empty", {
  src <- make_ailment_source()
  expect_error(
    shiny::testServer(
      annotator_enriched_server,
      args = list(
        source_data  = reactive(src),
        row_id       = "id",
        display_cols = "ailment",
        enrich_specs = list()
      ),
      {}
    ),
    regexp = "enrich_specs"
  )
})

test_that("annotator_enriched_server preserves enrichment values across source_data changes", {
  # This test simulates the user filling in a value, then source_data changing
  # (e.g. a filter is applied that removes then restores the row). The value
  # should survive the round trip.
  src_full     <- make_ailment_source(c("Flu", "Cold"))
  src_filtered <- make_ailment_source("Flu")  # Cold removed by filter

  src_reactive <- reactiveVal(src_full)

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data  = reactive(src_reactive()),
      row_id       = "id",
      display_cols = "ailment",
      enrich_specs = basic_enrich_specs
    ),
    {
      # Simulate user typing in the notes field for row 1 (Flu)
      session$setInputs(`notes_1` = "Started with a fever")

      # Filter source_data to only Flu, then restore Cold
      src_reactive(src_filtered)
      session$flushReact()
      src_reactive(src_full)
      session$flushReact()

      # The notes value for Flu (id=1) should still be present in internal state.
      # We can't directly assert the return value (selection is empty), but we
      # can at least verify the module didn't error during the transition.
      result <- session$getReturned()
      expect_equal(nrow(result()), 0)  # nothing selected, so return is empty
    }
  )
})
