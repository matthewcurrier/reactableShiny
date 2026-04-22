# tests/testthat/test-annotator_enriched.R
#
# Tests for the annotator_enriched module and its utils.
#
# Organised in four sections:
#   1. Pure utility functions   — default_enrichment_value, no Shiny
#   2. Data frame helpers       — initial_enrichments_blank, merge_enrichments
#   3. Module server — startup  — backward-compat, initial_selected/enrichments
#   4. Module server — reset_to — the new mid-session context-switch API

library(testthat)
library(shiny)


# -----------------------------------------------------------------------
# Shared fixtures
# -----------------------------------------------------------------------

make_src <- function(ids = c("a", "b", "c")) {
  data.frame(
    id = ids,
    label = paste0("Item ", ids),
    stringsAsFactors = FALSE
  )
}

basic_enrich_specs <- list(
  list(
    name = "status",
    type = "select",
    label = "Status",
    choices = c("Select..." = "", "Active" = "active", "Closed" = "closed")
  ),
  list(name = "notes", type = "text", label = "Notes"),
  list(name = "score", type = "number", label = "Score"),
  list(name = "due", type = "date", label = "Due Date")
)


# =======================================================================
# 1. default_enrichment_value
# =======================================================================

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


# =======================================================================
# 2. initial_enrichments_blank
# =======================================================================

test_that("initial_enrichments_blank produces correct column names", {
  src <- make_src()
  result <- initial_enrichments_blank(src, "id", basic_enrich_specs)
  expect_named(result, c("id", "status", "notes", "score", "due"))
})

test_that("initial_enrichments_blank uses actual row_id name as column name", {
  src <- make_src()
  result <- initial_enrichments_blank(src, "id", basic_enrich_specs)
  expect_true("id" %in% names(result))
  expect_false("row_id" %in% names(result))
})

test_that("initial_enrichments_blank has one row per source row", {
  src <- make_src()
  result <- initial_enrichments_blank(src, "id", basic_enrich_specs)
  expect_equal(nrow(result), nrow(src))
})

test_that("initial_enrichments_blank stores row IDs as character", {
  src <- make_src(ids = c("1", "2", "3"))
  result <- initial_enrichments_blank(src, "id", basic_enrich_specs)
  expect_type(result$id, "character")
})

test_that("initial_enrichments_blank seeds correct per-type defaults", {
  src <- make_src(ids = "x")
  result <- initial_enrichments_blank(src, "id", basic_enrich_specs)

  expect_identical(result$status[1], NA_character_) # select
  expect_identical(result$notes[1], NA_character_) # text
  expect_identical(result$due[1], NA_character_) # date
  expect_true(is.na(result$score[1])) # number (NA_real_)
  expect_type(result$score, "double")
})

test_that("initial_enrichments_blank handles zero-row source data without error", {
  src <- make_src()[0L, ]
  result <- initial_enrichments_blank(src, "id", basic_enrich_specs)

  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "status", "notes", "score", "due"))
})


# =======================================================================
# 3. merge_enrichments
# =======================================================================

test_that("merge_enrichments preserves existing values for matching rows", {
  src <- make_src()
  existing <- initial_enrichments_blank(src, "id", basic_enrich_specs)
  existing$status[existing$id == "a"] <- "active"

  result <- merge_enrichments(src, "id", basic_enrich_specs, existing)
  expect_equal(result$status[result$id == "a"], "active")
})

test_that("merge_enrichments gives per-type defaults to new rows", {
  old_src <- make_src(ids = c("a", "b"))
  existing <- initial_enrichments_blank(old_src, "id", basic_enrich_specs)
  existing$status[existing$id == "a"] <- "active"

  new_src <- make_src(ids = c("a", "b", "c"))
  result <- merge_enrichments(new_src, "id", basic_enrich_specs, existing)

  expect_equal(nrow(result), 3)
  expect_identical(result$status[result$id == "c"], NA_character_)
  expect_identical(result$notes[result$id == "c"], NA_character_)
  expect_true(is.na(result$score[result$id == "c"]))
})

test_that("merge_enrichments result contains only rows from the new source_data", {
  old_src <- make_src(ids = c("a", "b", "c"))
  existing <- initial_enrichments_blank(old_src, "id", basic_enrich_specs)

  new_src <- make_src(ids = c("a", "c"))
  result <- merge_enrichments(new_src, "id", basic_enrich_specs, existing)

  expect_equal(nrow(result), 2)
  expect_false("b" %in% result$id)
})

test_that("merge_enrichments seeded from a blank frame equals initial_enrichments_blank", {
  src <- make_src()
  blank <- initial_enrichments_blank(src, "id", basic_enrich_specs)
  merged <- merge_enrichments(src, "id", basic_enrich_specs, blank)

  expect_equal(merged, blank)
})

test_that("merge_enrichments correctly seeds values from a partial enrichments frame", {
  src <- make_src()
  seed <- data.frame(
    id = "b",
    status = "closed",
    notes = "pre-filled",
    score = 7.5,
    due = "2026-01-01",
    stringsAsFactors = FALSE
  )

  result <- merge_enrichments(src, "id", basic_enrich_specs, seed)

  expect_equal(result$status[result$id == "b"], "closed")
  expect_equal(result$notes[result$id == "b"], "pre-filled")
  expect_identical(result$status[result$id == "a"], NA_character_)
})


# =======================================================================
# 4. annotator_enriched_server — startup (backward compatibility)
# =======================================================================

test_that("server starts without reset_to and returns zero rows initially", {
  src <- make_src()
  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs
    ),
    {
      result <- session$getReturned()
      expect_equal(nrow(result()), 0)
    }
  )
})

test_that("server errors informatively when enrich_specs is NULL", {
  src <- make_src()
  expect_error(
    shiny::testServer(
      annotator_enriched_server,
      args = list(
        source_data = reactive(src),
        row_id = "id",
        display_cols = "label",
        enrich_specs = NULL
      ),
      {}
    ),
    regexp = "enrich_specs"
  )
})

test_that("server return value excludes display columns", {
  src <- make_src()
  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs
    ),
    {
      result <- session$getReturned()
      expect_false("label" %in% names(result()))
    }
  )
})

test_that("server honors initial_selected at startup", {
  src <- make_src()
  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      initial_selected = c("b", "c")
    ),
    {
      result <- session$getReturned()
      expect_equal(nrow(result()), 2)
      expect_setequal(result()$id, c("b", "c"))
    }
  )
})

test_that("server honors initial_enrichments at startup", {
  src <- make_src()
  seed <- data.frame(
    id = "a",
    status = "active",
    notes = "startup note",
    score = NA_real_,
    due = NA_character_,
    stringsAsFactors = FALSE
  )
  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      initial_selected = c("a"),
      initial_enrichments = seed
    ),
    {
      result <- session$getReturned()
      expect_equal(result()$status, "active")
      expect_equal(result()$notes, "startup note")
    }
  )
})

test_that("initial_selected values absent from source_data are silently ignored", {
  src <- make_src(ids = c("a", "b"))
  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      initial_selected = c("a", "z") # "z" not in src
    ),
    {
      result <- session$getReturned()
      expect_equal(nrow(result()), 1)
      expect_equal(result()$id, "a")
    }
  )
})


# =======================================================================
# 5. annotator_enriched_server — reset_to (new API)
# =======================================================================

test_that("reset_to = NULL is the backward-compatible default (no error)", {
  src <- make_src()
  expect_no_error(
    shiny::testServer(
      annotator_enriched_server,
      args = list(
        source_data = reactive(src),
        row_id = "id",
        display_cols = "label",
        enrich_specs = basic_enrich_specs,
        reset_to = NULL
      ),
      {
        result <- session$getReturned()
        expect_equal(nrow(result()), 0)
      }
    )
  )
})

test_that("reset_to resets selected_ids to the new selection", {
  src <- make_src()
  rv_reset <- shiny::reactiveVal(NULL)

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      reset_to = rv_reset
    ),
    {
      result <- session$getReturned()
      expect_equal(nrow(result()), 0)

      rv_reset(list(selected = c("a"), enrichments = NULL))
      session$flushReact()

      expect_equal(nrow(result()), 1)
      expect_equal(result()$id, "a")
    }
  )
})

test_that("reset_to seeds enrichment values from the provided data frame", {
  src <- make_src()
  rv_reset <- shiny::reactiveVal(NULL)
  enrich_seed <- data.frame(
    id = "a",
    status = "active",
    notes = "seeded note",
    score = NA_real_,
    due = NA_character_,
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      reset_to = rv_reset
    ),
    {
      result <- session$getReturned()

      rv_reset(list(selected = c("a"), enrichments = enrich_seed))
      session$flushReact()

      expect_equal(nrow(result()), 1)
      expect_equal(result()$status, "active")
      expect_equal(result()$notes, "seeded note")
    }
  )
})

test_that("reset_to with NULL enrichments produces a blank enrichment frame", {
  src <- make_src()
  rv_reset <- shiny::reactiveVal(NULL)
  init_seed <- data.frame(
    id = "a",
    status = "active",
    notes = "old note",
    score = NA_real_,
    due = NA_character_,
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      initial_selected = c("a"),
      initial_enrichments = init_seed,
      reset_to = rv_reset
    ),
    {
      result <- session$getReturned()
      expect_equal(result()$status, "active")

      # Switch to a new report: select "b", no enrichment seed
      rv_reset(list(selected = c("b"), enrichments = NULL))
      session$flushReact()

      expect_equal(nrow(result()), 1)
      expect_equal(result()$id, "b")
      expect_true(is.na(result()$status))
      expect_true(is.na(result()$notes))
    }
  )
})

test_that("reset_to with character(0) selection clears all selections", {
  src <- make_src()
  rv_reset <- shiny::reactiveVal(NULL)

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      initial_selected = c("a", "b"),
      reset_to = rv_reset
    ),
    {
      result <- session$getReturned()
      expect_equal(nrow(result()), 2)

      rv_reset(list(selected = character(0), enrichments = NULL))
      session$flushReact()

      expect_equal(nrow(result()), 0)
    }
  )
})

test_that("reset_to with NULL selected field clears selection gracefully", {
  src <- make_src()
  rv_reset <- shiny::reactiveVal(NULL)

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      initial_selected = c("a"),
      reset_to = rv_reset
    ),
    {
      result <- session$getReturned()
      expect_equal(nrow(result()), 1)

      rv_reset(list(selected = NULL, enrichments = NULL))
      session$flushReact()

      expect_equal(nrow(result()), 0)
    }
  )
})

test_that("sequential reset_to calls correctly replace state each time", {
  src <- make_src()
  rv_reset <- shiny::reactiveVal(NULL)

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      reset_to = rv_reset
    ),
    {
      result <- session$getReturned()

      # Report A: select "a"
      rv_reset(list(selected = c("a"), enrichments = NULL))
      session$flushReact()
      expect_equal(result()$id, "a")

      # Report B: select "b" and "c" — "a" must no longer appear
      rv_reset(list(selected = c("b", "c"), enrichments = NULL))
      session$flushReact()
      expect_setequal(result()$id, c("b", "c"))
      expect_false("a" %in% result()$id)
    }
  )
})

test_that("reset_to does not fire at startup when reset_to starts as NULL (reactiveVal contract)", {
  # The module uses ignoreNULL = TRUE and ignoreInit = FALSE.
  # Startup suppression is the CALLER'S responsibility: pass reactiveVal(NULL)
  # so the observer is silently skipped until an explicit switch signal arrives.
  # This test verifies that a NULL reset_to leaves initial_selected intact.
  src <- make_src()
  rv_reset <- shiny::reactiveVal(NULL) # never set — stays NULL throughout

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      initial_selected = c("b"),
      reset_to = rv_reset
    ),
    {
      result <- session$getReturned()
      # "b" must still be selected — the NULL reset_to was silently skipped
      expect_equal(result()$id, "b")
    }
  )
})

test_that("reset_to selected IDs not in source_data are silently dropped from return value", {
  src <- make_src(ids = c("a", "b"))
  rv_reset <- shiny::reactiveVal(NULL)

  shiny::testServer(
    annotator_enriched_server,
    args = list(
      source_data = reactive(src),
      row_id = "id",
      display_cols = "label",
      enrich_specs = basic_enrich_specs,
      reset_to = rv_reset
    ),
    {
      result <- session$getReturned()

      # "z" is not in source_data — should be silently ignored in return value
      rv_reset(list(selected = c("a", "z"), enrichments = NULL))
      session$flushReact()

      expect_equal(nrow(result()), 1)
      expect_equal(result()$id, "a")
    }
  )
})
