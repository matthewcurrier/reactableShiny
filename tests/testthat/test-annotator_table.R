# tests/testthat/test-annotator_table.R
#
# Tests for the annotator_table module.
#
# Organised in three sections:
#   1. Pure utility functions — no Shiny, fast to run
#   2. Data frame helpers    — initial_annotations, merge_annotations
#   3. Module server         — shiny::testServer for reactive behaviour

library(testthat)
library(shiny)


# -----------------------------------------------------------------------
# Shared fixtures
# -----------------------------------------------------------------------

# A minimal set of input specs covering every type
all_input_specs <- list(
  list(name = "category", type = "select", choices = c("a", "b")),
  list(name = "approved", type = "checkbox"),
  list(name = "selected", type = "text_checkbox", display_col = "car"),
  list(name = "notes", type = "text"),
  list(name = "score", type = "number"),
  list(name = "priority", type = "radio", choices = c("High" = "high"))
)

# Minimal source data frame with a named ID column
make_source <- function(cars = c("Mazda", "Honda", "Toyota")) {
  data.frame(
    car = cars,
    mpg = seq(21, by = 3, length.out = length(cars)),
    stringsAsFactors = FALSE
  )
}

# Minimal col_specs for use with testServer (display + two input types)
basic_col_specs <- list(
  list(name = "car", type = "display", label = "Car"),
  list(
    name = "category",
    type = "select",
    label = "Category",
    choices = setNames(c("", "cheap"), c("", "Cheap"))
  ),
  list(name = "approved", type = "checkbox", label = "Approved?")
)


# =======================================================================
# 1. Pure utility functions
# =======================================================================

# -----------------------------------------------------------------------
# default_annotation_value
# -----------------------------------------------------------------------

test_that("default_annotation_value returns NA_character_ for select", {
  expect_identical(
    default_annotation_value(list(type = "select")),
    NA_character_
  )
})

test_that("default_annotation_value returns NA_character_ for text", {
  expect_identical(
    default_annotation_value(list(type = "text")),
    NA_character_
  )
})

test_that("default_annotation_value returns NA_character_ for radio", {
  expect_identical(
    default_annotation_value(list(type = "radio")),
    NA_character_
  )
})

test_that("default_annotation_value returns FALSE for checkbox", {
  expect_identical(
    default_annotation_value(list(type = "checkbox")),
    FALSE
  )
})

test_that("default_annotation_value returns FALSE for text_checkbox", {
  expect_identical(
    default_annotation_value(list(type = "text_checkbox")),
    FALSE
  )
})

test_that("default_annotation_value returns NA_real_ for number", {
  expect_identical(
    default_annotation_value(list(type = "number")),
    NA_real_
  )
})

test_that("default_annotation_value errors on unknown type", {
  expect_error(
    default_annotation_value(list(type = "slider")),
    regexp = "Unknown input column type"
  )
})


# -----------------------------------------------------------------------
# is_touched
# -----------------------------------------------------------------------

test_that("is_touched: select is touched only when non-NA and non-empty", {
  expect_false(is_touched(NA_character_, "select"))
  expect_false(is_touched("", "select"))
  expect_false(is_touched(NULL, "select"))
  expect_true(is_touched("cheap", "select"))
})

test_that("is_touched: text is touched only when non-NA and non-empty", {
  expect_false(is_touched(NA_character_, "text"))
  expect_false(is_touched("", "text"))
  expect_true(is_touched("a note", "text"))
})

test_that("is_touched: radio is touched only when non-NA and non-empty", {
  expect_false(is_touched(NA_character_, "radio"))
  expect_false(is_touched("", "radio"))
  expect_true(is_touched("high", "radio"))
})

test_that("is_touched: checkbox is touched only when TRUE", {
  expect_false(is_touched(FALSE, "checkbox"))
  expect_false(is_touched(NULL, "checkbox"))
  expect_true(is_touched(TRUE, "checkbox"))
})

test_that("is_touched: text_checkbox is touched only when TRUE", {
  expect_false(is_touched(FALSE, "text_checkbox"))
  expect_true(is_touched(TRUE, "text_checkbox"))
})

test_that("is_touched: number is touched only when non-NA and non-zero", {
  expect_false(is_touched(NA_real_, "number"))
  expect_false(is_touched(0, "number"))
  expect_false(is_touched(NULL, "number"))
  expect_true(is_touched(5, "number"))
  expect_true(is_touched(-1, "number"))
})

test_that("is_touched errors on unknown type", {
  expect_error(is_touched("x", "slider"), regexp = "Unknown")
})


# -----------------------------------------------------------------------
# any_touched
# -----------------------------------------------------------------------

test_that("any_touched returns FALSE for all-default rows", {
  ann <- data.frame(
    car = c("Mazda", "Honda"),
    category = c(NA_character_, NA_character_),
    approved = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  specs <- list(
    list(name = "category", type = "select"),
    list(name = "approved", type = "checkbox")
  )
  expect_equal(any_touched(ann, specs), c(FALSE, FALSE))
})

test_that("any_touched returns TRUE for rows with at least one touched value", {
  ann <- data.frame(
    car = c("Mazda", "Honda"),
    category = c("cheap", NA_character_),
    approved = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  specs <- list(
    list(name = "category", type = "select"),
    list(name = "approved", type = "checkbox")
  )
  expect_equal(any_touched(ann, specs), c(TRUE, TRUE))
})

test_that("any_touched is not confused by apply() matrix coercion", {
  # This test guards against the regression where apply() coerced the
  # data frame to character, turning FALSE into "FALSE" and breaking isTRUE()
  ann <- data.frame(
    car = "Mazda",
    approved = TRUE,
    score = 0,
    stringsAsFactors = FALSE
  )
  specs <- list(
    list(name = "approved", type = "checkbox"),
    list(name = "score", type = "number")
  )
  expect_true(any_touched(ann, specs)[1])
})


# -----------------------------------------------------------------------
# gate_is_open
# -----------------------------------------------------------------------

test_that("gate_is_open returns TRUE when spec has no gates field", {
  spec <- list(name = "category", type = "select")
  ann <- data.frame(car = "Mazda", selected = FALSE, stringsAsFactors = FALSE)
  expect_true(gate_is_open(spec, ann, 1))
})

test_that("gate_is_open returns FALSE when gate checkbox is FALSE", {
  spec <- list(name = "category", type = "select", gates = "selected")
  ann <- data.frame(car = "Mazda", selected = FALSE, stringsAsFactors = FALSE)
  expect_false(gate_is_open(spec, ann, 1))
})

test_that("gate_is_open returns TRUE when gate checkbox is TRUE", {
  spec <- list(name = "category", type = "select", gates = "selected")
  ann <- data.frame(car = "Mazda", selected = TRUE, stringsAsFactors = FALSE)
  expect_true(gate_is_open(spec, ann, 1))
})


# =======================================================================
# 2. Data frame helpers
# =======================================================================

# -----------------------------------------------------------------------
# initial_annotations
# -----------------------------------------------------------------------

test_that("initial_annotations has correct column names", {
  src <- make_source()
  specs <- list(
    list(name = "category", type = "select"),
    list(name = "approved", type = "checkbox")
  )
  result <- initial_annotations(src, "car", specs)
  expect_named(result, c("car", "category", "approved"))
})

test_that("initial_annotations has one row per source row", {
  src <- make_source()
  specs <- list(list(name = "category", type = "select"))
  result <- initial_annotations(src, "car", specs)
  expect_equal(nrow(result), nrow(src))
})

test_that("initial_annotations populates correct defaults for each type", {
  src <- make_source(cars = "Mazda")
  result <- initial_annotations(src, "car", all_input_specs)

  expect_equal(nrow(result), 1)
  expect_identical(result$category[1], NA_character_)
  expect_identical(result$approved[1], FALSE)
  expect_identical(result$selected[1], FALSE)
  expect_identical(result$notes[1], NA_character_)
  expect_true(is.na(result$score[1]))
  expect_identical(result$priority[1], NA_character_)
})

test_that("initial_annotations uses actual row_id name as column name", {
  src <- make_source()
  specs <- list(list(name = "category", type = "select"))
  result <- initial_annotations(src, "car", specs)
  expect_true("car" %in% names(result))
  expect_false("row_id" %in% names(result))
})

test_that("initial_annotations handles zero-row source data without error", {
  src <- make_source()[0, ]
  specs <- list(list(name = "category", type = "select"))
  expect_no_error(initial_annotations(src, "car", specs))
  result <- initial_annotations(src, "car", specs)
  expect_equal(nrow(result), 0)
  expect_named(result, c("car", "category"))
})


# -----------------------------------------------------------------------
# merge_annotations
# -----------------------------------------------------------------------

test_that("merge_annotations preserves existing annotation values", {
  src <- make_source()
  specs <- list(list(name = "category", type = "select"))
  existing <- initial_annotations(src, "car", specs)

  # Simulate user having annotated the first row
  existing$category[1] <- "cheap"

  # Source data unchanged — merge should preserve the annotation
  result <- merge_annotations(src, "car", specs, existing)
  expect_equal(result$category[result$car == "Mazda"], "cheap")
})

test_that("merge_annotations gives blank defaults to new rows", {
  specs <- list(list(name = "category", type = "select"))
  old_src <- make_source(cars = c("Mazda", "Honda"))
  existing <- initial_annotations(old_src, "car", specs)
  existing$category[1] <- "cheap"

  # Toyota is a new row — should get NA_character_ default
  new_src <- make_source(cars = c("Mazda", "Honda", "Toyota"))
  result <- merge_annotations(new_src, "car", specs, existing)

  expect_equal(nrow(result), 3)
  expect_identical(result$category[result$car == "Toyota"], NA_character_)
})

test_that("merge_annotations result has only rows from new source_data", {
  specs <- list(list(name = "category", type = "select"))
  old_src <- make_source(cars = c("Mazda", "Honda", "Toyota"))
  existing <- initial_annotations(old_src, "car", specs)

  # Honda removed from source — result should only have Mazda and Toyota
  new_src <- make_source(cars = c("Mazda", "Toyota"))
  result <- merge_annotations(new_src, "car", specs, existing)

  expect_equal(nrow(result), 2)
  expect_false("Honda" %in% result$car)
})


# =======================================================================
# 3. Module server
# =======================================================================

test_that("annotator_table_server returns empty data frame when nothing is touched", {
  src <- make_source()

  shiny::testServer(
    annotator_table_server,
    args = list(
      source_data = reactive(src),
      row_id = "car",
      col_specs = basic_col_specs
    ),
    {
      result <- session$getReturned()
      expect_equal(nrow(result()), 0)
    }
  )
})

test_that("annotator_table_server errors informatively when col_specs is NULL", {
  src <- make_source()

  expect_error(
    shiny::testServer(
      annotator_table_server,
      args = list(
        source_data = reactive(src),
        row_id = "car",
        col_specs = NULL
      ),
      {}
    ),
    regexp = "col_specs"
  )
})

test_that("annotator_table_server return value excludes id column from source_data", {
  src <- make_source()

  shiny::testServer(
    annotator_table_server,
    args = list(
      source_data = reactive(src),
      row_id = "car",
      col_specs = basic_col_specs
    ),
    {
      result <- session$getReturned()
      # Even when empty, the returned frame should only have the row_id
      # and input annotation columns — not source display columns like mpg
      expect_false("mpg" %in% names(result()))
    }
  )
})
