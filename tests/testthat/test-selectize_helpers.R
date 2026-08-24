# =============================================================================
# Tests for selectize type support in annotator helper functions.
#
# These tests verify that the selectize type is handled correctly by the
# internal helpers in utils_annotator.R. Run with testthat::test_file().
# =============================================================================

library(testthat)

# Source the helpers under test (adjust path as needed for your project)
source("utils_annotator.R")


# -------------------------------------------------------------------------
# default_annotation_value
# -------------------------------------------------------------------------

test_that("default_annotation_value returns NA_character_ for selectize", {
  spec <- list(type = "selectize", name = "tag")
  expect_identical(default_annotation_value(spec), NA_character_)
})


# -------------------------------------------------------------------------
# is_touched
# -------------------------------------------------------------------------

test_that("is_touched returns FALSE for untouched selectize values", {
  expect_false(is_touched(NA_character_, "selectize"))
  expect_false(is_touched("", "selectize"))
  expect_false(is_touched(NA, "selectize"))
})

test_that("is_touched returns TRUE for non-empty selectize values", {
  expect_true(is_touched("some_value", "selectize"))
  expect_true(is_touched("user_created_tag", "selectize"))
})

test_that("is_touched returns FALSE for NULL selectize value", {
  expect_false(is_touched(NULL, "selectize"))
})


# -------------------------------------------------------------------------
# initial_annotations — selectize columns initialise correctly
# -------------------------------------------------------------------------

test_that("initial_annotations creates NA_character_ column for selectize spec", {
  source_data <- data.frame(
    car = c("Civic", "Camry", "Model 3"),
    stringsAsFactors = FALSE
  )
  input_specs <- list(
    list(name = "tag", type = "selectize")
  )

  result <- initial_annotations(source_data, "car", input_specs)

  expect_equal(nrow(result), 3)
  expect_true("tag" %in% names(result))
  expect_true(all(is.na(result$tag)))
  expect_type(result$tag, "character")
})

test_that("initial_annotations handles selectize alongside other types", {
  source_data <- data.frame(
    id = c("a", "b"),
    stringsAsFactors = FALSE
  )
  input_specs <- list(
    list(name = "approved", type = "checkbox"),
    list(name = "tag", type = "selectize"),
    list(name = "score", type = "number")
  )

  result <- initial_annotations(source_data, "id", input_specs)

  expect_equal(nrow(result), 2)
  expect_identical(result$approved, c(FALSE, FALSE))
  expect_identical(result$tag, c(NA_character_, NA_character_))
  expect_identical(result$score, c(NA_real_, NA_real_))
})


# -------------------------------------------------------------------------
# initial_annotations — empty source data edge case
# -------------------------------------------------------------------------

test_that("initial_annotations returns zero-row frame with selectize column", {
  source_data <- data.frame(id = character(0), stringsAsFactors = FALSE)
  input_specs <- list(
    list(name = "tag", type = "selectize")
  )

  result <- initial_annotations(source_data, "id", input_specs)

  expect_equal(nrow(result), 0)
  expect_true("tag" %in% names(result))
})


# -------------------------------------------------------------------------
# merge_annotations — selectize values survive source_data changes
# -------------------------------------------------------------------------

test_that("merge_annotations preserves existing selectize annotations", {
  new_source <- data.frame(
    id = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  input_specs <- list(
    list(name = "tag", type = "selectize")
  )
  existing <- data.frame(
    id = c("a", "b"),
    tag = c("custom_value", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- merge_annotations(new_source, "id", input_specs, existing)

  expect_equal(nrow(result), 3)
  expect_equal(result$tag[result$id == "a"], "custom_value")
  expect_true(is.na(result$tag[result$id == "b"]))
  expect_true(is.na(result$tag[result$id == "c"]))
})


# -------------------------------------------------------------------------
# any_touched — selectize column detection
# -------------------------------------------------------------------------

test_that("any_touched detects rows with touched selectize values", {
  input_specs <- list(
    list(name = "tag", type = "selectize")
  )
  annotations <- data.frame(
    id = c("a", "b", "c"),
    tag = c("picked", NA_character_, ""),
    stringsAsFactors = FALSE
  )

  touched <- any_touched(annotations, input_specs)
  expect_equal(touched, c(TRUE, FALSE, FALSE))
})
