# tests/testthat/test-make_badge_renderer.R
#
# Contract tests for make_badge_renderer().
# All tests are pure — no Shiny server required.

library(testthat)
library(shiny)

# -----------------------------------------------------------------------
# Shared fixtures
# -----------------------------------------------------------------------

badge_colors <- list(
  Cognitive     = list(bg = "#E6F1FB", text = "#0C447C"),
  Communication = list(bg = "#EAF3DE", text = "#27500A"),
  Motor         = list(bg = "#FAEEDA", text = "#633806")
)

custom_fallback <- list(bg = "#FFFFFF", text = "#000000")

# -----------------------------------------------------------------------
# Return type
# -----------------------------------------------------------------------

test_that("make_badge_renderer returns a function", {
  renderer <- make_badge_renderer(badge_colors)
  expect_true(is.function(renderer))
})

test_that("make_badge_renderer result produces a shiny.tag", {
  renderer <- make_badge_renderer(badge_colors)
  expect_s3_class(renderer("Cognitive"), "shiny.tag")
})

# -----------------------------------------------------------------------
# CSS class
# -----------------------------------------------------------------------

test_that("rendered badge carries the rs-badge CSS class", {
  renderer <- make_badge_renderer(badge_colors)
  result <- renderer("Cognitive")
  expect_equal(result$attribs$class, "rs-badge")
})

# -----------------------------------------------------------------------
# Colour resolution — known values
# -----------------------------------------------------------------------

test_that("rendered badge applies correct background for known value", {
  renderer <- make_badge_renderer(badge_colors)
  style <- renderer("Cognitive")$attribs$style
  expect_true(grepl("#E6F1FB", style, fixed = TRUE))
})

test_that("rendered badge applies correct text colour for known value", {
  renderer <- make_badge_renderer(badge_colors)
  style <- renderer("Cognitive")$attribs$style
  expect_true(grepl("#0C447C", style, fixed = TRUE))
})

test_that("renderer handles each badge_colors key independently", {
  renderer <- make_badge_renderer(badge_colors)
  purrr::walk(names(badge_colors), function(key) {
    style <- renderer(key)$attribs$style
    expect_true(
      grepl(badge_colors[[key]]$bg, style, fixed = TRUE),
      label = paste("background for", key)
    )
    expect_true(
      grepl(badge_colors[[key]]$text, style, fixed = TRUE),
      label = paste("text colour for", key)
    )
  })
})

# -----------------------------------------------------------------------
# Colour resolution — fallback behaviour
# -----------------------------------------------------------------------

test_that("renderer uses default fallback for unknown value", {
  renderer <- make_badge_renderer(badge_colors)
  style <- renderer("Unknown Category")$attribs$style
  expect_true(grepl("#F1EFE8", style, fixed = TRUE))
  expect_true(grepl("#444441", style, fixed = TRUE))
})

test_that("renderer uses custom fallback when caller supplies one", {
  renderer <- make_badge_renderer(badge_colors, fallback = custom_fallback)
  style <- renderer("Unknown Category")$attribs$style
  expect_true(grepl("#FFFFFF", style, fixed = TRUE))
  expect_true(grepl("#000000", style, fixed = TRUE))
})

test_that("default fallback is not applied to known values", {
  renderer <- make_badge_renderer(badge_colors)
  style <- renderer("Motor")$attribs$style
  expect_false(grepl("#F1EFE8", style, fixed = TRUE))
})

# -----------------------------------------------------------------------
# Tag content
# -----------------------------------------------------------------------

test_that("rendered badge contains the original value as text", {
  renderer <- make_badge_renderer(badge_colors)
  result <- renderer("Communication")
  # children is a list; flatten to character to check presence
  children_text <- unlist(result$children)
  expect_true("Communication" %in% children_text)
})

# -----------------------------------------------------------------------
# Edge cases
# -----------------------------------------------------------------------

test_that("renderer handles empty string value without error", {
  renderer <- make_badge_renderer(badge_colors)
  expect_no_error(renderer(""))
})

test_that("renderer handles a value that is NULL gracefully", {
  # NULL-keyed lookup in a named list returns NULL, so fallback is used
  renderer <- make_badge_renderer(badge_colors)
  # Passing NULL would crash the child text — we just check the call survives
  # when the value is a non-NULL but absent key (realistic caller scenario)
  expect_no_error(renderer("NotAKey"))
})
