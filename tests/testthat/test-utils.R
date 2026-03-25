test_that("make_row returns a one-row data frame with correct columns", {
  specs <- list(
    list(name = "fruit", default = NULL),
    list(name = "quantity", default = "1")
  )

  row <- make_row(row_id = 5, col_specs = specs)

  expect_equal(nrow(row), 1)
  expect_equal(row$id, 5)
  expect_equal(row$fruit, "") # NULL default becomes ""
  expect_equal(row$quantity, "1") # explicit default is respected
  expect_equal(is.data.frame(row), TRUE)
  expect_true("delete" %in% names(row))
})


test_that("load_data assigns sequential IDs starting from start_id", {
  specs <- list(list(name = "fruit", default = NULL))
  df <- data.frame(fruit = c("apple", "banana"))

  result <- load_data(df, start_id = 10, col_specs = specs)

  expect_equal(nrow(result), 2)
  expect_equal(result$id, c(10, 11))
  expect_equal(result$fruit, c("apple", "banana"))
})

test_that("resolve_choices returns static choices when no dependency", {
  spec <- list(choices = c("a", "b"), depends_on = NULL)
  expect_equal(resolve_choices(spec, data = NULL, index = 1), c("a", "b"))
})

test_that("resolve_choices calls get_filtered_choices when dependency exists", {
  spec <- list(
    depends_on = "fruit",
    get_filtered_choices = function(val) if (val == "apple") c("red") else c()
  )
  data <- data.frame(fruit = "apple")

  expect_equal(resolve_choices(spec, data, index = 1), c("red"))
})
