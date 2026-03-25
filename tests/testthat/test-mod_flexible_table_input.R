# The standard case — blank table on startup returns no rows
test_that("returns no rows on startup before any selection is made", {
  specs <- list(list(
    name = "fruit",
    label = "Fruit",
    choices = c("Apple" = "apple", "Pear" = "pear")
  ))

  shiny::testServer(flexible_table_server, args = list(col_specs = specs), {
    result <- session$getReturned()
    expect_equal(nrow(result()), 0)
  })
})

# Edge case — NULL choices, verify it doesn't error on startup
test_that("does not error on startup when choices is NULL", {
  specs <- list(list(
    name = "fruit",
    label = "Fruit",
    choices = NULL
  ))

  expect_no_error(
    shiny::testServer(flexible_table_server, args = list(col_specs = specs), {
      session$getReturned()
    })
  )
})


test_that("errors informatively when col_specs is NULL", {
  expect_error(
    shiny::testServer(flexible_table_server, args = list(col_specs = NULL), {
      session$getReturned()
    }),
    regexp = "col_specs" # checks the error message mentions col_specs
  )
})
