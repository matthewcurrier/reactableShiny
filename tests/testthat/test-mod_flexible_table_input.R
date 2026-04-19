# ---------------------------------------------------------------------------
# make_flexible_col_def dispatcher
# ---------------------------------------------------------------------------

test_that("make_flexible_col_def dispatches to select renderer when type is 'select'", {
  spec <- list(
    name = "fruit",
    label = "Fruit",
    type = "select",
    choices = c("Apple" = "apple")
  )
  col <- make_flexible_col_def(
    spec,
    table_data = reactive(make_row(1, list(spec))),
    ns = identity
  )
  expect_s3_class(col, "colDef")
})

test_that("make_flexible_col_def dispatches to select renderer when type is absent (backward compat)", {
  spec <- list(name = "fruit", label = "Fruit", choices = c("Apple" = "apple"))
  col <- make_flexible_col_def(
    spec,
    table_data = reactive(make_row(1, list(spec))),
    ns = identity
  )
  expect_s3_class(col, "colDef")
})

test_that("make_flexible_col_def dispatches to text renderer when type is 'text'", {
  spec <- list(name = "notes", label = "Notes", type = "text")
  col <- make_flexible_col_def(
    spec,
    table_data = reactive(make_row(1, list(spec))),
    ns = identity
  )
  expect_s3_class(col, "colDef")
})

test_that("make_flexible_col_def dispatches to date renderer when type is 'date'", {
  spec <- list(name = "dob", label = "Date of Birth", type = "date")
  col <- make_flexible_col_def(
    spec,
    table_data = reactive(make_row(1, list(spec))),
    ns = identity
  )
  expect_s3_class(col, "colDef")
})

test_that("make_flexible_col_def errors informatively on unknown type", {
  spec <- list(name = "x", label = "X", type = "slider")
  expect_error(
    make_flexible_col_def(
      spec,
      table_data = reactive(make_row(1, list(spec))),
      ns = identity
    ),
    regexp = "Unknown column type"
  )
})

# ---------------------------------------------------------------------------
# Server integration — text and date cols don't error on startup
# ---------------------------------------------------------------------------

test_that("server starts cleanly with a text col_spec", {
  specs <- list(list(name = "notes", label = "Notes", type = "text"))
  expect_no_error(
    shiny::testServer(flexible_table_server, args = list(col_specs = specs), {
      session$getReturned()
    })
  )
})

test_that("server starts cleanly with a date col_spec", {
  specs <- list(list(name = "dob", label = "Date", type = "date"))
  expect_no_error(
    shiny::testServer(flexible_table_server, args = list(col_specs = specs), {
      session$getReturned()
    })
  )
})

test_that("server returns no rows on startup for text col_spec", {
  specs <- list(list(name = "notes", label = "Notes", type = "text"))
  shiny::testServer(flexible_table_server, args = list(col_specs = specs), {
    result <- session$getReturned()
    expect_equal(nrow(result()), 0)
  })
})

test_that("server returns no rows on startup for date col_spec", {
  specs <- list(list(name = "dob", label = "Date", type = "date"))
  shiny::testServer(flexible_table_server, args = list(col_specs = specs), {
    result <- session$getReturned()
    expect_equal(nrow(result()), 0)
  })
})

test_that("server handles mixed select/text/date col_specs without error", {
  specs <- list(
    list(
      name = "fruit",
      label = "Fruit",
      type = "select",
      choices = c("Apple" = "apple", "Pear" = "pear")
    ),
    list(name = "notes", label = "Notes", type = "text"),
    list(name = "picked", label = "Picked On", type = "date")
  )
  expect_no_error(
    shiny::testServer(flexible_table_server, args = list(col_specs = specs), {
      session$getReturned()
    })
  )
})
