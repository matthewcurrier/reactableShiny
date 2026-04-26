# tests/testthat/test-mod_annotator_super_simple.R
#
# Tests for annotator_super_simple_server.
#
# The module has no pure utility functions — all logic lives in three
# reactive components:
#   1. The sync observer  — maps getReactableState indices → character IDs
#   2. The return reactive — intersects selected_ids() with visible rows
#   3. The reset path     — NULL from reactable clears visible-row selections
#
# In testServer, getReactableState("table", "selected", session) reads from
# session$input[["table__reactable__selected"]]. We drive it with setInputs.
#
# ignoreInit = TRUE / testServer interaction
# ------------------------------------------
# The sync observer has ignoreInit = TRUE, which suppresses the observer
# body on its first execution. testServer does not flush the reactive graph
# between module setup and the start of the expr block, so the observer's
# "init" is consumed by the first setInputs call rather than at startup.
# set_reactable_selected therefore issues two calls:
#   1. integer(0) — consumes ignoreInit (observer body suppressed)
#   2. actual indices — fires the observer body for real
# integer(0) is safe as the primer: visible_ids[integer(0)] = character(0),
# so the setdiff/union logic in the observer is a no-op against any
# existing selected_ids state.

library(testthat)
library(shiny)


# -----------------------------------------------------------------------
# Shared fixtures
# -----------------------------------------------------------------------

make_src <- function(ids = c("a", "b", "c")) {
  data.frame(
    id    = ids,
    label = paste0("Item ", ids),
    stringsAsFactors = FALSE
  )
}

# Simulate reactable reporting a selection (or NULL) to Shiny.
# Two calls are required: the first consumes the ignoreInit suppression on
# the sync observer; the second fires the observer body with the real value.
set_reactable_selected <- function(session, indices) {
  do.call(session$setInputs, list("table__reactable__selected" = integer(0)))
  do.call(session$setInputs, stats::setNames(list(indices), "table__reactable__selected"))
}

std_args <- function(src, ...) {
  list(
    source_data  = if (inherits(src, "reactiveVal")) src else reactive(src),
    row_id       = "id",
    display_cols = "label",
    ...
  )
}


# =======================================================================
# Test 1 — Baseline contract: module starts with no selections
# =======================================================================
#
# Verifies the return value is character(0) before the user has interacted
# with the table. This pins the module's initial contract and ensures
# selected_ids() is not accidentally seeded with stale data on startup.

test_that("module returns character(0) at startup before any selection", {
  testServer(
    annotator_super_simple_server,
    args = std_args(make_src()),
    {
      result <- session$getReturned()
      expect_identical(result(), character(0))
    }
  )
})


# =======================================================================
# Test 2 — Sync observer: integer indices map to correct character IDs
# =======================================================================
#
# The sync observer receives 1-based integer indices from reactable and
# must translate them to the character values of the row_id column.
# This is the module's core transformation; a bug here breaks everything.
# We verify both the correct IDs are returned AND that un-selected rows
# are absent.

test_that("sync observer maps reactable indices to correct character row IDs", {
  testServer(
    annotator_super_simple_server,
    args = std_args(make_src(c("a", "b", "c"))),
    {
      result <- session$getReturned()

      # Select rows 1 and 3 ("a" and "c"); "b" must not appear
      set_reactable_selected(session, c(1L, 3L))

      expect_setequal(result(), c("a", "c"))
      expect_false("b" %in% result())
    }
  )
})


# =======================================================================
# Test 3 — Persistence: selections survive a source_data change
# =======================================================================
#
# selected_ids() is stored independently of source_data so that a reactive
# re-render of the table (e.g. a filter upstream) does not wipe the user's
# selections. After source_data changes, selected IDs that are still present
# in the new data must still appear in the return value.
#
# In testServer there is no real reactable JS, so we do not re-fire the
# sync observer after the data change — we rely solely on selected_ids()
# persisting and the return reactive intersecting it with the new visible
# rows. That is exactly the code path that protects against data changes.

test_that("selected IDs persist in return value when source_data changes", {
  src_rv <- reactiveVal(make_src(c("a", "b", "c")))

  testServer(
    annotator_super_simple_server,
    args = std_args(src_rv),
    {
      result <- session$getReturned()

      # Select "b"
      set_reactable_selected(session, 2L)
      expect_equal(result(), "b")

      # Source data changes (e.g. a new column is added upstream); "b" stays
      src_rv(make_src(c("a", "b", "d")))
      session$flushReact()

      expect_equal(result(), "b")
    }
  )
})


# =======================================================================
# Test 4 — Reset path: NULL from reactable clears visible-row selections
# =======================================================================
#
# The reset button calls updateReactable(selected = NA), which causes
# reactable to emit NULL from getReactableState. The sync observer's
# NULL branch must clear any visible-row IDs from selected_ids(). We
# simulate this by firing the reset button and then driving
# getReactableState to NULL, mirroring what the real browser would do.

test_that("reset clears all selections when reactable reports NULL", {
  testServer(
    annotator_super_simple_server,
    args = std_args(make_src()),
    {
      result <- session$getReturned()

      # Establish a selection
      set_reactable_selected(session, c(1L, 2L))
      expect_length(result(), 2)

      # Trigger reset button, then simulate reactable confirming the clear
      session$setInputs(reset = 1L)
      set_reactable_selected(session, NULL)

      expect_identical(result(), character(0))
    }
  )
})


# =======================================================================
# Test 5 — Hidden-row preservation: IDs survive source_data filtering
# =======================================================================
#
# This is the subtlest invariant in the module. When source_data is filtered
# to exclude a selected row:
#   a) The return value must NOT include that ID (it is not visible).
#   b) selected_ids() must RETAIN the ID internally.
#   c) When the row reappears in source_data, the return value must restore it.
#
# The NULL branch of the sync observer implements (b): it calls
#   selected_ids(setdiff(current, visible_ids))
# which removes only IDs that ARE visible but deselected — it never
# removes IDs for rows that are absent from source_data entirely.

test_that("ID for a filtered-out row is excluded from return value but restored when row reappears", {
  src_rv <- reactiveVal(make_src(c("a", "b", "c")))

  testServer(
    annotator_super_simple_server,
    args = std_args(src_rv),
    {
      result <- session$getReturned()

      # Select "b" (row index 2 in the full table)
      set_reactable_selected(session, 2L)
      expect_equal(result(), "b")

      # Filter "b" out of source_data; reactable would rerender with no
      # selection — simulate it reporting NULL
      src_rv(make_src(c("a", "c")))
      set_reactable_selected(session, NULL)
      session$flushReact()

      # (a) "b" must not appear in the return value — it is not visible
      expect_identical(result(), character(0))

      # Restore "b" — no JS interaction needed; the return reactive
      # re-evaluates intersect(selected_ids(), visible_ids) automatically
      src_rv(make_src(c("a", "b", "c")))
      session$flushReact()

      # (b) + (c) "b" is back in source_data and was preserved in selected_ids
      expect_equal(result(), "b")
    }
  )
})


# =======================================================================
# Test 6 — Reset clears hidden-row selections, not just visible ones
# =======================================================================
#
# This test catches the bug where the reset path relied solely on the sync
# observer's NULL branch (setdiff(current, visible_ids)) to clear state.
# That branch intentionally preserves hidden-row IDs as part of the normal
# persistence logic, so a reset triggered while rows were hidden would leave
# those IDs in selected_ids() — they would reappear when the filter changed
# back. The fix clears selected_ids() unconditionally in the reset observer
# before calling updateReactable.
#
# Sequence:
#   1. Select "a" and "b" while all rows are visible
#   2. Filter source_data to only "c" (hiding "a" and "b")
#   3. Simulate reactable reporting NULL (re-render with no selection visible)
#   4. Reset — must clear "a" and "b" from selected_ids even though hidden
#   5. Restore full source_data — return value must still be character(0)

test_that("reset clears selections for hidden rows, not just visible ones", {
  src_rv <- reactiveVal(make_src(c("a", "b", "c")))

  testServer(
    annotator_super_simple_server,
    args = std_args(src_rv),
    {
      result <- session$getReturned()

      # Select "a" and "b" while all three rows are visible
      set_reactable_selected(session, c(1L, 2L))
      expect_setequal(result(), c("a", "b"))

      # Filter down to only "c" — "a" and "b" are now hidden
      src_rv(make_src(c("c")))
      # Reactable re-renders with no matching selections; simulate NULL
      set_reactable_selected(session, NULL)
      session$flushReact()

      # Sanity check: return value correctly shows nothing visible selected
      expect_identical(result(), character(0))

      # Reset while "a" and "b" are hidden — this is the critical step
      session$setInputs(reset = 1L)
      set_reactable_selected(session, NULL)

      # Restore full data — if reset correctly cleared hidden IDs,
      # "a" and "b" must NOT reappear in the return value
      src_rv(make_src(c("a", "b", "c")))
      session$flushReact()

      expect_identical(result(), character(0))
    }
  )
})
