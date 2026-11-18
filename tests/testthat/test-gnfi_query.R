test_that("gnfi_query returns data and forwards query settings", {
  expected <- data.frame(id = c("p1", "p2"), value = c(10, 20))
  captured_arguments <- NULL

  local_mocked_bindings(
    fetch_single_table = function(...) {
      captured_arguments <<- list(...)
      expected
    },
    .package = "gnfidatR"
  )

  actual <- gnfidatR_query(params = list(select = "id,value", order = "id.asc"), limit = -1)

  expect_identical(actual, expected)
  expect_identical(captured_arguments$endpoint, "plot")
  expect_identical(captured_arguments$params$select, "id,value")
  expect_identical(captured_arguments$limit, 10000L)
  expect_identical(captured_arguments$accept_profile, "inventory_archive")
})

test_that("gnfi_query rejects invalid queries before downloading", {
  local_mocked_bindings(fetch_single_table = gnfidatR_no_download, .package = "gnfidatR")

  expect_error(gnfidatR_query(table = "Bad Table"), "valid identifier")
  expect_error(gnfidatR_query(params = list(order = "id")), "select")
  expect_error(gnfidatR_query(limit = 0), "positive whole number")
  expect_error(
    gnfidatR_query(
      create_csv = FALSE,
      create_sqlite = FALSE,
      return_data = FALSE
    ),
    "At least one"
  )
  expect_error(gnfidatR_query(filtering_options = list(unknown = 1)), "Unsupported")
})

test_that("gnfi_query adds and removes its private plot filter key", {
  captured_arguments <- list()
  downloaded <- data.frame(gnfi_filter_key = c("p1", "p2", "p1"), age = c(10, 20, 30))

  local_mocked_bindings(
    fetch_single_table = function(...) {
      captured_arguments$download <<- list(...)
      downloaded
    },
    get_filtered_plots = function(...) {
      captured_arguments$filter <<- list(...)
      data.frame(plot_id = c("p1", "p1"), cluster_id = "c1")
    },
    .package = "gnfidatR"
  )

  actual <- gnfidatR_query(
    params = list(select = "age:stand_age"),
    filtering_options = list(federal_states = 3)
  )

  expect_identical(actual$age, c(10, 30))
  expect_named(actual, "age")
  expect_identical(
    captured_arguments$download$params$select,
    "gnfi_filter_key:id,age:stand_age"
  )
  expect_identical(captured_arguments$download$params$order, "id.asc")
  expect_false(captured_arguments$filter$include_tree_ids)
})

test_that("gnfi_query requests tree IDs for tree-coordinate filters", {
  captured_arguments <- NULL

  local_mocked_bindings(
    fetch_single_table = function(...) {
      data.frame(gnfi_filter_key = c("t1", "t2"), x = 1:2)
    },
    get_filtered_plots = function(...) {
      captured_arguments <<- list(...)
      data.frame(plot_id = "p1", cluster_id = "c1", tree_id = "t2")
    },
    .package = "gnfidatR"
  )

  actual <- gnfidatR_query(
    table = "tree_coordinates",
    params = list(select = "x"),
    filtering_options = list(federal_states = 3)
  )

  expect_identical(actual$x, 2L)
  expect_true(captured_arguments$include_tree_ids)
})

test_that("gnfi_query filters clusters by cluster ID", {
  local_mocked_bindings(
    fetch_single_table = function(...) {
      data.frame(gnfi_filter_key = c("c1", "c2"), value = 1:2)
    },
    get_filtered_plots = function(...) {
      data.frame(plot_id = "p9", cluster_id = "c2")
    },
    .package = "gnfidatR"
  )

  actual <- gnfidatR_query(
    table = "cluster",
    params = list(select = "value"),
    filtering_options = list(federal_states = 3)
  )

  expect_identical(actual$value, 2L)
})

test_that("gnfi_query reports missing internal filter keys", {
  local_mocked_bindings(
    fetch_single_table = function(...) {
      data.frame(id = "p1")
    },
    get_filtered_plots = gnfidatR_no_download,
    .package = "gnfidatR"
  )

  expect_error(
      gnfidatR_query(filtering_options = list(federal_states = 3)),
    "missing the internal filter key"
  )
})

test_that("gnfi_query warns and returns invisibly for empty endpoint results", {
  local_mocked_bindings(
    fetch_single_table = function(...) {
      data.frame()
    },
    .package = "gnfidatR"
  )

  expect_warning(result <- withVisible(gnfidatR_query()), "returned no data")
  expect_s3_class(result$value, "data.frame")
  expect_equal(nrow(result$value), 0L)
  expect_false(result$visible)
})

test_that("gnfi_query preserves public columns when no filtered rows remain", {
  local_mocked_bindings(
    fetch_single_table = function(...) {
      data.frame(gnfi_filter_key = "p1", value = 10)
    },
    get_filtered_plots = function(...) {
      data.frame(plot_id = character(), cluster_id = character())
    },
    .package = "gnfidatR"
  )

  expect_warning(
    result <- withVisible(
      gnfidatR_query(
        params = list(select = "value"),
        filtering_options = list(federal_states = 3)
      )
    ),
    "No rows remain"
  )
  expect_equal(nrow(result$value), 0L)
  expect_named(result$value, "value")
  expect_false(result$visible)
})

test_that("gnfi_query writes CSV values that can be read back", {
  expected <- data.frame(id = c("p1", "p2"), value = c(10, 20))

  local_mocked_bindings(
    fetch_single_table = function(...) {
      expected
    },
    .package = "gnfidatR"
  )

  path <- file.path(withr::local_tempdir(), "plots")

  expect_message(
    actual <- gnfidatR_query(create_csv = TRUE, csv_name = path),
    "CSV file written"
  )
  expect_identical(actual, expected)

  csv <- readr::read_csv(paste0(path, ".csv"), show_col_types = FALSE)

  expect_identical(csv$id, expected$id)
  expect_equal(csv$value, expected$value)
})

test_that("gnfi_query replaces SQLite output and preserves unrelated tables", {
  expected <- data.frame(id = c("p1", "p2"), value = c(10, 20))
  path <- file.path(withr::local_tempdir(), "plots.sqlite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(connection, "keep_me", data.frame(value = 99L))
  DBI::dbWriteTable(connection, "plots_2022", data.frame(old = 1L))
  DBI::dbDisconnect(connection)

  local_mocked_bindings(
    fetch_single_table = function(...) {
      expected
    },
    .package = "gnfidatR"
  )

  invisible(
    capture.output(
      result <- withVisible(
        gnfidatR_query(
          create_sqlite = TRUE,
          sqlite_name = path,
          sqlite_table = "plots_2022",
          return_data = FALSE
        )
      )
    )
  )

  expect_null(result$value)
  expect_false(result$visible)

  connection <- DBI::dbConnect(RSQLite::SQLite(), path)
  withr::defer(DBI::dbDisconnect(connection))

  expect_equal(DBI::dbReadTable(connection, "plots_2022"), expected)
  expect_identical(DBI::dbReadTable(connection, "keep_me")$value, 99L)
})

test_that("gnfi_query creates CSV and SQLite output in one call", {
  expected <- data.frame(id = "p1", value = 10)

  local_mocked_bindings(
    fetch_single_table = function(...) {
      expected
    },
    .package = "gnfidatR"
  )

  directory <- withr::local_tempdir()
  csv_path <- file.path(directory, "plots.csv")
  db_path <- file.path(directory, "plots.db")
  invisible(
    capture.output(
      suppressMessages(
        gnfidatR_query(
          create_csv = TRUE,
          csv_name = csv_path,
          create_sqlite = TRUE,
          sqlite_name = db_path,
          sqlite_table = "plots"
        )
      )
    )
  )

  expect_true(file.exists(csv_path))

  connection <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  withr::defer(DBI::dbDisconnect(connection))

  expect_equal(DBI::dbReadTable(connection, "plots"), expected)
})

test_that("gnfi_query writes public CSV headers for empty filtered results", {
  local_mocked_bindings(
    fetch_single_table = function(...) {
      data.frame(gnfi_filter_key = "p1", value = 10)
    },
    get_filtered_plots = function(...) {
      data.frame(plot_id = character(), cluster_id = character())
    },
    .package = "gnfidatR"
  )

  path <- file.path(withr::local_tempdir(), "empty.csv")

  expect_warning(
    suppressMessages(
      gnfidatR_query(
        params = list(select = "value"),
        create_csv = TRUE,
        csv_name = path,
        filtering_options = list(federal_states = 3)
      )
    ),
    "No rows remain"
  )
  expect_identical(readLines(path), "value")
})
