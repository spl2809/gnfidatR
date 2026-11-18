test_that("explode_column keeps parent IDs and duplicate child rows", {
  parent_records <- data.frame(plot_id = c("p1", "p2"))
  parent_records$tree <- list(data.frame(value = c(10L, 10L)), data.frame(value = 20L))

  actual <- explode_column(df = parent_records, col = "tree")

  expect_identical(actual$plot_id, c("p1", "p1", "p2"))
  expect_identical(actual$value, c(10L, 10L, 20L))
  expect_false("tree" %in% names(actual))
})

test_that("explode_column converts NULL child fields to missing values", {
  child_records <- data.frame(number = 1:2)
  child_records$value <- list(10, NULL)

  parent_records <- data.frame(plot_id = "p1")
  parent_records$tree <- list(child_records)

  actual <- explode_column(df = parent_records, col = "tree")

  expect_equal(actual$value, c(10, NA_real_))
  expect_identical(actual$number, 1:2)
})

test_that("explode_column drops parents with empty child tables", {
  parent_records <- data.frame(plot_id = c("p1", "p2", "p3"))
  parent_records$tree <- list(
    data.frame(value = 1L),
    data.frame(value = integer()),
    NULL
  )

  actual <- explode_column(df = parent_records, col = "tree")

  expect_identical(actual$plot_id, "p1")
  expect_identical(actual$value, 1L)
})

test_that("log_step prints a timestamp and returns NULL invisibly", {
  expected_pattern <- "^\\[[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9:]{8}\\] Starting test$"

  output <- capture.output(result <- withVisible(log_step(msg = "Starting test")))

  expect_match(output, expected_pattern)
  expect_null(result$value)
  expect_false(result$visible)
})

test_that("log_step can include elapsed time", {
  start_time <- proc.time()[["elapsed"]]

  expect_output(
    log_step(msg = "Finished", start_time = start_time),
    "Finished \\([0-9]+[.,][0-9]{2} s\\)"
  )
})
