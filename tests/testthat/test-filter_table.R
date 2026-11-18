test_that("filter_df keeps matching rows without multiplying them", {
  data <- data.frame(plot_id = c("p2", "p1", "p1", "p3"), value = 1:4)
  ids <- data.frame(plot_id = c("p1", "p1"))

  actual <- filter_df(data_df = data, filter_df = ids)

  expect_identical(actual$value, c(2L, 3L))
  expect_identical(names(actual), names(data))
  expect_identical(data$value, 1:4)
})

test_that("filter_df supports composite keys with different names", {
  data <- data.frame(cluster = c(101L, 101L, 205L), plot = c(1L, 2L, 1L), value = 1:3)
  ids <- data.frame(trakt = 101L, ecke = 2L)

  actual <- filter_df(
    data_df = data,
    filter_df = ids,
    columns_data = c("cluster", "plot"),
    columns_filter = c("trakt", "ecke")
  )

  expect_identical(actual$value, 2L)
})

test_that("filter_df preserves empty schemas and excludes missing keys", {
  data <- data.frame(plot_id = c("p1", NA_character_), value = 1:2)

  actual <- filter_df(data_df = data, filter_df = data.frame(plot_id = character()))

  expect_equal(nrow(actual), 0L)
  expect_identical(names(actual), names(data))
  expect_type(actual$value, "integer")
  expect_equal(
    nrow(filter_df(data_df = data, filter_df = data.frame(plot_id = NA_character_))),
    0L
  )
})

test_that("gnfi_filter_in quotes and escapes filter values", {
  expect_identical(gnfi_filter_in(values = c(3L, 9L)), 'in.("3","9")')
  expect_identical(gnfi_filter_in(values = c("a,b", "(c)")), 'in.("a,b","(c)")')
  expect_identical(gnfi_filter_in(values = 'a"b'), paste0('in.("a', '\\"', 'b")'))
  expect_identical(gnfi_filter_in(values = "a\\b"), paste0('in.("a', "\\\\", 'b")'))
})

test_that("get_filtered_plots maps filter options to API columns", {
  captured_arguments <- NULL
  expected <- data.frame(plot_id = "p1", cluster_id = "c1")

  local_mocked_bindings(
    fetch_single_table = function(...) {
      captured_arguments <<- list(...)
      expected
    },
    get_cluster = gnfidatR_no_download,
    .package = "gnfidatR"
  )

  api <- gnfidatR_test_api()

  filtering_options <- list(
    interval_name = "bwi2022",
    federal_states = 3,
    growth_district = 7,
    forest_office = 8,
    municipality = 9,
    sampling_stratum = 10,
    property_type = 1,
    property_size_class = 2
  )

  actual <- get_filtered_plots(
    filtering_options = filtering_options,
    url = api$url,
    apikey = api$apikey,
    page_size = 2
  )

  expect_identical(actual, expected)
  expect_identical(captured_arguments$endpoint, "plot")
  expect_identical(captured_arguments$limit, 2L)
  expect_identical(captured_arguments$params$select, "plot_id:id,cluster_id")
  expect_identical(captured_arguments$params$interval_name, 'in.("bwi2022")')

  mapping <- c(
    federal_state = 3,
    growth_district = 7,
    forest_office = 8,
    municipality = 9,
    sampling_stratum = 10,
    property_type = 1,
    property_size_class = 2
  )

  for (column in names(mapping)) {
    expect_identical(
      captured_arguments$params[[column]],
      paste0('in.("', mapping[[column]], '")')
    )
  }
})


test_that("get_filtered_plots returns typed columns for empty results", {
  local_mocked_bindings(
    fetch_single_table = function(...) {
      data.frame()
    },
    .package = "gnfidatR"
  )

  api <- gnfidatR_test_api()

  actual <- get_filtered_plots(url = api$url, apikey = api$apikey)

  expect_identical(actual, data.frame(plot_id = character(), cluster_id = character()))
})

test_that("get_filtered_plots expands tree IDs and forwards cluster options", {
  captured_arguments <- list()
  nested_plots <- data.frame(
    plot_id = c("p1", "p2"),
    cluster_id = c("c1", "c2"),
    cluster_name = c(101L, 205L)
  )
  nested_plots$tree <- list(
    data.frame(tree_id = c("t1", "t2")),
    data.frame(tree_id = "t3")
  )

  local_mocked_bindings(
    get_cluster = function(...) {
      captured_arguments$cluster <<- list(...)
      c(101, 205)
    },
    fetch_join_table = function(...) {
      captured_arguments$join <<- list(...)
      nested_plots
    },
    fetch_single_table = gnfidatR_no_download,
    .package = "gnfidatR"
  )

  api <- gnfidatR_test_api()

  actual <- get_filtered_plots(
    url = api$url,
    apikey = api$apikey,
    include_tree_ids = TRUE,
    include_test_cluster = TRUE,
    page_size = 2,
    step = 1
  )

  expect_identical(actual$tree_id, c("t1", "t2", "t3"))
  expect_identical(actual$plot_id, c("p1", "p1", "p2"))
  expect_true(captured_arguments$cluster$include_test_cluster)
  expect_identical(captured_arguments$join$cluster_list, c(101, 205))
  expect_identical(captured_arguments$join$step, 1L)
})

test_that("get_filtered_plots stops tree downloads when no clusters match", {
  local_mocked_bindings(
    get_cluster = function(...) {
      numeric()
    },
    fetch_join_table = gnfidatR_no_download,
    .package = "gnfidatR"
  )

  api <- gnfidatR_test_api()

  actual <- get_filtered_plots(
    url = api$url,
    apikey = api$apikey,
    include_tree_ids = TRUE
  )

  expect_equal(nrow(actual), 0L)
  expect_named(actual, c("plot_id", "cluster_id", "cluster_name", "tree_id"))
  expect_type(actual$cluster_name, "integer")
})
