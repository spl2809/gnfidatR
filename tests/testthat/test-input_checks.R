test_that("validate_string accepts scalar strings and rejects invalid input", {
  expect_identical(validate_string(value = "plot", argument = "table"), "plot")
  expect_null(validate_string(value = NULL, argument = "table", allow_null = TRUE))
  expect_error(validate_string(argument = "table"), "must be supplied")

  invalid <- list(
    NULL,
    character(),
    "",
    "  ",
    NA_character_,
    1,
    TRUE,
    c("a", "b"),
    factor("a"),
    matrix("a"),
    structure("a", class = "custom")
  )

  for (value in invalid) {
    expect_error(validate_string(value = value, argument = "table"), "table")
  }

  expect_error(
    validate_string(value = "a\nb", argument = "table"),
    "control characters"
  )
})

test_that("validate_flags requires single non-missing logical values", {
  expect_invisible(validate_flags(flags = list(csv = TRUE, sqlite = FALSE)))

  invalid_values <- list(NULL, logical(), NA, 0, "TRUE", c(TRUE, FALSE), matrix(TRUE))

  for (value in invalid_values) {
    expect_error(validate_flags(flags = list(create_csv = value)), "create_csv")
  }
})

test_that("is_whole_number accepts boundaries and rejects invalid numbers", {
  expect_true(is_whole_number(value = 0, lower = 0, upper = 100))
  expect_true(is_whole_number(value = 100L, lower = 0, upper = 100))

  invalid_values <- list(
    -1,
    101,
    0.5,
    NA_real_,
    NaN,
    Inf,
    "1",
    TRUE,
    numeric(),
    c(1, 2),
    matrix(1)
  )

  for (value in invalid_values) {
    expect_false(is_whole_number(value = value, lower = 0, upper = 100))
  }
})

test_that("validate_page_size normalizes integers and the all-rows value", {
  expect_identical(validate_page_size(value = 2), 2L)
  expect_identical(
    validate_page_size(value = .Machine$integer.max),
    .Machine$integer.max
  )
  expect_identical(validate_page_size(value = -1, allow_all = TRUE), 10000L)

  invalid_values <- list(-1, 0, 1.5, Inf, NA_real_, "2", .Machine$integer.max + 1)

  for (value in invalid_values) {
    expect_error(validate_page_size(value = value), "positive whole number")
  }

  expect_error(
    validate_page_size(value = -2, allow_all = TRUE),
    "positive whole number"
  )
})

test_that("validate_named_list requires unique clean names", {
  expect_identical(
    validate_named_list(value = list(a = 1), argument = "params"),
    list(a = 1)
  )
  expect_identical(
    validate_named_list(value = list(), argument = "params", allow_empty = TRUE),
    list()
  )
  expect_error(validate_named_list(argument = "params"), "must be supplied")

  invalid <- list(
    list(),
    list(1),
    c(a = 1),
    data.frame(a = 1),
    setNames(list(1, 2), c("a", "a")),
    setNames(list(1), " a"),
    setNames(list(1), ""),
    setNames(list(1), NA_character_),
    setNames(list(1), "a\nb")
  )

  for (value in invalid) {
    expect_error(validate_named_list(value = value, argument = "params"), "params")
  }
})

test_that("validate_api normalizes HTTPS URLs and rejects unsafe URLs", {
  expect_identical(
    validate_api(url = " https://example.invalid/rest/v1 ", apikey = "key"),
    "https://example.invalid/rest/v1/"
  )
  expect_identical(
    validate_api(url = "https://example.invalid/", apikey = "key"),
    "https://example.invalid/"
  )

  for (url in c(
    "http://example.invalid",
    "https://user:pw@example.invalid/",
    "https://example.invalid/?x=1",
    "https://example.invalid/#x",
    "https://example.invalid/a b",
    "https://example.invalid/\\x"
  )) {
    expect_error(validate_api(url = url, apikey = "key"), "HTTPS base URL")
  }

  expect_error(validate_api(url = "https://example.invalid", apikey = ""), "apikey")
})

test_that("validate_identifier normalizes and checks identifiers", {
  expect_identical(
    validate_identifier(value = " tree_2022 ", argument = "table"),
    "tree_2022"
  )

  for (value in c("Tree", "1tree", "tree-name", "tree;DROP TABLE x")) {
    expect_error(
      validate_identifier(value = value, argument = "table"),
      "valid identifier"
    )
  }

  expect_identical(
    validate_identifier(
      value = "_Trees",
      argument = "table",
      pattern = "^[A-Za-z_][A-Za-z0-9_]*$"
    ),
    "_Trees"
  )
})

test_that("validate_query_params preserves filters and checks parameters", {
  params <- list(
    select = "id,plot_name",
    plot_name = c("gte.1", "lte.4"),
    order = "id.asc",
    flag = FALSE,
    number = 2
  )

  expect_identical(validate_query_params(params = params), params)
  expect_error(validate_query_params(params = list(order = "id")), "select")
  expect_error(
    validate_query_params(params = list(select = c("id", "name"))),
    "params\\$select"
  )
  expect_error(
    validate_query_params(params = list(select = "*", order = c("id", "name"))),
    "params\\$order"
  )

  for (key in c("limit", "offset")) {
    invalid <- list(select = "*")
    invalid[[key]] <- 1

    expect_error(validate_query_params(params = invalid), "Do not put")
  }

  invalid_values <- list(
    NULL,
    character(),
    NA_character_,
    "",
    "a\nb",
    Inf,
    list("eq.1"),
    factor("eq.1"),
    matrix("eq.1")
  )

  for (value in invalid_values) {
    expect_error(
      validate_query_params(params = list(select = "*", id = value)),
      "parameter"
    )
  }
})

test_that("validate_output_path adds extensions without creating files", {
  directory <- withr::local_tempdir()
  path <- file.path(directory, "plots")

  expect_identical(
    validate_output_path(value = path, argument = "csv_name", extension = ".csv"),
    paste0(path, ".csv")
  )
  expect_false(file.exists(paste0(path, ".csv")))

  for (suffix in c(".sqlite", ".sqlite3", ".db", ".DB")) {
    database <- paste0(path, suffix)

    expect_identical(
      validate_output_path(
        value = database,
        argument = "db",
        extension = ".sqlite",
        sqlite = TRUE
      ),
      database
    )
  }

  expect_error(
    validate_output_path(value = directory, argument = "csv_name"),
    "directory, not a file"
  )
  expect_error(
    validate_output_path(
      value = file.path(directory, "missing", "x.csv"),
      argument = "csv_name"
    ),
    "does not exist"
  )
})

test_that("validate_exports requires output and rejects reserved table names", {
  directory <- withr::local_tempdir()

  output <- validate_exports(
    table = "plot",
    create_csv = TRUE,
    csv_name = file.path(directory, "plots"),
    create_sqlite = TRUE,
    sqlite_name = file.path(directory, "data"),
    sqlite_table = "Plots_2022",
    return_data = FALSE
  )

  expect_identical(output$csv_name, file.path(directory, "plots.csv"))
  expect_identical(output$sqlite_name, file.path(directory, "data.sqlite"))
  expect_identical(output$sqlite_table, "Plots_2022")
  expect_error(
    validate_exports(
      table = "plot",
      create_csv = FALSE,
      csv_name = NULL,
      create_sqlite = FALSE,
      sqlite_name = NULL,
      sqlite_table = NULL,
      return_data = FALSE
    ),
    "At least one"
  )
  expect_error(
    validate_exports(
      table = "plot",
      create_csv = FALSE,
      csv_name = NULL,
      create_sqlite = TRUE,
      sqlite_name = file.path(directory, "x.db"),
      sqlite_table = "SQLite_master",
      return_data = TRUE
    ),
    "reserved prefix"
  )
})

test_that("validate_exports ignores paths for inactive exports", {
  output <- validate_exports(
    table = "plot",
    create_csv = FALSE,
    csv_name = "missing/folder/x.csv",
    create_sqlite = FALSE,
    sqlite_name = "missing/folder/x.sqlite",
    sqlite_table = NULL,
    return_data = TRUE
  )

  expect_identical(output$csv_name, "missing/folder/x.csv")
  expect_identical(output$sqlite_name, "missing/folder/x.sqlite")
})

test_that("validate_filtering_options normalizes and removes inactive options", {
  expect_identical(validate_filtering_options(filtering_options = NULL), list())
  expect_identical(validate_filtering_options(filtering_options = list()), list())
  expect_identical(
    validate_filtering_options(filtering_options = list(federal_states = NULL)),
    list()
  )

  actual <- validate_filtering_options(
    filtering_options = list(
      federal_states = c("3", "3", "9"),
      interval_name = c("bwi2022", "bwi2022"),
      property_type = NULL
    )
  )

  expect_identical(actual, list(federal_states = c(3L, 9L), interval_name = "bwi2022"))
  expect_error(
    validate_filtering_options(filtering_options = list(unknown = 1)),
    "Unsupported"
  )

  invalid_values <- list(
    numeric(),
    NA_real_,
    TRUE,
    1.5,
    Inf,
    "1.5",
    " 3",
    "abc",
    .Machine$integer.max + 1,
    matrix(3)
  )

  for (value in invalid_values) {
    expect_error(
      validate_filtering_options(filtering_options = list(federal_states = value)),
      "federal_state"
    )
  }

  invalid_values <- list(2022, "", "a\nb")

  for (value in invalid_values) {
    expect_error(
      validate_filtering_options(filtering_options = list(interval_name = value)),
      "interval_name"
    )
  }
})

test_that("validate_query_filter selects and validates join keys", {
  check_filter_keys <- function(table, options = list(federal_states = 3L)) {
    validate_query_filter(
      table = table,
      params = list(select = "*"),
      accept_profile = "inventory_archive",
      filtering_options = options,
      filter_key_alias = "gnfi_filter_key"
    )
  }

  expect_null(check_filter_keys(table = "unknown", options = list()))
  expect_identical(
    check_filter_keys(table = "cluster"),
    list(data = "id", ids = "cluster_id")
  )
  expect_identical(
    check_filter_keys(table = "cluster_move"),
    list(data = "cluster_id", ids = "cluster_id")
  )
  expect_identical(
    check_filter_keys(table = "plot"),
    list(data = "id", ids = "plot_id")
  )
  expect_identical(
    check_filter_keys(table = "tree"),
    list(data = "plot_id", ids = "plot_id")
  )
  expect_identical(
    check_filter_keys(table = "tree_coordinates"),
    list(data = "tree_id", ids = "tree_id")
  )
  expect_error(check_filter_keys(table = "unknown"), "not supported")
  expect_error(
    validate_query_filter(
      table = "plot",
      params = list(select = "*"),
      accept_profile = "lookup",
      filtering_options = list(federal_states = 3L),
      filter_key_alias = "gnfi_filter_key"
    ),
    "inventory_archive"
  )

  for (select in c("gnfi_filter_key:id", "dbh.sum()", "count()")) {
    expect_error(
      validate_query_filter(
        table = "plot",
        params = list(select = select),
        accept_profile = "inventory_archive",
        filtering_options = list(federal_states = 3L),
        filter_key_alias = "gnfi_filter_key"
      )
    )
  }
})

test_that("WEHAM validators check inventory intervals and regeneration shares", {
  expect_identical(
    validate_weham_interval(interval_name = "bwi2022", filtering_options = list()),
    "bwi2022"
  )
  expect_identical(
    validate_weham_interval(
      interval_name = "bwi2022",
      filtering_options = list(interval_name = "bwi2022")
    ),
    "bwi2022"
  )
  expect_error(
    validate_weham_interval(interval_name = "unknown", filtering_options = list()),
    "must be one of"
  )
  expect_error(
    validate_weham_interval(
      interval_name = "bwi2022",
      filtering_options = list(interval_name = "bwi2012")
    ),
    "must include"
  )

  for (share in c(0, 50, 100)) {
    expect_identical(validate_regeneration_share(value = share), as.integer(share))
  }

  invalid_values <- list(-1, 101, 50.5, NA, "50", c(0, 100))

  for (share in invalid_values) {
    expect_error(validate_regeneration_share(value = share), "between 0 and 100")
  }
})

test_that("validate_cluster_list returns sorted unique numeric IDs", {
  expect_identical(
    validate_cluster_list(cluster_list = c("205", "101", "101")),
    c(101, 205)
  )

  invalid_ids <- list(
    NULL,
    numeric(),
    NA,
    TRUE,
    c(1, NA),
    0,
    -1,
    1.5,
    Inf,
    "unknown",
    .Machine$integer.max + 1,
    matrix(1)
  )

  for (ids in invalid_ids) {
    expect_error(validate_cluster_list(cluster_list = ids), "cluster_list")
  }
})

test_that("validate_filter_df checks data frames and join columns", {
  data <- data.frame(plot_id = c("p1", "p2"), value = 1:2)
  ids <- data.frame(id = "p1")

  expect_invisible(
    validate_filter_df(
      data_df = data,
      filter_df = ids,
      columns_data = "plot_id",
      columns_filter = "id"
    )
  )
  expect_error(
    validate_filter_df(
      data_df = list(),
      filter_df = ids,
      columns_data = "plot_id",
      columns_filter = "id"
    ),
    "data frame"
  )
  expect_error(
    validate_filter_df(
      data_df = data,
      filter_df = ids,
      columns_data = "missing",
      columns_filter = "id"
    ),
    "Missing columns"
  )
  expect_error(
    validate_filter_df(
      data_df = data,
      filter_df = ids,
      columns_data = c("plot_id", "value"),
      columns_filter = "id"
    ),
    "equal length"
  )
  expect_error(
    validate_filter_df(
      data_df = data,
      filter_df = ids,
      columns_data = c("plot_id", "plot_id"),
      columns_filter = c("id", "id")
    ),
    "duplicates"
  )

  duplicate_column_data <- data
  names(duplicate_column_data) <- c("plot_id", "plot_id")

  expect_error(
    validate_filter_df(
      data_df = duplicate_column_data,
      filter_df = ids,
      columns_data = "plot_id",
      columns_filter = "id"
    ),
    "column names"
  )
})
