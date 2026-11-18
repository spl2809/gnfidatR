# Small synthetic fixtures. Replace or extend these with real fixtures later.
# Each helper creates fresh data so tests cannot affect one another.

gnfidatR_test_api <- function() {
  list(url = "https://example.invalid/rest/v1/", apikey = "test-key")
}

gnfidatR_no_download <- function(...) {
  stop("Unexpected download in an offline test.", call. = FALSE)
}

gnfidatR_query <- function(
  table = "plot",
  params = list(select = "id", order = "id.asc"),
  ...
) {
  api <- gnfidatR_test_api()

  gnfidatR::gnfi_query(
    table = table,
    params = params,
    url = api$url,
    apikey = api$apikey,
    print_progress = FALSE,
    ...
  )
}

gnfidatR_plot_fixture <- function() {
  data.frame(
    plot_id = c("p1", "p2", "p3"),
    cluster_name = c(101L, 101L, 205L),
    plot_name = c(1L, 2L, 1L),
    federal_state = c(3L, 3L, 9L),
    property_type = c(1L, 2L, 1L),
    acquisition_date = c("2022-06-01", "2022-06-02", "2022-06-03"),
    property_size_class = c(NA_integer_, 2L, 3L),
    management_type = c(1L, 1L, 2L),
    harvest_restriction = c(0L, 0L, 1L),
    stand_layer_regeneration = c(1L, 2L, 1L),
    sampling_stratum = c(10L, 10L, 20L)
  )
}

gnfidatR_tree_fixture <- function() {
  trees <- gnfidatR_plot_fixture()[c(1L, 1L, 2L), ]
  rownames(trees) <- NULL

  trees$intkey <- c("bwi2022-101-1-1", "bwi2022-101-1-2", "bwi2022-101-2-1")
  trees$tree_number <- c(1L, 2L, 1L)
  trees$stand_layer <- c(1L, 2L, 1L)
  trees$tree_species <- c(10L, 20L, 10L)
  trees$stem_breakage <- c(0L, 1L, 0L)
  trees$stem_form <- c(1L, 2L, 1L)
  trees$tree_age <- c(80L, 60L, 45L)

  trees
}

gnfidatR_regeneration_fixture <- function() {
  regeneration <- gnfidatR_plot_fixture()[c(1L, 3L), ]
  rownames(regeneration) <- NULL

  regeneration$intkey <- c("bwi2022-101-1-r1", "bwi2022-205-1-r1")
  regeneration$tree_species <- c(10L, 30L)
  regeneration$tree_size_class <- c(1L, 2L)
  regeneration$tree_count <- c(4L, 7L)

  regeneration
}

gnfidatR_derived_fixture <- function() {
  data.frame(
    intkey = gnfidatR_tree_fixture()$intkey,
    dbh = c(300, 250, 200),
    tree_height = c(28, 24, 20),
    stem_height = c(14, 12, 10),
    trees_per_hectare = c(20, 30, 40),
    diameter_30perc = c(220, 180, 140)
  )
}

gnfidatR_nested_fixture <- function(child = "tree") {
  records <- switch(
    child,
    tree = gnfidatR_tree_fixture(),
    regeneration = gnfidatR_regeneration_fixture(),
    stop("Unknown fixture child.")
  )

  child_columns <- setdiff(names(records), names(gnfidatR_plot_fixture()))
  plot_ids <- unique(records$plot_id)
  plots <- gnfidatR_plot_fixture()

  nested_plots <- plots[match(plot_ids, plots$plot_id), , drop = FALSE]
  nested_plots[[child]] <- lapply(
    plot_ids,
    function(plot_id) {
      records[records$plot_id == plot_id, child_columns, drop = FALSE]
    }
  )
  rownames(nested_plots) <- NULL

  nested_plots
}






# Build real httr2_response objects without using the HTTP transport.
gnfidatR_response <- function(body = "", range = "*/0", status = 200L, type = "text/csv") {
  headers <- list(`Content-Type` = paste0(type, "; charset=UTF-8"))

  if (!is.null(range)) {
    headers[["Content-Range"]] <- range
  }

  httr2::response(
    status_code = as.integer(status),
    url = "https://example.invalid/rest/v1/plot",
    headers = headers,
    body = charToRaw(enc2utf8(body))
  )
}

# Record requests and fail if a test makes more requests than expected.
gnfidatR_transport <- function(responses) {
  transport <- new.env(parent = emptyenv())
  transport$requests <- list()

  transport$perform <- function(req) {
    request_index <- length(transport$requests) + 1L
    transport$requests[[request_index]] <- req

    if (request_index > length(responses)) {
      stop("Unexpected additional HTTP request.", call. = FALSE)
    }

    responses[[request_index]]
  }

  transport
}

gnfidatR_fetch_fixture_pages <- function(
  transport,
  page_size = 2L,
  params = list(select = "*", order = "id.asc"),
  accept = "text/csv"
) {
  api <- gnfidatR_test_api()

  gnfidatR:::fetch_pages(
    params = params,
    endpoint = "plot",
    url = api$url,
    apikey = api$apikey,
    page_size = page_size,
    accept = accept,
    .perform = transport$perform
  )
}
