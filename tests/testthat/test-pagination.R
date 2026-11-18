test_that("fetch_pages infers CSV types after combining pages", {
  responses <- list(
    gnfidatR_response(body = "id,value\n1,10\n3,NULL\n3,-1.6\n", range = "0-2/6"),
    gnfidatR_response(body = "id,value\n4,true\n5,FALSE\n", range = "3-4/6"),
    gnfidatR_response(body = "id,value\n2,text\n", range = "5-5/6")
  )

  transport <- gnfidatR_transport(responses = responses)

  actual <- gnfidatR_fetch_fixture_pages(transport = transport)

  expect_identical(
    actual$value, c("10", "NULL", "-1.6", "true", "FALSE", "text")
  )
})


test_that("fetch_pages advances CSV offsets by the received row count", {
  responses <- list(
    gnfidatR_response(body = "id,value\n1,10\n2,20\n", range = "0-1/5", status = 206L),
    gnfidatR_response(body = "id,value\n3,30\n4,40\n", range = "2-3/5", status = 206L),
    gnfidatR_response(body = "id,value\n5,50\n", range = "4-4/5")
  )

  transport <- gnfidatR_transport(responses = responses)

  actual <- gnfidatR_fetch_fixture_pages(transport = transport, page_size = 100L)

  expect_equal(actual$id, 1:5)
  expect_equal(actual$value, c(10, 20, 30, 40, 50))
  expect_length(transport$requests, 3L)

  expect_match(transport$requests[[1]]$url, "offset=0")
  expect_match(transport$requests[[2]]$url, "offset=2")
  expect_match(transport$requests[[3]]$url, "offset=4")

  expect_match(transport$requests[[1]]$url, "limit=100")
  expect_match(transport$requests[[2]]$url, "limit=100")
  expect_match(transport$requests[[3]]$url, "limit=100")

  expect_identical(
    transport$requests[[1]]$headers[["Accept-Profile"]],
    "inventory_archive"
  )
  expect_identical(
    transport$requests[[2]]$headers[["Accept-Profile"]],
    "inventory_archive"
  )
  expect_identical(
    transport$requests[[3]]$headers[["Accept-Profile"]],
    "inventory_archive"
  )

  expect_identical(transport$requests[[1]]$headers[["apikey"]], "test-key")
  expect_identical(transport$requests[[2]]$headers[["apikey"]], "test-key")
  expect_identical(transport$requests[[3]]$headers[["apikey"]], "test-key")

  expect_identical(transport$requests[[1]]$headers[["Prefer"]], "count=exact")
  expect_null(transport$requests[[2]]$headers[["Prefer"]])
  expect_null(transport$requests[[3]]$headers[["Prefer"]])
})


test_that("fetch_pages combines JSON rows and keeps optional fields", {
  responses <- list(
    gnfidatR_response(
      body = '[{"id":1,"value":10}]',
      range = "0-0/2",
      type = "application/json"
    ),
    gnfidatR_response(
      body = '[{"id":2,"extra":20}]',
      range = "1-1/2",
      type = "application/json"
    )
  )

  transport <- gnfidatR_transport(responses = responses)

  actual <- gnfidatR_fetch_fixture_pages(
    transport = transport, accept = "application/json"
  )

  expect_equal(actual$id, 1:2)
  expect_equal(actual$value, c(10, NA))
  expect_equal(actual$extra, c(NA, 20))
})

test_that("fetch_pages combines JSON rows and keeps optional fields", {
  responses <- list(
    gnfidatR_response(
      body = '[{"id":1,"value":10},{"id":2,"value":30}]',
      range = "0-1/3",
      type = "application/json"
    ),
    gnfidatR_response(
      body = '[{"id":3,"extra":20}]',
      range = "2-2/3",
      type = "application/json"
    )
  )

  transport <- gnfidatR_transport(responses = responses)

  actual <- gnfidatR_fetch_fixture_pages(
    transport = transport, accept = "application/json"
  )

  expect_equal(actual$id, 1:3)
  expect_equal(actual$value, c(10, 30, NA))
  expect_equal(actual$extra, c(NA, NA, 20))
})


test_that("fetch_pages preserves column names for empty CSV results", {
  responses <- list(gnfidatR_response(body = "id,value,test\n", range = "*/0"))

  transport <- gnfidatR_transport(responses = responses)

  actual <- gnfidatR_fetch_fixture_pages(transport = transport)

  expect_equal(nrow(actual), 0L)
  expect_named(actual, c("id", "value", "test"))
  expect_length(transport$requests, 1L)
})







test_that("fetch_pages rejects malformed CSV on the first page", {
  malformed_bodies <- c(
    "id,value,test\n1\n",
    "id,value\n1,23,3\n"
  )

  for (body in malformed_bodies) {
    responses <- list(
      gnfidatR_response(
        body = body,
        range = "0-0/1"
      )
    )

    transport <- gnfidatR_transport(responses = responses)

    expect_warning(
      expect_error(
        gnfidatR_fetch_fixture_pages(transport = transport),
        regexp = "The API returned malformed CSV data.",
        fixed = TRUE
      ),
      class = "vroom_parse_issue"
    )
  }
})

test_that("fetch_pages rejects malformed CSV on the first page", {
  malformed_bodies <- c(
    "id,value,test\n1\n",
    "id,value\n1,23,3\n"
  )

  for (body in malformed_bodies) {
    responses <- list(
      gnfidatR_response(
        body = body,
        range = "0-0/1"
      )
    )

    transport <- gnfidatR_transport(responses = responses)

    expect_warning(
      expect_error(
        gnfidatR_fetch_fixture_pages(transport = transport),
        regexp = "The API returned malformed CSV data.",
        fixed = TRUE
      ),
      class = "vroom_parse_issue"
    )
  }
})

test_that("fetch_pages rejects malformed CSV on a later page", {
  responses <- list(
    gnfidatR_response(body = "id,value,test\n1,2,3\n", range = "0-0/2"),
    gnfidatR_response(body = "id,value,test\n2\n", range = "1-1/2")
  )

  transport <- gnfidatR_transport(responses = responses)

  expect_warning(
    expect_error(
      gnfidatR_fetch_fixture_pages(
        transport = transport,
        page_size = 1L
      ),
      regexp = "The API returned malformed CSV data.",
      fixed = TRUE
    ), class = "vroom_parse_issue"
  )

  expect_length(transport$requests, 2L)
})


test_that("fetch_pages returns zero rows for empty JSON results", {
  responses <- list(
    gnfidatR_response(body = "[]", range = "*/0", type = "application/json")
  )

  transport <- gnfidatR_transport(responses = responses)

  expect_equal(
    nrow(
      gnfidatR_fetch_fixture_pages(
        transport = transport,
        accept = "application/json"
      )
    ),
    0L
  )
})

test_that("fetch_pages accepts a single page without explicit ordering", {
  responses <- list(gnfidatR_response(body = "id\n1\n", range = "0-0/1"))

  transport <- gnfidatR_transport(responses = responses)

  actual <- gnfidatR_fetch_fixture_pages(
    transport = transport,
    params = list(select = "id")
  )

  expect_equal(actual$id, 1)
  expect_length(transport$requests, 1L)
})

test_that("fetch_pages requires explicit ordering for multiple pages", {
  responses <- list(gnfidatR_response(body = "id\n1\n", range = "0-0/2"))

  transport <- gnfidatR_transport(responses = responses)

  expect_error(
    gnfidatR_fetch_fixture_pages(
      transport = transport,
      params = list(select = "id")
    ),
    regexp = "explicit unique ordering"
  )
})


test_that("fetch_pages rejects missing and malformed range headers", {
  responses <- list(gnfidatR_response(body = "id\n1\n", range = NULL))

  transport <- gnfidatR_transport(responses = responses)

  expect_error(
    gnfidatR_fetch_fixture_pages(transport = transport),
    "did not return Content-Range"
  )

  for (range in c("invalidtext", "0:1/2", "0-0", "0-0/NaN")) {
    responses <- list(gnfidatR_response(body = "id\n1\n", range = range))

    transport <- gnfidatR_transport(responses = responses)

    expect_error(
      gnfidatR_fetch_fixture_pages(transport = transport),
      "invalid Content-Range"
    )
  }
})

test_that("fetch_pages requires an exact total on the first page", {
  responses <- list(gnfidatR_response(body = "id\n1\n", range = "0-0/*"))

  transport <- gnfidatR_transport(responses = responses)

  expect_error(gnfidatR_fetch_fixture_pages(transport = transport), "exact row count")
})

test_that("fetch_pages rejects changing total row counts", {
  responses <- list(
    gnfidatR_response(body = "id\n1\n", range = "0-0/2"),
    gnfidatR_response(body = "id\n2\n", range = "1-1/3")
  )

  transport <- gnfidatR_transport(responses = responses)

  expect_error(gnfidatR_fetch_fixture_pages(transport = transport), "row count changed")
})

test_that("fetch_pages accepts later pages without a repeated total", {
  responses <- list(
    gnfidatR_response(body = "id\n1\n", range = "0-0/2"),
    gnfidatR_response(body = "id\n2\n", range = "1-1/*")
  )

  transport <- gnfidatR_transport(responses = responses)

  expect_equal(gnfidatR_fetch_fixture_pages(transport = transport)$id, 1:2)
})

test_that("fetch_pages rejects an empty page before the download is complete", {
  responses <- list(
    gnfidatR_response(body = "id\n1\n", range = "0-0/2"),
    gnfidatR_response(body = "id\n", range = "*/2")
  )

  transport <- gnfidatR_transport(responses = responses)

  expect_error(
    gnfidatR_fetch_fixture_pages(transport = transport),
    "empty page after 1 of 2"
  )
})

test_that("fetch_pages checks range offsets and returned row counts", {
  for (range in c("1-1/2", "0-1/2", "*/2")) {
    responses <- list(gnfidatR_response(body = "id\n1\n", range = range))

    transport <- gnfidatR_transport(responses = responses)

    expect_error(gnfidatR_fetch_fixture_pages(transport = transport), "does not match")
  }

  responses <- list(gnfidatR_response(body = "id\n1\n2\n", range = "0-1/1"))

  transport <- gnfidatR_transport(responses = responses)

  expect_error(gnfidatR_fetch_fixture_pages(transport = transport), "more rows")
})

test_that("fetch_pages rejects changing CSV column names", {
  responses <- list(
    gnfidatR_response(body = "id,value\n1,10\n", range = "0-0/2"),
    gnfidatR_response(body = "id,other\n2,20\n", range = "1-1/2")
  )

  transport <- gnfidatR_transport(responses = responses)

  expect_error(
    gnfidatR_fetch_fixture_pages(transport = transport),
    "CSV column names changed"
  )
})

test_that("fetch_pages rejects malformed CSV", {
  responses <- list(
    gnfidatR_response(body = "id,value\n1,10,unexpected\n", range = "0-0/1")
  )

  transport <- gnfidatR_transport(responses = responses)

  expect_error(
    suppressWarnings(gnfidatR_fetch_fixture_pages(transport = transport)),
    "malformed CSV"
  )
})

test_that("fetch_pages reports HTTP errors", {
  responses <- list(gnfidatR_response(body = "failure", range = NULL, status = 500L))

  transport <- gnfidatR_transport(responses = responses)

  expect_error(gnfidatR_fetch_fixture_pages(transport = transport), "500")
})

test_that("fetch_pages preserves repeated query parameters", {
  responses <- list(gnfidatR_response(body = "id\n1\n", range = "0-0/1"))

  transport <- gnfidatR_transport(responses = responses)

  gnfidatR_fetch_fixture_pages(
    transport = transport,
    params = list(select = "id", plot_name = c("gte.1", "lte.4"))
  )

  url <- URLdecode(transport$requests[[1]]$url)

  expect_match(url, "plot_name=gte.1", fixed = TRUE)
  expect_match(url, "plot_name=lte.4", fixed = TRUE)
})

test_that("fetch_single_table normalizes the all-rows value and requests CSV", {
  captured_arguments <- NULL

  local_mocked_bindings(
    fetch_pages = function(...) {
      captured_arguments <<- list(...)
      data.frame(id = 1L)
    },
    .package = "gnfidatR"
  )

  api <- gnfidatR_test_api()

  actual <- fetch_single_table(
    params = list(select = "id"),
    endpoint = "plot",
    url = api$url,
    apikey = api$apikey,
    limit = -1
  )

  expect_identical(actual$id, 1L)
  expect_identical(captured_arguments$page_size, 10000L)
  expect_identical(captured_arguments$accept, "text/csv")
})

test_that("fetch_join_table batches clusters and excludes gaps", {
  captured_calls <- list()

  local_mocked_bindings(
    fetch_pages = function(...) {
      arguments <- list(...)
      captured_calls[[length(captured_calls) + 1L]] <<- arguments

      if (length(captured_calls) == 1L) {
        return(
          data.table::data.table(cluster_name = c(101L, 150L, 205L), plot_name = 1L)
        )
      }

      data.table::data.table(cluster_name = 999L, plot_name = 1L)
    },
    .package = "gnfidatR"
  )

  api <- gnfidatR_test_api()

  actual <- fetch_join_table(
    params = list(select = "*"),
    endpoint = "plot",
    cluster_list = c(999, 205, 101, 101),
    step = 2,
    url = api$url,
    apikey = api$apikey,
    page_size = 3
  )

  expect_equal(actual$cluster_name, c(101, 205, 999))
  expect_length(captured_calls, 2L)
  expect_identical(captured_calls[[1]]$params$cluster_name, c("gte.101", "lte.205"))
  expect_identical(captured_calls[[2]]$params$cluster_name, c("gte.999", "lte.999"))
  expect_identical(
    captured_calls[[1]]$params$order,
    "interval_name.asc,cluster_name.asc,plot_name.asc"
  )
  expect_identical(captured_calls[[1]]$page_size, 3L)
})

test_that("fetch_join_table handles empty batches and checks membership columns", {
  api <- gnfidatR_test_api()

  local_mocked_bindings(
    fetch_pages = function(...) {
      data.frame()
    },
    .package = "gnfidatR"
  )

  expect_equal(
    nrow(
      fetch_join_table(
        params = list(select = "*"),
        endpoint = "plot",
        cluster_list = 101,
        step = 1,
        url = api$url,
        apikey = api$apikey
      )
    ),
    0L
  )

  local_mocked_bindings(
    fetch_pages = function(...) {
      data.frame(id = 1L)
    },
    .package = "gnfidatR"
  )

  expect_error(
    fetch_join_table(
      params = list(select = "*"),
      endpoint = "plot",
      cluster_list = 101,
      step = 1,
      url = api$url,
      apikey = api$apikey
    ),
    "cluster_name"
  )
})
