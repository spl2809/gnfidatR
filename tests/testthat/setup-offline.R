# Block real httr2 network requests, including requests from incomplete mocks.
# Tests for fetch_pages supply their own transport through .perform.
httr2::local_mocked_responses(
  function(req) {
    stop("Network access is disabled in the gnfidatR test suite.", call. = FALSE)
  },
  env = testthat::teardown_env()
)
