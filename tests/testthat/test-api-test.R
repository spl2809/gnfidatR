test_that("Testing meta data and database structure for the Inventory Archive accept-profile", {
  # Ausnahme von deiner Netzwerksperre für diesen Online-Test:
  httr2::local_mocked_responses(NULL)

  expected <- readRDS(
    testthat::test_path(
      "fixtures",
      "schema-inventory_archive.rds"
    )
  )

  actual <- database_structure("inventory_archive", view = FALSE)
  attr(actual, "openapi") <- NULL

  expect_identical(actual, expected, info = "inventory_archive")
})

test_that("Testing meta data and database structure for the Derived-Tmp accept-profile", {
  # Ausnahme von deiner Netzwerksperre für diesen Online-Test:
  httr2::local_mocked_responses(NULL)

  expected <- readRDS(
    testthat::test_path(
      "fixtures",
      "schema-derived_tmp.rds"
    )
  )

  actual <- database_structure("derived_tmp", view = FALSE)
  attr(actual, "openapi") <- NULL

  expect_identical(actual, expected, info = "derived_tmp")
})

test_that("Testing meta data and database structure for the Derived accept-profile", {
  # Ausnahme von deiner Netzwerksperre für diesen Online-Test:
  httr2::local_mocked_responses(NULL)

  expected <- readRDS(
    testthat::test_path(
      "fixtures",
      "schema-derived.rds"
    )
  )

  actual <- database_structure("derived", view = FALSE)
  attr(actual, "openapi") <- NULL

  expect_identical(actual, expected, info = "derived")
})

test_that("Testing meta data and database structure for the Lookup accept-profile", {
  # Ausnahme von deiner Netzwerksperre für diesen Online-Test:
  httr2::local_mocked_responses(NULL)

  expected <- readRDS(
    testthat::test_path(
      "fixtures",
      "schema-lookup.rds"
    )
  )

  actual <- database_structure("lookup", view = FALSE)
  attr(actual, "openapi") <- NULL

  expect_identical(actual, expected, info = "lookup")
})

test_that("Testing meta data and database structure for the Public accept-profile", {
  # Ausnahme von deiner Netzwerksperre für diesen Online-Test:
  httr2::local_mocked_responses(NULL)

  expected <- readRDS(
    testthat::test_path(
      "fixtures",
      "schema-public.rds"
    )
  )

  actual <- database_structure("public", view = FALSE)
  attr(actual, "openapi") <- NULL

  expect_identical(actual, expected, info = "public")
})
