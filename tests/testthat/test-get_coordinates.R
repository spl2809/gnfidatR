test_that("get_cluster_coordinates requires an output option", {
  expect_error(
    get_cluster_coordinates(return_values = FALSE, save_values = NULL),
    "does nothing"
  )
  expect_error(get_cluster_coordinates(save_values = "coordinates.csv"), "save_values")
})

test_that("get_cluster_ids_from_shapefile selects inside and boundary points", {
  skip_if_not_installed("sf")
  coordinates <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)

  polygon_geometry <- sf::st_polygon(list(coordinates))
  polygon <- sf::st_sfc(polygon_geometry, crs = 3857)
  points <- data.frame(Tnr = c(101L, 205L, 303L), x = c(5, 50, 0), y = c(5, 50, 5))

  actual <- get_cluster_ids_from_shapefile(
    shapefile = polygon,
    cluster_coordinates = points,
    crs = "epsg:3857"
  )

  expect_identical(actual, c(101L, 303L))
})

test_that("get_cluster_ids_from_shapefile returns no IDs outside the polygon", {
  skip_if_not_installed("sf")
  coordinates <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)

  polygon_geometry <- sf::st_polygon(list(coordinates))
  polygon <- sf::st_sfc(polygon_geometry, crs = 3857)
  points <- data.frame(Tnr = 101L, x = 50, y = 50)

  expect_identical(
    get_cluster_ids_from_shapefile(
      shapefile = polygon,
      cluster_coordinates = points,
      crs = "epsg:3857"
    ),
    integer()
  )
})

test_that("get_cluster_ids_from_shapefile transforms in-memory polygons", {
  skip_if_not_installed("sf")
  coordinates <- matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE)

  polygon_geometry <- sf::st_polygon(list(coordinates))
  polygon <- sf::st_sfc(polygon_geometry, crs = 4326)
  # The first point is approximately at 1 degree east and 1 degree north.
  points <- data.frame(
    Tnr = c(101L, 205L),
    x = c(111319.49, 1000000),
    y = c(111325.14, 1000000)
  )

  expect_identical(
    get_cluster_ids_from_shapefile(
      shapefile = polygon,
      cluster_coordinates = points,
      crs = "epsg:3857"
    ),
    101L
  )
})

test_that("get_cluster_ids_from_shapefile accepts local vector files", {
  skip_if_not_installed("sf")
  # A small GeoJSON fixture keeps the test independent of binary write
  # drivers. This test exercises the file-path input.
  path <- file.path(withr::local_tempdir(), "area.geojson")
  writeLines(
    paste0(
      '{"type":"FeatureCollection","features":[',
      '{"type":"Feature","properties":{"id":1},',
      '"geometry":{"type":"Polygon","coordinates":[',
      '[[0,0],[10,0],[10,10],[0,10],[0,0]]]}}]}'
    ),
    path
  )
  points <- data.frame(Tnr = c(101L, 205L), x = c(5, 50), y = c(5, 50))

  expect_identical(
    get_cluster_ids_from_shapefile(
      shapefile = path,
      cluster_coordinates = points,
      crs = "epsg:4326"
    ),
    101L
  )
})

test_that("get_cluster_ids_from_shapefile rejects invalid spatial classes", {
  skip_if_not_installed("sf")
  points <- data.frame(Tnr = 101L, x = 5, y = 5)

  expect_error(
    get_cluster_ids_from_shapefile(
      shapefile = data.frame(id = 1),
      cluster_coordinates = points
    ),
    "shapefile must be a file path or an sf/sfc object."
  )
})

test_that("get_cluster_ids_from_shapefile transforms polygons read from files", {
  skip_if_not_installed("sf")
  path <- file.path(withr::local_tempdir(), "area.geojson")
  writeLines(
    paste0(
      '{"type":"FeatureCollection","features":[',
      '{"type":"Feature","properties":{"id":1},',
      '"geometry":{"type":"Polygon","coordinates":[',
      '[[0,0],[2,0],[2,2],[0,2],[0,0]]]}}]}'
    ),
    path
  )
  points <- data.frame(
    Tnr = c(101L, 205L),
    x = c(111319.49, 1000000),
    y = c(111325.14, 1000000)
  )

  expect_identical(
    get_cluster_ids_from_shapefile(
      shapefile = path,
      cluster_coordinates = points,
      crs = "epsg:3857"
    ),
    101L
  )
})
