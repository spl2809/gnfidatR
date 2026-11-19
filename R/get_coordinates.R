
#' Retrieve anonymized BWI cluster coordinates
#'
#' Downloads anonymized cluster coordinates of the German National Forest
#' Inventory (BWI) from the Thuenen Institute's Web Feature Service (WFS).
#' The coordinates are anonymized by assigning cluster to 1 km x 1 km
#' INSPIRE grid cells; exact tract coordinates are not publicly available.
#'
#' @param return_values logical - Should the resulting data.frame be returned
#' @param save_values Name of the R Object in which the result should be saved
#' for later use. If NULL (=default) no file is written. File must be provided
#' in the form 'fileName.RData'.
#' @param crs EPSG code, in case no reprojection is wanted choose NULL
#' @param timeout timeout in s. Useful to prevent undesirable long function
#' runs in case of server issues.
#'
#' @returns data.frame with cluster coordinates (optional)
#' @note Function needs package 'sf'. Please install if not available. Currently
#' spatial filtering is not available in \code{\link{gnfi_query}}. But feel free
#' to write your own filtering steps after data retrieval using this function.
#'
#' @details Function accesses the Thuenen geoserver to download anonymized
#' coordinates
#' of the NFI clusters. For more information about the data see
#' https://bwi.info/download/de/bwi-basisdaten/_Hinweise_BWI-Koordinaten.pdf?
#' @references
#' Thuenen-Institut: \emph{Informationen zur Nutzung / Weitergabe
#' anonymisierter Trakt-Koordinaten der Bundeswaldinventur}.
#' \url{https://bwi.info/download/de/bwi-basisdaten/_Hinweise_BWI-Koordinaten.pdf}
#'
#' @seealso
#' \code{\link{get_cluster_ids_from_shapefile}},
#' \url{https://gdi.thuenen.de/geoserver/wo-bwi/bwi-tnr-ano/wfs}
#'
#'
#' @export
#' @author Stefan Paul
#'
get_cluster_coordinates <- function(
  return_values = TRUE,
  save_values = NULL,
  crs = "epsg:4326",
  timeout = 30
) {

  if (return_values == FALSE && is.null(save_values)) {
    stop("return_values == FALSE and save_values is not provided. Function
         does nothing.")
  }

  if (!is.null(save_values) && !endsWith(save_values, ".RData")) {
    stop("Parameter 'save_values' must be provided in the form 'fileName.RData")
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package 'sf' is required for this functionality. ",
      "Please install it with install.packages('sf').",
      call. = FALSE
    )
  }

  url <- paste0(
    "https://gdi.thuenen.de/geoserver/wo-bwi/bwi-tnr-ano/wfs?",
    "service=WFS&",
    "version=2.0.0&",
    "request=GetFeature&",
    "typeNames=Anonym_TraktI_Point&",
    "outputFormat=application/json&",
    "srsName=EPSG:4326"
  )




  cluster <- tryCatch({

    setTimeLimit(elapsed = timeout, transient = TRUE)

    cluster <- sf::st_read(url, quiet = TRUE) |>
      dplyr::select(.data$Tnr, .data$geometry)

    if (!is.null(crs)) {
      cluster <- sf::st_transform(cluster, crs)
    }

  }, finally = {

    setTimeLimit(elapsed = Inf, transient = FALSE)

  })


  if (!is.null(save_values)) {
    save(cluster, file = save_values)
  }

  if (return_values == TRUE) {
    return(cluster)
  }

}




############################################################
###########################################################

#' Get cluster IDs from a provided shapefile
#'
#' @param shapefile shapefile of the extend
#' @param cluster_coordinates cluster coordinates (see details), if NULL (default) the coordinates are downloaded
#' using \code{\link{get_cluster_coordinates}}
#' @param crs the desired crs
#' @param timeout timeout in s for data retrieval
#'
#' @returns cluster coordinates
#' @details
#' Cluster coordinates can be provided as a `sf` object containing the geographic locations of the
#' sample cluster. The object consists
#' of POINT geometries in WGS 84 (EPSG:4326) and a single attribute column
#' `Tnr`, containing the unique tract identifiers. If in doubt see the
#' output of \code{\link{get_cluster_coordinates}}.
#'
#'
#' @export
#' @author Stefan Paul, Jan Schick
#'
#' @seealso
#' \code{\link{get_cluster_coordinates}}

get_cluster_ids_from_shapefile <- function(
    shapefile = NULL,
    cluster_coordinates = NULL,
    crs = "EPSG:4326",
    timeout = 30
) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package 'sf' is required for this functionality. ",
      "Please install it with install.packages('sf').",
      call. = FALSE
    )
  }

  # Validate and read shapefile
  if (is.null(shapefile)) {
    stop("Please provide a shapefile or an sf/sfc object.")
  }

  if (is.character(shapefile)) {
    shapefile <- sf::read_sf(shapefile)
  }

  if (!inherits(shapefile, c("sf", "sfc"))) {
    stop("shapefile must be a file path or an sf/sfc object.")
  }

  if (is.na(sf::st_crs(shapefile))) {
    stop("The shapefile must have a defined CRS.")
  }

  shapefile <- sf::st_transform(shapefile, crs)

  # Get cluster coordinates
  if (is.null(cluster_coordinates)) {

    cluster_coordinates <- get_cluster_coordinates(
      return_values = TRUE,
      save_values = NULL,
      crs = crs,
      timeout = timeout
    ) |>
      dplyr::select(.data$Tnr, .data$geometry)

  } else if (inherits(cluster_coordinates, "sf")) {

    cluster_coordinates <- sf::st_transform(
      cluster_coordinates,
      crs
    )

  } else if (inherits(cluster_coordinates, "sfc")) {

    cluster_coordinates <- sf::st_sf(
      Tnr = seq_along(cluster_coordinates),
      geometry = sf::st_transform(cluster_coordinates, crs)
    )

  } else if (all(c("x", "y", "Tnr") %in% names(cluster_coordinates))) {

    cluster_coordinates <- sf::st_as_sf(
      cluster_coordinates,
      coords = c("x", "y"),
      crs = crs
    )

  } else {
    stop(
      "cluster_coordinates must be an sf/sfc object or a data.frame ",
      "containing Tnr, x and y columns."
    )
  }

  # Check CRS
  if (is.na(sf::st_crs(cluster_coordinates))) {
    stop("cluster_coordinates must have a defined CRS.")
  }

  # Spatial intersection
  intersects <- sf::st_intersects(
    cluster_coordinates,
    shapefile
  )

  # Return intersecting tract IDs
  cluster_coordinates$Tnr[lengths(intersects) > 0] |>
    unique()
}
