openapi_schema_to_df <- function(openapi, accept_profile) {
  if (
    !is.list(openapi) ||
      !identical(openapi[["swagger"]], "2.0") ||
      !is.list(openapi[["paths"]])
  ) {
    stop("Expected a PostgREST OpenAPI document (Swagger 2.0 with paths).",
      call. = FALSE
    )
  }

  to_json <- function(x) {
    as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    ))
  }
  field <- function(object, name, json = FALSE) {
    if (!name %in% names(object)) {
      return(NA_character_)
    }
    value <- object[[name]]
    if (!json && is.null(value)) {
      return(NA_character_)
    }
    if (!json && is.character(value) && length(value) == 1L) {
      return(value)
    }
    to_json(value)
  }

  # Use the same row definition for actual data and the empty result.
  make_row <- function(table = NA_character_, column = NA_character_,
                       definition = list()) {
    has_column <- !is.na(column)
    property <- if (has_column) definition[["properties"]][[column]] else list()
    data.frame(
      accept_profile = accept_profile,
      table = table,
      table_description = field(definition, "description"),
      column = column,
      type = field(property, "type"),
      format = field(property, "format"),
      required = if (has_column) {
        column %in% unlist(definition[["required"]], use.names = FALSE)
      } else {
        NA
      },
      default = field(property, "default", json = TRUE),
      enum = field(property, "enum", json = TRUE),
      description = field(property, "description"),
      column_schema = if (has_column) to_json(property) else NA_character_,
      stringsAsFactors = FALSE
    )
  }

  tables <- lapply(
    names(openapi[["definitions"]]),
    function(table) {
      definition <- openapi[["definitions"]][[table]]
      columns <- names(definition[["properties"]])
      if (length(columns) == 0L) columns <- NA_character_
      do.call(
        rbind,
        lapply(
          columns,
          function(column) {
            make_row(table, column, definition)
          }
        )
      )
    }
  )
  df <- if (length(tables) > 0L) do.call(rbind, tables) else make_row()[FALSE, ]
  rownames(df) <- NULL
  attr(df, "openapi") <- openapi
  df
}

#' Inspect the database structure exposed by the API
#'
#' Retrieve table, view, and column metadata from the API's OpenAPI document
#' for a selected schema. Optionally display the result in a data viewer.
#'
#' @param accept_profile API schema to inspect. One of
#'   \code{"inventory_archive"}, \code{"derived"}, \code{"derived"},
#'   \code{"lookup"}, or \code{"public"}.
#' @param apikey API key. Defaults to the included public key.
#' @param url REST API base URL. Defaults to
#'   \code{"https://ci.thuenen.de/rest/v1/"}.
#' @param view Logical. Open the metadata in a data viewer. Defaults to
#'   \code{TRUE}.
#' @param return_data Logical. Return the metadata as a data frame. Defaults
#'   to \code{TRUE}.
#' @param .perform Function used to perform the HTTP request. Defaults to
#'   \code{httr2::req_perform}; intended for testing.
#'
#' @return If \code{return_data = TRUE}, a data frame with one row per column,
#'   including table and column descriptions, type, format, required status,
#'   defaults, and allowed values. Definitions without columns have a row with
#'   \code{column = NA}. The original OpenAPI document is stored in the
#'   \code{openapi} attribute. Otherwise, \code{NULL} is returned invisibly.
#'
#' @details
#' At least one of \code{view} and \code{return_data} must be \code{TRUE}.
#' The result describes the structure exposed by the API, not inventory data.
#'
#' @export
#' @author Nils Steuernagel
#' @examples
#' \dontrun{
#' schema <- database_structure(accept_profile = "lookup", view = FALSE)
#' head(schema)
#' }
database_structure <- function(
  accept_profile,
  apikey = paste0(
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.",
    "eyJyb2xlIjoiYW5vbiIsImlzcyI6InN1cGFiYXN",
    "lIiwiaWF0IjoxNzQ1NzkxMjAwLCJleHAiOjE5MDM1",
    "NTc2MDB9.hXiYlA_168hHZ6fk3zPgABQUpEcqkYRMzu0A5W5PtYU"
  ),
  url = "https://ci.thuenen.de/rest/v1/",
  view = TRUE,
  return_data = TRUE,
  .perform = httr2::req_perform
) {
  # input checks -------------------------------------------------------------
  accept_profile <- validate_identifier(
    accept_profile,
    "accept_profile",
    "^(inventory_archive|derived|lookup|public)$"
  )
  url <- validate_api(url, apikey)
  validate_flags(list(view = view, return_data = return_data))
  if (!view && !return_data) {
    stop("At least one of `view` and `return_data` must be TRUE.",
      call. = FALSE
    )
  }

  # Download data -----------------------------------------------------------
  req <- httr2::request(url) |>
    httr2::req_headers(
      apikey = apikey, Accept = "application/openapi+json",
      `Accept-Profile` = accept_profile
    )
  resp <- .perform(req)
  httr2::resp_check_status(resp)

  # Format Data --------------------------------------------------------------
  df <- openapi_schema_to_df(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    accept_profile
  )

  # Export data --------------------------------------------------------------
  if (view) {
    if (nrow(df) == 0L) {
      warning("The API exposed no table/view definitions; there is nothing ",
        "to display.",
        call. = FALSE
      )
    } else {
      viewer <- get("View",
        envir = .GlobalEnv, mode = "function",
        inherits = TRUE
      )
      viewer(df, title = paste("API schema:", accept_profile))
    }
  }
  if (return_data) {
    return(df)
  }
  invisible(NULL)
}
