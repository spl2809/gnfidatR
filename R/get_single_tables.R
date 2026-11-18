# ------------------------------------------------------------------------------
# PostgREST pagination helpers
# ------------------------------------------------------------------------------

#' Download every top-level row of one PostgREST query
#'
#' The first request asks for an exact total.
#' The server can make its own row limits.
#' Offsets are calculated based on the number of rows actually returned.
#' For multiple pages, params$order must specify a deterministic unique order.
#' The caller must choose a unique key.
#'
#' @keywords internal
#' @noRd
#' @author Nils Steuernagel
fetch_pages <- function(
  params,
  endpoint,
  url,
  apikey,
  page_size,
  accept_profile = "inventory_archive",
  accept = "application/json",
  .perform = httr2::req_perform
) {
  page_size <- validate_page_size(page_size, "page_size")
  full_url <- paste0(sub("/+$", "", url), "/", endpoint)
  offset <- 0
  total <- NULL
  pages <- list()

  repeat {
    req_params <- params
    req_params[["limit"]] <- page_size
    req_params[["offset"]] <- format(offset, scientific = FALSE, trim = TRUE)

    req <- httr2::request(full_url) |>
      httr2::req_headers(
        apikey = apikey,
        Accept = accept,
        `Accept-Profile` = accept_profile,
        Prefer = if (is.null(total)) "count=exact" else NULL
      ) |>
      httr2::req_url_query(!!!req_params, .multi = "explode")

    req <- httr2::req_error(
      req,
      is_error = function(resp) {
        !httr2::resp_status(resp) %in% c(200L, 206L)
      },
      body = function(resp) {
        body <- tryCatch(
          httr2::resp_body_string(resp),
          error = function(e) ""
        )

        if (!nzchar(trimws(body))) {
          return(NULL)
        }

        paste("API-Error:", body)
      }
    )

    resp <- .perform(req)
    httr2::resp_check_status(resp)

    content_range <- httr2::resp_header(resp, "Content-Range")
    if (is.null(content_range)) {
      stop(
        paste(
          "The API did not return Content-Range;",
          "download completeness cannot be verified."
        ),
        call. = FALSE
      )
    }

    parts <- regmatches(
      trimws(content_range),
      regexec(
        "^(?:([0-9]+)-([0-9]+)|\\*)/([0-9]+|\\*)$",
        trimws(content_range),
        perl = TRUE
      )
    )[[1L]]

    if (length(parts) != 4L) {
      stop("The API returned an invalid Content-Range header.", call. = FALSE)
    }

    first <- if (nzchar(parts[2L])) as.numeric(parts[2L]) else NA_real_
    last <- if (nzchar(parts[3L])) as.numeric(parts[3L]) else NA_real_
    reported_total <- if (parts[4L] == "*") NA_real_ else as.numeric(parts[4L])

    if (is.null(total)) {
      if (is.na(reported_total) || !is.finite(reported_total)) {
        stop(
          paste(
            "The API did not return an exact row count despite asking it",
            "to do so with Prefer: count=exact. The download was stopped to",
            "avoid returning an unverified partial result."
          ),
          call. = FALSE
        )
      }
      total <- reported_total
    } else if (!is.na(reported_total) && reported_total != total) {
      stop(
        "The API row count changed during pagination. Retry the download.",
        call. = FALSE
      )
    }

    body <- if (httr2::resp_has_body(resp)) httr2::resp_body_string(resp) else ""
    if (!nzchar(trimws(body))) {
      page <- data.frame()
    } else if (accept == "text/csv") {
      # Parse text explicitly. Infer column types once after all pages are bound
      page <- readr::read_csv(
        I(body),
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE
      )
      if (nrow(readr::problems(page)) > 0L) {
        stop("The API returned malformed CSV data.", call. = FALSE)
      }
      if (length(pages) > 0L && !identical(names(page), names(pages[[1L]]))) {
        stop("CSV column names changed during pagination.", call. = FALSE)
      }
    } else {
      page <- data.table::as.data.table(yyjsonr::read_json_str(body))
    }

    received <- nrow(page)
    if (received == 0L) {
      if (offset < total) {
        stop(
          sprintf(
            "The API returned an empty page after %s of %s expected rows.",
            offset, total
          ),
          call. = FALSE
        )
      }
      # Preserve CSV column names even when the result contains no rows.
      if (length(pages) == 0L) {
        return(page)
      }
      break
    }

    if (is.na(first) || is.na(last) || first != offset ||
      last - first + 1 != received) {
      stop(
        paste(
          "Content-Range does not match the requested offset",
          "or the returned row count."
        ),
        call. = FALSE
      )
    }
    if (offset + received > total) {
      stop(
        "The API returned more rows than its initial exact count.",
        call. = FALSE
      )
    }

    pages[[length(pages) + 1L]] <- page
    offset <- offset + received

    # Stop before making a request beyond the last row (HTTP 416).
    if (offset == total) break

    order <- params[["order"]]
    if (!is.character(order) || length(order) != 1L ||
      is.na(order) || !nzchar(trimws(order))) {
      stop(
        paste(
          "Pagination requires an explicit unique ordering in `params$order`.",
          "Specify the table's primary key columns and retry."
        ),
        call. = FALSE
      )
    }
  }

  if (accept == "text/csv") {
    result <- dplyr::bind_rows(pages)
    return(suppressMessages(readr::type_convert(result)))
  }
  data.table::rbindlist(pages, fill = TRUE)
}

#' Fetch a flat table from PostgREST, handling server-side limits
#'
#' @param params Named query parameters. For multiple pages, include a unique
#'   deterministic order in params$order.
#' @param endpoint API endpoint name.
#' @param url Base API URL.
#' @param apikey API key.
#' @param limit Requested rows per page.
#' -1 fetches all rows using pages of 10000;
#'   it no longer disables pagination. Positive values also fetch all pages.
#' @param accept_profile PostgREST schema.
#' @return A data frame containing all matching rows.
#' @keywords internal
#' @author Nils Steuernagel
fetch_single_table <- function(
  params,
  endpoint,
  url,
  apikey,
  limit,
  accept_profile = "inventory_archive"
) {
  fetch_pages(
    params = params,
    endpoint = endpoint,
    url = url,
    apikey = apikey,
    page_size = validate_page_size(limit, "limit", allow_all = TRUE),
    accept_profile = accept_profile,
    accept = "text/csv"
  )
}

#' Fetch plot data in cluster blocks and paginate within every block
#'
#' @param params         Named list of PostgREST query parameters.
#' @param endpoint       API endpoint name.
#' @param cluster_list   Numeric cluster IDs (or numeric strings) to include.
#' @param step           Number of entries from cluster_list per block.
#' @param url            Base API URL.
#' @param apikey         API key.
#' @param accept_profile PostgREST schema.
#' @param page_size      Requested number of rows per response.
#' @keywords internal
#' @return A data.table of matching rows, or an empty data frame.
#' @author Nils Steuernagel
fetch_join_table <- function(
  params,
  endpoint,
  cluster_list,
  step,
  url,
  apikey,
  accept_profile = "inventory_archive",
  page_size = 10000L
) {
  cluster_list <- validate_cluster_list(cluster_list)
  n <- length(cluster_list)

  step <- validate_page_size(step, "step")
  page_size <- validate_page_size(page_size, "page_size")
  dfs <- list()

  # Include the interval so ordering remains unique across inventory cycles.
  if (endpoint == "plot" && is.null(params[["order"]])) {
    params[["order"]] <- "interval_name.asc,cluster_name.asc,plot_name.asc"
  }

  for (i in seq.int(1L, n, by = step)) {
    end_idx <- min(i + as.double(step) - 1, n)
    allowed_ids <- cluster_list[i:end_idx]
    req_params <- params
    req_params[["cluster_name"]] <- c(
      paste0("gte.", cluster_list[i]),
      paste0("lte.", cluster_list[end_idx])
    )

    # Download the entire range before filtering: offsets count API plot rows,
    # not locally retained rows and not unnested tree/regeneration records.
    dt <- fetch_pages(
      params = req_params, endpoint = endpoint, url = url, apikey = apikey,
      page_size = page_size, accept_profile = accept_profile
    )
    if (nrow(dt) == 0L) next
    if (!"cluster_name" %in% names(dt)) {
      stop(
        "The API result must contain `cluster_name` for local filtering.",
        call. = FALSE
      )
    }
    dt <- dt[dt$cluster_name %in% allowed_ids, ]
    if (nrow(dt) == 0L) next
    dfs[[length(dfs) + 1L]] <- dt
  }

  if (length(dfs) == 0L) {
    return(data.frame())
  }
  data.table::rbindlist(dfs, fill = TRUE)
}


# ------------------------------------------------------------------------------
# Query Entry Point
# ------------------------------------------------------------------------------

#' Query a table from the Thuenen API
#'
#' Downloads a table from a PostgREST endpoint, hosted by the Thuenen Institut.
#' The result can optionally be written to a CSV file, a table in a SQLite
#' database, or both.
#'
#' @param table A string naming the API endpoint,
#' for example `"plot"` or `"tree"`.
#' @param params A named list of PostgREST query parameters. A `select` element
#'   is required. Filters use PostgREST syntax, for example
#'   `interval_name = "eq.bwi2022"`. Use a character vector to apply multiple
#'   filters to the same field, for example `plot_name = c("gte.1", "lte.4")`.
#'   Do not include `limit` or `offset`. Pagination is controlled by the `limit`
#'   argument. For multiple pages, supply `order` using unique key columns.
#' @param limit A number specifying the number of rows requested per API call.
#'   Use `-1` to fetch all matching rows in pages of 10000.
#'   Positive values set the requested page size; all pages are fetched.
#' @param url A string containing the HTTPS base URL of the REST API.
#' @param apikey A string containing the API key. Defaults to the default key.
#' @param accept_profile A string naming the PostgREST schema. Defaults to
#'   `"inventory_archive"`.
#' @param create_csv A boolean value. If `TRUE`, the downloaded data is
#'   written to a CSV file. Defaults to `FALSE`.
#' @param csv_name A CSV filename or path. If `NULL`, `"<table>.csv"` is used.
#' @param create_sqlite A boolean value. If `TRUE`, the downloaded data
#'   is written to a table in a SQLite database. Defaults to `FALSE`. If used,
#'   check table for wrong type conversion.
#' @param sqlite_name A SQLite database filename or path. If `NULL`,
#'   `"<table>.sqlite"` is used.
#' @param sqlite_table A string naming the SQLite table. If `NULL`, the value of
#'   `table` is used.
#' @param filtering_options NULL or a named list of plot-level filters. Supports
#'   interval_name, federal_states, growth_district, forest_office,
#'   municipality, sampling_stratum, property_type, and property_size_class.
#'   NULL entries are ignored.
#'   NULL or an empty list disables extra filtering. Active filtering is only
#'   supported in inventory_archive for cluster, cluster_move, plot,
#'   plot_coordinates, plot_support_points, notes, tree, tree_coordinates,
#'   position, edges, regeneration, structure_lt4m, structure_gt4m, deadwood,
#'   and subplots_relative_position. The required join key is downloaded under
#'   a reserved internal alias (gnfi_filter_key) and removed before output.
#'   Aggregates in params$select are not supported with local filtering.
#' @param include_test_cluster Logical, should test cluster be included.
#' @param print_progress Logical; print progress messages. Defaults to TRUE.
#' @param return_data Logical; return the downloaded data in R.
#' Defaults to TRUE.
#' At least one of return_data, create_csv, and create_sqlite must be TRUE.
#'
#' @details
#' Inputs are validated before the first API request. Output paths must refer
#' to writable files in existing, writable directories. Query values must be
#' plain character, numeric, or logical vectors without missing values;
#' numeric values must be finite. The SQLite prefix `sqlite_` is reserved.
#'
#' The function downloads successive pages of data from the API, then combines
#' the pages into one data frame. It checks Content-Range against the exact
#' total from the first response, including when the server returns fewer rows
#' than requested.
#'
#' Plot-level filtering is local, after the full table download and before any
#' export. Existing params filters and filtering_options are combined with AND.
#' Cluster-level tables retain a cluster if at least one of its plots matches.
#' Tree tables retain all trees of matching plots. Test clusters are not
#' implicitly excluded by the optional plot filter.
#'
#' If the endpoint returns no data, the function emits a warning and returns an
#' empty data frame invisibly. In that case possible CSV files or SQLite tables
#' are empty.
#'
#' @return If return_data is TRUE, a data frame containing all downloaded rows.
#'   If no rows are found, an empty data frame is returned invisibly.
#'   If return_data is FALSE, NULL is returned invisibly after any file exports.
#'
#' @examples
#' \dontrun{
#' # Download selected plot data from the 2022 inventory and store it in SQLite.
#' plot_params <- list(
#'   select = paste(
#'     c(
#'       "interval_name",
#'       "cluster_name",
#'       "plot_name",
#'       "stand_age"
#'     ),
#'     collapse = ","
#'   ),
#'   interval_name = "eq.bwi2022",
#'   order = "cluster_name.asc,plot_name.asc",
#'   plot_name = c("gte.1", "lte.4"),
#'   stand_age = c("gte.1", "lte.300")
#' )
#'
#' plot_data <- gnfi_query(
#'   table = "plot",
#'   params = plot_params,
#'   limit = 10000,
#'   create_sqlite = TRUE,
#'   sqlite_name = "bwi2022_plots.sqlite",
#'   sqlite_table = "plots_2022"
#' )
#'
#' head(plot_data)
#'
#'
#' # Downloads selected plot data from the 2022 inventory and store it in CSV.
#' plot_params <- list(
#'   select = paste(
#'     c(
#'       "interval_name",
#'       "coast",
#'       "sandy",
#'       "fence_regeneration",
#'       "harvest_restriction_nature_reserve",
#'       "plot_name",
#'       "terrain_slope",
#'       "terrain_exposure",
#'       "stand_age",
#'       "acquisition_date"
#'     ),
#'     collapse = ","
#'   ),
#'   interval_name = "eq.bwi2022",
#'   order = "cluster_name.asc,plot_name.asc",
#'   coast = "eq.false",
#'   sandy = "eq.false",
#'   fence_regeneration = "eq.false",
#'   harvest_restriction_nature_reserve = "eq.false",
#'   plot_name = c("gte.1", "lte.4"),
#'   terrain_slope = c("gte.0", "lte.90"),
#'   terrain_exposure = c("gte.0", "lte.400"),
#'   stand_age = c("gte.1", "lte.300"),
#'   acquisition_date = c(
#'     "gte.2022-01-01",
#'     "lte.2022-12-31"
#'   )
#' )
#'
#' plot_data <- gnfi_query(
#'   table          = "plot",
#'   params         = plot_params,
#'   limit          = 100000,
#'   create_csv     = TRUE
#' )
#'
#' plot_data
#' }
#'
#' @export
#' @author Nils Steuernagel
gnfi_query <- function(
  table,
  params,
  limit = -1,
  url = "https://ci.thuenen.de/rest/v1/",
  apikey = paste0(
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.",
    "eyJyb2xlIjoiYW5vbiIsImlzcyI6InN1cGFiYXN",
    "lIiwiaWF0IjoxNzQ1NzkxMjAwLCJleHAiOjE5MDM1",
    "NTc2MDB9.hXiYlA_168hHZ6fk3zPgABQUpEcqkYRMzu0A5W5PtYU"
  ),
  accept_profile = "inventory_archive",
  create_csv = FALSE,
  csv_name = NULL,
  create_sqlite = FALSE,
  sqlite_name = NULL,
  sqlite_table = NULL,
  return_data = TRUE,
  filtering_options = NULL,
  include_test_cluster = FALSE,
  print_progress = TRUE
) {
  validate_flags(list(print_progress = print_progress))
  if (print_progress) {
    log_step("Input checks...")
    t0 <- proc.time()[["elapsed"]]
  }

  table <- validate_identifier(table, "table")
  params <- validate_query_params(params)
  limit <- validate_page_size(limit, "limit", allow_all = TRUE)
  url <- validate_api(url, apikey)
  accept_profile <- validate_identifier(
    accept_profile, "accept_profile", "^[a-z_][a-z0-9_]*$"
  )
  output <- validate_exports(
    table,
    create_csv,
    csv_name,
    create_sqlite,
    sqlite_name,
    sqlite_table,
    return_data
  )
  csv_name <- output$csv_name
  sqlite_name <- output$sqlite_name
  sqlite_table <- output$sqlite_table
  filtering_options <- validate_filtering_options(filtering_options)
  filter_key_alias <- "gnfi_filter_key"
  filter_keys <- validate_query_filter(
    table, params, accept_profile, filtering_options, filter_key_alias
  )
  use_filter <- !is.null(filter_keys)

  if (use_filter) {
    filter_column_data <- filter_keys$data
    filter_column_ids <- filter_keys$ids

    # Keep the user's select output unchanged, including aliases and '*'.
    params[["select"]] <- paste0(
      filter_key_alias, ":", filter_column_data, ",", params[["select"]]
    )
    if (is.null(params[["order"]])) params[["order"]] <- "id.asc"
  }
  if (print_progress) {
    log_step("Finished Input Checks", t0)
  }

  # ------------------------------ DOWNLOAD DATA -------------------------------
  if (print_progress) {
    log_step("Downloading data...")
    t0 <- proc.time()[["elapsed"]]
  }
  data <- fetch_single_table(
    params         = params,
    endpoint       = table,
    url            = url,
    apikey         = apikey,
    limit          = limit,
    accept_profile = accept_profile
  )

  if (nrow(data) == 0L) {
    warning(
      sprintf(
        "Endpoint '%s' returned no data for the current query",
        table
      ),
      call. = FALSE
    )

    if (return_data) {
      return(invisible(data))
    }
    return(invisible(NULL))
  }
  if (print_progress) {
    log_step(paste0("Finished downloading data: ", t0, "s"))
  }

  # --------------------------- Optional filtering -----------------------------
  if (use_filter) {
    if (print_progress) {
      log_step("Filtering...")
      t0 <- proc.time()[["elapsed"]]
    }
    if (!filter_key_alias %in% names(data)) {
      stop(
        "The API result is missing the internal filter key `",
        filter_key_alias,
        "` (source column `", filter_column_data, "`).",
        call. = FALSE
      )
    }

    if (nrow(data) > 0L) {
      filter_plot_ids <- get_filtered_plots(
        filtering_options = filtering_options,
        url = url,
        apikey = apikey,
        page_size = validate_page_size(
          limit, "limit",
          allow_all = TRUE
        ),
        include_tree_ids = identical(table, "tree_coordinates"),
        include_test_cluster = include_test_cluster
      )
      if (nrow(filter_plot_ids) == 0L) {
        data <- data[FALSE, , drop = FALSE]
      } else {
        data <- filter_df(
          data_df        = data,
          filter_df      = filter_plot_ids,
          columns_data   = filter_key_alias,
          columns_filter = filter_column_ids
        )
      }
    }
    data[[filter_key_alias]] <- NULL
    if (nrow(data) == 0L) {
      warning("No rows remain after plot-level filtering for table `", table,
        "`.",
        call. = FALSE
      )
    }
    if (print_progress) {
      log_step(paste0("Finished filtering: ", t0, "s"))
    }
  }

  # ------------------------------- EXPORT DATA --------------------------------

  # creating the csv
  if (isTRUE(create_csv)) {
    if (print_progress) {
      log_step("Exporting CSV file...")
      t0 <- proc.time()[["elapsed"]]
    }
    readr::write_csv(data, csv_name)
    message(
      "CSV file written to: ",
      normalizePath(csv_name, winslash = "/", mustWork = TRUE)
    )
    if (print_progress) {
      log_step(paste0("Finished exporting CSV file: ", t0, "s"))
    }
  }

  # creating the SQLite table
  if (isTRUE(create_sqlite)) {
    if (print_progress) {
      log_step("Exporting SQlite file...")
      t0 <- proc.time()[["elapsed"]]
    }
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_name)
    on.exit(RSQLite::dbDisconnect(conn), add = TRUE)

    RSQLite::dbWriteTable(
      conn,
      sqlite_table,
      data,
      overwrite = TRUE,
      row.names = FALSE
    )
    if (print_progress) {
      log_step(paste0("Finished exporting SQlite file: ", t0, "s"))
    }
    log_step(paste(
      "SQLite table '",
      sqlite_table,
      "' written to: ",
      normalizePath(sqlite_name, winslash = "/", mustWork = TRUE)
    ))
  }

  if (return_data) {
    if (nrow(data) == 0L) {
      return(invisible(data))
    }
    return(data)
  }
  invisible(NULL)
}
