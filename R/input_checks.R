# Shared input validation. API-response and data-integrity checks stay where
# the corresponding data become available. No validator performs a download.

#' Validate a scalar string
#' @keywords internal
#' @noRd
validate_string <- function(value, argument, allow_null = FALSE) {
  if (missing(value)) {
    stop("`", argument, "` must be supplied.", call. = FALSE)
  }
  if (allow_null && is.null(value)) {
    return(NULL)
  }
  if (!is.character(value) ||
    is.object(value) ||
    !is.null(dim(value)) ||
    length(value) != 1L ||
    is.na(value) ||
    !nzchar(trimws(value))) {
    stop("`", argument, "` must be ", if (allow_null) "NULL or " else "",
      "one non-empty character string.",
      call. = FALSE
    )
  }
  if (grepl("[[:cntrl:]]", value)) {
    stop("`", argument, "` must not contain control characters.", call. = FALSE)
  }
  return(value)
}

#' Validate named logical switches
#' @keywords internal
#' @noRd
validate_flags <- function(flags) {
  valid <- vapply(
    flags, function(value) {
      is.logical(value) &&
        !is.object(value) &&
        is.null(dim(value)) &&
        length(value) == 1L &&
        !is.na(value)
    },
    logical(1)
  )
  if (any(!valid)) {
    stop(paste(sprintf("`%s`", names(flags)[!valid]), collapse = ", "),
      " must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Test for one finite whole number within a range
#' @keywords internal
#' @noRd
is_whole_number <- function(value, lower, upper = .Machine$integer.max) {
  return(
    is.numeric(value) &&
      !is.object(value) &&
      is.null(dim(value)) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value == floor(value) &&
      value >= lower &&
      value <= upper
  )
}

#' Validate and normalize a download page or batch size
#' @keywords internal
#' @noRd
validate_page_size <- function(value, argument = "limit", allow_all = FALSE) {
  valid <- is_whole_number(value, 1) ||
    (allow_all && is_whole_number(value, -1, -1))
  if (!valid) {
    stop("`", argument, "` must be ", if (allow_all) "-1 or " else "",
      "a positive whole number within the R integer range.",
      call. = FALSE
    )
  }
  # -1 means all matching rows, fetched in bounded pages.
  if (value == -1) {
    return(10000L)
  } else {
    return(as.integer(value))
  }
}

#' Validate a named list
#' @keywords internal
#' @noRd
validate_named_list <- function(value, argument, allow_empty = FALSE) {
  if (missing(value)) {
    stop("`", argument, "` must be supplied.", call. = FALSE)
  }
  if (!is.list(value) || is.object(value) || !is.null(dim(value))) {
    stop("`", argument, "` must be a named list.", call. = FALSE)
  }
  if (allow_empty && length(value) == 0L) {
    return(value)
  }
  nms <- names(value)
  if (length(value) == 0L ||
    is.null(nms) ||
    anyNA(nms) ||
    any(!nzchar(trimws(nms))) ||
    any(nms != trimws(nms)) ||
    any(grepl("[[:cntrl:]]", nms)) ||
    anyDuplicated(nms)) {
    stop("`", argument, "` must have unique, non-empty names without ",
      "surrounding whitespace or control characters.",
      call. = FALSE
    )
  }
  return(value)
}

#' Validate shared connection settings and normalize the base URL
#' @keywords internal
#' @noRd
validate_api <- function(url, apikey) {
  url <- trimws(validate_string(url, "url"))
  if (!grepl("^https://[^[:space:]/?#@]+(?:/[^[:space:]?#]*)?/?$", url, perl = TRUE) ||
    grepl("\\", url, fixed = TRUE)) {
    stop("`url` must be an HTTPS base URL without credentials, ",
      "a query string, or a fragment.",
      call. = FALSE
    )
  }
  validate_string(apikey, "apikey")
  if (!endsWith(url, "/")) url <- paste0(url, "/")

  return(url)
}

#' Validate an API or SQLite identifier
#' @keywords internal
#' @noRd
validate_identifier <- function(value, argument,
                                pattern = "^[a-z][a-z0-9_]*$") {
  value <- trimws(validate_string(value, argument))
  if (!grepl(pattern, value)) {
    stop("`", argument, "` is not a valid identifier (expected ", pattern, ").",
      call. = FALSE
    )
  }
  value
}

#' Validate PostgREST query parameters
#' @keywords internal
#' @noRd
validate_query_params <- function(params) {
  params <- validate_named_list(params, "params")
  valid <- vapply(
    params, function(value) {
      if ((!is.character(value) && !is.numeric(value) && !is.logical(value)) ||
        is.object(value) ||
        !is.null(dim(value)) ||
        length(value) == 0L ||
        anyNA(value)
      ) {
        return(FALSE)
      }
      if (is.numeric(value) && any(!is.finite(value))) {
        return(FALSE)
      }
      if (is.character(value) &&
        (any(!nzchar(trimws(value))) || any(grepl("[[:cntrl:]]", value)))) {
        return(FALSE)
      }
      TRUE
    },
    logical(1)
  )
  if (any(!valid)) {
    stop("Invalid value for parameter(s): ",
      paste(names(params)[!valid], collapse = ", "),
      ". Use non-empty character, numeric, or logical vectors without ",
      "NA, non-finite numbers, empty strings, or control characters.",
      call. = FALSE
    )
  }
  pagination_params <- intersect(names(params), c("limit", "offset"))
  if (length(pagination_params) > 0L) {
    stop("Do not put ", paste(pagination_params, collapse = " or "),
      " in `params`; use the `limit` argument instead.",
      call. = FALSE
    )
  }
  if (!"select" %in% names(params)) {
    stop("`params` must contain a `select` parameter.", call. = FALSE)
  }
  validate_string(params[["select"]], "params$select")
  if ("order" %in% names(params)) {
    validate_string(params[["order"]], "params$order")
  }
  return(params)
}

#' Validate a writable file destination without creating it
#' @keywords internal
#' @noRd
validate_output_path <- function(value, argument, extension = NULL,
                                 sqlite = FALSE) {
  value <- path.expand(trimws(validate_string(value, argument)))
  if (dir.exists(value)) {
    stop("`", argument, "` points to a directory, not a file.", call. = FALSE)
  }
  if (!is.null(extension)) {
    suffix <- if (sqlite) "\\.(sqlite|sqlite3|db)$" else "\\.csv$"
    if (!grepl(suffix, value, ignore.case = TRUE)) {
      value <- paste0(value, extension)
    }
  }
  parent <- dirname(value)
  if (!dir.exists(parent)) {
    stop("The directory for `", argument, "` does not exist: '", parent, "'.",
      call. = FALSE
    )
  }
  if (dir.exists(value)) {
    stop("`", argument, "` points to a directory, not a file.", call. = FALSE)
  }
  if (file.access(parent, 2L) != 0L ||
    (file.exists(value) && file.access(value, 2L) != 0L)) {
    stop("`", argument, "` must point to a writable file in a writable ",
      "directory.",
      call. = FALSE
    )
  }
  if (sqlite && file.exists(value) && file.access(value, 4L) != 0L) {
    stop("The existing database at `", argument, "` must be readable.",
      call. = FALSE
    )
  }
  return(value)
}

#' Validate output switches and normalize active export destinations
#' @keywords internal
#' @noRd
validate_exports <- function(table, create_csv, csv_name, create_sqlite,
                             sqlite_name, sqlite_table, return_data) {
  flags <- list(
    create_csv = create_csv,
    create_sqlite = create_sqlite,
    return_data = return_data
  )
  validate_flags(flags)
  if (!any(unlist(flags))) {
    stop("At least one of create_csv, create_sqlite, and return_data must be TRUE.",
      call. = FALSE
    )
  }
  validate_string(csv_name, "csv_name", allow_null = TRUE)
  validate_string(sqlite_name, "sqlite_name", allow_null = TRUE)
  validate_string(sqlite_table, "sqlite_table", allow_null = TRUE)
  if (create_csv) {
    if (is.null(csv_name)) csv_name <- paste0(table, ".csv")
    csv_name <- validate_output_path(csv_name, "csv_name", ".csv")
  }
  if (create_sqlite) {
    if (is.null(sqlite_name)) sqlite_name <- paste0(table, ".sqlite")
    sqlite_name <- validate_output_path(
      sqlite_name, "sqlite_name", ".sqlite",
      sqlite = TRUE
    )
    if (is.null(sqlite_table)) sqlite_table <- table
    sqlite_table <- validate_identifier(
      sqlite_table, "sqlite_table", "^[A-Za-z_][A-Za-z0-9_]*$"
    )
    if (startsWith(tolower(sqlite_table), "sqlite_")) {
      stop("`sqlite_table` must not use the reserved prefix 'sqlite_'.",
        call. = FALSE
      )
    }
  }
  return(list(
    csv_name = csv_name,
    sqlite_name = sqlite_name,
    sqlite_table = sqlite_table
  ))
}

#' Validate and normalize plot-level filtering options
#' @keywords internal
#' @noRd
validate_filtering_options <- function(filtering_options) {
  if (is.null(filtering_options)) {
    return(list())
  }
  filtering_options <- validate_named_list(
    filtering_options,
    "filtering_options",
    allow_empty = TRUE
  )
  option_names <- names(filtering_options)
  supported <- c(
    "interval_name",
    "federal_states",
    "growth_district",
    "forest_office",
    "municipality",
    "sampling_stratum",
    "property_type",
    "property_size_class"
  )
  unknown <- setdiff(option_names, supported)
  if (length(unknown) > 0L) {
    stop("Unsupported filtering option(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  for (option in option_names) {
    values <- filtering_options[[option]]
    if (is.null(values)) next
    if ((
      !is.numeric(values) &&
        !is.character(values)) ||
      is.object(values) ||
      !is.null(dim(values)) ||
      length(values) == 0L ||
      anyNA(values)
    ) {
      stop("Filter `", option, "` must contain a non-empty numeric or ",
        "character vector without NA values.",
        call. = FALSE
      )
    }

    if (option == "interval_name") {
      if (!is.character(values) || any(!nzchar(trimws(values))) ||
        any(grepl("[[:cntrl:]]", values))) {
        stop("`interval_name` must contain non-empty character values ",
          "without control characters.",
          call. = FALSE
        )
      }
    } else {
      if (is.character(values) && any(!grepl("^-?[0-9]+$", values))) {
        stop("Filter `", option, "` must contain integer codes.", call. = FALSE)
      }
      numeric_values <- suppressWarnings(as.numeric(values))
      if (
        any(!is.finite(numeric_values)) ||
          any(numeric_values != floor(numeric_values)) ||
          any(abs(numeric_values) > .Machine$integer.max)
      ) {
        stop("Filter `", option, "` must contain finite integer codes ",
          "within the R integer range.",
          call. = FALSE
        )
      }
      values <- as.integer(numeric_values)
    }
    filtering_options[[option]] <- unique(values)
  }

  # NULL entries and an empty list mean that no extra filter is applied.
  active <- !vapply(filtering_options, is.null, logical(1))
  if (!any(active)) {
    return(list())
  }
  return(filtering_options[active])
}

#' Validate query/filter compatibility and return the required join keys
#' @keywords internal
#' @noRd
validate_query_filter <- function(table, params, accept_profile,
                                  filtering_options, filter_key_alias) {
  if (length(filtering_options) == 0L) {
    return(NULL)
  }
  if (accept_profile != "inventory_archive") {
    stop("`filtering_options` is only supported for `inventory_archive`.",
      call. = FALSE
    )
  }
  table_keys <- c(
    cluster = "id",
    cluster_move = "cluster_id",
    plot = "id",
    plot_coordinates = "plot_id",
    plot_support_points = "plot_id",
    notes = "plot_id",
    tree = "plot_id",
    position = "plot_id",
    edges = "plot_id",
    regeneration = "plot_id",
    structure_lt4m = "plot_id",
    structure_gt4m = "plot_id",
    deadwood = "plot_id",
    subplots_relative_position = "plot_id",
    tree_coordinates = "tree_id"
  )
  if (!table %in% names(table_keys)) {
    stop("Plot-level filtering is not supported for table `", table,
      "`. Supported tables: ", paste(names(table_keys), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (grepl(filter_key_alias, params[["select"]], fixed = TRUE)) {
    stop("`params$select` must not use the reserved name `", filter_key_alias,
      "` when filtering is active.",
      call. = FALSE
    )
  }
  if (grepl("\\.(avg|count|max|min|sum)\\s*\\(|\\bcount\\s*\\(",
    params[["select"]],
    perl = TRUE
  )) {
    stop("Aggregate selections cannot be combined with local plot-level ",
      "filtering. Download and filter rows before aggregating them.",
      call. = FALSE
    )
  }
  return(list(
    data = unname(table_keys[[table]]),
    ids = if (table %in% c("cluster", "cluster_move")) {
      "cluster_id"
    } else if (table == "tree_coordinates") {
      "tree_id"
    } else {
      "plot_id"
    }
  ))
}

#' Validate the requested WEHAM inventory and any interval filter
#' @keywords internal
#' @noRd
validate_weham_interval <- function(interval_name, filtering_options) {
  interval_name <- validate_string(interval_name, "interval_name")
  valid_intervals <- c(
    "bwi1987",
    "bwi1992",
    "bwi2002",
    "bwi2012",
    "bwi2022",
    "ci2008",
    "ci2012",
    "ci2017",
    "ci2027"
  )
  if (!interval_name %in% valid_intervals) {
    stop("`interval_name` must be one of: ",
      paste(valid_intervals, collapse = ", "), ".",
      call. = FALSE
    )
  }
  intervals <- filtering_options[["interval_name"]]
  if (!is.null(intervals) && !interval_name %in% intervals) {
    stop("`filtering_options$interval_name` must include the requested ",
      "`interval_name`.",
      call. = FALSE
    )
  }
  return(interval_name)
}

#' Validate a whole-number percentage for WEHAM
#' @keywords internal
#' @noRd
validate_regeneration_share <- function(value) {
  if (!is_whole_number(value, 0, 100)) {
    stop("`share_of_natural_regeneration` must be one whole number ",
      "between 0 and 100.",
      call. = FALSE
    )
  }
  return(as.integer(value))
}

#' Validate and normalize numeric cluster IDs
#' @keywords internal
#' @noRd
validate_cluster_list <- function(cluster_list) {
  if (
    (!is.numeric(cluster_list) && !is.character(cluster_list)) ||
      is.object(cluster_list) ||
      !is.null(dim(cluster_list)) ||
      length(cluster_list) == 0L ||
      anyNA(cluster_list)
  ) {
    stop("`cluster_list` must be a non-empty numeric or character vector ",
      "of cluster IDs without missing values.",
      call. = FALSE
    )
  }
  cluster_list <- suppressWarnings(as.numeric(cluster_list))
  if (
    anyNA(cluster_list) ||
      any(!is.finite(cluster_list)) ||
      any(cluster_list < 1 | cluster_list != floor(cluster_list)) ||
      any(cluster_list > .Machine$integer.max)
  ) {
    stop("`cluster_list` must contain positive integer cluster IDs.", call. = FALSE)
  }
  return(sort(unique(cluster_list)))
}

#' Validate data frames and join columns used for local filtering
#' @keywords internal
#' @noRd
validate_filter_df <- function(data_df, filter_df, columns_data, columns_filter) {
  frames <- list(
    data_df = data_df,
    filter_df = filter_df
  )
  for (argument in names(frames)) {
    frame <- frames[[argument]]
    if (!is.data.frame(frame)) {
      stop("`", argument, "` must be a data frame.", call. = FALSE)
    }
    nms <- names(frame)
    if (is.null(nms) || anyNA(nms) || any(!nzchar(trimws(nms))) || anyDuplicated(nms)) {
      stop("`", argument, "` must have unique, non-empty column names.",
        call. = FALSE
      )
    }
  }
  valid_columns <- function(value) {
    is.character(value) &&
      !is.object(value) &&
      is.null(dim(value)) &&
      length(value) > 0L &&
      !anyNA(value) &&
      all(nzchar(trimws(value))) &&
      !anyDuplicated(value)
  }
  if (!valid_columns(columns_data) || !valid_columns(columns_filter) ||
    length(columns_data) != length(columns_filter)
  ) {
    stop("Column vectors must be non-empty character vectors of equal ",
      "length, without NA values, empty names, or duplicates.",
      call. = FALSE
    )
  }
  for (argument in names(frames)) {
    columns <- if (argument == "data_df") columns_data else columns_filter
    absent <- setdiff(columns, names(frames[[argument]]))
    if (length(absent) > 0L) {
      stop("Missing columns in ", argument, ": ", paste(absent, collapse = ", "),
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}
