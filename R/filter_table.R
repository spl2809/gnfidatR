#' Encode values for a PostgREST IN filter
#' @keywords internal
#' @noRd
gnfi_filter_in <- function(values) {
  values <- as.character(values)
  values <- gsub("\\", "\\\\", values, fixed = TRUE)
  values <- gsub('"', '\\"', values, fixed = TRUE)
  paste0('in.("', paste(values, collapse = '","'), '")')
}


#' Retain data rows matching one or more filter keys
#' @keywords internal
#' @noRd
filter_df <- function(
  data_df,
  filter_df,
  columns_data = c("plot_id"),
  columns_filter = c("plot_id")
) {
  validate_filter_df(data_df, filter_df, columns_data, columns_filter)

  return(dplyr::semi_join(
    data_df,
    filter_df,
    by = stats::setNames(columns_filter, columns_data),
    na_matches = "never"
  ))
}

#' Query filtered plots from the Thuenen API
#'
#' @param filtering_options Named list of filter values, or NULL. Supported
#'   names are interval_name, federal_states, growth_district, forest_office,
#'   municipality, sampling_stratum, property_type, and property_size_class.
#'   NULL entries are ignored.
#'   Unknown names and invalid or empty vectors are rejected.
#' @param url HTTPS base URL of the API, without a query string or fragment.
#' @param apikey API key used for authentication.
#' @param step Number of clusters per plot-request batch when including trees.
#' @param page_size Page size passed to the download helpers.
#' @param include_tree_ids Logical. If TRUE, return one row per tree. Plots
#'   without trees are excluded by the final inner join.
#' @param include_test_cluster Logical. Whether get_cluster() should include
#'   test clusters. Only used when include_tree_ids is TRUE.
#'
#' @details
#' Connection settings, filters, flags, and batch sizes are validated before
#' any API request, including when no extra filters are supplied.
#'
#' Plot-only queries use fetch_single_table(). Tree queries fetch plots in
#' cluster batches and tree IDs in separate, fully paginated flat requests.
#' At most 100 plot UUIDs are sent in one tree request to bound URL length.
#' Top-level pagination therefore also verifies completeness of tree rows;
#' no nested tree arrays need to be exploded or paginated separately.
#'
#' @return A data frame with plot_id and cluster_id. If include_tree_ids is
#'   TRUE, cluster_name and tree_id are also included. Empty results retain
#'   these column names and their types.
#' @export
#' @author Nils Steuernagel
get_filtered_plots <- function(
  filtering_options = NULL,
  url = "https://ci.thuenen.de/rest/v1/",
  apikey = paste0(
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.",
    "eyJyb2xlIjoiYW5vbiIsImlzcyI6InN1cGFiYXN",
    "lIiwiaWF0IjoxNzQ1NzkxMjAwLCJleHAiOjE5MDM1",
    "NTc2MDB9.hXiYlA_168hHZ6fk3zPgABQUpEcqkYRMzu0A5W5PtYU"
  ),
  step = 1000,
  page_size = 10000L,
  include_tree_ids = FALSE,
  include_test_cluster = FALSE
) {
  filtering_options <- validate_filtering_options(filtering_options)
  url <- validate_api(url, apikey)
  validate_flags(list(
    include_tree_ids = include_tree_ids,
    include_test_cluster = include_test_cluster
  ))
  page_size <- validate_page_size(page_size, "page_size")
  step <- validate_page_size(step, "step")

  empty_result <- data.frame(plot_id = character(), cluster_id = character())
  if (include_tree_ids) {
    empty_result$cluster_name <- integer()
    empty_result$tree_id <- character()
  }

  filter_params <- list(
    select = if (include_tree_ids) {
      "plot_id:id,cluster_id,cluster_name,tree!inner(tree_id:id)"
    } else {
      "plot_id:id,cluster_id"
    },
    order = "id"
  )

  filter_columns <- c(
    interval_name = "interval_name",
    federal_states = "federal_state",
    growth_district = "growth_district",
    forest_office = "forest_office",
    municipality = "municipality",
    sampling_stratum = "sampling_stratum",
    property_type = "property_type",
    property_size_class = "property_size_class"
  )

  # Build the "WHERE" parameter
  for (option in names(filtering_options)) {
    filter_params[[filter_columns[[option]]]] <- gnfi_filter_in(filtering_options[[option]])
  }

  # Download and Return the Cluster and Plots
  if (!include_tree_ids) {
    data <- fetch_single_table(
      params = filter_params,
      endpoint = "plot",
      url = url,
      apikey = apikey,
      limit = page_size,
      accept_profile = "inventory_archive"
    )
    if (nrow(data) == 0L) {
      return(empty_result)
    }
    return(data)
  }


  # Download and Return the Cluster, Plots and Trees
  cluster_list <- get_cluster(
    url                  = url,
    apikey               = apikey,
    limit                = page_size,
    include_test_cluster = include_test_cluster
  )
  if (length(cluster_list) == 0L) {
    return(empty_result)
  }

  data <- fetch_join_table(
    params         = filter_params,
    endpoint       = "plot",
    cluster_list   = cluster_list,
    step           = step,
    url            = url,
    apikey         = apikey,
    accept_profile = "inventory_archive",
    page_size      = page_size
  )
  if (nrow(data) == 0L) {
    return(empty_result)
  }
  if (nrow(data) > 0L) {
    data <- explode_column(data, "tree")
  } else {
    data[["tree"]] <- NULL
    data[["tree_id"]] <- character(0)
  }
}
