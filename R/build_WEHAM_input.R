# ------------------------------------------------------------------------------
# SQLite Table Builders
# ------------------------------------------------------------------------------

#' Join plot/tree data with derived metrics and write WEHAMI_WZP to SQLite
#'
#' Overwrites existing tables names `"WEHAMI_WZP"`
#'
#' @param url             Base URL of the REST API.
#' @param apikey          API key.
#' @param conn            An open DBI connection to the SQLite database.
#' @param plots_and_trees Data frame returned by `plots_and_trees_fetch()`.
#' @param rename          Logical; if TRUE columns are renamed to German
#'   short codes (Tnr, Enr, ...) used by downstream WEHAM models.
#' @param page_size  Requested page size for derived data; all pages are fetched.
#' @keywords internal
#' @return NULL invisibly (called for its side-effect of writing to SQLite).
#' @author Nils Steuernagel
build_wzp <- function(url, apikey, conn, plots_and_trees, rename, page_size) {
  derived <- if (nrow(plots_and_trees) == 0L) {
    data.frame(
      intkey = character(),
      dbh = numeric(),
      tree_height = numeric(),
      stem_height = numeric(),
      trees_per_hectare = numeric(),
      diameter_30perc = numeric()
    )
  } else {
    get_derived_tree(url, apikey, page_size)
  }

  if (!"intkey" %in% names(plots_and_trees) || !"intkey" %in% names(derived)) {
    stop("Full tree intkeys are required to join derived metrics safely.", call. = FALSE)
  }
  if (anyNA(plots_and_trees$intkey) || anyNA(derived$intkey) ||
    anyDuplicated(derived$intkey)) {
    stop("Tree intkeys must be non-missing; derived intkeys must be unique.", call. = FALSE)
  }

  # Retain the full identity, including the inventory context encoded in intkey.
  wzp <- dplyr::inner_join(
    plots_and_trees, derived,
    by = "intkey", na_matches = "never"
  )
  if (nrow(plots_and_trees) > 0L && nrow(wzp) == 0L) {
    stop("No derived metrics match the requested inventory's full tree intkeys.",
      call. = FALSE
    )
  }
  omitted <- nrow(plots_and_trees) - nrow(wzp)
  if (omitted > 0L) {
    warning(sprintf(
      "%s tree rows have no matching derived metrics and are omitted.",
      omitted
    ), call. = FALSE)
  }

  if (rename) {
    wzp <- wzp |>
      dplyr::rename(
        Tnr   = tidyselect::all_of("cluster_name"),
        Enr   = tidyselect::all_of("plot_name"),
        Bnr   = tidyselect::all_of("tree_number"),
        Bs    = tidyselect::all_of("stand_layer"),
        Ba    = tidyselect::all_of("tree_species"),
        Kh    = tidyselect::all_of("stem_breakage"),
        Kst   = tidyselect::all_of("stem_form"),
        Bhd   = tidyselect::all_of("dbh"),
        Hoe   = tidyselect::all_of("tree_height"),
        StHoe = tidyselect::all_of("stem_height"),
        Al_ba = tidyselect::all_of("tree_age"),
        N_ha  = tidyselect::all_of("trees_per_hectare"),
        D03   = tidyselect::all_of("diameter_30perc")
      )
    cols <- c(
      "Tnr", "Enr", "Bnr", "Bs", "Ba", "Kh", "Kst",
      "Bhd", "D03", "Hoe", "StHoe", "Al_ba", "N_ha"
    )
  } else {
    cols <- c(
      "cluster_name", "plot_name", "tree_number", "stand_layer",
      "tree_species", "stem_breakage", "stem_form", "dbh",
      "diameter_30perc", "tree_height", "stem_height",
      "tree_age", "trees_per_hectare"
    )
  }

  wzp <- dplyr::select(wzp, tidyselect::all_of(cols))

  RSQLite::dbWriteTable(conn, "WEHAMI_WZP", wzp, overwrite = TRUE, row.names = FALSE)
  invisible(NULL)
}


#' Renames data and write WEHAMI_Jung to SQLite
#'
#' Overwrites the existing table named WEHAMI_Jung.
#'
#' @param conn                         An open DBI connection to the SQLite database.
#' @param plots_and_trees_regeneration The data frame holding the jung data
#' @param rename                       Logical; rename columns to German short codes.
#' @keywords internal
#' @return NULL invisibly.
#' @author Nils Steuernagel
build_jung <- function(conn, plots_and_trees_regeneration, rename) {
  jung <- plots_and_trees_regeneration

  if (rename) {
    jung <- jung |>
      dplyr::rename(
        Tnr = tidyselect::all_of("cluster_name"),
        Enr = tidyselect::all_of("plot_name"),
        Bs  = tidyselect::all_of("stand_layer_regeneration"),
        Ba  = tidyselect::all_of("tree_species"),
        Gr  = tidyselect::all_of("tree_size_class"),
        Anz = tidyselect::all_of("tree_count")
      )
    cols <- c("Tnr", "Enr", "Ba", "Bs", "Gr", "Anz")
  } else {
    cols <- c(
      "cluster_name", "plot_name", "tree_species",
      "stand_layer_regeneration", "tree_size_class", "tree_count"
    )
  }

  jung <- dplyr::select(jung, tidyselect::all_of(cols))
  RSQLite::dbWriteTable(conn, "WEHAMI_Jung", jung, overwrite = TRUE, row.names = FALSE)
  invisible(NULL)
}


#' Extract plot-level stand attributes and write WEHAMI_Eck to SQLite
#'
#' Selects the plot metadata columns from the combined plot/tree data frame,
#' deduplicates to one row per plot, and writes to SQLite.
#'
#' Overwrites the existing table named WEHAMI_Eck.
#'
#' @param conn                          An open DBI connection to the SQLite database.
#' @param plots_and_trees               Data frame returned by `plots_and_trees_fetch()`.
#' @param plots_and_trees_regeneration  Data frame returned by `regeneration_fetch()`.
#' @param share_of_natural_regeneration The share of natural regeneration used for WEHAM.
#' @param rename                        Logical; rename columns to German short codes.
#' @keywords internal
#' @return NULL invisibly.
#' @author Nils Steuernagel
build_eck <- function(conn, plots_and_trees, plots_and_trees_regeneration, share_of_natural_regeneration, rename) {
  cols <- c(
    "cluster_name", "plot_name", "federal_state", "property_type",
    "acquisition_date", "property_size_class", "management_type",
    "harvest_restriction", "stand_layer_regeneration", "sampling_stratum"
  )
  # Combine
  eck <- dplyr::bind_rows(
    plots_and_trees |> dplyr::select(dplyr::all_of(cols)),
    plots_and_trees_regeneration |> dplyr::select(dplyr::all_of(cols))
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(BsP175 = .data$stand_layer_regeneration) |>
    dplyr::relocate(tidyselect::all_of("BsP175"), .after = tidyselect::all_of("stand_layer_regeneration")) |>
    dplyr::mutate(NatVjProz = as.integer(share_of_natural_regeneration)) |>
    dplyr::mutate(property_size_class = as.integer(ifelse(is.na(.data$property_size_class), -1, .data$property_size_class)))

  if (rename) {
    eck <- eck |>
      dplyr::rename(
        Tnr       = tidyselect::all_of("cluster_name"),
        Enr       = tidyselect::all_of("plot_name"),
        Bl        = tidyselect::all_of("federal_state"),
        Eg        = tidyselect::all_of("property_type"),
        DatumFeld = tidyselect::all_of("acquisition_date"),
        Grkl      = tidyselect::all_of("property_size_class"),
        Be        = tidyselect::all_of("management_type"),
        Ne        = tidyselect::all_of("harvest_restriction"),
        BsP100    = tidyselect::all_of("stand_layer_regeneration"),
        Vbl       = tidyselect::all_of("sampling_stratum")
      )
  }

  # Only keep plots which occur in WZP or Jung
  key_cols <- if (rename) {
    c("Tnr", "Enr")
  } else {
    c("cluster_name", "plot_name")
  }
  key_sql <- paste(key_cols, collapse = ", ")
  if (anyDuplicated(as.data.frame(dplyr::select(eck, tidyselect::all_of(key_cols))))) {
    stop("Conflicting plot metadata would create duplicate WEHAMI_Eck keys.",
      call. = FALSE
    )
  }
  existing_plots <- DBI::dbGetQuery(
    conn,
    paste0(
      "SELECT ", key_sql, " FROM WEHAMI_WZP ",
      "UNION ",
      "SELECT ", key_sql, " FROM WEHAMI_Jung"
    )
  )

  eck <- dplyr::semi_join(
    eck,
    existing_plots,
    by = key_cols,
    na_matches = "never"
  )

  RSQLite::dbWriteTable(conn, "WEHAMI_Eck", eck, overwrite = TRUE, row.names = FALSE)
  invisible(NULL)
}


# ------------------------------------------------------------------------------
# WEHAM Entry Point
# ------------------------------------------------------------------------------

#' Download NFI data from Thuenen API
#'
#' Orchestrates cluster retrieval, plot/tree fetching, and writing the three
#' SQLite tables (WEHAMI_Eck, WEHAMI_WZP, WEHAMI_Jung) in one transaction.
#' Derived tree metrics are matched by the full tree intkey. An inventory needs
#' corresponding derived metrics; acceptance of an interval name alone does not
#' guarantee that these data are available. If no tree metrics match, import stops.
#'
#' @param db_path              Path to the target SQLite file (created if absent).
#' @param interval_name        Inventory interval to import (e.g. "bwi2022").
#' @param share_of_natural_regeneration Whole-number percentage in 0:100.
#' @param filtering_options NULL or a named list of plot-level filters, as in
#'   get_filtered_plots(). Empty lists and NULL entries add no filtering. An
#'   interval_name filter must include the requested inventory interval.
#' @param include_test_cluster Logical; whether to include training clusters.
#' @param rename               Logical; use German short-code column names.
#' @param print_progress       Logical; should information about progress be printed.
#' @param url                  URL of the Thuenen-API
#' @param apikey              API-key, see details
#' @param step_size            Number of clusters per batch downloaded at each step. Reduce if the server sends a timeout.
#' @param page_size Requested rows per response for the cluster list, plot
#'   batches, and derived data. Defaults to 10000; independent of step_size.
#' @details
#' Public connection settings are shown in the Thuenen API documentation:
#' \url{https://thuenen-forest-ecosystems.github.io/TFM-Documentation/api/data-structure.html}.
#'
#'
#' @return NULL invisibly.
#' @examples
#' \dontrun{
#' db_path <- "example_weham.sqlite"
#'
#' get_weham_data(
#'   db_path = db_path,
#'   interval_name = "bwi2022",
#'   share_of_natural_regeneration = 100,
#'   include_test_cluster = FALSE,
#'   rename = TRUE,
#'   print_progress = TRUE,
#'   step_size = 1000,
#'   page_size = 10000
#' )
#' }
#' @export
#' @author Nils Steuernagel, Stefan Paul
get_weham_data <- function(db_path,
                           interval_name,
                           share_of_natural_regeneration = 100,
                           filtering_options = NULL,
                           include_test_cluster = FALSE,
                           rename = TRUE,
                           print_progress = TRUE,
                           url = "https://ci.thuenen.de/rest/v1/",
                           apikey = NULL,
                           step_size = 1000,
                           page_size = 10000) {
  if (is.null(apikey)) {
    apikey <- paste0(
      "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.",
      "eyJyb2xlIjoiYW5vbiIsImlzcyI6InN1cGFiYXNlIiwiaWF0IjoxNzQ1",
      "NzkxMjAwLCJleHAiOjE5MDM1NTc2MDB9.hXiYlA_168hHZ6fk3zPgABQUpEcqkYRMzu0A5W5PtYU"
    )
  }

  db_path <- validate_output_path(db_path, "db_path", sqlite = TRUE)
  filtering_options <- validate_filtering_options(filtering_options)
  interval_name <- validate_weham_interval(interval_name, filtering_options)
  share_of_natural_regeneration <- validate_regeneration_share(share_of_natural_regeneration)
  validate_flags(list(
    include_test_cluster = include_test_cluster,
    rename = rename,
    print_progress = print_progress
  ))
  url <- validate_api(url, apikey)
  step_size <- validate_page_size(step_size, "step_size")
  page_size <- validate_page_size(page_size, "page_size")


  json_step <- step_size

  if (print_progress) {
    start_total <- proc.time()[["elapsed"]]

    log_step("Starting WEHAM-import")
    log_step(paste("Database:", db_path))
    log_step(paste("Interval:", interval_name))
  }

  # -- 1. Cluster list ---------------------------------------------------------
  if (print_progress) {
    log_step("Loading cluster list...")
    t0 <- proc.time()[["elapsed"]]
  }
  cluster_liste <- get_cluster(url, apikey, page_size, include_test_cluster)
  if (length(cluster_liste) == 0L) {
    stop(
      "The downloaded list of clusters is empty. This should NEVER occur.
      Check if you gave the correct URL, API key and try again. Contact the maintainer!",
      call. = FALSE
    )
  }
  if (print_progress) {
    log_step(paste("Finished loading cluster list:", length(cluster_liste)), t0)
  }


  # -- 2. Plot and tree data ---------------------------------------------------
  if (print_progress) {
    log_step("Loading plot and tree data (this may take a while)...")
    t0 <- proc.time()[["elapsed"]]
  }
  PaT <- plots_and_trees_fetch(
    url = url,
    apikey = apikey,
    cluster_liste = cluster_liste,
    interval_name = interval_name,
    step = json_step,
    page_size = page_size
  )
  PaT_reg <- regeneration_fetch(
    url = url,
    apikey = apikey,
    cluster_liste = cluster_liste,
    interval_name = interval_name,
    step = json_step,
    page_size = page_size
  )

  if (nrow(PaT) == 0L || nrow(PaT_reg) == 0L) {
    stop("No tree or regeneration data were found for the requested inventory.",
      call. = FALSE
    )
  }
  if (print_progress) {
    log_step(
      paste("Finished loading plots and trees. Count unique trees :", nrow(PaT)),
      t0
    )
  }

  # -- Optional filtering of the downloaded plots ------------------------------
  if (length(filtering_options) > 0L) {
    if (print_progress) {
      log_step("Start filtering the plot data...")
      t0 <- proc.time()[["elapsed"]]
    }
    filter_plot_ids <- get_filtered_plots(
      filtering_options = filtering_options,
      url               = url,
      apikey            = apikey,
      step              = step_size,
      page_size         = page_size,
      include_tree_ids  = FALSE # we only need the plots
    )

    PaT <- filter_df(PaT, filter_plot_ids)
    PaT_reg <- filter_df(PaT_reg, filter_plot_ids)

    if (nrow(PaT) == 0L || nrow(PaT_reg) == 0L) {
      stop("After filtering, no tree or regeneration data remained for the requested inventory.",
        call. = FALSE
      )
    }

    if (print_progress) {
      log_step(
        paste("Finished filtering the plot data.", nrow(PaT), "unique trees remain"),
        t0
      )
    }
  }

  # -- 3. Open database --------------------------------------------------------
  if (print_progress) {
    log_step("Opening SQLite database ...")
  }
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(RSQLite::dbDisconnect(conn), add = TRUE)


  DBI::dbWithTransaction(conn, {
    # -- 4. Winkelzählprobe ------------------------------------------------------
    if (print_progress) {
      log_step("Writing WEHAMI_WZP ...")
      t0 <- proc.time()[["elapsed"]]
    }
    build_wzp(url, apikey, conn, PaT, rename, page_size)
    if (print_progress) {
      log_step("Finished WEHAMI_WZP", t0)
    }

    # -- 5. Jungholz -------------------------------------------------------------
    if (print_progress) {
      log_step("Writing WEHAMI_Jung ...")
      t0 <- proc.time()[["elapsed"]]
    }
    build_jung(
      conn,
      PaT_reg,
      rename
    )
    if (print_progress) {
      log_step("Finished WEHAMI_Jung", t0)
    }

    # -- 6. Tracktecken-----------------------------------------------------------
    if (print_progress) {
      log_step("Writing WEHAMI_Eck ...")
      t0 <- proc.time()[["elapsed"]]
    }
    build_eck(
      conn,
      PaT,
      PaT_reg,
      share_of_natural_regeneration,
      rename
    )
  })
  if (print_progress) {
    log_step("Finished WEHAMI_Eck", t0)

    # --------------------------------------------------------------------------
    total_duration <- proc.time()[["elapsed"]] - start_total
    log_step(sprintf("Finished import - Time elapsed: %.2f s", total_duration))
  }
  invisible(NULL)
}


#' Retrieve the sorted list of cluster names from the API
#'
#' @param url                  Base URL of the REST API.
#' @param apikey               API key.
#' @param limit Requested rows per response; all matching pages are fetched.
#' @param include_test_cluster Logical; if FALSE (default) training clusters
#'   are excluded via an `is_training=eq.false` filter.
#' @keywords internal
#' @return A sorted numeric vector of unique cluster names.
#' @author Nils Steuernagel
get_cluster <- function(url, apikey, limit, include_test_cluster = FALSE) {
  params <- list(
    select = "cluster_name",
    order = "cluster_name.asc"
  )

  if (!include_test_cluster) {
    params[["is_training"]] <- "eq.false"
  }

  cluster <- fetch_single_table(params, "cluster", url, apikey, limit)

  return(sort(unique(cluster$cluster_name)))
}

#' Fetch plot metadata joined with individual tree measurements
#'
#' Queries the `plot` endpoint with an embedded `tree` join and expands the
#' nested tree records into one row per tree.
#'
#' @param url           Base URL of the REST API.
#' @param apikey        API key.
#' @param cluster_liste Numeric cluster IDs (or numeric strings) to include.
#' @param interval_name Inventory interval identifier (e.g. "bwi2022").
#' @param step          Cluster batch size passed to `fetch_join_table()`.
#' @param page_size     Requested plot rows per response within a cluster batch.
#' @keywords internal
#' @return A data frame with one row per tree, including plot-level columns.
#' @author Nils Steuernagel
plots_and_trees_fetch <- function(url, apikey, cluster_liste, interval_name, step, page_size) {
  params <- list(
    interval_name = paste0("eq.", interval_name),
    select = paste0(
      "plot_id:id,cluster_name,plot_name,federal_state,property_type,",
      "acquisition_date,property_size_class,management_type,harvest_restriction,",
      "stand_layer_regeneration,sampling_stratum,",
      "tree!inner(",
      "intkey,tree_number,stand_layer,tree_species,stem_breakage,",
      "stem_form,tree_age",
      ")"
    ),
    order = "id.asc"
  )

  df <- fetch_join_table(
    params = params,
    endpoint = "plot",
    cluster_list = cluster_liste,
    step = step,
    url = url,
    apikey = apikey,
    accept_profile = "inventory_archive",
    page_size = page_size
  )
  if (nrow(df) == 0L) {
    return(empty_plot_records("tree"))
  }
  df <- explode_column(df, "tree")
  return(df)
}


#' Fetch plot metadata joined with individual tree measurements from regeneration
#'
#' Queries the `plot` endpoint with an embedded `regeneration` join.
#'
#' @param url           Base URL of the REST API.
#' @param apikey        API key.
#' @param cluster_liste Numeric cluster IDs (or numeric strings) to include.
#' @param interval_name Inventory interval identifier (e.g. "bwi2022").
#' @param step          Cluster batch size passed to `fetch_join_table()`.
#' @param page_size     Requested plot rows per response within a cluster batch.
#' @keywords internal
#' @return A data frame with one row per regeneration record, including plot
#'   metadata. Equal projections of distinct source records are retained.
#' @author Nils Steuernagel, Stefan Paul
regeneration_fetch <- function(url, apikey, cluster_liste, interval_name, step, page_size) {
  params <- list(
    interval_name = paste0("eq.", interval_name),
    select = paste0(
      "plot_id:id,cluster_name,plot_name,federal_state,property_type,",
      "acquisition_date,property_size_class,management_type,harvest_restriction,",
      "stand_layer_regeneration,sampling_stratum,",
      "regeneration!inner(intkey,tree_species,tree_size_class,tree_count)"
    ),
    order = "id.asc"
  )

  jung <- fetch_join_table(
    params = params,
    endpoint = "plot",
    cluster_list = cluster_liste,
    step = step,
    url = url,
    apikey = apikey,
    accept_profile = "inventory_archive",
    page_size = page_size
  )
  if (nrow(jung) == 0L) {
    return(empty_plot_records("regeneration"))
  }
  jung <- explode_column(jung, "regeneration")
  return(jung)
}


#' Download derived per-tree metrics (trees_per_hectare, diameter_30perc)
#'
#' Fetches from the derived schema. Keeps the complete intkey so that
#' inventory context is retained when joining with measured trees.
#'
#' @param url    Base URL of the REST API.
#' @param apikey API key.
#' @param limit  Requested rows per page; -1 uses automatic pages of 10000.
#' @keywords internal
#' @return A data frame with intkey, trees_per_hectare, and diameter_30perc.
#' @author Nils Steuernagel

get_derived_tree <- function(url, apikey, limit, interval_name = "bwi2022") {
  params <- list(
    select = paste0(
      "intkey,dbh,tree_height,stem_height,trees_per_hectare,diameter_30perc"
    ),
    intkey = paste0("like.*", interval_name),
    order = "intkey.asc"
  )

  derived <- fetch_single_table(
    params,
    "tree",
    url,
    apikey,
    limit,
    accept_profile = "derived"
  )

  return(derived)
}

#' Preserve the selected column schema when a joined query has no rows
#' @keywords internal
#' @noRd
empty_plot_records <- function(child) {
  out <- data.frame(
    cluster_name = integer(),
    plot_name = integer(),
    plot_id = integer(),
    federal_state = integer(),
    property_type = integer(),
    acquisition_date = character(),
    property_size_class = integer(),
    management_type = integer(),
    harvest_restriction = integer(),
    sampling_stratum = integer(),
    stand_layer_regeneration = integer()
  )
  child_data <- switch(child,
    tree = data.frame(
      intkey = character(),
      tree_number = integer(),
      stand_layer = integer(),
      tree_species = integer(),
      stem_breakage = integer(),
      stem_form = integer(),
      tree_age = integer()
    ),
    regeneration = data.frame(
      intkey = character(),
      tree_species = integer(),
      tree_size_class = integer(),
      tree_count = integer()
    ),
    stop("Unknown embedded table.", call. = FALSE)
  )
  cbind(out, child_data)
}
