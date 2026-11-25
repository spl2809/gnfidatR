#' Query, display, and optionally export a lookup table
#'
#' Retrieve a lookup table selected with \code{table} through
#' \code{gnfi_query()}. Optionally open the result in a data viewer or export
#' it to CSV or SQLite.
#'
#' @param table A single character string identifying a lookup table, without
#'   the \code{lookup_} prefix, for example \code{"state"} or
#'   \code{"tree_species"}. The prefix is added internally. See the
#'   \emph{Supported lookup tables} section for all accepted values.
#' @param url Base URL of the REST API. Defaults to
#'   \code{"https://ci.thuenen.de/rest/v1/"}.
#' @param apikey API key passed to \code{gnfi_query()}. A public API key is
#'   included as the default. Supply this argument explicitly to use a
#'   different key.
#' @param page_size Page size for API requests, passed to the \code{limit}
#'   argument of \code{gnfi_query()}. Defaults to \code{-1}; its interpretation is
#'   handled by \code{gnfi_query()}.
#' @param create_csv Logical. Enable CSV export through \code{gnfi_query()}.
#'   Defaults to \code{FALSE}.
#' @param csv_name File name or path for the CSV export. Defaults to
#'   \code{NULL}; its interpretation is handled by \code{gnfi_query()}.
#' @param create_sqlite Logical. Enable SQLite export through
#'   \code{gnfi_query()}. Defaults to \code{FALSE}.
#' @param sqlite_name File name or path for the SQLite database. Defaults to
#'   \code{NULL}; its interpretation is handled by \code{gnfi_query()}.
#' @param sqlite_table Table name within the SQLite database. Defaults to
#'   \code{NULL}; its interpretation is handled by \code{gnfi_query()}.
#' @param view_data Logical. If \code{TRUE}, open the result with
#'   \code{utils::View()}. Defaults to \code{TRUE}. Set to \code{FALSE} in
#'   scripts, when knitting, or when no data viewer is available.
#'
#' @return The object returned by \code{gnfi_query(return_data = TRUE)},
#'   returned invisibly. It can be assigned to a variable whether or not the
#'   data viewer is opened. Its type is determined by \code{gnfi_query()}.
#'
#' @details
#' The \code{lookup_} prefix is added to \code{table} automatically. For
#' example, \code{table = "state"} queries \code{lookup_state} in the
#' \code{lookup} API schema. Supply the short name to this function;
#' a direct call to \code{gnfi_query()} uses the full API table name.
#'
#' All columns are requested with
#' \code{params = list(select = "*", order = "code,id")},
#' \code{accept_profile = "lookup"}, and \code{return_data = TRUE}.
#' Rows are ordered by \code{code}, then \code{id}. The function does not
#' select an inventory cycle automatically; check the returned
#' \code{interval} values when interpreting historical codes.
#'
#' The default viewer title is derived from the lookup name by replacing
#' underscores with spaces and applying title case, for example
#' \code{"land_use"} becomes \code{"Land Use"}.
#'
#' Export options are forwarded to \code{gnfi_query()}. The result is still
#' returned when an export is enabled. API and export errors from
#' \code{gnfi_query()} are propagated, and results are not cached.
#'
#' #' @section Supported lookup tables:
#' The following values are accepted for \code{table}. Supply the names
#' exactly as shown, without the \code{lookup_} prefix:
#'
#' \code{"accessibility"}, \code{"bark_condition"}, \code{"basal_area_factor"},
#' \code{"biogeographische_region"}, \code{"biosphaere"}, \code{"biotope"},
#' \code{"browsing"}, \code{"cluster_situation"}, \code{"cluster_status"},
#' \code{"cover_percentage"}, \code{"damage_peel"}, \code{"dead_wood_type"},
#' \code{"decomposition"}, \code{"district"}, \code{"edge_stand_difference"},
#' \code{"edge_status"}, \code{"edge_type"}, \code{"edge_type_deprecated"},
#' \code{"elevation_level"}, \code{"exploration_instruction"}, \code{"ffh"},
#' \code{"ffh_forest_type"}, \code{"forest_community"}, \code{"forest_office"},
#' \code{"forest_status"}, \code{"gnss_quality"}, \code{"grid_density"},
#' \code{"growth_district"}, \code{"growth_region"},
#' \code{"harvest_condition"}, \code{"harvest_method"},
#' \code{"harvest_reason"}, \code{"harvest_restriction"}, \code{"interval"},
#' \code{"land_use"}, \code{"layer"}, \code{"management_type"},
#' \code{"marker_profile"}, \code{"marker_status"}, \code{"municipality"},
#' \code{"national_park"}, \code{"natur_park"}, \code{"natur_schutzgebiet"},
#' \code{"object_type"}, \code{"property_size_class"}, \code{"property_type"},
#' \code{"pruning"}, \code{"sampling_stratum"},
#' \code{"stand_development_phase"}, \code{"stand_layer"},
#' \code{"stand_structure"}, \code{"state"}, \code{"stem_breakage"},
#' \code{"stem_form"}, \code{"support_point_type"}, \code{"terrain"},
#' \code{"terrain_form"}, \code{"tree_size_class"}, \code{"tree_species"},
#' \code{"tree_species_group"}, \code{"tree_status"},
#' \code{"trees_greater_4meter_mirrored"}, \code{"trees_less_4meter_origin"},
#' \code{"vogel_schutzgebiet"}. 
#'
#' @seealso \code{\link{gnfi_query}}, \code{\link[utils]{View}}
#' @examples
#' \dontrun{
#' # Retrieve a lookup table and open it in the data viewer:
#' view_lookup(table = "state")
#'
#' # Keep the result without opening the viewer:
#' tree_species <- view_lookup(table = "tree_species", view_data = FALSE)
#' head(tree_species)
#'
#' # Use a different API key:
#' land_use <- view_lookup(
#'   table = "land_use",
#'   apikey = "another-api-key",
#'   view_data = FALSE
#' )
#'
#' # Export to CSV and keep the returned object:
#' marker_profile <- view_lookup(
#'   table = "marker_profile",
#'   create_csv = TRUE,
#'   csv_name = "marker_profile.csv",
#'   view_data = FALSE
#' )
#'
#' # Export to a table in a SQLite database:
#' view_lookup(
#'   table = "harvest_method",
#'   create_sqlite = TRUE,
#'   sqlite_name = "lookups.sqlite",
#'   sqlite_table = "harvest_method",
#'   view_data = FALSE
#' )
#' }
#' @export
#' @author Nils Steuernagel
view_lookup <- function(
  table,
  url = "https://ci.thuenen.de/rest/v1/",
  apikey = paste0(
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.",
    "eyJyb2xlIjoiYW5vbiIsImlzcyI6InN1cGFiYXN",
    "lIiwiaWF0IjoxNzQ1NzkxMjAwLCJleHAiOjE5MDM1",
    "NTc2MDB9.hXiYlA_168hHZ6fk3zPgABQUpEcqkYRMzu0A5W5PtYU"
  ),
  page_size = -1,
  create_csv = FALSE,
  csv_name = NULL,
  create_sqlite = FALSE,
  sqlite_name = NULL,
  sqlite_table = NULL,
  view_data = TRUE
) {
  table <- paste0("lookup_", table)
  table <- validate_identifier(
    table,
    "table",
    paste0(
      "^(lookup_accessibility|lookup_bark_condition|lookup_basal_area_factor|",
      "lookup_biogeographische_region|lookup_biosphaere|lookup_biotope",
      "|lookup_browsing|lookup_cluster_situation|lookup_cluster_status",
      "|lookup_cover_percentage|lookup_damage_peel|lookup_dead_wood_type|",
      "lookup_decomposition|lookup_district|lookup_edge_stand_difference|",
      "lookup_edge_status|lookup_edge_type|lookup_edge_type_deprecated|",
      "lookup_elevation_level|lookup_exploration_instruction|lookup_ffh|",
      "lookup_ffh_forest_type|lookup_forest_community|lookup_forest_office|",
      "lookup_forest_status|lookup_gnss_quality|lookup_grid_density|",
      "lookup_growth_district|lookup_growth_region|lookup_harvest_condition|",
      "lookup_harvest_method|lookup_harvest_reason|lookup_harvest_restriction",
      "|lookup_interval|lookup_land_use|lookup_layer|lookup_management_type|",
      "lookup_marker_profile|lookup_marker_status|lookup_municipality|",
      "lookup_national_park|lookup_natur_park|lookup_natur_schutzgebiet",
      "|lookup_object_type|lookup_property_size_class|lookup_property_type|",
      "lookup_pruning|lookup_sampling_stratum|lookup_stand_development_phase|",
      "lookup_stand_layer|lookup_stand_structure|lookup_state|",
      "lookup_stem_breakage|lookup_stem_form|lookup_support_point_type|",
      "lookup_terrain|lookup_terrain_form|lookup_tree_size_class|",
      "lookup_tree_species|lookup_tree_species_group|lookup_tree_status|",
      "lookup_trees_greater_4meter_mirrored|lookup_trees_less_4meter_origin|",
      "lookup_vogel_schutzgebiet)$"
    )
  )
  validate_flags(list(view_data = view_data))
  page_size <- validate_page_size(page_size, "page_size", allow_all = TRUE)

  data <- gnfi_query(
    table = table,
    params = list(select = "*", order = "code,id"),
    limit = page_size,
    url = url,
    apikey = apikey,
    accept_profile = "lookup",
    create_csv = create_csv,
    csv_name = csv_name,
    create_sqlite = create_sqlite,
    sqlite_name = sqlite_name,
    sqlite_table = sqlite_table,
    return_data = TRUE
  )

  if (view_data) {
    title <- sub("^lookup_", "", table)
    title <- tools::toTitleCase(gsub("_", " ", title))

    utils::View(data, title = title)
  }

  invisible(data)
}
