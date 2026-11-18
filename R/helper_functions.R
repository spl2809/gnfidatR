## Helper functions

# ------------------------------------------------------------------------------
# Logging
# ------------------------------------------------------------------------------

#' Log a timestamped message, optionally with elapsed time
#'
#' @param msg Character string to print.
#' @param start_time Optional elapsed-time scalar from proc.time();
#'   if supplied, elapsed
#'   seconds since that point are appended to the message.
#' @keywords internal
#' @return NULL invisibly
#' @author Nils Steuernagel

log_step <- function(msg, start_time = NULL) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (is.null(start_time)) {
    cat(sprintf("[%s] %s\n", ts, msg))
  } else {
    dur <- proc.time()[["elapsed"]] - start_time
    cat(sprintf("[%s] %s (%.2f s)\n", ts, msg, dur))
  }
  invisible(NULL)
}




#' Unnest child data frames and replace NULL scalar fields with NA
#'
#' Useful after reading nested JSON responses where child records arrive as
#' embedded data frames (one per parent row).
#'
#' @param df  A data frame containing the list-column to expand.
#' @param col Character; name of the list-column to unnest.
#' @keywords internal
#' @return A data frame with one row per child record. Parents with empty or
#'   NULL child tables are dropped by tidyr::unnest()'s default behavior.
#' @author Nils Steuernagel
explode_column <- function(df, col) {

  df[[col]] <- lapply(df[[col]], function(nested) {
    if (is.null(nested) || !is.data.frame(nested)) return(nested)
    # Replace NULL list elements inside the nested frame with NA so that
    # unnest() can coerce columns to atomic vectors
    nested[] <- lapply(nested, function(x) {
      if (is.list(x)) {
        x <- unlist(lapply(x, function(v) if (is.null(v)) NA else v))
      }
      x
    })
    nested
  })

  tidyr::unnest(df, tidyselect::all_of(col), names_sep = NULL)
}

