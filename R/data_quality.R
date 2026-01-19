#' Census Data Quality Report
#'
#' @description Analyzes census data for missing values and suppression patterns.
#' When data includes suppression flag columns (created by \code{get_census(..., preserve_suppression_flags = TRUE)}),
#' provides detailed breakdown by suppression type. Otherwise, reports NA counts.
#'
#' Statistics Canada suppresses data for various reasons including:
#' \itemize{
#'   \item \code{x}, \code{X}: Data suppressed for confidentiality
#'   \item \code{F}: Too unreliable to be published (high coefficient of variation)
#'   \item \code{...}, \code{..}: Not applicable or not available
#'   \item \code{-}: Zero or rounded to zero
#'   \item \code{N}: Not applicable for reference period
#'   \item \code{*}, \code{**}: Various special cases
#' }
#'
#' @param data A data frame returned by \code{get_census()}.
#' @param warn_threshold Numeric between 0 and 1. If the missing rate exceeds this
#'   threshold for any variable, a warning is issued. Default is 0.2 (20\%).
#' @param quiet If TRUE, suppress warnings about high suppression rates. Default is FALSE.
#'
#' @return A tibble with data quality statistics for each census vector variable:
#' \describe{
#'   \item{variable}{The census vector code}
#'   \item{total}{Total number of observations}
#'   \item{non_missing}{Number of non-NA values}
#'   \item{missing}{Number of NA values}
#'   \item{missing_pct}{Percentage of missing values}
#'   \item{x_count}{Count of 'x'/'X' suppressions (if flag columns present)}
#'   \item{F_count}{Count of 'F' suppressions (if flag columns present)}
#'   \item{other_na}{Count of other NA types (if flag columns present)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage - shows NA counts without breakdown
#' data <- get_census("CA21", regions = list(CMA = "59933"),
#'                    vectors = c("v_CA21_434"), level = "DA")
#' census_data_quality(data)
#'
#' # With suppression flags preserved - shows detailed breakdown
#' data <- get_census("CA21", regions = list(CMA = "59933"),
#'                    vectors = c("v_CA21_434"), level = "DA",
#'                    preserve_suppression_flags = TRUE)
#' census_data_quality(data)
#' }
census_data_quality <- function(data, warn_threshold = 0.2, quiet = FALSE) {
  if (!inherits(data, "data.frame")) {
    stop("Input must be a data frame from get_census().", call. = FALSE)
  }

  # Find census vector columns (v_XXX_NNN pattern)
  all_cols <- names(data)
  vector_cols <- all_cols[grepl("^v_[A-Z]+[0-9]+_[0-9]+$", all_cols)]

  if (length(vector_cols) == 0) {
    message("No census vector columns found in data.")
    return(invisible(NULL))
  }

  # Check if flag columns exist
  flag_cols <- all_cols[grepl("_flag$", all_cols)]
  has_flags <- length(flag_cols) > 0

  # Build quality report
  quality_rows <- lapply(vector_cols, function(vec_col) {
    values <- data[[vec_col]]
    total <- length(values)
    non_missing <- sum(!is.na(values))
    missing <- sum(is.na(values))
    missing_pct <- round(100 * missing / total, 1)

    result <- list(
      variable = vec_col,
      total = total,
      non_missing = non_missing,
      missing = missing,
      missing_pct = missing_pct
    )

    # If flag column exists, add breakdown
    flag_col_name <- paste0(vec_col, "_flag")
    if (flag_col_name %in% flag_cols) {
      flags <- data[[flag_col_name]]
      result$x_count <- sum(flags %in% c("x", "X"), na.rm = TRUE)
      result$F_count <- sum(flags == "F", na.rm = TRUE)
      result$other_na <- missing - result$x_count - result$F_count
    }

    result
  })

  quality_df <- dplyr::bind_rows(quality_rows)

  # Warn about high suppression rates
  if (!quiet) {
    high_missing <- quality_df[quality_df$missing_pct > (warn_threshold * 100), ]
    if (nrow(high_missing) > 0) {
      vars <- paste(high_missing$variable, collapse = ", ")
      warning(sprintf(
        "High missing rate (>%d%%) detected for: %s\nConsider using a coarser geographic level (e.g., CSD instead of DA) for better data coverage.",
        as.integer(warn_threshold * 100), vars
      ), call. = FALSE)
    }
  }

  # Add attribute indicating if flags are present
  attr(quality_df, "has_suppression_flags") <- has_flags

  if (!has_flags && !quiet) {
    message("Note: For detailed suppression breakdowns, use get_census(..., preserve_suppression_flags = TRUE)")
  }

  quality_df
}


#' Create suppression flag columns for census vectors
#'
#' @description Internal function that extracts suppression codes from character
#' vector columns before they are converted to numeric. Creates companion columns
#' with suffix \code{_flag} containing the original suppression codes.
#'
#' @param data A data frame with character columns
#' @param na_strings Vector of strings that represent suppression codes
#'
#' @return The data frame with additional \code{_flag} columns for each census vector
#'
#' @keywords internal
#' @noRd
create_suppression_flags <- function(data, na_strings = cancensus_na_strings) {
  # Find census vector columns (v_XXX_NNN pattern)
  all_cols <- names(data)
  vector_cols <- all_cols[grepl("^v_[A-Z]+[0-9]+_[0-9]+$", all_cols)]

  for (vec_col in vector_cols) {
    flag_col_name <- paste0(vec_col, "_flag")
    values <- data[[vec_col]]

    # Extract suppression codes (only for values that are in na_strings)
    flags <- ifelse(values %in% na_strings, values, NA_character_)

    # Add flag column using base R assignment
    data[[flag_col_name]] <- flags
  }

  # Reorder columns to put _flag columns right after their source columns
  new_order <- character(0)
  for (col in all_cols) {
    new_order <- c(new_order, col)
    flag_col <- paste0(col, "_flag")
    if (flag_col %in% names(data)) {
      new_order <- c(new_order, flag_col)
    }
  }
  data <- data[, new_order]

  data
}
