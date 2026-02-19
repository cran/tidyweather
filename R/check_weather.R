#' Check weather records for data quality issues
#' 
#' This function validates weather records for:
#' * Continuous weather data (no gaps in dates)
#' * No missing values in key columns (mint, maxt, radn, rain)
#' * No extreme values (e.g., less than -100 or above 100 for temperature,
#'   less than 0 for radiation and rain)
#' * Latitude and longitude columns exist and contain a single non-NA value for all records
#' 
#' @param data A data.frame or tibble containing weather records with at minimum
#'   a date column, latitude, longitude, and key weather variables (mint, maxt, radn, rain).
#' @param key_cols A character vector of column names to check for missing values
#'   and extreme values. Default is c("mint", "maxt", "radn", "rain").
#' @param temp_range A numeric vector of length 2 specifying the acceptable range
#'   for temperature values (mint, maxt). Default is c(-100, 100).
#' @param radn_range A numeric vector of length 2 specifying the acceptable range
#'   for radiation values. Default is c(0, 50).
#' @param rain_range A numeric vector of length 2 specifying the acceptable range
#'   for rainfall values. Default is c(0, 500).
#' @param stop_on_error Logical. If TRUE, the function will stop with an error
#'   when issues are found. If FALSE, it will return a list of issues. Default is FALSE.
#' @return If stop_on_error is FALSE, returns a list with the following components:
#'   \item{is_valid}{Logical indicating if all checks passed}
#'   \item{date_gaps}{Data frame of date gaps found, or NULL if none}
#'   \item{missing_values}{Data frame summarizing missing values, or NULL if none}
#'   \item{extreme_values}{Data frame of rows with extreme values, or NULL if none}
#'   If stop_on_error is TRUE and issues are found, the function stops with an error message.
#' @examples
#' file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
#' records <- read_weather(file)
#' result <- check_weather(records)
#' if (result$is_valid) {
#'   print("Weather data passed all quality checks")
#' } else {
#'   print(result)
#' }
#' @export
check_weather <- function(data, 
                        key_cols = c("mint", "maxt", "radn", "rain"),
                        temp_range = c(-100, 100),
                        radn_range = c(0, 50),
                        rain_range = c(0, 500),
                        stop_on_error = FALSE) {
    
    # Input validation
    stopifnot(is.data.frame(data))
    stopifnot(is.character(key_cols))
    stopifnot(is.numeric(temp_range) && length(temp_range) == 2)
    stopifnot(is.numeric(radn_range) && length(radn_range) == 2)
    stopifnot(is.numeric(rain_range) && length(rain_range) == 2)
    stopifnot(is.logical(stop_on_error))
    
    # Check if date column exists
    if (!"date" %in% names(data)) {
        stop("Data must contain a 'date' column")
    }
    
    # Check if key columns exist
    missing_cols <- key_cols[!key_cols %in% names(data)]
    if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check that latitude and longitude columns exist and contain single non-NA values
    if (!"latitude" %in% names(data)) {
        stop("Data must contain a 'latitude' column")
    }
    if (all(is.na(data$latitude))) {
        stop("Latitude column contains only NA values")
    }
    if (length(unique(data$latitude)) != 1) {
        stop("Latitude column should contain a single value for all records")
    }
    
    if (!"longitude" %in% names(data)) {
        stop("Data must contain a 'longitude' column")
    }
    if (all(is.na(data$longitude))) {
        stop("Longitude column contains only NA values")
    }
    if (length(unique(data$longitude)) != 1) {
        stop("Longitude column should contain a single value for all records")
    }
    
    issues <- list()
    is_valid <- TRUE
    
    # Check 1: Continuous dates (no gaps)
    date_gaps <- check_date_continuity(data$date)
    if (!is.null(date_gaps)) {
        is_valid <- FALSE
        issues$date_gaps <- date_gaps
    }
    
    # Check 2: Missing values in key columns
    missing_summary <- check_missing_values(data, key_cols)
    if (!is.null(missing_summary)) {
        is_valid <- FALSE
        issues$missing_values <- missing_summary
    }
    
    # Check 3: Extreme values
    extreme_rows <- check_extreme_values(data, key_cols, 
                                        temp_range, radn_range, rain_range)
    if (!is.null(extreme_rows)) {
        is_valid <- FALSE
        issues$extreme_values <- extreme_rows
    }
    
    # Prepare result
    result <- list(
        is_valid = is_valid,
        date_gaps = issues$date_gaps,
        missing_values = issues$missing_values,
        extreme_values = issues$extreme_values
    )
    
    # Handle errors
    if (!is_valid && stop_on_error) {
        error_msg <- "Weather data quality issues found:\n"
        if (!is.null(issues$date_gaps)) {
            error_msg <- paste0(error_msg, "- ", nrow(issues$date_gaps), 
                                " date gap(s) detected\n")
        }
        if (!is.null(issues$missing_values)) {
            error_msg <- paste0(error_msg, "- Missing values detected: ",
                                paste(issues$missing_values$column, 
                                    paste0("(", issues$missing_values$n_missing, ")"),
                                    collapse = ", "), "\n")
        }
        if (!is.null(issues$extreme_values)) {
            error_msg <- paste0(error_msg, "- ", nrow(issues$extreme_values), 
                                " row(s) with extreme values\n")
        }
        stop(error_msg)
    }
    
    return(result)
}


#' Check for gaps in date sequence
#' @param dates A vector of Date objects
#' @return A data frame with gap information, or NULL if no gaps
#' @keywords internal
check_date_continuity <- function(dates) {
    if (length(dates) < 2) {
        return(NULL)
    }
    
    dates <- sort(dates)
    date_diffs <- as.numeric(diff(dates))
    
    gaps <- which(date_diffs > 1)
    
    if (length(gaps) == 0) {
        return(NULL)
    }
    
    gap_info <- data.frame(
        gap_start = dates[gaps],
        gap_start_doy = format(dates[gaps], "%j"),
        gap_end = dates[gaps + 1],
        gap_end_doy = format(dates[gaps + 1], "%j"),
        days_missing = date_diffs[gaps] - 1
    )
    
    return(gap_info)
}


#' Check for missing values in key columns
#' @param data A data frame
#' @param key_cols Character vector of column names to check
#' @return A data frame with missing value counts, or NULL if no missing values
#' @keywords internal
check_missing_values <- function(data, key_cols) {
    missing_counts <- sapply(data[, key_cols, drop = FALSE], function(x) sum(is.na(x)))
    
    if (all(missing_counts == 0)) {
        return(NULL)
    }
    
    missing_summary <- data.frame(
        column = names(missing_counts),
        n_missing = as.numeric(missing_counts),
        pct_missing = round(100 * as.numeric(missing_counts) / nrow(data), 2)
    )
    
    missing_summary <- missing_summary[missing_summary$n_missing > 0, ]
    rownames(missing_summary) <- NULL
    
    return(missing_summary)
}


#' Check for extreme values in weather columns
#' @param data A data frame
#' @param key_cols Character vector of column names to check
#' @param temp_range Acceptable range for temperature
#' @param radn_range Acceptable range for radiation
#' @param rain_range Acceptable range for rainfall
#' @return A data frame of rows with extreme values, or NULL if none found
#' @keywords internal
check_extreme_values <- function(data, key_cols, temp_range, radn_range, rain_range) {
    extreme_mask <- rep(FALSE, nrow(data))
    issues <- character(0)
    
    # Check temperature columns
    if ("mint" %in% key_cols && "mint" %in% names(data)) {
        mint_extreme <- data$mint < temp_range[1] | data$mint > temp_range[2]
        mint_extreme[is.na(mint_extreme)] <- FALSE
        extreme_mask <- extreme_mask | mint_extreme
    }
    
    if ("maxt" %in% key_cols && "maxt" %in% names(data)) {
        maxt_extreme <- data$maxt < temp_range[1] | data$maxt > temp_range[2]
        maxt_extreme[is.na(maxt_extreme)] <- FALSE
        extreme_mask <- extreme_mask | maxt_extreme
    }
    
    # Check radiation
    if ("radn" %in% key_cols && "radn" %in% names(data)) {
        radn_extreme <- data$radn < radn_range[1] | data$radn > radn_range[2]
        radn_extreme[is.na(radn_extreme)] <- FALSE
        extreme_mask <- extreme_mask | radn_extreme
    }
    
    # Check rainfall
    if ("rain" %in% key_cols && "rain" %in% names(data)) {
        rain_extreme <- data$rain < rain_range[1] | data$rain > rain_range[2]
        rain_extreme[is.na(rain_extreme)] <- FALSE
        extreme_mask <- extreme_mask | rain_extreme
    }
    
    if (!any(extreme_mask)) {
        return(NULL)
    }
    
    extreme_data <- data[extreme_mask, c("date", key_cols), drop = FALSE]
    rownames(extreme_data) <- which(extreme_mask)
    
    return(extreme_data)
}
