#' Calculate the number of frost days
#'
#' This function calculates the number of frost days from a numeric vector of daily
#' minimum temperatures using tidyverse principles.
#'
#' @param .data A data frame or tibble containing daily minimum temperatures in a column named "mint".
#' @param threshold The stress temperature threshold for frost (default: 0)
#' @param require_full_year Logical. If TRUE, requires exactly 365 or 366 days (default: TRUE)
#'
#' @return An data.frame or tibble representing the number of frost days, or 0 if no frost occurs
#' @export
#'
#' @examples
#' file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
#' records <- read_weather(file)
#' records |>
#'     dplyr::group_by(year) |> 
#'     number_frost_day(require_full_year = FALSE)
number_frost_day <- function(.data,
                           threshold = get_weather_option("extreme.frost_threshold"),
                           require_full_year = get_weather_option("require_full_year")) {

    stopifnot(tibble::is_tibble(.data) || is.data.frame(.data))
    stopifnot(is.numeric(threshold) && length(threshold) == 1)
    stopifnot(is.logical(require_full_year) && length(require_full_year) == 1)
    if (!tibble::has_name(.data, "mint")) {
        stop("Input data should be a column mint for daily minimum temperatures.")
    }
    # Group-wise operation
    .data |>
        dplyr::group_modify(~ {
            df <- .x

            # Check for completeness if required
            if (require_full_year) {
                .check_full_year(df)
            }

            # Main calculation
            frost_days <- .calc_number_frost_days(df$mint, threshold)
            tibble::tibble(number_frost_days = frost_days)
        })
}

.calc_number_frost_days <- function(mint, threshold = get_weather_option("extreme.frost_threshold")) {
    sum(mint < threshold, na.rm = TRUE)
}