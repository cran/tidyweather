#' Calculate the last frost day
#'
#' This function calculates the last frost day from a numeric vector of daily
#' minimum temperatures using tidyverse principles.
#'
#' @param .data A data frame or tibble containing daily minimum temperatures in a column named "mint".
#' @param threshold The stress temperature threshold for frost (default: 0)
#' @param hemisphere Hemisphere indicator: "south" or "north" (default: "south").
#'      If latitude information is available in the data, it will be used to determine the hemisphere.
#' @param require_full_year Logical. If TRUE, requires exactly 365 or 366 days (default: TRUE)
#'
#' @return An data.frame or tibble representing the day of year for the last frost, or NA if no frost occurs
#' @export
#'
#' @examples
#' file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
#' records <- read_weather(file)
#' records |>
#'     dplyr::group_by(year) |>
#'     last_frost_day(require_full_year = FALSE)
last_frost_day <- function(.data,
                           threshold = get_weather_option("extreme.frost_threshold"),
                           hemisphere = "south",
                           require_full_year = get_weather_option("require_full_year")) {
    stopifnot(tibble::is_tibble(.data) || is.data.frame(.data))
    stopifnot(is.numeric(threshold) && length(threshold) == 1)
    stopifnot(is.logical(require_full_year) && length(require_full_year) == 1)
    stopifnot(is.character(hemisphere) && length(hemisphere) == 1)
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

            hemisphere <- .detect_hemisphere(df, hemisphere)
            if (hemisphere == "north") {
                stop("Northern hemisphere calculations not yet implemented")
            }
            res <- .calc_last_frost_day(df$mint, hemisphere, threshold)
            tibble::tibble(last_frost_day = res)
        })
}

.calc_last_frost_day <- function(mint, hemisphere, threshold = get_weather_option("extreme.frost_threshold")) {
    stopifnot(hemisphere == c("south"))
    mint_rev <- rev(mint)
    mint_rev[mint_rev < threshold] <- -99999
    min_pos <- which.min(mint_rev)
    if (min(mint_rev) > -99999) {
        return(NA_integer_)
    }
    length(mint_rev) + 1 - min_pos
}
