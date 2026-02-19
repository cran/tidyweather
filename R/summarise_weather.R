#' Summarise Weather Extremes and Key Indicators
#'
#' This function calculates summary metrics for weather data, including the number
#' of frost days and the last frost day, grouped by one or more grouping variables.
#' The function uses package-wide options for thresholds and year completeness.
#'
#' @param .data A tibble or data frame containing daily weather data. Must include
#'   at least a `mint` column for daily minimum temperatures. A `day` column is
#'   recommended if `require_full_year = TRUE`.
#'
#' @return A tibble with one row per group, containing the following columns:
#'   \describe{
#'     \item{number_frost_days}{Number of days where minimum temperature is below
#'         the frost threshold.}
#'     \item{last_frost_day}{The day of year of the last frost (or `NA` if none).}
#'   }
#'
#' @details
#' The function retrieves thresholds and settings from the global tidyweather
#' options via `weather_options$get()`. The default frost threshold is
#' `weather_options$get("extreme.frost_threshold")` and `require_full_year` is
#' `weather_options$get("require_full_year")`. These can be changed using
#' `weather_options$set()`.
#'
#' This function is designed to work with grouped tibbles (e.g., after
#' `dplyr::group_by()`), applying the summary per group.
#'
#' @examples
#' library(dplyr)
#'
#' # Example weather data (daily minimum temperatures)
#' weather_data <- read_weather(system.file("extdata/ppd_72150.met", package = "tidyweather"))
#'
#' # Summarise without grouping
#' weather_options$set("require_full_year" = FALSE)
#' summarise_weather(weather_data)
#'
#' # Summarise by group (e.g., year)
#' weather_data_grouped <- weather_data %>% group_by(year)
#' summarise_weather(weather_data_grouped)
#'
#' @export
summarise_weather <- function(.data) {
    stopifnot(tibble::is_tibble(.data) || is.data.frame(.data))
    require_full_year <- weather_options$get("require_full_year")
    .data |>
        dplyr::group_modify(~ {
            df <- .x
            if (require_full_year) .check_full_year(df)
            if (!tibble::has_name(df, "latitude")) {
                stop("Input data should have a 'latitude' column to determine hemisphere", call. = FALSE)
            }
            hemisphere <- .detect_hemisphere(df, hemisphere)
            res <- tibble::tibble(
                number_frost_days = .calc_number_frost_days(df$mint),
                last_frost_day = .calc_last_frost_day(df$mint, hemisphere)
            )

            res
        })
}
