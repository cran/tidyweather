thermaltime_validator <- function(value) {
    if (!is.list(value) || !all(c("x", "y") %in% names(value))) {
        stop("thermaltime must be a list with both x and y")
    }
    if (length(value$x) != length(value$y)) stop("thermaltime x and y must have same length")
}

#' tidyweather options
#'
#' An options manager for configuring tidyweather parameters. This object provides
#' methods to get and set weather-related parameters.
#'
#' @section Available Options:
#' \describe{
#'   \item{extreme.frost_threshold}{Frost threshold for extreme weather events. Default: 0}
#'   \item{require_full_year}{Whether to require a full year of data for certain calculations. Default: TRUE}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{get(key)}{Retrieve the value of an option by its key (e.g., "extreme.frost_threshold")}
#'   \item{set(key, value)}{Set the value of an option by its key}
#'   \item{reset()}{Reset all options to their default values}
#' }
#'
#' @export
#' @importFrom optree create_options_manager
#' @importFrom optree v_numeric_range
#' @importFrom optree v_logical_scalar
#' @examples
#' # Get default frost threshold
#' weather_options$get("extreme.frost_threshold")
#' 
#' # Set custom values
#' weather_options$set("extreme.frost_threshold" = -2)
#' 
#' # Reset to defaults
#' weather_options$reset()
weather_options <- optree::create_options_manager(
    defaults = list(
        extreme = list(
            frost_threshold = 0
        ),
        require_full_year = TRUE
    ),
    validators = list(
        "extreme.frost_threshold" = optree::v_numeric_range(-10, 10),
        "require_full_year" = optree::v_logical_scalar()
    )
)
