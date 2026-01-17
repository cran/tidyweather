# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
WEATHER_OPTIONS <- settings::options_manager(
    extreme = list(
        frost_threshold = 0
    ),
    require_full_year = TRUE
)


#' Set or get options for tidyweather
#'
#' This function allows users to get or set configuration options for the
#' tidyweather package.
#'
#' @param ... Option names to retrieve or key-value pairs to set.
#'
#' @details
#' The options are managed via a nested structure that distinguishes between
#' the tidyweather package.
#'
#' @section Supported options:
#' \describe{
#'   \item{\code{extreme.frost_threshold}}{ Frost threshold for extreme weather events. }
#'   \item{\code{require_full_year}}{ Whether to require a full year of data for calculations. }
#' }
#'
#' @return If called with no arguments, returns all current options.
#' If called with named arguments, updates and returns the modified options.
#'
#' @examples
#' # Get all options
#' weather_options()
#'
#' # Set frost_threshold
#' weather_options(extreme.frost_threshold = 2)
#'
#' @export
weather_options <- function(...) {
    args <- list(...)
    if (length(args) == 0) {
        return(WEATHER_OPTIONS())
    }

    if (is.null(names(args)) || any(nchar(names(args)) == 0)) {
        stop("All arguments must be named.", call. = FALSE)
    }

    for (key in names(args)) {
        value <- args[[key]]

        if (grepl("\\.", key)) {
            parts <- strsplit(key, "\\.")[[1]]
            if (length(parts) != 2) {
                stop(sprintf("Invalid option name: '%s'", key), call. = FALSE)
            }
            
            group <- parts[1]
            subkey <- parts[2]

            if (!group %in% c("extreme")) {
                stop(sprintf("Unknown group: '%s' (must be 'extreme')", group), call. = FALSE)
            }

            # Get current value to check type
            current_value <- WEATHER_OPTIONS()[[group]][[subkey]]
            .check_option_value(current_value, value)

            current <- WEATHER_OPTIONS()[[group]]
            if (is.null(current)) {
                current <- list()
            }
            current[[subkey]] <- value
            
            # Update the entire group
            update_args <- list()
            update_args[[group]] <- current
            do.call(WEATHER_OPTIONS, update_args)
        } else {
            # Handle top-level options
            if (!key %in% c("require_full_year")) {
                stop(sprintf("Unknown top-level option: '%s' (must be 'require_full_year')", key), call. = FALSE)
            }
            
            # Get current value to check type
            current_value <- WEATHER_OPTIONS()[[key]]
            .check_option_value(current_value, value)
            
            # Update the top-level option
            update_args <- list()
            update_args[[key]] <- value
            do.call(WEATHER_OPTIONS, update_args)
        }
    }
    
    invisible(WEATHER_OPTIONS())
}

#' Reset all tidyweather options to defaults
#'
#' This restores all package settings to their initial defaults as defined
#' when tidyweather was loaded. Use this if you want to undo all customizations.
#'
#' @return Invisibly returns the default settings after reset.
#' @examples
#' weather_options(extreme.frost_threshold = -2)
#' weather_reset()
#' weather_options()
#' @export
weather_reset <- function() {
    settings::reset(WEATHER_OPTIONS)
    invisible(WEATHER_OPTIONS())
}


#' Get weather package option
#'
#' Retrieves the value of a specified option from the weather package configuration.
#' This function provides access to package-level settings and preferences.
#'
#' @param name A character string specifying the name of the option to retrieve.
#'
#' @return The value of the specified option, or NULL if the option does not exist.
#'
#' @examples
#' # Get the default frost threshold
#' get_weather_option("extreme.frost_threshold")
#'
#' @export
get_weather_option <- function(name) {
    parts <- strsplit(name, "\\.")[[1]]
    if (length(parts) == 1) {
        WEATHER_OPTIONS()[[parts]]
    } else {
        WEATHER_OPTIONS()[[parts[1]]][[parts[2]]]
    }
}


.check_option_value <- function(current_value, value) {
    if (is.null(value)) {
        stop("Option value cannot be NULL", call. = FALSE)
    }
    if (class(value) != class(current_value)) {
        stop(sprintf("Option expects %s, got %s", class(current_value), class(value)), call. = FALSE)
    }
    if (length(current_value) != length(value)) {
        stop(sprintf("Option expects length %d, got length %d", length(current_value), length(value)), call. = FALSE)
    }
}
