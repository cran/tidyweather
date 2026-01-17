.check_full_year <- function(df) {
    if (!tibble::has_name(df, "day")) {
        stop("Input data should include a column 'day' for day of year.")
    }
    if (nrow(df) != 365 && nrow(df) != 366) {
        stop(
            "Data does not contain a full year. ",
            "Set require_full_year = FALSE to proceed anyway."
        )
    }
    if (min(df$day) != 1 || max(df$day) != nrow(df)) {
        stop(
            "Data does not contain a full year. ",
            "Set require_full_year = FALSE to proceed anyway."
        )
    }
}


# Detect hemisphere
.detect_hemisphere <- function(df, hemisphere) {
    if (tibble::has_name(df, "latitude")) {
        if (length(unique(df$latitude)) != 1) {
            stop("Latitude values are not consistent within a group.")
        }
        if (df$latitude[1] < 0) {
            return("south")
        } else {
            return("north")
        }
    }
    match.arg(hemisphere, c("south", "north"))
}
