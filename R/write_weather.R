file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
records <- read_weather(file)

file <- tempfile("test", fileext = ".met")
format <- "APSIM"
overwrite <- FALSE

#' Write weather data to file
#'
#' Exports weather records to a file in the specified format. Currently supports
#' APSIM format for agricultural modeling applications.
#'
#' @param records A data frame containing weather data with columns for date,
#'   temperature, precipitation, and other meteorological variables
#' @param file Character string specifying the output file path
#' @param format Character string specifying the output format. Currently 
#'   supports "APSIM" (default)
#' @param overwrite Logical indicating whether to overwrite existing files.
#'   Default is FALSE
#'
#' @return Invisibly returns the file path of the written file
#'
#' @examples
#' # Read sample weather data from package
#' file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
#' records <- read_weather(file)
#' 
#' # Write to temporary file
#' temp_file <- tempfile(fileext = ".met")
#' 
#' # Write to APSIM format
#' write_weather(records, temp_file, format = "APSIM")
#' 
#' # Overwrite existing file
#' write_weather(records, temp_file, format = "APSIM", overwrite = TRUE)
#'
#' @export
write_weather <- function(records, file, format = "APSIM", overwrite = FALSE) {
    stopifnot(is.data.frame(records))
    stopifnot(is.logical(overwrite) && length(overwrite) == 1)
    stopifnot(!is.null(file) && is.character(file) && length(file) == 1 && file != "")
    stopifnot(is.character(format) && length(format) == 1)
    format <- toupper(format)
    match.arg(format, choices = c("APSIM"))
    if (file.exists(file) && !overwrite) {
        stop(paste("File", file, "already exists. Set overwrite = TRUE to overwrite it."))
    }

    if (format == "APSIM") {
        write_apsim(records, file)
    } else {
        stop("Wrong data format!")
    }
    return(invisible(file))
}


write_apsim <- function(records, file) {  
    var_name <- c("year", "day", "radn", "maxt", "mint", "rain", 
        "evap", "vp", "rhmint", "rhmaxt", "code")
    var_unit <- c("()", "()", "(mj/m2)", "(oC)", "(oC)",
        "(mm)", "(mm)", "(hPa)", "(%)", "(%)", "()")
    required <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    var_width <- c(4, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7)
    nsmall <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    var_cols <- c("year", "day", "radn", "maxt", "mint", "rain", 
        "evap", "vp", "rhmint", "rhmaxt", "code")
    
    add_header <- function(col_name, format, required = TRUE) {
        if (!tibble::has_name(records, col_name) && required) {
            stop(paste("Input data should have column", col_name))
        } else if (!tibble::has_name(records, col_name) && !required) {
            return()
        }
        value <- unique(records[[col_name]])
        if (length(value) != 1) {
            stop(paste("Input data should have only one unique value for", 
                col_name))
        }
        res_str <<- c(res_str, sprintf(format, value))
    }
    res_str <- NULL
    add_header("number", "!station number = %s")
    add_header("name", "!station name = %s")
    add_header("latitude", "latitude = %s  (DECIMAL DEGREES)")
    add_header("longitude", "longitude = %s  (DECIMAL DEGREES)")
    add_header("tav", "tav = %s (oC) ! Annual average ambient temperature", FALSE)
    add_header("amp", "amp = %s (oC) ! Annual amplitude in mean monthly temperature", FALSE)
    
    res_str <- c(res_str, "") 

    values <- NULL
    pos <- NULL
    j <- 1
    for (j in seq(along = var_cols)) {
        if (!tibble::has_name(records, var_cols[j])) {
            if (required[j]) {
                stop(paste("Input data should have column", var_cols[j]))
            } else {
                next
            }
        }
        v <- records[[var_cols[j]]]
        values[[var_cols[j]]] <- format(v, 
                width = var_width[j], justify = "right",
                nsmall = nsmall[j])
        pos <- c(pos, j)
    }
    values <- as.data.frame(values)
    values <- apply(values, 1, FUN = function(x) {
            return(paste(x, collapse = " "))
    })

    var_name <- paste(format(var_name[pos], 
        width = var_width[pos], justify = "right",
        nsmall = nsmall[pos]), collapse = " ")
    var_unit <- paste(format(var_unit[pos], 
        width = var_width[pos], justify = "right",
        nsmall = nsmall[pos]), collapse = " ")
    res_str <- c(res_str, var_name, var_unit) 
    
    res_str <- c(res_str, values)
    writeLines(res_str, file)

}