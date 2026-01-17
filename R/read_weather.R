#' Read weather records from a file list and/or a folder list
#' 
#' @param file A character string to specify weather filename.
#' @param format A character string to specify the format of weather file.
#' @param ... Other arguments
#' @return A data.frame which contains all weather data.
#' @examples
#' file <- system.file("extdata/ppd_72150.met", package = "tidyweather")
#' records <- read_weather(file)
#' head(records)
#' @export
read_weather <- function(file, format = "APSIM", ...) {
    stopifnot(is.character(file) && length(file) == 1 && file.exists(file))
    stopifnot(is.character(format) && length(format) == 1)
    format <- toupper(format)
    match.arg(format, choices = c("APSIM"))
    record <- NULL
    if (format == "APSIM") {
        record <- read_apsim(file)
    } else {
        stop("Wrong data format!")
    }

    if (is.null(record)) {
        stop("No weather records are found.")
    }
    record <- tibble::tibble(record)
    return(record)
}


# Read weather records from a weather data file with APSIM format
# 
# @param filename The file name of weather data file.
# @return A data.frame which contains all weather data.
read_apsim <- function(filename) {    
    a <- NULL
    station.number <- as.character(NA)
    station.name <- as.character(NA)
    latitude <- as.numeric(NA)
    longitude <- as.numeric(NA)
    temp <- readLines(filename, n = 100)
    
    sta.num.str <- temp[grep("!station number", temp)]
    
    if (length((sta.num.str)) > 0) {
        station.number <- stringr::str_trim(substr(sta.num.str, 19, 1000))
    } 
    
    sta.name.str <- temp[grep("!station name", temp)]
    if (length((sta.name.str)) > 0) {
        station.name <- stringr::str_trim(substr(sta.name.str, 16, 1000))
    } 
    lat.str <- temp[grep("latitude", tolower(temp))]
    if (length((lat.str)) > 0) {
        latitude <- gsub("^latitude( |\t)*=( |\t)*(-?\\d*\\.{0,1}\\d*).*$", "\\3", tolower(lat.str))
        latitude <- as.numeric(latitude)
    }
    
    lon.str <- temp[grep("longitude", tolower(temp))]
    if (length((lon.str)) > 0) {    
        longitude <- gsub("^longitude( |\t)*=( |\t)*(-?\\d*\\.{0,1}\\d*).*$", "\\3", tolower(lon.str))
        longitude <- as.numeric(longitude)
    } 
    
    tav.str <- temp[grep("^tav", temp)]
    tav <- -999
    if (length((tav.str)) > 0) {    
        tav <- as.numeric(
            gsub("(^tav += +)(\\d+\\.?\\d*)( .*$)", "\\2", tav.str))
    } 
    
    amp.str <- temp[grep("^amp", temp)]
    amp <- -999
    if (length((amp.str)) > 0) {    
        amp <- as.numeric(
            gsub("(^amp += +)(\\d+\\.?\\d*)( .*$)", "\\2", amp.str))
    }

    # for year
    start.line <- grep("^.*(year|Year|date|Date)", temp)
    if (length(start.line)  == 0) { 
        stop("Keywords year or date ae not found.")
    }
    
    a <- utils::read.table(filename, head = FALSE, sep = "", skip = start.line + 1,
        col.names = scan(filename, "", sep = "", skip = start.line - 1, nlines = 1,
            quiet = TRUE),
        as.is = TRUE)
    names(a) <- tolower(names(a))
    # Convert date
    if (!is.null(a$date)) {
        date_format <- scan(filename, "", sep = "", skip = start.line, nlines = 1,
                            quiet = TRUE)
        date_format <- date_format[which(names(a) == "date")]
        if (nchar(date_format) == 0) {
            stop("Date format is not found")
        }
        date_format <- gsub("(\\(|\\))", "", date_format)
        date_format <- "%d/%m/%Y"
        a$date <- as.Date(a$date, format = date_format)
        if (sum(is.na(a$date)) > 0) {
            stop("NA values are found for date columns.")
        }
        a$year <- format(a$date, "%Y")
        a$day <- format(a$date, "%j")
    } else {
        if (is.null(a$year) || is.null(a$day)) {
            stop("Either date or year and day columns are required.")
        }
        a$date <- as.Date(paste0(a$year, "-01-01")) + as.numeric(a$day) - 1
    }
    a$year <- as.numeric(a$year)
    a$day <- as.numeric(a$day)
    
    if (!is.null(a$pan)) {
        a$evap <- a$pan
    }
    extra <- NULL
    a$maxt <- as.numeric(a$maxt)
    a$mint <- as.numeric(a$mint)
    a$radn <- as.numeric(a$radn)
    a$rain <- as.numeric(a$rain)
    if (!is.null(a$evap)) {
        a$evap <- as.numeric(a$evap)
    }
    if (is.null(a$avgt)) {
        a$avgt <- (a$maxt + a$mint) / 2
    } else {
        a$avgt <- a$avgt
    }
    # if (is.null(a$vpd)) {
    #     a$vpd <- vpd.apsim(a$maxt, a$mint)
    # } else {
    #     a$vpd <- a$vpd
    # }
    if (!is.null(a$rhmint)) {
        a$rhmint <- a$rhmint
    }
    if (!is.null(a$rhmaxt)) {
        a$rhmaxt <- a$rhmaxt
    }

    a$name   <-  station.name
    a$number  <-  station.number
    a$latitude  <-  latitude
    a$longitude  <-  longitude
    a$tav <- tav
    a$amp <- amp
    return(a)
}
