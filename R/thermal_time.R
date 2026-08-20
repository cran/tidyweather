#' Calculate thermal time using cardinal temperatures
#'
#' @param maxt The maximum temperature
#' @param mint The minimum temperature
#' @param x_temp The cardinal temperatures 
#' @param y_temp The effective thermal time
#' @param method The method to calculate thermal time. 
#' The default method is ( maxt + mint ) / 2 - base. 
#' The three hour temperature methods will be usesd if method = '3hr'.
#' The APSIM HourlySinPpAdjusted method can be selected with
#' method = 'HourlySinPpAdjusted'.
#' @param ... Additional arguments passed to the method-specific functions.
#'   Required arguments for method = 'HourlySinPpAdjusted' are, lat and date.
#' @return The thermal time.
#' @export
#' @examples 
#' mint <- c(0, 10)
#' maxt <- c(30, 40)
#' x_temp <- c(0, 20, 35)
#' y_temp <- c(0, 20, 0)
#' thermal_time(mint, maxt, x_temp, y_temp)
#' thermal_time(mint, maxt, x_temp, y_temp, method = '3hr')
#' date <- as.Date(c("2020-01-01", "2020-01-02"))
#' thermal_time(mint, maxt, x_temp, y_temp, method = 'HourlySinPpAdjusted',
#'              lat = -27.5, date = date)
thermal_time <- function(mint, maxt, x_temp, y_temp,
                         method = NULL, ...)
{
    if (!is.numeric(maxt) | !is.numeric(mint)) {
        stop("Numeric vector is required for mint and maxt.")
    }
    if (length(mint) != length(maxt)) {
        stop("mint and maxt require the same length.")
    }
    if (!is.numeric(x_temp) | !is.numeric(y_temp)) {
        stop("Numeric vector is required for x_temp and y_temp.")
    }
    if (length(x_temp) != length(y_temp)) {
        stop("x_temp and y_temp require the same length.")
    }
    if (!is.null(method) && !method %in% c("3hr", "HourlySinPpAdjusted")) {
        stop("Method should be either NULL, '3hr' or 'HourlySinPpAdjusted'.")
    }
    stopifnot(sum(is.na(mint)) == 0, sum(is.na(maxt)) == 0)
    stopifnot(sum(is.na(x_temp)) == 0, sum(is.na(y_temp)) == 0)
    stopifnot(all(mint <= maxt))

    if (is.null(method)) {
        meant <- (maxt + mint) / 2
        tt <- interpolation_function(x = x_temp, y = y_temp, values = meant)
        return (tt)
    } else if (method == "3hr") {
        pos <- mint > maxt
        if (sum(pos) > 0) {
            stop("Minimum temperature is more than maximum temperature.")
        }
        temp <- interpolate_3hr(mint = mint, maxt = maxt)
        tt <- matrix(interpolation_function(x = x_temp, y = y_temp, values = temp), ncol = 8)
        res <- apply(tt, 1, mean)
        return(res)
    } else if (method == "HourlySinPpAdjusted") {
        args <- list(...)
        if (is.null(args$lat) || is.null(args$date)) {
            stop("lat and date are required when method = 'HourlySinPpAdjusted'.")
        }
        temp <- interpolate_hourly_sin_pp_adjusted(mint = mint, maxt = maxt,
                                                    lat = args$lat, date = args$date)
        return(interpolation_function(x = x_temp, y = y_temp, values = temp))
    } else {
        stop("Not implemented for method ", method)
    }
}


#' Interpolate Hourly Temperature Values using APSIM HourlySinPpAdjusted.
#'
#' Interpolates hourly temperatures from daily minimum and maximum temperatures
#' using the APSIM HourlySinPpAdjusted method. Daytime temperatures are
#' sinusoidal and nighttime temperatures follow an exponential cooling curve.
#' Day length, sunrise and sunset are calculated from \code{lat} and \code{date}.
#'
#' @param mint A numeric vector of daily minimum temperatures.
#' @param maxt A numeric vector of daily maximum temperatures.
#' @param date A \code{Date} vector with the same length as \code{mint}, used to
#'   determine the day of year for day length calculations.
#' @param lat Latitude of the site (deg) as a single numeric value.
#' @param hourly A logical value indicating whether to return hourly temperatures. If \code{FALSE}, daily mean temperatures are returned.
#'
#' @return A numeric vector of daily mean temperatures, with one value per
#'   input day if \code{hourly = FALSE}. If \code{hourly = TRUE}, a data frame
#'   is returned with columns \code{date}, \code{mint}, \code{maxt}, \code{hour}, and \code{temp}.
#'
#' @examples
#' mint <- c(10, 11, 12)
#' maxt <- c(30, 31, 32)
#' date <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
#' interpolate_hourly_sin_pp_adjusted(mint, maxt, date = date, lat = -27.5)
#' 
#' hourly <- interpolate_hourly_sin_pp_adjusted(mint, maxt, date = date, lat = -27.5, hourly = TRUE)
#' library(ggplot2)
#' hourly |>
#'     ggplot(aes(x = timestamp, y = temp)) +
#'     geom_line() +
#'     geom_point(aes(x = timestamp, y = mint), color = "blue") +
#'     geom_point(aes(x = timestamp, y = maxt), color = "red") +
#'     theme_bw() +
#'     labs(x = "Date Time", y = "Temperature (°C)", title = "Hourly Temperature Interpolation")
#'
#' @export
interpolate_hourly_sin_pp_adjusted <- function(
    mint,
    maxt,
    date,
    lat,
    hourly = FALSE
) {
    stopifnot(is.numeric(mint), is.numeric(maxt), length(mint) == length(maxt))
    stopifnot(sum(is.na(mint)) == 0, sum(is.na(maxt)) == 0)
    stopifnot(all(mint <= maxt))
    stopifnot(is.numeric(lat), length(lat) == 1)
    stopifnot(is.logical(hourly), length(hourly) == 1)
    
    nday <- length(mint)
    stopifnot(inherits(date, "Date"), length(date) == nday)
    stopifnot(sum(is.na(date)) == 0)

    lat <- rep(lat, nday)

    doy <- as.numeric(format(date, "%j"))
    daylength <- mapply(day_length, doy, lat)
    sunrise <- 12 - daylength / 2
    sunset <- sunrise + daylength

    p <- 1.5
    tc <- 4.0
    hours <- 0:23
    out <- matrix(NA_real_, nrow = nday, ncol = 24)

    for (i in seq_len(nday)) {
        tmin <- mint[i]
        tmax <- maxt[i]
        tmax_b <- if (i == 1) tmax else maxt[i - 1]
        tmin_a <- if (i == nday) tmin else mint[i + 1]
        d <- daylength[i]
        hsrise <- sunrise[i]
        hsset <- sunset[i]

        for (h_i in seq_along(hours)) {
            th <- hours[h_i]

            if (th < hsrise) {
                n <- 24 - d
                tsset <- tmin + (tmax_b - tmin) * sin(pi * (d / (d + 2 * p)))
                ta <- (tmin - tsset * exp(-n / tc) +
                    (tsset - tmin) * exp(-(th + 24 - hsset) / tc)) /
                    (1 - exp(-n / tc))
            } else if (th >= hsrise && th < 12 + p) {
                ta <- tmin + (tmax - tmin) *
                    sin(pi * (th - hsrise) / (d + 2 * p))
            } else if (th >= 12 + p && th < hsset) {
                ta <- tmin_a + (tmax - tmin_a) *
                    sin(pi * (th - hsrise) / (d + 2 * p))
            } else {
                tsset <- tmin_a + (tmax - tmin_a) * sin(pi * (d / (d + 2 * p)))
                n <- 24 - d
                ta <- (tmin_a - tsset * exp(-n / tc) +
                    (tsset - tmin_a) * exp(-(th - hsset) / tc)) /
                    (1 - exp(-n / tc))
            }

            out[i, h_i] <- ta
        }
    }
    if (hourly) {
        res <- data.frame(
            date = rep(date, each = length(hours)),
            mint = rep(mint, each = length(hours)),
            maxt = rep(maxt, each = length(hours)),
            hour = hours, 
            temp = as.vector(t(out))
        ) 
        res$timestamp <- as.POSIXct(paste(res$date, sprintf("%02d:00:00", res$hour)))
        return(res)
    }
    return(rowMeans(out))
}

#' Interpolate 3-Hourly Temperature Values using sine curve.
#'
#' Interpolates temperature values at 3-hourly intervals from daily minimum
#' and maximum temperatures using a sine curve.
#'
#' @param mint A numeric vector of daily minimum temperatures.
#' @param maxt A numeric vector of daily maximum temperatures.
#'
#' @return A numeric matrix of interpolated 3-hourly temperature values. Rows correspond to input minimum and maximum temperatures and columns correspond to the eight interpolated 3-hourly intervals.
#'
#' @examples
#' mint <- c(0, 10)
#' maxt <- c(30, 40)
#' interpolate_3hr(mint = mint, maxt = maxt)
#'
#' @export
interpolate_3hr <- function(mint, maxt) {
    stopifnot(is.numeric(mint), is.numeric(maxt), length(mint) == length(maxt))
    stopifnot(sum(is.na(mint)) == 0, sum(is.na(maxt)) == 0)
    stopifnot(all(mint <= maxt))
    hour <- seq(1, 8)
    frac <- 0.92105 + 0.1140 * hour - 0.0703 * 
        hour * hour + 0.0053 * hour * hour * hour
    mint2 <- matrix(rep(mint, times = 8), ncol = 8)
    maxt2 <- matrix(rep(maxt, times = 8), ncol = 8)
    frac2 <- matrix(rep(frac, each = length(mint)), ncol = 8)
    temp <- mint2 + (maxt2 - mint2) * frac2
    return(temp)
}
