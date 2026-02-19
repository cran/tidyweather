# Transfer of sign - from FORTRAN.
# The result is of the same type and kind as a. Its value is the abs(a) of a,
# if b is greater than or equal positive zero; and -abs(a), if b is less than
# or equal to negative zero.
# Example a = sign_apsim (30,-2) ! a is assigned the value -30
# 
# @param a value 1
# @param b value 2
sign_apsim <- function( a, b ) {
    if ( b >= 0 )
    {
        return( abs( a ) )
    } else
    {
        return( -abs(a) )
    }
}

# Some utility functions for weather analysis
#' Significantly t-test with auto-correlation for time serial data 
#' 
#' Method is presented by Santer et al. 2000
#' @param y A vector of time serial data
#' @param slope Whether export slope
#' @return p values of t-test
#' @export
ttest_ts <- function(y, slope = NULL) {
    if(sum(is.na(y)) == 0) 
    {
        y <- as.numeric(y)
        num <- length(y)
        x <- seq(along = y)
        if (is.null(slope))
        {
            slope <- stats::cor(x, y) * stats::sd(y)/stats::sd(x)
        }
        sb_m <- sqrt(sum((x - mean(x)) ^ 2))
        inercept <- (sum(y) - slope * sum(x)) / num
        et_x <- y - (inercept + slope * x)
        ne_x <- stats::cor(et_x[-1], et_x[-(num)])
        ne_x <- num * (1 - ne_x) / (1 + ne_x)
        se_x <- sqrt((1 / (ne_x - 2)) * sum(et_x * et_x, na.rm = TRUE))
        sb_x <- se_x / sb_m
        tb_x <- abs(slope / sb_x)
        p_x <- (1 - stats::pt(tb_x, df = ne_x - 2)) * 2
        return (p_x)
    } else 
    {
        return (NA)
    }
}


#' The time elapsed in hours between the specified sun angle 
#' from 90 degree in am and pm. +ve above the horizon, -ve below the horizon.
#' @param  doy day of year number    
#' @param lat latitude of site (deg) 
#' @param  angle angle to measure time between, such as twilight (deg).
#' angular distance between 90 deg and end of twilight - altitude of sun. +ve up, -ve down.
#' @return day length in hours
#' @export
day_length <- function( doy, lat, angle = -6 ) {
    # Constant Values
    aeqnox <- 82.25
    dg2rdn <- ( 2.0 * pi ) / 360.0
    decsol <- 23.45116 * dg2rdn
    dy2rdn <- ( 2.0 * pi ) / 365.25
    rdn2hr <- 24.0 / ( 2.0 *pi )
    
    sun_alt <- angle * dg2rdn;
    dec <- decsol * sin( dy2rdn * ( doy - aeqnox ) )
    
    
    if ( ( abs( lat ) == 90.0 ) )
    {
        coshra <- rep( sign_apsim( 1.0, -dec) * sign_apsim( 1.0, lat ), 
                times = length( doy ) )
    } else 
    {
        latrn <- lat * dg2rdn
        slsd <- sin( latrn ) * sin( dec )
        clcd <- cos( latrn ) * cos( dec )
        
        altmn <- asin( min( max( slsd - clcd, -1.0 ), 1.0 ) )
        altmx <- asin( min( max( slsd + clcd, -1.0 ), 1.0 ) )
        alt <- min( max( sun_alt, altmn ), altmx )
        
        coshra <- (sin( alt ) - slsd ) / clcd
        coshra[coshra < -1]  <- -1
        coshra[coshra > 1]  <- 1
    }
    
    hrangl <- acos( coshra )
    hrlt <- hrangl * rdn2hr * 2.0
    return( hrlt )
}


#'Return a y value from a linear interpolation function
#'
#' @param x x
#' @param y y
#' @param values values
#' @param split split
#' @return The interpolated values
#' @export
interpolation_function <- function( x, y, values, split = '\\s+' ) {
    if (is.character(x) & length(x) == 1)
    {
        x <- as.numeric(strsplit(x, split)[[1]])
    }
    if (is.character(y) & length(y) == 1)
    {
        y <- as.numeric(strsplit(y, split)[[1]])
    }
    res <- rep(NA, length(values))

    pos <- values < x[1]
    res[pos] <- y[1]

    for (i in seq(length = length(x) - 1))
    {
        pos <- values >= x[i] & values < x[i + 1]
        slope <- (y[i+1] - y[i] ) / (x[i+1] - x[i])
        res[pos] <- y[i] + slope * (values[pos] - x[i])
    }
    pos <- values >= x[length(x)]
    res[pos] <- y[length(y)]
    return ( res )
}


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
