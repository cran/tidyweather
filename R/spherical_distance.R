#' Calculate the sphere distance between two points on the Earth
#'
#' @param lat1 Latitude of the first point in degrees. Numeric scalar (-90 to 90)
#' @param lon1 Longitude of the first point in degrees. Numeric scalar (-180 to 180)
#' @param lat2 Latitudes of the second point in degrees. Numeric vector (-90 to 90)
#' @param lon2 Longitudes of the second point in degrees. Numeric vector (-180 to 180)
#' @return Distance in km
#' @examples
#' spherical_distance(34.05, -118.25, 40.7128, -74.0060) # Distance between Los Angeles and New York
#' @export
spherical_distance <- function(lat1, lon1, lat2, lon2) {
    stopifnot(is.numeric(lat1), is.numeric(lon1), is.numeric(lat2), is.numeric(lon2))
    stopifnot(length(lat1) == length(lon1), length(lat2) == length(lon2))
    stopifnot(length(lat1) == 1, length(lon1) == 1)
    stopifnot(all(lat1 >= -90 & lat1 <= 90), all(lat2 >= -90 & lat2 <= 90))
    stopifnot(all(lon1 >= -180 & lon1 <= 180), all(lon2 >= -180 & lon2 <= 180))

    lon1 <- lon1 * pi /180
    lat1 <- lat1 * pi /180
    lon2 <- lon2 * pi /180
    lat2 <- lat2 * pi /180
    dLat <- lat2 - lat1
    dLon <- lon1 - lon2

    a <- sin(dLat/2) * sin(dLat/2) +
            cos(lat1) * cos(lat2) * 
            sin(dLon/2) * sin(dLon/2)
    c <- 2 * atan2(sqrt(a), sqrt(1-a)) 
    d <- 6371 * c
    return( d )
}
