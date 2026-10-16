#' Wang-Engel temperature response function
#'
#' Calculate the shape of the Wang and Engel beta temperature response
#' function defined by three cardinal temperatures.
#'
#' @param min_temp The minimum cardinal temperature.
#' @param opt_temp The optimum cardinal temperature.
#' @param max_temp The maximum cardinal temperature.
#' @param beta The exponent applied to the relative response (0-1). Default is 1.
#' @param n The number of evenly spaced temperatures between `min_temp` and
#'   `max_temp` at which the response is evaluated. Default is 101.
#' @return A data frame with columns `x` and `y` for the temperature and response, respectively.
#' @references
#' Wang E, Engel T (1998). Simulation of phenological development of wheat
#' crops. *Agricultural Systems* 58, 1-24.
#' @export
#' @examples
#' library(ggplot2)
#' tt <- wang_engel_tt(0, 25, 35)
#' tt |>
#'     ggplot() +
#'     geom_line(aes(x = x, y = y)) +
#'     labs(x = "Temperature", y = "Response") +
#'     geom_vline(xintercept = c(0, 25, 35), linetype = "dashed", color = "red") +
#'     geom_hline(yintercept = c(0, 25), linetype = "dashed", color = "blue") +
#'     theme_bw()
#' # Change the beta and number of points
#' tt <- wang_engel_tt(0, 25, 35, beta = 0.5, n = 36)
wang_engel_tt <- function(min_temp, opt_temp, max_temp,
                            beta = 1, n = 101) {
    is_single <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
    if (!all(vapply(list(min_temp, opt_temp, max_temp, beta, n), is_single, logical(1)))) {
        stop("min_temp, opt_temp, max_temp, beta and n must be single numeric values.")
    }
    if (!(min_temp < opt_temp && opt_temp < max_temp)) {
        stop("Cardinal temperatures must satisfy min_temp < opt_temp < max_temp.")
    }
    if (beta < 0 || beta > 1) {
        stop("beta must be between 0 and 1.")
    }
    if (n < 2) {
        stop("n must be at least 2.")
    }

    p <- log(2) / log((max_temp - min_temp) / (opt_temp - min_temp))
    rel_eff <- function(t) {
        d <- t - min_temp
        o <- opt_temp - min_temp
        (2 * d ^ p * o ^ p - d ^ (2 * p)) / o ^ (2 * p)
    }

    ref_eff <- 1

    temp <- seq(min_temp, max_temp, length.out = n)
    res <- data.frame(x = temp, y = (rel_eff(temp) / ref_eff) ^ beta)
    res$y <- res$y * (opt_temp - min_temp)
    return(res)
}
