#' Fit von Bertalanffy curve
#' 
#' Fit a von Bertalanffy growth curve to fish age and length of data to generate
#' posterior samples of age-length parameters to feed into an operating model
#' simulation.
#' 
#' @param age A numeric vector of ages
#' @param length A numeric vector of lengths
#' @param ... Other arguments to pass to \code{\link[rstan]{sampling}}.
#' @importFrom rstan sampling
#' @import Rcpp methods
#' @seealso \code{\link{get_age_length}}
#' @export
#' 
#' @examples
#' m <- fit_vb(short_age_length$age, short_age_length$length)
#' m

fit_vb <- function(age, length, ...) {
  sampling_args <- list(
    object = stanmodels$vb,
    data = list(
      N = length(age), 
      age = age, 
      length = length),
    ...)
  
  do.call(sampling, sampling_args)
}
