#' Monte Carloe test of differences between medians
#'
#' The 'M test' tests the median differences between two groups. This test is
#' non-parametric, and uses a Monte-Carlo procedure to approximate the null
#' distribution of the statistic.
#'
#' @param x quantitative values to be tested
#'
#' @param group a factor defining groups; same length as 'x'
#'
#' @param nperm the number of permutations to be used to generate the reference
#'   distribution of the statistic
#'
#' @export
#'
#' @examples
#'
#' head(VL)
#' test <- M_test(VL$titer, VL$type, alter = "less")
#' test
#' plot(test)
#'
M_test <- function(x, group, nperm = 999, ...) {

  ## function computing the test statistic
  M <- function(x, group) {
    medians <- tapply(x, group, median)
    return(unname(medians[2] - medians[1]))
  }

  ## compute observed statistic
  obs <- M(x, group)

  ## compute permuted statistics
  sims <- replicate(nperm, M(VL$titer, sample(VL$type)))

  ## make a randtest object

  out <- ade4::as.randtest(sims, obs, ...)
  return(out)
}
