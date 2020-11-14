#' Generate n observations from a mixture of two normal distributions
#'
#' This function allows you to Generate n observations from a mixture of two normal distributions
#' @param p1 probability
#' @keywords Bernoulli
#' @export
#' @examples
#' MixtureNormalDistribution(0.1, 5, 4, 1, 2)

MixtureNormalDistribution <- function(p1, mu1, mu2, sigma1, sigma2) {
  U <- rbern(1, p = p1)
  ifelse(U == 1, rnorm(1, mean = mu1, sd = sigma1), rnorm(1, mean = mu2, sd = sigma2))
}





