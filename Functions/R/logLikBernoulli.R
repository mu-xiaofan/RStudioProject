#' Calculates the parameter p that maximizes the log-likelihood
#'
#' This function takes a vector and calculates the parameter p that maximizes the log-likelihood
#' and it uses a grid-based search approach, with p in steps of 0.001.
#'
#' @param data A numeric vector containing binary outcomes (0s and 1s).
#' @return The value of p that maximizes the log-likelihood.
#' @export
#' @examples
#' data = c(1, 0, 0, 0, 1, 1, 1)
#' logLikBernoulli(data)
#'
logLikBernoulli = function(data) {
  p_grid = seq(0, 1, by = 0.001)
  log_likelihoods = sapply(p_grid, function(p)
    {
    sum(dbinom(data, size = 1, prob = p, log = TRUE))
    })
  optimal_p = p_grid[which.max(log_likelihoods)]
  return(optimal_p)
}

