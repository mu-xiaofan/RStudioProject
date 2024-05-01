#' Calculate minimum sample size for T-Tests,and smallest sample size can be returned was set to 2
#'
#' This function is a wrapper around pwr::pwr.t2n.test that takes either one ( x1 )
#' or two ( x2 ) samples of preliminary data and returns the minimum sample size needed
#' for a t-test of the null hypotheses that either μ_x1==0 or μ_x1==μ_x2 with 80% power at alpha=0.05.
#' Note: if a very small sample size is required as the effect size is very large and pwr calculation
#' fails, we will just return 2 (This is also the minimum sample size set in this function).
#'
#' @param x1 A numeric vector of preliminary data for the first sample.
#' @param x2 An optional numeric vector of preliminary data for the second sample.
#'
#' @return The minimum sample size needed for each group to achieve the desired power.
#' @import pwr
#' @examples
#' x1 = c(0.5, 0.1, -0.2, 0.3, 0.1, -0.1, 0, 0.2, 0.3, -0.4)
#' x2 = c(5, 5, 5, 5, 5)
#' minimumN(x1)#One-sample
#' minimumN(x1, x2)#Two-sample
#' @export
minimumN = function(x1, x2 = NULL) {
  if (!is.null(x2)) {
    if (all(x1 == x1[1]) && all(x2 == x2[1]))
      {
      if (x1[1] != x2[1])
        {
        #Set 2 to the default smallest sample size because the effect size is extremely large
        return(2)
      }
      else
        {
        return("Samples are identical or have zero variance; an infinite sample size is implied for any meaningful test.")
      }
    }
    else
      {
      n1 = length(x1)
      n2 = length(x2)
      #d = abs(mean(x1) - mean(x2)) / sqrt((sd(x1)^2 + sd(x2)^2) / 2)
      d = abs(mean(x1) - mean(x2)) / (sqrt(((n1 - 1) * (sd(x1))^2 + (n2 - 1) * (sd(x2))^2) / (n1 + n2 - 2)))
    }
  }
  else
    {
    if (all(x1 == x1[1]))
      {
      return("Sample has zero variance or all values are equal: An infinite sample size is implied for any meaningful test.")
    }
    d = abs(mean(x1) / sd(x1))
  }
  # Add a check for practicality of d values
  if (is.finite(d) && d > 0) {
    tryCatch({
      result = pwr.t.test(d = d, power = 0.80, sig.level = 0.05, type = if (is.null(x2)) "one.sample" else "two.sample", alternative = "two.sided")
      return(max(ceiling(result$n), 2))#Ensure at least a sample size of 2
    }, error = function(e) {
      return("Failed to compute the sample size due to extreme d value: likely too large or too small for practical testing.")
    })
  } else {
    return("Effect size is zero or infinite: No practical sample size can be computed.")
  }
}
