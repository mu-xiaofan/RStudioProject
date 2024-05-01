#' Unscale vectors that has been passed through scale()
#'
#' This function takes a vector that has been put through scale
#' and reverses the centering/scaling, if any.
#'
#' @param x A numeric vector that has been scaled.
#' @return A numeric vector with the scaling reversed.
#'
#' @export
#' @examples
#' original_vector = c(1, 2, 3, 4, 5)
#' scaled_vector = scale(original_vector)
#' unscaled_vector = unscale(scaled_vector)

unscale = function(x) {
  #Check if the vector x has the necessary attributes(center and scale)
  center = attr(x, "scaled:center")
  scale = attr(x, "scaled:scale")
  #Check if centering and scaling were applied
  if (!is.null(center) && !is.null(scale))
    {
    #Reverse the scaling and centering
    x = x * scale + center
  }
  else
    {
      #Handle cases where x was not scaled or missing attributes
      warning("The input vector doesn't not have scaling attributes and may not be scaled.")
    }
  if (is.matrix(x) && ncol(x) == 1)
    {
    #reformatting
    x = c(x)
    }
  return(x)
}

