#' Calculate the approximation to a data x based on PCs
#'
#' This function returns an approximation to the data x based on
#' npc PCs. The function also reconstructs the data, ensuring that it is
#' rescaled and centered to match the original dataset.
#'
#' @param x A numeric matrix of data to be approximated.
#' @param npc The number of principal components to use in the approximation.
#' @return A numeric matrix approximated using the specified number of principal components.
#' @export
#' @examples
#' set.seed(123)
#' data_matrix = matrix(rnorm(100), nrow=10)
#' approx_data = pcApprox(data_matrix, npc=2)
#' print(approx_data)
pcApprox = function(x, npc) {
  #Check if npc is within a valid range
  if (npc < 1 || npc > ncol(x))
    {
    stop("Number of principal components must be between 1 and the number of columns in x")
    }
  #Perform PCA on the data
  #pca = prcomp(x, center = TRUE, scale. = TRUE)
  pca = prcomp(x, scale. = FALSE)
  #Project the data onto the first 'npc' principal components
  projection = pca$x[, 1:npc]
  #Reconstruct the data using the selected principal components
  reconstruction = projection %*% t(pca$rotation[, 1:npc])
  #reconstructed_data = sweep(reconstruction, 2, -pca$center, "+")
  #if (!is.null(pca$scale)) {
    #reconstructed_data = sweep(reconstructed_data, 2, pca$scale, "*")
  #}
  reconstructed_data = scale(reconstruction,center =-pca$center,scale=FALSE)
  return(reconstructed_data)
}
