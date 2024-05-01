#' Standardize Column Names to camelCase
#'
#' This function is a wrapper around dplyr::rename_with and janitor::make_clean_names that
#' converts the variables in a tibble data to "small_camel" case.
#'
#' @param data A tibble whose column names you want to standardize.
#' @return A tibble with standardized column names in camelCase.
#' @import dplyr
#' @import janitor
#' @import snakecase
#' @export
#' @examples
#' #Creating a sample tibble
#' sample_data = tibble(`first column` = 1:5, `second column` = 6:10)
#' #Standardizing names
#' new_data = standardizeNames(sample_data)
#' print(new_data)

standardizeNames <- function(data) {
  #Ensure the input is a tibble
  if (!inherits(data, "tbl_df")) {
    stop("The provided data must be a tibble.")
  }
  #Rename columns to clean names and then convert to camelCase
  result = data %>%
    rename_with(.fn = function(names) {
      #Clean the names
      cleaned_names = make_clean_names(names)
      #Convert to camelCase
      to_any_case(cleaned_names, case = "small_camel")
    })
  return(result)
}
