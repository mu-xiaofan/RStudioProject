#' Download a Report from REDCap
#'
#' This function retrieves a report from a REDCap project using API credentials and returns it as a tibble.
#' It requires the user to have set an API token in their .Renviron file under the specified name.
#'
#' @param redcapTokenName The name of the environment variable containing the REDCap API token.
#' @param redcapUrl The URL of the REDCap API endpoint.
#' @param redcapReportId The ID of the report to retrieve from REDCap.
#'
#' @return A tibble containing the data from the specified REDCap report.
#' @import httr
#' @import readr
#' @examples
#' \dontrun{
#'   result = downloadRedcapReport("MY_REDCAP_TOKEN", "https://redcap.emory.edu/api/", "46524")
#'   print(result)
#' }
#' @export
downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId) {
  #Retrieve the API token from environment variables
  token = Sys.getenv(redcapTokenName)
  #Check if the token was retrieved successfully
  if (token == "")
    {
    stop("API token is not retrieved successfully.")
    }
  #Setup the data for the POST request
  formData <- list("token" = token,
    content = 'report',
    format = 'csv',
    report_id = as.character(redcapReportId),
    csvDelimiter = '',
    rawOrLabel = 'raw',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'csv'
  )
  #Perform the POST request
  response <- httr::POST(redcapUrl, body = formData, encode = "form")
  #Check for request success
  if (response$status_code != 200)
    {
    stop("Failed to download the report: HTTP status code ", response$status_code)
    }
  #Get the content of the response
  content = httr::content(response, type = "text", encoding = "UTF-8")
  #Convert the CSV content to a tibble
  result = read_csv(content)
  return(result)
}
