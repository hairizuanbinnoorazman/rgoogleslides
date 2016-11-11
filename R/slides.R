#' Check existence of presentation slide
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
slides_exists <- function(id = NULL){
  endpoint <- "https://slides.googleapis.com/v1/presentations/"
  url <- paste0(endpoint, id)
  token <- get_token()
  config <- httr::config(token=token)
  result <- httr::GET(url, config = config, accept_json())
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}
