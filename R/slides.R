#' Check existence of presentation slide
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
get_slides_properties <- function(id = NULL){
  # Check that id parameter is a character, if not throw an error
  if(!is.character(id)){
    stop("id is not a character.")
  }
  url <- paste0(getOption("slides.endpoint"), id)
  token <- get_token()
  config <- httr::config(token=token)
  result <- httr::GET(url, config = config, accept_json())
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}
