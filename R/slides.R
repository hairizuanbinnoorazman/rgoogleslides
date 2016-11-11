#' Check existence of presentation slide
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
get_slides_properties <- function(id = NULL){
  # Get endpoint url
  url <- get_endpoint("slides.endpoint.get", id)
  token <- get_token()
  config <- httr::config(token=token)
  result <- httr::GET(url, config = config, accept_json())
  if(httr::status_code(result) != 200){
    stop("ID provided does not point towards any slide")
  }
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}

#' Replace all text within the slide
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
replace_all_text <- function(id=NULL, replaceText=NULL, containText=NULL, matchCase=TRUE){
  url <- get_endpoint("slides.endpoint.batchUpdate", id)
  token <- get_token()
  config <- httr::config(token=token)
  body_params <- list(replaceText = replaceText, containsText = list(text = text, matchCase = matchCase))
  result <- httr::POST(url, config = config, accept_json())
  if(httr::status_code(result) != 200){
    stop("ID provided does not point towards any slide")
  }
}
