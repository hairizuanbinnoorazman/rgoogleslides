#' Get Google Slides Properties
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
get_slides_properties <- function(id = NULL){
  # Get endpoint url
  url <- get_endpoint("slides.endpoint.get", id)
  # Get auth token
  token <- get_token()
  config <- httr::config(token=token)
  # Get slide properties
  result <- httr::GET(url, config = config, accept_json())
  if(httr::status_code(result) != 200){
    stop("ID provided does not point towards any slide")
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}

#' Get a single page of a Google Slides property
#' @export
get_slide_page_properties <- function(id = NULL, pageObjectId = NULL){
  # Get endpoint url
  url <- get_endpoint("slides.endpoint.page.get", id, pageObjectId)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # Get slide properties
  result <- httr::GET(url, config = config, accept_json())
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  if(httr::status_code(result) != 200){
    stop(result_list$error$message)
  }
  # Process and return results
  return(result_list)
}

#' Replace all text within the slide
#' @export
replace_all_text <- function(id=NULL, replaceText=NULL, text=NULL, matchCase=TRUE){
  # Creating the list object
  requests_list <- build_replace_all_text(replaceText, text, matchCase)
  result_list <- post_batchUpdate(id, requests_list)
  # Check the occurencesChanged field
  occurencesChanged <- result_list$replies$replaceAllText$occurrencesChanged
  if(is.null(occurencesChanged)){
    return("No changes applied to slides.")
  } else {
    return(paste(occurencesChanged, "occurence(s) changed."))
  }
}

#' Add page in slides
#' @export
create_slide <- function(id=NULL){
  # Creating the list object
  requests_list <- build_create_slide()
  result_list <- post_batchUpdate(id, requests_list)
  # Check the occurencesChanged field
  occurencesChanged <- result_list$replies$replaceAllText$occurrencesChanged
  if(is.null(occurencesChanged)){
    return("No changes applied to slides.")
  } else {
    return(paste(occurencesChanged, "occurence(s) changed."))
  }
}
