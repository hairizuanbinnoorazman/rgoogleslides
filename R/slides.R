#' Check existence of presentation slide
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

#' Replace all text within the slide
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
replace_all_text <- function(id=NULL, replaceText=NULL, text=NULL, matchCase=TRUE){
  # Get endpoint url
  url <- get_endpoint("slides.endpoint.batchUpdate", id)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # Creating the list object
  replace_all_text_list = list(replaceAllText = list(replaceText = replaceText, containsText = list(text = text, matchCase = matchCase)))
  body_params <- list(requests = c(replace_all_text_list))
  # Modify slides
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")
  # Process results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  # If endpoint return url status other than 200, return error message
  if(httr::status_code(result) != 200){
    stop(result_list$error$message)
  }
  # Check the occurencesChanged field
  occurencesChanged <- result_list$replies$replaceAllText$occurrencesChanged
  if(is.null(occurencesChanged)){
    return("No changes applied to slides.")
  } else {
    return(paste(occurencesChanged, "occurence(s) changed."))
  }
}
