#' Create a new googleslide
#' @param title Title of the presentation slide
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
create_slides <- function(title = NULL, full_response = FALSE){
  # Get endpoint url
  url <- get_endpoint("slides.endpoint.create")
  # Get auth token
  token <- get_token()
  config <- httr::config(token=token)
  # Wrapping body parameters in a requests list
  body_params <- list(title=title)
  # Modify slides
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")
  if(httr::status_code(result) != 200){
    stop("ID provided does not point towards any slide")
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  # If user request for minimal response
  if(full_response){
    return(result_list)
  } else {
    return(result_list$presentationId)
  }
}

#' Get Google Slides Properties
#' @param id ID of the presentation slide
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
#' @param id ID of the presentation slide
#' @param pageObjectId The page ID of the presentation slide
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
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
