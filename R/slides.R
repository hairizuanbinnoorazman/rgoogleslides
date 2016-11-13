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

#' Replace all text within the slide
#' @inheritParams build_replace_all_text
#' @param id ID of the presentation slide
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
#' @inheritParams build_create_slide
#' @param id ID of the presentation slide
#' @export
create_slide <- function(id=NULL, no_of_slides=1, insertionIndex=NULL,
                         layoutId=NULL, predefinedLayout=NULL,
                         objectId=NULL){
  # Creating the list object
  requests_list <- build_create_slide(no_of_slides, insertionIndex, layoutId, predefinedLayout, objectId)
  result_list <- post_batchUpdate(id, requests_list)
  # Check the occurencesChanged field
  occurencesChanged <- result_list$replies$replaceAllText$occurrencesChanged
  if(is.null(occurencesChanged)){
    return("No changes applied to slides.")
  } else {
    return(paste(occurencesChanged, "occurence(s) changed."))
  }
}

#' Delete object in slides
#' @inheritParams build_delete_object
#' @param id ID of the presentation slide
#' @export
delete_object <- function(id=NULL, objectId=NULL){
  # Creating the list object
  requests_list <- build_delete_object(objectId)
  result_list <- post_batchUpdate(id, requests_list)
  return(result_list)
}

#' Update slides position
#' @inheritParams build_update_slides_position
#' @param id ID of the presentation slide
#' @export
update_slides_position <- function(id=NULL, slideObjectIds=NULL, insertionIndex=NULL){
  # Creating the list object
  requests_list <- build_update_slides_position(slideObjectIds, insertionIndex)
  result_list <- post_batchUpdate(id, requests_list)
  return(result_list)
}

#' Insert text to shapes/tables
#' @inheritParams build_insert_text
#' @param id ID of the presentation slide
#' @export
insert_text <- function(id=NULL, objectId=NULL, rowIndex=NULL, columnIndex=NULL,
                        text=NULL, insertionIndex=NULL){
  # Create the list object
  requests_list <- build_insert_text(objectId, rowIndex, columnIndex, text, insertionIndex)
  result_list <- post_batchUpdate(id, requests_list)
  return(result_list)
}

#' Create table to a page in a slide
#' @inheritParams build_create_table
#' @param id ID of the presentation slide
#' @export
create_table <- function(id=NULL, pageElementProperty=NULL,
                         rows=NULL, columns=NULL, objectId=NULL){
  # Create the list object
  requests_list <- build_create_table(pageElementProperty, rows, columns, objectId)
  result_list <- post_batchUpdate(id, requests_list)
  return(result_list)
}

#' Insert image to a page in a slide
#' @inheritParams build_create_image
#' @param id ID of the presenation slide
#' @export
create_image <- function(id=NULL, url=NULL, pageElementProperty=NULL, objectId=NULL){
  # Create the list object
  requests_list <- build_create_image(url, pageElementProperty, objectId)
  result_list <- post_batchUpdate(id, requests_list)
  return(result_list)
}
