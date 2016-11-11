#' Generate endpoint for the Google Slides API
#' Temporarily available
#' @export
get_endpoint <- function(typeOfEndpoint = "slides.endpoint.get", id = NULL, pageObjectId=NULL){
  # Check that id parameter is a character, if not throw an error
  if(!is.character(id)){
    stop("id is not a character.")
  }
  # Check if type of endpoint is slides.endpoint.page.get
  if(typeOfEndpoint == "slides.endpoint.page.get"){
    # Check that pageObjectId parameter is a character, if not throw an error
    if(!is.character(pageObjectId)){
      stop("pageObjectId is not a character")
    }
    url_temp <- gsub("{presentationId}", id, getOption(typeOfEndpoint), fixed=TRUE)
    url_temp <- gsub("{pageObjectId}", id, url_temp, fixed=TRUE)
    return(url_temp)
  }
  return(gsub("{presentationId}", id, getOption(typeOfEndpoint), fixed=TRUE))
}
