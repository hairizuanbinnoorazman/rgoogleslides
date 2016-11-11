#' Generate endpoint for the Google Slides API
#' Temporarily available
#' @export
get_endpoint <- function(typeOfEndpoint = "slides.endpoint.get", id = NULL){
  # Check that id parameter is a character, if not throw an error
  if(!is.character(id)){
    stop("id is not a character.")
  }
  return(gsub("{presentationId}", id, getOption(typeOfEndpoint), fixed=TRUE))
}
