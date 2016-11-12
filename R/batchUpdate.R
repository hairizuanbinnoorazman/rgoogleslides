#' Send a POST request to do a batch update on the slides
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
post_batchUpdate <- function(id=NULL, requests_list=NULL){
  # Get endpoint url
  url <- get_endpoint("slides.endpoint.batchUpdate", id)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # Wrapping body parameters in a requests list
  body_params <- list(requests=requests_list)
  # Modify slides
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")
  # Process results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  # If endpoint return url status other than 200, return error message
  if(httr::status_code(result) != 200){
    stop(result_list$error$message)
  }
  return(result_list)
}
