#' Building replace all text request
#' @export
build_replace_all_text <- function(replaceText=NULL, text=NULL, matchCase=TRUE, requests_list=NULL){
  if(is.null(requests_list)){
    warning("No/Invalid request_list provided. request_list reinitialized")
    requests_list <- list()
  }
  iterator <- 1
  while(iterator <= length(replaceText)){
    replace_all_text_list <- list(replaceAllText = list(replaceText = replaceText[iterator], containsText = list(text = text[iterator], matchCase = matchCase)))
    requests_list[[iterator]] <- replace_all_text_list
    iterator <- iterator + 1
  }
  return(requests_list)
}
