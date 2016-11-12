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

#' Building create slide request
#' @export
build_create_slide <- function(no_of_slides=1, insertionIndex=NULL,
                               layoutId=NULL, predefinedLayout=NULL,
                               objectId=NULL, requests_list=NULL){
  # Check to see if there is any requests_list provided. Else, reinitialize it.
  if(is.null(requests_list)){
    warning("No/Invalid request_list provided. request_list reinitialized")
    requests_list <- list()
  }
  # Check to see if length of insertionIndex vector is the same as no of slides
  if(!is.null(insertionIndex)){
    if(length(insertionIndex) != no_of_slides){
      stop("insertionIndex is not the same as the number of slides being added")
    }
  }
  iterator <- 1
  while(iterator <= no_of_slides){
    create_slide_list <- list(createSlide=list(slideLayoutReference=list()))
    if(is.null(layoutId) & is.null(predefinedLayout)){
      create_slide_list[["createSlide"]][["slideLayoutReference"]][["predefinedLayout"]] <- "BLANK"
    } else {
      create_slide_list[["createSlide"]][["slideLayoutReference"]][["predefinedLayout"]] <- predefinedLayout[iterator]
    }
    requests_list[[iterator]] <- create_slide_list
    iterator <- iterator + 1
  }
  return(requests_list)
}
