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
#' @param no_of_slides A number to indicate the number of slides that is to be added
#' @param insertionIndex A numeric vector on where the slide is to be added
#' @param layoutId A character vector that provides guidance on which layout the new slide is to follow
#' @param predefinedLayout A character vector that provides guidance on which layout the new slide
#' is to follow. The ones declared here
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
    # Define object id as slide reference
    if(!is.null(objectId)){
      create_slide_list[["createSlide"]][["objectId"]] <- objectId[iterator]
    }
    # Define insertion index on where the slides to be appended
    if(!is.null(insertionIndex)){
      create_slide_list[["createSlide"]][["insertionIndex"]] <- insertionIndex[iterator]
    }
    # If layout id is available, use layout id, else use predefinedLayout or else if uses default blank layout
    if(!is.null(layoutId)){
      create_slide_list[["createSlide"]][["slideLayoutReference"]][["layoutId"]] <- layoutId[iterator]
    } else if(!is.null(predefinedLayout)){
      create_slide_list[["createSlide"]][["slideLayoutReference"]][["predefinedLayout"]] <- predefinedLayout[iterator]
    } else {
      create_slide_list[["createSlide"]][["slideLayoutReference"]][["predefinedLayout"]] <- "BLANK"
    }
    requests_list[[iterator]] <- create_slide_list
    iterator <- iterator + 1
  }
  return(requests_list)
}

#' Building delete object request
#' @param objectId A character vector of object ids that is to be deleted from the slides
#' @param requests_list A list of requests that is to be passed to the post_batchUpdate function.
#' @export
build_delete_object <- function(objectId=NULL, requests_list=NULL){
  # Check to see if there is any requests_list provided. Else, reinitialize it.
  if(is.null(requests_list)){
    warning("No/Invalid request_list provided. request_list reinitialized")
    requests_list <- list()
  }
  # Loop through the object id vector
  iterator <- 1
  while(iterator <= length(objectId)){
    delete_object_list <- list(deleteObject = list(objectId = objectId[iterator]))
    requests_list[[iterator]] <- delete_object_list
    iterator <- iterator + 1
  }
  return(requests_list)
}

#' Building update slides position request
#' @param slideObjectIds List of Character Vector.
#' @param insertionIndex Numeric Vector. This is where the slides selected in
#' slideobjectids parameter is inserted into
#' @export
build_update_slides_position <- function(slideObjectIds=NULL, insertionIndex=NULL,
                                         requests_list=NULL){
  # Check to see if there is any requests_list provided. Else, reinitialize it.
  if(is.null(requests_list)){
    warning("No/Invalid request_list provided. request_list reinitialized")
    requests_list <- list()
  }
  # Loop through the length of slide object ids
  iterator <- 1
  while(iterator <= length(slideObjectIds)){
    update_slides_postion_list <- list(updateSlidesPosition = list(slideObjectIds = list(slideObjectIds[[iterator]]),
                                       insertionIndex=insertionIndex[iterator]))
    requests_list[[iterator]] <- update_slides_postion_list
    iterator <- iterator + 1
  }
  return(requests_list)
}
