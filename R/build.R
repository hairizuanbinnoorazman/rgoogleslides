#' Building create slide request
#' @param no_of_slides A number to indicate the number of slides that is to be added
#' @param insertionIndex A numeric vector on where the slide is to be added
#' @param layoutId A character vector that provides guidance on which layout the new slide is to follow
#' @param predefinedLayout A character vector that provides guidance on which layout the new slide
#' is to follow. The ones declared here
#' @param objectId A character vector that is to be used to give names to new slides created via the
#' slides API
#' @param requests_list A list of requests that is to be passed to the post_batchUpdate function.
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


#' Building replace all text request
#' @param replaceText A character vector of text that would replace the ones in the text parameter.
#' The order of the replaceText matters
#' @param text A character vector of text that would replaced by the ones in the replaceText parameter.
#' The order of the text matters
#' @param matchCase A boolean that takes in only TRUE or FALSE only. This would be applied across all
#' requests
#' @param requests_list A list of requests that is to be passed to the post_batchUpdate function.
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


#' Building insert text request
#' @param objectId A character vector of objects to insert text into. You can only insert text in
#' tables and shapes
#' @param rowIndex A numeric vector of rows to insert the text into
#' @param columnIndex A numeric vector of columns to insert the text into
#' @param text A character vector of text which is to be inserted into the shape or table
#' @param insertionIndex A numeric vector which indicate the starting point of how the text
#' is to be inserted
#' @param requests_list A list of requests that is to be passed to the post_batchUpdate function.
#' @export
build_insert_text <- function(objectId=NULL, rowIndex=NULL, columnIndex=NULL,
                              text=NULL, insertionIndex=NULL, requests_list=NULL){
  # Check to see if there is any requests_list provided. Else, reinitialize it.
  if(is.null(requests_list)){
    warning("No/Invalid request_list provided. request_list reinitialized")
    requests_list <- list()
  }
  # Loop through the object id vector
  iterator <- 1
  while(iterator <= length(objectId)){
    insert_text_list <- list(insertText = list(objectId = objectId[iterator], text = text[iterator]))
    if(!is.null(rowIndex) & !is.null(columnIndex)){
      insert_text_list[["insertText"]][["cellLocation"]] = list()
      insert_text_list[["insertText"]][["cellLocation"]][["rowIndex"]] <- rowIndex[iterator]
      insert_text_list[["insertText"]][["cellLocation"]][["columnIndex"]] <- columnIndex[iterator]
    }
    if(!is.null(insertionIndex)){
      insert_text_list[["insertText"]][["insertionIndex"]] <- insertionIndex[iterator]
    }
    requests_list[[iterator]] <- insert_text_list
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
#' @param requests_list A list of requests that is to be passed to the post_batchUpdate function.
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

#' Building create table request
#' @param pageElementProperty A list that contains a page element property. The page element is to be
#' generated by the common_page_element_property function in this package.
#' @param rows A numeric vector with the row index of the data to be filled in the table. Only for tables
#' @param columns A numeric vector with the column index of the data to be filled in the table. Only for tables
#' @param objectId (Optional) A character vector to name the object created instead of leaving it to Google
#' @param requests_list A list of requests that is to be passed to the post_batchUpdate function.
#' @export
build_create_table <- function(pageElementProperty=NULL, rows=NULL, columns=NULL,
                               objectId=NULL, requests_list=NULL){
  # Check to see if there is any requests_list provided. Else, reinitialize it.
  if(is.null(requests_list)){
    warning("No/Invalid request_list provided. request_list reinitialized")
    requests_list <- list()
  }
  # Loop through the length of slide object ids
  iterator <- 1
  while(iterator <= length(pageElementProperty)){
    create_table_list <- list(createTable = list(elementProperties = pageElementProperty[[iterator]],
                                                 rows = rows[iterator],
                                                 columns = columns[iterator]))
    requests_list[[iterator]] <- create_table_list
    iterator <- iterator + 1
  }
  return(requests_list)
}

#' Building create image request
#' @param url A character vector container a list of image urls that is to be used to add image to
#' the slides
#' @param pageElementProperty A list that contains a page element property. The page element is to be
#' generated by the common_page_element_property function in this package.
#' @param objectId (Optional) A character vector to name the object created instead of leaving it to Google
#' @export
build_create_image <- function(url=NULL, pageElementProperty=NULL, objectId=NULL, requests_list=NULL){
  # Check to see if there is any requests_list provided. Else, reinitialize it.
  if(is.null(requests_list)){
    warning("No/Invalid request_list provided. request_list reinitialized")
    requests_list <- list()
  }
  # Loop through the length of slide object ids
  iterator <- 1
  while(iterator <= length(pageElementProperty)){
    create_image_list <- list(createImage = list(elementProperties = pageElementProperty[[iterator]],
                                                 url = url[iterator]))
    requests_list[[iterator]] <- create_image_list
    iterator <- iterator + 1
  }
  return(requests_list)
}
