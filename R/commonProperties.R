#' Common Property: Page Element Property
#' @description This property is repeated in various of the request. Hence, to reduce the number of
#' times this would appear in the code base, a function is created to handle the creation of this
#' list.
#' @param pageObjectId A list of character vectors that contains the page id of the slides
#' @param width_magnitude (Optional) A list of numeric vector that contains the width of the component
#' @param height_magnitude (Optional) A list of numeric vector that contains the height of the component
#' @param scaleX (Optional) A list of numeric vector that tells how the object is to be sized
#' @param scaleY (Optional) A list of numeric vector that tells how the object is to be sized
#' @param shearX (Optional) A list of numeric vector that tells how the object is to be sheared
#' @param shearY (Optional) A list of numeric vector that tells how the object is to be sheared
#' @param transformX (Optional) A list of numeric vector that tells how the object is to be located
#' @param transformY (Optional) A list of numeric vector that tells how the object is to be located
#' @export
page_element_property <- function(pageObjectId=NULL,
                                         width_magnitude=NULL,
                                         height_magnitude=NULL,
                                         scaleX=NULL, scaleY=NULL,
                                         shearX=NULL, shearY=NULL,
                                         translateX=NULL, translateY=NULL,
                                         width_unit="EMU", height_unit="EMU",
                                         transform_unit="EMU"){
  # Basic data checks - pageObjectId cannot be null
  if(is.null(pageObjectId)){
    stop("pageObjectId cannot be null")
  }
  # Iterate over each pageObjectId
  iterator <- 1
  page_element_list_multi <- list()
  while(iterator <= length(pageObjectId)){
    page_element_list <- list(pageObjectId = pageObjectId[iterator])
    if(!is.null(check_validity(width_magnitude[iterator])) | !is.null(check_validity(height_magnitude[iterator]))){
      page_element_list[["size"]] <- list()
      if(!is.null(check_validity(width_magnitude[iterator]))){
        page_element_list[["size"]][['width']] <- list()
        page_element_list[["size"]][['width']][['magnitude']] <- width_magnitude[iterator]
        page_element_list[['size']][['width']][['unit']] <- width_unit
      }
      if(!is.null(check_validity(height_magnitude[iterator]))){
        page_element_list[["size"]][['height']] <- list()
        page_element_list[["size"]][['height']][['magnitude']] <- height_magnitude[iterator]
        page_element_list[['size']][['height']][['unit']] <- height_unit
      }
    }
    if(!is.null(check_validity(scaleX[iterator])) | !is.null(check_validity(scaleY[iterator])) | !is.null(check_validity(shearX[iterator])) |
       !is.null(check_validity(shearY[iterator])) | !is.null(check_validity(translateX[iterator])) | !is.null(check_validity(translateY[iterator]))){
      page_element_list[['transform']] <- list()
      page_element_list[['transform']][['unit']] <- transform_unit
    }
    if(!is.null(check_validity(scaleX[iterator]))){
      page_element_list[['transform']][['scaleX']] <- scaleX[iterator]
    }
    if(!is.null(check_validity(scaleY[iterator]))){
      page_element_list[['transform']][['scaleY']] <- scaleY[iterator]
    }
    if(!is.null(check_validity(shearX[iterator]))){
      page_element_list[['transform']][['shearX']] <- shearX[iterator]
    }
    if(!is.null(check_validity(shearY[iterator]))){
      page_element_list[['transform']][['shearY']] <- shearY[iterator]
    }
    if(!is.null(check_validity(translateX[iterator]))){
      page_element_list[['transform']][['translateX']] <- translateX[iterator]
    }
    if(!is.null(check_validity(translateY[iterator]))){
      page_element_list[['transform']][['translateY']] <- translateY[iterator]
    }
    page_element_list_multi[[iterator]] <- page_element_list
    iterator <- iterator + 1
  }
  return(page_element_list_multi)
}
