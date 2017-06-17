#' Common Property: Page Element Property
#' @description This property is repeated in various of the request. Hence, to reduce the number of
#' times this would appear in the code base, a function is created to handle the creation of this
#' list.
#' @param page_object_id A character vector that contains the page id of the slide
#' @param width_magnitude (Optional) A numeric vector that contains the width of the component
#' @param height_magnitude (Optional) A numeric vector that contains the height of the component
#' @param scale_x (Optional) A numeric vector that tells how the object is to be sized
#' @param scale_y (Optional) A numeric vector that tells how the object is to be sized
#' @param shear_x (Optional) A numeric vector that tells how the object is to be sheared
#' @param shear_y (Optional) A numeric vector that tells how the object is to be sheared
#' @param transform_x (Optional) A numeric vector that tells how the object is to be located
#' @param transform_y (Optional) A numeric vector that tells how the object is to be located
#' @return A PageElementProperty Object
#' @importFrom assertthat assert_that
#' @examples
#' \dontrun{
#' library(googleslides)
#'
#' # There is only one compulsory field which is pageObjectId is the
#' # 'slide id' of the slide being referenced to
#' pageElementProperty <- page_element_property("12345")
#' }
#' @export
page_element_property <- function(page_object_id=NULL,
  width_magnitude=NULL, height_magnitude=NULL,
  scale_x=NULL, scale_y=NULL, shear_x=NULL, shear_y=NULL,
  translate_x=NULL, translate_y=NULL, width_unit="PT", height_unit="PT", transform_unit="PT"){
  # Basic data checks
  assert_that(!is.null(page_object_id))
  page_element_property <- page_element_property_container$new(page_object_id)

  page_element_property$page_object_id <- page_object_id
  page_element_property$width_magnitude <- width_magnitude
  page_element_property$height_magnitude <- height_magnitude
  page_element_property$scale_x <- scale_x
  page_element_property$scale_y <- scale_y
  page_element_property$shear_x <- shear_x
  page_element_property$shear_y <- shear_y
  page_element_property$translate_x <- translate_x
  page_element_property$translate_y <- translate_y
  page_element_property$width_unit <- width_unit
  page_element_property$height_unit <- height_unit
  page_element_property$transform_unit <- transform_unit

  return(page_element_property)
}

#' @importFrom R6 R6Class
page_element_property_container <- R6Class('PageElementProperty',
   public = list(
     page_object_id = NULL,
     width_magnitude = NULL, height_magnitude = NULL,
     scale_x = NULL, scale_y = NULL,
     shear_x = NULL, shear_y = NULL,
     translate_x = NULL, translate_y = NULL,
     width_unit = "PT", height_unit = "PT", transform_unit = "PT",
     initialize = function(page_object_id){
       self$page_object_id <- page_object_id
     },
     to_list = function(){
        page_element_list <- list(pageObjectId = self$page_object_id)

        if(is.numeric(self$width_magnitude) | is.numeric(self$height_magnitude)){
          page_element_list[["size"]] <- list()
        }

        if(is.numeric(self$width_magnitude)){
          page_element_list[["size"]][['width']] <- list()
          page_element_list[["size"]][['width']][['magnitude']] <- self$width_magnitude
          page_element_list[['size']][['width']][['unit']] <- self$width_unit
        }

        if(is.numeric(self$height_magnitude)){
          page_element_list[["size"]][['height']] <- list()
          page_element_list[["size"]][['height']][['magnitude']] <- self$height_magnitude
          page_element_list[['size']][['height']][['unit']] <- self$height_unit
        }

        if(is.numeric(self$scale_x) | is.numeric(self$scale_y) |
           is.numeric(self$shear_x) | is.numeric(self$shear_y) |
           is.numeric(self$translate_X) | is.numeric(self$translate_y)){
          page_element_list[['transform']] <- list()
          page_element_list[['transform']][['unit']] <- self$transform_unit
          page_element_list[['transform']][['scaleX']] <- self$scale_x
          page_element_list[['transform']][['scaleY']] <- self$scale_y
          page_element_list[['transform']][['shearX']] <- self$shear_x
          page_element_list[['transform']][['shearY']] <- self$shear_y
          page_element_list[['transform']][['translateX']] <- self$translate_x
          page_element_list[['transform']][['translateY']] <- self$translate_y
        }
        return(page_element_list)
     })
)

