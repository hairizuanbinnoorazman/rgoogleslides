#' Generate endpoint for the Google Slides API
#' Temporarily available
#' @export
get_endpoint <- function(typeOfEndpoint = "slides.endpoint.get", id = NULL, pageObjectId=NULL){
  # Check if type of endpoint is create presentation slide endpoint
  if(typeOfEndpoint == "slides.endpoint.create"){
    return(getOption(typeOfEndpoint))
  }
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
    url_temp <- gsub("{pageObjectId}", pageObjectId, url_temp, fixed=TRUE)
    return(url_temp)
  }
  return(gsub("{presentationId}", id, getOption(typeOfEndpoint), fixed=TRUE))
}

#' Convert dataframe to dataframe with rows and columns
#' @param data Dataframe of the dataset that is to be converted so that it can be used with the google slides API
#' @param headers Boolean to indicate whether to convert taking in mind of the headers or not
#' @export
dataframe_convert <- function(data=NULL, headers=TRUE){
  temp_dataframe <- data.frame()
  i <- 1
  j <- 1
  rowCorrection <- 1
  if(headers){
    header_names <- names(data)
    while(j <= ncol(data)){
      single_header <- data.frame(value = header_names[j], row = i - 1, column = j - 1)
      temp_dataframe <- rbind(temp_dataframe, single_header)
      j <- j + 1
    }
    j <- 1
    rowCorrection <- 0
  }
  while(i <= nrow(data)){
    while(j <= ncol(data)){
      single_row <- data.frame(value = as.character(data[i, j]), row = i - rowCorrection, column = j - 1)
      temp_dataframe <- rbind(temp_dataframe, single_row)
      j <- j + 1
    }
    i <- i + 1
    j <- 1
  }
  return(temp_dataframe)
}

#' Get the list of google drive url
#' @export
get_google_drive_urls <- function(imageId){
  access_token <- get_token()$credentials$access_token
  drive_api_url <- "https://www.googleapis.com/drive/v3/files/"
  get_params <- paste0("?access_token=", access_token, "&alt=media")
  url <- c()
  iterator <- 1
  while(iterator <= length(imageId)){
    url_single <- paste0(drive_api_url, imageId, get_params)
    url <- c(url, url_single)
    iterator <- iterator + 1
  }
  return(url)
}

#' Check if the object is a google slide request object
#' @export
is.google_slide_request <- function(x){
  "GoogleSlidesRequest" %in% class(x)
}

#' Check if the object is a google slide request object
#' @export
is.page_element_property <- function(x){
  "PageElementProperty" %in% class(x)
}

#' Convenience function to return a value if the value is NA
#' @description A function that checks and ensure that the value only returns null or a number.
#' This function can only check one value at a time.
#' @export
check_validity <- function(value){
  if(!is.null(value)){
    if(is.na(value)){
      return(NULL)
    } else {
      return(value)
    }
  }
  return(value)
}

#' Convenience function to return a page element property that aligns said element
#' @description The Googleslides do not provide convenient ways to align the element. In order to do
#' the necessary calculation, one has to be do take into account the slide size as well as image size.
#' @details The following pointers are thought of when doing the following calculations.
#' \itemize{
#'  \item The image is to be scaled without any distortions. No skews etc
#'  \item The translation coordinates of an image is defined by the top left corner of the image
#'  \item The following function will not query the Googleslides API to retrieve the page size automatically.
#'  The user would need to obtain that information and feed it into this function. This is to prevent
#'  this function from overutilizing the API unnecessarily when utilizing this function
#' }
#' @param slide_page_id The id of the slide page that is to be altered
#' @param slide_page_height The slide page height. It is set to default of 9144000
#' @param slide_page_width The slide page width. It is set to default of 5143500
#' @param image_height Image height in pt
#' @param image_width Image width in pt
#' @param align Alignment mode that is to be selected. For now, only the center mode is available
#' @importFrom assertthat assert_that
#' @export
aligned_page_element_property <- function(slide_page_id, slide_page_height = 5143500,
                                          slide_page_width = 9144000,
                                          image_height, image_width, align = "center"){
  # Validate input
  assert_that(is.character(slide_page_id))
  assert_that(is.numeric(slide_page_height))
  assert_that(is.numeric(slide_page_width))
  assert_that(is.numeric(image_height))
  assert_that(is.numeric(image_width))
  assert_that(align %in% c('center'))

  # To convert pt to EMU, use the following calculation: 12700 * 1pt
  image_height_adj <- 12700 * image_height
  image_width_adj <- 12700 * image_width

  # Calculate out translation adj
  translate_x_adj <- as.integer(slide_page_width/2 -image_width_adj/2)
  translate_y_adj <- as.integer(slide_page_height/2 - image_height_adj/2)

  adj_page_element_property <- page_element_property(slide_page_id,
                                                     height_magnitude = image_height,
                                                     width_magnitude = image_width,
                                                     scale_x = 1, scale_y = 1,
                                                     translate_x = translate_x_adj,
                                                     translate_y = translate_y_adj,
                                                     transform_unit = 'EMU')
  return(adj_page_element_property)
}
