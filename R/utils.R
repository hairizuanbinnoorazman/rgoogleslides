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
