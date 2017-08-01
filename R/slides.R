#' Create a new googleslide
#' @param title Title of the presentation slide
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
create_slides <- function(title = NULL, full_response = FALSE){
  # Get endpoint url
  url <- get_endpoint("slides.endpoint.create")
  # Get auth token
  token <- get_token()
  config <- httr::config(token=token)
  # Wrapping body parameters in a requests list
  body_params <- list(title=title)
  # Modify slides
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")
  if(httr::status_code(result) != 200){
    stop("ID provided does not point towards any slide")
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  # If user request for minimal response
  if(full_response){
    return(result_list)
  } else {
    return(result_list$presentationId)
  }
}

#' Get Google Slides Properties
#' @param id ID of the presentation slide
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_slides_properties <- function(id){
  # Check validity of inputs
  assert_that(is.string(id))

  # Get endpoint url
  url <- get_endpoint("slides.endpoint.get", id)
  # Get auth token
  token <- get_token()
  config <- httr::config(token=token)
  # Get slide properties
  result <- httr::GET(url, config = config, accept_json())
  if(httr::status_code(result) != 200){
    stop("ID provided does not point towards any slide")
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}

#' Get a single page of a Google Slides property
#' @param id ID of the presentation slide
#' @param page_object_id The page ID of the presentation slide
#' @param response Type of response. Values can be "simple" or "raw".
#' A simple response provides a simplified object to query parts of the slide.
#' A raw response provides the actual response via Google API.
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_slide_page_properties <- function(id, page_object_id, response = "simple"){
  # Check validity of inputs
  assert_that(is.string(id))
  assert_that(is.string(page_object_id))

  # Get endpoint url
  url <- get_endpoint("slides.endpoint.page.get", id, page_object_id)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # Get slide properties
  result <- httr::GET(url, config = config, accept_json())
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content, simplifyVector = FALSE)
  if(httr::status_code(result) != 200){
    stop(result_list$error$message)
  }
  # Process and return results
  if(response == 'raw'){
    return(result_list)
  } else {
    slide_page_response <- slide_page_container$new(result_list)
    return(slide_page_response)
  }
}


#' @importFrom R6 R6Class
slide_page_container <- R6Class('SlidePage',
  public = list(
    raw_response = NULL,
    initialize = function(slide_page_list_response){
      self$raw_response <- slide_page_list_response
    },
    # Retrieve a list of tables from the raw response
    get_tables = function(){
      list_tables <- list()
      for (item in self$raw_response$pageElements){
        if (!is.null(item$table)){

          # Retrieve object id
          object_id <- item$objectId

          # Retrieve table
          retrieved_table <- data.frame(stringsAsFactors = FALSE)
          for(table_row in item$table$tableRows){
            retrieved_table_row <- c()
            for(table_cell in table_row$tableCells){
              # Retrieve content from table cells
              text_content <- ""
              if(!is.null(table_cell$text$textElements)){
                for (text_element in table_cell$text$textElements){
                  text_content <- paste0(text_content, text_element$textRun$content)
                }
              }
              retrieved_table_row <- c(retrieved_table_row, text_content)
            }
            retrieved_table <- rbind(retrieved_table, retrieved_table_row, make.row.names = FALSE, deparse.level = 0, stringsAsFactors = FALSE)
          }

          # Concatenate results
          names(retrieved_table)  <- NULL
          retrieved_table <- data.frame(retrieved_table)
          temporary_list <- list()
          temporary_list[['object_id']] <- object_id
          temporary_list[['table']] <- retrieved_table
          list_tables[[length(list_tables) + 1]] <- temporary_list
        }
      }
      return(list_tables)
    },
    # Retrieve a list of text boxes from the raw response
    get_text_boxes = function(){
      list_text_boxes <- data.frame(stringsAsFactors = FALSE)
      for (item in self$raw_response$pageElements){
        if (!is.null(item$shape$shapeType)){
          if (item$shape$shapeType == "TEXT_BOX"){

            # Retrieve text content
            text_content <- ""
            if(!is.null(item$shape$text$textElements)){
              for (text_element in item$shape$text$textElements){
                text_content <- paste0(text_content, text_element$textRun$content)
              }
            }

            # Retrieve object id
            object_id = item$objectId

            # Concatenate results
            list_text_boxes <- rbind(list_text_boxes, c(object_id, text_content), stringsAsFactors = FALSE)
          }
        }
      }
      names(list_text_boxes) <- c('object_id', 'text_content')
      return(list_text_boxes)
    }
  )
)
