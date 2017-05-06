#' @importFrom R6 R6Class
temp <- R6Class("GoogleSlidesRequest",
  public = list(
    requests = list(),
    add_replace_all_text_request = function(replace_text, text, match_case){
      new_request <- list("replaceText" = replace_text,
                         "containsText" = list("text" = text, "matchCase" = match_case))
      wrap_request <- list("replaceAllText" = new_request)
      self$requests[[length(self$requests) + 1]] <- wrap_request
      print("New replace all text request added.")
    },
    get_parameters = function(){
      return(self$requests)
    }
  )
)
