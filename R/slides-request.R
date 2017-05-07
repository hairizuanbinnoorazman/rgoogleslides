#' @importFrom R6 R6Class
google_slide_request_container <- R6Class("GoogleSlidesRequest",
  public = list(
    requests = list(),
    add_request = function(request){
      self$requests[[length(self$requests) + 1]] <- request
    },
    to_list = function(){
      return(self$requests)
    }
  )
)
