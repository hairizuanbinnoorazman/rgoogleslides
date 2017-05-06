.slidesEnv <- new.env(parent = emptyenv())
.slidesEnv$Token <- NULL

# Set token to environment
set_token <- function(value) {
  .slidesEnv$Token <- value
  return(value)
}

# Get token from environment
get_token <- function() {
  .slidesEnv$Token
}

#' Authorize R package to access Google Slides API
#' @description This is a function to authorize the R package to access the Googleslides API. If no
#' client.id and client.secret is provided, the package would provide predefined values.
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @param client_id OAuth client ID. This is obtained from Google API Credentials
#' @param client_secret OAuth client secret. This is obtained from Google API Credentials
#' @export
authorize <- function(client_id = getOption("slides.client.id"),
                      client_secret = getOption("slides.client.secret")){
  app <- oauth_app(appname = "googleslides", key = client_id, secret = client_secret)
  endpoint <- oauth_endpoints("google")
  token <- oauth2.0_token(endpoint = endpoint, app = app,
                          scope = c("https://www.googleapis.com/auth/presentations",
                                    "https://www.googleapis.com/auth/drive.readonly"))
  set_token(token)
  return(invisible(token))
}
