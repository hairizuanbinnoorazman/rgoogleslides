.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please use predefined Credentials only for the testing requests. To obtain your own Credentials see help(authorize).")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.slides <- list(
    slides.client.id = "10709400262-28lpv43nui21l1172cup6kh68blvkllq.apps.googleusercontent.com",
    slides.client.secret = "FADkDoE_H0B7j-VytEjTbgaU",
    slides.endpoint = "https://slides.googleapis.com/v1/presentations/"
  )
  toset <- !(names(op.slides) %in% names(op))
  if (any(toset)) options(op.slides[toset])

  invisible()
}
