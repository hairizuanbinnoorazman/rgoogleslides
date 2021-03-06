% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slides.R
\name{get_slide_page_properties}
\alias{get_slide_page_properties}
\title{Get a single page of a Google Slides property}
\usage{
get_slide_page_properties(id, page_object_id, response = "simple")
}
\arguments{
\item{id}{ID of the presentation slide}

\item{page_object_id}{The page ID of the presentation slide}

\item{response}{Type of response. Values can be "simple" or "raw".
A simple response provides a simplified object to query parts of the slide.
A raw response provides the actual response via Google API.}
}
\description{
Get a single page of a Google Slides property
}
