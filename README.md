# Google Slides for R

[![Travis build status](https://travis-ci.org/hairizuanbinnoorazman/rgoogleslides.svg?branch=master)](https://travis-ci.org/hairizuanbinnoorazman/rgoogleslides)

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/hairizuanbinnoorazman/rgoogleslides?branch=master&svg=true)](https://ci.appveyor.com/project/hairizuanbinnoorazman/rgoogleslides)

This package is wrapper around the Google Slides API and it serves to power data flows from R environment to Google Slides.

The package is available on CRAN. You can install the package using the following:

```
install.packages("rgoogleslides")
```

Or, if you would want to use the latest features in the package, you can run the following:

```
install.packages("devtools")
library(devtools)
devtools::install_github("hairizuanbinnoorazman/rgoogleslides", build_vignettes = TRUE)
```

If you encounter a bug while using the package, take a screenshot of the error and file an issue on this repository.

# Examples and Blog Posts

For latest news and examples of how to use the rgoogleslides package, go to the following blog:
https://www.hairizuan.com/tags/rgoogleslides/

# Privacy Policy

The rgoogleslides package includes a default google credentials in order to make it easier to try the following package. (It is recommended for you to use your client id/client secret from your own Google Project though)

This project does not collect any information from any services that you authorize using this package. To confirm this, you are free to inspect the code base within this R package
