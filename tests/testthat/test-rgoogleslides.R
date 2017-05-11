context("build requests")

test_that("Test Authorization", {
  token <- readRDS(".httr-oauth")[1]
  rgoogleslides::authorize(token = token)
})

test_that("Test To Ensure Functions work as expected", {
  token <- readRDS(".httr-oauth")[1]
  rgoogleslides::authorize(token = token)

  request <- add_replace_all_text_request(replace_text = "test1000", text = "test")
  rgoogleslides::commit_to_slides(id, request)
})
