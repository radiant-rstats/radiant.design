######### tests ########
test_that("DOE", {
  res1 <- "price; $10; $13; $16\nfood; popcorn; gourmet; no food" %>% doe
  expect_equal(unlist(res1$eff[5,]),c(Trials = 9, `D-efficiency` = 1, Balanced = TRUE))
})

test_that("Sample size", {
  res1 <- sample_size(type = "mean", err_mean = 2, sd_mean = 10)
  expect_equal(res1$n,97)
})
