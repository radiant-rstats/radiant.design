# library(radiant.design)
# library(testthat)

######### tests ########
test_that("DOE", {
  res1 <- "price; $10; $13; $16\nfood; popcorn; gourmet; no food" %>% doe()
  expect_equal(unlist(res1$eff[5, ]), c(Trials = 9, `D-efficiency` = 1, Balanced = TRUE))
})

test_that("Sample size", {
  res1 <- sample_size(type = "mean", err_mean = 2, sd_mean = 10)
  expect_equal(res1$n, 97)
})

test_that("Sample size (compare) -- n2", {
  res <- sample_size_comp(
    type = "proportion",
    n1 = 38073,
    p1 = 0.008,
    p2 = 0.01,
    conf_lev = 0.95,
    power = 0.9,
    alternative = "less"
  )
  # summary(res)
  expect_equal(ceiling(res$n2), 38073)
})

test_that("Sample size (compare) -- n1 and n2", {
  res <- sample_size_comp(
    type = "proportion",
    p1 = 0.008,
    p2 = 0.01,
    conf_lev = 0.95,
    power = 0.9,
    alternative = "less"
  )
  # summary(res)
  expect_equal(ceiling(res$n1), 38073)
  expect_equal(ceiling(res$n2), 38073)
})

test_that("Sample size (compare) -- power", {
  res <- sample_size_comp(
    type = "proportion",
    n1 = 38073,
    n2 = 38073,
    p1 = 0.008,
    p2 = 0.01,
    conf_lev = 0.95,
    alternative = "less"
  )
  # summary(res)
  expect_equal(round(res$res$power, 1), 0.9)
})

test_that("Sample size (compare) -- sig", {
  res <- sample_size_comp(
    type = "proportion",
    n1 = 38073,
    n2 = 38073,
    p1 = 0.008,
    p2 = 0.01,
    power = 0.9,
    alternative = "less"
  )
  # summary(res)
  expect_equal(round(res$res$sig.level, 2), 0.05)
})
