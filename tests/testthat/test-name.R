context("test-name.R")
library(YetAnotherRStudioApplication)
library(testthat)

test_that("bootstrap", {
  bset <- bootstrap(iris, 100)
  expect_is(bset, "bootstrap")
  expect_is(bootstrap_map(bset, nrow), "list")
})

test_that("rlm", {
  X <- matrix(c(1,1,1, 4, 3, 1), nrow = 3)
  y <- c(5.5, 3.5, 1.2)
  mod <- bootstrap_rlm(X, y, 100)
  expect_is(mod, "list")
  expect_equal(names(mod), c("coefficients", "se", "residuals", "leverages", "modified residuals"))
})
