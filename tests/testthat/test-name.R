context("test-name.R")
library(YetAnotherRStudioApplication)
library(testthat)

test_that("test modified residals", {
  X <- matrix(c(1,1,1, 4, 3, 1), nrow = 3)
  y <- c(5.5, 3.5, 1.2)
  mod <- bootstrap_robust_regression(X, y, 100)
  expect_is(mod, "list")
  expect_equal(names(mod), c("coefficients", "se", "residuals", "leverages", "modified residuals"))
})
