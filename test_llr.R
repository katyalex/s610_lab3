library(testthat)
context("Check local linear regression function")
source("llr_functions.R")
# testthat::test_dir(".")
n = 15

## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})


test_that("make_weight_matrix works on simple cases", {
  ## check that the output is a diagonal matrix, that all the elements are positive, that the weights are correct in simple cases where you know what the output shuold be
  expect_true(is.diagonal.matrix(make_weight_matrix(0.2,x,omega = 2)))
  ## check if all the elements are positive
  expect_true(all(diag(make_weight_matrix(0.2,x,omega = 2)) >= 0))
  ## check if the weights are correct in simple cases where you know what the output should be
  expect_equal(round(diag(make_weight_matrix(0.2, c(-0.2,0,0.2), omega = 1)), 3), c(0.820, 0.976, 1))
})

test_that("make_predictor_matrix works on simple cases", {
  ## write tests to check that the dimensions are correct, the first column is all 1's, etc.
  expect_equal(ncol(make_predictor_matrix(rnorm(5))), 2) # check the number of columns
  expect_equal(nrow(make_predictor_matrix(rnorm(5))), 5) # check the number of rows
  # test if the first column is all 1's 
  expect_equal(make_predictor_matrix(rnorm(4))[,1], rep(1, 4))
})
