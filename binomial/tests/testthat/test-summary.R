library(testthat)
#source("./R/binomial-code.R")

context("summary measures in the binomial package")

test_that("aux_mean function works properly with correct inputs", {

  expect_equal(aux_mean(10L, 0.3), 3)
  expect_equivalent(aux_mean(10L, 0.3), aux_mean(10, 0.3))
  expect_lt(aux_mean(10, 0.3), 5)

})

test_that("aux_variance function works properly with correct inputs", {
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equivalent(aux_variance(10L, 0.3), aux_variance(10, 0.3))
  expect_lt(aux_variance(10, 0.3), 5)

})

test_that("aux_mode function works properly with correct inputs", {
  expect_equal(aux_mode(10L, 0.3), 3.3)
  expect_equivalent(aux_mode(10L, 0.3), aux_mode(10, 0.3))
  expect_lt(aux_mean(10, 0.3), 5)

})

test_that("aux_skewness function works properly with correct inputs", {

  expect_equal(aux_skewness(10, 0.3), (1-2*0.3)/(sqrt((10*0.3)*(1-0.3))))
  expect_equivalent(aux_skewness(10L, 0.3), aux_skewness(10, 0.3))
  expect_lt(aux_skewness(10, 0.3), 1)

})


test_that("aux_kurtosis function works properly with correct inputs", {

  expect_equal(aux_kurtosis(10, 0.3), ((1-6*0.3)*(1-0.3)) / ((10*0.3)*(1-0.3)))
  expect_equivalent(aux_kurtosis(10L, 0.3), aux_kurtosis(10, 0.3))
  expect_lt(aux_skewness(10, 0.3), 1)

})



