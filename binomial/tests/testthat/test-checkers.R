library(testthat)
#source("./R/binomial-code.R")

context("checker functions in the binomial package")
test_that("check_prob function works with normal input",{
  a <- 0.5
  b <- 3

  expect_true(check_prob(a))
  expect_error(check_prob(b))
  expect_length(a, 1)
})

test_that("check_trial function works with normal input", {
  a <- 10L
  b <- 5
  expect_error(check_trials(b))
  expect_true(check_trials(a))
  expect_type(a, "integer")
})

test_that("check_success function works with normal input", {
  a <- 10L
  b <- 20L
  expect_true(check_success(20L, 10L))
  expect_error(check_success(10L, 20L))
  expect_gte(a, 0)
})
