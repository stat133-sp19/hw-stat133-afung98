library(testthat)
#source("./R/binomial-code.R")

context("main binomial functions in the binomial package")

test_that("bin_choose function works properly with correct inputs", {
  expect_error(bin_choose(10L, 20))
  expect_equal(bin_choose(5L, 2), 10)
  expect_equivalent(bin_choose(5L, 2), (factorial(5L))/(factorial(2)*factorial(5L-2)))
})

test_that("bin_probability function works properly with correct inputs", {

  expect_error(bin_probability(10, 5L, 0.5))
  expect_equivalent(bin_probability(2, 5L, 0.5), (bin_choose(5L, 2))*(0.5^2)*((1-0.5)^(5L-2)))
  expect_equal(bin_probability(2, 5L, 0.5), 0.3125)
})

test_that("bin_distribution function works properly with correct inputs", {

  expect_error(bin_distribution(10, 0.5))
  expect_error(bin_distribution(10L, 7))
  expect_is(bin_distribution(10L, 0.5), c("bindis", "data.frame"))
})

test_that("bin_cumulative function works properly with correct inputs", {
  expect_error(bin_cumulative(10, 0.5))
  expect_error(bin_cumulative(10L, 7))
  expect_is(bin_cumulative(10L, 0.5), c("bincum", "data.frame"))

})
