years <- 0:100
future_value <- function(amount = 1000, rate = 0.05, years = 1) {
  for (i in years) {
    futvalue <- amount * ((1 + rate)^i)
  }
  return(futvalue)
}

years <- 0:100
annuity <- function(contrib = 200, rate = 0.05, years = 1) {
  for (i in years) {
    fvannuity <- contrib * ((((1 + rate)^i) - 1) / rate)
  }
  return(fvannuity)
}

years <- 0:100
growing_annuity <- function(contrib = 200, rate = 0.05, growth = 0.03, years = 1) {
  for (i in years) {
    fvgannuity <- contrib * ((((1 + rate)^i) - ((1 + growth)^i)) / (rate - growth))
  }
  return(fvgannuity)
}
