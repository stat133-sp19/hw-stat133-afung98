#code for functions in the "binomial" package
library(roxygen2)
#1.1 private checker functions

#check_prob function
#description: checks the validity of the probabilities in binomial function
#inputs: has one argument 'prob' for probability, p
#stops executing the function when 'prob' is beyond the default range
#output: TRUE or error message
check_prob <- function(prob = 0) {
  if(prob >= 0 & prob <= 1) {
    TRUE
  } else {
    stop("p has to be a number betwen 0 and 1'")
  }
}

#check_trials function
#description: checks if the number of trials is a non-negative integer
#input: has one argument of 'trials' for trials, n
#stops executing the function if trials is not the desired type and value
#specify trials using "L" when calling function
#output: TRUE or error message
check_trials <- function(trials) {
  if (typeof(trials) == "integer" & trials >= 0) {
    TRUE
  } else {
    stop("invalid trials value; try specifying trials with suffix 'L'")
  }
}

#check_success function
#description: checks if the number of successes is greater than number of trials
#input: has argument of 'trials' and 'success'
#stops executing when any successes is larger than trials
#output: TRUE or error message
check_success <- function(trials = 0L, success = c(0:trials)) {
if(any(success > trials) == TRUE)
{ stop("invalid success value; success cannot be greater than trials")
} else {
  TRUE
}
}


#1.2 private auxiliary functions

#aux_mean function
#description: calculates expected value or mean of a binomial distribution
#input: takes argument trials and prob for probability of success
#output: mean of distribution
aux_mean <- function(trials, prob) {
  mean <- trials*prob
  return(mean)
}

#aux_variance function
#description: calculates the variance of the distribution
#input: takes argument trials and prob for probability
#output: variance of distribution
aux_variance <- function(trials, prob) {
  variance <- (trials*prob)*(1-prob)
  return(variance)
}
aux_variance(10, 0.3)

#aux_mode function
#description: calculates the mode; most likely number of success
#input: takes argument trials and prob for probability
#if np+p is an integer, generates 2 modes; otherwise, 1 mode
#output: mode of distribution
aux_mode <- function(trials, prob) {
  if(is.integer((trials*prob) + prob) == TRUE) {
    mode <- (trials*prob) + prob
    mode_1 <- ((trials*prob) + prob) - 1
    output <- c(mode, mode_1)
return(output)
  } else {
    mode <- (trials*prob) + prob
    return(mode)
  }
  }

#aux_skewness function
#description: calculates skewness of the distribution
#input: takes arguments of trials and prob for probability
#output: skewness of distribution
aux_skewness <- function(trials, prob) {
  skewness <- (1-2*prob)/(sqrt((trials*prob)*(1-prob)))
  return(skewness)
}

#aux_kurtosis function
#description: calculates the 'tailedness' of the probability distribution
#input: takes arguments of trials and prob for probability
#output: the tailedness of distribution
aux_kurtosis <- function(trials, prob) {
  kurtosis <- ((1-6*prob)*(1-prob)) / ((trials*prob)*(1-prob))
  return(kurtosis)
}
aux_kurtosis(10, 0.3)

#1.3 function bin_choose()
#' @title Number of Combinations
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param trials number of trials
#' @param success number of successes
#' @return number of combinations
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)

bin_choose <- function(trials, success) {
  if(success > trials) {
    stop("success cannot be greater than trials")
  } else {
    combinations <- (factorial(trials))/(factorial(success)*factorial(trials-success))
    return(combinations)
  }
}

#1.4 function bin_probability
#' @title Probability of a Combination
#' @description calculates the probability of a number of successes in some trials
#' @param success number of successes
#' @param trials number of trials
#' @param prob chance of success
#' @return the probability of getting a number of successes
#' @export
#' @examples
#' probability of getting 2 successes in 5 trials
#' (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)

bin_probability <- function(success, trials, prob) {

    if((check_prob(prob) == TRUE) & (check_success(trials, success) == TRUE) & (check_trials(trials) == TRUE)){
     probability <- (bin_choose(trials, success))*(prob^success)*((1-prob)^(trials-success))
    return(probability)
    }
  else {stop("invalid trials value")}

}



#1.5 function bin_distribution
#' @title Binomial Distribution Data Frame
#' @description creates data frame with the probabilities of successes within a number of trials
#' @param trials number of trials
#' @param prob probability of getting a number of successes
#' @return a probability of success data frame
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)

bin_distribution <- function(trials, prob){
  distrib.dat <- data.frame(
    success = 0:trials,
    probability = bin_probability(0:trials, trials, prob)
  )
  class(distrib.dat) <- c("bindis", "data.frame")
  return(distrib.dat)
}

#function plot.bindis()
#' @export
plot.bindis <- function(x) {
  dis1_plot <- barplot(x$probability, names.arg = x$success)
  return(dis1_plot)
}

dis1 <- bin_distribution(trials = 5L, prob = 0.5)
plot(dis1)

#1.6 function bin_cumulative()
#' @title Cumulative probabilities data frame
#' @description creates a data frame showing successes, their probabilities, and cumulative probabilities
#' @param trials for number of trials
#' @param prob for probability
#' @return a data frame with successes, their probabilities, and cumulative probabilities
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)

bin_cumulative <- function(trials, prob) {
  cum_dat <- data.frame(
    success = 0:trials,
    probability = bin_probability(0:trials, trials, prob),
    cumulative = cumsum(bin_probability(0:trials, trials, prob))
  )
  class(cum_dat) <- c("bincum","data.frame")
  return(cum_dat)
}

#function plot.bincum()
#' @export
plot.bincum <- function(x) {
  bincum <- plot(x$success, x$cumulative, type = "b")
  return(bincum)
}

dis2<- bin_cumulative(trials = 5L, prob = 0.5)
plot(dis2)

#1.7 function bin_variable()
#' @title Binomial Random Variable Object
#' @description lists the number of trials, probability, and attribute of the object
#' @param trials number of trials
#' @param prob probability
#' @return lists number of trials, probability, and object attribute
#' @export
#' @example
#' bin_variable(5L, 0.2)

bin_variable <- function(trials, prob) {
check_trials(trials)
  check_prob(prob)
  binvar <- list(trials = trials, probability = prob)
  class(binvar) <- "binvar"
  return(binvar)
  }

bin_variable(5L, 0.3)

#Method print.binvar()
#' @export
print.binvar <- function(x) {
  cat("Binomial variable")
  cat("\n")
  cat("\n")
  cat("Parameters")
  cat("\n")
  cat("number of trials", x$trials)
  cat("\n")
  cat("probability of success", x$prob)
}

bin1 <- bin_variable(trials = 10L, prob = 0.3)
bin1

#methods summary.binvar()
#' @export
summary.binvar <- function(x){
  class(x) <- "binvar"
  output <- list(
    trials = x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials, x$prob),
    variance = aux_variance(x$trials, x$prob),
    mode = aux_mode(x$trials, x$prob),
    skewness = aux_skewness(x$trials, x$prob),
    kurtosis = aux_kurtosis(x$trials, x$prob)
  )
  class(output) <- "summary.binvar"
  return(output)
}

#methods print.summary.binvar()
#' @export
print.summary.binvar <- function(x) {
  class(x) <- "summary.binvar"
  cat("Binomial variable")
  cat("\n")
  cat("\n")
  cat("Parameters")
  cat("\n")
  cat("- number of trials:", x$trials)
  cat("\n")
  cat("- probability of success:", x$prob)
  cat("\n")
  cat("\n")
  cat("Measures")
  cat("\n")
  cat("- mean:", aux_mean(x$trials, x$prob))
  cat("\n")
  cat("- variance:", aux_variance(x$trials, x$prob))
  cat("\n")
  cat("- mode:", aux_mode(x$trials, x$prob))
  cat("\n")
  cat("- skewness:", aux_skewness(x$trials, x$prob))
  cat("\n")
  cat("- kurtosis:", aux_kurtosis(x$trials, x$prob))
}

#1.8 functions of measures

#bin_mean() function
#' @title mean of binomial distribution
#' @description calculates expected value or mean of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return mean of the binomial distribution
#' @export
#' @example
#' bin_mean(10L, 0.3)
bin_mean <- function(trials, prob) {
  if (check_trials(trials) != TRUE & check_prob(prob) != TRUE) {
    stop("validate number of trials and/or probability")
  } else {
    res <- aux_mean(trials, prob)
    return(res)
  }
}

#bin_variance() function
#' @title variance of binomial distribution
#' @description calculates variance of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return variance of the binomial distribution
#' @export
#' @example
#' bin_variance(10L, 0.3)

bin_variance <- function(trials, prob) {
  if (check_trials(trials) != TRUE & check_prob(prob) != TRUE) {
    stop("validate number of trials and/or probability")
  } else {
    res <- aux_variance(trials, prob)
    return(res)
  }
}

#bin_mode() function
#' @title mode of binomial distribution
#' @description calculates mode of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return mode of the binomial distribution
#' @export
#' @example
#' bin_mode(10L, 0.3)

bin_mode <- function(trials, prob) {
  if (check_trials(trials) != TRUE & check_prob(prob) != TRUE) {
    stop("validate number of trials and/or probability")
  } else {
    res <- aux_mode(trials, prob)
    return(res)
  }
}


#bin_skewness() function
#' @title skewness of binomial distribution
#' @description calculates the skewness of a binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return skewness of the binomial distribution
#' @export
#' @example
#' bin_skewness(10L, 0.3)

bin_skewness <- function(trials, prob) {
  if (check_trials(trials) != TRUE & check_prob(prob) != TRUE) {
    stop("validate number of trials and/or probability")
  } else {
    res <- aux_skewness(trials, prob)
    return(res)
  }
}

#bin_kurtosis() function
#' @title tailedness of binomial distribution
#' @description calculates tailedness of a binomial distribution; kurtosis formula
#' @param trials number of trials
#' @param prob probability
#' @return tailedness of the binomial distribution
#' @export
#' @example
#' bin_kurtosis(10L, 0.3)

bin_kurtosis <- function(trials, prob) {
  if (check_trials(trials) != TRUE & check_prob(prob) != TRUE) {
    stop("validate number of trials and/or probability")
  } else {
    res <- aux_kurtosis(trials, prob)
    return(res)
  }
}
