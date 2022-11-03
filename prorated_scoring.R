# title: "Prorated Scoring"
# author: "Joshua Langfus"
# date: "2022-11-03"

# This file contains function useful for doing prorated scoring of item-level data in R. 
# This is meant to roughly replicate SPSS syntax such as, e.g.,  MEAN.5 (which computes 
# a mean as long as there are 5 valid observations). Functions are written in base R whenever
# possible, with additional package requirements noted when possible. Functions are documented
# in Roxygen syntax for consistency.

#' Compute a prorated mean for a give number of valid observations
#'
#' This function mimics the behavior of SPSS MEAN.N statements. A mean
#' is computed as long as N valid items are present in the input vector.
#' 
#' @param x A numeric vector for which to take the mean
#' @param n, A scalar indicating the minimum number of valid observations
#' @param ... Additional arguments (not yet implemented)
#'
#' @return The mean of the input vector or NA
#' @export
#' @examples
#' mean.n(c(1,2,4,NA,5), 4) # returns 5
#' mean.n(c(1,2,4,NA,5), 5) # returns NA

mean.n <- function(x, n) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector (and not a data frame)")
  }
  if (!is.numeric(n) | !length(n) == 1) {
    stop("n must be a single numeric value (e.g., 4)")
  }
  n.miss <- sum(is.na(x))
  if ((length(x) - n.miss) < n) {
    return(NA_real_)
  } else {
    return(mean(x, na.rm =TRUE))
  }
}

#' Compute a prorated sum for a give number of valid observations
#'
#' This function mimics the behavior of SPSS SUM.N statements. A sum
#' is computed as long as N valid items are present in the input vector
#' and prorated (with mean or median imputation) to to the full length of x
#'
#' @param x A numeric vector for which to take the sum
#' @param n, A scalar indicating the minimum number of valid observations
#' @param impute A character vector indicating either "mean" (default) or "median" imputation of missing values. If "none" is given, no prorating is performed and the simple sum is returned (same as sum(x, na.rm = TRUE))
#' @param ... Additional arguments (not yet implemented)
#'
#' @return A numeric, the prorated sum of x
#' @export
#' @examples
#' d <- data.frame(x = c(1,2,3,NA,5), y = c(1, 3, NA, NA, 5))
#' d %>% summarise(across(everything(), sum.n, n=4))

sum.n <- function(x, n, impute = "mean") {
  if (!is.numeric(x)) {
    stop("x must be a numeric vector (not a data frame)")
  }
  if (!is.numeric(n) | !length(n) == 1) {
    stop("n must be a single numeric value (e.g., 4)")
  }
  n.miss <- sum(is.na(x))
  legal.imputes <- c("mean","median","none")
  if (!impute %in% legal.imputes) {
    stop(paste("imputes must be one of:", paste0(legal.imputes,collapse = ', ')))
  }
  if ((length(x) - n.miss) < n) {
    return(NA_real_)
    
  } else if (impute == "mean") {
    m <- mean(x, na.rm = TRUE)
  } else if (impute == "median") {
    m <- median(x, na.rm = TRUE)
  } else if (impute == "none") {
    x <- na.omit(x)
  }
  x[is.na(x)] <- m
  return(sum(x))
}

