# title: "Prorated Scoring"
# author: "Joshua Langfus"
# date: "2022-11-03"

# This file contains function useful for doing prorated scoring of item-level data in R. This is meant to roughly replicate SPSS syntax such as, e.g.,  MEAN.5 (which computes a mean as long as there are 5 valid observations).

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
  n.miss <- sum(is.na(x))
  if ((length(x) - n.miss) < n) {
    return(NA_real_)
  } else {
    return(mean(x, na.rm =TRUE))
  }
}
