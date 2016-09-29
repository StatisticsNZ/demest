

#' Bayesian demographic estimation and forecasting.
#' 
#' Estimation and forecasting of cross-classified counts, rates, means, and
#' probabilities, using Bayesian hierarchical models.
#'
#' Counts of people and events can be organized into demographic accounts,
#' so that, for instance, population at the end of a period equals
#' population at the start of the period plus births minus deaths.
#' Inference can be carried out simultaneously for all components in
#' the account.
#'
#' Missing data, noisy data, and multiple datasets are all accommodated.
#'
#' The key functions:
#' \describe{
#'   \item{\code{\link{estimateModel}}}{Model a single demographic series,
#' using a single accurate dataset.}
#'   \item{\code{\link{estimateCounts}}}{Model a single demographic series,
#' using multiple noisy datasets.}
#'   \item{\code{\link{estimateAccount}}}{Model a demographic account,
#' using multiple noisy datasets.}
#' }
#'
#' Forecasts based on the estimation results are constructed using functions
#' \code{predictModel}, \code{predictCounts}, and \code{predictAccount}.
#' 
#' Package \pkg{demest} uses S4 classes.  It builds on package \pkg{dembase},
#' which provides the basic data structures and methods for manipulating
#' cross-classified data.
#' 
#' @name demest-package
#'
#' @docType package
#'
#' @useDynLib demest, .registration = TRUE
#'
#' @import dembase
NULL

