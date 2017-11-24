#' S4 class summarizing results from estimation or prediction.
#'
#' \code{SummaryResults} is the superclass for classes holding
#' results from functions \code{\link{estimateModel}},
#' \code{\link{estimateCounts}}, \code{\link{estimateAccount}},
#' \code{\link{predictModel}}, \code{\link{predictCounts}}, and
#' \code{\link{predictAccount}} (though the summaries from the
#' predict functions are generally less informative.)
#'
#' Objects of class \code{SummaryResults} are created using
#' function \code{\link{fetchSummary}}.  Individual components from
#' summary objects can be extracted using functions \code{\link{gelmanDiag}},
#' \code{\link{metropolis}}, and \code{\link{parameters}}.
#' 
#' @param object Object of class \code{SummaryResults}.
#'
#' @slot mcmc named integer vector containing nBurnin, nSim, nChain,
#' nThin, nIteration
#' @slot parameters data.frame summarizing parameter estimates.
#' 
#' @export
setClass("SummaryResults",
         slots = c(mcmc = "integer",
             parameters = "dataframeOrNULL"),
         contains = "VIRTUAL")

## HAS_TESTS
setClass("SummaryDataset",
         slots = c(classStr = "character",
                        dimensions = "character",
                        nCell = "integer",
                        nMissing = "integer",
                        isIntegers = "logical",
                        nZero = "integer",
                        median = "numeric"),
         validity = function(object) {
             nCell <- object@nCell
             nMissing <- object@nMissing
             isIntegers <- object@isIntegers
             nZero <- object@nZero
             median <- object@median
             ## length 1
             for (name in c("classStr", "nCell", "nMissing", "isIntegers", "nZero", "median")) {
                 value <- methods::slot(object, name)
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
             }
             ## no missing values
             for (name in c("classStr", "dimensions", "nCell", "nMissing")) {
                 value <- methods::slot(object, name)
                 if (any(is.na(value)))
                     return(gettextf("'%s' has missing values",
                                     name))
             }
             ## non-negative
             for (name in c("nCell", "nMissing")) {
                 value <- methods::slot(object, name)
                 if (value < 0L)
                     return(gettextf("'%s' is negative",
                                     name))
             }
             ## 'nMissing' less or equal to than 'nCell'
             if (nMissing > nCell)
                 return(gettextf("'%s' is greater than '%s'",
                                 "nMissing", "nCell"))
             all.missing <- nMissing == nCell
             ## if all values missing, then 'isIntegers' and 'median' are NA
             if (all.missing) {
                 for (name in c("isIntegers", "median")) {
                     value <- methods::slot(object, name)
                     if (!is.na(value))
                         return(gettextf("all cells have missing values but '%s' is %s",
                                         name, value))
                 }
             }
             ## if some values observed, then 'isIntegers' and 'median' are not NA
             else {
                 for (name in c("isIntegers", "median")) {
                     value <- methods::slot(object, name)
                     if (is.na(value))
                     return(gettextf("'%s' is missing",
                                     name))
                 }
             }
             ## if 'isIntegers' is TRUE, then 'nZero' is inside valid range
             if (isTRUE(isIntegers)) {
                 if (is.na(nZero))
                     return(gettextf("'%s' is missing",
                                     "nZero"))
                 if (nZero < 0L)
                     return(gettextf("'%s' is negative",
                                     "nZero"))
                 if (nZero > (nCell - nMissing))
                     return(gettextf("'%s' is greater than '%s' minus '%s'",
                                     "nZero", "nCell", "nMissing"))
             }
             ## if 'isIntegers' is not TRUE, then 'nZero' is NA
             else {
                 if (!is.na(nZero))
                     return(gettextf("'%s' is not %s but '%s' is %s",
                                     "isIntegers", "TRUE", "nZero", nZero))
             }
             TRUE
         })

## HAS_TESTS
setClass("SummarySeries",
         slots = c(dimensions = "character",
                        nCell = "integer"),
         validity = function(object) {
             dimensions <- object@dimensions
             nCell <- object@nCell
             ## 'dimensions' has no missing values
             if (any(is.na(dimensions)))
                 return(gettextf("'%s' has missing values",
                                 "dimensions"))
             ## 'dimensions' has no blanks
             if (!all(nzchar(dimensions)))
                 return(gettextf("'%s' has blanks",
                                 "dimensions"))
             ## 'dimensions' has no duplicates
             if (any(duplicated(dimensions)))
                 return(gettextf("'%s' has duplicates",
                                 "dimensions"))
             ## 'nCell' has length 1
             if (!identical(length(nCell), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "nCell", 1L))
             ## 'nCell' is not missing
             if (is.na(nCell))
                 return(gettextf("'%s' is missing",
                                 "nCell"))
             ## 'nCell' is non-negative
             if (nCell < 0L)
                 return(gettextf("'%s' is negative",
                                 "nCell"))
             TRUE
         })

## HAS_TESTS
setClass("SummaryModel",
         slots = c(specification = "character",
             dimensions = "character"),
         validity = function(object) {
             specification <- object@specification
             dimensions <- object@dimensions
             ## 'specification' has length 1
             if (!identical(length(specification), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "specification", 1L))
             ## 'specification' is not missing
             if (is.na(specification))
                 return(gettextf("'%s' is missing",
                                 "specification"))
             ## 'specification' is not blank
             if (!nzchar(specification))
                 return(gettextf("'%s' is blank",
                                 "specification"))
             ## 'dimensions' has no missing values
             if (any(is.na(dimensions)))
                 return(gettextf("'%s' has missing values",
                                 "dimensions"))
             ## 'dimensions' has no blanks
             if (!all(nzchar(dimensions)))
                 return(gettextf("'%s' has blanks",
                                 "dimensions"))
             ## 'dimensions' has no duplicates
             if (any(duplicated(dimensions)))
                 return(gettextf("'%s' has duplicates",
                                 "dimensions"))
             TRUE
         })

## HAS_TESTS
setClass("SummaryResultsModelEst",
         slots = c(gelmanDiag = "numeric",
             metropolis = "dataframeOrNULL",
             model = "SummaryModel",
             y = "SummaryDataset"),
         contains = "SummaryResults")

## HAS_TESTS
setClass("SummaryResultsModelPred",
         slots = c(model = "SummaryModel",
                   metropolis = "NULL"),
         contains = "SummaryResults")

## HAS_TESTS
setClass("SummaryResultsCounts",
         slots = c(gelmanDiag = "numeric",
             metropolis = "dataframeOrNULL",
             model = "SummaryModel",
             y = "SummarySeries",
             dataModels = "list",
             datasets = "list",
             namesDatasets = "character"),
         contains = "SummaryResults",
         validity = function(object) {
             dataModels <- object@dataModels
             datasets <- object@datasets
             namesDatasets <- object@namesDatasets
             ## all elements of 'dataModels' have class "SummaryModel"
             if (!all(sapply(dataModels, is, "SummaryModel")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "dataModels", "SummaryModel"))
             ## all elements of 'datasets' have class "SummaryDataset"
             if (!all(sapply(datasets, is, "SummaryDataset")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "datasets", "SummaryDataset"))
             TRUE
         })

## HAS_TESTS
#' S4 class to hold finite-population standard deviations.
#'
#' Object of class \code{FiniteSD} hold the finite-population standard
#' deviations of main effects and interactions from hierarchical models.
#' See the documentation for \code{\link{fetchFiniteSD}} for details.
#'
#' @param object Object of class \code{FiniteSD}.
#'
#' @slot df Integer vector holding the degrees of freedom.
#'
#' @export
setClass("FiniteSD",
         slots = c(df = "integer"),
         contains = "Values",
         validity = function(object) {
             .Data <- object@.Data
             dim <- dim(object)
             names <- names(object)
             dimtypes <- dembase::dimtypes(object, use.names = FALSE)
             df <- object@df
             ## has names "term" and "quantile"
             if (!identical(names, c("term", "quantile")))
                 return(gettextf("does not have dimensions \"%s\" and \"%s\"",
                                 "term", "quantile"))
             ## has dimtypes "state" and "quantile"
             if (!identical(dimtypes, c("state", "quantile")))
                 return(gettextf("does not have dimtypes \"%s\" and \"%s\"",
                                 "state", "quantile"))
             ## "quantile" dimension has length of at least 1
             if (dim[2L] < 1L)
                 return(gettextf("\"%s\" dimension has length %d",
                                 "quantile", 0L))
             ## 'df' has no missing values
             if (any(is.na(df)))
                 return(gettextf("'%s' has missing values",
                                 "df"))
             ## 'df' all positive
             if (any(df < 1L))
                 return(gettextf("'%s' has values less than %d",
                                 "df", 1L))
             ## 'df' has length equal to 'terms' dimension
             if (!identical(length(df), dim[1L]))
                 return(gettextf("'%s' and \"%s\" dimension have different lengths",
                                 "df", "term"))
             TRUE
         })












