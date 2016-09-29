
## Mixin Classes ###############################################################

## NO_TESTS
setClass("ArgsAgMixin",
         slots = c(xArgsAg = "list",
             weightsArgsAg = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             xArgsAg <- object@xArgsAg
             weightsArgsAg <- object@weightsArgsAg
             ## all elements of 'xArgsAg' have class "Values"
             if (!all(sapply(xArgsAg, methods::is, "Values")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "xArgsAg", "Values"))
             ## all elements of 'xArgsAg' have same dimensions
             n.x <- length(xArgsAg)
             if (n.x > 1L) {
                 dim <- lapply(xArgsAg, dim)
                 if (!all(mapply(identical, dim[1L], dim[-1L])))
                     return(gettextf("elements of '%s' do not all have same dimensions",
                                     "xArgsAg"))
             }
             ## all elements of 'weightsArgsAg' have class "Counts"
             if (!all(sapply(weightsArgsAg, methods::is, "Counts")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "weightsArgsAg", "Counts"))
             ## all elements of 'weightsArgsAg' have same dimensions
             n.w <- length(weightsArgsAg)
             if (n.w > 1L) {
                 dim <- lapply(weightsArgsAg, dim)
                 if (!all(mapply(identical, dim[1L], dim[-1L])))
                     return(gettextf("elements of '%s' do not all have same dimensions",
                                     "weightsArgsAg"))
             }
             ## 'xArgsAg' and 'weightsArgsAg' have same length
             if (!identical(n.x, n.w))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "xArgsAg", "weightsArgsAg"))
             ## elements of 'xArgsAg' and 'weightsArgsAg' have same dimensions
             if (!identical(dim(xArgsAg[[1L]]), dim(weightsArgsAg[[1L]])))
                 return(gettextf("elements of '%s' and '%s' have different dimensions",
                                 "xArgsAg", "weightsArgsAg"))
             TRUE
         })

## HAS_TESTS
setClass("ExposureAgMixin",
         slots = c(exposureAg = "ScaleVec"),
         contains = "VIRTUAL",
         validity = function(object) {
             exposureAg <- object@exposureAg@.Data
             valueAg <- object@valueAg@.Data
             ## 'exposureAg' and 'valueAg' have same length
             if (!identical(length(exposureAg), length(valueAg)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "exposureAg", "valueAg"))
             TRUE
         })         

## HAS_TESTS
setClass("FunAgMixin",
         slots = c(funAg = "function"),
         contains = "VIRTUAL",
         validity = function(object) {
             funAg <- object@funAg
             ## 'funAg' has arguments 'x' and 'weights'
             formals.obtained <- formals(funAg)
             formals.expected <- formals(function(x, weights) NULL)
             if (!identical(formals.obtained, formals.expected))
                 return(gettextf("'%s' does not have formal arguments '%s' and '%s'",
                                 "funAg", "x", "weights"))
             TRUE
         })

## HAS_TESTS 
setClass("MeanAgMixin",
         slots = c(meanAg = "ParameterVector"),
         contains = "VIRTUAL",
         validity = function(object) {
             meanAg <- object@meanAg@.Data
             valueAg <- object@valueAg@.Data
             lower <- object@lower
             upper <- object@upper
             tolerance <- object@tolerance
             g <- getTransform(object)
             ## 'meanAg' and 'valueAg' have same length
             if (!identical(length(meanAg), length(valueAg)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "meanAg", "valueAg"))
             ## 'meanAg' within lower, upper bounds
             if (any(g(meanAg) < lower - tolerance))
                 return(gettextf("'%s' less than '%s'",
                                 "meanAg", "lower"))
             if (any(g(meanAg) > upper + tolerance))
                 return(gettextf("'%s' greater than '%s'",
                                 "meanAg", "upper"))
             TRUE
         })

## HAS_TESTS
setClass("MetaDataAgMixin",
         slots = c(metadataAg = "MetaDataOrNULL"),
         contains = "VIRTUAL",
         validity = function(object) {
             metadataAg <- object@metadataAg
             valueAg <- object@valueAg@.Data
             length.value <- length(valueAg)
             if (is.null(metadataAg)) {
                 ## if 'metadataAg' is NULL, then 'valueAg' has length 1
                 if (!identical(length.value, 1L))
                     return(gettextf("'%s' is %s but '%s' does not have length %d",
                                     "metadataAg", "NULL", "valueAg", 1L))
             }
             else {
                 ## if 'metadataAg' is not NULL, its dimensions are consistent
                 ## with the length of 'valueAg'
                 length.metadata <- as.integer(prod(dim(metadataAg)))
                 if (!identical(length.metadata, length.value))
                     return(gettextf("dimensions of '%s' not consistent with length of '%s'",
                                     "metadataAg", "valueAg"))
                 ## 'metadataAg' does not have dimension with dimtype "iteration" or "quantile"
                 for (dimtype in c("iteration", "quantile")) {
                     if (dimtype %in% dembase::dimtypes(metadataAg))
                         return(gettextf("'%s' has dimension with dimtype \"%s\"",
                                         "metadataAg", dimtype))
                 }
             }
             TRUE
         })

## HAS_TESTS
setClass("MuMixin",
         slots = c(mu = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             mu <- object@mu
             theta <- object@theta
             ## 'mu' has type "double"
             if (!is.double(mu))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "mu", "double"))
             ## 'mu' has no missing values
             if (any(is.na(mu)))
                 return(gettextf("'%s' has missing values",
                                 "mu"))
             ## 'mu' has same length as 'theta'
             if (!identical(length(mu), length(theta)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mu", "theta"))
             TRUE
         })

setClass("NAcceptAgMixin",
         slots = c(nAcceptAg = "Counter"),
         contains = "VIRTUAL")

setClass("NFailedPropValueAgMixin",
         slots = c(nFailedPropValueAg = "Counter"),
         contains = "VIRTUAL")

setClass("ScaleAgMixin",
         slots = c(scaleAg = "Scale"),
         contains = "VIRTUAL")

## HAS_TESTS
setClass("SDAgMixin",
         slots = c(sdAg = "ScaleVec"),
         contains = "VIRTUAL",
         validity = function(object) {
             sdAg <- object@sdAg@.Data
             valueAg <- object@valueAg@.Data
             ## 'sdAg' and 'valueAg' have same length
             if (!identical(length(sdAg), length(valueAg)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "sdAg", "valueAg"))
             TRUE
         })

setClass("SpecValueAgMixin",
         slots = c(valueAg = "ParameterVector"),
         contains = "VIRTUAL")

## HAS_TESTS
setClass("SpecWeightAgMixin",
         slots = c(weightAg = "CountsOrNULL"),
         contains = "VIRTUAL",
         validity = function(object) {
             weightAg <- object@weightAg
             ## 'weightAg' has no negative values
             if (!is.null(weightAg)) {
                 if (any(weightAg < 0, na.rm = TRUE))
                     return(gettextf("'%s' has negative values",
                                     "weightAg"))
             }
             TRUE
         })

## HAS_TESTS
setClass("TransformAgMixin",
         slots = c(transformAg = "CollapseTransformExtra"),
         contains = "VIRTUAL",
         validity = function(object) {
             valueAg <- object@valueAg@.Data
             transformAg <- object@transformAg
             theta <- object@theta
             ## 'valueAg' has the length implied by 'transformAg'
             if (!identical(length(valueAg), as.integer(prod(transformAg@dimAfter))))
                 return(gettextf("'%s' does not have length implied by '%s'",
                                 "valueAg", "transformAg"))
             TRUE
         })

## HAS_TESTS 
setClass("ValueAgMixin",
         slots = c(valueAg = "ParameterVector"),
         contains = "VIRTUAL")


## HAS_TESTS
setClass("WeightAgMixin",
         slots = c(weightAg = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             weightAg <- object@weightAg
             transformAg <- object@transformAg
             valueAg <- object@valueAg
             theta <- object@theta
             ## 'weightAg' has type "double"
             if (!is.double(weightAg))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "weightAg", "double"))
             ## all non-missing values in 'weightAg' are non-negative
             if (any(weightAg < 0, na.rm = TRUE))
                 return(gettextf("'%s' has negative values",
                                 "weightAg"))
             ## 'weightAg' has length implied by 'transformAg'
             if (!identical(length(weightAg), as.integer(prod(transformAg@dimBefore))))
                 return(gettextf("'%s' does not have length implied by '%s'",
                                 "weightAg", "transformAg"))
             ## elements of 'weightAg' missing iff not used for aggregate value
             for (i.before in seq_along(weightAg)) {
                 i.after <- dembase::getIAfter(i.before,
                                               transform = transformAg,
                                               check = FALSE,
                                               useC = TRUE)
                 in.after <- i.after > 0L
                 weight.missing <- is.na(weightAg[i.before])
                 if (in.after) {
                     if (weight.missing)
                         return(gettextf("element %d of '%s' is used for aggregate value but is missing",
                                         i.before, "weightAg"))
                 }
                 else {
                     if (!weight.missing)
                         return(gettextf("element %d of '%s' is not used for aggregate value but is not missing",
                                         i.before, "weightAg"))
                 }
             }
             ## 'valueAg' consistent with 'theta' and 'weights'
             for (i.after in seq_along(valueAg)) {
                 i.before <- dembase::getIBefore(i.after,
                                                 transform = transformAg,
                                                 useC = TRUE)
                 weights <- weightAg[i.before]
                 value <- valueAg[i.after]
                 weighted.sum.theta <- sum(theta[i.before] * weights)
                 if (!isTRUE(all.equal(value, weighted.sum.theta)))
                     return(gettextf("aggregate value %d [%s] not equal to weighted sum of associated '%s' [%s]",
                                     i.after, format(value, digits = 5),
                                     "theta", format(weighted.sum.theta, digits = 5)))
             }
             TRUE
         })


## CLASS HIERARCHY - SpecAggregate ##########################################################

#' S4 classes to represent aggregate values.
#'
#' Aggregate values can be used to represent additional
#' information beyond what is contained in 'y',
#' such as expert judgements.
#'
#' @slot metadataAg Metadata (such as \code{\link[dembase]{dimtypes}}
#'     for aggregate values.
#' @slot scaleAg Standard deviation of proposal density
#'     for Metropolis-Hastings updates.
#' @slot valueAg Parameter(s) estimated by the aggregate values.
#' @slot weightAg Weights used to calculate \code{valueAg} from
#'     disaggregated rates, counts, probabilities, or means.
#' @name SpecAggregate-class
#' @export
setClass("SpecAggregate",
         contains = "VIRTUAL")

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgCertain",
         contains = c("SpecAggregate",
             "MetaDataAgMixin",             
             "SpecValueAgMixin",
             "SpecWeightAgMixin"))

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgUncertain",
         contains = c("VIRTUAL",
             "SpecAggregate",
             "MetaDataAgMixin",
             "ScaleAgMixin",
             "SpecValueAgMixin",
             "SpecWeightAgMixin"))

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgNormal",
         contains = c("SpecAgUncertain",
             "SDAgMixin"))

setClass("SpecAgLife",
         contains = "SpecAgNormal")

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgFun",
         contains = c("SpecAgUncertain",
             "FunAgMixin",
             "SDAgMixin"))

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgPoisson",
         contains = "SpecAgUncertain")

setClass("SpecAgPlaceholder",
         contains = "SpecAggregate")


## CLASS HIERARCHY - Aggregate ##############################################################

setClass("Aggregate",
         contains = c("VIRTUAL",
             "MetaDataAgMixin",
             "TransformAgMixin",
             "ValueAgMixin"))

setClass("AgCertain",
         contains = c("VIRTUAL",
             "Aggregate",
             "MuMixin",
             "WeightAgMixin"))

setClass("AgUncertain",
         contains = c("VIRTUAL",
             "Aggregate",
             "MeanAgMixin"))

setClass("AgNormal",
         contains = c("VIRTUAL",
             "AgUncertain",
             "MuMixin",
             "NAcceptAgMixin",
             "NFailedPropValueAgMixin",
             "ScaleAgMixin",
             "SDAgMixin",
             "WeightAgMixin"))

setClass("AgPoisson",
         contains = c("VIRTUAL",
             "AgUncertain",
             "ExposureAgMixin",
             "MuMixin",
             "NAcceptAgMixin",
             "NFailedPropValueAgMixin",
             "ScaleAgMixin",
             "WeightAgMixin"))

## NO_TESTS
setClass("AgFun",
         contains = c("VIRTUAL",
             "AgUncertain",
             "ArgsAgMixin",
             "FunAgMixin",
             "SDAgMixin"),
         validity = function(object) {
             valueAg <- object@valueAg
             funAg <- object@funAg
             xArgsAg <- object@xArgsAg
             weightsArgsAg <- object@weightsArgsAg
             for (i in seq_along(valueAg)) {
                 ans.obtained <- valueAg[i]
                 x <- xArgsAg[[i]]
                 weights <- weightsArgsAg[[i]]
                 ## funAg runs without error
                 ans.expected <- tryCatch(funAg(x = x, weights = weights),
                                          error = function(e) e)
                 if (methods::is(ans.expected, "error"))
                     return(gettextf("error calculating element %d of '%s' : %s",
                                     i, 'valueAg', ans.expected$message))
                 ## funAg, valueAg, weightsArgsAg, xArgsAg consistent
                 if (!isTRUE(all.equal(ans.obtained, ans.expected)))
                     return(gettextf("element %d of '%s' not equal to funAg(x, weights)",
                                     i, "valueAg"))
             }
             TRUE
         })

