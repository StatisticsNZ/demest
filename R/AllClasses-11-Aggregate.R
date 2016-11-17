
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
             ## all elements of 'xArgsAg' have same number of dimensions
             n.x <- length(xArgsAg)
             if (n.x > 1L) {
                 dim <- lapply(xArgsAg, dim)
                 n.dim <- lapply(dim, length)
                 if (!all(mapply(identical, n.dim[1L], n.dim[-1L])))
                     return(gettextf("elements of '%s' do not all have same number of dimensions",
                                     "xArgsAg"))
             }
             ## all elements of 'weightsArgsAg' have class "Counts"
             if (!all(sapply(weightsArgsAg, methods::is, "Counts")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "weightsArgsAg", "Counts"))
             ## all elements of 'weightsArgsAg' have same number of dimensions
             n.w <- length(weightsArgsAg)
             if (n.w > 1L) {
                 dim <- lapply(weightsArgsAg, dim)
                 n.dim <- lapply(dim, length)
                 if (!all(mapply(identical, n.dim[1L], n.dim[-1L])))
                     return(gettextf("elements of '%s' do not all have same number of dimensions",
                                     "weightsArgsAg"))
             }
             ## 'xArgsAg' and 'weightsArgsAg' have same length
             if (!identical(n.x, n.w))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "xArgsAg", "weightsArgsAg"))
             ## elements of 'xArgsAg' and 'weightsArgsAg' have same dimensions
             for (i in seq_along(xArgsAg)) {
                 if (!identical(dim(xArgsAg[[i]]), dim(weightsArgsAg[[i]])))
                     return(gettextf("elements of '%s' and '%s' have different dimensions",
                                     "xArgsAg", "weightsArgsAg"))
             }
             TRUE
         })

## HAS_TESTS
setClass("AxAgMixin",
         slots = c(axAg = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             axAg <- object@axAg
             ## 'axAg' has no missing values
             if (any(is.na(axAg)))
                 return(gettextf("'%s' has missing values",
                                 "axAg"))
             ## 'axAg' is non-negative
             if (any(axAg < 0))
                 return(gettextf("'%s' has negative values",
                                 "axAg"))
             TRUE
         })

## HAS_TESTS
setClass("ConcordancesAgMixin",
         slots = c(concordancesAg = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             concordancesAg <- object@concordancesAg
             ## all elements of 'concordancesAg' have class "ManyToOne"
             if (!all(sapply(concordancesAg, methods::is, "ManyToOne")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "concordancesAg", "ManyToOne"))
             if (length(concordancesAg) > 0L) {
                 names.conc <- names(concordancesAg)
                 ## 'concordancesAg' has names
                 if (is.null(names.conc))
                     stop(gettextf("'%s' does not have names",
                                   "concordancesAg"))
                 ## no duplicated names for 'concordancesAg'
                 if (any(duplicated(names.conc)))
                     stop(gettextf("'%s' has duplicate names",
                                   "concordancesAg"))
             }
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
setClass("MetaDataMxAgMixin",
         slots = c(metadataMxAg = "MetaData"),
         contains = "VIRTUAL",
         validity = function(object) {
             metadataMxAg <- object@metadataMxAg
             dimtypes <- dimtypes(metadataMxAg, use.names = FALSE)
             DimScales <- DimScales(metadataMxAg, use.names = FALSE)
             i.age <- match("age", dimtypes, nomatch = 0L)
             has.age <- i.age > 0L
             ## 'metadataMxAg' has dimension with dimtype "age"
             if (!has.age)
                 return(gettextf("'%s' does not have a dimension with %s \"%s\"",
                                 "metadataMxAg", "dimtype", "age"))
             ## age dimension of 'metadataMxAg' has dimscale "Intervals"
             DimScale.age <- DimScales[[i.age]]
             if (!methods::is(DimScale.age, "Intervals"))
                 return(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                                 "metadataMxAg", "dimtype", "age", "dimscale", "Intervals"))
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

## HAS_TESTS
setClass("MxAgMixin",
         slots = c(mxAg = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             mxAg <- object@mxAg
             ## 'mxAg' has no missing values
             if (any(is.na(mxAg)))
                 return(gettextf("'%s' has missing values",
                                 "mxAg"))
             ## 'mxAg' is non-negative
             if (any(mxAg < 0))
                 return(gettextf("'%s' has negative values",
                                 "mxAg"))
             TRUE
         })

setClass("NAcceptAgMixin",
         slots = c(nAcceptAg = "Counter"),
         contains = "VIRTUAL")

setClass("NAgeAgMixin",
         slots = c(nAgeAg = "Length"),
         contains = "VIRTUAL")

setClass("NFailedPropValueAgMixin",
         slots = c(nFailedPropValueAg = "Counter"),
         contains = "VIRTUAL")

## HAS_TESTS
setClass("NxAgMixin",
         slots = c(nxAg = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             nxAg <- object@nxAg
             ## 'nxAg' has no missing values
             if (any(is.na(nxAg)))
                 return(gettextf("'%s' has missing values",
                                 "nxAg"))
             ## 'nxAg' all positive
             if (any(nxAg <= 0))
                 return(gettextf("'%s' has non-positive values",
                                 "nxAg"))
             TRUE
         })

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
             ## 'valueAg' has the length implied by 'transformAg'
             if (!identical(length(valueAg), as.integer(prod(transformAg@dimAfter))))
                 return(gettextf("'%s' does not have length implied by '%s'",
                                 "valueAg", "transformAg"))
             TRUE
         })

setClass("TransformThetaToMxAgMixin",
         slots = c(transformThetaToMxAg = "CollapseTransformExtra"),
         contains = "VIRTUAL")

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

## HAS_TESTS
setClass("SpecAxAgMixin",
         slots = c(axAg = "ValuesOrNULL"),
         contains = "VIRTUAL",
         validity = function(object) {
             axAg <- object@axAg
             if (!is.null(axAg)) {
                 .Data <- axAg@.Data
                 dimtypes <- dimtypes(axAg, use.names = FALSE)
                 DimScales <- DimScales(axAg, use.names = FALSE)
                 i.age <- match("age", dimtypes, nomatch = 0L)
                 has.age <- i.age > 0L
                 ## 'axAg' has dimension with dimtype "age"
                 if (!has.age)
                     return(gettextf("'%s' does not have a dimension with %s \"%s\"",
                                     "axAg", "dimtype", "age"))
                 ## age dimension of 'axAg' has dimscale "Intervals"
                 DimScale.age <- DimScales[[i.age]]
                 if (!methods::is(DimScale.age, "Intervals"))
                     return(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                                     "axAg", "dimtype", "age", "dimscale", "Intervals"))
                 ## 'axAg' has no missing values
                 if (any(is.na(.Data)))
                     return(gettextf("'%s' has missing values",
                                     "axAg"))
                 ## 'axAg' is non-negative
                 if (any(.Data < 0))
                     return(gettextf("'%s' has negative values",
                                     "axAg"))
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
#' @slot funAg Function to calculate elements of \code{valueAg} from
#'     disaggregated rates, counts, probabilities, or means.
#' @slot weightAg Weights used to calculate \code{valueAg} from
#'     disaggregated rates, counts, probabilities, or means.
#' @slot concordancesAg List of \code{\link[classconc]{ManyToOne}}
#' concordances.
#' @name SpecAggregate-class
#' @export
setClass("SpecAggregate",
         contains = "VIRTUAL")

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgCertain",
         contains = c("SpecAggregate",
                      "ConcordancesAgMixin",
                      "MetaDataAgMixin",             
                      "SpecValueAgMixin",                      
                      "SpecWeightAgMixin"))

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgUncertain",
         contains = c("VIRTUAL",
             "SpecAggregate",
             "ConcordancesAgMixin",
             "MetaDataAgMixin",             
             "SpecValueAgMixin"))

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgNormal",
         contains = c("SpecAgUncertain",
                      "ScaleAgMixin",
                      "SDAgMixin",
                      "SpecWeightAgMixin"))

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgFun",
         contains = c("SpecAgUncertain",
             "FunAgMixin",
             "SDAgMixin",
             "SpecWeightAgMixin"))

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgPoisson",
         contains = c("SpecAgUncertain",
                      "ScaleAgMixin"))

#' @rdname SpecAggregate-class
#' @export
setClass("SpecAgLife",
         contains = c("SpecAgUncertain",
                      "SDAgMixin",
                      "SpecAxAgMixin"))

setClass("SpecAgPlaceholder",
         contains = "SpecAggregate")


## CLASS HIERARCHY - Aggregate ##############################################################

setClass("Aggregate",
         contains = c("VIRTUAL",
             "MetaDataAgMixin",
             "ValueAgMixin"))

setClass("AgCertain",
         contains = c("VIRTUAL",
             "Aggregate",
             "MuMixin",
             "TransformAgMixin",
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
             "TransformAgMixin",
             "WeightAgMixin"))

setClass("AgPoisson",
         contains = c("VIRTUAL",
             "AgUncertain",
             "ExposureAgMixin",
             "MuMixin",
             "NAcceptAgMixin",
             "NFailedPropValueAgMixin",
             "ScaleAgMixin",
             "TransformAgMixin",
             "WeightAgMixin"))

## HAS_TESTS
setClass("AgFun",
         contains = c("VIRTUAL",
             "AgUncertain",
             "ArgsAgMixin",
             "FunAgMixin",
             "SDAgMixin",
             "TransformAgMixin"),
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

## HAS_TESTS
setClass("AgLife",
         contains = c("VIRTUAL",
                      "AgUncertain",
                      "AxAgMixin",
                      "MetaDataMxAgMixin",
                      "MxAgMixin",
                      "NAgeAgMixin",
                      "NxAgMixin",
                      "SDAgMixin",
                      "TransformThetaToMxAgMixin"),
         validity = function(object) {
             theta <- object@theta
             metadataY <- object@metadataY
             valueAg <- object@valueAg@.Data
             metadataAg <- object@metadataAg
             mxAg <- object@mxAg
             metadataMxAg <- object@metadataMxAg
             transformThetaToMxAg <- object@transformThetaToMxAg
             axAg <- object@axAg
             nxAg <- object@nxAg
             nAgeAg <- object@nAgeAg@.Data
             dimtypes.y <- dimtypes(metadataY, use.names = FALSE)
             DimScales.y <- DimScales(metadataY, use.names = FALSE)
             i.age.y <- match("age", dimtypes.y, nomatch = 0L)
             has.age.y <- i.age.y > 0L
             ## 'y' has dimension with dimtype "age"
             if (!has.age.y)
                 return(gettextf("'%s' does not have a dimension with %s \"%s\"",
                                 "y", "dimtype", "age"))
             ## age dimension of 'y' has dimscale "Intervals"
             DimScale.age.y <- DimScales.y[[i.age.y]]
             if (!methods::is(DimScale.age.y, "Intervals"))
                 return(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                                 "y", "dimtype", "age", "dimscale", "Intervals"))
             ## last interval of age dimension of 'y' is open
             dv.age.y <- DimScale.age.y@dimvalues
             if (!is.infinite(dv.age.y[length(dv.age.y)]))
                 return(gettextf("last interval of dimension of '%s' with %s \"%s\" is closed",
                                 "y", "dimtype", "age"))
             ## 'metadataAg' does not have dimension with dimtype "age"
             if (!is.null(metadataAg)) {
                 dimtypes.ag <- dimtypes(metadataAg, use.names = FALSE)
                 if ("age" %in% dimtypes.ag)
                     return(gettextf("'%s' has a dimension with %s \"%s\"",
                                     "metadataAg", "dimtype", "age"))
             }
             ## 'mxAg' and 'axAg' have same length
             if (!identical(length(mxAg), length(axAg)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mxAg", "axAg"))
             ## dimensions of 'metadataMxAg' consistent with length of 'mx'
             if (!identical(as.integer(prod(dim(metadataMxAg))), length(mxAg)))
                 return(gettextf("dimensions of '%s' inconsistent with length of '%s'",
                                 "metadataMxAg", "mxAg"))             
             ## 'dimBefore' for 'transformThetaToMxAg' consistent with 'theta'
             if (!identical(as.integer(prod(transformThetaToMxAg@dimBefore)),
                            length(theta)))
                 return(gettextf("'%s' from '%s' inconsistent with length of '%s'",
                                 "dimBefore", "transformThetaToMxAg", "theta"))
             ## 'dimAfter' for 'transformThetaToMxAg' consistent with 'axAg'
             if (!identical(as.integer(prod(transformThetaToMxAg@dimAfter)),
                            length(mxAg)))
                 return(gettextf("'%s' from '%s' inconsistent with length of '%s'",
                                 "dimAfter", "transformThetaToMxAg", "axAg"))
             ## length of 'mxAg' equal to 'nAge' times length of 'valueAg'
             if (!identical(length(mxAg), as.integer(nAgeAg * length(valueAg))))
                 return(gettextf("'%s', '%s', and '%s' inconsistent",
                                 "mxAg", "nAgeAg", "valueAg"))
             ## length of 'nxAg' equal to 'nAge'
             if (!identical(length(nxAg), nAgeAg))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "nxAg", "nAgeAg"))
             ## 'axAg' consistent with 'nxAg'
             nx.rep <- rep_len(nxAg, length.out = length(axAg))
             if (any(axAg > nx.rep))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "nxAg", "axAg"))
             TRUE
         })
