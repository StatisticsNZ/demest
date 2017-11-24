
## HAS_TESTS
setClass("CombinedBinomial",
         contains = c("VIRTUAL", "NotHasSubtotals"),
         validity = function(object) {
             model <- object@model
             ## 'model' has class Binomial
             if (!methods::is(model, "Binomial"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "model", class(model)))
             TRUE
         })

## HAS_TESTS
setClass("CombinedNormal",
         contains = c("VIRTUAL", "NotHasSubtotals"),
         validity = function(object) {
             model <- object@model
             w <- model@w
             y <- object@y
             ## 'model' has class "Normal"
             if (!methods::is(model, "Normal"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "model", class(model)))
             ## 'w' has missing values only if 'y' does
             if (length(w) == length(y)) { # to avoid confusing error message; this is tested elsewhere
                 if (any(is.na(w) > is.na(y)))
                     return(gettextf("'%s' has missing values where '%s' does not",
                                     "w", "y"))
             }
             TRUE
         })

## HAS_TESTS
setClass("CombinedPoisson",
         contains = "VIRTUAL",
         validity = function(object) {
             model <- object@model
             y <- object@y
             ## 'model' has class Poisson
             if (!methods::is(model, "Poisson"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "model", class(model)))
             TRUE
         })



setClass("Combined",
         contains = c("VIRTUAL", "SlotsToExtract", "IMethodCombined"))

## CombinedModel

## HAS_TESTS
setClass("CombinedModel",
         slots = c(model = "Model",
                        y = "DemographicArray"),
         prototype = prototype(slotsToExtract = "model"),
         contains = c("Combined", "VIRTUAL"))

setClass("CombinedModelNotHasExp",
         contains = c("CombinedModel", "NotHasExposure", "VIRTUAL"))

setClass("CombinedModelHasExp",
         contains = c("CombinedModel", "HasExposure", "VIRTUAL"))

## HAS_TESTS
setClass("CombinedModelBinomial",
         prototype = prototype(iMethodCombined = 1L),
         contains = c("CombinedModelHasExp", "CombinedBinomial", "YNonNegativeCounts"),
         validity = function(object) {
             model <- object@model
             theta <- model@theta
             y <- object@y
             exposure <- object@exposure
             ## 'exposure' has type "integer"
             if (!is.integer(exposure))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "exposure", "integer"))
             ## 'y' and 'theta' have same length
             if (!identical(length(y), length(theta)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "y", "theta"))
             ## 'y' <= 'exposure'
             if (any(y[!is.na(y)] > exposure[!is.na(y)]))
                 return(gettextf("%s for some cells", "y > exposure"))
             TRUE
         })

## HAS_TESTS
setClass("CombinedModelNormal",
         prototype = prototype(iMethodCombined = 2L),
         contains = c("CombinedModelNotHasExp", "CombinedNormal", "Y"),
         validity = function(object) {
             model <- object@model
             theta <- model@theta
             w <- model@w
             y <- object@y
             ## 'y' has type "double"
             if (!is.double(y))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "y", "double"))
             ## 'y' and 'theta' have same length
             if (!identical(length(y), length(theta)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "y", "theta"))
             ## 'w' is missing only if 'y' is
             if (any(is.na(w) > is.na(y)))
                 return(gettextf("'%s' has missing values where '%s' does not",
                                 "w", "y"))
             TRUE
         })

## HAS_TESTS
setClass("CombinedModelPoissonNotHasExp",
         prototype = prototype(iMethodCombined = 3L),
         contains = c("CombinedModelNotHasExp", "CombinedPoisson", "YNonNegativeCounts"),
         validity = function(object) {
             model <- object@model
             theta <- model@theta
             y <- object@y
             ## 'y' and 'theta' have same length
             if (!identical(length(y), length(theta)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "y", "theta"))
             TRUE
         })

## HAS_TESTS
setClass("CombinedModelPoissonHasExp",
         prototype = prototype(iMethodCombined = 4L),
         contains = c("CombinedModelHasExp", "CombinedPoisson", "YNonNegativeCounts"),
         validity = function(object) {
             model <- object@model
             theta <- model@theta
             y <- object@y
             exposure <- object@exposure
             ## 'y' and 'theta' have same length
             if (!identical(length(y), length(theta)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "y", "theta"))
             ## 'exposure' has type "double"
             if (!is.double(exposure))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "exposure", "double"))
             ## y is 0 if exposure is 0
             if (any(y[!is.na(y)][exposure[!is.na(y)] == 0] > 0L))
                 return(gettextf("%s but %s for some cells", "y > 0", "exposure == 0"))
             TRUE
         })

setClass("CombinedCounts",
         slots = c(model = "Model"),
         prototype = prototype(slotsToExtract = c("model", "y", "dataModels")),
         contains = c("VIRTUAL",
                      "Combined",
                      "YNonNegativeCounts",
                      "DataMixin"))

## HAS_TESTS
setClass("CombinedCountsPoissonNotHasExp",
         prototype = prototype(iMethodCombined = 6L),
         contains = c("CombinedCounts",
                      "CombinedPoisson",
                      "NotHasExposure"))

## HAS_TESTS
setClass("CombinedCountsPoissonHasExp",
         prototype = prototype(iMethodCombined = 7L),
         contains = c("CombinedCounts", "CombinedPoisson", "HasExposure"),
         validity = function(object) {
             exposure <- object@exposure
             ## 'exposure' has type "double"
             if (!is.double(exposure))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "exposure", "double"))
             TRUE
         })

## HAS_TESTS
setClass("CombinedCountsBinomial",
         prototype = prototype(iMethodCombined = 8L),
         contains = c("CombinedCounts",
                      "CombinedBinomial",
                      "HasExposure"))

setClass("CombinedAccount",
         contains = c("VIRTUAL",
                      "AccountMixin",
                      "SystemMixin",
                      "DataMixin",
                      "SeriesIndicesMixin"),
         validity = function(object) {
             systemModels <- object@systemModels
             dataModels <- object@dataModels
             ## 'dataModels' has length 0 iff 'systemModels' consists
             ## entirely of models with class "SingleIter"
             obs.length.0 <- identical(length(dataModels), 0L)
             sys.all.single <- all(sapply(systemModels, methods::is, "SingleIter"))
             if (!identical(obs.length.0, sys.all.single))
                 return(gettextf("'%s' should have length %d iff '%s' consists entirely of models with class \"%s\"",
                                 "dataModels", 0L, "systemModels", "SingleIter"))
             TRUE
         })
                               
setClass("CombinedAccountMovements",
         prototype = prototype(iMethodCombined = 9L),
         slots = c(account = "Movements"),
         contains = c("CombinedAccount",
                      "SystemMovementsMixin"),
         validity = function(object) {
             datasets <- object@datasets
             namesDatasets <- object@namesDatasets
             seriesIndices <- object@seriesIndices
             components <- object@account@components
             ## all elements of "datasets' are non-negative, unless component
             ## has class "NetMovements", or "InternalMovementsNet"
             for (i in seq_along(datasets)) {
                 dataset <- datasets[[i]]
                 has.negative <- any(dataset[!is.na(dataset)] < 0L)
                 if (has.negative) {
                     is.net <- FALSE
                     i.comp <- seriesIndices[i]
                     is.net <- ((i.comp > 0L)
                         && (methods::is(components[[i.comp]], "NetMovements")
                             || methods::is(components[[i.comp]], "InternalMovementsNet")))
                     if (!is.net)
                         return(gettextf("dataset '%s' has negative values (and associated demographic series does not have 'net' format)",
                                         namesDatasets[i]))
                 }
             }
             TRUE
         })


setClass("CombinedAccountMovementsHasAge",
         prototype = prototype(iMethodCombined = 10L),
         contains = c("CombinedAccountMovements",
                      "MovementsAgeMixin"))


