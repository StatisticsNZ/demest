
## HAS_TESTS
setClass("HasExposure",
         slots = c(exposure = "Counts"),
         contains = "VIRTUAL",
         validity = function(object) {
             model <- object@model
             y <- object@y
             exposure <- object@exposure
             ## 'exposure' is missing only if 'y' is
             if (any(is.na(exposure) > is.na(y)))
                 return(gettextf("'%s' has missing values where '%s' does not",
                                 "exposure", "y"))
             ## 'exposure' non-negative
             if (any(exposure < 0, na.rm = TRUE))
                 return(gettextf("'%s' has negative values",
                                 "exposure"))
             ## 'exposure' and 'y' have identical metadata
             if (!identical(exposure@metadata, y@metadata))
                 return(gettextf("'%s' and '%s' have different metadata",
                                 "exposure", "y"))
             ## y is 0 if exposure is 0
             if (any((y[!is.na(y)] > 0) & (exposure[!is.na(y)] == 0)))
                 return(gettextf("%s but %s for some cells",
                                 "y > 0", "exposure == 0"))
             ## 'model' has class "UseExposure"
             if (!methods::is(model, "UseExposure"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "model", class(model)))
             TRUE
         })

## HAS_TESTS
setClass("NotHasExposure",
         contains = "VIRTUAL",
         validity = function(object) {
             model <- object@model
             ## 'model' has class "NotUseExposure"
             if (!methods::is(model, "NotUseExposure"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "model", class(model)))
             TRUE
         })

## HAS_TESTS
setClass("Y",
         slots = c(y = "DemographicArray"),
         contains = "VIRTUAL",
         validity = function(object) {
             y <- object@y
             if (!is.null(y)) {
                 ## 'y' does not have iteration dimension
                 if (any(dembase::dimtypes(y) == "iteration"))
                     return(gettextf("'%s' has dimension with dimtype \"%s\"",
                                     "y", "iteration"))
                 ## 'y' does not have quantile dimension
                 if (any(dembase::dimtypes(y) == "quantile"))
                     return(gettextf("'%s' has dimension with dimtype \"%s\"",
                                     "y", "quantile"))
             }
             TRUE
         })

## HAS_TESTS
setClass("YCounts",
         contains = c("Y", "VIRTUAL"),
         validity = function(object) {
             y <- object@y
             ## 'y' has class "Counts"
             if (!methods::is(y, "Counts"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "y", class(y)))
             ## 'y' has type "integer"
             if (!is.integer(y))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "y", "integer"))
             TRUE
         })

## HAS_TESTS
setClass("YNonNegativeCounts",
         contains = c("YCounts", "VIRTUAL"),
         validity = function(object) {
             y <- object@y
             ## 'y' non-negative
             if (any(y < 0L, na.rm = TRUE))
                 return(gettextf("'%s' has negative values",
                                 "y"))
             TRUE
         })

## HAS_TESTS
setClass("NotHasSubtotals",
         contains = "VIRTUAL",
         validity = function(object) {
             model <- object@model
             y <- object@y
             if (methods::is(y, "HasSubtotals"))
                 return(gettextf("'%s' has class \"%s\" but '%s' has subtotals",
                                 "model", class(model), "y"))
             TRUE
         })

## HAS_TESTS
setClass("Observation",
         slots = c(observation = "list",
                   datasets = "list",
                   namesDatasets = "character",
                   transforms = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             observation <- object@observation
             datasets <- object@datasets
             namesDatasets <- object@namesDatasets
             transforms <- object@transforms
             hasMissing <- function(x) any(is.na(x))
             hasNegative <- function(x) any(x[!is.na(x)] < 0)
             dimBefore <- function(x) x@dimBefore
             ## 'observation' has at least one element
             if (identical(length(observation), 0L))
                 return(gettextf("'%s' has length %d",
                                 "observation", 0L))
             ## all elements of 'observation' have class "Model"
             if (!all(sapply(observation, is, "Model")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "observation", "Model"))
             ## all elements of 'observation' have class "UseExposure"
             if (!all(sapply(observation, is, "UseExposure")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "observation", "UseExposure"))
             ## 'observation' does not have names
             if (!is.null(names(observation)))
                 return(gettextf("'%s' has names",
                                 "observation"))
             ## all elements of 'datasets' have class "Counts"
             if (!all(sapply(datasets, is, "Counts")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "datasets", "Counts"))
             ## all elements of 'datasets' have type "integer"
             if (!all(sapply(datasets, is.integer)))
                 return(gettextf("'%s' has elements not of type \"%s\"",
                                 "datasets", "integer"))
             ## all elements of "datasets' are non-negative
             if (any(sapply(datasets, hasNegative)))
                 return(gettextf("'%s' has elements with negative values",
                                 "datasets"))
             ## 'datasets' does not have names
             if (!is.null(names(datasets)))
                 return(gettextf("'%s' has names",
                                 "datasets"))
             ## 'namesDatasets' has no missing values
             if (hasMissing(namesDatasets))
                 return(gettextf("'%s' has missing values",
                                 "namesDatasets"))
             ## 'namesDatasets' has no blanks
             if (!all(nzchar(namesDatasets)))
                 return(gettextf("'%s' has blanks",
                                 "namesDatasets"))
             ## 'namesDatasets' has no duplicates
             if (any(duplicated(namesDatasets)))
                 return(gettextf("'%s' has duplicates",
                                 "namesDatasets"))
             ## all elements of 'transforms' have class "CollapseTransformExtra"
             if (!all(sapply(transforms, is, "CollapseTransformExtra")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "transforms", "CollapseTransformExtra"))
             ## all elements of 'transforms' have same 'dimBefore'
             if (length(transforms) > 1L) {
                 dim.before.first <- transforms[[1L]]@dimBefore
                 dim.before.other <- lapply(transforms[-1L], dimBefore)
                 if (!all(mapply(identical, x = list(dim.before.first), y = dim.before.other)))
                     return(gettextf("elements of '%s' do not all have same '%s'",
                                     "transforms", "dimBefore"))
             }
             ## 'transforms' does not have names
             if (!is.null(names(transforms)))
                 return(gettextf("'%s' has names",
                                 "transforms"))
             ## 'observation' and 'datasets' have same length
             if (!identical(length(observation), length(datasets)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "observation", "datasets"))
             ## 'observation' and 'namesDatasets' have same length
             if (!identical(length(observation), length(namesDatasets)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "observation", "namesDatasets"))
             ## 'observation' and 'transforms' have same length
             if (!identical(length(observation), length(transforms)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "observation", "transforms"))
             ## 'transforms' have 'dimAfter' consistent with datasets
             for (i in seq_along(datasets)) {
                 if (!identical(dim(datasets[[i]]), transforms[[i]]@dimAfter))
                     return(gettextf("'%s' and '%s' for \"%s\" inconsistent",
                                     "dataset", "transform", namesDatasets[i]))
             }
             TRUE
         })


setClass("IMethodCombined",
         slots = c(iMethodCombined = "integer"),
         contains = "VIRTUAL")
