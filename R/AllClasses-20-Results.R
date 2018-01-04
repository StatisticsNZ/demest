
## HAS_TESTS
setClass("Results",
         slots = c(control = "list",
                   final = "list",
                   seed = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             kNamesControl <- c("call", "parallel", "lengthIter")
             control <- object@control
             final <- object@final
             seed <- object@seed
             ## control has required names
             if (!all(kNamesControl %in% names(control)))
                 return(gettextf("'%s' does not have required names",
                                 "control"))
             ## control has no missing values
             for (name in setdiff(kNamesControl, "call"))
                 if (any(is.na(control[[name]])))
                     return(gettextf("'%s' is missing",
                                     name))
             ## TODO - ADD TESTS FOR CALL (ALLOWING FOR continueEstimation)
             ## parallel is logical
             if (!is.logical(control$parallel))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "parallel", "logical"))
             ## lengthIter is integer
             if (!is.integer(control$lengthIter))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "lengthIter", "integer"))
             if (control$parallel) {
                 ## valid L'Ecuyer seeds
                 for (i in seq_along(seed)) {
                     if (!identical(seed[[i]][1L], 407L) || !identical(length(seed[[i]]), 7L))
                         return(gettextf("element %d of '%s' is not a valid %s seed",
                                         i, "seed", "L'Ecuyer"))
                 }
             }
             else {
                 ## length of seed equal to 1
                 if (!identical(length(seed), 1L))
                     return(gettextf("'%s' is FALSE but length of '%s' is not equal to %d",
                                     "parallel", "seed", 1L))
             }
             ## all elements of 'final' have same class
             if (length(final) > 1L) {
                 classes.final <- sapply(final, class)
                 if (!all(mapply(identical, x = classes.final[1L], y = classes.final[-1L])))
                     return(gettextf("elements of '%s' have different classes",
                                     "final"))
             }
             ## lengthIter consistent with final
             if (!identical(control$lengthIter, length(extractValues(final[[1L]]))))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "lengthIter", "final"))
             TRUE
         })

## HAS_TESTS
setClass("ResultsEst",
         slots = c(mcmc = "integer"),
         contains = c("VIRTUAL", "Results"),
         validity = function(object) {
             kNamesMCMC <- c("nBurnin", "nSim", "nChain", "nThin", "nIteration")
             kNonNegative <- c("nBurnin", "nSim", "nIteration")
             mcmc <- object@mcmc
             seed <- object@seed
             control <- object@control
             final <- object@final
             nSim <- mcmc[["nSim"]]
             ## 'control' has "nUpdateMax"
             if (!("nUpdateMax" %in% names(control)))
                 return(gettextf("'%s' does not include '%s'",
                                 "control", "nUpdateMax"))
             ## mcmc has correct names
             if (!identical(names(mcmc), kNamesMCMC))
                 return(gettextf("'%s' has incorrect names",
                                 "mcmc"))
             ## mcmc has no missing values
             for (name in kNamesMCMC)
                 if (is.na(mcmc[[name]]))
                     return(gettextf("'%s' is missing",
                                     name))
             ## elements of mcmc that should be non-negative are non-negative
             for (name in kNonNegative)
                 if (mcmc[[name]] < 0L)
                     return(gettextf("'%s' is negative",
                                     name))
             ## elements of mcmc that should be positive are positive
             for (name in setdiff(kNamesMCMC, kNonNegative))
                 if (mcmc[[name]] < 1L)
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
             ## nThin <= nSim if nSim > 0L
             if ((nSim > 0L) && (mcmc[["nThin"]] > mcmc[["nSim"]]))
                 return(gettextf("'%s' is greater than '%s'",
                                 "nThin", "nSim"))
             ## nIteration == (nSim %/% nThin) * nChain
             if (mcmc[["nIteration"]] != (nSim %/% mcmc[["nThin"]]) * mcmc[["nChain"]])
                 return(gettextf("'%s', '%s', '%s', and '%s' inconsistent",
                                 "nIteration", "nSim", "nThin", "nChain"))
             ## if parellel is TRUE, length of seed equal to nChain
             if (control$parallel) {
                 if (!identical(length(seed), mcmc[["nChain"]]))
                     return(gettextf("'%s' is TRUE but length of '%s' is not equal to '%s'",
                                     "parallel", "seed", "nChain"))
             }
             ## length of final equal to nChain
             if (!identical(length(final), mcmc[["nChain"]]))
                 return(gettextf("length of '%s' not equal to '%s'",
                                 "final", "nChain"))
             TRUE
         })

## HAS_TESTS
setClass("ResultsPred",
         slots = c(mcmc = "integer"),
         contains = c("VIRTUAL", "Results"),
         validity = function(object) {
             kNamesMCMC <- "nIteration"
             kNonNegative <- "nIteration"
             mcmc <- object@mcmc
             ## mcmc has correct names
             if (!identical(names(mcmc), kNamesMCMC))
                 return(gettextf("'%s' has incorrect names",
                                 "mcmc"))
             ## mcmc has no missing values
             for (name in kNamesMCMC)
                 if (is.na(mcmc[[name]]))
                     return(gettextf("'%s' is missing",
                                     name))
             ## elements of mcmc that should be non-negative are non-negative
             for (name in kNonNegative)
                 if (mcmc[[name]] < 0L)
                     return(gettextf("'%s' is negative",
                                     name))
             ## elements of mcmc that should be positive are positive
             for (name in setdiff(kNamesMCMC, kNonNegative))
                 if (mcmc[[name]] < 1L)
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
             TRUE
         })

## HAS_TESTS
setClass("ResultsModelEst",
         slots = c(model = "list",
                        y = "DemographicOrSkeletonMissingData"),
         contains = "ResultsEst",
         validity = function(object) {
             mcmc <- object@mcmc
             model <- object@model
             nSim <- mcmc[["nSim"]]
             final <- object@final
             ## model is empty list iff nSim is 0
             if (identical(nSim, 0L) && !identical(model, list()))
                 return(gettextf("'%s' is 0 but '%s' is not an empty list",
                                 "nSim", "model"))
             if (!identical(nSim, 0L) && identical(model, list()))
                 return(gettextf("'%s' is not 0 but '%s' is an empty list",
                                 "nSim", "model"))
             ## all elements of final have class "CombinedModel"
             if (!all(sapply(final, is, "CombinedModel")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "final", "CombinedModel"))
             TRUE
         })

## HAS_TESTS
## repeat arguments 'model' and 'y' to make
## sure the slots are listed in this order
setClass("ResultsModelExposureEst",
         slots = c(model = "list",
                        y = "DemographicOrSkeletonMissingData",
                        exposure = "Counts"),
         contains = "ResultsModelEst")

## HAS_TESTS
setClass("ResultsModelPred",
         slots = c(model = "list"),
         contains = "ResultsPred",
         validity = function(object) {
             mcmc <- object@mcmc
             model <- object@model
             final <- object@final
             ## all elements of final have class "CombinedModel"
             if (!all(sapply(final, is, "CombinedModel")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "final", "CombinedModel"))
             TRUE
         })

setClassUnion("ResultsModel",
              members = c("ResultsModelEst", "ResultsModelPred"))

## HAS_TESTS
setClass("ResultsCountsEst",
         slots = c(model = "list",
                   y = "Skeleton",
                   dataModels = "list",
                   datasets = "list"),
         contains = "ResultsEst",
         validity = function(object) {
             model <- object@model
             final <- object@final
             dataModels <- object@dataModels
             datasets <- object@datasets
             mcmc <- object@mcmc
             nSim <- mcmc[["nSim"]]
             ## model is empty list iff nSim is 0
             if (identical(nSim, 0L) && !identical(model, list()))
                 return(gettextf("'%s' is 0 but '%s' is not an empty list",
                                 "nSim", "model"))
             if (!identical(nSim, 0L) && identical(model, list()))
                 return(gettextf("'%s' is not 0 but '%s' is an empty list",
                                 "nSim", "model"))
             ## dataModels is empty list iff nSim is 0
             if (identical(nSim, 0L) && !identical(dataModels, list()))
                 return(gettextf("'%s' is 0 but '%s' is not an empty list",
                                 "nSim", "dataModels"))
             if (!identical(nSim, 0L) && identical(dataModels, list()))
                 return(gettextf("'%s' is not 0 but '%s' is an empty list",
                                 "nSim", "dataModels"))
             ## all elements of 'final' have class "CombinedCounts"
             if (!all(sapply(final, is, "CombinedCounts")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "final", "CombinedCounts"))
             ## all elements of 'dataModels' have class "list"
             if (!all(sapply(dataModels, is.list)))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "dataModels", "list"))
             ## 'dataModels' has names
             if (is.null(names(dataModels)))
                 return(gettextf("'%s' does not have names",
                                 "dataModels"))
             ## all elements of 'datasets' have class "Counts" or "SkeletonMissingDataset"
             is.counts <- sapply(datasets, is, "Counts")
             is.skeleton <- sapply(datasets, is, "SkeletonMissingDataset")
             if (!all(is.counts | is.skeleton))
                 return(gettextf("'%s' has elements not of class \"%s\" or \"%s\"",
                                 "datasets", "Counts", "SkeletonMissingDataset"))
             ## if an element of 'dataset' has class "Counts" it does not have any missing values
             hasMissing <- function(x) any(is.na(x))
             has.missing <- sapply(datasets[sapply(datasets, is, "Counts")], hasMissing)
             if (any(has.missing))
                 return(gettextf("'%s' has elements of class \"%s\" with missing values",
                                 "datasets", "Counts"))             
             ## 'dataModels' and 'datasets' have same names
             if (!identical(names(dataModels), names(datasets)))
                 return(gettextf("'%s' and '%s' have different names",
                                 "dataModels", "datasets"))
             TRUE
         })

## HAS_TESTS
setClass("ResultsCountsExposureEst",
         slots = c(model = "list",
                   y = "Skeleton",
                   exposure = "Counts",
                   dataModels = "list",
                   datasets = "list"),
         contains = "ResultsCountsEst",
         validity = function(object) {
             y <- object@y
             exposure <- object@exposure
             ## 'y' and 'exposure' have identical metadata
             if (!identical(y@metadata, exposure@metadata))
                 return(gettextf("'%s' and '%s' have different %s",
                                 "y", "exposure", "metadata"))
             TRUE
         })

## NO_TESTS
setClass("ResultsAccount",
         slots = c(account = "list",
                   systemModels = "list",
                   dataModels = "list",
                   datasets = "list"),
         contains = "ResultsEst",
         validity = function(object) {
             account <- object@account
             systemModels <- object@systemModels
             dataModels <- object@dataModels
             datasets <- object@datasets
             ## 'account' and 'systemModels' have same length
             if (!identical(length(account), length(systemModels)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "account", "systemModels"))
             ## 'dataModels' and 'datasets' have same length
             if (!identical(length(account), length(systemModels)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "dataModels", "datasets"))
             TRUE
         })
         

