

## VIRTUAL CLASSES ###########################################################

## HAS_TESTS
setClass("Skeleton",
         contains = "VIRTUAL")


setClass("SkeletonDemographic",
         contains = "VIRTUAL")

## HAS_TESTS
setClass("SkeletonMetadata",
         slots = c(metadata = "MetaData"),
         contains = "VIRTUAL",
         validity = function(object) {
             metadata <- object@metadata
             ## 'metadata' does not have iteration or quantile dimensions
             dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
             for (dimtype in c("iteration", "quantile"))
                 if (dimtype %in% dimtypes)
                     return(gettextf("'%s' has dimension with dimtype \"%s\"",
                                     "metadata", dimtype))
             TRUE
         })


setClass("SkeletonMetadata0",
         slots = c(metadata0 = "MetaDataOrNULL"),
         contains = "VIRTUAL")


## HAS_TESTS
setClass("SkeletonMetadataIncl0",
         slots = c(metadataIncl0 = "MetaData"),
         contains = "VIRTUAL",
         validity = function(object) {
             metadataIncl0 <- object@metadataIncl0
             metadata <- object@metadata
             iAlong <- object@iAlong
             dimtypes <- dembase::dimtypes(metadataIncl0, use.names = FALSE)
             ## 'metadata' does not have iteration or quantile dimensions
             for (dimtype in c("iteration", "quantile"))
                 if (dimtype %in% dimtypes)
                     return(gettextf("'%s' has dimension with dimtype \"%s\"",
                                     "metadata", dimtype))
             TRUE
         })


## HAS_TESTS
setClass("SkeletonFirst",
         slots = c(first = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             first <- object@first
             ## 'first' has length 1
                 if (!identical(length(first), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     "first", 1L))
             ## 'first' is not missing
                 if (is.na(first))
                     return(gettextf("'%s' is missing",
                                     "first"))
             ## 'first' is positive
             if (first < 1L)
                 return(gettextf("'%s' is less than %d",
                               "first", 1L))
             TRUE
         })

setClass("SkeletonOne",
         contains = c("VIRTUAL", "Skeleton", "SkeletonFirst"))

## HAS_TESTS
setClass("SkeletonMany",
         slots = c(last = "integer"),
         contains = c("VIRTUAL", "Skeleton", "SkeletonFirst"),
         validity = function(object) {
             first <- object@first
             last <- object@last
             ## 'last' has length 1
                 if (!identical(length(last), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     "last", 1L))
             ## 'last' is not missing
                 if (is.na(last))
                     return(gettextf("'%s' is missing",
                                     "last"))
             ## 'last' >= 'first'
             if (last < first)
                 return(gettextf("'%s' is less than '%s'",
                               "last", "first"))
             TRUE
         })

## NO_TESTS
setClass("SkeletonIndices0",
         slots = c(indices0 = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             indices0 <- object@indices0
             first <- object@first
             last <- object@last
             ## 'indices0' has no missing values
             if (any(is.na(indices0)))
                 return(gettextf("'%s' has missing values",
                               "indices0"))
             ## 'indices0' has not duplicates
             if (any(duplicated(indices0)))
                 return(gettextf("'%s' has duplicates",
                               "indices0"))
             ## 'indices0' within valid range
             valid.range <- seq_len(last - first + 1L)
             if (!all(indices0 %in% valid.range))
                 return(gettextf("'%s' has elements outside valid range",
                                 "indices0"))
             TRUE
         })


## HAS_TESTS
setClass("SkeletonIndicesShow",
         slots = c(indicesShow = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             indicesShow <- object@indicesShow
             first <- object@first
             last <- object@last
             ## 'indicesShow' has no missing values
             if (any(is.na(indicesShow)))
                 return(gettextf("'%s' has missing values",
                               "indicesShow"))
             ## 'indicesShow' has not duplicates
             if (any(duplicated(indicesShow)))
                 return(gettextf("'%s' has duplicates",
                               "indicesShow"))
             ## 'indicesShow' within valid range
             valid.range <- seq_len(last - first + 1L)
             if (!all(indicesShow %in% valid.range))
                 return(gettextf("'%s' has elements outside valid range",
                                 "indicesShow"))
             TRUE
         })            

## HAS_TESTS
setClass("SkeletonOffsetsTheta",
         slots = c(offsetsTheta = "Offsets"),
         contains = "VIRTUAL",
         validity = function(object) {
             data <- object@data
             offsetsTheta <- object@offsetsTheta
             ## 'data' and 'offsetsTheta' consistent
             if (!identical(length(data), offsetsTheta[2L] - offsetsTheta[1L] + 1L))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "data", "offsetsTheta"))
             TRUE
         })

## HAS_TESTS
setClass("SkeletonExposure",
         slots = c(exposure = "Counts"),
         contains = "VIRTUAL",
         validity = function(object) {
             data <- object@data
             exposure <- object@exposure
             ## 'data' and 'exposure' have same metadata
             if (!identical(data@metadata, exposure@metadata))
                 return(gettextf("'%s' and '%s' have different metadata",
                                 "data", "exposure"))
             TRUE
         })

## HAS_TESTS
setClass("SkeletonW",
         slots = c(w = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             data <- object@data
             w <- object@w
             ## 'data' and 'w' have same length
             if (!identical(length(data), length(w)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "data", "w"))
             TRUE
         })

## HAS_TESTS
setClass("SkeletonHasSubtotals",
         contains = "VIRTUAL",
         validity = function(object) {
             data <- object@data
             ## 'data' has class "HasSubtotals"
             if (!methods::is(data, "HasSubtotals"))
                 return(gettextf("'%s' does not have class \"%s\"",
                                 "data", "HasSubtotals"))
             TRUE
         })

## HAS_TESTS
setClass("SkeletonMissingData",
         slots = c(data = "DemographicArray"),
         contains = c("VIRTUAL", "Skeleton"),
         validity = function(object) {
             data <- object@data
             ## 'data' has missing values
             if (!any(is.na(data)))
                 return(gettextf("'%s' has no missing values",
                                 "data"))
             TRUE
         })

## HAS_TESTS
setClass("SkeletonMissingDataNormal",
         contains = c("VIRTUAL", "SkeletonMissingData",
             "SkeletonOffsetsTheta", "SkeletonW"))

## HAS_TESTS
setClass("SkeletonMissingDataset",
         slots = c(offsetsComponent = "Offsets",
                   transformComponent = "CollapseTransform"),
         contains = c("VIRTUAL", "SkeletonMissingData"),
         validity = function(object) {
             offsetsComponent <- object@offsetsComponent
             transformComponent <- object@transformComponent
             ## 'offsetsComponent' consistent with 'transformComponent'
             if (!identical(as.integer(prod(transformComponent@dimBefore)),
                            offsetsComponent[2L] - offsetsComponent[1L] + 1L))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "offsetsComponent", "transformComponent"))
             TRUE
         })

## HAS_TESTS
setClass("SkeletonIndicesStrucZero",
         slots = c(indicesStrucZero = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             indicesStrucZero <- object@indicesStrucZero
             metadata <- object@metadata
             ## 'indicesStrucZero' has no missing values
             if (any(is.na(indicesStrucZero)))
                 return(gettextf("'%s' has missing values",
                                 "indicesStrucZero"))
             ## 'indicesStrucZero' has no duplicates
             if (any(duplicated(indicesStrucZero)))
                 return(gettextf("'%s' has duplicates",
                                 "indicesStrucZero"))
             ## 'indicesStrucZero' picks out indices of array
             ## specified by 'metadata'
             s <- seq_len(prod(dim(metadata)))
             if (!all(indicesStrucZero %in% s))
                 return(gettextf("'%s' outside valid range",
                                 "indicesStrucZero"))
             TRUE
         })



setClass("SkeletonMeanSD",
         slots = c(mean = "ParameterVector",
                   sd = "ScaleVec"),
         contains = "VIRTUAL",
         validity = function(object) {
             mean <- object@mean@.Data
             sd <- object@sd@.Data
             metadata <- object@metadata
             ## 'mean' and 'sd' have same length
             if (!identical(length(mean), length(sd)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mean", "sd"))
             ## length of 'mean' consistent with 'metadata'
             if (!identical(length(mean), as.integer(prod(dim(metadata)))))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "mean", "metadata"))
             TRUE
         })




## NON-VIRTUAL CLASSES ########################################################

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonOneCounts",
         contains = c("SkeletonOne", "SkeletonDemographic"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonOneValues",
         contains = c("SkeletonOne", "SkeletonDemographic"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonManyCounts",
         contains = c("SkeletonMany",
                      "SkeletonDemographic",
                      "SkeletonMetadata",
                      "SkeletonIndicesStrucZero"),
         validity = function(object) {
             metadata <- object@metadata
             first <- object@first
             last <- object@last
             ## dim(metadata) consistent with 'first', 'last'
             ## check 'implied.length' valid first, to avoid confusing error messages
             implied.length <- last - first + 1L
             implied.length.valid <- identical(length(implied.length), 1L) && !is.na(implied.length)
             if (implied.length.valid) {
                 if (!identical(as.integer(prod(dim(metadata))), implied.length))
                     return(gettextf("'%s', '%s', and '%s' inconsistent",
                                     "metadata", "first", "last"))
             }
             TRUE
         })

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonManyValues",
         contains = c("SkeletonMany",
                      "SkeletonDemographic",
                      "SkeletonMetadata",
                      "SkeletonIndicesStrucZero"),
         prototype = prototype(indicesStrucZero = integer()),
         validity = function(object) {
             metadata <- object@metadata
             first <- object@first
             last <- object@last
             ## dim(metadata) consistent with 'first', 'last'
             ## check 'implied.length' valid first, to avoid confusing error messages
             implied.length <- last - first + 1L
             implied.length.valid <- identical(length(implied.length), 1L) && !is.na(implied.length)
             if (implied.length.valid) {
                 if (!identical(as.integer(prod(dim(metadata))), implied.length))
                     return(gettextf("'%s', '%s', and '%s' inconsistent",
                                     "metadata", "first", "last"))
             }
             TRUE
         })

## HAS_TESTS
## HAS_FETCH
## include "last" slot because expected by functions such as 'overwriteValuesOnFile'
setClass("SkeletonBetaIntercept",
         slots = c(last = "integer"),
         contains = "SkeletonOneValues",
         validity = function(object) {
             first <- object@first
             last <- object@last
             ## 'last' has length 1
                 if (!identical(length(last), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     "last", 1L))
             ## 'last' is not missing
                 if (is.na(last))
                     return(gettextf("'%s' is missing",
                                     "last"))
             ## 'last' == 'first'
             if (last != first)
                 return(gettextf("'%s' does not equal '%s'",
                               "last", "first"))
             TRUE
         })

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonBetaTerm",
         contains = "SkeletonManyValues")

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMu",
         slots = c(margins = "list",
                   offsets = "list"),
         contains = c("Skeleton",
                      "SkeletonMetadata",
                      "Margins",
                      "SkeletonIndicesStrucZero"),
         prototype = prototype(indicesStrucZero = integer()),
         validity = function(object) {
             margins <- object@margins
             offsets <- object@offsets
             ## all elements of 'offsets' have class "Offsets"
             if (!all(sapply(offsets, is, "Offsets")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "offsets", "Offsets"))
             ## 'offsets' and 'margins' have same length
             if (!identical(length(offsets), length(margins)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "margins", "offsets"))
             TRUE
         })         

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonCovariates",
         slots = c(metadata = "MetaData"),
         contains = "SkeletonMany", 
         validity = function(object) {
             first <- object@first
             last <- object@last
             metadata <- object@metadata
             ## 'metadata' has only one dimension
             if (!identical(length(dim(metadata)), 1L))
                 return(gettextf("'%s' has more than one dimension",
                                 "metadata"))
             ## dimension has dimtype "state"
             if (!identical(dembase::dimtypes(metadata, use.names = FALSE), "state"))
                 return(gettextf("dimension does not have dimtype \"%s\"",
                                 "state"))
             ## dim(metadata) consistent with 'first', 'last',
             ## allowing for fact that metadata one element shorter
             if (!identical(dim(metadata), last - first))
                 return(gettextf("'%s', '%s', and '%s' inconsistent",
                                 "metadata", "first", "last"))
             TRUE
         })

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonStateDLM",
         contains = c("SkeletonMany",
                      "SkeletonIndices0",
                      "SkeletonIndicesShow",
                      "SkeletonMetadata",
                      "SkeletonMetadata0",
                      "SkeletonMetadataIncl0",
                      "SkeletonIndicesStrucZero",
                      "IAlongMixin"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonAccept",
         slots = c(iFirstInChain = "integer"),
         contains = "SkeletonOne",
         validity = function(object) {
             iFirstInChain <- object@iFirstInChain
             ## 'iFirstInChain' has positive length
             if (identical(length(iFirstInChain), 0L))
                 return(gettextf("'%s' has length %d",
                                 "iFirstInChain", 0L))
             ## 'iFirstInChain' has no missing values
             if (any(is.na(iFirstInChain)))
                 return(gettextf("'%s' has missing values",
                                 "iFirstInChain"))
             ## 'iFirstInChain' has not non-positive values
             if (any(iFirstInChain < 1L))
                 return(gettextf("'%s' has values less than %d",
                                 "iFirstInChain", 1L))
             ## 'iFirstInChain' has no duplicates
             if (any(duplicated(iFirstInChain)))
                 return(gettextf("'%s' has duplicates",
                                 "iFirstInChain"))
             TRUE
         })

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonNAccept",
         slots = c(nAttempt = "integer"),
         contains = "SkeletonAccept",
         validity = function(object) {
             nAttempt <- object@nAttempt
             ## 'nAttempt' has length 1
             if (!identical(length(nAttempt), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "nAttempt", 1L))
             ## 'nAttempt' is not missing
             if (is.na(nAttempt))
                 return(gettextf("'%s' is missing",
                                 "nAttempt"))
             ## 'nAttempt' is positive
             if (nAttempt < 1L)
                 return(gettextf("'%s' is less than %d",
                                 "nAttempt", 1L))
             TRUE
         })

## DemographicArray objects with missing data

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDataNormalVarsigmaKnown",
         contains = c("SkeletonMissingDataNormal", "VarsigmaMixin"))
             
## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDataNormalVarsigmaUnknown",
         slots = c(offsetsVarsigma = "Offsets"),
         contains = "SkeletonMissingDataNormal",
         validity = function(object) {
             offsetsVarsigma <- object@offsetsVarsigma
             ## 'offsetsVarsigma' imply 'varsigma' has length 1
             if (!identical(offsetsVarsigma[1L], offsetsVarsigma[2L]))
                 return(gettextf("'%s' implies '%s' does not have length %d",
                                 "offsetsVarsigma", "varsigma", 1L))
             TRUE
         })

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDataPoissonNotUseExp",
         contains = c("SkeletonMissingData", "SkeletonOffsetsTheta"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDataPoissonNotUseExpSubtotals",
         contains = c("SkeletonMissingDataPoissonNotUseExp", "SkeletonHasSubtotals"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDataPoissonUseExp",
         contains = c("SkeletonMissingData", "SkeletonOffsetsTheta", "SkeletonExposure"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDataPoissonUseExpSubtotals",
         contains = c("SkeletonMissingDataPoissonUseExp", "SkeletonHasSubtotals"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDataBinomial",
         contains = c("SkeletonMissingData", "SkeletonOffsetsTheta", "SkeletonExposure"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDatasetPoisson",
         contains = c("SkeletonMissingDataset", "SkeletonOffsetsTheta"),
         validity = function(object) {
             data <- object@data
             transformComponent <- object@transformComponent
             ## 'data' consistent with 'transformComponent'
             if (!identical(transformComponent@dimAfter, dim(data)))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "data", "transformComponent"))
             TRUE
         })

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDatasetPoissonSubtotals",
         contains = c("SkeletonMissingDatasetPoisson", "SkeletonHasSubtotals"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDatasetBinomial",
         contains = c("SkeletonMissingDataset", "SkeletonOffsetsTheta"),
         validity = function(object) {
             data <- object@data
             transformComponent <- object@transformComponent
             ## 'data' consistent with 'transformComponent'
             if (!identical(transformComponent@dimAfter, dim(data)))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "data", "transformComponent"))
             TRUE
         })

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDatasetPoissonBinomial",
         contains = c("SkeletonMissingDataset", "Prob"))

## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDatasetRound3",
         contains = "SkeletonMissingDataset")


## HAS_TESTS
## HAS_FETCH
setClass("SkeletonMissingDatasetNormalFixedUseExp",
         contains = c("SkeletonMissingDataset",
                      "SkeletonMetadata",
                      "SkeletonMeanSD"))




setClassUnion("DemographicOrSkeletonMissingData",
              members = c("DemographicArray", "SkeletonMissingData"))


