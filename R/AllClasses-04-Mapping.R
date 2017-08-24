
## HAS_TESTS
setClass("Mapping",
         slots = c(isOneToOne = "logical",
                   nSharedVec = "integer",
                   stepSharedCurrentVec = "integer",
                   stepSharedTargetVec = "integer"),
         prototype = prototype(isOneToOne = FALSE),
         contains = "VIRTUAL",
         validity = function(object) {
             isOneToOne <- object@isOneToOne
             nSharedVec <- object@nSharedVec
             stepSharedCurrentVec <- object@stepSharedCurrentVec
             stepSharedTargetVec <- object@stepSharedTargetVec
             ## isOneToOne has length 1
             if (!identical(length(isOneToOne), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "isOneToOne", 1L))
             ## isOneToOne not missing
             if (is.na(isOneToOne))
                 return(gettextf("'%s' is missing",
                                 "isOneToOne"))
             ## nSharedVec, stepSharedCurrentVec, stepSharedTargetVec,
             ## have no missing values
             for (name in c("nSharedVec", "stepSharedCurrentVec", "stepSharedTargetVec")) {
                 value <- methods::slot(object, name)
                 if (any(is.na(value)))
                     return(gettextf("'%s' has missing values",
                                     name))
             }
             ## nSharedVec, stepSharedCurrentVec, stepSharedTargetVec,
             ## all positive values
             for (name in c("nSharedVec", "stepSharedCurrentVec", "stepSharedTargetVec")) {
                 value <- methods::slot(object, name)
                 if (any(value < 1L))
                     return(gettextf("'%s' has non-positive values",
                                     name))
             }
             ## nSharedVec, stepSharedCurrentVec have same length
             if (!identical(length(nSharedVec), length(stepSharedCurrentVec)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nSharedVec", "stepSharedCurrentVec"))
             ## nSharedVec, stepSharedTargetVec have same length
             if (!identical(length(nSharedVec), length(stepSharedTargetVec)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nSharedVec", "stepSharedTargetVec"))
             TRUE
         })

## HAS_TESTS
setClass("MappingMixinTime",
         slots = c(nTimeCurrent = "integer",
                   stepTimeCurrent = "integer",
                   stepTimeTarget = "integer"),
         prototype = prototype(isOneToOne = FALSE),
         contains = "VIRTUAL",
         validity = function(object) {
             for (name in c("nTimeCurrent", "stepTimeCurrent", "stepTimeTarget")) {
                 value <- methods::slot(object, name)
                 ## nTimeCurrent, stepTimeCurrent, stepTimeTarget have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## nTimeCurrent, stepTimeCurrent, stepTimeTarget not missing
                 if (is.na(value))
                     return(gettextf("'%s' is missing",
                                     name))
             }             
             ## nTimeCurrent, stepTimeCurrent, stepTimeTarget positive
             for (name in c("nTimeCurrent", "stepTimeCurrent", "stepTimeTarget")) {
                 value <- methods::slot(object, name)
                 if (value < 1L)
                     return(gettextf("'%s' is non-positive",
                                     name))
             }
             TRUE
         })

## HAS_TESTS
setClass("MappingMixinAge",
         slots = c(hasAge = "logical",
                   nAgeCurrent = "integer",
                   stepAgeCurrent = "integer",
                   stepAgeTarget = "integer",
                   stepTriangleCurrent = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             hasAge <- object@hasAge
             ## hasAge, nAgeCurrent, stepAgeCurrent, stepAgeTarget have length 1
             for (name in c("hasAge", "nAgeCurrent", "stepAgeCurrent", "stepAgeTarget")) {
                 value <- methods::slot(object, name)
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
             }
             ## hasAge is not missing
             if (is.na(hasAge))
                 return(gettextf("'%s' is missing",
                                 "hasAge"))
             if (hasAge) {
                 ## if hasAge: nAgeCurrent, stepAgeCurrent, stepAgeTarget not missing
                 for (name in c("nAgeCurrent", "stepAgeCurrent", "stepAgeTarget")) {
                     value <- methods::slot(object, name)
                     if (is.na(value))
                         return(gettextf("'%s' is missing",
                                         name))
                 }
                 ## if hasAge: nAgeCurrent, stepAgeCurrent, stepAgeTarget positive
                 for (name in c("nAgeCurrent", "stepAgeCurrent", "stepAgeTarget")) {
                     value <- methods::slot(object, name)
                     if (value < 1L)
                         return(gettextf("'%s' is non-positive",
                                         name))
                 }
                 ## if hasAge: stepAge not in stepShared
                 for (type in c("Current", "Target")) {
                     name.age <- sprintf("stepAge%s", type)
                     name.shared.vec <- sprintf("stepShared%sVec", type)
                     step.age <- methods::slot(object, name.age)
                     step.shared.vec <- methods::slot(object, name.shared.vec)
                     if (step.age %in% step.shared.vec)
                         stop(gettextf("overlap between '%s' and '%s'",
                                       name.age, name.shared.vec))
                 }
             }
             else {
                 ## if not hasAge: nAgeCurrent, stepAgeCurrent, stepAgeTarget missing
                 for (name in c("nAgeCurrent", "stepAgeCurrent", "stepAgeTarget")) {
                     value <- methods::slot(object, name)
                     if (!is.na(value))
                         return(gettextf("'%s' is %s but '%s' is not missing",
                                         "hasAge", FALSE, name))
                 }
             }
             TRUE
         })

## HAS_TESTS
setClass("MappingMixinStepTriangleCurrent",
         slots = c(stepTriangleCurrent = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             stepTriangleCurrent <- object@stepTriangleCurrent
             hasAge <- object@hasAge
             ## 'stepTriangleCurrent' has length 1
             if (!identical(length(stepTriangleCurrent), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "stepTriangleCurrent", 1L))
             ## 'hasAge' is not missing
             if (is.na(hasAge))
                 return(gettextf("'%s' is missing",
                                 "hasAge"))
             if (hasAge) {
                 ## if hasAge: 'stepTriangleCurrent' not missing
                 if (is.na(stepTriangleCurrent))
                     return(gettextf("'%s' is missing",
                                     "stepTriangleCurrent"))
                 ## if hasAge: 'stepTriangleCurrent' positive
                 if (stepTriangleCurrent < 1L)
                     return(gettextf("'%s' is non-positive",
                                     "stepTriangleCurrent"))
             }
             else {
                 ## if not hasAge: 'stepTriangleCurrent' is missing
                 if (!is.na(stepTriangleCurrent))
                     return(gettextf("'%s' is %s but '%s' is not missing",
                                     "hasAge", "FALSE", "stepTriangleCurrent"))
             }
             TRUE
         })

## HAS_TESTS
setClass("MappingMixinStepTriangleTarget",
         slots = c(stepTriangleTarget = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             stepTriangleTarget <- object@stepTriangleTarget
             hasAge <- object@hasAge
             ## 'stepTriangleTarget' has length 1
             if (!identical(length(stepTriangleTarget), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "stepTriangleTarget", 1L))
             ## 'hasAge' is not missing
             if (is.na(hasAge))
                 return(gettextf("'%s' is missing",
                                 "hasAge"))
             if (hasAge) {
                 ## if hasAge: 'stepTriangleTarget' not missing
                 if (is.na(stepTriangleTarget))
                     return(gettextf("'%s' is missing",
                                     "stepTriangleTarget"))
                 ## if hasAge: 'stepTriangleTarget' positive
                 if (stepTriangleTarget < 1L)
                     return(gettextf("'%s' is non-positive",
                                     "stepTriangleTarget"))
             }
             else {
                 ## if not hasAge: 'stepTriangleTarget' is missing
                 if (!is.na(stepTriangleTarget))
                     return(gettextf("'%s' is %s but '%s' is not missing",
                                     "hasAge", "FALSE", "stepTriangleTarget"))
             }
             TRUE
         })

## HAS_TESTS
setClass("MappingMixinIMinAge",
         slots = c(iMinAge = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iMinAge <- object@iMinAge
             ## 'iMinAge' has length 1
             if (!identical(length(iMinAge), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "iMinAge", 1L))
             ## iMinAge positive if not missing
             if (!is.na(iMinAge) && iMinAge <= 1L)
                 return(gettextf("'%s' is non-positive",
                                 "iMinAge"))
             TRUE
         })


## NO_TESTS
setClass("MappingMixinNAgeTarget",
         slots = c(nAgeTarget = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             nAgeTarget <- object@nAgeTarget
             ## 'nAgeTarget' has length 1
             if (!identical(length(nAgeTarget), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "nAgeTarget", 1L))
             ## nAgeTarget positive if not missing
             if (!is.na(nAgeTarget) && nAgeTarget <= 1L)
                 return(gettextf("'%s' is non-positive",
                                 "nAgeTarget"))
             TRUE
         })



## HAS_TESTS
setClass("MappingMixinOrigDest",
         slots = c(nOrigDestVec = "integer",
                   stepOrigCurrentVec = "integer",
                   stepDestCurrentVec = "integer",
                   stepOrigDestTargetVec = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             nOrigDestVec <- object@nOrigDestVec
             stepOrigCurrentVec <- object@stepOrigCurrentVec
             stepDestCurrentVec <- object@stepDestCurrentVec
             stepOrigDestTargetVec <- object@stepOrigDestTargetVec
             ## nOrigDestVec, stepOrigCurrentVec, stepDestCurrentVec, stepOrigDestTargetVec
             ## have no missing values
             for (name in c("nOrigDestVec", "stepOrigCurrentVec", "stepDestCurrentVec",
                            "stepOrigDestTargetVec")) {
                 value <- methods::slot(object, name)
                 if (any(is.na(value)))
                     return(gettextf("'%s' has missing values",
                                     name))
             }
             ## nOrigDestVec, stepOrigCurrentVec, stepDestCurrentVec, stepOrigDestTargetVec
             ## all positive values
             for (name in c("nOrigDestVec", "stepOrigCurrentVec", "stepDestCurrentVec",
                            "stepOrigDestTargetVec")) {
                 value <- methods::slot(object, name)
                 if (any(value < 1L))
                     return(gettextf("'%s' has non-positive values",
                                     name))
             }
             ## nOrigDestVec, stepOrigCurrentVec have same length
             if (!identical(length(nOrigDestVec), length(stepOrigCurrentVec)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nOrigDestVec", "stepOrigCurrentVec"))
             ## nOrigDestVec, stepDestCurrentVec have same length
             if (!identical(length(nOrigDestVec), length(stepDestCurrentVec)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nOrigDestVec", "stepDestCurrentVec"))
             ## nOrigDestVec, stepOrigDestTargetVec have same length
             if (!identical(length(nOrigDestVec), length(stepOrigDestTargetVec)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nOrigDestVec", "stepOrigDestTargetVec"))
             TRUE
         })

## Mappings to population

setClass("MappingToPopn",
         contains = c("VIRTUAL",
                      "Mapping",
                      "MappingMixinTime",
                      "MappingMixinAge",
                      "MappingMixinStepTriangleCurrent"))

## HAS_TESTS
setClass("MappingCompToPopn",
         contains = "MappingToPopn")

## HAS_TESTS
setClass("MappingOrigDestToPopn",
         contains = c("MappingToPopn",
                      "MappingMixinOrigDest"))


## Mappings to accession

setClass("MappingToAcc",
         contains = c("VIRTUAL",
                      "Mapping",
                      "MappingMixinTime",
                      "MappingMixinAge",
                      "MappingMixinStepTriangleCurrent"))

## HAS_TESTS
setClass("MappingCompToAcc",
         contains = "MappingToAcc")

## HAS_TESTS
setClass("MappingOrigDestToAcc",
         contains = c("MappingToAcc",
                      "MappingMixinOrigDest"))


## Mappings to exposure

setClass("MappingToExp",
         contains = c("VIRTUAL",
                      "Mapping",
                      "MappingMixinTime",
                      "MappingMixinAge",
                      "MappingMixinStepTriangleCurrent",
                      "MappingMixinStepTriangleTarget"))

## HAS_TESTS
setClass("MappingCompToExp",
         contains = "MappingToExp")

## HAS_TESTS
setClass("MappingBirthsToExp",
         contains = c("MappingToExp",
                      "MappingMixinIMinAge"))

## HAS_TESTS
setClass("MappingOrigDestToExp",
         contains = c("MappingToExp",
                      "MappingMixinOrigDest"))


## Mappings from Exposure

setClass("MappingFromExp",
         contains = c("VIRTUAL",
                      "Mapping"))

## NO_TESTS
setClass("MappingExpToComp",
         contains = "MappingFromExp")

## NO_TESTS
setClass("MappingExpToBirths",
         contains = c("MappingFromExp",
                      "MappingMixinTime",
                      "MappingMixinAge",
                      "MappingMixinIMinAge",
                      "MappingMixinNAgeTarget",
                      "MappingMixinStepTriangleCurrent",
                      "MappingMixinStepTriangleTarget"))

