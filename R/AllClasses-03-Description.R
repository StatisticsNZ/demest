

## Descriptions of components of account. Used when updating accounts.

## HAS_TESTS
setClass("Description",
         slots = c(nTime = "integer",
                   stepTime = "integer",
                   hasAge = "logical",
                   nAge = "integer",
                   stepAge = "integer",
                   length = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             nTime <- object@nTime
             hasAge <- object@hasAge
             length <- object@length
             ## nTime, stepTime, hasAge, nAge, stepAge, length have length 1
             for (name in c("nTime", "stepTime", "hasAge",
                            "nAge", "stepAge", "length")) {
                 value <- methods::slot(object, name)
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
             }
             ## nTime, stepTime, hasAge, length not missing
             for (name in c("nTime", "stepTime", "hasAge", "length")) {
                 value <- methods::slot(object, name)
                 if (is.na(value))
                     return(gettextf("'%s' is missing",
                                     name))
             }
             ## nTime, stepTime, length positive
             for (name in c("nTime", "stepTime", "length")) {
                 value <- methods::slot(object, name)
                 if (value < 1L)
                     return(gettextf("'%s' is non-positive",
                                     name))
             }
             if (hasAge) {
                 ## if hasAge: nAge, stepAge not missing
                 for (name in c("nAge", "stepAge")) {
                     value <- methods::slot(object, name)
                     if (is.na(value))
                         return(gettextf("'%s' is missing",
                                         name))
                 }
                 ## if hasAge: nAge, stepAge positive
                 for (name in c("nAge", "stepAge")) {
                     value <- methods::slot(object, name)
                     if (value < 1L)
                         return(gettextf("'%s' is non-positive",
                                         name))
                 }
             }
             else {
                 ## if not hasAge: nAge, stepAge both missing
                 for (name in c("nAge", "stepAge")) {
                     value <- methods::slot(object, name)
                     if (!is.na(value))
                         return(gettextf("'%s' is %s but '%s' is not missing",
                                         "hasAge", FALSE, name))
                 }
             }
             ## length >= nTime
             if (length < nTime)
                 return(gettextf("'%s' is less than '%s'",
                                 "length", "nTime"))
             TRUE
         })

## HAS_TESTS
setClass("DescriptionPopn",
         contains = "Description")

## HAS_TESTS
setClass("DescriptionComp",
         slots = c(stepTriangle = "integer"),
         contains = "Description",
         validity = function(object) {
             hasAge <- object@hasAge
             stepTriangle <- object@stepTriangle
             if (hasAge) {
                 ## 'stepTriangle' has length 1
                 if (!identical(length(stepTriangle), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     "stepTriangle", 1L))
                 ## 'stepTriangle' is not missing
                 if (is.na(stepTriangle))
                     return(gettextf("'%s' is missing",
                                     "stepTriangle"))
                 ## 'stepTriangle' positive
                 if (stepTriangle < 1L)
                     return(gettextf("'%s' is non-positive",
                                     "stepTriangle"))
             }
             else {
                 ## 'stepTriangle' is not missing
                 if (!is.na(stepTriangle))
                     return(gettextf("'%s' is %s but '%s' is not missing",
                                     "hasAge", FALSE, "stepTriangle"))
             }
             TRUE
         })

## HAS_TESTS
setClass("StepDirectionMixin",
         slots = c(stepDirection = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             stepDirection <- object@stepDirection
             ## 'stepDirection' has length 1
             if (!identical(length(stepDirection), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "stepDirection", 1L))
             ## 'stepDirection' is not missing
             if (is.na(stepDirection))
                 return(gettextf("'%s' is missing",
                                 "stepDirection"))
             ## 'stepDirection' positive
             if (stepDirection < 1L)
                 return(gettextf("'%s' is non-positive",
                                 "stepDirection"))
             TRUE
         })

## HAS_TESTS
setClass("BetweenWithinMixin",
         slots = c(nBetweenVec = "integer",
                   stepBetweenVec = "integer",
                   nWithinVec = "integer",
                   stepWithinVec = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             nTime <- object@nTime
             stepTime <- object@stepTime
             hasAge <- object@hasAge
             nAge <- object@nAge
             nBetweenVec <- object@nBetweenVec
             stepBetweenVec <- object@stepBetweenVec
             nWithinVec <- object@nWithinVec
             stepWithinVec <- object@stepWithinVec
             length <- object@length
             for (name in c("nBetweenVec", "stepBetweenVec", "nWithinVec", "stepWithinVec")) {
                 value <- methods::slot(object, name)
                 ## 'nBetweenVec', 'stepBetweenVec', 'nWithinVec', 'stepWithinVec'
                 ## all have positive length
                 if (identical(length(value), 0L))
                     return(gettextf("'%s' has length %d",
                                     name, 0L))
                 ## 'nBetweenVec', 'stepBetweenVec', 'nWithinVec', 'stepWithinVec'
                 ## have no missing values
                 if (any(is.na(value)))
                     return(gettextf("'%s' has missing values",
                                     name))
                 ## 'nBetweenVec', 'stepBetweenVec', 'nWithinVec', 'stepWithinVec'
                 ## all positive values
                 if (any(value < 1L))
                     return(gettextf("'%s' has non-positive values",
                                     name))
             }
             ## 'nTime' included in 'nWithinVec'
             if (!(nTime %in% nWithinVec))
                 return(gettextf("'%s' does not include '%s'",
                                 "nWithinVec", "nTime"))
             ## 'stepTime' included in 'stepWithinVec'
             if (!(stepTime %in% stepWithinVec))
                 return(gettextf("'%s' does not include '%s'",
                                 "stepWithinVec", "stepTime"))
             if (hasAge) {
                 ## 'nAge' included in 'nWithinVec'
                 if (!(nAge %in% nWithinVec))
                     return(gettextf("'%s' does not include '%s'",
                                     "nWithinVec", "nAge"))
                 ## 'stepAge', 'stepTriangle' included in 'stepWithinVec'
                 for (name in c("stepAge", "stepTriangle")) {
                     value <- methods::slot(object, name)
                     if (!(value %in% stepWithinVec))
                         return(gettextf("'%s' does not include '%s'",
                                         "stepWithinVec", name))
                 }
             }
             ## 'nBetweenVec' and 'stepBetweenVec' have same length
             if (!identical(length(nBetweenVec), length(stepBetweenVec)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nBetweenVec", "stepBetweenVec"))
             ## 'nWithinVec' and 'stepWithinVec' have same length
             if (!identical(length(nWithinVec), length(stepWithinVec)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nWithinVec", "stepWithinVec"))
             TRUE
         })

## HAS_TESTS
setClass("DescriptionPool",
         contains = c("DescriptionComp",
                      "StepDirectionMixin",
                      "BetweenWithinMixin"),
         validity = function(object) {
             nBetweenVec <- object@nBetweenVec
             nWithinVec <- object@nWithinVec
             length <- object@length
             ## 2 * prod(nBetweenVec) * prod(nWithinVec) equals length
             if (!isTRUE(all.equal(2 * prod(nBetweenVec) * prod(nWithinVec), length)))
                 return(gettextf("'%s', '%s', and '%s' inconsistent",
                                 "nBetweenVec", "nWithinVec", "length"))
             TRUE
         })

## HAS_TESTS
setClass("DescriptionNet",
         contains = c("DescriptionComp",
                      "BetweenWithinMixin"),
         validity = function(object) {
             nBetweenVec <- object@nBetweenVec
             nWithinVec <- object@nWithinVec
             length <- object@length
             ## prod(nBetweenVec) * prod(nWithinVec) equals length
             if (!isTRUE(all.equal(prod(nBetweenVec) * prod(nWithinVec), length)))
                 return(gettextf("'%s', '%s', and '%s' inconsistent",
                                 "nBetweenVec", "nWithinVec", "length"))
             TRUE
         })


