
## AlongIterator gives indices of values along the "along" dimension,
## for successive values for the classifying variables.

## HAS_TESTS
setClass("AlongIterator",
         slots = c(indices = "integer",
             initial = "integer",
             iWithin = "integer",
             nWithin = "integer",
             iBetween = "integer",
             nBetween = "integer",
             incrementBetween = "integer"),
         prototype = prototype(iWithin = 1L, iBetween = 1L),
         validity = function(object)  {
             indices <- object@indices
             initial <- object@initial
             iWithin <- object@iWithin
             nWithin <- object@nWithin
             iBetween <- object@iBetween
             nBetween <- object@nBetween
             for (name in c("indices", "initial")) {
                 value <- methods::slot(object, name)
                 ## length of 'indices', 'initial' greater than 0
                 if (identical(length(value), 0L))
                     return(gettextf("'%s' has length %d", name, 0L))
                 ## 'indices', 'initial' have no missing values
                 if (any(is.na(value)))
                     return(gettextf("'%s' has missing values", name))
                 ## values of 'indices', 'initial' at least 1
                 if (any(value < 1L))
                     return(gettextf("'%s' has values less than %d", name, 1L))
             }
             ## 'indices' and 'initial' have same length
             if (!identical(length(indices), length(initial)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "indices", "initial"))
             for (name in c("iWithin", "nWithin", "iBetween",
                            "nBetween", "incrementBetween")) {
                 value <- methods::slot(object, name)
                 ## length of 'iWithin', 'nWithin', 'iBetween', 'nBetween',
                 ## 'incrementBetween' all 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d", name, 1L))
                 ## iWithin', 'nWithin', 'iBetween', 'nBetween',
                 ## 'incrementBetween' have no missing values
                 if (is.na(value))
                     return(gettextf("'%s' is missing", name))
                 ## values of 'iWithin', 'nWithin', 'iBetween', 'nBetween',
                 ## 'incrementBetween' at least 1
                 if (value < 1L)
                     return(gettextf("'%s' is less than %d", name, 1L))
             }
             ## 'iWithin' less than or equal to 'nWithin'
             if (iWithin > nWithin)
                 return(gettextf("'%s' is greater than '%s'", "iWithin", "nWithin"))
             ## 'iBetween' less than or equal to 'nBetween'
             if (iBetween > nBetween)
                 return(gettextf("'%s' is greater than '%s'", "iBetween", "nBetween"))
             TRUE
         })


## BetaIterator gives the elements of each 'beta' associated with
## each element of 'mu'.  Slot 'indices' gives the indices of each 'beta'.
## The first element of 'indices' is always 1, since the first 'beta'
## is always an intercept term.  If, for instance, indices is c(1L, 3L, 5L),
## then the current value of 'mu' equals the intercept term plus the
## third element of the first 'beta' plus the fifth element of the second 'beta'.
## Slot 'dimIterators' is a list of objects of class "DimIterator".  It has
## an iterator for each dimension contained in the 'beta'.  For instance,
## if beta consists of a main effect for dimension 1, a main effect for dimension
## 3 and an interaction between dimensions 1 and 3, then 'dimIterators' will
## have an interator for dimensions 1 and 3 (but not dimension 2).
## Slot 'strideLengths' is a list containing an integer vector for each beta
## other than the intercept.  The vectors record how far to advance along the 'beta'
## vector for each stride returned by the associated dimIterator.  The distance is
## always 1 if 'beta' is a main effect, but is greater than 1 for all dimensions
## other than the first if 'beta' is an interaction.  Note that the terms in
## 'margin' apart from the intercept can be out of order - including terms
## within each interaction.  For instance 'margin' might be list(0L, 2:1, 2L, 1L).

## HAS_TESTS
setClass("BetaIterator",
         slots = c(indices = "integer",
                        strideLengths = "list",
                        dimIterators = "list"),
         validity = function(object) {
             indices <- object@indices
             strideLengths <- object@strideLengths
             dimIterators <- object@dimIterators
             hasMissing <- function(x) any(is.na(x))
             ## 'indices' has no missing values
             if (any(is.na(indices)))
                 return(gettextf("'%s' has missing values",
                                 "indices"))
             ## 'indices' has no values less than 1
             if (any(indices < 1L))
                 return(gettextf("'%s' has values less than %d",
                                 "indices", 1L))
             ## all elements of 'strideLengths' have type "integer"
             if (!all(sapply(strideLengths, is.integer)))
                 return(gettextf("'%s' has elements not of type \"%s\"",
                                 "strideLengths", "integer"))
             ## 'strideLengths' has no missing values
             if (any(sapply(strideLengths, hasMissing)))
                 return(gettextf("'%s' has missing values",
                                 "strideLengths"))
             ## all elements of 'dimIterators' have class "DimIterator"
             if (!all(sapply(dimIterators, is, "DimIterator")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "dimIterators", "DimIterator"))
             ## 'strideLenths' has one fewer elements than 'indices'
             if (!identical(length(strideLengths), length(indices) - 1L))
                 return(gettextf("'%s' should have one fewer elements than '%s'",
                                 "strideLengths", "indices"))
             ## each element within 'strideLengths' has same length as 'dimIterators'
             ## (because intercept term does not use stride lengths)
             if (!all(sapply(strideLengths, length) == length(dimIterators)))
                 return(gettextf("each element of '%s' should have same length as '%s'",
                                 "strideLengths", "dimIterators"))
             TRUE
         })

## HAS_TESTS
setClass("CohortIterator",
         slots = c(i = "integer",
             nTime = "integer",
             stepTime = "integer",
             iTime = "integer",
             hasAge = "logical",
             nAge = "integer",
             stepAge = "integer",
             iAge = "integer",
             finished = "logical"),
         contains = "VIRTUAL",
         validity = function(object) {
             iTime <- object@iTime
             nTime <- object@nTime
             hasAge <- object@hasAge
             iAge <- object@iAge
             nAge <- object@nAge
             finished <- object@finished
             ## all slots have length 1
             for (name in c("i",
                            "nTime", "stepTime", "iTime",
                            "hasAge", "nAge", "stepAge", "iAge",
                            "finished")) {
                 value <- methods::slot(object, name)
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
             }
             ## i, nTime, stepTime, iTime, hasAge, finished not missing
             for (name in c("i", "nTime", "stepTime", "iTime", "hasAge", "finished")) {
                 value <- methods::slot(object, name)
                 if (is.na(value))
                     return(gettextf("'%s' is missing",
                                     name))
             }
             ## i, nTime, stepTime, iTime positive
             for (name in c("i", "nTime", "stepTime", "iTime")) {
                 value <- methods::slot(object, name)
                 if (value < 1L)
                     return(gettextf("'%s' is non-positive",
                                     name))
             }
             ## iTime less than or equal to nTime
             if (iTime > nTime)
                 return(gettextf("'%s' is greater than '%s'",
                                 "iTime", "nTime"))
             if (hasAge) {
                 ## if hasAge: nAge, stepAge, iAge not missing
                 for (name in c("nAge", "stepAge", "iAge")) {
                     value <- methods::slot(object, name)
                     if (is.na(value))
                         return(gettextf("'%s' is missing",
                                         name))
                 }
                 ## if hasAge: nAge, stepAge, iAge positive
                 for (name in c("nAge", "stepAge", "iAge")) {
                     value <- methods::slot(object, name)
                     if (value < 1L)
                         return(gettextf("'%s' is non-positive",
                                         name))
                 }
                 ## if hasAge: iAge less than or equal to nAge
                 if (iAge > nAge)
                     return(gettextf("'%s' is greater than '%s'",
                                     "iAge", "nAge"))
             }
             else {
                 ## if not hasAge: nAge, stepAge, iAge all missing
                 for (name in c("nAge", "stepAge", "iAge")) {
                     value <- methods::slot(object, name)
                     if (!is.na(value))
                         return(gettextf("'%s' is %s but '%s' is not missing",
                                         "hasAge", FALSE, name))
                 }
             }
             ## finished is TRUE iff iTime >= nTime
             if (!identical(finished, iTime >= nTime))
                 return(gettextf("'%s', '%s', and '%s' inconsistent",
                                 "finished", "iTime", "nTime"))
             TRUE
         })

## HAS_TESTS
setClass("CohortIteratorAccessionPopulation",
         contains = "CohortIterator")

## HAS_TESTS
setClass("CohortIteratorComponent",
         slots = c(stepTriangle = "integer",
                        iTriangle = "integer"),
         contains = "CohortIterator",
         validity = function(object) {
             hasAge <- object@hasAge
             for (name in c("stepTriangle", "iTriangle")) {
                 value <- methods::slot(object, name)
                 ## stepTriangle, iTriangle have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 if (hasAge) {
                     ## if hasAge is TRUE: stepTriangle, iTriangle not missing
                     if (is.na(value))
                         return(gettextf("'%s' is missing",
                                         name))
                     ## if hasAge is TRUE: stepTriangle, iTriangle positive
                     if (value < 1L)
                         return(gettextf("'%s' is non-positive",
                                         name))
                 }
                 else {
                     ## if hasAge is FALSE: stepTriangle, iTriangle missing
                     if (!is.na(value))
                         return(gettextf("'%s' is %s but '%s' is not missing",
                                         "hasAge", FALSE, name))
                 }
             }
             TRUE
         })

## HAS_TESTS
setClass("CohortIteratorOrigDestParChPool",
         slots = c(iVec = "integer",
                        lengthVec = "integer",
                        increment = "integer"),
         contains = "CohortIteratorComponent",
         validity = function(object) {
             iVec <- object@iVec
             lengthVec <- object@lengthVec
             increment <- object@increment
             for (name in c("iVec", "lengthVec", "increment")) {
                 value <- methods::slot(object, name)
                 ## iVec, lengthVec, increment do not have length 0
                 if (identical(length(value), 0L))
                     return(gettextf("'%s' has length %d",
                                     name, 0L))
                 ## iVec, lengthVec, increment do not have missing values
                 if (any(is.na(value)))
                     return(gettextf("'%s' has missing values",
                                     name))
                 ## iVec, lengthVec, increment have no negative values
                 if (any(value < 0L))
                     return(gettextf("'%s' has negative values",
                                     name))
             }
             ## 'iVec' and 'increment' have same length
             if (!identical(length(iVec), length(increment)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "iVec", "increment"))
             ## 'lengthVec' has length 1
             if (!identical(length(lengthVec), 1L))
                 return(gettextf("'%s' has length %d",
                                 "lengthVec", length(lengthVec)))
             ## length of 'iVec' equal to 'lengthVec'
             if (!identical(length(iVec), lengthVec))
                 return(gettextf("length of '%s' not equal to '%s'",
                                 "iVec", "lengthVec"))
             TRUE
         })

## HAS_TESTS
## DimIterator is a 'helper' iterator for BetaIterator and MarginIterator.
## It shows how position along a chosen margin changes as the
## iterator moves along the length of an array.  For instance,
## a particular iterator might show how position along the
## second dimension changes as the iterator moves along an
## object of dimension c(3L, 2L, 4L).  The iterator does not
## show the current position along the margin, but instead the
## number of strides taken on the most recent move (as this
## is what is required by BetaIterator).
setClass("DimIterator",
         slots = c(nStrides = "integer",
                        iWithin = "integer",
                        nWithin = "integer",
                        iBetween = "integer",
                        nBetween = "integer"),
         prototype = prototype(iWithin = 1L, iBetween = 1L),
         validity = function(object) {
             ## all slots have length 1 and are not missing
             for (name in methods::slotNames(object)) {
                 value <- methods::slot(object, name)
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d", name, 1L))
                 if (is.na(value))
                     return(gettextf("'%s' is missing", name))
             }
             ## iWithin, nWithin, iBetween, nBetween all at least one.
             ## Enforces R-style indexing. Probably clearest to do so,
             ## even if it requires some -1 subtractions in C.
             for (name in c("iWithin", "nWithin", "iBetween", "nBetween")) {
                 value <- methods::slot(object, name)
                 if (value < 1L)
                     return(gettextf("'%s' is less than %d", name, 1L))
             }
             ## iWithin <= nWithin
             if (object@iWithin > object@nWithin)
                 return(gettextf("'%s' is greater than '%s'", "iWithin", "nWithin"))
             ## iBetween <= nBetween
             if (object@iBetween > object@nBetween)
                 return(gettextf("'%s' is greater than '%s'", "iBetween", "nBetween"))
             TRUE
         })

## MarginIterator is for stepping through an array of dimension 'dim'.
## 'indices' shows the position along each of the length(dim) dimensions.
## HAS_TESTS
setClass("MarginIterator",
         slots = c(indices = "integer",
                        dimIterators = "list"),
         validity = function(object) {
             indices <- object@indices
             dimIterators <- object@dimIterators
             hasMissing <- function(x) any(is.na(x))
             ## 'indices' has no missing values
             if (any(is.na(indices)))
                 return(gettextf("'%s' has missing values",
                                 "indices"))
             ## 'indices' has no values less than 1
             if (any(indices < 1L))
                 return(gettextf("'%s' has values less than %d",
                                 "indices", 1L))
             ## all elements of 'dimIterators' have class "DimIterator"
             if (!all(sapply(dimIterators, is, "DimIterator")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "dimIterators", "DimIterator"))
             ## 'indices' and 'dimIterators' have same length
             if (!identical(length(indices), length(dimIterators)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "indices", "dimIterators"))
             ## indices and dimIterators consistent
             indices.implied <- sapply(dimIterators, slot, name = "iBetween")
             if (!identical(indices.implied, indices))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "dimIterators", "indices"))
             TRUE
         })
