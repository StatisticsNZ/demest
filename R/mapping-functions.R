
## MAPPINGS TO POPULATION ################################################################

## component - getIPopnNextFromComp DONE
## births no parent - getIPopnNextFromComp DONE
## births with parent - getIPopnNextFromComp DONE
## orig-dest - getIPopnNextFromOrigDest DONE
## pool - getIPopnNextFromComp DONE
## net - getIPopnNextFromComp DONE

## TRANSLATED
## HAS_TESTS
getIPopnNextFromComp <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingCompToPopn"))
    if (useC) {
        .Call(getIPopnNextFromComp_R, i, mapping)
    }
    else {
        n.shared.vec <- mapping@nSharedVec
        step.shared.comp.vec <- mapping@stepSharedCurrentVec
        step.shared.popn.vec <- mapping@stepSharedTargetVec
        n.time.comp <- mapping@nTimeCurrent
        step.time.comp <- mapping@stepTimeCurrent
        step.time.popn <- mapping@stepTimeTarget
        has.age <- mapping@hasAge
        n.dim.shared <- length(n.shared.vec)
        i.popn.next <- 1L # R-style
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            step.shared.popn <- step.shared.popn.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
            i.popn.next <- i.popn.next + i.shared * step.shared.popn # R-style
        }
        i.time <- ((i - 1L) %/% step.time.comp) %% n.time.comp # C-style
        i.time <- i.time + 1L # end of interval; C-style
        i.popn.next <- i.popn.next + i.time * step.time.popn # R-style
        if (has.age) {
            n.age <- mapping@nAge
            step.age.comp <- mapping@stepAgeCurrent
            step.age.popn <- mapping@stepAgeTarget
            i.age <- ((i - 1L) %/% step.age.comp) %% n.age # C-style
            if (i.age < n.age - 1L) {
                step.triangle.current <- mapping@stepTriangleCurrent
                i.triangle <- ((i - 1L) %/% step.triangle.current) %% 2L # C-style
                is.upper <- i.triangle == 1L
                if (is.upper)
                    i.age <- i.age + 1L
            }
            i.popn.next <- i.popn.next + i.age * step.age.popn # R-style
        }
        i.popn.next # R-style
    }
}

## TRANSLATED
## HAS_TESTS
getIPopnNextFromOrigDest <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingOrigDestToPopn"))
    if (useC) {
        .Call(getIPopnNextFromOrigDest_R, i, mapping)
    }
    else {
        n.shared.vec <- mapping@nSharedVec
        step.shared.comp.vec <- mapping@stepSharedCurrentVec
        step.shared.popn.vec <- mapping@stepSharedTargetVec
        n.time.comp <- mapping@nTimeCurrent
        step.time.comp <- mapping@stepTimeCurrent
        step.time.popn <- mapping@stepTimeTarget
        has.age <- mapping@hasAge
        n.orig.dest.vec <- mapping@nOrigDestVec
        step.orig.comp.vec <- mapping@stepOrigCurrentVec
        step.dest.comp.vec <- mapping@stepDestCurrentVec
        step.orig.dest.popn.vec <- mapping@stepOrigDestTargetVec    
        n.dim.shared <- length(n.shared.vec)
        n.dim.orig.dest <- length(n.orig.dest.vec)
        i.popn.next.orig <- 1L
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            step.shared.popn <- step.shared.popn.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
            i.popn.next.orig <- i.popn.next.orig + i.shared * step.shared.popn
        }
        i.time <- ((i - 1L) %/% step.time.comp) %% n.time.comp # C-style
        i.time <- i.time + 1L # end of interval
        i.popn.next.orig <- i.popn.next.orig + i.time * step.time.popn
        if (has.age) {
            n.age <- mapping@nAge
            step.age.comp <- mapping@stepAgeCurrent
            step.age.popn <- mapping@stepAgeTarget
            i.age <- ((i - 1L) %/% step.age.comp) %% n.age # C-style
            if (i.age < n.age - 1L) {
                step.triangle.current <- mapping@stepTriangleCurrent
                i.triangle <- ((i - 1L) %/% step.triangle.current) %% 2L # C-style
                is.upper <- i.triangle == 1L
                if (is.upper)
                    i.age <- i.age + 1L
            }
            i.popn.next.orig <- i.popn.next.orig + i.age * step.age.popn
        }
        i.popn.next.dest <- i.popn.next.orig
        for (d in seq_len(n.dim.orig.dest)) {
            n.orig.dest <- n.orig.dest.vec[d]
            step.orig.comp <- step.orig.comp.vec[d]
            step.dest.comp <- step.dest.comp.vec[d]
            step.orig.dest.popn <- step.orig.dest.popn.vec[d]
            i.orig <- ((i - 1L) %/% step.orig.comp) %% n.orig.dest # C-style
            i.dest <- ((i - 1L) %/% step.dest.comp) %% n.orig.dest # C-style
            i.popn.next.orig <- i.popn.next.orig + i.orig * step.orig.dest.popn
            i.popn.next.dest <- i.popn.next.dest + i.dest * step.orig.dest.popn
        }
        c(i.popn.next.orig, i.popn.next.dest)
    }
}


## MAPPINGS TO ACCESSION ################################################################

## component - getIAccNextFromComp DONE
## births no parent - getIAccNextFromComp DONE
## births with parent - getIAccNextFromComp DONE
## orig-dest - getIAccNextFromOrigDest DONE
## pool - getIAccNextFromComp DONE
## net - getIAccNextFromComp DONE

## READY_TO_TRANSLATE
## HAS_TESTS
## Assume that 'accession' does not contain age 0,
## so has one less age group than 'component'
## The only way that 'hasAge' in 'component' is FALSE, but the account
## has accession (and therefore has an age dimension) is if
## 'component' is births.
getIAccNextFromComp <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingCompToAcc"))
    if (useC) {
        .Call(getIAccNextFromComp_R, i, mapping)
    }
    else {
        n.time <- mapping@nTimeCurrent
        step.time.comp <- mapping@stepTimeCurrent
        step.time.acc <- mapping@stepTimeTarget
        n.shared.vec <- mapping@nSharedVec
        step.shared.comp.vec <- mapping@stepSharedCurrentVec
        step.shared.acc.vec <- mapping@stepSharedTargetVec
        is.births <- !mapping@hasAge
        i.time.comp <- ((i - 1L) %/% step.time.comp) %% n.time # C-style
        i.acc.next <- 1L
        if (is.births) {
            if (i.time.comp < (n.time - 1L))
                i.time.acc <- i.time.comp + 1L
            else
                return(0L)
        }
        else {
            n.age.comp <- mapping@nAge
            step.age.comp <- mapping@stepAgeCurrent
            step.age.acc <- mapping@stepAgeTarget
            step.triangle.comp <- mapping@stepTriangleCurrent
            i.triangle.comp <- ((i - 1L) %/% step.triangle.comp) %% 2L # C-style
            is.lower <- i.triangle.comp == 0L
            if (is.lower) {
                if (i.time.comp < (n.time - 1L))
                    i.time.acc <- i.time.comp + 1L
                else
                    return(0L)
            }
            else
                i.time.acc <- i.time.comp
            i.age.comp <- ((i - 1L) %/% step.age.comp) %% n.age.comp # C-style
            if (i.age.comp == (n.age.comp - 1L)) # C-style
                return(0L)
            i.acc.next <- i.acc.next + i.age.comp * step.age.acc    
        }
        i.acc.next <- i.acc.next + i.time.acc * step.time.acc
        n.dim.shared <- length(n.shared.vec)
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            step.shared.acc <- step.shared.acc.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
            i.acc.next <- i.acc.next + i.shared * step.shared.acc
        }
        i.acc.next
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
getIAccNextFromOrigDest <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingOrigDestToAcc"))
    if (useC) {
        .Call(getIAccNextFromOrigDest_R, i, mapping)
    }
    else {
        n.time <- mapping@nTimeCurrent
        step.time.comp <- mapping@stepTimeCurrent
        step.time.acc <- mapping@stepTimeTarget
        n.age <- mapping@nAge
        step.age.comp <- mapping@stepAgeCurrent
        step.age.acc <- mapping@stepAgeTarget
        step.triangle <- mapping@stepTriangleCurrent
        n.shared.vec <- mapping@nSharedVec
        step.shared.comp.vec <- mapping@stepSharedCurrentVec
        step.shared.acc.vec <- mapping@stepSharedTargetVec
        n.orig.dest.vec <- mapping@nOrigDestVec
        step.orig.comp.vec <- mapping@stepOrigCurrentVec
        step.dest.comp.vec <- mapping@stepDestCurrentVec
        step.orig.dest.acc.vec <- mapping@stepOrigDestTargetVec    
        i.time.comp <- ((i - 1L) %/% step.time.comp) %% n.time # C-style
        i.triangle <- ((i - 1L) %/% step.triangle) %% 2L # C-style
        is.lower <- i.triangle == 0L
        if (is.lower) {
            if (i.time.comp < (n.time - 1L))
                i.time.acc <- i.time.comp + 1L
            else
                return(c(0L, 0L))
        }
        else
            i.time.acc <- i.time.comp
        i.acc.next <- 1L + i.time.acc * step.time.acc
        i.age <- ((i - 1L) %/% step.age.comp) %% n.age # C-style
        if (i.age == (n.age - 1L))
            return(c(0L, 0L))
        i.acc.next <- i.acc.next + i.age * step.age.acc
        n.dim.shared <- length(n.shared.vec)
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            step.shared.acc <- step.shared.acc.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
            i.acc.next <- i.acc.next + i.shared * step.shared.acc
        }
        i.acc.next.orig <- i.acc.next
        i.acc.next.dest <- i.acc.next
        n.dim.orig.dest <- length(n.orig.dest.vec)
        for (d in seq_len(n.dim.orig.dest)) {
            n.orig.dest <- n.orig.dest.vec[d]
            step.orig.comp <- step.orig.comp.vec[d]
            step.dest.comp <- step.dest.comp.vec[d]
            i.orig <- ((i - 1L) %/% step.orig.comp) %% n.orig.dest # C-style
            i.dest <- ((i - 1L) %/% step.dest.comp) %% n.orig.dest # C-style
            step.orig.dest.acc <- step.orig.dest.acc.vec[d]
            i.acc.next.orig <- i.acc.next.orig + i.orig * step.orig.dest.acc
            i.acc.next.dest <- i.acc.next.dest + i.dest * step.orig.dest.acc
        }
        c(i.acc.next.orig, i.acc.next.dest)
    }
}


## MAPPINGS TO EXPOSURE - iExposure ############################################################

## iExposure is index of the cell in 'exposure' that appears in the likelihood for
## the cell being updated. iExposure is 0L if the model for the cell being updated does
## not include exposure.

## Function only called if 'component' has exposure

## component - getIExposureFromComp
## births no parent - getIExposureFromBirths
## births with parent - getIExposureFromBirths
## orig-dest - getIExposureFromComp
## pool - getIExposureFromComp
## net - Not used, since model for 'net' does not use exposure


getIExposureFromComp <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingCompToExp"))
    if (useC) {
        .Call(getIExposureFromComp_R, i, mapping)
    }
    else {
        is.one.to.one <- mapping@isOneToOne
        if (is.one.to.one)
            return(i)
        n.time <- mapping@nTimeCurrent
        step.time.comp <- mapping@stepTimeCurrent
        step.time.exp <- mapping@stepTimeTarget
        has.age <- mapping@hasAge
        n.shared.vec <- mapping@nSharedVec
        step.shared.exp.vec <- mapping@stepSharedCurrentVec
        step.shared.comp.vec <- mapping@stepSharedTargetVec
        n.dim.shared <- length(n.shared.vec)
        i.exp <- 1L # R-style
        if (has.age) {
            n.age <- mapping@nAge
            step.age.comp <- mapping@stepAgeCurrent
            step.age.exp <- mapping@stepAgeTarget
            step.triangle.comp <- mapping@stepTriangleCurrent
            step.triangle.exp <- mapping@stepTriangleTarget
            i.age <- ((i - 1L) %/% step.age.comp) %% n.age
            i.triangle <- ((i - 1L) %/% step.triangle.comp) %% 2L
            i.exp <- i.exp + i.age * step.age.exp
            i.exp <- i.exp + i.triangle * step.triangle.exp
        }
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.exp <- step.shared.exp.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
            i.exp <- i.exp + i.shared * step.shared.exp # R-style
        }
        i.exp # R-style
    }
}


getIExposureFromBirths <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingToExpFromBirths"))
    if (useC) {
        .Call(getIExposure_R, i, mapping)
    }
    else {
        has.age <- mapping@hasAge
        n.shared.vec <- mapping@nSharedVec
        step.shared.exp.vec <- mapping@stepSharedCurrentVec
        step.shared.comp.vec <- mapping@stepSharedTargetVec
        n.dim.shared <- length(n.shared.vec)
        i.exp <- 1L # R-style
        if (has.age) {
            n.age.births <- mapping@nAge
            step.age.births <- mapping@stepAgeCurrent
            step.age.exp <- mapping@stepAgeTarget
            step.triangle.comp <- mapping@stepTriangleCurrent
            step.triangle.exp <- mapping@stepTriangleTarget
            i.min.age <- mapping@iMinAge
            i.age.births <- ((i - 1L) %/% step.age.births) %% n.age.births
            i.age.exp <- i.age.births + i.min.age - 1L
            i.triangle <- ((i - 1L) %/% step.triangle.births) %% n.age.births
            i.exp <- i.exp + i.age.exp * step.age.exp
            i.exp <- i.exp + i.triangle * step.triangle.exp
        }
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.exp <- step.shared.exp.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.exp) %% n.shared # C-style
            i.exp <- i.exp + i.shared * step.shared.comp # R-style
        }
        i.exp # R-style
    }
}



## MAPPINGS TO EXPOSURE - iExpFirst ############################################################

## iExpFirst is the index of the first cell in 'exposure' that
## will change if the cell being updated is changed

## component - 
## births no parent - 
## births with parent - 
## orig-dest - 
## pool - 
## net - 


getIExpFirstFromComp <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingToExpFirstFromComp"))
    if (useC) {
        .Call(getIExpFirstFromComp_R, i, mapping)
    }
    else {
        n.time <- mapping@nTimeCurrent
        step.time.comp <- mapping@stepTimeCurrent
        step.time.exp <- mapping@stepTimeTarget
        n.shared.vec <- mapping@nSharedVec
        step.shared.exp.vec <- mapping@stepSharedCurrentVec
        step.shared.comp.vec <- mapping@stepSharedTargetVec
        has.age <- mapping@hasAge
        i.exp <- 1L
        i.time.comp <- ((i - 1L) %/% step.time.comp) %% n.time
        if (has.age) {
            n.age <- mapping@nAge
            step.age.comp <- mapping@stepAgeCurrent
            step.age.exp <- mapping@stepAgeTarget
            step.triangle.comp <- mapping@stepTriangleCurrent
            step.triangle.exp <- mapping@stepTriangleTarget
            i.age.comp <- ((i - 1L) %/% step.age.comp) %% n.age
            i.triangle.comp <- ((i - 1L) %/% step.triangle.comp) %% 2L
            is.lower <- i.triangle.comp == 0L
            if (is.lower) {
                if (i.time.comp == (n.time - 1L))
                    return(0L)
                i.time.exp <- i.time.comp + 1L
                i.age.exp <- i.age.comp
                i.triangle.exp <- 1L
            }
            else {
                i.time.exp <- i.time.comp
                if (i.age == (n.age - 1L)) {
                    i.age.exp <- i.age.comp
                    i.triangle.exp <- 1L
                }
                else {
                    i.age.exp <- i.age.comp + 1L
                    i.triangle.exp <- 0L
                }
            }
            i.exp <- i.exp + i.age.exp * step.age.comp
            i.exp <- i.exp + i.triangle.exp * step.triangle.exp
        }
        else
            i.time.exp <- i.time.comp + 1L
        i.exp <- i.exp + i.time.exp * step.time.exp
        n.dim.shared <- length(n.shared.vec)
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            step.shared.exp <- step.shared.exp.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared
            i.exp <- i.exp + i.shared * step.shared.exp
        }
        i.exp
    }
}

## i.age.exp and i.triangle.exp are both 0, so do not
## include them in the calculations
getIExpFirstFromBirths <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingToExpFirstFromBirths"))
    if (useC) {
        .Call(getIExpFirstFromBirths_R, i, mapping)
    }
    else {
        n.time <- mapping@nTimeCurrent
        step.time.births <- mapping@stepTimeCurrent
        step.time.exp <- mapping@stepTimeTarget
        n.shared.vec <- mapping@nSharedVec
        step.shared.exp.vec <- mapping@stepSharedCurrentVec
        step.shared.births.vec <- mapping@stepSharedTargetVec
        has.age <- mapping@hasAge
        i.exp <- 1L
        i.time <- ((i - 1L) %/% step.time.births) %% n.time
        i.exp <- i.exp + i.time * step.time.exp
        n.dim.shared <- length(n.shared.vec)
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.births <- step.shared.births.vec[d]
            step.shared.exp <- step.shared.exp.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.births) %% n.shared
            i.exp <- i.exp + i.shared * step.shared.exp
        }
        i.exp
    }
}



## MAPPINGS FROM EXPOSURE ################################################################

## This is dodgy. Do as part of coding of logLik


## component - getICellCompFromExp
## births no parent - getICellBirthsFromExp
## births with parent - getICellBirthsFromExp
## orig-dest - getICellCompFromExp
## pool - getICellCompFromExp
## net - getICellCompFromExp

## ## also works with pool - dimension in comp but not in exp (direction)
## ## gets first value (Out), which is what we want
## ## can also get it to work with orig-dest, provided
## ## region dimension from exposure matched to origin dimension
## ## of orig-dest
## getICellCompFromExp <- function(i, mapping, useC = FALSE) {
##     ## 'i'
##     stopifnot(is.integer(i))
##     stopifnot(identical(length(i), 1L))
##     stopifnot(!is.na(i))
##     stopifnot(i >= 1L)
##     ## 'mapping'
##     stopifnot(methods::is(mapping, "MappingExpToComp"))
##     if (useC) {
##         .Call(getICellCompFromExp_R, i, mapping)
##     }
##     else {
##         is.one.to.one <- mapping@isOneToOne
##         if (is.one.to.one)
##             return(i)
##         n.shared.vec <- mapping@nSharedVec
##         step.shared.exp.vec <- mapping@stepSharedCurrentVec
##         step.shared.comp.vec <- mapping@stepSharedTargetVec
##         n.dim.shared <- length(n.shared.vec)
##         i.comp <- 1L # R-style
##         for (d in seq_len(n.dim.shared)) {
##             n.shared <- n.shared.vec[d]
##             step.shared.exp <- step.shared.exp.vec[d]
##             step.shared.comp <- step.shared.comp.vec[d]
##             i.shared <- ((i - 1L) %/% step.shared.exp) %% n.shared # C-style
##             i.comp <- i.comp + i.shared * step.shared.comp # R-style
##         }
##         i.comp # R-style
##     }
## }



## getICellBirthsFromExp <- function(i, mapping, useC = FALSE) {
##     ## 'i'
##     stopifnot(is.integer(i))
##     stopifnot(identical(length(i), 1L))
##     stopifnot(!is.na(i))
##     stopifnot(i >= 1L)
##     ## 'mapping'
##     stopifnot(methods::is(mapping, "MappingExpToBirths"))
##     if (useC) {
##         .Call(getICellBirthsFromExp_R, i, mapping)
##     }
##     else {
##         n.shared.vec <- mapping@nSharedVec
##         step.shared.exp.vec <- mapping@stepSharedCurrentVec
##         step.shared.births.vec <- mapping@stepSharedTargetVec
##         n.time.exp <- mapping@nTimeCurrent
##         step.time.exp <- mapping@stepTimeCurrent
##         step.time.births <- mapping@stepTimeTarget
##         i.min.age <- mapping@iMinAge
##         i.births <- 1L ## R-style
##         has.age <- !is.na(i.min.age)
##         if (has.age) {
##             if ((i.time + i.min.age - 1L) > n.time.exp)
##                 return(0L)
##             i.births <- i.births + (i.min.age - 1L) * step.time.births # R-style
##         }        
##         n.dim.shared <- length(n.shared.vec)
##         for (d in seq_len(n.dim.shared)) {
##             n.shared <- n.shared.vec[d]
##             step.shared.exp <- step.shared.exp.vec[d]
##             step.shared.births <- step.shared.births.vec[d]
##             i.shared <- ((i - 1L) %/% step.shared.exp) %% n.shared # C-style
##             i.births <- i.births + i.shared * step.shared.births # R-style
##         }
##         i.time <- ((i - 1L) %/% step.time.exp) %% n.time.exp # C-style
##         i.births <- i.births + i.time * step.time.births # R-style
##         i.births
##     }
## }




