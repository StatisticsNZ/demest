
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

## component - getIAccNextFromComp
## births no parent - 
## births with parent - 
## orig-dest - getIAccNextFromOrigDest
## pool - 
## net - 

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
        i.acc.next <- 1L + i.time.acc * step.time.acc
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


getIAccNextFromOrigDest <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(methods::is(mapping, "MappingOrigDestToAcc"))
    if (useC) {
        .Call(getIAccNextFromComp_R, i, mapping)
    }
    else {
        n.time <- mapping@nTimeCurrent
        step.time.comp <- mapping@stepTimeCurrent
        step.time.acc <- mapping@stepTimeTarget
        n.age <- mapping@nAge
        step.age.comp <- mapping@stepAgeCurrent
        step.age.acc <- mapping@stepAgeTarget
        step.triangle <- mapping@stepTriangle
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
                return(0L)
        }
        else
            i.time.acc <- i.time.comp
        i.acc.next <- 1L + i.time.acc * step.time.acc
        i.age <- ((i - 1L) %/% step.age.comp) %% n.age # C-style
        if (i.age == n.age)
            return(0L)
        i.acc.next <- i.acc.next + i.age * step.age.acc
        n.dim.shared <- length(n.shared.vec)
        n.dim.orig.dest <- length(n.orig.dest.vec)
        for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            step.shared.acc <- step.shared.acc.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
            i.acc.next <- i.acc.next + i.shared * step.shared.acc
        }
        i.acc.next.orig <- i.acc.next
        i.acc.next.dest <- i.acc.next
        for (d in seq_len(n.dim.orig.dest)) {
            n.orig.dest <- n.orig.dest.vec[d]
            step.orig.comp <- step.orig.comp.vec[d]
            step.dest.comp <- step.dest.comp.vec[d]
            i.orig <- ((i - 1L) %/% step.orig.comp) %% n.orig.dest # C-style
            i.dest <- ((i - 1L) %/% step.dest.comp) %% n.orig.dest # C-style
            i.acc.next.orig <- i.acc.next.orig + i.orig * step.orig.dest.acc
            i.acc.next.dest <- i.acc.next.dest + i.dest * step.orig.dest.acc
        }
        c(i.acc.next.orig, i.acc.next.dest)
    }
}


## MAPPINGS FROM EXPOSURE ################################################################

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
##         has.age <- !is.na(i.min.age)
##         n.dim.shared <- length(n.shared.vec)
##         i.births <- 1L ## R-style
##         for (d in seq_len(n.dim.shared)) {
##             n.shared <- n.shared.vec[d]
##             step.shared.exp <- step.shared.exp.vec[d]
##             step.shared.births <- step.shared.births.vec[d]
##             i.shared <- ((i - 1L) %/% step.shared.exp) %% n.shared # C-style
##             i.births <- i.births + i.shared * step.shared.births # R-style
##         }
##         i.time <- ((i - 1L) %/% step.time.exp) %% n.time.exp # C-style
##         i.births <- i.births + i.time * step.time.births # R-style
##         if (has.age) {
##             if ((i.time + i.min.age - 1L) > n.time.exp)
##                 return(0L)
##             i.births <- i.births + (i.min.age - 1L) * step.time.births # R-style
##         }
##         i.births
##     }
## }
