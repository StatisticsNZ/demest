#include "mapping-functions.h"
#include "demest.h"


/* File "mapping-function.c" contains C versions of functions 
 * from "mapping-functions.R". */

/* mappings to population */

int
getIPopnNextFromComp(int i, SEXP mapping_R)
{
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedPopnVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nTimeComp  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimePopn  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    
    int hasAge  = *INTEGER(GET_SLOT(mapping_R, hasAge_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iPopnNext_R = 1;
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedPopn = stepSharedPopnVec[d];
        int iShared = ((i-1)/stepSharedComp) % nShared; 
        iPopnNext_R += iShared * stepSharedPopn;
    }
    
    int iTime = ((i-1)/stepTimeComp) % nTimeComp; 
    ++iTime; 
    iPopnNext_R += iTime * stepTimePopn;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgePopn = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int iAge = (i - 1)/stepAgeComp % nAge;
        
        if (iAge < nAge - 1) {
            int stepTriangleCurrent = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
            int iTriangle = (i - 1)/stepTriangleCurrent % 2;
            int isUpper = (iTriangle == 1);
            if (isUpper) {
                ++iAge;
            }
        }
        
        iPopnNext_R += iAge * stepAgePopn;
    }
    
    return iPopnNext_R;
}

/*getIPopnNextFromOrigDest <- function(i, mapping, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'mapping'
    stopifnot(is(mapping, "MappingOrigDestToPopn"))
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
*/

SEXP
getIPopnNextFromOrigDest(int i, SEXP mapping_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, 2));
    int *ans = INTEGER(ans_R);
    
    getIPopnNextFromOrigDestInternal(ans, i, mapping_R);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}

/* ans must have 2 elements */
void
getIPopnNextFromOrigDestInternal(int *ans, int i, SEXP mapping_R)
{
    /*n.shared.vec <- mapping@nSharedVec
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
        */
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedPopnVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nTimeComp  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimePopn  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    
    int hasAge  = *INTEGER(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepDestCompVec  = INTEGER(GET_SLOT(mapping_R, stepDestCurrentVec_sym));
    int *stepOrigDestPopnVec  = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    
    int iPopnNextOrig_R = 1;
    
    /*for (d in seq_len(n.dim.shared)) {
            n.shared <- n.shared.vec[d]
            step.shared.comp <- step.shared.comp.vec[d]
            step.shared.popn <- step.shared.popn.vec[d]
            i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
            i.popn.next.orig <- i.popn.next.orig + i.shared * step.shared.popn
        }*/
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedPopn = stepSharedPopnVec[d];
        int iShared = ((i-1)/stepSharedComp) % nShared; 
        iPopnNextOrig_R += iShared * stepSharedPopn;
    }
    
    /*i.time <- ((i - 1L) %/% step.time.comp) %% n.time.comp # C-style
        i.time <- i.time + 1L # end of interval
        i.popn.next.orig <- i.popn.next.orig + i.time * step.time.popn
        */
    int iTime = ((i-1)/stepTimeComp) % nTimeComp; 
    ++iTime; 
    iPopnNextOrig_R += iTime * stepTimePopn;
    
    /*if (has.age) {
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
        }*/
        
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgePopn = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int iAge = (i - 1)/stepAgeComp % nAge;
        
        if (iAge < nAge - 1) {
            int stepTriangleCurrent = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
            int iTriangle = (i - 1)/stepTriangleCurrent % 2;
            int isUpper = (iTriangle == 1);
            if (isUpper) {
                ++iAge;
            }
        }
        
        iPopnNextOrig_R += iAge * stepAgePopn;
    }
    
    /* i.popn.next.dest <- i.popn.next.orig
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
        c(i.popn.next.orig, i.popn.next.dest) */
    
    int iPopnNextDest_R = iPopnNextOrig_R;
    
    for (int d = 0; d < nDimOrigDest; ++d) {
        int nOrigDest = nOrigDestVec[d];
        int stepOrigComp = stepOrigCompVec[d];
        int stepDestComp = stepDestCompVec[d];
        int stepOrigDestPopn = stepOrigDestPopnVec[d];
        
        int iOrig = ((i-1)/stepOrigComp) % nOrigDest; 
        int iDest = ((i-1)/stepDestComp) % nOrigDest; 
        iPopnNextOrig_R += iOrig * stepOrigDestPopn;
        iPopnNextDest_R += iDest * stepOrigDestPopn;
    }
    
    ans[0] = iPopnNextOrig_R;
    ans[1] = iPopnNextDest_R;
}
