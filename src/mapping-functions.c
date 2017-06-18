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
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedPopn = stepSharedPopnVec[d];
        int iShared = ((i-1)/stepSharedComp) % nShared; 
        iPopnNextOrig_R += iShared * stepSharedPopn;
    }
    
    int iTime = ((i-1)/stepTimeComp) % nTimeComp; 
    ++iTime; 
    iPopnNextOrig_R += iTime * stepTimePopn;
    
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

int
getIAccNextFromComp(int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeAcc  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedAccVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int hasAge  = *INTEGER(GET_SLOT(mapping_R, hasAge_sym));
    int isBirths = 1-hasAge;
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iTimeComp = ((i - 1) / stepTimeComp) % nTime;
    
    int iAccNext = 1;
    int iTimeAcc = 0;
    
    int iTimeCompLessNTimeMinusOne = (iTimeComp < (nTime - 1));
    
    if (isBirths) {
        if ( iTimeCompLessNTimeMinusOne ) {
            iTimeAcc = iTimeComp + 1;
        }
        else {
            iAccNext = 0;
            return iAccNext; /* return early */
        }
    }
    
    else { /* not isBirths */
        int nAgeComp = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeAcc = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, 
                                                stepTriangleCurrent_sym));
        
        int iTriangleComp = ( (i - 1) / stepTriangleComp ) % 2;
        int isLower = (iTriangleComp == 0);
        
        if (isLower) {
            if ( iTimeCompLessNTimeMinusOne ) {
                iTimeAcc = iTimeComp + 1;
            }
            else {
                iAccNext = 0;
                return iAccNext; /* return early */
            }
        }
        else {
            iTimeAcc = iTimeComp;
        }
        int iAgeComp  = ( (i - 1) / stepAgeComp ) % nAgeComp;
        
        if (iAgeComp == (nAgeComp - 1)) {
            iAccNext = 0;
                return iAccNext; /* return early */
        }
        
        iAccNext += iAgeComp * stepAgeAcc;
    }

    iAccNext += iTimeAcc * stepTimeAcc;
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedAcc = stepSharedAccVec[d];
        int iShared = ((i - 1) / stepSharedComp ) % nShared;
        iAccNext += iShared * stepSharedAcc;
    }
    
    return iAccNext;
}

