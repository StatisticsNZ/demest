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
    
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iPopnNext_r = 1;
    int iMinus1 = i-1;
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedPopn = stepSharedPopnVec[d];
        int iShared = (iMinus1/stepSharedComp) % nShared; 
        iPopnNext_r += iShared * stepSharedPopn;
    }
    
    int iTime = (iMinus1/stepTimeComp) % nTimeComp; 
    ++iTime; 
    iPopnNext_r += iTime * stepTimePopn;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgePopn = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int iAge = iMinus1/stepAgeComp % nAge;
        
        if (iAge < nAge - 1) {
            int stepTriangleCurrent = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
            int iTriangle = iMinus1/stepTriangleCurrent % 2;
            int isUpper = (iTriangle == 1);
            if (isUpper) {
                ++iAge;
            }
        }
        
        iPopnNext_r += iAge * stepAgePopn;
    }
    
    return iPopnNext_r;
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
    
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepDestCompVec  = INTEGER(GET_SLOT(mapping_R, stepDestCurrentVec_sym));
    int *stepOrigDestPopnVec  = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    
    int iPopnNextOrig_r = 1;
    int iMinus1 = i - 1;
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedPopn = stepSharedPopnVec[d];
        int iShared = (iMinus1/stepSharedComp) % nShared; 
        iPopnNextOrig_r += iShared * stepSharedPopn;
    }
    
    int iTime = (iMinus1/stepTimeComp) % nTimeComp; 
    ++iTime; 
    iPopnNextOrig_r += iTime * stepTimePopn;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgePopn = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int iAge = iMinus1/stepAgeComp % nAge;
        
        if (iAge < nAge - 1) {
            int stepTriangleCurrent = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
            int iTriangle = (iMinus1/stepTriangleCurrent) % 2;
            int isUpper = (iTriangle == 1);
            if (isUpper) {
                ++iAge;
            }
        }
        
        iPopnNextOrig_r += iAge * stepAgePopn;
    }
    
    int iPopnNextDest_r = iPopnNextOrig_r;
    
    for (int d = 0; d < nDimOrigDest; ++d) {
        int nOrigDest = nOrigDestVec[d];
        int stepOrigComp = stepOrigCompVec[d];
        int stepDestComp = stepDestCompVec[d];
        int stepOrigDestPopn = stepOrigDestPopnVec[d];
        
        int iOrig = (iMinus1/stepOrigComp) % nOrigDest; 
        int iDest = (iMinus1/stepDestComp) % nOrigDest; 
        iPopnNextOrig_r += iOrig * stepOrigDestPopn;
        iPopnNextDest_r += iDest * stepOrigDestPopn;
    }
    
    ans[0] = iPopnNextOrig_r;
    ans[1] = iPopnNextDest_r;
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
    
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    int isBirths = 1-hasAge;
    
    int nDimShared = LENGTH(nSharedVec_R);
    int iMinus1 = i - 1;
    
    int iTimeComp = (iMinus1 / stepTimeComp) % nTime;
    
    int iAccNext_r = 1;
    int iTimeAcc = 0;
    int returnZero = 0;
    
    int iTimeCompLessNTimeMinusOne = (iTimeComp < (nTime - 1));
    
    if (isBirths) {
        if ( iTimeCompLessNTimeMinusOne ) {
            iTimeAcc = iTimeComp + 1;
        }
        else {
            iAccNext_r = 0;
            returnZero = 1;
        }
    }
    
    else { /* not isBirths */
        int nAgeComp = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeAcc = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, 
                                                stepTriangleCurrent_sym));
        
        int iTriangleComp = ( iMinus1 / stepTriangleComp ) % 2;
        int isLower = (iTriangleComp == 0);
        
        iTimeAcc = iTimeComp;
        if (isLower) {
            if ( iTimeCompLessNTimeMinusOne ) {
                iTimeAcc = iTimeComp + 1;
            }
            else {
                iAccNext_r = 0;
                returnZero = 1;
            }
        }
        
        if (!returnZero) {
            int iAgeComp  = ( iMinus1 / stepAgeComp ) % nAgeComp;
        
            if (iAgeComp == (nAgeComp - 1)) {
                iAccNext_r = 0;
                returnZero = 1; 
            }
            else {
                iAccNext_r += iAgeComp * stepAgeAcc;
            }
        }
    }

    if (!returnZero) {

        iAccNext_r += iTimeAcc * stepTimeAcc;
        
        for (int d = 0; d < nDimShared; ++d) {
            int nShared = nSharedVec[d];
            int stepSharedComp = stepSharedCompVec[d];
            int stepSharedAcc = stepSharedAccVec[d];
            int iShared = (iMinus1 / stepSharedComp ) % nShared;
            iAccNext_r += iShared * stepSharedAcc;
        }
    }
    
    return iAccNext_r;
}


SEXP
getIAccNextFromOrigDest(int i, SEXP mapping_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, 2));
    int *ans = INTEGER(ans_R);
    
    getIAccNextFromOrigDestInternal(ans, i, mapping_R);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}

/* ans must have 2 elements */
void
getIAccNextFromOrigDestInternal(int *ans, int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeAcc  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
    int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
    int stepAgeAcc = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
    int stepTriangle = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedAccVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepDestCompVec  = INTEGER(GET_SLOT(mapping_R, stepDestCurrentVec_sym));
    int *stepOrigDestAccVec = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    int iMinus1 = i - 1;

    int iTimeComp = (iMinus1 / stepTimeComp) % nTime;
    int iTriangle = (iMinus1 / stepTriangle) % 2;
    int isLower = (iTriangle == 0);
    
    int iAccNextOrig_r = 0;
    int iAccNextDest_r = 0;
    int returnZeros = 0;
    int iTimeCompLessThanNTimeMinus1 = (iTimeComp < (nTime - 1));
    
    int iTimeAcc = iTimeComp;
    if (isLower) {
        if (iTimeCompLessThanNTimeMinus1) {
            iTimeAcc = iTimeComp + 1;
        }
        else {
            returnZeros = 1;
        }
    }
    
    if (!returnZeros) {
        int iAccNext = 1 + iTimeAcc * stepTimeAcc;
        int iAge = (iMinus1 / stepAgeComp) % nAge;
 
        if ( iAge == (nAge - 1) ) {
            returnZeros = 1;
        }
        else {
            
            iAccNext += iAge * stepAgeAcc;
            
            for (int d = 0; d < nDimShared; ++d) {
                int nShared = nSharedVec[d];
                int stepSharedComp = stepSharedCompVec[d];
                int stepSharedAcc = stepSharedAccVec[d];
                int iShared = (iMinus1/stepSharedComp) % nShared; 
                iAccNext += iShared * stepSharedAcc;
            }
            
            iAccNextOrig_r = iAccNext;
            iAccNextDest_r = iAccNext;
            
            for (int d = 0; d < nDimOrigDest; ++d) {
                int nOrigDest = nOrigDestVec[d];
                int stepOrigComp = stepOrigCompVec[d];
                int stepDestComp = stepDestCompVec[d];
                int stepOrigDestAcc = stepOrigDestAccVec[d];
                
                int iOrig = (iMinus1/stepOrigComp) % nOrigDest; 
                int iDest = (iMinus1/stepDestComp) % nOrigDest; 
                iAccNextOrig_r += iOrig * stepOrigDestAcc;
                iAccNextDest_r += iDest * stepOrigDestAcc;
            }
        }
    }

    ans[0] = iAccNextOrig_r;
    ans[1] = iAccNextDest_r;
}

/* *********** MAPPINGS TO EXPOSURE - iExposure *********** */

int
getIExposureFromComp(int i, SEXP mapping_R)
{
    int isOneToOne = *INTEGER(GET_SLOT(mapping_R, isOneToOne_sym));
    
    int returnValue = i;
    if (!isOneToOne) {
        returnValue = getIExposureFromCompNotOneToOne(i, mapping_R);
    }
    return returnValue;
}

/* does the work for getIExposureFromComp if mapping is not one-to-one */
int
getIExposureFromCompNotOneToOne(int i, SEXP mapping_R)
{    
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iMinus1 = i - 1;
    
    int iTime = ( iMinus1 / stepTimeComp ) % nTime;
    int iExp_r = 1 + iTime * stepTimeExp;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        
        int iAge = iMinus1/stepAgeComp % nAge;
        int iTriangle = iMinus1/stepTriangleComp % 2;
        
        iExp_r += iAge * stepAgeExp;
        iExp_r += iTriangle * stepTriangleExp;
    }
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1/stepSharedComp) % nShared; 
        iExp_r += iShared * stepSharedExp;
    }
    
    return iExp_r;
}

int
getIExposureFromBirths(int i, SEXP mapping_R)
{   
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeBirths  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    int hasSex  = *LOGICAL(GET_SLOT(mapping_R, hasSex_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedBirthsVec = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentExposureVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iMinus1 = i - 1;
    
    int iTime = ( iMinus1 / stepTimeBirths ) % nTime;
    int iExp_r = 1 + iTime * stepTimeExp;
    
    if (hasAge) {
        int nAgeBirths = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeBirths = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleBirths = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        
        int iMinAge = *INTEGER(GET_SLOT(mapping_R, iMinAge_sym));
        int iAgeBirths = iMinus1/stepAgeBirths % nAgeBirths;
        int iAgeExp = iAgeBirths + iMinAge - 1;
        
        int iTriangle = iMinus1/stepTriangleBirths % 2;
        
        iExp_r += iAgeExp * stepAgeExp;
        iExp_r += iTriangle * stepTriangleExp;
    }

    if (hasSex) {
        int iSexDominant = *INTEGER(GET_SLOT(mapping_R, iSexDominant_sym));
        int stepSexExp = *INTEGER(GET_SLOT(mapping_R, stepSexTarget_sym));
        iExp_r += iSexDominant * stepSexExp;
    }

    for (int d = 0; d < nDimShared; ++d) {
      int nShared = nSharedVec[d];
      int stepSharedBirths = stepSharedBirthsVec[d];
      int stepSharedExp = stepSharedExpVec[d];
      int iShared = (iMinus1/stepSharedBirths) % nShared; 
      iExp_r += iShared * stepSharedExp;
    }
    
    return iExp_r;
}


int
getIExposureFromOrigDest(int i, SEXP mapping_R)
{
    int isOneToOne = *INTEGER(GET_SLOT(mapping_R, isOneToOne_sym));
    
    int returnValue = i;
    if (!isOneToOne) {
        returnValue = getIExposureFromOrigDestNotOneToOne(i, mapping_R);
    }
    return returnValue;
}

/* does the work for getIExposureFromOrigDest if mapping is not one-to-one */
int
getIExposureFromOrigDestNotOneToOne(int i, SEXP mapping_R)
{    
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepOrigDestAccVec  = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    
    int iMinus1 = i - 1;
    
    int iTime = ( iMinus1 / stepTimeComp ) % nTime;
    int iExp_r = 1 + iTime * stepTimeExp;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        
        int iAge = iMinus1/stepAgeComp % nAge;
        int iTriangle = iMinus1/stepTriangleComp % 2;
        
        iExp_r += iAge * stepAgeExp;
        iExp_r += iTriangle * stepTriangleExp;
    }
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1/stepSharedComp) % nShared; 
        iExp_r += iShared * stepSharedExp;
    }
    
    for (int d = 0; d < nDimOrigDest; ++d) {
        int nOrigDest = nOrigDestVec[d];
        int stepOrigComp = stepOrigCompVec[d];
        int stepOrigDestAcc = stepOrigDestAccVec[d];
        
        int iOrig = (iMinus1/stepOrigComp) % nOrigDest;
        
        iExp_r += iOrig * stepOrigDestAcc;
    }
    
    return iExp_r;
    
}


/* *************** MAPPINGS TO EXPOSURE - iExpFirst **************** */


int
getIExpFirstFromComp(int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int iMinus1 = i - 1;
    
    int iExp_r = 1; 

    int iTimeComp = (iMinus1 / stepTimeComp) % nTime;
    iExp_r += iTimeComp * stepTimeExp;
    
    if (hasAge) { 
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, 
                                                stepTriangleCurrent_sym));
        int iAgeComp = ( iMinus1 / stepAgeComp ) % nAge;
        int iTriangleComp = ( iMinus1 / stepTriangleComp ) % 2;
        int iAgeExp = iAgeComp;
        int isLower = (iTriangleComp == 0);
        if (!isLower && (iAgeComp < (nAge - 1))) {
            iAgeExp++;
        }
        iExp_r += iAgeExp * stepAgeExp;
    }
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1 / stepSharedComp ) % nShared;
        iExp_r += iShared * stepSharedExp;
    }
                
    return iExp_r;
}

int
getIExpFirstFromBirths(int i, SEXP mapping_R)
{   

    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeBirths  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));

    int hasSex = *LOGICAL(GET_SLOT(mapping_R, hasSex_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedBirthsVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iMinus1 = i - 1;
    
    int iTime = ( iMinus1 / stepTimeBirths ) % nTime;
    int iExp_r = 1 + iTime * stepTimeExp;

    if (hasSex) {
        int stepSexBirths  = *INTEGER(GET_SLOT(mapping_R, stepSexCurrent_sym));
        int stepSexExp  = *INTEGER(GET_SLOT(mapping_R, stepSexTarget_sym));
        int iSex = ( iMinus1 / stepSexBirths ) % 2;
        iExp_r += iSex * stepSexExp;
    }
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedBirths = stepSharedBirthsVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1/stepSharedBirths) % nShared; 
        iExp_r += iShared * stepSharedExp;
    }
    
    return iExp_r;
}

SEXP
getIExpFirstPairFromOrigDest(int i, SEXP mapping_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, 2));
    int *ans = INTEGER(ans_R);
    
    getIExpFirstPairFromOrigDestInternal(ans, i, mapping_R);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}



/* ans must have 2 elements */
void
getIExpFirstPairFromOrigDestInternal(int *ans, int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int hasAge = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepDestCompVec  = INTEGER(GET_SLOT(mapping_R, stepDestCurrentVec_sym));
    int *stepOrigDestExpVec = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    int iMinus1 = i - 1;

    int iTimeComp = (iMinus1 / stepTimeComp) % nTime;
    
    int iExp = 1;
    int iExpOrig_r = 0;
    int iExpDest_r = 0;
    iExp += iTimeComp * stepTimeExp;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int iAgeComp = (iMinus1 / stepAgeComp) % nAge;
        int iTriangleComp = (iMinus1 / stepTriangleComp) % 2;
        int isLower = (iTriangleComp == 0);
        int iAgeExp = iAgeComp;
        if (!isLower && (iAgeComp < (nAge - 1))) {
           iAgeExp++;
        }
        iExp += iAgeExp * stepAgeExp;
    }
            
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1 / stepSharedComp ) % nShared;
        iExp += iShared * stepSharedExp;
    }
        
    iExpOrig_r = iExp;
    iExpDest_r = iExp;
        
    for (int d = 0; d < nDimOrigDest; ++d) {
        int nOrigDest = nOrigDestVec[d];
        int stepOrigComp = stepOrigCompVec[d];
        int stepDestComp = stepDestCompVec[d];
        int stepOrigDestExp = stepOrigDestExpVec[d];
                
        int iOrig = (iMinus1/stepOrigComp) % nOrigDest; 
        int iDest = (iMinus1/stepDestComp) % nOrigDest; 
                
        iExpOrig_r += iOrig * stepOrigDestExp;
        iExpDest_r += iDest * stepOrigDestExp;
    }

    ans[0] = iExpOrig_r;
    ans[1] = iExpDest_r;    
}


int
getICellCompFromExp(int i, SEXP mapping_R)
{
    int isOneToOne = *INTEGER(GET_SLOT(mapping_R, isOneToOne_sym));
    
    int returnValue = i;
    if (!isOneToOne) {
        returnValue = getICellCompFromExpNotOneToOne(i, mapping_R);
    }
    return returnValue;
}

/* does the work for getICellCompFromExp if mapping is not one-to-one */
int
getICellCompFromExpNotOneToOne(int i, SEXP mapping_R)
{    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iMinus1 = i - 1;
    
    int iComp_r = 1;
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int iShared = (iMinus1/stepSharedExp) % nShared; 
        iComp_r += iShared * stepSharedComp;
    }
    
    return iComp_r;
}

int
getICellBirthsFromExp(int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeBirths  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedBirthsVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int iMinus1 = i - 1;
        
    int iBirths_r = 1;
    int iTimeExp = (iMinus1 / stepTimeExp) % nTime;
    int iTimeBirths = iTimeExp;
    int returnZero = 0;
    
    if (hasAge) {

        int nAgeExp = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int nAgeBirths = *INTEGER(GET_SLOT(mapping_R, nAgeTarget_sym));
        
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeBirths = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleBirths = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        
        int iMinAge = *INTEGER(GET_SLOT(mapping_R, iMinAge_sym));
        
        int iAgeExp = iMinus1/stepAgeExp % nAgeExp;
        int iTriangleExp = iMinus1/stepTriangleExp % 2;
        
        iTimeBirths = iTimeExp + iMinAge - iAgeExp - 2;

        if ( iAgeExp < (iMinAge - 1) ) {
            
            if (iTriangleExp == 0) {
                iTimeBirths = iTimeExp + iMinAge - iAgeExp - 1;
            }
            if (iTimeBirths >= nTime) {
                iBirths_r = 0;
                returnZero = 1;
            }
        }
        else if ( iAgeExp < (iMinAge + nAgeBirths - 1) ) {
            int iAgeBirths = iAgeExp - iMinAge + 1;
            int iTriangleBirths = iTriangleExp;
            iTimeBirths = iTimeExp;
            iBirths_r += iAgeBirths * stepAgeBirths;
            iBirths_r += iTriangleBirths * stepTriangleBirths;
            
        }
        else {
                iBirths_r = 0;
                returnZero = 1;
        }
    } /* end hasAge */
    
    /* else iTimeBirths = iTimeExp as default */
   
    if (!returnZero) {
        iBirths_r += iTimeBirths * stepTimeBirths;
        
        for (int d = 0; d < nDimShared; ++d) {
            int nShared = nSharedVec[d];
            int stepSharedExp = stepSharedExpVec[d];
            int stepSharedBirths = stepSharedBirthsVec[d];
            int iShared = (iMinus1/stepSharedExp) % nShared; 
            iBirths_r += iShared * stepSharedBirths;
        }
    }
    
    return iBirths_r;
}

