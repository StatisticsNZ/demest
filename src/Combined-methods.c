#include "Combined-methods.h"
#include "model-methods.h"
#include "demest.h"

/* File "Combined-methods.c" contains C versions of functions 
 * from "Combined-methods.R". */

/* ******************** predictCombined ********************** */


void
predictCombined_CombinedModelNormal(SEXP object_R, 
        const char *filename, int lengthIter, int iteration)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    
    transferParamModel(model_R, filename, lengthIter, iteration);
    predictModelNotUseExp(model_R, y_R);
    
}


void
predictCombined_CombinedModelPoissonNotHasExp(SEXP object_R, 
        const char *filename, int lengthIter, int iteration)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    
    transferParamModel(model_R, filename, lengthIter, iteration);
    predictModelNotUseExp(model_R, y_R);
    
}


void
predictCombined_CombinedModelBinomial(SEXP object_R, 
        const char *filename, int lengthIter, int iteration)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);
    
    transferParamModel(model_R, filename, lengthIter, iteration);
    predictModelUseExp(model_R, y_R, exposure_R);
    
}

void
predictCombined_CombinedModelPoissonHasExp(SEXP object_R, 
        const char *filename, int lengthIter, int iteration)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);
    
    transferParamModel(model_R, filename, lengthIter, iteration);
    predictModelUseExp(model_R, y_R, exposure_R);
    
}

              
/* generic predict combined object method */
void
predictCombined(SEXP object_R, 
        const char *filename, int lengthIter, int iteration)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));
        
    switch(i_method_combined)
    {
        case 1: /* binomial model, has exposure */
            predictCombined_CombinedModelBinomial(object_R, 
                                        filename, lengthIter, iteration);
            break;
        case 2: /* normal model, not has exposure */
            predictCombined_CombinedModelNormal(object_R, 
                                        filename, lengthIter, iteration);
            break;
        case 3: /* poisson model, not has exposure */
            predictCombined_CombinedModelPoissonNotHasExp(object_R, 
                                        filename, lengthIter, iteration);
            break;
        case 4: /* poisson model, has exposure */
            predictCombined_CombinedModelPoissonHasExp(object_R, 
                                        filename, lengthIter, iteration);
            break;
        default:
            error("unknown iMethodCombined for predictCombined: %d", i_method_combined);
            break;
    }
}




/* ******************************************************************************** */
/* Functions for updating combined classes **************************************** */
/* ******************************************************************************** */

/* Note that these functions modify the models in place, 
   unlike the R versions, or the R-visible C versions
   created in init.c. */

void
updateCombined_CombinedModelNormal(SEXP object_R, int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {
        updateModelNotUseExp_Internal(model_R, y_R, i_method_model);
        
        --nUpdate;
    }
}

void
updateCombined_CombinedModelPoissonNotHasExp(SEXP object_R, int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {
        updateModelNotUseExp_Internal(model_R, y_R, i_method_model);
        
        --nUpdate;
    }
}

void
updateCombined_CombinedModelBinomial(SEXP object_R, int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {
        updateModelUseExp_Internal(model_R, y_R, exposure_R, i_method_model);
        
        --nUpdate;
    }
}

void
updateCombined_CombinedModelPoissonHasExp(SEXP object_R, int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {
        updateModelUseExp_Internal(model_R, y_R, exposure_R, i_method_model);
        
        --nUpdate;
    }
}


void
updateCombined_CombinedCountsPoissonNotHasExp(SEXP object_R,
                                                        int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP dataModels_R = GET_SLOT(object_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
    SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {

/*                     y <- updateCountsPoissonNotUseExp(y = y,
                                                        model = model,
                                                        dataModels = dataModels,
                                                        datasets = datasets,
                                                        transforms = transforms)
                      model <- updateModelNotUseExp(object = model,
                                                    y = y)
                      dataModels <- updateDataModelsCounts(dataModels = dataModels,
                                                             datasets = datasets,
                                                             transforms = transforms,
 */                
        updateCountsPoissonNotUseExp(y_R, model_R, dataModels_R,
                                        datasets_R, transforms_R);
        
        updateModelNotUseExp_Internal(model_R, y_R, i_method_model);
        
        updateDataModelsCounts(y_R, dataModels_R, datasets_R,
                                        transforms_R);
        
        --nUpdate;
    }
}


void
updateCombined_CombinedCountsPoissonHasExp(SEXP object_R, int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);
    SEXP dataModels_R = GET_SLOT(object_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
    SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {

        updateCountsPoissonUseExp(y_R, model_R, 
                                exposure_R, dataModels_R,
                                datasets_R, transforms_R);
        updateModelUseExp_Internal(model_R, y_R, exposure_R, i_method_model);
        updateDataModelsCounts(y_R, dataModels_R, datasets_R,
                                        transforms_R);
        --nUpdate;
    }
}


void
updateCombined_CombinedCountsBinomial(SEXP object_R, int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);
    SEXP dataModels_R = GET_SLOT(object_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
    SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {

        updateCountsBinomial(y_R, model_R, 
                                exposure_R, dataModels_R,
                                datasets_R, transforms_R);
        
        updateModelUseExp_Internal(model_R, y_R, exposure_R, i_method_model);
        
        updateDataModelsCounts(y_R, dataModels_R, datasets_R,
                                        transforms_R);
        
        --nUpdate;
    }
}



/* generic update combined object method */
void
updateCombined(SEXP object_R, int nUpdate)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));
        
    switch(i_method_combined)
    {
        case 1: /* binomial model, has exposure */
            updateCombined_CombinedModelBinomial(object_R, nUpdate);
            break;
        case 2: /* normal model, not has exposure */
            updateCombined_CombinedModelNormal(object_R, nUpdate);
            break;
        case 3: /* poisson model, not has exposure */
            updateCombined_CombinedModelPoissonNotHasExp(object_R, nUpdate);
            break;
        case 4: /* poisson model, has exposure */
            updateCombined_CombinedModelPoissonHasExp(object_R, nUpdate);
            break;
        case 6: /* poisson counts, not has exposure */
            updateCombined_CombinedCountsPoissonNotHasExp(object_R, nUpdate);
            break;
        case 7: /* poisson counts, has exposure */
            updateCombined_CombinedCountsPoissonHasExp(object_R, nUpdate);
            break;
        case 8: /* binomial counts */
            updateCombined_CombinedCountsBinomial(object_R, nUpdate);
            break;
        default:
            error("unknown iMethodCombined for updateCombined: %d", i_method_combined);
            break;
    }
}

/* generic diffLogDens account object method */
double 
diffLogDensAccount(SEXP object_R)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));
    double ans = 0;    
    switch(i_method_combined)
    {
        case 9: case 10:  /*  */
            ans = diffLogDensAccount_CombinedAccountMovements(object_R);
            break;
        default:
            error("unknown iMethodCombined for diffLogDensAccount: %d", i_method_combined);
            break;
    }
    
    return ans;
}


double 
diffLogDensAccount_CombinedAccountMovements(SEXP object_R)
{
    int iComp_r = *INTEGER(GET_SLOT(object_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(object_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(object_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(object_R, iIntNet_sym));
    
    int isPopn = (iComp_r == 0);
    int isOrigDest = (iComp_r == iOrigDest_r);
    int isPool = (iComp_r == iPool_r);
    int isIntNet = (iComp_r == iIntNet_r);     
    
    double ans = diffLogDensPopn(object_R);
    
    if(isPopn) {
        ans += diffLogDensExpPopn(object_R);
    }
    else {
        
        int * usesExposureVec = LOGICAL(GET_SLOT(object_R, modelUsesExposure_sym));
        int usesExposure = usesExposureVec[iComp_r - 1];

        if(isOrigDest) {
            if(usesExposure) {
                ans += diffLogDensJumpOrigDest(object_R);
            }
            ans += diffLogDensExpOrigDestPoolNet(object_R);
        } 
        else if(isPool) {
            if(usesExposure) {
                ans += diffLogDensJumpPoolWithExpose(object_R);
            }
            else {
                ans += diffLogDensJumpPoolNoExpose(object_R);
            }
            ans += diffLogDensExpOrigDestPoolNet(object_R);
        }
        else if(isIntNet) {
            ans += diffLogDensJumpNet(object_R);
            ans += diffLogDensExpOrigDestPoolNet(object_R);
        } 
        else {
            if(usesExposure) {
                ans += diffLogDensJumpComp(object_R);
            }
            ans += diffLogDensExpComp(object_R);
        }
    }
    return ans; 
}


/* generic diffLogLik account object method */
double 
diffLogLikAccount(SEXP object_R)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));
    double ans = 0;    
    switch(i_method_combined)
    {
        case 9: case 10:  /*  */
            ans = diffLogLikAccount_CombinedAccountMovements(object_R);
            break;
        default:
            error("unknown iMethodCombined for diffLogLikAccount: %d", i_method_combined);
            break;
    }
    
    return ans;
}

double 
diffLogLikAccount_CombinedAccountMovements(SEXP object_R)
{
    
    double ans = 0;
    
    int iComp_r = *INTEGER(GET_SLOT(object_R, iComp_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(object_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(object_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(object_R, iIntNet_sym));

    if(iComp_r == 0) {
        ans = diffLogLikAccountMovePopn(object_R);
    } 
    else if(iComp_r == iOrigDest_r) {
        ans = diffLogLikAccountMoveOrigDest(object_R);
    } 
    else if(iComp_r == iPool_r) {
        ans = diffLogLikAccountMovePool(object_R);
    } 
    else if(iComp_r == iIntNet_r) {
        ans = diffLogLikAccountMoveNet(object_R);
    } 
    else {
        ans = diffLogLikAccountMoveComp(object_R);
    }
    return ans; 
}


/* generic update proposal account object method */
void
updateProposalAccount(SEXP object_R)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));
        
    switch(i_method_combined)
    {
        case 9: case 10:  /*  */
            updateProposalAccount_CombinedAccountMovements(object_R);
            break;
        default:
            error("unknown iMethodCombined for updateProposalAccount: %d", i_method_combined);
            break;
    }
}

void
updateProposalAccount_CombinedAccountMovements(SEXP object_R)
{
    
    double probPopn = *REAL(GET_SLOT(object_R, probPopn_sym));
    
    double u = runif(0, 1);
    int updatePopn = (u < probPopn);
    
    if(updatePopn) {
        SET_INTSCALE_SLOT(object_R, iComp_sym, 0);
        updateProposalAccountMovePopn(object_R);
    }
    else {
        
        double * cumProb = REAL(GET_SLOT(object_R, cumProbComp_sym));
        int iBirths_r = *INTEGER(GET_SLOT(object_R, iBirths_sym));
        int iOrigDest_r = *INTEGER(GET_SLOT(object_R, iOrigDest_sym));
        int iPool_r = *INTEGER(GET_SLOT(object_R, iPool_sym));
        int iIntNet_r = *INTEGER(GET_SLOT(object_R, iIntNet_sym));
        
        int iComp_r = rcateg1(cumProb); 
        SET_INTSCALE_SLOT(object_R, iComp_sym, iComp_r);
        
        if(iComp_r == iBirths_r) {
            updateProposalAccountMoveBirths(object_R);
        } 
        else if(iComp_r == iOrigDest_r) {
            updateProposalAccountMoveOrigDest(object_R);
        } 
        else if(iComp_r == iPool_r) {
            updateProposalAccountMovePool(object_R);
        } 
        else if(iComp_r == iIntNet_r) {
            updateProposalAccountMoveNet(object_R);
        } 
        else {
            updateProposalAccountMoveComp(object_R);
        }
    } 
}



/* generic update proposal account object method */
void
updateValuesAccount(SEXP object_R)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));
    
    int nProtected = 0;
        
    switch(i_method_combined)
    {
        case 9: case 10:  /*  */
        #if(0) 
            SEXP iteratorPopn_R = GET_SLOT(object_R, iteratorPopn_sym);
            SEXP iteratorPopnDup_R = NULL;
            PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
            SEXP iteratorExposure_R = GET_SLOT(object_R, iteratorExposure_sym);
            SEXP iteratorExposureDup_R = NULL;
            PROTECT(iteratorExposureDup_R = duplicate(iteratorExposure_R));

            int hasAge = *LOGICAL(GET_SLOT(object_R, hasAge_sym));
            nProtected = 2;
            
            SEXP iteratorAccDup_R = NULL;
            if (hasAge) {
                SEXP iteratorAcc_R = GET_SLOT(object_R, iteratorAcc_sym);
                PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
                ++nProtected;
            }

            updateValuesAccount_CombinedAccountMovements(object_R);
            
            SET_SLOT(object_R, iteratorPopn_sym, iteratorPopnDup_R);
            SET_SLOT(object_R, iteratorExposure_sym, iteratorExposureDup_R);
            
            if (hasAge) {
                SET_SLOT(object_R, iteratorAcc_sym, iteratorAccDup_R);
            }
    #endif
                    
            break;
        default:
            error("unknown iMethodCombined for updateProposalAccount: %d", i_method_combined);
            break;
    }
    UNPROTECT(nProtected);
}

/*## READY_T0_TRANSLATE
## HAS_TESTS
setMethod("updateValuesAccount",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              stopifnot(methods::validObject(combined))
              if (useC) {
                  if (useSpecific)
                      .Call(updateValuesAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(updateValuesAccount_R, combined)
              }
              else {
                  has.age <- combined@hasAge
                  combined <- updateCellMove(combined)
                  combined <- updateSubsequentPopnMove(combined)
                  combined <- updateSubsequentExpMove(combined)
                  if (has.age)
                      combined <- updateSubsequentAccMove(combined)
                  combined
              }
          })
*/

void
updateValuesAccount_CombinedAccountMovements(SEXP object_R)
{
    int hasAge = *LOGICAL(GET_SLOT(object_R, hasAge_sym));
    updateCellMove(object_R);
    updateSubsequentPopnMove(object_R);
    updateSubsequentExpMove(object_R);
    
    if (hasAge) {
        updateSubsequentAccMove(object_R);
    }
}
