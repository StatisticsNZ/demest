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
    
    switch(i_method_combined)
    {
        case 9: case 10:  /*  */
            updateValuesAccount_CombinedAccountMovements(object_R);
            break;
        default:
            error("unknown iMethodCombined for updateProposalAccount: %d", i_method_combined);
            break;
    }
}


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

/* generic update expected exposure object method */
void
updateExpectedExposure(SEXP object_R)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));
    
    switch(i_method_combined)
    {
        case 9: case 10:  /*  */
            updateExpectedExposure_CombinedAccountMovements(object_R);
            break;
        default:
            error("unknown iMethodCombined for updateExpectedExposure: %d", i_method_combined);
            break;
    }
}

void
updateExpectedExposure_CombinedAccountMovements(SEXP combined_R)
{
    double * expectedExposure = REAL(GET_SLOT(combined_R, expectedExposure_sym));
    
    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisSystemModel_R = VECTOR_ELT(systemModels_R, 0);
    double * theta = REAL(GET_SLOT(thisSystemModel_R, theta_sym));
    
    SEXP descriptions_R = GET_SLOT(combined_R, descriptions_sym);
    SEXP description_R = VECTOR_ELT(descriptions_R, 0);
    
    int lengthPopn = *INTEGER(GET_SLOT(description_R, length_sym));
    
    double ageTimeStep = *REAL(GET_SLOT(combined_R, ageTimeStep_sym));
    int nTimePopn = *INTEGER(GET_SLOT(description_R, nTime_sym));
    int stepTime = *INTEGER(GET_SLOT(description_R, stepTime_sym));
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));
    
    int nTimeExp = nTimePopn - 1;

    int lengthExpNoTri = (lengthPopn / nTimePopn) * nTimeExp;
    int lengthSlicePopn = nTimePopn * stepTime;
    int lengthSliceExp = nTimeExp * stepTime;
    
    double halfAgeTimeStep = 0.5 * ageTimeStep;
    
    for (int i = 0; i < lengthExpNoTri; ++i) {

        int iPopnStart = (i / lengthSliceExp) * lengthSlicePopn
                        + i % lengthSliceExp; /* C style */
        int iPopnEnd = iPopnStart + stepTime;
        double expStart = halfAgeTimeStep * theta[iPopnStart];
        double expEnd = halfAgeTimeStep * theta[iPopnEnd];
        
        if (hasAge) {
            expectedExposure[i + lengthExpNoTri] = expStart;
            expectedExposure[i] = expEnd;
        }
        else {
            expectedExposure[i] = expStart + expEnd;
        }
            
    }
}  
    /*
## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("updateSystemModels",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updateSystemModels_CombinedAccountMovements_R, object)
                  else
                      .Call(updateSystemModels_R, object)
              }
              else {
                  system.models <- combined@systemModels
                  population <- combined@account@population
                  components <- combined@account@components
                  has.age <- combined@hasAge
                  model.uses.exposure <- combined@modelUsesExposure
                  transforms.exp.to.comp <- combined@transformsExpToComp
                  transform.exp.to.births <- combined@transformExpToBirths
                  i.births <- combined@iBirths
                  model <- system.models[[1L]]
                  model <- updateModelNotUseExp(model,
                                                y = population)
                  system.models[[1L]] <- model
                  for (i in seq_along(components)) {
                      model <- system.models[[i + 1L]]
                      component <- components[[i]]
                      uses.exposure <- model.uses.exposure[i + 1L]
                      if (uses.exposure) {
                          exposure <- combined@exposure@.Data
                          is.births <- i == i.births
                          if (is.births)
                              exposure <- collapse(exposure,
                                                   transform = transform.exp.to.births)
                          transform <- transforms.exp.to.comp[[i]]
                          if (!is.null(transform))
                              exposure <- extend(exposure,
                                                 transform = transforms.exp.to.comp[[i]])
                          model <- updateModelUseExp(object = model,
                                                     y = component,
                                                     exposure = exposure)
                      }
                      else {
                          if (methods::is(model, "Normal"))
                              component <- toDouble(component)
                          model <- updateModelNotUseExp(object = model,
                                                        y = component)
                      }
                      system.models[[i + 1L]] <- model
                  }
                  combined@systemModels <- system.models
                  combined
              }
          })

*/



/*
## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedAccountMovements"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedAccount_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  for (i in seq_len(nUpdate)) {
                      object <- updateAccount(object)
                      object <- updateSystemModels(object)
                      object <- updateExpectedExposure(object)
                      object <- updateDataModelsAccount(object)
                  }
                  object
              }
          })
*/
