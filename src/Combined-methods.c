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


/*if (useC) {
                  if (useSpecific)
                      .Call(predictCombined_CombinedModelBinomial_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- predictModelUseExp(model, y = y, exposure = exposure)
                  object@model <- model
                  object
              }*/

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
    SEXP observationModels_R = GET_SLOT(object_R, observationModels_sym);
    SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
    SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {

/*                     y <- updateCountsPoissonNotUseExp(y = y,
                                                        model = model,
                                                        observationModels = observationModels,
                                                        datasets = datasets,
                                                        transforms = transforms)
                      model <- updateModelNotUseExp(object = model,
                                                    y = y)
                      observationModels <- updateObservationCounts(observationModels = observationModels,
                                                             datasets = datasets,
                                                             transforms = transforms,
 */                
        updateCountsPoissonNotUseExp(y_R, model_R, observationModels_R,
                                        datasets_R, transforms_R);
        
        updateModelNotUseExp_Internal(model_R, y_R, i_method_model);
        
        updateObservationCounts(y_R, observationModels_R, datasets_R,
                                        transforms_R);
        
        --nUpdate;
    }
}

/*                  y <- object@y
                  model <- object@model
                  exposure <- object@exposure
                  observationModels <- object@observationModels
                  datasets <- object@datasets
                  transforms <- object@transforms
                  for (i in seq_len(nUpdate)) {
                      y <- updateCountsPoissonUseExp(y = y,
                                                     model = model,
                                                     exposure = exposure,
                                                     observationModels = observationModels,
                                                     datasets = datasets,
                                                     transforms = transforms)
                      model <- updateModelUseExp(object = model,
                                                 y = y,
                                                 exposure = exposure)
                      observationModels <- updateObservationCounts(observationModels = observationModels,
                                                             datasets = datasets,
                                                             transforms = transforms,
                                                             y = y)
                  }
                  object@y <- y
                  object@model <- model
                  object@observationModels <- observationModels
                  object
*/


void
updateCombined_CombinedCountsPoissonHasExp(SEXP object_R, int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);
    SEXP observationModels_R = GET_SLOT(object_R, observationModels_sym);
    SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
    SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {

/*                                           y <- updateCountsPoissonUseExp(y = y,
                                                     model = model,
                                                     exposure = exposure,
                                                     observationModels = observationModels,
                                                     datasets = datasets,
                                                     transforms = transforms)
                      model <- updateModelUseExp(object = model,
                                                 y = y,
                                                 exposure = exposure)
                      observationModels <- updateObservationCounts(observationModels = observationModels,
                                                             datasets = datasets,
                                                             transforms = transforms,
                                                             y = y)

 */                
        updateCountsPoissonUseExp(y_R, model_R, 
                                exposure_R, observationModels_R,
                                datasets_R, transforms_R);
        updateModelUseExp_Internal(model_R, y_R, exposure_R, i_method_model);
        updateObservationCounts(y_R, observationModels_R, datasets_R,
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
    SEXP observationModels_R = GET_SLOT(object_R, observationModels_sym);
    SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
    SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
    
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    while (nUpdate > 0) {

/*                        y <- updateCountsBinomial(y = y,
                                                model = model,
                                                exposure = exposure,
                                                observationModels = observationModels,
                                                datasets = datasets,
                                                transforms = transforms)
                      model <- updateModelUseExp(object = model,
                                                 y = y,
                                                 exposure = exposure)
                      observationModels <- updateObservationCounts(observationModels = observationModels,
                                                             datasets = datasets,
                                                             transforms = transforms,
                                                             y = y)

 */                
        
        updateCountsBinomial(y_R, model_R, 
                                exposure_R, observationModels_R,
                                datasets_R, transforms_R);
        
        updateModelUseExp_Internal(model_R, y_R, exposure_R, i_method_model);
        
        updateObservationCounts(y_R, observationModels_R, datasets_R,
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


