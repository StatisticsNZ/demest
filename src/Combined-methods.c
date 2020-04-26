#include "Combined-methods.h"
#include "model-methods.h"
#include "demest.h"

/* File "Combined-methods.c" contains C versions of functions
 * from "Combined-methods.R". */

/* ************************ drawCombined ************************ */


void
drawCombined_CombinedModelBinomial(SEXP object_R, int nUpdate)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);

    for (int i = 0; i < nUpdate; ++i) {

        drawModelUseExp(model_R, y_R, exposure_R);

    }
}


void
drawCombined_CombinedAccountMovements(SEXP object_R, int nUpdate)
{
  drawSystemModels(object_R);
  updateExpectedExposure(object_R);
  drawDataModelsAccount(object_R);
}

/* generic draw combined object method */
void
drawCombined(SEXP object_R, int nUpdate)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));

    switch(i_method_combined)
    {
        case 1: /* binomial model, has exposure */
            drawCombined_CombinedModelBinomial(object_R, nUpdate);
            break;
        case 9: /* combined account movements, no age */
            drawCombined_CombinedAccountMovements(object_R, nUpdate);
            break;
        case 10: /* combined account movements, has age */
            drawCombined_CombinedAccountMovements(object_R, nUpdate);
            break;
        default:
            error("unknown iMethodCombined for drawCombined: %d", i_method_combined);
            break;
    }
}


void
drawDataModels_CombinedAccountMovements(SEXP combined_R)
{
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP components_R = GET_SLOT(account_R, components_sym);

    int* seriesIndices = INTEGER(seriesIndices_R);

    int nObs = LENGTH(dataModels_R);
    int nProtect = 0;
    for (int i = 0; i < nObs; ++i) {

        SEXP model_R = VECTOR_ELT(dataModels_R, i);
        SEXP dataset_R = VECTOR_ELT(datasets_R, i);
        SEXP transform_R = VECTOR_ELT(transforms_R, i);
        int seriesIndex_r = seriesIndices[i];

        SEXP series_R = population_R;
        if (seriesIndex_r > 0) {
            series_R = VECTOR_ELT(components_R, seriesIndex_r-1);
        }

        SEXP seriesCollapsed_R;

        int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));

        const char *class_name = CHAR(STRING_ELT(GET_SLOT((model_R), R_ClassSymbol), 0));
        int found = !((strstr(class_name, "Poisson") == NULL) && (strstr(class_name, "CMP") == NULL));
        if (found) {

            SEXP seriesCollapsed_tmp_R;
            /* collapse_R in demographic is okay with series_R being integer
            * but type of contents of seriesCollapsed_R will be integer*/
            PROTECT(seriesCollapsed_tmp_R = dembase_Collapse_R(series_R, transform_R));
            PROTECT(seriesCollapsed_R = coerceVector(seriesCollapsed_tmp_R, REALSXP));
            nProtect = 2;
        }
        else {

            PROTECT(seriesCollapsed_R = dembase_Collapse_R(series_R, transform_R));
            nProtect = 1;
        }

        /* seriesCollapsed_R should now be in appropriate state for model */
        drawModelUseExp_Internal(model_R, dataset_R,
                 seriesCollapsed_R, i_method_model);

        UNPROTECT(nProtect); /* seriesCollapsed_R and possibly also series_Collapsed_tmp_R*/

    }
}
/* generic draw data models combined object method */
void
drawDataModels(SEXP combined_R)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    combined_R, iMethodCombined_sym)));

    switch(i_method_combined)
    {
        case 9: /* combined account movements, no age */
            drawDataModels_CombinedAccountMovements(combined_R);
            break;
        case 10: /* combined account movements, has age */
            drawDataModels_CombinedAccountMovements(combined_R);
            break;

        default:
            error("unknown iMethodCombined for drawCombined: %d", i_method_combined);
            break;
    }
}

/*
## drawSystemModels ################################################################

## Unlike with 'drawSystemModels', 'drawSystemModels' does
## not assume that outcome variables (ie the demographic series)
## have been set to missing, since, unlike the datasets,
## the series are generated as part of the estimation process,
## rather than imputed afterwards.

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawSystemModels",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(combined)
              if (useC) {
                  if (useSpecific)
                      .Call(drawSystemModels_CombinedAccountMovements_R, combined)
                  else
                      .Call(drawSystemModels_R, combined)
              }
              else {
                  system.models <- combined@systemModels
                  population <- combined@account@population
                  components <- combined@account@components
                  model.uses.exposure <- combined@modelUsesExposure
                  transforms.exp.to.comp <- combined@transformsExpToComp
                  transform.exp.to.births <- combined@transformExpToBirths
                  i.births <- combined@iBirths
                  ## population
                  population[] <- NA
                  model <- system.models[[1L]]
                  model <- drawModelNotUseExp(model,
                                              y = population)
                  system.models[[1L]] <- model
                  ## components
                  for (i in seq_along(components)) {
                      model <- system.models[[i + 1L]]
                      component <- components[[i]]
                      component[] <- NA
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
                          model <- drawModelUseExp(object = model,
                                                   y = component,
                                                   exposure = exposure)
                      }
                      else {
                          if (methods::is(model, "Normal"))
                              component <- toDouble(component)
                          model <- drawModelNotUseExp(object = model,
                                                      y = component)
                      }
                      system.models[[i + 1L]] <- model
                  }
                  combined@systemModels <- system.models
                  combined
              }
          })

*/

void
drawSystemModels_CombinedAccountMovements(SEXP combined_R)
{
    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP components_R = GET_SLOT(account_R, components_sym);
    int nComponents = LENGTH(components_R);

    int * modelUsesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));

    SEXP transformsExpToComp_R = GET_SLOT(combined_R, transformsExpToComp_sym); /* list */
    SEXP transformExpToBirths_R = GET_SLOT(combined_R, transformExpToBirths_sym);

    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));

    /* update models for population */

    int nPops = LENGTH(population_R);
    SEXP copy_population_R; /* added by JB, 5 Aug 2019 */
    PROTECT(copy_population_R = duplicate(population_R)); /* added by JB, 5 Aug 2019 */
    int * copy_population = INTEGER(copy_population_R); /* added by JB, 5 Aug 2019 */
    for (int i = 0; i < nPops; ++i){
        /* SET_VECTOR_ELT(population_R, i, ScalarLogical(NA_LOGICAL)); */
      copy_population[i] = NA_INTEGER; /* added by JB, 5 Aug 2019 */
    }

    SEXP model_R = VECTOR_ELT(systemModels_R, 0);
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    /* drawModelNotUseExp_Internal(model_R, population_R, i_method_model); */
    drawModelNotUseExp_Internal(model_R, copy_population_R, i_method_model); /* added by JB, 5 Aug 2019 */
    UNPROTECT(1);  /* copy_population_R - added by JB, 5 Aug 2019 */

    /* update models for components */

    for(int i = 0; i < nComponents; ++i) {

        SEXP this_model_R = VECTOR_ELT(systemModels_R, i+1);
        int this_i_method_model = *(INTEGER(GET_SLOT(this_model_R,
                                                iMethodModel_sym)));
        SEXP this_component_R = VECTOR_ELT(components_R, i);

        int nC = LENGTH(this_component_R);
    SEXP copy_this_component_R; /* added by JB, 5 Aug 2019 */
    PROTECT(copy_this_component_R = duplicate(this_component_R)); /* added by JB, 5 Aug 2019 */
    int * copy_this_component = INTEGER(copy_this_component_R); /* added by JB, 5 Aug 2019 */

        for (int i = 0; i < nC; ++i){
            /* SET_VECTOR_ELT(this_component_R, i, ScalarLogical(NA_LOGICAL)); */
        copy_this_component[i] = NA_INTEGER; /* added by JB, 5 Aug 2019 */
        }

        int usesExposure = modelUsesExposureVec[i+1];

        if (usesExposure) {

            SEXP transform_R = VECTOR_ELT(transformsExpToComp_R, i);
            int haveTransform = !isNull(transform_R);

            SEXP exposure_R = GET_SLOT(combined_R, exposure_sym);
            int isBirths = (i == (iBirths_r - 1));

            if(isBirths) {
                SEXP newExposure_R = NULL;
                PROTECT(newExposure_R = dembase_Collapse_R(exposure_R,
                         transformExpToBirths_R));
                if(haveTransform) {
                    SEXP anotherNewExposure_R = NULL;
                    PROTECT(anotherNewExposure_R = dembase_Extend_R(newExposure_R,
                                transform_R));
                    /* drawModelUseExp_Internal(this_model_R,  */
                    /*                         this_component_R, */
                    /*                         anotherNewExposure_R, */
                    /*                         this_i_method_model); */
                    drawModelUseExp_Internal(this_model_R, /* added by JB, 5 Aug 2019 */
                                            copy_this_component_R,
                                            anotherNewExposure_R,
                                            this_i_method_model);
                    UNPROTECT(1); /* anotherNewExposure */

                    #ifdef DEBUGGING
                    PrintValue(mkString("is Births and have transform"));
                    #endif
                }
                else {
                    /* drawModelUseExp_Internal(this_model_R, */
                    /*                             this_component_R, */
                    /*                             newExposure_R, */
                    /*                             this_i_method_model); */
                    drawModelUseExp_Internal(this_model_R, /* added by JB, 5 Aug 2019 */
                                                copy_this_component_R,
                                                newExposure_R,
                                                this_i_method_model);
                    #ifdef DEBUGGING
                    PrintValue(mkString("isBirths, no transform"));
                    #endif
                }

                UNPROTECT(1); /* newExposure */
            }
            else if (haveTransform) {
                SEXP newExposure_R = NULL;
                PROTECT(newExposure_R = dembase_Extend_R(exposure_R,
                       transform_R));
                /* drawModelUseExp_Internal(this_model_R, */
                /*                         this_component_R, */
                /*                         newExposure_R, */
                /*                         this_i_method_model); */
                drawModelUseExp_Internal(this_model_R, /* added by JB, 5 Aug 2019 */
                     copy_this_component_R,
                     newExposure_R,
                     this_i_method_model);
                UNPROTECT(1); /* newExposure */

                #ifdef DEBUGGING
                PrintValue(mkString("not isBirths, has transform"));
                #endif
                }
            else {
                /* updateModelUseExp_Internal(this_model_R, */
                /*                             this_component_R, */
                /*                             exposure_R, */
                /*                             this_i_method_model); */
                drawModelUseExp_Internal(this_model_R,
                     copy_this_component_R, /* added by JB, 5 Aug 2019 */
                     exposure_R,
                     this_i_method_model);
                #ifdef DEBUGGING
                PrintValue(mkString("not isBirths, no transform"));
                #endif
            }
        } /* end if usesExposure */

        else {
            const char *class_name = CHAR(STRING_ELT(GET_SLOT(this_model_R, R_ClassSymbol), 0));
            char *found = NULL;
            found = strstr(class_name, "Normal");
            if (found) {
                SEXP this_component_double_R;
                /* PROTECT(this_component_double_R = coerceVector(this_component_R, REALSXP)); */
        PROTECT(this_component_double_R = coerceVector(copy_this_component_R, REALSXP)); /* added by JB, 5 Aug 2019 */
                drawModelNotUseExp_Internal(this_model_R,
                                            this_component_double_R,
                                            this_i_method_model);
                UNPROTECT(1);   /* this_component_double_R */
                #ifdef DEBUGGING
                PrintValue(mkString("not use exp, normal"));
                #endif
            }
            else {
        drawModelNotUseExp(this_model_R, copy_this_component_R);
                #ifdef DEBUGGING
                PrintValue(mkString("not use exp, not normal"));
                #endif
            }
        }
    UNPROTECT(1);       /* copy_this_component_R - added by JB, 5 Aug 2019 */
    }
}

/* generic draw system models combined object method */
void
drawSystemModels(SEXP combined_R)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    combined_R, iMethodCombined_sym)));

    switch(i_method_combined)
    {
        case 9: /* combined account movements, no age */
            drawSystemModels_CombinedAccountMovements(combined_R);
            break;
        case 10: /* combined account movements, has age */
            drawSystemModels_CombinedAccountMovements(combined_R);
            break;

        default:
            error("unknown iMethodCombined for drawCombined: %d", i_method_combined);
            break;
    }
}



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

void
predictCombined_CombinedCountsPoissonHasExp(SEXP object_R,
        const char *filename, int lengthIter, int iteration)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP exposure_R = GET_SLOT(object_R, exposure_sym);
    SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
    int *strucZeroArray = INTEGER(GET_SLOT(model_R, strucZeroArray_sym));
    SEXP dataModels_R = GET_SLOT(object_R, dataModels_sym);

    transferParamModel(model_R, filename, lengthIter, iteration);

    /* reset y to have 0 if corresponding element of structZeroArray is 0,
     * and NA_INTEGER otherwise */
    int n_y = LENGTH(y_R);
    int * y = INTEGER(y_R);
    memset(y, 0, n_y * sizeof(int));

    for (int i = 0; i < n_y; ++i) {
        if (strucZeroArray[i] != 0) {
            y[i] = NA_INTEGER;
        }
    }
    predictModelUseExp(model_R, y_R, exposure_R);

    double * theta = REAL(GET_SLOT(model_R, theta_sym));
    double * exposure = REAL(exposure_R);

    for (int i = 0; i < n_y; ++i) {
        double this_lambda = theta[i] * exposure[i];
        y[i] = rpois(this_lambda);
    }

    int n_models = LENGTH(dataModels_R);

    for (int i = 0; i < n_models; ++i) {

        SEXP dataModel_R = VECTOR_ELT(dataModels_R, i);
        SEXP dataset_R = VECTOR_ELT(datasets_R, i);

        /* if it is not a poisson datamodel we can use dataset as is
         * as the exposure for predictModel,
         * but if it is a poisson model need to use a double version
         * of dataset as the exposure for predictModel */
        const char *class_name = CHAR(STRING_ELT(GET_SLOT((dataModel_R),
                                                    R_ClassSymbol), 0));
        char *found = NULL;
        found = strstr(class_name, "Poisson");

        if (found) {

            SEXP expose_R;
            SEXP expose_tmp_R;
            PROTECT( expose_tmp_R = duplicate(dataset_R) );
            PROTECT(expose_R = coerceVector(expose_tmp_R, REALSXP));

            predictModelUseExp(dataModel_R, dataset_R, expose_R);
            UNPROTECT(2);
        }
        else {
            predictModelUseExp(dataModel_R, dataset_R, dataset_R);
        }
    }
}


void
predictCombined_CombinedCountsPoissonNotHasExp(SEXP object_R,
        const char *filename, int lengthIter, int iteration)
{
    SEXP model_R = GET_SLOT(object_R, model_sym);
    SEXP y_R = GET_SLOT(object_R, y_sym);
    SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
    int *strucZeroArray = INTEGER(GET_SLOT(model_R, strucZeroArray_sym));
    SEXP dataModels_R = GET_SLOT(object_R, dataModels_sym);

    transferParamModel(model_R, filename, lengthIter, iteration);

    /* reset y to have 0 if corresponding element of structZeroArray is 0,
     * and NA_INTEGER otherwise */
    int n_y = LENGTH(y_R);
    int * y = INTEGER(y_R);
    memset(y, 0, n_y * sizeof(int));

    for (int i = 0; i < n_y; ++i) {
        if (strucZeroArray[i] != 0) {
            y[i] = NA_INTEGER;
        }
    }
    predictModelNotUseExp(model_R, y_R);

    double * theta = REAL(GET_SLOT(model_R, theta_sym));

    for (int i = 0; i < n_y; ++i) {
        y[i] = rpois(theta[i]);
    }

    int n_models = LENGTH(dataModels_R);

    for (int i = 0; i < n_models; ++i) {

        SEXP dataModel_R = VECTOR_ELT(dataModels_R, i);
        SEXP dataset_R = VECTOR_ELT(datasets_R, i);

        /* if it is not a poisson datamodel we can use dataset as is
         * as the exposure for predictModel,
         * but if it is a poisson model need to use a double version
         * of dataset as the exposure for predictModel */
        const char *class_name = CHAR(STRING_ELT(GET_SLOT((dataModel_R),
                                                    R_ClassSymbol), 0));
        char *found = NULL;
        found = strstr(class_name, "Poisson");

        if (found) {

            SEXP expose_R;
            SEXP expose_tmp_R;
            PROTECT( expose_tmp_R = duplicate(dataset_R) );
            PROTECT(expose_R = coerceVector(expose_tmp_R, REALSXP));

            predictModelUseExp(dataModel_R, dataset_R, expose_R);
            UNPROTECT(2);
        }
        else {
            predictModelUseExp(dataModel_R, dataset_R, dataset_R);
        }
    }
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
        case 6: /* poisson counts, not has exposure */
            predictCombined_CombinedCountsPoissonNotHasExp(object_R,
                                        filename, lengthIter, iteration);
            break;
        case 7: /* poisson counts, has exposure */
            predictCombined_CombinedCountsPoissonHasExp(object_R,
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
updateCombined_CombinedModelCMPNotHasExp(SEXP object_R, int nUpdate)
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
updateCombined_CombinedModelCMPHasExp(SEXP object_R, int nUpdate)
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
        case 9: case 10: /* combined account movements (no age and has age) */
        updateCombined_CombinedAccount(object_R, nUpdate);
            break;
        case 11: /* CMP model, not has exposure */
            updateCombined_CombinedModelCMPNotHasExp(object_R, nUpdate);
            break;
        case 12: /* CMP model, has exposure */
            updateCombined_CombinedModelCMPHasExp(object_R, nUpdate);
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
    int iBirths_r = *INTEGER(GET_SLOT(object_R, iBirths_sym));
    int iOrigDest_r = *INTEGER(GET_SLOT(object_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(object_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(object_R, iIntNet_sym));

    int isSmallUpdate = *LOGICAL(GET_SLOT(object_R, isSmallUpdate_sym));
    int isPopn = (iComp_r == 0);
    int isBirths = (iComp_r == iBirths_r);
    int isOrigDest = (iComp_r == iOrigDest_r);
    int isPool = (iComp_r == iPool_r);
    int isIntNet = (iComp_r == iIntNet_r);

    int usePriorPopn = *INTEGER(GET_SLOT(object_R, usePriorPopn_sym));

    double ans = 0;

    if (usePriorPopn && !(isSmallUpdate)) {
        ans += diffLogDensPopn(object_R);
    }

    if(isPopn) {
        ans += diffLogDensExpPopn(object_R);
    }
    else if (isBirths) {
        if (isSmallUpdate) {
            ans += diffLogDensJumpCompSmall(object_R);
        }
        else {
            ans += diffLogDensExpComp(object_R);
        }
    }
    else if (isOrigDest) {

        if (isSmallUpdate) {
            ans += diffLogDensJumpCompSmall(object_R);
        }
        else {

            int * usesExposureVec = LOGICAL(GET_SLOT(object_R, modelUsesExposure_sym));
            int usesExposure = usesExposureVec[iComp_r - 1];

            if(usesExposure) {
                ans += diffLogDensJumpOrigDest(object_R);
            }
            ans += diffLogDensExpOrigDestPoolNet(object_R);
        }
    }
    else if(isPool) {

        int * usesExposureVec = LOGICAL(GET_SLOT(object_R, modelUsesExposure_sym));
        int usesExposure = usesExposureVec[iComp_r - 1];

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

        if (isSmallUpdate) {
            ans += diffLogDensJumpCompSmall(object_R);
        }
        else {
            int * usesExposureVec = LOGICAL(GET_SLOT(object_R, modelUsesExposure_sym));
            int usesExposure = usesExposureVec[iComp_r - 1];

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
    int isSmallUpdate = *LOGICAL(GET_SLOT(object_R, isSmallUpdate_sym));

    if(iComp_r == 0) {
        ans = diffLogLikAccountMovePopn(object_R);
    }
    else if(iComp_r == iOrigDest_r) {
        if (isSmallUpdate) {
            ans = diffLogLikAccountMoveCompSmall(object_R);
        }
        else {
            ans = diffLogLikAccountMoveOrigDest(object_R);
        }
    }
    else if(iComp_r == iPool_r) {
        ans = diffLogLikAccountMovePool(object_R);
    }
    else if(iComp_r == iIntNet_r) {
        ans = diffLogLikAccountMoveNet(object_R);
    }
    else {
        if (isSmallUpdate) {
            ans = diffLogLikAccountMoveCompSmall(object_R);
        }
        else {
            ans = diffLogLikAccountMoveComp(object_R);
        }
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
    double probSmallUpdate = *REAL(GET_SLOT(object_R, probSmallUpdate_sym));
    int hasAge = *LOGICAL(GET_SLOT(object_R, hasAge_sym));

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
        int * isNetVec = LOGICAL(GET_SLOT(object_R, isNet_sym));

        int iComp_r = rcateg1(cumProb);
        SET_INTSCALE_SLOT(object_R, iComp_sym, iComp_r);

        if(iComp_r == iBirths_r) {
            int isSmallUpdate = (hasAge && (runif(0, 1) < probSmallUpdate));
            if (isSmallUpdate) {
                updateProposalAccountMoveBirthsSmall(object_R);
            }
            else {
                updateProposalAccountMoveBirths(object_R);
            }
        }
        else if(iComp_r == iOrigDest_r) {
            int isSmallUpdate = (hasAge && (runif(0, 1) < probSmallUpdate));
            if (isSmallUpdate) {
                updateProposalAccountMoveOrigDestSmall(object_R);
            }
            else {
                updateProposalAccountMoveOrigDest(object_R);
            }
        }
        else if(iComp_r == iPool_r) {
            updateProposalAccountMovePool(object_R);
        }
        else if(iComp_r == iIntNet_r) {
            updateProposalAccountMoveNet(object_R);
        }
        else {
            int isNet = isNetVec[iComp_r - 1];
            int isSmallUpdate = (!isNet && hasAge && (runif(0, 1) < probSmallUpdate));
            if (isSmallUpdate) {
                updateProposalAccountMoveCompSmall(object_R);
            }
            else {
                updateProposalAccountMoveComp(object_R);
            }
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
    int isSmallUpdate = *LOGICAL(GET_SLOT(object_R, isSmallUpdate_sym));

    updateCellMove(object_R);

    if (isSmallUpdate) {
        updateAccSmall(object_R);
	updateExpSmall(object_R);
    }
    else {
        updateSubsequentPopnMove(object_R);
        updateSubsequentExpMove(object_R);

        if (hasAge) {
            updateSubsequentAccMove(object_R);
        }
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

    int nAge = 0;
    int stepAgePopn = 0;
    if (hasAge) {
      nAge = *INTEGER(GET_SLOT(description_R, nAge_sym));
      stepAgePopn = *INTEGER(GET_SLOT(description_R, stepAge_sym));
    }

    for (int i = 0; i < lengthExpNoTri; ++i) {

      int iPopnStart = (i / lengthSliceExp) * lengthSlicePopn
	+ i % lengthSliceExp; /* C style */
      int iPopnEnd = iPopnStart + stepTime;
      double expStart = halfAgeTimeStep * theta[iPopnStart];
      double expEnd = halfAgeTimeStep * theta[iPopnEnd];

      if (hasAge) {
	int iAge_r = ((iPopnStart / stepAgePopn) % nAge) + 1;
	int isFinal = iAge_r == nAge;
	if (isFinal) {
	  double expTotal = expStart + expEnd;
	  expStart = (2.0/3.0) * expTotal;
	  expEnd = (1.0/3.0) * expTotal;
	}
	expectedExposure[i + lengthExpNoTri] = expStart;
	expectedExposure[i] = expEnd;
      }
      else {
	expectedExposure[i] = expStart + expEnd;
      }

    }

}

/* generic update SystemModels object method */
void
updateSystemModels(SEXP object_R)
{
    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));

    switch(i_method_combined)
    {
        case 9: case 10:  /*  */
            updateSystemModels_CombinedAccountMovements(object_R);
            break;
        default:
            error("unknown iMethodCombined for updateSystemModels: %d", i_method_combined);
            break;
    }
}



void
updateSystemModels_CombinedAccountMovements(SEXP combined_R)
{

    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    int *updateSystemModel = LOGICAL(GET_SLOT(combined_R, updateSystemModel_sym));

    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP components_R = GET_SLOT(account_R, components_sym);
    int nComponents = LENGTH(components_R);

    int * modelUsesExposureVec = LOGICAL(GET_SLOT(combined_R, modelUsesExposure_sym));

    SEXP transformsExpToComp_R = GET_SLOT(combined_R, transformsExpToComp_sym); /* list */
    SEXP transformExpToBirths_R = GET_SLOT(combined_R, transformExpToBirths_sym);

    int iBirths_r = *INTEGER(GET_SLOT(combined_R, iBirths_sym));

    /* update model for population */

    if (updateSystemModel[0]) {
        SEXP popnModel_R = VECTOR_ELT(systemModels_R, 0);
        updateModelNotUseExp(popnModel_R, population_R);
    }

    /* update models for components */

    for(int i = 0; i < nComponents; ++i) {

        if (updateSystemModel[i+1]) {

            SEXP model_R = VECTOR_ELT(systemModels_R, i+1);
            SEXP component_R = VECTOR_ELT(components_R, i);
            int usesExposure = modelUsesExposureVec[i+1];

            if (usesExposure) {

                SEXP transform_R = VECTOR_ELT(transformsExpToComp_R, i);
                int haveTransform = !isNull(transform_R);

                SEXP exposure_R = GET_SLOT(combined_R, exposure_sym);
                int isBirths = (i == (iBirths_r - 1));

                if(isBirths) {
                    SEXP newExposure_R = NULL;
                    PROTECT(newExposure_R = dembase_Collapse_R(exposure_R,
                             transformExpToBirths_R));
                    if(haveTransform) {
                        SEXP anotherNewExposure_R = NULL;
                        PROTECT(anotherNewExposure_R = dembase_Extend_R(newExposure_R,
                                    transform_R));
                        updateModelUseExp(model_R, component_R, anotherNewExposure_R);
                        UNPROTECT(1); /* anotherNewExposure */

                        #ifdef DEBUGGING
                        PrintValue(mkString("is Births and have transform"));
                        #endif
                    }
                    else {
                        updateModelUseExp(model_R, component_R, newExposure_R);

                        #ifdef DEBUGGING
                        PrintValue(mkString("isBirths, no transform"));
                        #endif
                    }

                    UNPROTECT(1); /* newExposure */
                }
                else if (haveTransform) {
                    SEXP newExposure_R = NULL;
                    PROTECT(newExposure_R = dembase_Extend_R(exposure_R,
                           transform_R));
                    updateModelUseExp(model_R, component_R, newExposure_R);
                    UNPROTECT(1); /* newExposure */

                    #ifdef DEBUGGING
                    PrintValue(mkString("not isBirths, has transform"));
                    #endif
                    }
                else {
                    updateModelUseExp(model_R, component_R, exposure_R);

                    #ifdef DEBUGGING
                    PrintValue(mkString("not isBirths, no transform"));
                    #endif
                }
            } /* end if usesExposure */

            else {
                const char *class_name = CHAR(STRING_ELT(GET_SLOT((model_R), R_ClassSymbol), 0));
                char *found = NULL;
                found = strstr(class_name, "Normal");

                if (found) {
                    SEXP componentDouble_R;
                    PROTECT(componentDouble_R = coerceVector(component_R, REALSXP));
                    updateModelNotUseExp(model_R, componentDouble_R);
                    UNPROTECT(1);
                    #ifdef DEBUGGING
                    PrintValue(mkString("not use exp, normal"));
                    #endif
                }
                else {
                    updateModelNotUseExp(model_R, component_R);
                    #ifdef DEBUGGING
                    PrintValue(mkString("not use exp, not normal"));
                    #endif
                }
            }
        }
    }
}


void
updateCombined_CombinedAccount(SEXP object_R, int nUpdate)
{
    for (int i = 0; i < nUpdate; ++i) {
        updateAccount(object_R);
        updateSystemModels(object_R);
        updateExpectedExposure(object_R);
        updateDataModelsAccount(object_R);
    }
}
