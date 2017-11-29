#include "helper-functions.h"
#include "demest.h"

/* File "update-accounts.c" contains C versions of functions 
 * from "update-accounts.R". */

/* ******************** Log-Likelihood ********************** */

double 
diffLogLikAccountMovePopn(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCell = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
         
    double ans = diffLogLikPopn(diff, iCell, iterator_R, 
                        population_R, dataModels_R, datasets_R, 
                        seriesIndices_R, transforms_R);
    return ans;
}


double 
diffLogLikPopn(int diff, int iFirst_r, SEXP iterator_R, 
                        SEXP population_R, SEXP dataModels_R, 
                        SEXP datasets_R, SEXP seriesIndices_R, 
                        SEXP transforms_R)
{
    double ans = 0;
    int * seriesIndices = INTEGER(seriesIndices_R);
    
    int nDatasets = LENGTH(datasets_R);
    
    for (int iDataset = 0; iDataset < nDatasets; ++iDataset) {
        
        int assocWithPopn = (seriesIndices[iDataset] == 0);
        
        if (assocWithPopn) {
            SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
            SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
            SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);
            
            double diffLogLik = diffLogLikPopnOneDataset(diff, iFirst_r, 
                                    iterator_R, population_R, 
                                    model_R, dataset_R, transform_R);
            if (R_finite(diffLogLik) ) {
                ans += diffLogLik;
            }
            else { /* infinite */
                ans = diffLogLik;
            }
        }
    }
     
    return ans;
}

double 
diffLogLikPopnOneDataset(int diff, int iFirst_r, SEXP iterator_R, 
                        SEXP population_R, SEXP model_R, 
                        SEXP dataset_R, SEXP transform_R)
{
    double ans = 0;
    int iAfter_r = dembase_getIAfter(iFirst_r, transform_R);
    
    if (iAfter_r > 0) {
        ans = diffLogLikPopnOneCell(iAfter_r, diff, population_R, 
                                    model_R, dataset_R, transform_R);
    }
    resetCP(iterator_R, iFirst_r);
    
    int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
    int finished = *finished_ptr;
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    
    while ( !finished ) {
        advanceCP(iterator_R);
        finished = *finished_ptr;
        int i_r = *i_ptr;
        
        iAfter_r = dembase_getIAfter(i_r, transform_R);
        
        if (iAfter_r > 0) {
            ans += diffLogLikPopnOneCell(iAfter_r, diff, population_R, 
                                    model_R, dataset_R, transform_R);
        }
    }
    return ans;
}

double 
diffLogLikPopnOneCell(int iAfter_r, int diff, SEXP population_R, 
                        SEXP model_R, SEXP dataset_R, SEXP transform_R)
{
    double retValue = 0;

    int * dataset = INTEGER(dataset_R);
    int iAfter = iAfter_r - 1;
    
    int cellHasNoData = ( dataset[iAfter] == NA_INTEGER );
    
    if (!cellHasNoData) {
        
        int * population = INTEGER(population_R);
        
        SEXP vec_iBefore_R = dembase_getIBefore(iAfter_r, transform_R);
        int nBefore = LENGTH(vec_iBefore_R);
        int *vec_iBefore = INTEGER(vec_iBefore_R);
        
        int totalPopnCurr = 0;
        
        for (int i = 0; i < nBefore; ++i) {
            int iBefore = vec_iBefore[i] - 1;
            totalPopnCurr += population[iBefore];
        }
        
        int totalPopnProp = totalPopnCurr + diff;
        
        double logLikProp = logLikelihood(model_R, totalPopnProp, dataset_R, iAfter_r);
        
        if(R_finite(logLikProp)) {
            
            double logLikCurr = logLikelihood(model_R, totalPopnCurr, dataset_R, iAfter_r);
            retValue = logLikProp - logLikCurr;
            
        }
        else { /* logLikProp infinite */
            retValue = logLikProp;
        }
    }
    /* if cellHasNoData, retValue stays at default value */
    
    return retValue;
}

/*## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMoveOrigDest <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMoveOrigDest_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        i.popn.orig <- combined@iPopnNext
        i.popn.dest <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.log.lik.cell <- diffLogLikCellComp(diff = diff,
                                                iComp = i.comp,
                                                iCell = i.cell,
                                                component = component,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        if (is.infinite(diff.log.lik.cell))
            return(diff.log.lik.cell)
        diff.log.lik.popn <- diffLogLikPopnPair(diff = diff,
                                                iPopnOrig = i.popn.orig,
                                                iPopnDest = i.popn.dest,
                                                iterator = iterator,
                                                population = population,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        diff.log.lik.cell + diff.log.lik.popn
    }
}
*/

/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikCellComp <- function(diff, iComp, iCell, component,
                               dataModels, datasets,
                               seriesIndices, transforms,
                               useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iComp
    stopifnot(identical(length(iComp), 1L))
    stopifnot(is.integer(iComp))
    stopifnot(!is.na(iComp))
    stopifnot(iComp >= 1L)
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell >= 1L)
    ## component
    stopifnot(is(component, "Component"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, methods::is, "Model")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and seriesIndices
    stopifnot(identical(length(dataModels), length(seriesIndices)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellComp_R,
              diff, iComp, iCell, component, dataModels,
              datasets, seriesIndices, transforms)
    }
    else {
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.comp <- seriesIndices[i.dataset] == iComp
            if (assoc.with.comp) {
                dataset <- datasets[[i.dataset]]
                model <- dataModels[[i.dataset]]
                transform <- transforms[[i.dataset]]
                diff.dataset <- diffLogLikCellOneDataset(diff = diff,
                                                         iCell = iCell,
                                                         component = component,
                                                         model = model,
                                                         dataset = dataset,
                                                         transform = transform)
                if (is.infinite(diff.dataset))
                    return(diff.dataset)
                ans <- ans + diff.dataset
            }
        }
        ans
    }
}
*/


double 
diffLogLikCellOneDataset(int diff, int iCell_r, SEXP component_R, 
                        SEXP model_R, SEXP dataset_R, SEXP transform_R)
{
    double ans = 0;
    
    int iAfter_r = dembase_getIAfter(iCell_r, transform_R);
    
    int * dataset = INTEGER(dataset_R);
    int iAfter = iAfter_r - 1;
    
    int cellHasNoData = ( (iAfter_r == 0 ) || 
                                    ( dataset[iAfter] == NA_INTEGER ) );
    
    if (!cellHasNoData) {
        
        int * component = INTEGER(component_R);
        
        SEXP vec_iBefore_R = dembase_getIBefore(iAfter_r, transform_R);
        int nBefore = LENGTH(vec_iBefore_R);
        int *vec_iBefore = INTEGER(vec_iBefore_R);
        
        int totalCompCurr = 0;
        
        for (int i = 0; i < nBefore; ++i) {
            int iBefore = vec_iBefore[i] - 1;
            totalCompCurr += component[iBefore];
        }
        
        int totalCompProp = totalCompCurr + diff;
        
        double logLikProp = logLikelihood(model_R, totalCompProp, 
                                                dataset_R, iAfter_r);
        
        if(R_finite(logLikProp)) {
            
            double logLikCurr = logLikelihood(model_R, totalCompCurr, 
                                                dataset_R, iAfter_r);
            ans = logLikProp - logLikCurr;
            
        }
        else { /* logLikProp infinite */
            ans = logLikProp;
        }
    }
    /* if cellHasNoData, retValue stays at default value */
    
    return ans;
}


double 
diffLogLikPopnPair(int diff, int iPopnOrig_r, int iPopnDest_r,
                        SEXP iterator_R, 
                        SEXP population_R, SEXP dataModels_R, 
                        SEXP datasets_R, SEXP seriesIndices_R, 
                        SEXP transforms_R)
{
    double ans = 0;
    
    if (iPopnOrig_r != iPopnDest_r) {
        
        int nDatasets = LENGTH(datasets_R);
        int * seriesIndices = INTEGER(seriesIndices_R);
        
        for(int iDataset = 0; iDataset < nDatasets; ++iDataset) {
            
            int assocWithPopn = ( seriesIndices[iDataset] == 0 );
            
            if (assocWithPopn) {
                
                SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);
                
                int iAfterOrig_r = dembase_getIAfter(iPopnOrig_r, transform_R);
                int iAfterDest_r = dembase_getIAfter(iPopnDest_r, transform_R);
                
                if(iAfterOrig_r != iAfterDest_r) {
                
                    SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
                    SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
                    
                    double diffOrig = diffLogLikPopnOneDataset( -diff,
                                            iPopnOrig_r, 
                                            iterator_R, population_R, 
                                            model_R, dataset_R, transform_R);
                    if (R_finite(diffOrig) ) {
                        
                        ans += diffOrig;
                        
                        double diffDest = diffLogLikPopnOneDataset( diff,
                                                iPopnDest_r, 
                                                iterator_R, population_R, 
                                                model_R, dataset_R, transform_R);
                        if (R_finite(diffDest) ) {
                            ans += diffDest;
                           
                        }
                        else { /* infinite */
                            ans = diffDest;
                        }    
                    }
                    else { /* infinite */
                        ans = diffOrig;
                    }
                }
            }
        }
    }
    
    return ans;
}
