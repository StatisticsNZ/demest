#include "demest.h"

/* File "update-accounts.c" contains C versions of functions 
 * from "update-accounts.R". */

/* ******************** Log-Likelihood ********************** */



/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMovePopn <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMovePopn_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        iterator <- combined@iteratorPopn
        dataModels <- combined@dataModels
        datasets <- combined@datasets
        seriesIndices <- combined@seriesIndices
        transforms <- combined@transforms
        iCell <- combined@iCell
        diff <- combined@diffProp
        diffLogLikPopn(diff = diff,
                       iFirst = iCell,
                       iterator = iterator,
                       population = population,
                       dataModels = dataModels,
                       datasets = datasets,
                       seriesIndices = seriesIndices,
                       transforms = transforms)
    }
}
*/


/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikPopn <- function(diff, iFirst, iterator, population,
                           dataModels, datasets,
                           seriesIndices, transforms,
                           useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    stopifnot(diff != 0L)
    ## iFirst
    stopifnot(identical(length(iFirst), 1L))
    stopifnot(is.integer(iFirst))
    stopifnot(!is.na(iFirst))
    stopifnot(iFirst > 0L)
    ## iterator
    stopifnot(is(iterator, "CohortIteratorPopulation"))
    ## population
    stopifnot(is(population, "Population"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, methods::is, "UseExposure")))
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
        .Call(diffLogLikPopn_R,
              diff, iFirst, iterator, population, dataModels,
              datasets, seriesIndices, transforms)
    }
    else {        
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.popn <- seriesIndices[i.dataset] == 0L
            if (assoc.with.popn) {
                model <- dataModels[[i.dataset]]
                dataset <- datasets[[i.dataset]]
                transform <- transforms[[i.dataset]]
                diff.log.lik <- diffLogLikPopnOneDataset(diff = diff,
                                                         iFirst = iFirst,
                                                         iterator = iterator,
                                                         population = population,
                                                         model = model,
                                                         dataset = dataset,
                                                         transform = transform)
                if (is.infinite(diff.log.lik))
                    return(diff.log.lik)
                ans <- ans + diff.log.lik
            }
        }
        ans
    }
}
*/


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
