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
                break; /* break out of for loop */
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


double 
diffLogLikAccountMoveOrigDest(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    
    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), iComp_r - 1);
    
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iPopnOrig_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnDest_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    double ans = 0;
    
    double diffLogLikCell = diffLogLikCellComp( diff, iComp_r, iCell_r, 
                                        component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCell) ) {

        ans += diffLogLikCell;
        
        double diffLogLikPopn = diffLogLikPopnPair( diff,
                                iPopnOrig_r, iPopnDest_r, 
                                iterator_R, population_R, 
                                dataModels_R, datasets_R, 
                                seriesIndices_R, transforms_R);
    
        if (R_finite(diffLogLikPopn) ) {
            ans += diffLogLikPopn;
           
        }
        else { /* infinite */
            ans = diffLogLikPopn;
        }    
    }
    else { /* infinite */
        ans = diffLogLikCell;
    }
    
    return ans;
}


double 
diffLogLikCellComp(int diff, int iComp_r, int iCell_r,  
                        SEXP component_R, SEXP dataModels_R, 
                        SEXP datasets_R, SEXP seriesIndices_R, 
                        SEXP transforms_R)
{
    double ans = 0;
    
    int nDatasets = LENGTH(datasets_R);
    int * seriesIndices = INTEGER(seriesIndices_R);
    
    for(int iDataset = 0; iDataset < nDatasets; ++iDataset) {
        
        int assocWithComp = ( seriesIndices[iDataset] == iComp_r );
        
        if (assocWithComp) {
            
            SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
            SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
            SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);
            
            double diffDataset = diffLogLikCellOneDataset( diff,
                                    iCell_r, component_R, 
                                    model_R, dataset_R, transform_R);
            if (R_finite(diffDataset) ) {
                
                ans += diffDataset;
                
            }
            else { /* infinite */
                ans = diffDataset;
                break; /* break out of for loop */
            }
        }
    }
    
    return ans;
}

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
                            break; /* break out of for loop */
                        }    
                    }
                    else { /* infinite */
                        ans = diffOrig;
                        break; /* break out of for loop */
                    }
                }
            }
        }
    }
    
    return ans;
}

double 
diffLogLikAccountMovePool(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    
    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), iComp_r - 1);
    
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCellOut_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iCellIn_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));
    int iPopnOut_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnIn_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    double ans = 0;
    
    double diffLogLikCells = diffLogLikCellsPool( diff, iComp_r,
                                        iCellOut_r, iCellIn_r, 
                                        component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCells) ) {

        ans += diffLogLikCells;
        
        double diffLogLikPopn = diffLogLikPopnPair( diff,
                                iPopnOut_r, iPopnIn_r, 
                                iterator_R, population_R, 
                                dataModels_R, datasets_R, 
                                seriesIndices_R, transforms_R);
    
        if (R_finite(diffLogLikPopn) ) {
            ans += diffLogLikPopn;
           
        }
        else { /* infinite */
            ans = diffLogLikPopn;
        }    
    }
    else { /* infinite */
        ans = diffLogLikCells;
    }
    
    return ans;
}


double 
diffLogLikCellsPool(int diff, int iComp_r, int iCellOut_r, int iCellIn_r,
                        SEXP component_R, SEXP dataModels_R, 
                        SEXP datasets_R, SEXP seriesIndices_R, 
                        SEXP transforms_R)
{
    double ans = 0;
    
    int nDatasets = LENGTH(datasets_R);
    int * seriesIndices = INTEGER(seriesIndices_R);
    
    for(int iDataset = 0; iDataset < nDatasets; ++iDataset) {
        
        int assocWithComp = ( seriesIndices[iDataset] == iComp_r );
        
        if (assocWithComp) {
            
            SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
            SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
            SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);
            
            double diffLogLikOut = diffLogLikCellOneDataset( diff,
                                    iCellOut_r, component_R, 
                                    model_R, dataset_R, transform_R);
            if (R_finite(diffLogLikOut) ) {
                
                ans += diffLogLikOut;
                
                double diffLogLikIn = diffLogLikCellOneDataset( diff,
                                    iCellIn_r, component_R, 
                                    model_R, dataset_R, transform_R);
                if (R_finite(diffLogLikIn) ) {
                    
                    ans += diffLogLikIn;
                    
                }
                else { /* infinite */
                    ans = diffLogLikIn;
                    break; /* break out of for loop */
                }
                
            }
            else { /* infinite */
                ans = diffLogLikOut;
                break; /* break out of for loop */
            }
        }
    }
    
    return ans;
}


double 
diffLogLikAccountMoveNet(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    
    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), iComp_r - 1);
    
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCellAdd_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iCellSub_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));
    int iPopnAdd_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnSub_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    double ans = 0;
    
    double diffLogLikCells = diffLogLikCellsNet( diff, iComp_r,
                                        iCellAdd_r, iCellSub_r, 
                                        component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCells) ) {

        ans += diffLogLikCells;
        
        /*'diffLogLikPopnPair assumes 'diff' is subtracted from first cohort
         and added to second, which is what happens with orig-dest and pool.
         To instead add and subtract, we use -diff.         */
        double diffLogLikPopn = diffLogLikPopnPair( -diff,
                                iPopnAdd_r, iPopnSub_r, 
                                iterator_R, population_R, 
                                dataModels_R, datasets_R, 
                                seriesIndices_R, transforms_R);
    
        if (R_finite(diffLogLikPopn) ) {
            ans += diffLogLikPopn;
           
        }
        else { /* infinite */
            ans = diffLogLikPopn;
        }    
    }
    else { /* infinite */
        ans = diffLogLikCells;
    }
    
    return ans;
}


double 
diffLogLikCellsNet(int diff, int iComp_r, int iCellAdd_r, int iCellSub_r,  
                        SEXP component_R, SEXP dataModels_R, 
                        SEXP datasets_R, SEXP seriesIndices_R, 
                        SEXP transforms_R)
{
    double ans = 0;
    
    int nDatasets = LENGTH(datasets_R);
    int * seriesIndices = INTEGER(seriesIndices_R);
    
    for(int iDataset = 0; iDataset < nDatasets; ++iDataset) {
        
        int assocWithComp = ( seriesIndices[iDataset] == iComp_r );
        
        if (assocWithComp) {
            
            SEXP model_R = VECTOR_ELT(dataModels_R, iDataset);
            SEXP dataset_R = VECTOR_ELT(datasets_R, iDataset);
            SEXP transform_R = VECTOR_ELT(transforms_R, iDataset);
            
            double diffLogLikAdd = diffLogLikCellOneDataset( diff,
                                    iCellAdd_r, component_R, 
                                    model_R, dataset_R, transform_R);
            if (R_finite(diffLogLikAdd) ) {
                
                ans += diffLogLikAdd;
                
                double diffLogLikSub = diffLogLikCellOneDataset( -diff,
                                    iCellSub_r, component_R, 
                                    model_R, dataset_R, transform_R);
                if (R_finite(diffLogLikSub) ) {
                    
                    ans += diffLogLikSub;
                    
                }
                else { /* infinite */
                    ans = diffLogLikSub;
                    break; /* break out of for loop */
                }
                
            }
            else { /* infinite */
                ans = diffLogLikAdd;
                break; /* break out of for loop */
            }
        }
    }
    
    return ans;
}

/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMoveComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMoveComp_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        population <- combined@account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        i.popn.next <- combined@iPopnNext
        diff <- combined@diffProp
        is.increment <- combined@isIncrement[i.comp]
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
        diff.popn <- if (is.increment) diff else -diff
        diff.log.lik.popn <- diffLogLikPopn(diff = diff.popn,
                                            iFirst = i.popn.next,
                                            iterator = iterator,
                                            population = population,
                                            dataModels = data.models,
                                            datasets = datasets,
                                            seriesIndices = series.indices,
                                            transforms = transforms)
        if (is.infinite(diff.log.lik.popn))
            return(diff.log.lik.popn)
        diff.log.lik.cell + diff.log.lik.popn
    }
}

*/


double 
diffLogLikAccountMoveComp(SEXP combined_R)
{
    /*        account <- combined@account
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        population <- combined@account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        i.popn.next <- combined@iPopnNext
        diff <- combined@diffProp
        is.increment <- combined@isIncrement[i.comp]
*/
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    
    SEXP component_R = VECTOR_ELT(GET_SLOT(account_R, components_sym), iComp_r - 1);
    
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);

    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iPopnNext_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
    int isIncrement = isIncrementVec[iComp_r - 1];
    
    /* is.increment <- combined@isIncrement[i.comp] */
    
    double ans = 0;
    /*diff.log.lik.cell <- diffLogLikCellComp(diff = diff,
                                                iComp = i.comp,
                                                iCell = i.cell,
                                                component = component,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)*/
    
    double diffLogLikCell = diffLogLikCellComp( diff, iComp_r,
                                        iCell_r, component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCell) ) {

        ans += diffLogLikCell;
        
        /*diff.popn <- if (is.increment) diff else -diff
        diff.log.lik.popn <- diffLogLikPopn(diff = diff.popn,
                                            iFirst = i.popn.next,
                                            iterator = iterator,
                                            population = population,
                                            dataModels = data.models,
                                            datasets = datasets,
                                            seriesIndices = series.indices,
                                            transforms = transforms)
        */
        
        double diffPopn = ( isIncrement ? diff : -diff );
        
        double diffLogLikPopnValue = diffLogLikPopn( diffPopn,
                                iPopnNext_r, 
                                iterator_R, population_R, 
                                dataModels_R, datasets_R, 
                                seriesIndices_R, transforms_R);
    
        if (R_finite(diffLogLikPopnValue) ) {
            ans += diffLogLikPopnValue;
           
        }
        else { /* infinite */
            ans = diffLogLikPopnValue;
        }    
    }
    else { /* infinite */
        ans = diffLogLikCell;
    }
    
    return ans;
}

/*------------------ LOG DENSITY ------------------------*/


double 
diffLogDensPopn(SEXP combined_R)
{
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    
    SEXP systemModels_R = GET_SLOT(combined_R, systemModels_sym);
    SEXP thisModel_R = VECTOR_ELT(systemModels_R, 0);
    
    double * theta = REAL(GET_SLOT(thisModel_R, theta_sym));
    
    int iPopnNext_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnNextOther_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    int iCell_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iComp_r = *INTEGER(GET_SLOT(combined_R, iComp_sym));
    
    int iOrigDest_r = *INTEGER(GET_SLOT(combined_R, iOrigDest_sym));
    int iPool_r = *INTEGER(GET_SLOT(combined_R, iPool_sym));
    int iIntNet_r = *INTEGER(GET_SLOT(combined_R, iIntNet_sym));
    int * isIncrementVec = LOGICAL(GET_SLOT(combined_R, isIncrement_sym));
    
    int isPopn = (iComp_r == 0);
    int isOrigDest = (iComp_r == iOrigDest_r);
    int isPool = (iComp_r == iPool_r);
    int isIntNet = (iComp_r == iIntNet_r);     
    
    double ans = 0;
    
    if (isPopn) {
        ans = diffLogDensPopnOneCohort(diff, population_R,
                                    iCell_r, iterator_R, theta);
    }   
    else if (isOrigDest || isPool) {
        
        double ansOrig = diffLogDensPopnOneCohort(-diff, population_R, 
                                    iPopnNext_r, iterator_R, theta);
        
        double ansDest = diffLogDensPopnOneCohort(diff, population_R, 
                                    iPopnNextOther_r, iterator_R, theta);
        ans = ansOrig + ansDest;
        
    } 
    else if (isIntNet) {
        
        double ansAdd = diffLogDensPopnOneCohort(diff, population_R, 
                                    iPopnNext_r, iterator_R, theta);
        
        double ansSub = diffLogDensPopnOneCohort(-diff, population_R, 
                                    iPopnNextOther_r, iterator_R, theta);
        ans = ansAdd + ansSub;
        
    }
    else {
        
        if ( !isIncrementVec[iComp_r - 1] ) {
            diff = -diff;
        }
        
        ans = diffLogDensPopnOneCohort(diff, population_R, 
                                    iPopnNext_r, iterator_R, theta);
        
    }
    
    return ans;
}


double
diffLogDensPopnOneCohort(int diff, SEXP population_R, int i_r, 
                        SEXP iterator_R, double * theta)
{
    int * population = INTEGER(population_R);
    resetCP(iterator_R, i_r);
    double ans = 0;
    
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    int * finished_ptr = LOGICAL(GET_SLOT(iterator_R, finished_sym));
    
    int i = *i_ptr - 1;
    int finished = *finished_ptr;
    
    int valCurr = population[i];
    int valProp = valCurr + diff;
    double lambda = theta[i];
    
    double logDensProp = dpois(valProp, lambda, USE_LOG);
    double logDensCurr = dpois(valCurr, lambda, USE_LOG);
    
    ans += (logDensProp - logDensCurr);
    
    while (!finished) {
        
        advanceCP(iterator_R);
        i = *i_ptr - 1;
        finished = *finished_ptr;
        
        valCurr = population[i];
        valProp = valCurr + diff;
        lambda = theta[i];
        
        logDensProp = dpois(valProp, lambda, USE_LOG);
        logDensCurr = dpois(valCurr, lambda, USE_LOG);
        
        ans += (logDensProp - logDensCurr);
        
    }
    return ans;
}


/*
## READY_TO_TRANSLATE
## HAS_TESTS
## Difference in log-density of current values for
## all components attributable to change in exposure,
## where change in exposure due to change in
## a cell in the initial population.
diffLogDensExpPopn <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensExpPopn_R, combined)
    }
    else {
        components <- combined@account@components
        iterators.comp <- combined@iteratorsComp
        model.uses.exposure <- combined@modelUsesExposure
        mappings.from.exp <- combined@mappingsFromExp
        i.exp.first <- combined@iExpFirst
        iterator.exposure <- combined@iteratorExposure
        diff <- combined@diffProp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.births <- combined@iBirths
        i.par.ch <- combined@iParCh
        exposure <- combined@exposure
        systemModels <- combined@systemModels
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        ans <- 0
        for (i in seq_along(components)) {
            if (model.uses.exposure[i + 1L]) { ## note that 'net' components never use exposure
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping.from.exp <- mappings.from.exp[[i]]
                is.orig.dest <- i == i.orig.dest
                is.pool <- i == i.pool
                is.births <- i == i.births
                is.par.ch <- i == i.par.ch
                if (is.orig.dest || is.pool) {
                    i.cell <- getICellCompFromExp(i = i.exp.first,
                                                  mapping = mapping.from.exp)
                    diff.log <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell,
                                                                   hasAge = has.age,
                                                                   ageTimeStep = age.time.step,
                                                                   updatedPopn = TRUE,
                                                                   component = component,
                                                                   theta = theta,
                                                                   iteratorComp = iterator.comp,
                                                                   iExpFirst = i.exp.first,
                                                                   exposure = exposure,
                                                                   iteratorExposure = iterator.exposure,
                                                                   diff = diff)
                    if (is.infinite(diff.log))
                        return(diff.log)
                    ans <- ans + diff.log
                }
                else if (is.births) {
                    i.cell <- getICellBirthsFromExp(i = i.exp.first,
                                                    mapping = mapping.from.exp)
                    cell.is.affected <- i.cell > 0L
                    if (cell.is.affected) {
                        if (is.par.ch)
                            diff.log <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell,
                                                                           hasAge = has.age,
                                                                           ageTimeStep = age.time.step,
                                                                           updatedPopn = TRUE,
                                                                           component = component,
                                                                           theta = theta,
                                                                           iteratorComp = iterator.comp,
                                                                           iExpFirst = i.exp.first,
                                                                           exposure = exposure,
                                                                           iteratorExposure = iterator.exposure,
                                                                           diff = diff)
                        else
                            diff.log <- diffLogDensExpOneComp(iCell = i.cell,
                                                              hasAge = has.age,
                                                              ageTimeStep = age.time.step,
                                                              updatedPopn = TRUE,
                                                              component = component,
                                                              theta = theta,
                                                              iteratorComp = iterator.comp,
                                                              iExpFirst = i.exp.first,
                                                              exposure = exposure,
                                                              iteratorExposure = iterator.exposure,
                                                              diff = diff)
                        if (is.infinite(diff.log))
                            return(diff.log)
                        ans <- ans + diff.log
                    }
                }
                else {
                    i.cell <- getICellCompFromExp(i = i.exp.first,
                                                  mapping = mapping.from.exp)
                    diff.log <- diffLogDensExpOneComp(iCell = i.cell,
                                                      hasAge = has.age,
                                                      ageTimeStep = age.time.step,
                                                      updatedPopn = TRUE,
                                                      component = component,
                                                      theta = theta,
                                                      iteratorComp = iterator.comp,
                                                      iExpFirst = i.exp.first,
                                                      exposure = exposure,
                                                      iteratorExposure = iterator.exposure,
                                                      diff = diff)
                    if (is.infinite(diff.log))
                        return(diff.log)
                    ans <- ans + diff.log
                }
            }
        }
        ans
    }
}
*/


double
diffLogDensExpOneOrigDestParChPool(int iCell_r, int hasAge, 
                        double ageTimeStep, int updatedPopn,
                        SEXP component_R, double * theta,
                        SEXP iteratorComp_R, 
                        int iExpFirst_r, double * exposure,
                        SEXP iteratorExposure_R,
                        int diff)

{
    double ans = 0;
    
    int * component = INTEGER(component_R);
    
    resetCODPCP(iteratorComp_R, iCell_r);
    resetCC(iteratorExposure_R, iExpFirst_r);
    
    int lengthVec = *INTEGER(GET_SLOT(iteratorComp_R, lengthVec_sym));
    
    int * iExp_ptr = INTEGER(GET_SLOT(iteratorExposure_R, i_sym)); 
    int *iCompVec = INTEGER(GET_SLOT(iteratorComp_R, iVec_sym));
    
    int * finishedComp_ptr = LOGICAL(GET_SLOT(iteratorComp_R, finished_sym)); 
    
       int keepGoing = 1;
       while (keepGoing) { 
           
           int iExp_r = *iExp_ptr;
           
           double diffExposure = 0;
           
           if (hasAge) {
               diffExposure = 0.5 * diff * ageTimeStep;
           }
           else {
               
               int isFirstCell = (iExp_r == iExpFirst_r);
               if (isFirstCell && !updatedPopn) {
                   diffExposure = 0.5 * diff * ageTimeStep;
               }
                else {
                   diffExposure = diff * ageTimeStep;
                }
            }
            double exposureCurr = exposure[iExp_r - 1];
            double exposureProp = exposureCurr + diffExposure;
            
            int j = 0;
            
            while (j < lengthVec && keepGoing ) {
                int iComp = iCompVec[j] - 1;
                int compCurr = component[iComp];
                
                if ( (compCurr > 0) && !(exposureProp > 0) ) {
                    
                    ans = R_NegInf;
                    keepGoing = 0;
                    
                }
                else {
                    double thetaCurr = theta[iComp];
                    double lambdaProp = thetaCurr * exposureProp;
                    double lambdaCurr = thetaCurr * exposureCurr;
                    double diffLogLik = dpois(compCurr, lambdaProp, USE_LOG)
                                 -
                                 dpois(compCurr, lambdaCurr, USE_LOG);
                    ans += diffLogLik;
                    
                }
                
                ++j;
            } 
    
            if (keepGoing) {
                int finishedComp = *finishedComp_ptr;
                if (finishedComp) {
                    keepGoing = 0;
                }
                else {
                    advanceCODPCP(iteratorComp_R);
                    advanceCC(iteratorExposure_R);
                }
            }  
           
       } /* end main while loop */
    
    return ans;
}



/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensExpOneComp <- function(iCell, hasAge, ageTimeStep, updatedPopn,
                                  component, theta, iteratorComp, 
                                  iExpFirst, exposure, iteratorExposure,
                                  diff, useC = FALSE) {
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell > 0L)
    ## hasAge
    stopifnot(identical(length(hasAge), 1L))
    stopifnot(is.logical(hasAge))
    stopifnot(!is.na(hasAge))
    ## ageTimeStep
    stopifnot(is.double(ageTimeStep))
    stopifnot(identical(length(ageTimeStep), 1L))
    stopifnot(!is.na(ageTimeStep))
    stopifnot(ageTimeStep > 0)
    ## updatedPopn
    stopifnot(identical(length(updatedPopn), 1L))
    stopifnot(is.logical(updatedPopn))
    stopifnot(!is.na(updatedPopn))
    ## component
    stopifnot(is(component, "Component"))
    ## theta
    stopifnot(is.double(theta))
    stopifnot(!any(is.na(theta)))
    stopifnot(all(theta >= 0))
    ## iteratorComp
    stopifnot(is(iteratorComp, "CohortIteratorComponent"))
    ## iExpFirst
    stopifnot(identical(length(iExpFirst), 1L))
    stopifnot(is.integer(iExpFirst))
    stopifnot(!is.na(iExpFirst))
    stopifnot(iExpFirst > 0L)
    ## exposure
    stopifnot(is(exposure, "Exposure"))
    stopifnot(is.double(exposure))
    stopifnot(!any(is.na(exposure)))
    stopifnot(all(exposure >= 0))
    ## iteratorExposure
    stopifnot(methods::is(iteratorExposure, "CohortIteratorComponent"))
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iCell and component
    stopifnot(iCell <= length(component))
    ## iExpFirst and exposure
    stopifnot(iExpFirst <= length(exposure))
    ## component and theta
    stopifnot(identical(length(component), length(theta)))
    if (useC) {
        .Call(diffLogDensExpOneComp_R,
              iCell, hasAge, ageTimeStep, updatedPopn,
              component, theta, iteratorComp,
              iExpFirst, exposure, iteratorExposure, diff)
    }
    else {
        iteratorComp <- resetCC(iteratorComp, i = iCell)
        iteratorExposure <- resetCC(iteratorExposure, i = iExpFirst)
        ans <- 0
        repeat {
            i.c <- iteratorComp@i
            i.e <- iteratorExposure@i
            comp.curr <- component[i.c]
            theta.curr <- theta[i.c]
            if (hasAge)
                diff.exposure <- 0.5 * diff * ageTimeStep
            else {
                is.first.cell <- i.c == iCell
                if (is.first.cell && !updatedPopn)
                    diff.exposure <- 0.5 * diff * ageTimeStep
                else
                    diff.exposure <- diff * ageTimeStep
            }
            exposure.curr <- exposure[i.e]
            exposure.prop <- exposure.curr + diff.exposure
            if ((comp.curr > 0L) && !(exposure.prop > 0)) ## testing for exposure.prop == 0, since exposure.prop should be >= 0
                return(-Inf)
            diff.log.lik <- (dpois(x = comp.curr,
                                   lambda = theta.curr * exposure.prop,
                                   log = TRUE)
                - dpois(x = comp.curr,
                        lambda = theta.curr * exposure.curr,
                        log = TRUE))
            ans <- ans + diff.log.lik
            if (iteratorComp@finished)
                break
            iteratorComp <- advanceCC(iteratorComp)
            iteratorExposure <- advanceCC(iteratorExposure)
        }
        ans
    }
}
*/



/*
## READY_TO_TRANSLATE
## HAS_TESTS
## function called only if component uses exposure
## (otherwise ratios cancel)
diffLogDensJumpOrigDest <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpOrigDest_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        exposure <- combined@exposure
        expected.exposure <- combined@expectedExposure
        i.cell <- combined@iCell
        i.exposure <- combined@iExposure
        diff <- combined@diffProp
        has.age <- combined@hasAge@.Data
        age.time.step <- combined@ageTimeStep
        theta.cell <- theta[i.cell]
        exposure.cell.curr <- exposure[i.exposure]
        exposure.cell.jump <- expected.exposure[i.exposure]
        if (has.age) {
            is.lower.triangle <- combined@isLowerTriangle
            if (is.lower.triangle)
                exposure.cell.prop <- exposure.cell.curr - 0.5 * diff * age.time.step
            else
                exposure.cell.prop <- exposure.cell.curr
        }
        else
            exposure.cell.prop <- exposure.cell.curr - 0.5 * diff * age.time.step
        lambda.dens.prop <- theta.cell * exposure.cell.prop
        lambda.jump <- theta.cell * exposure.cell.jump
        val.curr <- component[i.cell]
        val.prop <- val.curr + diff
        diff.log.dens <- (dpois(x = val.prop, lambda = lambda.dens.prop, log = TRUE)
            - dpois(x = val.curr, lambda = lambda.dens.prop, log = TRUE))
        diff.log.jump <- (dpois(x = val.curr, lambda = lambda.jump, log = TRUE)
            - dpois(x = val.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}
*/



/*
## READY_TO_TRANSLATE
## HAS_TESTS
## Difference in log-density of current values for
## all components attributable to change in exposure,
## where the change in exposure is due to a change in
## a cell in an Orig-Dest, Pool, or Net component
## (ie a component that potentially affects two cohorts).
diffLogDensExpOrigDestPoolNet <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensExpOrigDestPoolNet_R, combined)
    }
    else {
        components <- combined@account@components
        iterators.comp <- combined@iteratorsComp
        model.uses.exposure <- combined@modelUsesExposure
        mappings.from.exp <- combined@mappingsFromExp
        i.exp.first.orig <- combined@iExpFirst
        i.exp.first.dest <- combined@iExpFirstOther
        iterator.exposure <- combined@iteratorExposure
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        i.births <- combined@iBirths
        i.par.ch <- combined@iParCh
        exposure <- combined@exposure
        systemModels <- combined@systemModels
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        no.exposure.affected <- (i.exp.first.orig == 0L) || (i.exp.first.orig == i.exp.first.dest)
        if (no.exposure.affected)
            return(0)
        ans <- 0
        comp.is.int.net <- i.comp == i.int.net
        if (comp.is.int.net) {
            diff.orig <- diff
            diff.dest <- -diff
        }
        else {
            diff.orig <- -diff
            diff.dest <- diff
        }
        for (i in seq_along(components)) {
            if (model.uses.exposure[i + 1]) { ## note that 'net' components never use exposure
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping.from.exp <- mappings.from.exp[[i]]
                is.orig.dest <- i == i.orig.dest
                is.pool <- i == i.pool
                is.births <- i == i.births
                is.par.ch <- i == i.par.ch
                if (is.orig.dest || is.pool) {
                    i.cell.orig <- getICellCompFromExp(i = i.exp.first.orig,
                                                       mapping = mapping.from.exp)
                    i.cell.dest <- getICellCompFromExp(i = i.exp.first.dest,
                                                       mapping = mapping.from.exp)
                    diff.log.orig <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.orig,
                                                                        hasAge = has.age,
                                                                        ageTimeStep = age.time.step,
                                                                        updatedPopn = FALSE,
                                                                        component = component,
                                                                        theta = theta,
                                                                        iteratorComp = iterator.comp,
                                                                        iExpFirst = i.exp.first.orig,
                                                                        exposure = exposure,
                                                                        iteratorExposure = iterator.exposure,
                                                                        diff = diff.orig)
                    if (is.infinite(diff.log.orig))
                        return(diff.log.orig)
                    ans <- ans + diff.log.orig
                    diff.log.dest <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.dest,
                                                                        hasAge = has.age,
                                                                        ageTimeStep = age.time.step,
                                                                        updatedPopn = FALSE,
                                                                        component = component,
                                                                        theta = theta,
                                                                        iteratorComp = iterator.comp,
                                                                        iExpFirst = i.exp.first.dest,
                                                                        exposure = exposure,
                                                                        iteratorExposure = iterator.exposure,
                                                                        diff = diff.dest)
                    if (is.infinite(diff.log.dest))
                        return(diff.log.dest)
                    ans <- ans + diff.log.dest
                }
                else if (is.births) {
                    i.cell.orig <- getICellBirthsFromExp(i = i.exp.first.orig,
                                                         mapping = mapping.from.exp)
                    i.cell.dest <- getICellBirthsFromExp(i = i.exp.first.dest,
                                                         mapping = mapping.from.exp)
                    cell.is.affected <- i.cell.orig > 0L
                    if (cell.is.affected) {
                        if (is.par.ch) {
                            diff.log.orig <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.orig,
                                                                                hasAge = has.age,
                                                                                ageTimeStep = age.time.step,
                                                                                updatedPopn = FALSE,
                                                                                component = component,
                                                                                theta = theta,
                                                                                iteratorComp = iterator.comp,
                                                                                iExpFirst = i.exp.first.orig,
                                                                                exposure = exposure,
                                                                                iteratorExposure = iterator.exposure,
                                                                                diff = diff.orig)
                            if (is.infinite(diff.log.orig))
                                return(diff.log.orig)
                            ans <- ans + diff.log.orig
                            diff.log.dest <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.dest,
                                                                                hasAge = has.age,
                                                                                ageTimeStep = age.time.step,
                                                                                updatedPopn = FALSE,
                                                                                component = component,
                                                                                theta = theta,
                                                                                iteratorComp = iterator.comp,
                                                                                iExpFirst = i.exp.first.dest,
                                                                                exposure = exposure,
                                                                                iteratorExposure = iterator.exposure,
                                                                                diff = diff.dest)
                            if (is.infinite(diff.log.dest))
                                return(diff.log.dest)
                            ans <- ans + diff.log.dest
                        }
                        else {
                            diff.log.orig <- diffLogDensExpOneComp(iCell = i.cell.orig,
                                                                   hasAge = has.age,
                                                                   ageTimeStep = age.time.step,
                                                                   updatedPopn = FALSE,
                                                                   component = component,
                                                                   theta = theta,
                                                                   iteratorComp = iterator.comp,
                                                                   iExpFirst = i.exp.first.orig,
                                                                   exposure = exposure,
                                                                   iteratorExposure = iterator.exposure,
                                                                   diff = diff.orig)
                            if (is.infinite(diff.log.orig))
                                return(diff.log.orig)
                            ans <- ans + diff.log.orig
                            diff.log.dest <- diffLogDensExpOneComp(iCell = i.cell.dest,
                                                                   hasAge = has.age,
                                                                   ageTimeStep = age.time.step,
                                                                   updatedPopn = FALSE,
                                                                   component = component,
                                                                   theta = theta,
                                                                   iteratorComp = iterator.comp,
                                                                   iExpFirst = i.exp.first.dest,
                                                                   exposure = exposure,
                                                                   iteratorExposure = iterator.exposure,
                                                                   diff = diff.dest)
                            if (is.infinite(diff.log.dest))
                                return(diff.log.dest)
                            ans <- ans + diff.log.dest
                        }
                    }
                }
                else { ## is net
                    i.cell.orig <- getICellCompFromExp(i = i.exp.first.orig,
                                                       mapping = mapping.from.exp)
                    i.cell.dest <- getICellCompFromExp(i = i.exp.first.dest,
                                                       mapping = mapping.from.exp)
                    diff.log.orig <- diffLogDensExpOneComp(iCell = i.cell.orig,
                                                           hasAge = has.age,
                                                           ageTimeStep = age.time.step,
                                                           updatedPopn = FALSE,
                                                           component = component,
                                                           theta = theta,
                                                           iteratorComp = iterator.comp,
                                                           iExpFirst = i.exp.first.orig,
                                                           exposure = exposure,
                                                           iteratorExposure = iterator.exposure,
                                                           diff = diff.orig)
                    if (is.infinite(diff.log.orig))
                        return(diff.log.orig)
                    ans <- ans + diff.log.orig
                    diff.log.dest <- diffLogDensExpOneComp(iCell = i.cell.dest,
                                                           hasAge = has.age,
                                                           ageTimeStep = age.time.step,
                                                           updatedPopn = FALSE,
                                                           component = component,
                                                           theta = theta,
                                                           iteratorComp = iterator.comp,
                                                           iExpFirst = i.exp.first.dest,
                                                           exposure = exposure,
                                                           iteratorExposure = iterator.exposure,
                                                           diff = diff.dest)
                    if (is.infinite(diff.log.dest))
                        return(diff.log.dest)
                    ans <- ans + diff.log.dest
                }
            }
        }
        ans
    }
}
*/



/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensJumpPoolWithExpose <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpPoolWithExpose_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        exposure <- combined@exposure
        expected.exposure <- combined@expectedExposure
        i.cell.out <- combined@iCell
        i.cell.in <- combined@iCellOther
        i.exposure.out <- combined@iExposure
        i.exposure.in <- combined@iExposureOther
        diff <- combined@diffProp
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        theta.out <- theta[i.cell.out]
        theta.in <- theta[i.cell.in]
        exposure.out.curr <- exposure[i.exposure.out]
        exposure.in.curr <- exposure[i.exposure.in]
        exposure.out.jump <- expected.exposure[i.exposure.out]
        if (has.age) {
            is.lower.triangle <- combined@isLowerTriangle
            if (is.lower.triangle) {
                exposure.out.prop <- exposure.out.curr - 0.5 * diff * age.time.step
                exposure.in.prop <- exposure.in.curr + 0.5 * diff * age.time.step
            }
            else {
                exposure.out.prop <- exposure.out.curr
                exposure.in.prop <- exposure.in.curr
            }
        }
        else {
            exposure.out.prop <- exposure.out.curr - 0.5 * diff * age.time.step
            exposure.in.prop <- exposure.in.curr + 0.5 * diff * age.time.step
        }
        lambda.dens.out.prop <- theta.out * exposure.out.prop
        lambda.dens.in.prop <- theta.in * exposure.in.prop
        lambda.dens.out.curr <- theta.out * exposure.out.curr
        lambda.dens.in.curr <- theta.in * exposure.in.curr
        lambda.jump <- theta.out * exposure.out.jump
        val.out.curr <- component[i.cell.out]
        val.in.curr <- component[i.cell.in]
        val.out.prop <- val.out.curr + diff
        val.in.prop <- val.in.curr + diff
        diff.log.dens <- (dpois(x = val.out.prop, lambda = lambda.dens.out.prop, log = TRUE)
                          - dpois(x = val.out.curr, lambda = lambda.dens.out.curr, log = TRUE)
                          + dpois(x = val.in.prop, lambda = lambda.dens.in.prop, log = TRUE)
                          - dpois(x = val.in.curr, lambda = lambda.dens.in.curr, log = TRUE))
        diff.log.jump <- (dpois(x = val.out.curr, lambda = lambda.jump, log = TRUE)
                          - dpois(x = val.out.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}
*/



/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensJumpPoolNoExpose <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpPoolNoExpose_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        i.cell.in <- combined@iCellOther
        diff <- combined@diffProp
        theta.in <- theta[i.cell.in]
        val.in.curr <- component[i.cell.in]
        val.in.prop <- val.in.curr + diff
        (dpois(x = val.in.prop, lambda = theta.in, log = TRUE)
            - dpois(x = val.in.curr, lambda = theta.in, log = TRUE))
    }
}
*/



/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensJumpNet <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpNet_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        model <- combined@systemModels[[i.comp + 1L]]
        theta <- model@theta
        varsigma <- model@varsigma
        w <- model@w
        i.cell.sub <- combined@iCellOther
        diff <- combined@diffProp
        mean.sub <- theta[i.cell.sub]
        w.sub <- w[i.cell.sub]
        sd.sub <- varsigma / sqrt(w.sub)
        val.sub.curr <- component[i.cell.sub]
        val.sub.prop <- val.sub.curr - diff
        (dnorm(x = val.sub.prop,
               mean = mean.sub,
               sd = sd.sub,
               log = TRUE)
            - dnorm(x = val.sub.curr,
                    mean = mean.sub,
                    sd = sd.sub,
                    log = TRUE))
    }
}
*/



/*
## READY_TO_TRANSLATE
## HAS_TESTS
## function called only if component uses exposure
## (otherwise ratios cancel)
diffLogDensJumpComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpComp_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        exposure <- combined@exposure
        expected.exposure <- combined@expectedExposure
        i.cell <- combined@iCell
        i.exposure <- combined@iExposure
        diff <- combined@diffProp
        has.age <- combined@hasAge@.Data
        age.time.step <- combined@ageTimeStep
        is.increment <- combined@isIncrement
        theta.cell <- theta[i.cell]
        exposure.cell.curr <- exposure[i.exposure]
        exposure.cell.jump <- expected.exposure[i.exposure]
        if (has.age) {
            is.lower.triangle <- combined@isLowerTriangle
            if (is.lower.triangle) {
                if (is.increment[i.comp])
                    exposure.cell.prop <- exposure.cell.curr + 0.5 * diff * age.time.step
                else
                    exposure.cell.prop <- exposure.cell.curr - 0.5 * diff * age.time.step
            }
            else
                exposure.cell.prop <- exposure.cell.curr
        }
        else {
            if (is.increment[i.comp])
                exposure.cell.prop <- exposure.cell.curr + 0.5 * diff * age.time.step
            else
                exposure.cell.prop <- exposure.cell.curr - 0.5 * diff * age.time.step
        }
        lambda.dens.prop <- theta.cell * exposure.cell.prop
        lambda.jump <- theta.cell * exposure.cell.jump
        val.curr <- component[i.cell]
        val.prop <- val.curr + diff
        diff.log.dens <- (dpois(x = val.prop, lambda = lambda.dens.prop, log = TRUE)
            - dpois(x = val.curr, lambda = lambda.dens.prop, log = TRUE))
        diff.log.jump <- (dpois(x = val.curr, lambda = lambda.jump, log = TRUE)
            - dpois(x = val.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}
*/



/*
## READY_TO_TRANSLATE
## HAS_TESTS
## Difference in log-density of current values for
## all components attributable to change in exposure,
## where change in exposure due to change in
## cell in an ordinary component (ie a component
## that only potentially affects one cohort)
diffLogDensExpComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensExpComp_R, combined)
    }
    else {
        components <- combined@account@components
        iterators.comp <- combined@iteratorsComp
        model.uses.exposure <- combined@modelUsesExposure
        mappings.from.exp <- combined@mappingsFromExp
        i.exp.first <- combined@iExpFirst
        iterator.exposure <- combined@iteratorExposure
        is.increment <- combined@isIncrement
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.births <- combined@iBirths
        i.par.ch <- combined@iParCh
        exposure <- combined@exposure
        systemModels <- combined@systemModels
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        no.exposure.affected <- i.exp.first == 0L
        if (no.exposure.affected)
            return(0)
        ans <- 0
        if (!is.increment[i.comp])
            diff <- -diff
        for (i in seq_along(components)) {
            if (model.uses.exposure[i + 1L]) { ## note that 'net' components never use exposure
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping.from.exp <- mappings.from.exp[[i]]
                is.orig.dest <- i == i.orig.dest
                is.pool <- i == i.pool
                is.births <- i == i.births
                is.par.ch <- i == i.par.ch
                if (is.orig.dest || is.pool) {
                    i.cell <- getICellCompFromExp(i = i.exp.first,
                                                  mapping = mapping.from.exp)
                    diff.log <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell,
                                                                   hasAge = has.age,
                                                                   ageTimeStep = age.time.step,
                                                                   updatedPopn = FALSE,
                                                                   component = component,
                                                                   theta = theta,
                                                                   iteratorComp = iterator.comp,
                                                                   iExpFirst = i.exp.first,
                                                                   exposure = exposure,
                                                                   iteratorExposure = iterator.exposure,
                                                                   diff = diff)
                    if (is.infinite(diff.log))
                        return(diff.log)
                    ans <- ans + diff.log
                }
                else if (is.births) {
                    i.cell <- getICellBirthsFromExp(i = i.exp.first,
                                                    mapping = mapping.from.exp)
                    cell.is.affected <- i.cell > 0L
                    if (cell.is.affected) {
                        if (is.par.ch)
                            diff.log <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell,
                                                                           hasAge = has.age,
                                                                           ageTimeStep = age.time.step,
                                                                           updatedPopn = FALSE,
                                                                           component = component,
                                                                           theta = theta,
                                                                           iteratorComp = iterator.comp,
                                                                           iExpFirst = i.exp.first,
                                                                           exposure = exposure,
                                                                           iteratorExposure = iterator.exposure,
                                                                           diff = diff)
                        else
                            diff.log <- diffLogDensExpOneComp(iCell = i.cell,
                                                              hasAge = has.age,
                                                              ageTimeStep = age.time.step,
                                                              updatedPopn = FALSE,
                                                              component = component,
                                                              theta = theta,
                                                              iteratorComp = iterator.comp,
                                                              iExpFirst = i.exp.first,
                                                              exposure = exposure,
                                                              iteratorExposure = iterator.exposure,
                                                              diff = diff)
                        if (is.infinite(diff.log))
                            return(diff.log)
                        ans <- ans + diff.log
                    }
                }
                else {
                    i.cell <- getICellCompFromExp(i = i.exp.first,
                                                  mapping = mapping.from.exp)
                    diff.log <- diffLogDensExpOneComp(iCell = i.cell,
                                                      hasAge = has.age,
                                                      ageTimeStep = age.time.step,
                                                      updatedPopn = FALSE,
                                                      component = component,
                                                      theta = theta,
                                                      iteratorComp = iterator.comp,
                                                      iExpFirst = i.exp.first,
                                                      exposure = exposure,
                                                      iteratorExposure = iterator.exposure,
                                                      diff = diff)
                    if (is.infinite(diff.log))
                        return(diff.log)
                    ans <- ans + diff.log
                }
            }
        }
        ans
    }
}
*/
