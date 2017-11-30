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

/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMovePool <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMovePool_R, combined)
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
        i.cell.out <- combined@iCell
        i.cell.in <- combined@iCellOther
        i.popn.out <- combined@iPopnNext
        i.popn.in <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.log.lik.cells <- diffLogLikCellsPool(diff = diff,
                                                  iCellOut = i.cell.out,
                                                  iCellIn = i.cell.in,
                                                  iComp = i.comp,
                                                  component = component,
                                                  dataModels = data.models,
                                                  datasets = datasets,
                                                  seriesIndices = series.indices,
                                                  transforms = transforms)
        if (is.infinite(diff.log.lik.cells))
            return(diff.log.lik.cells)
        diff.log.lik.popn <- diffLogLikPopnPair(diff = diff,
                                                iPopnOrig = i.popn.out,
                                                iPopnDest = i.popn.in,
                                                iterator = iterator,
                                                population = population,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        diff.log.lik.cells + diff.log.lik.popn
    }
}
*/


/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikCellsPool <- function(diff, iComp, iCellOut, iCellIn,
                               component, dataModels, datasets,
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
    stopifnot(iComp > 0L)
    ## iCellOut
    stopifnot(identical(length(iCellOut), 1L))
    stopifnot(is.integer(iCellOut))
    stopifnot(!is.na(iCellOut))
    stopifnot(iCellOut > 0L)
    ## iCellIn
    stopifnot(identical(length(iCellIn), 1L))
    stopifnot(is.integer(iCellIn))
    stopifnot(!is.na(iCellIn))
    stopifnot(iCellIn > 0L)
    ## component
    stopifnot(is(component, "Component"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, is, "Model")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, is, "CollapseTransformExtra")))
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and seriesIndices
    stopifnot(identical(length(dataModels), length(seriesIndices)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellsPool_R,
              diff, iComp, iCellOut, iCellIn, component, dataModels,
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
                ## 'diff' added to 'out' and 'in' cells
                diff.log.lik.out <- diffLogLikCellOneDataset(diff = diff,
                                                             iCell = iCellOut,
                                                             component = component,
                                                             model = model,
                                                             dataset = dataset,
                                                             transform = transform)
                if (is.infinite(diff.log.lik.out))
                    return(diff.log.lik.out)
                diff.log.lik.in <- diffLogLikCellOneDataset(diff = diff,
                                                            iCell = iCellIn,
                                                            component = component,
                                                            model = model,
                                                            dataset = dataset,
                                                            transform = transform)
                if (is.infinite(diff.log.lik.in))
                    return(diff.log.lik.in)
                ans <- ans + diff.log.lik.out + diff.log.lik.in
            }
        }
        ans
    }
}
*/


/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMoveNet <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMoveNet_R, combined)
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
        i.cell.add <- combined@iCell
        i.cell.sub <- combined@iCellOther
        i.popn.add <- combined@iPopnNext
        i.popn.sub <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.log.lik.cells <- diffLogLikCellsNet(diff = diff,
                                                 iCellAdd = i.cell.add,
                                                 iCellSub = i.cell.sub,
                                                 iComp = i.comp,
                                                 component = component,
                                                 dataModels = data.models,
                                                 datasets = datasets,
                                                 seriesIndices = series.indices,
                                                 transforms = transforms)
        if (is.infinite(diff.log.lik.cells))
            return(diff.log.lik.cells)
        ## 'diffLogLikPopnPair assumes 'diff' is subtracted from first cohort
        ## and added to second, which is what happens with orig-dest and pool.
        ## To instead add and subtract, we use -diff.
        diff.log.lik.popn <- diffLogLikPopnPair(diff = -diff, 
                                                iPopnOrig = i.popn.add,
                                                iPopnDest = i.popn.sub,
                                                iterator = iterator,
                                                population = population,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        diff.log.lik.cells + diff.log.lik.popn
    }
}
*/

double 
diffLogLikAccountMoveNet(SEXP combined_R)
{
    /*        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell.add <- combined@iCell
        i.cell.sub <- combined@iCellOther
        i.popn.add <- combined@iPopnNext
        i.popn.sub <- combined@iPopnNextOther
        diff <- combined@diffProp
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
    int iCellAdd_r = *INTEGER(GET_SLOT(combined_R, iCell_sym));
    int iCellSub_r = *INTEGER(GET_SLOT(combined_R, iCellOther_sym));
    int iPopnAdd_r = *INTEGER(GET_SLOT(combined_R, iPopnNext_sym));
    int iPopnSub_r = *INTEGER(GET_SLOT(combined_R, iPopnNextOther_sym));
    int diff = *INTEGER(GET_SLOT(combined_R, diffProp_sym));
    
    double ans = 0;
    
    /*diff.log.lik.cells <- diffLogLikCellsNet(diff = diff,
                                                 iCellAdd = i.cell.add,
                                                 iCellSub = i.cell.sub,
                                                 iComp = i.comp,
                                                 component = component,
                                                 dataModels = data.models,
                                                 datasets = datasets,
                                                 seriesIndices = series.indices,
                                                 transforms = transforms)
        if (is.infinite(diff.log.lik.cells))
            return(diff.log.lik.cells)
        */
    
    double diffLogLikCells = diffLogLikCellsNet( diff, iComp_r,
                                        iCellAdd_r, iCellSub_r, 
                                        component_R, 
                                        dataModels_R, datasets_R, 
                                        seriesIndices_R, transforms_R);
    if (R_finite(diffLogLikCells) ) {

        ans += diffLogLikCells;
        
        /*## 'diffLogLikPopnPair assumes 'diff' is subtracted from first cohort
        ## and added to second, which is what happens with orig-dest and pool.
        ## To instead add and subtract, we use -diff.
        diff.log.lik.popn <- diffLogLikPopnPair(diff = -diff, 
                                                iPopnOrig = i.popn.add,
                                                iPopnDest = i.popn.sub,
                                                iterator = iterator,
                                                population = population,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        */
        
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



/*
## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikCellsNet <- function(diff, iComp, iCellAdd, iCellSub,
                               component, dataModels, datasets,
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
    stopifnot(iComp > 0L)
    ## iCellAdd
    stopifnot(identical(length(iCellAdd), 1L))
    stopifnot(is.integer(iCellAdd))
    stopifnot(!is.na(iCellAdd))
    stopifnot(iCellAdd > 0L)
    ## iCellSub
    stopifnot(identical(length(iCellSub), 1L))
    stopifnot(is.integer(iCellSub))
    stopifnot(!is.na(iCellSub))
    stopifnot(iCellSub > 0L)
    ## component
    stopifnot(is(component, "Component"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, is, "Model")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, is, "CollapseTransformExtra")))
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and seriesIndices
    stopifnot(identical(length(dataModels), length(seriesIndices)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellsNet_R,
              diff, iComp, iCellAdd, iCellSub, component, dataModels,
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
                ## 'diff' added to 'add' cell and subtracted from 'sub' cell
                diff.log.lik.add <- diffLogLikCellOneDataset(diff = diff,
                                                             iCell = iCellAdd,
                                                             component = component,
                                                             model = model,
                                                             dataset = dataset,
                                                             transform = transform)
                if (is.infinite(diff.log.lik.add))
                    return(diff.log.lik.add)
                diff.log.lik.sub <- diffLogLikCellOneDataset(diff = -diff,
                                                             iCell = iCellSub,
                                                             component = component,
                                                             model = model,
                                                             dataset = dataset,
                                                             transform = transform)
                if (is.infinite(diff.log.lik.sub))
                    return(diff.log.lik.sub)
                ans <- ans + diff.log.lik.add + diff.log.lik.sub
            }
        }
        ans
    }
}        


*/


double 
diffLogLikCellsNet(int diff, int iComp_r, int iCellAdd_r, int iCellSub_r,  
                        SEXP component_R, SEXP dataModels_R, 
                        SEXP datasets_R, SEXP seriesIndices_R, 
                        SEXP transforms_R)
{
    /*    ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.comp <- seriesIndices[i.dataset] == iComp
            if (assoc.with.comp) {
                dataset <- datasets[[i.dataset]]
                model <- dataModels[[i.dataset]]
                transform <- transforms[[i.dataset]]
                ## 'diff' added to 'add' cell and subtracted from 'sub' cell
                diff.log.lik.add <- diffLogLikCellOneDataset(diff = diff,
                                                             iCell = iCellAdd,
                                                             component = component,
                                                             model = model,
                                                             dataset = dataset,
                                                             transform = transform)
                if (is.infinite(diff.log.lik.add))
                    return(diff.log.lik.add)
    */
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
