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
