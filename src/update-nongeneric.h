
#ifndef __UPDATE_NONGENERIC_H__
#define __UPDATE_NONGENERIC_H__


    #include <Rinternals.h>

    /* updatePhi etc code */
    #define K_MAX_ATTEMPTS 1000


    void updateMu(SEXP object_R);

    void updateBetasWhereBetaEqualsMean(SEXP object_R);

    void updateLogPostBetas(SEXP object_R);

    void updateMeansBetas(SEXP object_R);

    void updateVariancesBetas(SEXP object_R);

    void updateSigma_Varying(SEXP object);
    
    void updateBetasAndPriorsBetas(SEXP object_R);

	
#endif
