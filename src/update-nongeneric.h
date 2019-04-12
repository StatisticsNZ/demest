
#ifndef __UPDATE_NONGENERIC_H__
#define __UPDATE_NONGENERIC_H__


    #include <Rinternals.h>

    /* updatePhi etc code */
    #define K_MAX_ATTEMPTS 1000


    void updateMu(SEXP object_R);

    void updateBetas(SEXP object_R);

    void updateBetasGibbs(SEXP object_R);

    void updateBetasHMC(SEXP object_R);

    void updateBetasOneStep(SEXP object_R, double stepSize);

    void updateBetasWhereBetaEqualsMean(SEXP object_R);

    void updateGradientBetas(SEXP object_R);

    void updateLogPostBetas(SEXP object_R);

    void updateMeansBetas(SEXP object_R);

    void updateMomentumOneStep(SEXP object_R, double stepSize, int isFirstLast);

    void updateVariancesBetas(SEXP object_R);

    void updateSigma_Varying(SEXP object);
    
    void updatePriorsBetas(SEXP object_R);

	
#endif
