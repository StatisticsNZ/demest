
#ifndef __UPDATE_NONGENERIC_H__
#define __UPDATE_NONGENERIC_H__


    #include <Rinternals.h>

    /* updatePhi etc code */
    #define K_MAX_ATTEMPTS 1000

    void updateSigma_Varying_General(SEXP object, double (*g)(double));
    
    void updateBetasAndPriorsBetas_General(SEXP object_R, double (*g)(double));

	
#endif
