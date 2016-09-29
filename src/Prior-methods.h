
#ifndef __PRIOR_METHODS_H__
#define __PRIOR_METHODS_H__

    #include <Rinternals.h>
    
    void updateBeta_AR1_Internal(SEXP beta_R, SEXP prior_R,
                        double *vbar, int n, double sigma);
                        
                        
#endif
