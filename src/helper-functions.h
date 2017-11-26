
#ifndef __HELPER_FUNCIONS_H__
#define __HELPER_FUNCIONS_H__

    /* threshold for calculation in dpoibin */
    #define THRESHOLD_POIBIN 1000 
    
    /* for Newton's method rootfinding code */
    #define K_TOLERANCE 1e-20
    #define K_EPSILON 1e-15
    #define K_MAX_ITER 1000
    #define K_CUTOFF_CONV_MODE_PHI 0.0001
    
    #define RNORMTRUNC_A 0.4
    #define RNORMTRUNC_TOL 2.05

    #define DEFAULT_LOGPOSTPHI 0.0001

    #include <Rinternals.h>
    
    /* utility functions for debugging printing */
    void printDblArray(double *a, int len);
    void printIntArray(int *a, int len);
    
    void getMu(double *mu, int n, SEXP betas_R, SEXP iterator_R);

    void getRnormTruncated(double* ans, int n, double* mean, double* sd, 
                        double lower, double upper, double tolerance,
                        int maxAttempt,
                        int uniform);
    
    double getRnormTruncatedSingle(double mean, double sd, 
                double lowerPlusTol, double upperMinusTol,
                int maxAttempt,
                int uniform);
    
    double getRtnorm1_x(double bnd1, double bnd2);
    
    void getTwoMultinomialProposalsNoExp (int *yProp,
                    int *y, double *theta, int ir, int ir_other);
    
    void getTwoMultinomialProposalsWithExp (int *yProp,
                    int *y, double *theta, double *exposure,
                    int ir, int ir_other);
    
    int intersect(int* intersect, int nIntersect,
                int* inputFirst, int nInputFirst,
                int* inputSecond, int nInputSecond);
    
    void betaHat(double *betaHat, SEXP prior_R, int J);
    void betaHat_AlphaCrossInternal(double *betaHat, SEXP prior_R, int J);
    void betaHat_AlphaDLMInternal(double *betaHat, SEXP prior_R, int J);
    void betaHat_AlphaICARInternal(double *betaHat, SEXP prior_R, int J);
    void betaHat_AlphaMixInternal(double *beta_hat, SEXP prior_R, int J);
    void betaHat_CovariatesInternal(double *betaHat, SEXP prior_R, int J);
    void betaHat_SeasonInternal(double *betaHat, SEXP prior_R, int J);
    
    void getV_Internal(double *V, SEXP prior_R, int J);
                                
    void diff(double *in_out, int n, int order);
    
    void getVBar(double *vbar, int len_vbar, 
        SEXP betas_R, SEXP iteratorBetas_R, 
                double *theta, int n_theta, int n_betas,
                int iBeta, double (*g)(double));
    
    SEXP makeVBar_General(SEXP object, int iBeta, double (*g)(double));
    
    void getVBarAndN(double *vbar, int *n_vec, 
        int len_vbar, int *cellInLik, 
        SEXP betas_R, SEXP iteratorBetas_R, 
                double *theta, int n_theta, int n_betas,
                int iBeta, double (*g)(double),
                int usesBoxCoxTransform, double boxCoxParam);
    
    SEXP makeVBarAndN_General(SEXP object, int iBeta, double (*g)(double));
    
    double logit(double x);
    
    double identity(double x);
    
    void writeValuesToFileBin(FILE *fp, SEXP object_R);
    
    int makeNewFile(const char * filename);
    
    int addToFile(const char * filename, SEXP object_R);
    
    double diffLogLik(int *yProp, SEXP y_R, 
                    int *indices, int n_element_indices_y, 
                SEXP dataModels_R, SEXP datasets_R, SEXP transforms_R);
    
    void rmvnorm1_Internal(double *ans, double *mean, double *var, int n);
    
    void rmvnorm2_Internal(double *ans, double *mean, double *var);
    
    void getDataFromFile(double *ans,
                        const char *filename, 
                        int first, 
                        int length_data, 
                        int lengthIter, 
                        int n_iter,
                        int *iterations);
    
    int runifInt(int lessThan);
                    
    void getOneIterFromFile(double *ans,
                        const char *filename, 
                        int first, 
                        int length_data, 
                        int lengthIter, 
                        int iteration);
    
    void chooseICellOutInPoolInternal(int *ans, SEXP description_R);
    void chooseICellSubAddNetInternal(int *ans, SEXP description_R);
    
    void predictBeta_ExchFixed(double* beta, SEXP prior_R, int J);
    void predictBeta_KnownCertain(double* beta, SEXP prior_R, int J);
    void predictBeta_KnownUncertain(double* beta, SEXP prior_R, int J);
    void predictBeta_Zero(double* beta, SEXP prior_R, int J);
    void predictBeta_Default(double* beta, SEXP prior_R, int J);
    
#endif
