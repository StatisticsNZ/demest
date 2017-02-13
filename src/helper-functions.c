
#include "helper-functions.h"
#include "iterators-methods.h"
#include "Combined-methods.h"
#include "demest.h"

#include "R_ext/BLAS.h" 
/* for BLAS level 2 documention see www.netlib.org/blas/blas2-paper.ps */ 

#include "R_ext/Lapack.h" 

/* File "helper-functions.c" contains C versions of 
 * functions from "helper-functions.R".   */

/* utility functions for debugging printing */
void printDblArray(double *a, int len)
{   
    SEXP tmp_R;
    PROTECT(tmp_R = allocVector(REALSXP, len));
    double *tmp = REAL(tmp_R);
    memcpy(tmp, a, len*sizeof(double));
    PrintValue(tmp_R);
    UNPROTECT(1);
}

void printIntArray(int *a, int len)
{   
    SEXP tmp_R;
    PROTECT(tmp_R = allocVector(INTSXP, len));
    int *tmp = INTEGER(tmp_R);
    memcpy(tmp, a, len*sizeof(int));
    PrintValue(tmp_R);
    UNPROTECT(1);
}

SEXP 
makeMu(int n, SEXP betas_R, SEXP iterator_R)
{
    resetB(iterator_R);
    
    SEXP mu_R;
    PROTECT(mu_R = allocVector(REALSXP, n));
    double *mu = REAL(mu_R);
    
    getMu(mu, n, betas_R, iterator_R);
    
    UNPROTECT(1); /* mu_R */
    return mu_R;
}

void 
getMu(double *mu, int n, SEXP betas_R, SEXP iterator_R)
{
    resetB(iterator_R);
    
    int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym));

    int n_betas = LENGTH(betas_R);
    double* betas[n_betas]; /* array of pointers */
    
    for (int b = 0; b < n_betas; ++b) {
        betas[b] = REAL(VECTOR_ELT(betas_R, b));
    }
    
    for (int i = 0; i < n; ++i) {
        
        double thisMu = 0.0;
        for (int b = 0; b < n_betas; ++b) {
            thisMu += betas[b][indices[b]-1];
        }
        
        mu[i] = thisMu;
        
        advanceB(iterator_R);
    
    }
}



/* *************************************************************************** */
/*** RANDOM VARIATES ********************************************************* */
/* *************************************************************************** */

double
dpoibin1(int x, int size, double prob, int use_log)
{
    double lambda = (1 - prob) * size;
    double ans = 0.0;
    
    if (x > THRESHOLD_POIBIN) {
        double mean_binom = prob * size;
        double var_binom = prob * lambda;
        double mean_pois = lambda;
        double var_pois = lambda;
        double mean = mean_binom + mean_pois;
        double sd = sqrt(var_binom + var_pois);
        ans = dnorm(x, mean, sd, use_log);
    }
    else {
        double limit = x < size ? x : size;
        
        for (int i = 0; i <= limit; ++i) {
            ans += dbinom(i, size, prob, NOT_USE_LOG)
                * dpois(x - i, lambda, NOT_USE_LOG);
        }
        if (use_log)
            ans = log(ans);
    }
    return ans;
}


double
invlogit1(double x)
{
    double retValue = 0.0;
    if (x > 0) {
        retValue = 1/(1 + exp(-x));
    }
    else {
        retValue = exp(x)/(1 + exp(x));
    }
    
    return retValue;
}


int
rcateg1(double *cumProb)
{
    double u = runif(0.0, 1.0);
    
    int i = 0;
    
    while ( !(cumProb[i] > u) ) {
        i += 1;
    }
    
    return i; 
}


double
rinvchisq1(double df, double scale)
{
    double x = rgamma(df / 2.0, 2.0);
    
    return df * scale / x;
}

SEXP
rmvnorm1(SEXP mean_R, SEXP var_R)
{
    int n = LENGTH(mean_R);
    double *mean = REAL(mean_R);
    double *var = REAL(var_R);
    
    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, n));
    double *ans = REAL(ans_R);
    
    rmvnorm1_Internal(ans, mean, var, n);
    
    UNPROTECT(1);
    return ans_R;
}

/* ans must have space for n elements, mean has n elements, var is nxn */
void
rmvnorm1_Internal(double *ans, double *mean, double *var, int n)
{
    double *varchol = (double *)R_alloc((n*n), sizeof(double));
    
    /* copy var because dpotrf will change the matrix passed in*/
    memcpy(varchol, var, n*n*sizeof(double));
    
    /* get the rnorms and put into ans*/
    for (int i = 0; i < n; ++i) {
        ans[i] = rnorm(0.0, 1.0);
    }
    
    /* dpotrf: compute the Cholesky factorization of a real sym-
      metric positive definite matrix A 
      * UPLO, N, A, LDA, INFO */
    char uplo = 'U';
    int info = 0; /* could be changed by call */
    
    F77_CALL(dpotrf)(&uplo, &n, varchol, &n, &info);
    if (info) error("error in dpotrf in rnorm1: %d", info);
    /* on exit, varchol contains U from factorisation varchol = U**T*U*/                        
    
    /* things needed for dtrmv x = A*x calculation*/
    char transN = 'N'; /* no transposition */
    char diag = 'N'; /* not unit triangular */
    int inc_blas = 1;
    
    /*dtrmv triangular matrix - vector multiplication*/
    F77_CALL(dtrmv)(&uplo, &transN, &diag, &n, 
                    varchol, &n, ans, &inc_blas);   
    /* ans should now contain U*ans */
    
    /* add on the means*/
    for (int i = 0; i < n; ++i) {
        ans[i] += mean[i];
    }

}  


/* mean is 1x2, var is 2x2, answer is 1x2 */
SEXP
rmvnorm2(SEXP mean_R, SEXP var_R)
{
    int n = 2; 
    
    double *mean = REAL(mean_R);
    double *var = REAL(var_R);
    
    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, n));
    double *ans = REAL(ans_R);
    
    rmvnorm2_Internal(ans, mean, var);
    
    UNPROTECT(1);
    return ans_R;
}

void
rmvnorm2_Internal(double *ans, double *mean, double *var)
{
    /* tolerance for K */
    double k_tolerance = -1e-6; 

    double mean1 = mean[0];
    double v0 = var[0];
    double sd1 = sqrt(v0);
    
    double ans0 = rnorm(mean1, sd1);
    
    double mean2 = mean[1] + (var[1] / var[0]) * (ans0 - mean1);
   
    double v2 = var[2];
    double var2 = var[3] - v2 * v2 / v0;
    
    double sd2 = 0;
    if (var2 < k_tolerance) {
         error("'var' is invalid");
    }
    else if (var2 < 0) {
        var2 = 0;
    }
    else {
        sd2 = sqrt(var2);
    }
        
    ans[0] = ans0;
    ans[1] = rnorm(mean2, sd2);
}


/* not quite sure what the best way to handle inputs and outputs for 
 * rnormTruncated is until I see how it might be used in the C code
 * itself so I may adapt the current, rather clumsy, version later */
SEXP
rnormTruncated(int n, SEXP mean_R, SEXP sd_R, 
                double lower, double upper, double tolerance,
                int maxAttempt,
                int uniform)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, n));
    double *ans = REAL(ans_R);
    double *mean = REAL(mean_R);
    double *sd = REAL(sd_R);
        
    
    /* fills in ans */
    getRnormTruncated(ans, n, mean, sd,
                    lower, upper, tolerance,
                    maxAttempt, 
                    uniform);
    
    UNPROTECT(1);
    return ans_R;
}  

void 
getRnormTruncated(double* ans, int n, double* mean, double* sd, 
                double lower, double upper, double tolerance,
                int maxAttempt,
                int uniform)
{
    double lowerPlusTol = lower + tolerance;
    double upperMinusTol = upper - tolerance;
    
    for (int i = 0; i < n; ++i) {
        
        ans[i] = getRnormTruncatedSingle(mean[i], sd[i], 
                lowerPlusTol, upperMinusTol,
                maxAttempt, uniform);
    }
}                

double
getRnormTruncatedSingle(double mean, double sd, 
                double lowerPlusTol, double upperMinusTol,
                int maxAttempt,
                int uniform)
{
    int found = 0;
    int nAttempt = 0;
    
    double prop_value = 0.0;
    double ans= 0.0;
    
    while (!found && nAttempt < maxAttempt) {
        
      ++ nAttempt;
      prop_value = rnorm( mean, sd );
      found = ( (prop_value > lowerPlusTol) 
            && (prop_value < upperMinusTol) ) ;
    }
    if (found) {
      ans = prop_value;
    }
    else {
      if (uniform) {
        double lower_unif;
        double upper_unif;
        if (lowerPlusTol > mean) {
          lower_unif = lowerPlusTol;
          if (lowerPlusTol + sd < upperMinusTol) {
        upper_unif = lowerPlusTol + sd;
          }
          else {
        upper_unif = upperMinusTol;
          }
        }
        else {
          upper_unif = upperMinusTol;
          if (upperMinusTol - sd > lowerPlusTol) {
        lower_unif = upperMinusTol - sd;
          }
          else {
        lower_unif = lowerPlusTol;
          }
        }
        ans = runif(lower_unif, upper_unif);
      }
      else {
        error("failed to generate value within specified range");
      }
    }
    return ans;
}
    
        
double
rtnorm1(double mean, double sd, double lower, double upper)
{
    double a = RNORMTRUNC_A;
    double tol = RNORMTRUNC_TOL;
    
    double l = (lower - mean)/sd;
    double u = (upper - mean)/sd;
    
    double ans = 0.0;
    
    if ( l > a) {
        double x = getRtnorm1_x(l, u);
        ans = sqrt(2*x);
    }
    else if (u < (-a)) {
        double x = getRtnorm1_x(-u, -l);
        ans = -sqrt(2*x);
    }
    else {
        if (fabs(u - l) > tol) {
            double x = rnorm(0,1);
            while ( (x < l) || (x > u) ) {
                x = rnorm(0,1);
            }
            ans = x;
        }
        else {
            double pl = pnorm(l, 0, 1, 1, 0);
            double pu = pnorm(u, 0, 1, 1, 0);
            double unif = runif(0,1);
            double trans = pl + (pu - pl) * unif;
            ans = qnorm(trans, 0, 1, 1, 0);
        }
    }
    
    return ans * sd + mean;
}

double getRtnorm1_x(double bnd1, double bnd2)
{
    double c = (bnd1 * bnd1) / 2;
    double f = expm1(c - (bnd2 * bnd2) / 2);
    double x = c - log(1 + runif(0,1)*f);
    
    double u = runif(0,1);
    
    while ( (u * u * x) > c) {
        x = c - log(1 + runif(0,1)*f);
        u = runif(0,1); 
    }
    
    return x;
}        
    
    
/* new (R >= 3.0.0) R rpois just seems to use a cast from
 * the double return from rpois in C to get new integer
 * return value in R so I have done the same. */
int
rpoisTrunc1(double lambda, int lower, int upper, int maxAttempt)
{
    int finite_upper = ( (upper == NA_INTEGER) ? 0 : 1);
    
    int retValue = NA_INTEGER;
    
    if ( finite_upper && (upper == lower) ) {
        retValue = lower;
    }
    else { /* non-finite upper or finite with lower < upper */
    
        int found = 0;
        int n_attempt = 0;
    
        double prop_value = 0;
    
        while ( !found && (n_attempt < maxAttempt) ) {
            n_attempt += 1;
            prop_value = rpois(lambda);
            if (finite_upper) {
     
                found = !( (prop_value < lower) || (prop_value > upper));
            }
            else { /* non finite upper */
                
              found = !(prop_value < lower);
              
            }
        }
        if (found) {
            
            retValue = (int) prop_value;
        }
    }
    
    return retValue;    
}

/* helper function to make two multinomial proposals, no exposure.
 * Results go into yProp, which must be at least 2 in length.
 * ir and ir_other give index positions in y and theta. */
void 
getTwoMultinomialProposalsNoExp (int *yProp,
                    int *y, double *theta, int ir, int ir_other) 
{
    int nInd = 2;
    
    int size = (int) y[ir-1];
    size += (int) y[ir_other-1];
    
    #ifdef DEBUGGING
    PrintValue(ScalarInteger(400));
    PrintValue(ScalarInteger(size));
    #endif
    
    /* version of rmultinom in C gives rubbish 
     * if probabilities not normalised */
    double prob_total = theta[ir-1] + theta[ir_other - 1];
    if (!(prob_total > 0.0)) {
        error("In getTwoMultinomialProposalsNoExp, selected probs sum to approx 0");
    } 
    double prob[] = {theta[ir-1]/prob_total, theta[ir_other - 1]/prob_total};
    
    rmultinom(size, prob, nInd, yProp);
    /* after call, yProp contains nInd proposals */
        
    #ifdef DEBUGGING
    PrintValue(ScalarInteger(500));
    PrintValue(ScalarInteger(size));
    #endif
    
    #ifdef DEBUGGING
    for (int i = 0; i < nInd; ++i) {
        PrintValue(ScalarInteger(yProp[i]));
    } 
    #endif
}

/* helper function to make two multinomial proposals, with exposure.
 * Results go into yProp, which must be at least 2 in length.
 * Temporary results as ints stored in indices, which must be at least
 * 2 in length.
 * ir and ir_other give index positions in y and theta. */
void 
getTwoMultinomialProposalsWithExp (int *yProp,
                    int *y, double *theta, double *exposure,
                    int ir, int ir_other) 
{
    int nInd = 2;
    
    int size = (int) y[ir-1];
    size += (int) y[ir_other-1];
    /* version of rmultinom in C gives rubbish 
     * if probabilities not normalised */
    double prob[] = {theta[ir-1]*exposure[ir-1],
                    theta[ir_other - 1]*exposure[ir_other - 1]};
    double prob_total = prob[0] + prob[1];
    if (!(prob_total > 0.0)) {
        error("In getTwoMultinomialProposalsWithExp, selected probs sum to approx 0");
    }
    prob[0] /= prob_total; 
    prob[1] /= prob_total; 
    
    rmultinom(size, prob, nInd, yProp);
    /* after call, indices contains nInd proposals */
    
}

/* assumes that nIntersect >= min(nInputFirst, nInputSecond),
 * assumes that contents of inputFirst and inputSecond are all unique. 
 * Returns number of elements nFound in the intersection
 * and sets the first nFound elements of intersect to be these elements,
 * in the order in which they are found (remaining elments will be 0s) */
int intersect(int* intersect, int nIntersect,
                int* inputFirst, int nInputFirst,
                int* inputSecond, int nInputSecond)
{
    /* set all of intersect vector to 0*/
    memset(intersect, 0, nIntersect * sizeof(int)); 
    
    int nFound = 0;
    for (int i = 0; (i < nInputFirst && nFound < nIntersect); ++i) {
        
        int first = inputFirst[i];
        
        for (int j = 0; j < nInputSecond; ++j) {
            if (first == inputSecond[j]) {
                
                intersect[nFound] = first;
                /* increment nFound and 
                 * break out of j loop if we find match */
                ++nFound;
                break; 
            }
        }
    }
    return nFound;
}

/* *************************************************************************** */
/* ** UPDATING ************************************************************* */
/* *************************************************************************** */


void
betaHat(double *beta_hat, SEXP prior_R, int J)
{
    double hasAlphaMove = *LOGICAL(GET_SLOT(prior_R, hasAlphaMove_sym));
    double hasAlphaDLM = *LOGICAL(GET_SLOT(prior_R, hasAlphaDLM_sym));
    double hasAlphaICAR = *LOGICAL(GET_SLOT(prior_R, hasAlphaICAR_sym));
    double hasCovariates = *LOGICAL(GET_SLOT(prior_R, hasCovariates_sym));
    double hasSeason = *LOGICAL(GET_SLOT(prior_R, hasSeason_sym));
    
    memset(beta_hat, 0, J * sizeof(double));
    
    #ifdef DEBUGGING
    PrintValue(mkString("hasAlphaMove"));
    PrintValue(ScalarLogical(hasAlphaMove));
    PrintValue(mkString("hasAlphaDLM"));
    PrintValue(ScalarLogical(hasAlphaDLM));
    PrintValue(mkString("hasAlphaICAR"));
    PrintValue(ScalarLogical(hasAlphaICAR));
    PrintValue(mkString("hasCovariates"));
    PrintValue(ScalarLogical(hasCovariates));
    PrintValue(mkString("hasSeason"));
    PrintValue(ScalarLogical(hasSeason));
    #endif
    
    if(hasAlphaMove) {
        betaHat_AlphaCrossInternal(beta_hat, prior_R, J);
    }


    if(hasAlphaDLM) {
        betaHat_AlphaDLMInternal(beta_hat, prior_R, J);
    }

    if(hasAlphaICAR) {
        betaHat_AlphaICARInternal(beta_hat, prior_R, J);
    }
        
    if(hasCovariates) {
        betaHat_CovariatesInternal(beta_hat, prior_R, J);
    }  
    
    if(hasSeason) {
        betaHat_SeasonInternal(beta_hat, prior_R, J);
    }   
}


void
betaHat_AlphaCrossInternal(double *beta_hat, SEXP prior_R, int J)
{
    double *alphaCross = REAL(GET_SLOT(prior_R, alphaCross_sym));
    int *indicesCross = INTEGER(GET_SLOT(prior_R, indicesCross_sym));
    
    for (int j = 0; j < J; ++j) {
        int indexCross = indicesCross[j];
        if (!R_finite(indexCross)) {
            beta_hat[j] = ((indexCross < 0) ? R_NegInf : R_PosInf);
        }
        else {
            beta_hat[j] += alphaCross[indexCross - 1];
        }
    }
}

void
betaHat_AlphaDLMInternal(double *beta_hat, SEXP prior_R, int J)
{
    double *alphaDLM = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* AlongIterators */
    SEXP iteratorAlpha = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorV = GET_SLOT(prior_R, iteratorV_sym);
    resetA(iteratorAlpha);
    resetA(iteratorV);
    
    int *indicesAlpha = INTEGER(GET_SLOT(iteratorAlpha, indices_sym));
    int *indicesV = INTEGER(GET_SLOT(iteratorV, indices_sym));
    
    for (int l = 0; l < L; ++l) {
    
            for (int k = 0; k < K; ++k) {
            int iAlpha = indicesAlpha[k+1];
            int iBetaHat = indicesV[k];
            beta_hat[iBetaHat - 1] += alphaDLM[iAlpha - 1]; 
        }
        
        advanceA(iteratorAlpha);
        advanceA(iteratorV);
    }
}

void
betaHat_AlphaICARInternal(double *beta_hat, SEXP prior_R, int J)
{
    double *alphaICAR = REAL(GET_SLOT(prior_R, alphaICAR_sym));
    
    for (int j = 0; j < J; ++j) {
        
        beta_hat[j] += alphaICAR[j]; 
        
    }
}
 
/* store result of matrix-vector multiplication prior@Z * prior@eta */
void
betaHat_CovariatesInternal(double *beta_hat, SEXP prior_R, int J)
{
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    
    SEXP z_R = GET_SLOT(prior_R, Z_sym);
    
    int *z_dim =  INTEGER(GET_DIM(z_R));
    int P = z_dim[1];
    double *z = REAL(z_R);
    
    char transN = 'N'; /* no operation */
    double alpha_blas = 1.0;
    double beta_blas = 1.0; /* adds z * eta to current beta_hat */
    int inc = 1;
    
    /* drop(Z %*% eta) */
    F77_CALL(dgemv)(&transN, &J, &P, &alpha_blas, 
                z, &J, eta, &inc, &beta_blas, 
                beta_hat, &inc);    
    /* result is stored in beta_hat */
    
}


void
betaHat_SeasonInternal(double *beta_hat, SEXP prior_R, int J)
{
    /* s is FFBS list */
    SEXP s_R = GET_SLOT(prior_R, s_sym);
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* AlongIterators */
    SEXP iteratorS = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorV = GET_SLOT(prior_R, iteratorV_sym);
    resetA(iteratorS);
    resetA(iteratorV);
    
    int *indicesS = INTEGER(GET_SLOT(iteratorS, indices_sym));
    int *indicesV = INTEGER(GET_SLOT(iteratorV, indices_sym));
        
    for (int l = 0; l < L; ++l) {
        
        for (int k = 0; k < K; ++k) {
            int iS = indicesS[k+1];
            int iBetaHat = indicesV[k];
            double *isVec = REAL(VECTOR_ELT(s_R, iS-1));
            beta_hat[iBetaHat - 1] += isVec[0]; 
        }
        
        advanceA(iteratorS);
        advanceA(iteratorV);
    }
}

void
betaHatAlphaDLM(double *beta_hat, SEXP prior_R, int J)
{
    memset(beta_hat, 0, J * sizeof(double));
    
    double *alphaDLM = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* AlongIterators */
    SEXP iteratorAlpha = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorV = GET_SLOT(prior_R, iteratorV_sym);
    resetA(iteratorAlpha);
    resetA(iteratorV);

    int *indicesAlpha = INTEGER(GET_SLOT(iteratorAlpha, indices_sym));
    int *indicesV = INTEGER(GET_SLOT(iteratorV, indices_sym));
    
    for (int l = 0; l < L; ++l) {
    
        for (int k = 0; k < K; ++k) {
            int iAlpha = indicesAlpha[k+1];
            int iBetaHat = indicesV[k];
            beta_hat[iBetaHat - 1] += alphaDLM[iAlpha - 1]; 
        }
        
        advanceA(iteratorAlpha);
        advanceA(iteratorV);
    }
}

/* store result of matrix-vector multiplication prior@Z * prior@eta */
void
betaHatCovariates(double *beta_hat, SEXP prior_R, int J)
{
    memset(beta_hat, 0, J * sizeof(double));
    
    /*Z <- unname(prior@Z)
        eta <- prior@eta@.Data
        drop(Z %*% eta)*/
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    
    SEXP z_R = GET_SLOT(prior_R, Z_sym);
    
    int *z_dim =  INTEGER(GET_DIM(z_R));
    int P = z_dim[1];
    double *z = REAL(z_R);
    
    char transN = 'N'; /* no operation */
    double alpha_blas = 1.0;
    double beta_blas = 1.0; /* adds z * eta to current beta_hat */
    int inc = 1;
    
    /* drop(Z %*% eta) */
    F77_CALL(dgemv)(&transN, &J, &P, &alpha_blas, 
                z, &J, eta, &inc, &beta_blas, 
                beta_hat, &inc);    
    /* result is stored in beta_hat */
    
}


void
betaHatSeason(double *beta_hat, SEXP prior_R, int J)
{
    memset(beta_hat, 0, J * sizeof(double));
    
    /* s is FFBS list */
    SEXP s_R = GET_SLOT(prior_R, s_sym);
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* AlongIterators */
    SEXP iteratorS = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorV = GET_SLOT(prior_R, iteratorV_sym);
    resetA(iteratorS);
    resetA(iteratorV);
    
    int *indicesS = INTEGER(GET_SLOT(iteratorS, indices_sym));
    int *indicesV = INTEGER(GET_SLOT(iteratorV, indices_sym));
    
    for (int l = 0; l < L; ++l) {
        
        for (int k = 0; k < K; ++k) {
            int iS = indicesS[k+1];
            int iBetaHat = indicesV[k];
            double *isVec = REAL(VECTOR_ELT(s_R, iS-1));
            beta_hat[iBetaHat - 1] += isVec[0]; 
        }
        
        advanceA(iteratorS);
        advanceA(iteratorV);
    }
}


double
findOneRootLogPostSigmaNorm(double sigma0, double z, double A, double nu,
                            double V, int n, double min, double max)
{
    int keepGoing = 1;
    int kIter = 0;
    double nuPlus1 = nu + 1;
    double nuASq = nu * A * A;
    
    double retValue = -99.0;
    
    double minPlusBoundaries = min + K_EPSILON_BOUNDARIES;
    double maxMinusBoundaries = max - K_EPSILON_BOUNDARIES;
    
    while (keepGoing && kIter < K_MAX_ITER) {
       
        double sigma0Sq = sigma0 * sigma0;
        
        double f = -n*log(sigma0) - V/(2*sigma0Sq)
                    - nuPlus1/2 * log(sigma0Sq + nuASq) - z;
        double fprime = -n/sigma0 + V/(sigma0Sq*sigma0)
                    - nuPlus1*sigma0 / (sigma0Sq + nuASq);
        
        int deriv_near_zero = (fabs(fprime) < K_EPSILON_MAX);
        
        if ( deriv_near_zero ) {
            retValue = -1.0;
            keepGoing = 0;
        }
        
        else {
            double sigma1 = sigma0 - f/fprime;
            if (sigma1 < minPlusBoundaries) {
                sigma1 = minPlusBoundaries;
            }
            else if (sigma1 > maxMinusBoundaries) {
                sigma1 = maxMinusBoundaries;
            }
            else if (sigma1 - sigma0 > 1) { /* a temporary hack added by John */
              sigma1 = sigma0 + 1;      /* 30 April 2016 */
            }
        
            int sigma1_equals_sigma0
                = (fabs(sigma1 - sigma0) < K_TOLERANCE * fabs(sigma1));

            if (sigma1_equals_sigma0) {
                retValue = sigma1;
                keepGoing = 0;
            }
            else {
                sigma0 = sigma1;
            }
        }
        
        ++kIter;
    }
    
    return retValue;
}




double
findOneRootLogPostSigmaRobust(double sigma0, double z, double A, 
                            double nuBeta, double nuTau,
                            double V, int n, double min, double max)
{
    int keepGoing = 1;
    int kIter = 0;
    double n_nuBeta = n * nuBeta;
    double nuBeta_V = nuBeta * V;
    double nuBeta_V_div2 = nuBeta_V/2;
    double nuTauPlus1 = nuTau + 1;
    double nuTauASq = nuTau * A * A;
    
    double retValue = -99.0;
    
    double minPlusBoundaries = min + K_EPSILON_BOUNDARIES;
    double maxMinusBoundaries = max - K_EPSILON_BOUNDARIES;
    
    while (keepGoing && kIter < K_MAX_ITER) {
       
        double sigma0Sq = sigma0 * sigma0;
        
        double f = n_nuBeta*log(sigma0) - nuBeta_V_div2 * sigma0Sq 
                    - nuTauPlus1/2 * log(sigma0Sq + nuTauASq) - z;
        double fprime = n_nuBeta/sigma0 - nuBeta_V * sigma0 
                    - nuTauPlus1*sigma0 / (sigma0Sq + nuTauASq);
        
        int deriv_near_zero = (fabs(fprime) < K_EPSILON_MAX);
        
        if ( deriv_near_zero ) {
            retValue = -1.0; 
            keepGoing = 0;
        }
        
        else {
            double sigma1 = sigma0 - f/fprime;
            if (sigma1 < minPlusBoundaries) {
                sigma1 = minPlusBoundaries;
            }
            else if (sigma1 > maxMinusBoundaries) {
                sigma1 = maxMinusBoundaries;
            }
            else if (sigma1 - sigma0 > 1) { /* a temporary hack added by John */
              sigma1 = sigma0 + 1;      /* 30 April 2016 */
            }
            
            int sigma1_equals_sigma0 
                = ((fabs(sigma1 - sigma0) < K_TOLERANCE * fabs(sigma1)));
            
            if (sigma1_equals_sigma0) {
                retValue = sigma1; 
                keepGoing = 0;
            }
            else {
                sigma0 = sigma1;
            }
        }
        
        ++kIter;
    }
    
    return retValue;
}


SEXP
getV_R(SEXP prior_R)
{
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));
    SEXP V_R;
    PROTECT(V_R = allocVector(REALSXP, J));
    double *V = REAL(V_R);
    
    getV_Internal(V, prior_R, J); /* fill in V */
    
    UNPROTECT(1); /* V_R */

    return V_R;
    
}

/* V must have space for J elements */
void
getV_Internal(double *V, SEXP prior_R, int J)
{
    int isRobust = *INTEGER(GET_SLOT(prior_R, isRobust_sym));
    
    if (isRobust) {
        double *UBeta = REAL(GET_SLOT(prior_R, UBeta_sym));
        memcpy(V, UBeta, J*sizeof(double));
    }
    else {
    
        double tau = *REAL(GET_SLOT(prior_R, tau_sym));
        double tauSq = tau*tau;
        for (int j = 0; j < J; ++j) {
            V[j] = tauSq;
        }
    }
}


/* centre random walk in argument vec_R using interatorRW iterator_R */
SEXP
centerA(SEXP vec_R, SEXP iterator_R)
{
  
    double *vec = REAL(vec_R);
    
    SEXP indices_R =  GET_SLOT(iterator_R, indices_sym);
    int *indices = INTEGER(indices_R);
    int indices_len =  LENGTH(indices_R);
    
    resetA(iterator_R);
    
    int len_ans = LENGTH(vec_R);
    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, len_ans));
    double *ans = REAL(ans_R);
    
    /* zero contents of ans_R */
    memset(ans, 0, len_ans * sizeof(double));
    
    int n_classifying = len_ans/indices_len; /* integer division */
    
    while (n_classifying > 0) {
        
        double adj_vec_values = 0.0;
        for (int i = 0; i < indices_len; ++i) {
            adj_vec_values += vec[indices[i]-1];
        }
        adj_vec_values /= indices_len; /* = mean */
        for (int i = 0; i < indices_len; ++i) {
            
            ans[indices[i]-1] = vec[indices[i]-1] - adj_vec_values;
        }
        
        advanceA(iterator_R); 
        /* updates values pointed to by iWithin_ptr, iBetween_ptr */
        
        --n_classifying;
    } 
    UNPROTECT(1); /* ans_R */
    return ans_R;
}


/* The diff_R here is a wrapper around my actual C
version, which makes it replicate the R version of diff completely. */
SEXP
diff_R(SEXP vec_R, SEXP order_R)
{
    double *vec = REAL(vec_R);
    int n = LENGTH(vec_R);
    
    int order = *(INTEGER(order_R));
    
    double *work = (double *)R_alloc(n, sizeof(double));
    memcpy( work, vec, n * sizeof(double) );
    
    diff(work, n, order);
    
    /* first n-order elements of work are what we want */
    
    int len_ans = n - order;
    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, len_ans));
    double *ans = REAL(ans_R);
    memcpy( ans, work, len_ans * sizeof(double) );

    UNPROTECT(1); /* ans_R */
    return ans_R;
} 


/* This actual underlying C implementation of diff as used
 * for the random walk routines is not quite the same as the diff used
 * by R, because replicating the R version exactly is not very efficient
 * in the context in which diff is being used by diffRandomWalk.
 * 
 * This diff changes in_out in place so that the final answer
 * is contained in the first n-order elements of in_out are
 * replaced by the answer elements and the rest are just left, where
 * n is the length of in_out. 
 * 
 * Doing it this way avoids the need to set up and destroy a new
 * 'work' array for each call to diff (see Brendan's orginal version)
 * and works well given our iterator implemention because we have 
 * to transfer the outcome from diff into the correct positions 
 * in the answer vector anyway.
 *
 * in_out is a pointer to the start of an array of doubles,
 * of length n.
 * order is the order of differences to be found (eg 1 = first order 
 * differences, etc). 
 * 
 * preconditions: n >= order + 1
 * postconditions: the first n-order positions in in_out will 
 * contain the order-order differences between the values originally
 * in in_out.  The remaining values in in_out will be the original
 * values.
 */
void diff(double *in_out, int n, int order)
{
    for (int ord = 0; ord < order; ++ord) {
        for (int j = 0; j < n-1; ++j) {
            in_out[j] = in_out[j+1] - in_out[j];  
        }
        --n; /* one less to operate on */
        
    }
}

/* used for transformations in update models */
double logit(double x)
{
    return log(x/(1-x));
}

/* used for transformations in update models */
double identity(double x)
{
    return x;
}


double
logPostPhiMix(double phi, double *level, double meanLevel, int nAlong, 
                int indexClassMaxMix_r, double omega)
{
    double ans = DEFAULT_LOGPOSTPHI;
    
    if (fabs(phi) < 1) {
        
        double ratio = meanLevel / (1 - phi);
        double ansFirst = 0;
        double ansRest = 0;
        for (int iClass = 0; iClass < indexClassMaxMix_r; ++ iClass) {

            int iWtFirst = iClass * nAlong;
            double levelFirst = level[iWtFirst];
            double tmp = levelFirst - ratio;
            ansFirst += tmp*tmp;
            
            for (int iAlong = 1; iAlong < nAlong; ++ iAlong) {

                int iWtCurr = iClass * nAlong + iAlong;
                int iWtPrev = iWtCurr-1;
                double levelCurr = level[iWtCurr];
                double levelPrev = level[iWtPrev];
                double tmp = levelCurr - meanLevel - phi*levelPrev;
                ansRest += tmp*tmp;
            }
        }
        ansFirst *= (1 - phi*phi);
        ans = (ansFirst + ansRest) / (-2 * omega*omega);
       
    } /* else ans stays as default */

    return ans;
}


double
logPostPhiFirstOrderMix(double phi, double *level, double meanLevel, int nAlong, 
                int indexClassMaxMix_r, double omega)
{
    double ans = DEFAULT_LOGPOSTPHI;
    
    if (fabs(phi) < 1) {
        
        double ratio = meanLevel / (1 - phi);
        double ansFirst = 0;
        double ansRest = 0;
        for (int iClass = 0; iClass < indexClassMaxMix_r; ++ iClass) {
            
            int iWtFirst = iClass * nAlong;
            double levelFirst = level[iWtFirst];
            
            ansFirst += (levelFirst - ratio) * (phi * levelFirst + ratio);
            
            for (int iAlong = 1; iAlong < nAlong; ++ iAlong) {
            
                int iWtCurr = iClass * nAlong + iAlong;
                int iWtPrev = iWtCurr-1;
                double levelCurr = level[iWtCurr];
                double levelPrev = level[iWtPrev];
                ansRest += levelPrev * 
                            (levelCurr - meanLevel - phi * levelPrev);
            }
        }
        
        ans = (ansFirst + ansRest) / (omega*omega);
       
    } /* else ans stays as default */

    return ans;
}

double
logPostPhiSecondOrderMix(double phi, double *level, double meanLevel, int nAlong, 
                int indexClassMaxMix_r, double omega)
{
    double ans = DEFAULT_LOGPOSTPHI;
    
    if (fabs(phi) < 1) {
        double oneMinusPhi = 1 - phi;
        double ansFirst = -2 * indexClassMaxMix_r * meanLevel * meanLevel 
                    / (oneMinusPhi * oneMinusPhi * oneMinusPhi);
        double ansRest = 0;
        if (nAlong > 2) {
        
            for (int iClass = 0; iClass < indexClassMaxMix_r; ++ iClass) {
                
                for (int iAlong = 1; iAlong < nAlong-1; ++iAlong) {
                
                    int iWt = iClass * nAlong + iAlong;
                    double levelWt = level[iWt];
                    ansRest -= (levelWt * levelWt);
                }
            }
        }
        ans = (ansFirst + ansRest) / (omega*omega);
       
    } /* else ans stays as default */

    return ans;
}


double
makeLifeExpBirth(double *mx, double *nx, double *ax, int iAge0_r, int nAge)
{
    double ans = 0;
    int iAge0 = iAge0_r - 1; /* adjust R indexing for C */
    double lx_i = 1;
    
    for (int i = 0; i < nAge - 1; ++i) {
        
        double mx_i = mx[iAge0 + i];
        double nx_i = nx[i];
        double ax_i = ax[iAge0 + i];
        
        double qx_i = nx_i * mx_i / (1 + (nx_i - ax_i) * mx_i);
        double lx_iplus1 = lx_i * (1 - qx_i);
        double Lx_i = lx_iplus1 * nx_i + (lx_i - lx_iplus1) * ax_i;
        ans += Lx_i;
        lx_i = lx_iplus1;
        
    }
    
    double newMx_i = mx[iAge0 + nAge - 1];
    double newLx_i = lx_i / newMx_i;
    
    ans += newLx_i;
    
    return ans;
}

/* only here for testing makeVBar_General: 
 * the uber update model function effectively duplicates this */
SEXP
makeVBar_R(SEXP object, SEXP iBeta_R) 
{
    int iBeta = *(INTEGER(iBeta_R))-1;
        
    SEXP vbar_R = NULL;
        
    int i_method_model = *(INTEGER(GET_SLOT(object, iMethodModel_sym)));
        
    switch(i_method_model)
    {
        case 4: case 5: /*Normal */
            PROTECT(vbar_R = makeVBar_General(object, iBeta, identity));
            break;
        case 6: case 10: /* Poisson */
            PROTECT(vbar_R =  makeVBar_General(object, iBeta, log));
            break;
        case 9: /* Binomial */
            PROTECT(vbar_R = makeVBar_General(object, iBeta, logit));
            break;
        default:
            error("unknown iMethodModel: %d", i_method_model);
            break;
    }
    
    UNPROTECT(1); /* vbar_R */
    return vbar_R;
}

/* only used when testing makeVBar from R */
SEXP
makeVBar_General(SEXP object, int iBeta, double (*g)(double))
{
    SEXP theta_R = GET_SLOT(object, theta_sym);
    int n_theta = LENGTH(theta_R);
    double *theta = REAL(theta_R);
    
    SEXP betas_R = GET_SLOT(object, betas_sym);
    int n_betas = LENGTH(betas_R);

    SEXP iteratorBetas_R = GET_SLOT(object, iteratorBetas_sym); 
    
    int len_vbar = LENGTH(VECTOR_ELT(betas_R, iBeta));
    
    SEXP vbar_R;
    PROTECT(vbar_R = allocVector(REALSXP, len_vbar));
    double *vbar = REAL(vbar_R);
    
    getVBar(vbar, len_vbar,
                betas_R, iteratorBetas_R,
                theta, n_theta, n_betas,
                iBeta, g);
                
    UNPROTECT(1); /* vbar_R */
    return vbar_R;
}

void
getVBar(double *vbar, int len_vbar, 
        SEXP betas_R, SEXP iteratorBetas_R, 
                double *theta, int n_theta, int n_betas,
                int iBeta, double (*g)(double))
{
    resetB(iteratorBetas_R);
    int *indices = INTEGER(GET_SLOT(iteratorBetas_R, indices_sym));
    
    double* betas[n_betas]; /* array of pointers */
    for (int b = 0; b < n_betas; ++b) {
        betas[b] = REAL(VECTOR_ELT(betas_R, b));
    }
    
    /* zero contents of vbar */
    memset(vbar, 0, len_vbar * sizeof(double));
    
    double mult = len_vbar/(1.0*n_theta);
    
    for (int i = 0; i < n_theta; ++i) {
        
        int pos_ans = indices[iBeta] - 1;
        
        double set_pos = g( theta[i] );
        
        for (int b = 0; b < n_betas; ++b) {
            
            if (b == iBeta) continue; /* skip b == iBeta */
            int pos_other_beta = indices[b] - 1;
            
            set_pos -= betas[b][pos_other_beta];
        }
        
        /* ans[pos.ans] <- ans[pos.ans] - other.beta[pos.other.beta]
 */
        vbar[pos_ans] += set_pos * mult;
        
        advanceB(iteratorBetas_R);
    }
}

double
modePhiMix(double * level, double meanLevel, int nAlong,
              int indexClassMax, double omega, double tolerance)
{
    double kCutoffConvergenceModePhi = K_CUTOFF_CONV_MODE_PHI;
    double lengthStepInc = 0.001;
    double phiCurr = 0.9;
    double diffOuter = 1;
    
    while (diffOuter > kCutoffConvergenceModePhi) {
        double lengthStep = 0.1;
        double logPostCurr = logPostPhiMix(phiCurr, level, meanLevel, 
                nAlong, indexClassMax, omega);
        double diffInner = 0;
        
        double phiNew = 0;
        while (!(diffInner > 0) && (lengthStep > lengthStepInc)) {
            
            double logPostFirst = logPostPhiFirstOrderMix(phiCurr, level,
                            meanLevel, nAlong, indexClassMax, omega);
            double logPostSecond = logPostPhiSecondOrderMix(phiCurr, level,
                            meanLevel, nAlong, indexClassMax, omega);
            
            phiNew = phiCurr - lengthStep * logPostFirst/logPostSecond;
            
            if ( phiNew > (1 - tolerance) ) {
                phiNew = 1-tolerance;
            }
            else if (phiNew < ( -1 + tolerance) ) {
                phiNew = -1 + tolerance;
            }
            
            double logPostNew = logPostPhiMix(phiNew, level, meanLevel, 
                                    nAlong, indexClassMax, omega);
            
            diffInner = logPostNew - logPostCurr;
            lengthStep -= lengthStepInc;
        }
        
        diffOuter = fabs(phiNew - phiCurr);
        phiCurr = phiNew;
    }
    
    return phiCurr; /* same as phiNew */
}

double safeLogProp_Binomial(double logit_th_new, 
                            double logit_th_other_new,
                            double logit_th_old, 
                            double logit_th_other_old, 
                            double scale,
                            double weight,
                            double weight_other)
{
    double outside = 0.0;
    double coef_first = 0.0;
    double coef_second = 0.0;

    if (fabs(logit_th_new) > fabs(logit_th_other_new)) {
        double ltn = (logit_th_new > 0) ? -logit_th_new : logit_th_new;
        outside = (logit_th_new > 0) ? logit_th_new : -logit_th_new;
        double exp_ltn = exp(ltn);
        coef_first = exp(2*ltn) 
                + 2 * exp_ltn + 1;
        coef_second = exp(-logit_th_other_new + ltn)
            + 2 * exp_ltn
            + exp(logit_th_other_new + ltn);
    }
    else {
        double lton = (logit_th_other_new > 0) 
                        ? -logit_th_other_new : logit_th_other_new;
        outside = (logit_th_other_new > 0) 
                        ? logit_th_other_new : -logit_th_other_new;
        double exp_lton = exp(lton);
        coef_first = exp(-logit_th_new + lton)
            + 2 * exp_lton
            + exp(logit_th_new + lton);
        coef_second = exp(2*lton) 
                + 2 * exp_lton + 1;
        
    }
    double dens_first = dnorm(logit_th_new, logit_th_old, scale, NOT_USE_LOG);
    double dens_second = dnorm(logit_th_other_new, 
                                    logit_th_other_old, scale, NOT_USE_LOG);
    double weight_ratio = fabs(weight/weight_other);
    double retValue = outside + log(coef_first * dens_first 
                            + weight_ratio * coef_second * dens_second);
    return retValue;
        
}

double safeLogProp_Poisson(double log_th_new, 
                            double log_th_other_new,
                            double log_th_old, 
                            double log_th_other_old, 
                            double scale,
                            double weight,
                            double weight_other)
{
    double outside = 0.0;
    double coef_first = 0.0;
    double coef_second = 0.0;
    
    if ( (log_th_new < log_th_other_new) && (log_th_new < 0) ) {
        outside = -log_th_new;
        coef_first = 1.0;
        coef_second = exp(-log_th_other_new + log_th_new);
    }
    else {
        outside = 0;
        coef_first = exp(-log_th_new);
        coef_second = exp(-log_th_other_new);
    }
    double dens_first = dnorm(log_th_new, log_th_old, scale, NOT_USE_LOG);
    double dens_second = dnorm(log_th_other_new, 
                                    log_th_other_old, scale, NOT_USE_LOG);
    double weight_ratio = fabs(weight/weight_other);
    double retValue = outside + log(coef_first * dens_first 
                            + weight_ratio * coef_second * dens_second);
    return retValue;
        
}

void
predictAlphaDLMNoTrend(SEXP prior_R)
{
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym)); /* vector, length (K+1)L */
    
    double phi = *REAL(GET_SLOT(prior_R, phi_sym));
    double omega = *REAL(GET_SLOT(prior_R, omegaAlpha_sym));
    
    SEXP iterator_R = GET_SLOT(prior_R, iteratorState_sym);
    
    resetA(iterator_R);
    
    int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym)); 
    
    for (int l = 0; l < L; ++l) {
        
        for (int i = 0; i < K; ++i) {
            
            int k_curr = indices[i+1] - 1;
            int k_prev = indices[i] - 1;
            
            double mean = phi * alpha[k_prev];
            alpha[k_curr] = rnorm(mean, omega);
        }
            
        advanceA(iterator_R);
    }
    /* only alphaDLM gets updated in the prior */
}



void
predictAlphaDeltaDLMWithTrend(SEXP prior_R)
{
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym)); /* vector, length (K+1)L */
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym)); /* vector, length (K+1)L */
    
    double phi = *REAL(GET_SLOT(prior_R, phi_sym)); 
    double omegaAlpha = *REAL(GET_SLOT(prior_R, omegaAlpha_sym)); 
    double omegaDelta = *REAL(GET_SLOT(prior_R, omegaDelta_sym)); 
    
    SEXP iterator_R = GET_SLOT(prior_R, iteratorState_sym);
    
    resetA(iterator_R);
    
    int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym)); 

    for (int l = 0; l < L; ++l) {
        
        for (int i = 0; i < K; ++i) {
            
            int k_curr = indices[i+1] - 1;
            int k_prev = indices[i] - 1;
            
            double delta_k_prev = delta[k_prev];
            
            double meanDelta = phi * delta_k_prev;
            delta[k_curr] = rnorm(meanDelta, omegaDelta);
            
            double meanAlpha = alpha[k_prev] + delta_k_prev;
            alpha[k_curr] = rnorm(meanAlpha, omegaAlpha);
        }
            
        advanceA(iterator_R);
    }
}


void
predictBeta(double* beta, SEXP prior_R, int J)
{
    int i_method_prior = *(INTEGER(GET_SLOT(prior_R, iMethodPrior_sym)));

    switch(i_method_prior)
    {
        case 0:/*ExchFixed */
            predictBeta_ExchFixed(beta, prior_R, J);
            break;
        default:
            predictBeta_Default(beta, prior_R, J);;
            break;
    }
}

void
predictBeta_ExchFixed(double* beta, SEXP prior_R, int J)
{
    double sd = *REAL(GET_SLOT(prior_R, tau_sym));
    
    for (int j = 0; j < J; ++j) {
        beta[j] = rnorm(0, sd);
    }
}


void
predictBeta_Default(double* beta, SEXP prior_R, int J)
{
    double *work = (double*)R_alloc(2*J, sizeof(double));
    
    double *mean = work;
    double *var = work + J;
    betaHat(mean, prior_R, J);
    
    getV_Internal(var, prior_R, J);
    
    for (int j = 0; j < J; ++j) {
        beta[j] = rnorm(mean[j], sqrt(var[j]) );
    }
}

void
predictBetas(SEXP object_R)
{
    /* betas and priors both lists */
    SEXP betas_R = GET_SLOT(object_R, betas_sym);
    int nBetas = LENGTH(betas_R);
    
    SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
    
    /* a vector of logicals */
    int *betaIsPredicteds = INTEGER(GET_SLOT(object_R, betaIsPredicted_sym));
    
    for (int i = 0; i < nBetas; ++i) {
        if ( betaIsPredicteds[i] ) {
            
            SEXP this_beta_R = VECTOR_ELT(betas_R, i);
            int J = LENGTH(this_beta_R);
            double *this_beta = REAL(this_beta_R);
            
            predictBeta(this_beta, VECTOR_ELT(priors_R, i), J);
        }
    }
}

/* component_R is a polycomponent, forward_R is a logical */
void
predictPriorsBetas(SEXP object_R)
{
        
    /* a list of priors */
    SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
    /* a vector of logicals */
    int *betaIsPredicteds = INTEGER(GET_SLOT(object_R, betaIsPredicted_sym));
    
    int nPriors = LENGTH(priors_R);
    
    for (int i = 0; i < nPriors; ++i) {
        if ( betaIsPredicteds[i] ) {
            predictPrior(VECTOR_ELT(priors_R, i));
        }
    }
    
}

void
predictSeason(SEXP prior_R)
{
        
    /* s is FFBS list */
    SEXP s_R = GET_SLOT(prior_R, s_sym);
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
    double omega = *REAL(GET_SLOT(prior_R, omegaSeason_sym));
    
    /* AlongIterators */
    SEXP iteratorS = GET_SLOT(prior_R, iteratorState_sym);
    resetA(iteratorS);
    
    int *indices = INTEGER(GET_SLOT(iteratorS, indices_sym));
    
    for (int l = 0; l < L; ++l) {
        
        for (int k = 0; k < K; ++k) {
            
            int k_curr = indices[k + 1] - 1; /* C style indices */
            int k_prev = indices[k] - 1;
            
            double *s_curr = REAL(VECTOR_ELT(s_R, k_curr));
            double *s_prev = REAL(VECTOR_ELT(s_R, k_prev));
            
            double mean = s_prev[nSeason - 1];
            
            s_curr[0] = rnorm(mean, omega);

            /* copy nSeason-1 values from s_prev (starting at first)
             * to s_curr (starting at second) */
            memcpy((s_curr+1), s_prev, (nSeason-1)*sizeof(double));
                        
        }
        
        advanceA(iteratorS);

    }
    
}


void
predictUBeta(SEXP prior_R)
{
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuBeta_sym));
    double tau = *REAL(GET_SLOT(prior_R, tau_sym));
    
    double *U = REAL(GET_SLOT(prior_R, UBeta_sym));
    
    double scale = tau*tau;
    
    for (int j = 0; j < J; ++j) {
        
        U[j] = rinvchisq1(nu, scale);
    }

}


void
transferAlphaDelta0(double *state, double *values, int offset,
                    SEXP iteratorState_R, SEXP iteratorValues_R)
{
    
    resetA(iteratorValues_R);
    resetA(iteratorState_R);
    
    SEXP indicesValues_R = GET_SLOT(iteratorValues_R, indices_sym); 
    int *indicesValues = INTEGER(indicesValues_R); 
    int *indicesState = INTEGER(GET_SLOT(iteratorState_R, indices_sym)); 
    
    int n = *INTEGER(GET_SLOT(iteratorValues_R, nWithin_sym)) *
            *INTEGER(GET_SLOT(iteratorValues_R, nBetween_sym));
    
    int offset_adj = offset - 2; /* takes account offset - 1 and change to C index */
    
    int KPlusOne_Values = LENGTH(indicesValues_R) - 1;
    
    
    for (int i = 0; i < n; ++i) {
        int i_first_state = indicesState[0] - 1;
        int i_last_values = indicesValues[KPlusOne_Values] + offset_adj;
        
        state[i_first_state] = values[i_last_values];
        
        advanceA(iteratorValues_R);
        advanceA(iteratorState_R);
    }

}

void
transferSeason0(SEXP s_R, int nSeason, double *values, int offset,
                    SEXP iteratorState_R, SEXP iteratorValues_R)
{
    resetA(iteratorState_R);
    resetA(iteratorValues_R);
    
    SEXP indicesValues_R = GET_SLOT(iteratorValues_R, indices_sym);
    int *indicesValues = INTEGER(indicesValues_R); 
    int *indicesState = INTEGER(GET_SLOT(iteratorState_R, indices_sym)); 
    
    int n = *INTEGER(GET_SLOT(iteratorValues_R, nWithin_sym)) *
            *INTEGER(GET_SLOT(iteratorValues_R, nBetween_sym));
    
    int KPlusOne_Values = LENGTH(indicesValues_R); 
    
    for (int i = 0; i < n; ++i) {
        int i_first_state = indicesState[0] - 1; /* C style */
        int i_last_values = (indicesValues[KPlusOne_Values-1] - 1)*nSeason + offset;
        
        double *this_s = REAL(VECTOR_ELT(s_R, i_first_state));
        
        for (int j = 0; j < nSeason; ++j) {
            
            this_s[j] = values[i_last_values + j - 1]; 
            
       }
        
        advanceA(iteratorValues_R);
        advanceA(iteratorState_R);
    }

}


void transferParamBetas(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    SEXP betas_R = GET_SLOT(model_R, betas_sym);
    /* a vector of logicals */
    int *betaIsPredicteds = INTEGER(GET_SLOT(model_R, betaIsPredicted_sym));
    /* list of Offsets */
    SEXP offsetsBetas_R = GET_SLOT(model_R, offsetsBetas_sym);
    
    int nBeta = LENGTH(betas_R);
    
    for (int i = 0; i < nBeta; ++i) {
        
        if (!betaIsPredicteds[i]) {
            
            double *this_beta = REAL(VECTOR_ELT(betas_R, i));
            
            int *this_offset_beta = INTEGER(VECTOR_ELT(offsetsBetas_R, i));
            int first = this_offset_beta[0] - 1;
            int last = this_offset_beta[1] - 1;
            
            int length_data = last - first + 1;
            
            getOneIterFromFile(this_beta,
                        filename, 
                        first, 
                        length_data, 
                        lengthIter, 
                        iteration);
        }
    }
}

void
transferParamPriorsBetas(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    SEXP betas_R = GET_SLOT(model_R, betas_sym);
    
    SEXP priors_R = GET_SLOT(model_R, priorsBetas_sym);
    
    /* list of OffsetsOrNull */
    SEXP offsetsPriorsBetas_R = GET_SLOT(model_R, offsetsPriorsBetas_sym);
    /* a vector of logicals */
    int *betaIsPredicteds = INTEGER(GET_SLOT(model_R, betaIsPredicted_sym));
    
    int nBeta = LENGTH(betas_R);
    
    int max_length_data = 0;
    int array_length_data[nBeta]; /* array of ints */
    int array_first_offset[nBeta]; /* array of ints */
    
    for (int i = 0; i < nBeta; ++i) 
    { /* preparatory work for transfer priors */    
        SEXP this_offsetPriorsBetas_R = VECTOR_ELT(offsetsPriorsBetas_R, i);
        int isPredicted = betaIsPredicteds[i];
    
        if (isPredicted && !isNull(this_offsetPriorsBetas_R)) {
        
            int *this_offset = INTEGER(this_offsetPriorsBetas_R);
            
            int first = this_offset[0] - 1;
            int last = this_offset[1] - 1;
                        
            int length_data = last - first + 1;
            if (length_data > max_length_data) {
                max_length_data = length_data;
            }
            /* store length for offsetsPriorsBetas */
            array_length_data[i] = length_data;
            /* store first for offsetsPriorsBetas */
            array_first_offset[i] = first;
        }
        else {
            array_length_data[i] = 0;
        }
    }
        
    double *values = (double*)R_alloc(max_length_data, sizeof(double));
    
    for (int i = 0; i < nBeta; ++i) {

        int length_data = array_length_data[i];
        
        if (length_data) {
        
            int first = array_first_offset[i];
                
            getOneIterFromFile(values,
                        filename, 
                        first, 
                        length_data, 
                        lengthIter, 
                        iteration);
            /* function deleted */
            transferParamPrior(VECTOR_ELT(priors_R, i),
                                values, length_data);
        } 
    }
}


void
transferParamSigma(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    double *sigmaPtr = REAL(GET_SLOT(model_R, sigma_sym));

    int *this_offsets = INTEGER(GET_SLOT(model_R, offsetsSigma_sym));
    int first = this_offsets[0] - 1;
    int last = this_offsets[1] - 1;
    
    int length_data = last - first + 1;
        
    getOneIterFromFile(sigmaPtr,
                        filename, 
                        first, 
                        length_data, 
                        lengthIter, 
                        iteration);
}


void
transferParamVarsigma(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    double *varsigmaPtr = REAL(GET_SLOT(model_R, varsigma_sym));

    int *this_offsets = INTEGER(GET_SLOT(model_R, offsetsVarsigma_sym));
    int first = this_offsets[0] - 1;
    int last = this_offsets[1] - 1;
    
    int length_data = last - first + 1;
        
    getOneIterFromFile(varsigmaPtr,
                        filename, 
                        first, 
                        length_data, 
                        lengthIter, 
                        iteration);
}


/* *********************** updating ****************************** */


  
     

/* *********** UPDATING COUNTS ************** */

SEXP 
diffLogLik_R(SEXP yProp_R, SEXP y_R, SEXP indicesY_R, 
                SEXP observation_R, SEXP datasets_R, SEXP transforms_R)
{
    
    int n_element_indices_y = LENGTH(indicesY_R);
    int *indices = INTEGER(indicesY_R);
    
    int *yProp = INTEGER(yProp_R);
    
    return ScalarReal(diffLogLik(yProp, y_R, 
                indices, n_element_indices_y,
                observation_R, datasets_R, transforms_R));
}


/* y and yProp are made of integers */
double 
diffLogLik(int *yProp, SEXP y_R, 
                int *indices, int n_element_indices_y, 
                SEXP observation_R, SEXP datasets_R, SEXP transforms_R)
{
    
    int *y = INTEGER(y_R);
        
    int n_dataset = LENGTH(datasets_R);
    
    int i_element_indices_y = 0;
    
    int ans_infinite = 0;
    double ans = 0.0;
    
    while (!ans_infinite && 
                    ( i_element_indices_y < n_element_indices_y )) { 
        
        int ir_cell_y = indices[i_element_indices_y];
        
        /* only do something if yProp value not same as current value */
        if ( yProp[i_element_indices_y] != y[ir_cell_y-1] ) {
                  
            int i_dataset = 0;
            
            while (i_dataset < n_dataset ) {
                
                SEXP this_transform_R = VECTOR_ELT(transforms_R, i_dataset);
                int ir_cell_dataset = dembase_getIAfter(ir_cell_y, this_transform_R);
                
                #ifdef DEBUGGING
                PrintValue(ScalarInteger(100001));
                PrintValue(ScalarInteger(i_element_indices_y));
                PrintValue(ScalarInteger(ir_cell_y-1));
                PrintValue(ScalarInteger(i_dataset));
                PrintValue(ScalarInteger(ir_cell_dataset));
                #endif    

                if (ir_cell_dataset > 0) {
                    
                    #ifdef DEBUGGING
                    PrintValue(ScalarInteger(100010));
                    PrintValue(ScalarInteger(100011));
                    PrintValue(ScalarInteger(yProp[i_element_indices_y]));
                    PrintValue(ScalarInteger(100012));
                    PrintValue(ScalarInteger(y[ir_cell_y-1]));
                    PrintValue(ScalarInteger(100013));
                    int checkid = R_compute_identical(ScalarReal(yProp[i_element_indices_y]),
                                                    ScalarReal(y[ir_cell_y-1]),0);
                    PrintValue(ScalarInteger(checkid));
                    checkid = (yProp[i_element_indices_y]< y[ir_cell_y-1]);
                    PrintValue(ScalarInteger(checkid));
                    checkid = (yProp[i_element_indices_y]> y[ir_cell_y-1]);
                    PrintValue(ScalarInteger(checkid));
                    
                    #endif
                    
                    SEXP this_dataset_R = VECTOR_ELT(datasets_R, i_dataset);
                    int *this_dataset = INTEGER(this_dataset_R);
                    int i_cell_dataset = ir_cell_dataset - 1; /* C style index */
                    int cellObserved = !( this_dataset[i_cell_dataset] == NA_INTEGER
                            || ISNA(this_dataset[i_cell_dataset]) ); 
                                                /* testing for an R NA seems 
                                                to be very illogical!
                                                What works here is the NA_INTEGER
                                                test but I wanted to add belt & braces*/
                    
                    if (cellObserved) {
                        
                        SEXP this_model_R = VECTOR_ELT(observation_R, i_dataset);
                        
                        SEXP i_contrib_to_cell_R;
                        PROTECT( i_contrib_to_cell_R 
                                    = dembase_getIShared(ir_cell_y, this_transform_R) ); 
                        int n_contrib_to_cell = LENGTH(i_contrib_to_cell_R);
                        int *ir_contrib_to_cell = INTEGER(i_contrib_to_cell_R);

                        int collapsed_y_curr = 0.0;
                        for (int i = 0; i < n_contrib_to_cell; ++i) {
                            collapsed_y_curr += y[ ir_contrib_to_cell[i] - 1 ];
                        }
                        
                        UNPROTECT(1); /* i_contrib_to_cell_R */
                        
                        int diff_prop_curr = yProp[i_element_indices_y]
                                                    - y[ir_cell_y - 1];
                        int collapsed_y_prop 
                                            = collapsed_y_curr + diff_prop_curr;
                        
                        #ifdef DEBUGGING
                        PrintValue(ScalarInteger(100015));
                        PrintValue(ScalarReal(collapsed_y_curr));
                        PrintValue(ScalarReal(diff_prop_curr));
                        PrintValue(ScalarReal(collapsed_y_prop));
                        #endif
                        
                        double log_lik_prop = logLikelihood(this_model_R, 
                                                            collapsed_y_prop,
                                                            this_dataset_R,
                                                            ir_cell_dataset);
                        
                        if ( !R_FINITE(log_lik_prop) ) {
                            
                            ans = R_NegInf;
                            ans_infinite = 1;
                            
                            #ifdef DEBUGGING
                            PrintValue(ScalarInteger(99999999));
                            #endif
                            
                            break;
                        }
                    
                        double log_lik_curr = logLikelihood(this_model_R, 
                                                        collapsed_y_curr,
                                                        this_dataset_R,
                                                        ir_cell_dataset);    

                        ans += ( log_lik_prop - log_lik_curr);
                    
                        #ifdef DEBUGGING
                        PrintValue(ScalarInteger(100020));
                        PrintValue(ScalarReal(log_lik_prop));
                        PrintValue(ScalarReal(log_lik_curr));
                        PrintValue(ScalarReal(ans));
                        #endif
                    }
                }
                
                ++i_dataset;
            } /* end dataset loop */
        } /* end if we need to do dataset loop */
        else {
            #ifdef DEBUGGING
            PrintValue(ScalarReal(yProp[i_element_indices_y]));
            PrintValue(ScalarReal(y[ir_cell_y - 1]));
            PrintValue(ScalarReal(yProp[i_element_indices_y] - y[ir_cell_y -1 ]));
           
            PrintValue(ScalarInteger(100000));
            #endif    
        }
    
        ++i_element_indices_y;
    } /* end i_element_indices_y loop */
    
    #ifdef DEBUGGING
    PrintValue(ScalarInteger(100050));
    PrintValue(ScalarReal(ans));
    #endif
                
    return ans;
}

/* dataset_R integers  for all likelihood functions */

double
logLikelihood_Binomial(SEXP model_R, int count, 
                                SEXP dataset_R, int i)
{
    int *dataset = INTEGER(dataset_R);
    double x = dataset[i-1];
    double *theta = REAL(GET_SLOT(model_R,theta_sym));
    double prob = theta[i-1];
    
    return dbinom(x, count, prob, USE_LOG);
}

double
logLikelihood_Poisson(SEXP model_R, int count, 
                                SEXP dataset_R, int i)
{
    int *dataset = INTEGER(dataset_R);
    double x = dataset[i-1];
    double *theta = REAL(GET_SLOT(model_R,theta_sym));
    double lambda = (theta[i-1])*count;
    
    return dpois(x, lambda, USE_LOG);
}

double
logLikelihood_PoissonBinomialMixture(SEXP model_R, int count, 
                                SEXP dataset_R, int i)
{
    int *dataset = INTEGER(dataset_R);
    double x = dataset[i-1];
    double prob = *REAL(GET_SLOT(model_R,prob_sym));
    
    return dpoibin1(x, count, prob, USE_LOG);
}

int
makeIOther(int i, SEXP transform_R)
{
    SEXP i_shared_R;
    PROTECT( i_shared_R 
                = dembase_getIShared(i, transform_R) ); 
    int n_shared = LENGTH(i_shared_R);
    
    int ans = -1;
    if (n_shared == 1) {
        ans = 0;
    }
    else if (n_shared > 1) {
        int *ir_shared = INTEGER(i_shared_R);
        int ir_self = 1;
        for ( ; ir_self <= n_shared; ++ ir_self) {
            if (ir_shared[ir_self-1] == i) break;
        }
        int whichr_shared = (int)floor(runif(0.0,1.0)*(n_shared-1) + 1);
        if (whichr_shared == n_shared) whichr_shared = n_shared - 1;
        /* max whichr_shared = n_shared - 1 */
        if (whichr_shared >= ir_self) whichr_shared += 1;
        ans = ir_shared[whichr_shared - 1];
    }
    
    UNPROTECT(1); /* i_shared_R */
    return ans;
    
}


/* ************** ESTIMATION ************** */

/* Function 'estimateOneChain' updates 'combined' many times and writes
 * the results out to file.  It returns the  final 'combined' object if
 * every write was successful, and NULL otherwise.
 * When called from within an estimate* function:
 * if (nBurnin > 0)
 *   (i) update nBurnin-1 times (so that burnin consists of nBurnin
 *       versions of 'combined', including initial version)
 *   (ii) for (i = 1,...,nSim/nThin)
 *          update nThin times
 *          record
 * if (nBurnin == 0)
 *   update nThin-1 times (so that first nThin-1 versions, including initial, are not recorded)
 *   record
 *   for (i = 1,...,nSim/nThin-1)
 *     update nThin times
 *     record
 * When called from within continueEstimation:
 * for (i = 1,...,nSim/nThin)
 *   update nThin times
 *   record
 * (Note that whenever continuing is TRUE, nBurnin is 0.) */

#define KEEP_FILE_OPEN
#ifdef KEEP_FILE_OPEN

/* need to guarantee that nThin > 0, nBurnin non-negative */
void 
estimateOneChain(SEXP object_R, SEXP filename_R, 
                SEXP nBurnin_R, SEXP nSim_R, SEXP nThin_R,
                SEXP continuing_R)
{
    const char *filename = CHAR(STRING_ELT(filename_R, 0));
    
    FILE * fp;
    fp = fopen (filename,"wb"); /* overwrite if exists */
    
    if ( NULL != fp ) {
    
        int nBurnin = *INTEGER(nBurnin_R);
        int nSim = *INTEGER(nSim_R);
        int nThin = *INTEGER(nThin_R);
        
        int continuing = *LOGICAL(continuing_R);
        
        /* update nBurnin-1 times */
        if (nBurnin > 1) {
            updateCombined(object_R, nBurnin - 1);
        }
        
        /* n.prod <- nSim %/% nThin */
        
        int n_prod = nSim/nThin; /* integer division */
        
        /* for (i in seq_len(n.prod)) {
            ## when C versions of updateCombined are finished, change to useC = TRUE
            if ((nBurnin == 0L) && (i == 1L) && !continuing)
                combined <- updateCombined(combined, nUpdate = nThin - 1L)
            else
                combined <- updateCombined(combined, nUpdate = nThin) */
        for (int i = 0; i < n_prod; ++i) {
            
            if (!continuing && (nBurnin == 0) && (i == 0)) {
                /* if nThin==1 nUpdate will be 0 and nothing actually happens
                 * ie we go straight on to record the object */
                updateCombined(object_R, nThin-1);
            }
            else { /* nBurnin > 0 or i > 0 or continuing */
                updateCombined(object_R, nThin);
            }
            
            writeValuesToFileBin(fp, object_R);
            if (ferror (fp)) {
                error("unsuccessful write to file %s", filename);
                /* terminates on error
                 * this is where we could choose to do something else
                 * other than write the error */
            }
        }
        
        fclose(fp);
    }
    else {
        error("could not open file %s", filename); /* terminates now */
    }
}


#else // old version

/* need to guarantee that nThin > 0, nBurnin non-negative */
void 
estimateOneChain(SEXP object_R, SEXP filename_R, 
                SEXP nBurnin_R, SEXP nSim_R, SEXP nThin_R,
                SEXP continuing_R)
{
    const char *filename = CHAR(STRING_ELT(filename_R, 0));
    
    if ( makeNewFile(filename) ) {
    
        int nBurnin = *INTEGER(nBurnin_R);
        int nSim = *INTEGER(nSim_R);
        int nThin = *INTEGER(nThin_R);
        
        int continuing = *LOGICAL(continuing_R);
        
        /* update nBurnin-1 times */
        if (nBurnin > 1) {
            updateCombined(object_R, nBurnin - 1);
        }
        
        /* n.prod <- nSim %/% nThin */
        
        int n_prod = nSim/nThin; /* integer division */
        
        /* for (i in seq_len(n.prod)) {
            ## when C versions of updateCombined are finished, change to useC = TRUE
            if ((nBurnin == 0L) && (i == 1L) && !continuing)
                combined <- updateCombined(combined, nUpdate = nThin - 1L)
            else
                combined <- updateCombined(combined, nUpdate = nThin) */
        for (int i = 0; i < n_prod; ++i) {
            
            if (!continuing && (nBurnin == 0) && (i == 0)) {
                /* if nThin==1 nUpdate will be 0 and nothing actually happens
                 * ie we go straight on to record the object */
                updateCombined(object_R, nThin-1);
            }
            else { /* nBurnin > 0 or i > 0 or continuing */
                updateCombined(object_R, nThin);
            }
            
            /* open file for added writing, write, and close */
            int success = addToFile(filename, object_R);
            if (!success) {
                error("unsuccessful write to file %s", filename);
                /* terminates on error
                 * this is where we could choose to do something else
                 * other than write the error */
            }
        }
    }
    else {
        error("could not open file %s", filename); /* terminates now */
    }
}

#endif

/* open file for writing, overwriting if exists, and close.
 * return 1 if file could be successfully opened,
 * else 0 */
//#define THROW_ERROR_ON_OPEN /* for testing not being able to open file */
int
makeNewFile(const char * filename)
{
    int success = 1;
    
    FILE * fp;
    fp = fopen (filename,"wb"); /* overwrite if exists */
    if (fp == NULL) success = 0;
    else fclose (fp);
    
    #ifndef THROW_ERROR_ON_OPEN
    return success; 
    #else
    return 0;
    #endif
}

/* open file for added writing, write, and close.
 * return 1 if file could be successfully open and written to,
 * else 0 */
//#define THROW_ERROR_ON_WRITE /* simulating throw an error on writing */
int
addToFile(const char * filename, SEXP object_R)
{
    int success = 1;
    
    FILE * fp;
    fp = fopen (filename,"ab"); /* add to file if exists */
    if (fp == NULL) success = 0;
    else {
        writeValuesToFileBin(fp, object_R);
        if (ferror (fp)) success = 0;
        fclose (fp);
    }
    #ifndef THROW_ERROR_ON_WRITE
    return success; 
    #else
    return 0;
    #endif
}


/* Based on Brendan's code */

/* more efficient than Brendan's original because it avoids individual
 * writes per value in integer and logical vectors, but it does mean 
 * allocating the extra space temporarily for each temporary 
 * vector of doubles.  But half the time on my tests doing it this way
 * (could use coerseVector instead but we still have to make a copy
 * and on my tests coerceVector actually took marginally longer)  */
void
writeValuesToFileBin(FILE *fp, SEXP object_R)
{
    
    if (IS_S4_OBJECT(object_R)) {
        
        if (R_has_slot(object_R, slotsToExtract_sym)) {
            SEXP slots_to_extract = GET_SLOT(object_R, slotsToExtract_sym);
            
            for (int i=0; i<LENGTH(slots_to_extract); i++) {
                
                const char *sym_name = CHAR(STRING_ELT(slots_to_extract, i));
                writeValuesToFileBin(fp, GET_SLOT(object_R, install(sym_name)));
            }
        }
        else if (R_has_slot(object_R, Data_sym)) {
            writeValuesToFileBin(fp, GET_SLOT(object_R, Data_sym));
        }
        else {
            error("write_values_to_file cannot handle object_R of class \"%s\"",
                /* get the class name */
                CHAR(STRING_ELT(GET_SLOT((object_R), R_ClassSymbol), 0))
                );
        }
    }
    else {
        switch(TYPEOF(object_R)) {
            case REALSXP:
                fwrite(REAL(object_R), sizeof(double), LENGTH(object_R), fp);
                break;
            case INTSXP:
                {
                    int n = LENGTH(object_R);
                    double *tmpArray = (double *)R_alloc(n, sizeof(double));  
                    int *object = INTEGER(object_R);
                    for (int i = 0; i < n; ++i) {
                        tmpArray[i] = (double)object[i];
                    }
                    fwrite(tmpArray, sizeof(double), n, fp);
                    fflush(fp); /* flush buffer - forces output to file */
                    
               }
                break;
                
            case LGLSXP:
                {
                    int n = LENGTH(object_R);
                    double *tmpArray = (double *)R_alloc(n, sizeof(double));  
                    int *object = LOGICAL(object_R);
                    for (int i = 0; i < n; ++i) {
                        tmpArray[i] = (double)object[i];
                        
                    }
                    fwrite(tmpArray, sizeof(double), n, fp);
                    fflush(fp); /* flush buffer - forces output to file */

                }
                break;
            case VECSXP:
                for (int i=0; i<LENGTH(object_R); i++) {
                    writeValuesToFileBin(fp, VECTOR_ELT(object_R, i));
                }
                break;
            default:
            error("write_values_to_file cannot handle object_R type "
                  "'%s' (%d).\n", 
                  type2char(TYPEOF(object_R)), TYPEOF(object_R));
        }
    }
    
    
}

/* note that these getDataFromFile functions are only okay if 
* we guarantee that the data is read using the same machine that wrote it.
* Different architectures arrange bytes in different ways (endian-ness)
* and trying to read with a machine that uses one endian type from
* data written by a machine that uses the other will be a Bad Idea. */ 



/* Assume all arguments have been checked
* 'filename' is a string;
* 'first', 'last', and 'lengthIter' are all positive integer scalars,
*  such that 'first' <= 'last' <= 'lengthIter';
* 'iterations' is vector of integers of length >= 1, in order, with no duplicates
-*/

/* version that does n_iter separate reads from file and
* does not have to allocate the extra space for the buffer*/

#if(0)
SEXP getDataFromFile_R(SEXP filename_R, 
                       SEXP first_R, SEXP last_R, 
                       SEXP lengthIter_R, SEXP iterations_R)
{
    /* strings are character vectors, in this case just one element */
    const char *filename = CHAR(STRING_ELT(filename_R,0)); 

    int first = *(INTEGER(first_R))- 1;
    int last = *(INTEGER(last_R)) - 1;
    int lengthIter = *(INTEGER(lengthIter_R));
    int n_iter = LENGTH(iterations_R);
    int *iterations = INTEGER(iterations_R);

    int length_data = last - first + 1;

    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, n_iter*length_data));
    double *ans = REAL(ans_R);

    getDataFromFile(ans,
                    filename, 
                    first, 
                    length_data, 
                    lengthIter, 
                    n_iter,
                    iterations);

    UNPROTECT(1); /* ans_R */

    return ans_R;
}

/* ans must have space for n_iter*length_data elements */
void getDataFromFile(double *ans,
                    const char *filename, 
                    int first, 
                    int length_data, 
                    int lengthIter, 
                    int n_iter,
                    int *iterations)
{
    #ifdef DEBUGFILEREAD
        PrintValue(mkString("entering getDataFromFile"));
        PrintValue(mkString("first"));
        PrintValue(ScalarInteger(first));
        PrintValue(mkString("length_data"));
        PrintValue(ScalarInteger(length_data));
        PrintValue(mkString("lengthIter"));
        PrintValue(ScalarInteger(lengthIter));
        PrintValue(mkString("n_iter"));
        PrintValue(ScalarInteger(n_iter));
    #endif

    FILE * fp = fopen(filename, "rb"); /* binary mode */
    if (NULL == fp) {
        error("could not open file %s", filename); /* terminates now */
    }

    int length_gap = lengthIter - length_data;
    
    int sizeResultsBytes = 0;
    
    /* read the size of the results file - presumably in bytes?? */
    size_t nRead1 = fread(&sizeResultsBytes, sizeof(int), 1, fp);
    
    if (nRead1 < 1) {
        error("could not successfully read file %s", filename); 
    }
    
    /* skip sizeResultsBytes bytes from current pos*/
    fseek (fp , sizeResultsBytes , SEEK_CUR );

    int n_skip = iterations[0] - 1; /* iterations to skip */

    /* skip n_skip iterations of length lengthIter, and first values */
    long skipBytes = (n_skip * lengthIter + first ) * sizeof(double);

    /* position this far into file, in bytes, from current position */
    fseek (fp , skipBytes , SEEK_CUR );

    size_t nRead = fread(ans, sizeof(double), length_data, fp);

    #ifdef DEBUGFILEREAD
        PrintValue(mkString("length_gap"));
        PrintValue(ScalarInteger(length_gap));
        PrintValue(mkString("n_skip"));
        PrintValue(ScalarInteger(n_skip));
        PrintValue(mkString("skipBytes"));
        PrintValue(ScalarInteger(skipBytes));
        PrintValue(mkString("nRead"));
        PrintValue(ScalarInteger(nRead));
        PrintValue(mkString("ans"));
        printDblArray(ans, length_data);
    #endif

    if (nRead != length_data) {
        error("could not successfully read file %s", filename); 
    }

    /* read rest of data */
    double *ansPtr = ans;   
    for (int i = 1; i < n_iter; ++i) {
        
        /* move to end of last block in ans */
        ansPtr += length_data;
        
        /* calculations for start of next block of data */
        n_skip = iterations[i] - iterations[i-1] - 1;

        skipBytes = (n_skip * lengthIter + length_gap) * sizeof(double);
        fseek (fp , skipBytes , SEEK_CUR );

        nRead = fread(ansPtr, sizeof(double), length_data, fp);

        if (nRead != length_data) {
            error("could not successfully read file %s", filename); 
        }    
    }

    fclose(fp); /* close the file */

}
#endif

/* alternative version that does 1 read from file and
* has to allocate the extra space for  a buffer to then
* extract the desired data from.
* Cheaper on io time to read the whole lot in 
* and sort it out from there 
* but at the expense of allocation of the buffer
*/

#if(1)
SEXP getDataFromFile_R(SEXP filename_R, 
                        SEXP first_R, SEXP last_R, 
                        SEXP lengthIter_R, SEXP iterations_R)
{

    /* strings are character vectors, in this case just one element */
    const char *filename = CHAR(STRING_ELT(filename_R,0)); 

    FILE * fp = fopen(filename, "rb"); /* binary mode */

    if (NULL == fp) {
        error("could not open file %s", filename); /* terminates now */
    }

    int first = *(INTEGER(first_R));
    int last = *(INTEGER(last_R));
    int lengthIter = *(INTEGER(lengthIter_R));
    int n_iter = LENGTH(iterations_R);
    int *iterations = INTEGER(iterations_R);

    int length_data = last - first + 1;
    int length_gap = lengthIter - length_data;

    /* find size of file in total */
    fseek(fp, 0, SEEK_END);

    long size = ftell(fp); /* Bytes in file*/
    
    rewind (fp); /* return to start of file */
    
    int sizeResultsBytes = 0;
    
    /* read the size of the results file - presumably in bytes?? */
    size_t nRead1 = fread(&sizeResultsBytes, sizeof(int), 1, fp);
    
    if (nRead1 < 1) {
        error("could not successfully read file %s", filename); 
    }
    
    /* skip sizeResultsBytes bytes from current pos*/
    fseek (fp , sizeResultsBytes , SEEK_CUR );
    
    int n_skip = iterations[0] - 1; /* iterations to skip */

    /* skip n_skip iterations of length lengthIter, and first -1 values */
    long skipBytes = (n_skip * lengthIter + first - 1) * sizeof(double);

    /* position this far into file, in bytes, from current pos */
    fseek (fp , skipBytes , SEEK_CUR );

    /* work out how many doubles to read and allocate a buffer */
    long nDoubleRead = (size - sizeof(int) - sizeResultsBytes - skipBytes)
                        /sizeof(double);

    double *buffer = (double*)R_alloc(nDoubleRead, sizeof(double));
    size_t nRead2 = fread(buffer, sizeof(double), nDoubleRead, fp);

    if (nRead2 != nDoubleRead) {
        error("could not successfully read file %s", filename); 
    }

    fclose(fp); /* close the file */

    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, n_iter*length_data));
    double *ans = REAL(ans_R);

    /* transfer length_data values to ans */    
    memcpy(ans, buffer, length_data*sizeof(double));

    double *bufferPtr = buffer;   
    double *ansPtr = ans;   

    for (int i = 1; i < n_iter; ++i) {
        /* move to end of last block in ans */
        ansPtr += length_data;
        /* calculations for start of next block of data */
        n_skip = iterations[i] - iterations[i-1] - 1;

        /* position of start of next block in buffer*/
        bufferPtr += length_data + n_skip * lengthIter + length_gap;

        /* transfer length_data values to ans */    
        memcpy(ansPtr, bufferPtr, length_data*sizeof(double));
    }

    //free(buffer);
    UNPROTECT(1); /* ans_R */

    return ans_R;
}
#endif




SEXP getOneIterFromFile_R(SEXP filename_R, 
                        SEXP first_R, SEXP last_R, 
                        SEXP lengthIter_R, SEXP iteration_R)
{
    /* strings are character vectors, in this case just one element */
    const char *filename = CHAR(STRING_ELT(filename_R,0)); 
    
    int first = *(INTEGER(first_R)) - 1;
    int last = *(INTEGER(last_R)) - 1;
    int lengthIter = *(INTEGER(lengthIter_R));
    int iteration = *INTEGER(iteration_R);
    
    int length_data = last - first + 1;
    
    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, length_data));
    double *ans = REAL(ans_R);

    getOneIterFromFile(ans,
                    filename, 
                    first, 
                    length_data, 
                    lengthIter, 
                    iteration);

    UNPROTECT(1); /* ans_R */
    return ans_R;
    
}

/* ans must have space for length_data elements */
void getOneIterFromFile(double *ans,
                        const char *filename, 
                        int first, 
                        int length_data, 
                        int lengthIter, 
                        int iteration)
{
    #ifdef DEBUGFILEREAD
        PrintValue(mkString("getOneIterFromFile"));
        PrintValue(mkString("first"));
        PrintValue(ScalarInteger(first));
        PrintValue(mkString("length_data"));
        PrintValue(ScalarInteger(length_data));
        PrintValue(mkString("lengthIter"));
        PrintValue(ScalarInteger(lengthIter));
    #endif
    
    FILE * fp = fopen(filename, "rb"); /* binary mode */
    if (NULL == fp) {
        error("could not open file %s", filename); /* terminates now */
    }

    int n_skip = iteration - 1; /* iterations to skip */
    
    /* skip n_skip iterations of length lengthIter, and first values */
    long skipBytes = (n_skip * lengthIter + first ) * sizeof(double);
    
    /* position this far into file, in bytes, from current pos */
    fseek (fp , skipBytes , SEEK_CUR );
    
    size_t nRead = fread(ans, sizeof(double), length_data, fp);

    #ifdef DEBUGFILEREAD
        PrintValue(mkString("n_skip"));
        PrintValue(ScalarInteger(n_skip));
        PrintValue(mkString("skipBytes"));
        PrintValue(ScalarInteger(skipBytes));
        PrintValue(mkString("nRead"));
        PrintValue(ScalarInteger(nRead2));
        PrintValue(mkString("ans"));
        printDblArray(ans, length_data);
    #endif
    
    
    if (nRead != length_data) {
        error("could not successfully read file %s", filename); 
    }
    
    fclose(fp); /* close the file */
}


/* **** DEMOGRAPHIC ACCOUNTS ******************** */

/* random integer in interval [0, lessThan-1] */
int
runifInt(int lessThan)
{
    int i = (int)(runif(0, 1) * lessThan); /* C-style index */
    if (i == lessThan) {
        i = lessThan - 1;
    }
    return i;
}


int
chooseICellComp(SEXP description_R)
{
    int length = *INTEGER(GET_SLOT(description_R, length_sym));
    
    int i = runifInt(length); /* C-style index */
    
    int i_R = i + 1;
    return i_R;
}


SEXP
chooseICellOutInPool(SEXP description_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, 2));
    int *ans = INTEGER(ans_R);
    
    chooseICellOutInPoolInternal(ans, description_R);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}

/* ans must have 2 elements */
void
chooseICellOutInPoolInternal(int *ans, SEXP description_R)
{       
    int stepDirection = *INTEGER(GET_SLOT(description_R, stepDirection_sym));
    SEXP nBetweenVec_R = GET_SLOT(description_R, nBetweenVec_sym);
    int *nBetweenVec = INTEGER(nBetweenVec_R);
    int *stepBetweenVec = INTEGER(GET_SLOT(description_R, stepBetweenVec_sym));
    SEXP nWithinVec_R = GET_SLOT(description_R, nWithinVec_sym);
    int *nWithinVec = INTEGER(nWithinVec_R);
    int *stepWithinVec = INTEGER(GET_SLOT(description_R, stepWithinVec_sym));
    
    int nDimBetween = LENGTH(nBetweenVec_R);
    int nDimWithin = LENGTH(nWithinVec_R);
    
    int iOut = 1;
    int iIn = 1 + stepDirection;
    
    for (int d = 0; d < nDimBetween; ++d) {
        
        int nBetween = nBetweenVec[d];
        int stepBetween = stepBetweenVec[d];
        
        int iBetweenOut = runifInt(nBetween); /* C-style index */
                
        /* pick iBetweenOut from integers 
         * in {0,..iBetweenIn-1} U {iBetweenIn + 1, nBetween-1} */
        int iBetweenIn = runifInt(nBetween - 1); /* C-style index */
        if (iBetweenIn >= iBetweenOut) {
            
            ++iBetweenIn; 
        }
 
        iOut +=  iBetweenOut * stepBetween;
        iIn += iBetweenIn * stepBetween;
    }

    for (int d = 0; d < nDimWithin; ++d) {
        int nWithin = nWithinVec[d];
        int stepWithin = stepWithinVec[d];
        
        int iWithin = runifInt(nWithin); /* C-style index */
        
        iOut += iWithin * stepWithin;
        iIn +=  iWithin * stepWithin;
    }
    ans[0] = iOut;
    ans[1] = iIn;
}

int
chooseICellPopn(SEXP description_R)
{
    int length = *INTEGER(GET_SLOT(description_R, length_sym));
    int nTime = *INTEGER(GET_SLOT(description_R, nTime_sym));
    int stepTime = *INTEGER(GET_SLOT(description_R, stepTime_sym));
    
    int nInitial = length/nTime; /* integer division */
    
    int i = runifInt(nInitial); /* C-style index */
    int iOverStepTime = i/stepTime; /* integer division */
    i = i % stepTime + iOverStepTime * nTime*stepTime;
    
    int i_R = i + 1;
    return i_R;
}

int
getIAccNextFromPopn(int i, SEXP description_R)
{
    int stepTime = *INTEGER(GET_SLOT(description_R, stepTime_sym));
    int nTimePopn = *INTEGER(GET_SLOT(description_R, nTime_sym));
    int stepAgePopn = *INTEGER(GET_SLOT(description_R, stepAge_sym));
    int nAge = *INTEGER(GET_SLOT(description_R, nAge_sym));
    
    int nTimeAcc = nTimePopn - 1;
    int iTime_r = (((i - 1) / stepTime) % nTimePopn) + 1; /* R-style */
    
    int iAccNext = 0;
    
    if (iTime_r < nTimePopn) {
        int tTimePopn = stepTime * nTimePopn;
        int diff = i - iTime_r; 
        int tmp = diff / tTimePopn; /* integer div */
        int iAcc = tmp * (stepTime * nTimeAcc) 
            + (diff % tTimePopn)
            + iTime_r;
        
        int iAge_r = (((i - 1) / stepAgePopn) % nAge) + 1; /* R-style */
        
        if (iAge_r < nAge) {
            
            int stepAgeAcc = stepAgePopn;
            
            if (stepAgePopn >= stepTime) {
                stepAgeAcc = (stepAgeAcc * nTimeAcc) / nTimePopn;
            }
            
            iAccNext = iAcc + stepAgeAcc;
            
        }
        else {
            iAccNext = iAcc;
        }
    }
    
    return iAccNext;
}
    
int
getIPopnNextFromPopn(int i, SEXP description_R)
{
    int stepTime = *INTEGER(GET_SLOT(description_R, stepTime_sym));
    int nTime = *INTEGER(GET_SLOT(description_R, nTime_sym));
    
    int iTime_r = (((i - 1) / stepTime) % nTime) + 1; /* R-style */
    
    int iPopnNext = 0;
    
    if (iTime_r < nTime) {
        iPopnNext = i + stepTime;
        
        int hasAge = *INTEGER(GET_SLOT(description_R, hasAge_sym));
        
        if (hasAge) {
            int stepAge = *INTEGER(GET_SLOT(description_R, stepAge_sym));
            int nAge = *INTEGER(GET_SLOT(description_R, nAge_sym));
            int iAge_r = (((i - 1) / stepAge) % nAge) + 1; /* R-style */
            if (iAge_r < nAge) {
                iPopnNext += stepAge;
            }
        }
    }
    
    return iPopnNext;
}

int
getMinValCohort(int i, SEXP series_R, SEXP iterator_R)
{
    int *series = INTEGER(series_R);
    
    int ans = series[i-1];
    
    resetCAP(iterator_R, i);
    
    int *finished = INTEGER(GET_SLOT(iterator_R, finished_sym));
    int *iPtr = INTEGER(GET_SLOT(iterator_R, i_sym));
    
    while(!(*finished)) {
        
        advanceCAP(iterator_R);
        int check = series[*iPtr - 1];
        
        if (check < ans) {
            ans = check;
        }
    }
    
    return ans;
}
