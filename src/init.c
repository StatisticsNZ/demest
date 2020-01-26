
#include "demest.h"
#include <R_ext/Rdynload.h>


/* File "init.c" consists of routines that are run when package
   "demest" is loaded in R */


/* ******************************************************************************* */
/* Create R-visible versions of C functions ************************************** */
/* ******************************************************************************* */

/* Wrapper macro to use for the functions that return predicted Betas.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for prior update functions that
 * use prngs),
 * gets the beta as a vector of doubles,
 * and ensures that the R version returns the beta as an SEXP */
#define PREDICTBETA_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R, SEXP J_R) {    \
    int J = *INTEGER(J_R);               \
    SEXP ans_R;               \
    PROTECT(ans_R = allocVector(REALSXP, J));               \
    double *ans = REAL(ans_R);               \
    GetRNGstate();              \
    name(ans, prior_R, J);          \
    PutRNGstate();              \
    UNPROTECT(1);               \
    return ans_R;             \
    }

/* Wrapper macro to use for the functions that return betaHats.
 * The wrapper puts a _R suffix on end of function name,
 * gets the beta as a vector of doubles,
 * and ensures that the R version returns the beta as an SEXP */
/* Wrapper macro to use for the functions that return betaHats.
 * The wrapper puts a _R suffix on end of function name,
 * gets the beta as a vector of doubles,
 * and ensures that the R version returns the beta as an SEXP */
#define BETAHAT_NOPRNG_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R) {         \
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));         \
    SEXP ans_R;         \
    PROTECT(ans_R = allocVector(REALSXP, J));         \
    double *ans = REAL(ans_R);         \
    name(ans, prior_R, J);         \
    UNPROTECT(1);         \
    return ans_R;         \
    }

/* Wrapper macro to use for the functions that return updated prior.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for prior update functions that
 * use prngs) */
#define UPDATE_PRIORBETA_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R, SEXP beta_R, SEXP thetaTransformed_R, SEXP sigma_R) { \
    double *beta = REAL(beta_R);    \
    double *thetaTransformed = REAL(thetaTransformed_R);    \
    double sigma = *REAL(sigma_R);    \
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));    \
    SEXP ans_R;    \
    PROTECT(ans_R = duplicate(prior_R));    \
    GetRNGstate();    \
    name(beta, J, ans_R, thetaTransformed, sigma);    \
    PutRNGstate();    \
    UNPROTECT(1);    \
    return ans_R;             \
    }

/* Wrapper macro to use for the functions that return predicted priors
 * or betas or models or thetas by updating in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for prior update functions that
 * use prngs),
 * and ensures that the R version returns the updated prior as the SEXP */
#define PREDICTOBJECT_WRAPPER_R(name)         \
    SEXP name##_R(SEXP object_R) {    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(object_R));    \
    GetRNGstate();              \
    name(ans_R);          \
    PutRNGstate();              \
    UNPROTECT(1);               \
    return ans_R;             \
    }


/* Wrapper macro to use for the functions that return predicted object
 * using zeta argument by updating in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for prior update functions that
 * use prngs),
 * and ensures that the R version returns the predicted var as an SEXP */
#define PREDICTOBJECTWITHZETA_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R, SEXP zeta_R) {    \
    double zeta = *REAL(zeta_R);              \
    GetRNGstate();              \
    double ans = name(prior_R, zeta);    \
    PutRNGstate();              \
    return ScalarReal(ans);    \
    }

/* Wrapper macro to use for the functions that update priors in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for prior update functions that
 * use prngs),
 * and ensures that the R version returns the updated prior as the SEXP */
#define UPDATEPRIORWITHBETA_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R, SEXP beta_R) {    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(prior_R));    \
    double *beta = REAL(beta_R);    \
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));   \
    GetRNGstate();              \
    name(ans_R, beta, J);          \
    PutRNGstate();              \
    UNPROTECT(1);               \
    return ans_R;             \
    }

/* Wrapper macro to use for the functions that update priors in place.
 * The wrapper puts a _R suffix on end of function name,
 * and ensures that the R version returns the updated prior as the SEXP */
#define UPDATEPRIOR_NOPRNG_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R, SEXP object_R) {    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(prior_R));    \
    name(ans_R, object_R);          \
    UNPROTECT(1);               \
    return ans_R;             \
    }


/* Wrapper macro to use for the functions that update priors in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for prior update functions that
 * use prngs),
 * and ensures that the R version returns the updated prior as the SEXP */
#define UPDATEPRIORWITHZETA_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R, SEXP beta_R, SEXP zeta_R) {    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(prior_R));    \
    GetRNGstate();              \
    double zeta = *REAL(zeta_R);              \
    name(ans_R, beta_R, zeta);          \
    PutRNGstate();              \
    UNPROTECT(1);               \
    return ans_R;             \
    }

/* Wrapper macro to use for the functions that update object using zeta.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for update functions that use prngs),
 * and ensures that the R version returns the updated prior as the SEXP */
#define UPDATEWITHZETA_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R, SEXP zeta_R) {    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(prior_R));    \
    GetRNGstate();              \
    double zeta = *REAL(zeta_R);              \
    name(ans_R, zeta);          \
    PutRNGstate();              \
    UNPROTECT(1);               \
    return ans_R;             \
    }


/* Wrapper macro to use for the transferParamPrior
 * and transferParamPriorVarDLM functions.
 * The wrapper puts a _R suffix on end of function name,
 * and ensures that the R version returns the updated prior as the SEXP
 * deals with prng state */
#define TRANSFERPARAMPRIOR_WRAPPER_R(name)         \
    SEXP name##_R(SEXP prior_R, SEXP values_R) {    \
    double *values = REAL(values_R);    \
    int nValues = LENGTH(values_R);    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(prior_R));    \
    GetRNGstate();              \
    name(ans_R, values, nValues);          \
    PutRNGstate();              \
    UNPROTECT(1);               \
    return ans_R;             \
    }

/* Wrapper macro to use for the transferParam functions
 * The wrapper puts a _R suffix on end of function name,
 * and ensures that the R version returns the updated prior as the SEXP */
#define TRANSFERPARAM_WRAPPER_R(name)         \
    SEXP name##_R(SEXP model_R, SEXP filename_R, SEXP lengthIter_R, SEXP iteration_R) {    \
    const char *filename = CHAR(STRING_ELT(filename_R,0));    \
    int lengthIter = *(INTEGER(lengthIter_R));    \
    int iteration = *(INTEGER(iteration_R));    \
    SEXP ans_R;    \
    PROTECT(ans_R = duplicate(model_R));    \
    name(ans_R, filename, lengthIter, iteration);    \
    UNPROTECT(1);    \
    return ans_R;             \
    }



/* Wrapper macro to use for the functions that update an R object in place.
 * The wrapper puts a _R suffix on end of function name,
 * and ensures that the R version returns the updated object as the SEXP.
 * Note: this wrapper does not deal with prng state */
#define UPDATEOBJECT_NOPRNG_WRAPPER_R(name)         \
    SEXP name##_R(SEXP object) {    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(object));   \
    name(ans_R);          \
    UNPROTECT(1);               \
    return ans_R;             \
    }


/* Wrapper macro to use for the functions that update an R object in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate,
 * and ensures that the R version returns the updated object as the SEXP. */
#define UPDATEOBJECT_WRAPPER_R(name)         \
    SEXP name##_R(SEXP object) {    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(object));   \
    GetRNGstate();      \
    name(ans_R);          \
    PutRNGstate();          \
    UNPROTECT(1);               \
    return ans_R;             \
    }

/* Wrapper macro to use for the functions that returns an
 * updated object when using exposure.
 * No arguments to functions should be modified,
 * other than the duplicate of object, which is changed in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for update functions that
 * use prngs),
 * and ensures that the R version returns the updated object */
#define UPDATEOBJECT_WITHEXP_WRAPPER_R(name)      \
    SEXP name##_R(SEXP object, SEXP unchanged_R, SEXP exposure_R) {       \
    SEXP ans_R;         \
    PROTECT(ans_R = duplicate(object));         \
    GetRNGstate();      \
    name(ans_R, unchanged_R, exposure_R);       \
    PutRNGstate();      \
    UNPROTECT(1);       \
    return ans_R;       \
    }

/* Wrapper macro to use for the functions that returns an
 * updated object when not using exposure.
 * No arguments to functions should be modified,
 * other than the duplicate of object, which is changed in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for update functions that
 * use prngs),
 * and ensures that the R version returns the updated object */
#define UPDATEOBJECT_NOEXP_WRAPPER_R(name)      \
    SEXP name##_R(SEXP object, SEXP unchanged_R) {       \
    SEXP ans_R;         \
    PROTECT(ans_R = duplicate(object));         \
    GetRNGstate();      \
    name(ans_R, unchanged_R);       \
    PutRNGstate();      \
    UNPROTECT(1);       \
    return ans_R;       \
    }


/* Wrapper macro to use for the functions that returns an
 * updated Counts object when not using exposure.
 * No arguments to functions should be modified,
 * other than the duplicate of the Counts object y,
 * which is changed in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for update functions that
 * use prngs),
 * and ensures that the R version returns the updated object */
#define UPDATECOUNTS_NOEXP_WRAPPER_R(name)      \
    SEXP name##_R(SEXP y_R, SEXP model_R, SEXP dataModels_R,       \
                            SEXP datasets_R, SEXP transforms_R) {       \
    SEXP ans_R;         \
    PROTECT(ans_R = duplicate(y_R));         \
    GetRNGstate();      \
    name(ans_R, model_R, dataModels_R, datasets_R, transforms_R);       \
    PutRNGstate();      \
    UNPROTECT(1);       \
    return ans_R;       \
    }

/* Wrapper macro to use for the functions that returns an
 * updated Counts object when using exposure.
 * No arguments to functions should be modified,
 * other than the duplicate of the Counts object y,
 * which is changed in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for update functions that
 * use prngs),
 * and ensures that the R version returns the updated object */
#define UPDATECOUNTS_WITHEXP_WRAPPER_R(name)      \
    SEXP name##_R(SEXP y_R, SEXP model_R,       \
                            SEXP exposure_R, SEXP dataModels_R,       \
                            SEXP datasets_R, SEXP transforms_R) {       \
    SEXP ans_R;         \
    PROTECT(ans_R = duplicate(y_R));         \
    GetRNGstate();      \
    name(ans_R, model_R, exposure_R, dataModels_R, datasets_R, transforms_R);       \
    PutRNGstate();      \
    UNPROTECT(1);       \
    return ans_R;       \
    }

/* Wrapper macro to use for the functions that return combined object
 * updated for predictions.
 * No arguments to functions should be modified,
 * other than the duplicate of object, which is changed in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for update functions that
 * use prngs),
 * and ensures that the R version returns the updated object */
#define PREDICTCOMBINEDOBJECT_WRAPPER_R(name)      \
    SEXP name##_R(SEXP object_R, SEXP filename_R, SEXP lengthIter_R, SEXP iteration_R) {    \
    const char *filename = CHAR(STRING_ELT(filename_R,0));    \
    int lengthIter = *(INTEGER(lengthIter_R));    \
    int iteration = *(INTEGER(iteration_R));    \
    SEXP ans_R;         \
    PROTECT(ans_R = duplicate(object_R));         \
    GetRNGstate();      \
    name(ans_R, filename, lengthIter, iteration);       \
    PutRNGstate();      \
    UNPROTECT(1);       \
    return ans_R;       \
    }


/* Wrapper macro to use for the functions that return an
 * updated combined counts object (using exposure or not using exposure).
 * No arguments to functions should be modified,
 * other than the duplicate of object, which is changed in place.
 * The wrapper puts a _R suffix on end of function name,
 * and deals with RNGstate (relevant for update functions that
 * use prngs),
 * and ensures that the R version returns the updated object */
#define UPDATECOMBINEDOBJECT_WRAPPER_R(name)      \
    SEXP name##_R(SEXP object_R, SEXP nUpdate_R) {       \
    int nUpdate = *INTEGER(nUpdate_R);         \
    SEXP ans_R;         \
    PROTECT(ans_R = duplicate(object_R));         \
    GetRNGstate();      \
    name(ans_R, nUpdate);       \
    PutRNGstate();      \
    UNPROTECT(1);       \
    return ans_R;       \
    }


/* Wrapper macro to use for the log likelihood functions
 * No arguments to functions should be modified.
 * The wrapper puts a _R suffix on end of function name
 * and ensures that the R version returns the SEXP'd value */
#define LOGLIKELIHOOD_WRAPPER_R(name)      \
    SEXP name##_R(SEXP model_R, SEXP count_R, SEXP dataset_R, SEXP i_R) {       \
    int count = *INTEGER(count_R);         \
    int i = *INTEGER(i_R);         \
    double ans = name(model_R, count, dataset_R, i);       \
    return ScalarReal(ans);       \
    }

/* Wrapper macro to use for the filter functions that returns an
 * updated object.
 * No arguments to functions should be modified,
 * other than the duplicate of object, which is changed in place.
 * The wrapper puts a _R suffix on end of function name,
 * and ensures that the R version returns the updated object.
 * No prng state dealt with*/
#define FILTER_NOPRNG_WRAPPER_R(name)      \
    SEXP name##_R(SEXP object_R, SEXP y_R, SEXP v_R, SEXP forward_R) {       \
    int forward = *INTEGER(forward_R);         \
    SEXP ans_R;         \
    PROTECT(ans_R = duplicate(object_R));         \
    name(ans_R, y_R, v_R, forward);       \
    UNPROTECT(1);       \
    return ans_R;       \
    }



/* Wrapper macro to use for the rmv norm functions that return an SEXP*/
#define RMV_WRAPPER_R(name)      \
    SEXP name##_R(SEXP mean_R, SEXP var_R) {       \
    SEXP ans_R;         \
    GetRNGstate();         \
    PROTECT(ans_R = name(mean_R, var_R));         \
    PutRNGstate();         \
    UNPROTECT(1);       \
    return ans_R;       \
    }

/* Wrapper for the Mapping Get I functions (no manipulatoin of PRNG state) */
#define MAPPING_GET_I_WRAPPER(name)      \
    SEXP name##_R(SEXP i_R, SEXP mapping_R)  {       \
    int i = *INTEGER(i_R);         \
    int ans = name(i, mapping_R);         \
    return ScalarInteger(ans);         \
    }

/* Wrapper for the Mapping Get I functions that return an SEXP vec
 * (no manipulatoin of PRNG state) */
#define MAPPING_GET_IVEC_WRAPPER(name)      \
    SEXP name##_R(SEXP i_R, SEXP mapping_R)  {       \
    int i = *INTEGER(i_R);         \
    SEXP ans_R;         \
    PROTECT(ans_R = name(i, mapping_R));         \
    UNPROTECT(1);         \
    return ans_R;         \
    }

/* Wrapper for logPostPhiMix functions*/
#define LOGPOSTPHI_WRAPPER(name)      \
    SEXP name##_R(SEXP phi_R, SEXP level_R, SEXP meanLevel_R,      \
    SEXP nAlong_R, SEXP indexClassMax_R, SEXP omega_R) {      \
    double phi = *REAL(phi_R);      \
    double *level = REAL(level_R);      \
    double meanLevel = *REAL(meanLevel_R);      \
    int nAlong = *INTEGER(nAlong_R);      \
    int indexClassMax_r = *INTEGER(indexClassMax_R);      \
    double omega = *REAL(omega_R);      \
    double ans = name(phi, level, meanLevel, nAlong, indexClassMax_r, omega);      \
    return ScalarReal(ans);      \
}

/* wrapper for rcmp functions with parameters mu, nu, maxAttempts */
#define RCMP_WRAPPER_R(name)         \
    SEXP name##_R(SEXP mu_R, SEXP nu_R, SEXP maxAttempts_R) {    \
    double mu = *REAL(mu_R);         \
    double nu = *REAL(nu_R);         \
    int maxAttempts = *INTEGER(maxAttempts_R);         \
    GetRNGstate();         \
    double ans = name(mu, nu, maxAttempts);         \
    PutRNGstate();         \
    return ScalarReal(ans);         \
    }

/* wrapper for update-accounts update proposal functions that
 * need to have the Popn and Acc iterators duplicated and originals replaced */
#define UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(name)         \
    SEXP name##_R(SEXP combined_R) {    \
    SEXP ans_R;               \
    PROTECT(ans_R = duplicate(combined_R));   \
    SEXP iteratorPopn_R = GET_SLOT(combined_R, iteratorPopn_sym);    \
    SEXP iteratorPopnDup_R = NULL;    \
    PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));    \
    int nProtected = 2;    \
    int hasAge = *LOGICAL(GET_SLOT(combined_R, hasAge_sym));    \
    SEXP iteratorAccDup_R = NULL;    \
    if(hasAge) {    \
        SEXP iteratorAcc_R = GET_SLOT(combined_R, iteratorAcc_sym);    \
        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));    \
        ++nProtected;    \
    }    \
    GetRNGstate();      \
    name(ans_R);    \
    PutRNGstate();          \
    SET_SLOT(ans_R, iteratorPopn_sym, iteratorPopnDup_R);    \
    if(hasAge) {    \
        SET_SLOT(ans_R, iteratorAcc_sym, iteratorAccDup_R);    \
    }    \
    UNPROTECT(nProtected);    \
    return ans_R;             \
    }

/* wrapper for diffLogLik functions with parameter combined */
#define DIFFLOGLIKCOMBINED_WRAPPER_R(name)         \
    SEXP name##_R(SEXP combined_R) {    \
    SEXP combinednew_R;    \
    PROTECT(combinednew_R = duplicate(combined_R));    \
    double ans = name(combinednew_R);         \
    UNPROTECT(1);         \
    return ScalarReal(ans);         \
    }



/* one-off wrapper for makeMu */
SEXP makeMu_R(SEXP n_R, SEXP betas_R, SEXP iterator_R)
{
    int n = *INTEGER(n_R);
    SEXP ans_R;
    PROTECT(ans_R = makeMu(n, betas_R, iterator_R));
    UNPROTECT(1);
    return ans_R;
}

/* one-off wrapper for updateTauRobust */
SEXP updateTauRobust_R(SEXP prior_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(prior_R));
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));
    GetRNGstate();
    updateTauRobust(ans_R, J);
    PutRNGstate();
    UNPROTECT(1);
    return ans_R;
}


/* one off wrapper for makeLifeExpBirth */
SEXP makeLifeExpBirth_R(SEXP mx_R, SEXP nx_R, SEXP ax_R,
                        SEXP iAge0_R, SEXP nAge_R)

{
    double *mx = REAL(mx_R);
    double *nx = REAL(nx_R);
    double *ax = REAL(ax_R);
    int iAge0_r = *INTEGER(iAge0_R);
    int nAge = *INTEGER(nAge_R);

    double ans = makeLifeExpBirth(mx, nx, ax, iAge0_r, nAge);

    return ScalarReal(ans);
}


/* one-off wrapper for rnormTruncated */
SEXP rnormTruncated_R(SEXP n_R, SEXP mean_R, SEXP sd_R,
                SEXP lower_R, SEXP upper_R, SEXP tolerance_R,
                SEXP maxAttempt_R,
                SEXP uniform_R)
{
    SEXP ans_R;
    GetRNGstate();
    PROTECT(ans_R = rnormTruncated(*INTEGER(n_R),
                        mean_R, sd_R,
                *REAL(lower_R), *REAL(upper_R), *REAL(tolerance_R),
                *INTEGER(maxAttempt_R),
                *LOGICAL(uniform_R) ) );
    PutRNGstate();
    UNPROTECT(1);
    return ans_R;

}

/* one-off wrapper for rnormIntTrunc1 */
SEXP rnormIntTrunc1_R(SEXP mean_R, SEXP sd_R, SEXP lower_R, SEXP upper_R)
{
    double mean = *REAL(mean_R);
    double sd = *REAL(sd_R);
    int lower = *INTEGER(lower_R);
    int upper = *INTEGER(upper_R);

    GetRNGstate();
    int ans = rnormIntTrunc1(mean, sd, lower, upper);
    PutRNGstate();
    return ScalarInteger(ans);

}


/* one-off wrapper for rtnorm1 */
SEXP rtnorm1_R(SEXP mean_R, SEXP sd_R, SEXP lower_R, SEXP upper_R)
{
    double mean = *REAL(mean_R);
    double sd = *REAL(sd_R);
    double lower = *REAL(lower_R);
    double upper = *REAL(upper_R);

    GetRNGstate();
    double ans = rtnorm1(mean, sd, lower, upper);
    PutRNGstate();
    return ScalarReal(ans);

}


SEXP
rpoisTrunc1_R(SEXP lambda_R, SEXP lower_R, SEXP upper_R, SEXP maxAttempt_R)
{
    double lambda = *REAL(lambda_R);
    int lower = *INTEGER(lower_R);
    int upper = *INTEGER(upper_R);
    int maxAttempt = *INTEGER(maxAttempt_R);

    GetRNGstate();
    int ans = rpoisTrunc1(lambda, lower, upper, maxAttempt);
    PutRNGstate();

    return ScalarInteger(ans);
}

SEXP
findOneRootLogPostSigmaNorm_R(SEXP sigma0_R, SEXP z_R, SEXP A_R, SEXP nu_R,
                            SEXP V_R, SEXP n_R, SEXP min_R, SEXP max_R)
{

    double sigma0 = *REAL(sigma0_R);
    double z = *REAL(z_R);
    double A = *REAL(A_R);
    double nu = *REAL(nu_R);
    double V = *REAL(V_R);
    int n = *INTEGER(n_R);
    double min = *REAL(min_R);
    double max = *REAL(max_R);

    double ans = findOneRootLogPostSigmaNorm(sigma0, z, A, nu,
                            V, n, min, max);
    return ScalarReal(ans);
}

SEXP
findOneRootLogPostSigmaRobust_R(SEXP sigma0_R, SEXP z_R, SEXP A_R,
                            SEXP nuBeta_R, SEXP nuTau_R,
                            SEXP V_R, SEXP n_R, SEXP min_R, SEXP max_R)
{

    double sigma0 = *REAL(sigma0_R);
    double z = *REAL(z_R);
    double A = *REAL(A_R);
    double nuBeta = *REAL(nuBeta_R);
    double nuTau = *REAL(nuTau_R);
    double V = *REAL(V_R);
    int n = *INTEGER(n_R);
    double min = *REAL(min_R);
    double max = *REAL(max_R);

    double ans = findOneRootLogPostSigmaRobust(sigma0, z, A,
                            nuBeta, nuTau,
                            V, n, min, max);
    return ScalarReal(ans);
}

/* one off wrapper for modePhiMix */
SEXP modePhiMix_R(SEXP level_R, SEXP meanLevel_R, SEXP nAlong_R,
              SEXP indexClassMax_R, SEXP omega_R, SEXP tolerance_R)
{
    double *level = REAL(level_R);
    double meanLevel = *REAL(meanLevel_R);
    int nAlong = *INTEGER(nAlong_R);
    int indexClassMax_r = *INTEGER(indexClassMax_R);
    double omega = *REAL(omega_R);
    double tolerance = *REAL(tolerance_R);
    double ans = modePhiMix(level, meanLevel, nAlong, indexClassMax_r,
                            omega, tolerance);
    return ScalarReal(ans);
}


//* one-off wrapper for safeLogProp_Binomial */
SEXP safeLogProp_Binomial_R(SEXP logit_th_new_R, SEXP logit_th_other_new_R,
              SEXP logit_th_old_R, SEXP logit_th_other_old_R, SEXP scale_R,
              SEXP weight_R, SEXP weight_other_R)
{
    double logit_th_new = *REAL(logit_th_new_R);
    double logit_th_other_new = *REAL(logit_th_other_new_R);
    double logit_th_old = *REAL(logit_th_old_R);
    double logit_th_other_old = *REAL(logit_th_other_old_R);
    double scale = *REAL(scale_R);
    double weight = *REAL(weight_R);
    double weight_other = *REAL(weight_other_R);

    double ans = safeLogProp_Binomial(logit_th_new,
                            logit_th_other_new,
                            logit_th_old,
                            logit_th_other_old,
                            scale,
                            weight,
                            weight_other);
    return ScalarReal(ans);
}

/* one-off wrapper for safeLogProp_Poisson */
SEXP safeLogProp_Poisson_R(SEXP log_th_new_R, SEXP log_th_other_new_R,
              SEXP log_th_old_R, SEXP log_th_other_old_R, SEXP scale_R,
              SEXP weight_R, SEXP weight_other_R)
{
    double log_th_new = *REAL(log_th_new_R);
    double log_th_other_new = *REAL(log_th_other_new_R);
    double log_th_old = *REAL(log_th_old_R);
    double log_th_other_old = *REAL(log_th_other_old_R);
    double scale = *REAL(scale_R);
    double weight = *REAL(weight_R);
    double weight_other = *REAL(weight_other_R);

    double ans = safeLogProp_Poisson(log_th_new,
                            log_th_other_new,
                            log_th_old,
                            log_th_other_old,
                            scale,
                            weight,
                            weight_other);
    return ScalarReal(ans);
}

/* wrapper logPostPhi functions */
LOGPOSTPHI_WRAPPER(logPostPhiMix);
LOGPOSTPHI_WRAPPER(logPostPhiFirstOrderMix);
LOGPOSTPHI_WRAPPER(logPostPhiSecondOrderMix);

BETAHAT_NOPRNG_WRAPPER_R(betaHat);
BETAHAT_NOPRNG_WRAPPER_R(betaHatAlphaDLM);
BETAHAT_NOPRNG_WRAPPER_R(betaHatCovariates);
BETAHAT_NOPRNG_WRAPPER_R(betaHatSeason);

UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_ExchFixed);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_ExchNormZero);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_ExchNormCov);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_ExchRobustZero);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_ExchRobustCov);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMNoTrendNormZeroNoSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMWithTrendNormZeroNoSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMNoTrendNormZeroWithSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMWithTrendNormZeroWithSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMNoTrendNormCovNoSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMWithTrendNormCovNoSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMNoTrendNormCovWithSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMWithTrendNormCovWithSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMNoTrendRobustZeroNoSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMWithTrendRobustZeroNoSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMNoTrendRobustZeroWithSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMWithTrendRobustZeroWithSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMNoTrendRobustCovNoSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMWithTrendRobustCovNoSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMNoTrendRobustCovWithSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_DLMWithTrendRobustCovWithSeason);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_KnownCertain);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_KnownUncertain);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_MixNormZero);
UPDATE_PRIORBETA_WRAPPER_R(updatePriorBeta_Zero);

UPDATEOBJECT_NOPRNG_WRAPPER_R(updateGWithTrend);
UPDATEOBJECT_WRAPPER_R(updateLatentComponentWeightMix);
UPDATEOBJECT_WRAPPER_R(updateLatentWeightMix);
UPDATEOBJECT_WRAPPER_R(updateLevelComponentWeightMix);
UPDATEOBJECT_WRAPPER_R(updateMeanLevelComponentWeightMix);
UPDATEOBJECT_WRAPPER_R(updateIndexClassMaxPossibleMix);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateIndexClassMaxUsedMix);
UPDATEPRIORWITHBETA_WRAPPER_R(updateIndexClassMix);
UPDATEPRIORWITHBETA_WRAPPER_R(updateVectorsMixAndProdVectorsMix);

SEXP
updateOmegaAlpha_R(SEXP prior_R, SEXP withTrend_R)
{
    int isWithTrend = *LOGICAL(withTrend_R);
    SEXP ans_R;
    PROTECT(ans_R = duplicate(prior_R));
    updateOmegaAlpha(ans_R, isWithTrend);
    UNPROTECT(1);
    return ans_R;
}

UPDATEOBJECT_NOPRNG_WRAPPER_R(updateOmegaComponentWeightMix);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateOmegaDelta);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateOmegaLevelComponentWeightMix);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateOmegaSeason);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateOmegaVectorsMix);

SEXP
updatePhi_R(SEXP prior_R, SEXP withTrend_R)
{
    int isWithTrend = *LOGICAL(withTrend_R);
    SEXP ans_R;
    PROTECT(ans_R = duplicate(prior_R));
    GetRNGstate();
    updatePhi(ans_R, isWithTrend);
    PutRNGstate();
    UNPROTECT(1);
    return ans_R;
}

UPDATEOBJECT_WRAPPER_R(updatePhiMix);
UPDATEOBJECT_WRAPPER_R(updateUEtaCoef);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateWSqrt);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateWSqrtInvG);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateWeightMix);

UPDATEPRIORWITHBETA_WRAPPER_R(updateTauNorm);

UPDATEPRIORWITHBETA_WRAPPER_R(updateUBeta);

/* wrap transferParam function; */
TRANSFERPARAM_WRAPPER_R(transferParamBetas);
TRANSFERPARAM_WRAPPER_R(transferParamPriorsBetas);
TRANSFERPARAM_WRAPPER_R(transferParamSigma);
TRANSFERPARAM_WRAPPER_R(transferParamVarsigma);

/* one off wrapper for transferLevelComponentWeightOldMix */
SEXP
transferLevelComponentWeightOldMix_R(SEXP values_R, SEXP offset_R,
                                SEXP nAlongOld_R, SEXP indexClassMax_R)
{
    double *values = REAL(values_R);
    int offset = *INTEGER(offset_R);
    int nAlongOld = *INTEGER(nAlongOld_R);
    int indexClassMax = *INTEGER(indexClassMax_R);
    SEXP ans_R;
    PROTECT(ans_R = allocVector(REALSXP, indexClassMax));
    double *ans = REAL(ans_R);
    transferLevelComponentWeightOldMix(ans, values, offset,
                                nAlongOld, indexClassMax);

    UNPROTECT(1);
    return ans_R;
}

/* wrapper for rmvnorm functions */
RMV_WRAPPER_R(rmvnorm1);
RMV_WRAPPER_R(rmvnorm2);

/* one-off wrapper for estimateOneChain */
SEXP estimateOneChain_R(SEXP object_R, SEXP filename_R,
                    SEXP nBurnin_R, SEXP nSim_R, SEXP nThin_R,
                    SEXP continuing_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(object_R));
    GetRNGstate();
    estimateOneChain(ans_R, filename_R, nBurnin_R, nSim_R,
                        nThin_R, continuing_R);
    PutRNGstate();
    UNPROTECT(1);
    return ans_R;
}

/* one off wrapper for chooseICellComp */
SEXP chooseICellComp_R(SEXP description_R)
{
    GetRNGstate();
    int ans = chooseICellComp(description_R);
    PutRNGstate();
    return ScalarInteger(ans);
}

/* one off wrapper for chooseICellCompUpperTri */
SEXP chooseICellCompUpperTri_R(SEXP description_R)
{
    GetRNGstate();
    int ans = chooseICellCompUpperTri(description_R);
    PutRNGstate();
    return ScalarInteger(ans);
}
/* one off wrapper for chooseICellOutInPool */
SEXP chooseICellOutInPool_R(SEXP description_R)
{
    SEXP ans_R;
    GetRNGstate();
    PROTECT(ans_R = chooseICellOutInPool(description_R));
    PutRNGstate();
    UNPROTECT(1);
    return ans_R;
}

/* one off wrapper for chooseICellSubAddNet */
SEXP chooseICellSubAddNet_R(SEXP description_R)
{
    SEXP ans_R;
    GetRNGstate();
    PROTECT(ans_R = chooseICellSubAddNet(description_R));
    PutRNGstate();
    UNPROTECT(1);
    return ans_R;
}

/* one off wrapper for chooseICellPopn */
SEXP chooseICellPopn_R(SEXP description_R)
{
    GetRNGstate();
    int ans = chooseICellPopn(description_R);
    PutRNGstate();
    return ScalarInteger(ans);
}

/* one off wrapper for isLowerTriangle */
SEXP isLowerTriangle_R(SEXP i_R, SEXP description_R)
{
    int i = *INTEGER(i_R);
    int ans = isLowerTriangle(i, description_R);
    return ScalarLogical(ans);
}

/* wrap getIMappingFunctions */
MAPPING_GET_I_WRAPPER(getIAccNextFromPopn);
MAPPING_GET_I_WRAPPER(getIPopnNextFromPopn);
MAPPING_GET_I_WRAPPER(getIExpFirstFromPopn);
MAPPING_GET_I_WRAPPER(getIPopnNextFromComp);
MAPPING_GET_I_WRAPPER(getIAccNextFromComp);
MAPPING_GET_IVEC_WRAPPER(getIPopnNextFromOrigDest);
MAPPING_GET_IVEC_WRAPPER(getIAccNextFromOrigDest);
MAPPING_GET_I_WRAPPER(getIExposureFromComp);
MAPPING_GET_I_WRAPPER(getIExposureFromBirths);
MAPPING_GET_I_WRAPPER(getIExposureFromOrigDest);
MAPPING_GET_I_WRAPPER(getIExpFirstFromComp);
MAPPING_GET_I_WRAPPER(getIExpFirstFromBirths);
MAPPING_GET_IVEC_WRAPPER(getIExpFirstPairFromOrigDest);
MAPPING_GET_I_WRAPPER(getICellCompFromExp);
MAPPING_GET_I_WRAPPER(getICellBirthsFromExp);
MAPPING_GET_I_WRAPPER(getICellLowerTriFromComp);
MAPPING_GET_I_WRAPPER(getICellLowerTriNextFromComp);

/* one off wrapper for getMinValCohortAccession */
SEXP getMinValCohortAccession_R(SEXP i_R, SEXP series_R, SEXP iterator_R)
{
    int i = *INTEGER(i_R);

    SEXP dup_R; /* iter_R is reset and advanced in getMinValCohortAccession */
    PROTECT(dup_R = duplicate(iterator_R));

    int ans = getMinValCohortAccession(i, series_R, dup_R);
    UNPROTECT(1);
    return ScalarInteger(ans);
}

/* one off wrapper for getMinValCohortPopulation */
SEXP getMinValCohortPopulation_R(SEXP i_R, SEXP series_R, SEXP iterator_R)
{
    int i = *INTEGER(i_R);

    SEXP dup_R; /* iter_R is reset and advanced in getMinValCohortPopulation */
    PROTECT(dup_R = duplicate(iterator_R));

    int ans = getMinValCohortPopulation(i, series_R, dup_R);
    UNPROTECT(1);
    return ScalarInteger(ans);
}

/* one off wrapper for resetCA */
SEXP resetCA_R(SEXP iterator_R, SEXP i_R)
{
    int i = *INTEGER(i_R);

    SEXP ans_R;
    PROTECT(ans_R = duplicate(iterator_R));
    resetCA(ans_R, i);
    UNPROTECT(1);
    return ans_R;
}

/* one off wrapper for resetCP */
SEXP resetCP_R(SEXP iterator_R, SEXP i_R)
{
    int i = *INTEGER(i_R);

    SEXP ans_R;
    PROTECT(ans_R = duplicate(iterator_R));
    resetCP(ans_R, i);
    UNPROTECT(1);
    return ans_R;
}

/* one off wrapper for resetCC */
SEXP resetCC_R(SEXP iterator_R, SEXP i_R)
{
    int i = *INTEGER(i_R);

    SEXP ans_R;
    PROTECT(ans_R = duplicate(iterator_R));
    resetCC(ans_R, i);
    UNPROTECT(1);
    return ans_R;
}

/* one off wrapper for resetCODPCP */
SEXP resetCODPCP_R(SEXP iterator_R, SEXP i_R)
{
    int i = *INTEGER(i_R);

    SEXP ans_R;
    PROTECT(ans_R = duplicate(iterator_R));
    resetCODPCP(ans_R, i);
    UNPROTECT(1);
    return ans_R;
}

/* Priors-methods */

/* predict */
PREDICTOBJECT_WRAPPER_R(predictAlphaDLMNoTrend);
PREDICTOBJECT_WRAPPER_R(predictAlphaDeltaDLMWithTrend);
PREDICTOBJECT_WRAPPER_R(predictAlphaLN2);

/* update betas */
SEXP
predictBeta_R(SEXP prior_R)
{
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));

    SEXP beta_R;
    PROTECT(beta_R = allocVector(REALSXP, J));
    double *beta = REAL(beta_R);

    GetRNGstate();

    predictBeta(beta, prior_R, J);

    PutRNGstate();

    UNPROTECT(1);
    return beta_R;
}

SEXP
transferAlphaDelta0_R(SEXP state_R, SEXP values_R, SEXP offset_R,
                    SEXP iteratorNew_R, SEXP iteratorOld_R)
{
    double *values = REAL(values_R);
    SEXP ans_R;
    PROTECT(ans_R = duplicate(state_R));
    double *ans = REAL(ans_R);

    int offset = *INTEGER(offset_R);

    transferAlphaDelta0(ans, values, offset, iteratorNew_R, iteratorOld_R);
    UNPROTECT(1);
    return ans_R;
}

SEXP
transferSeason0_R(SEXP s_R, SEXP nSeason_R, SEXP values_R, SEXP offset_R,
                    SEXP iteratorNew_R, SEXP iteratorOld_R)
{
    int nSeason = *INTEGER(nSeason_R);
    double *values = REAL(values_R);
    int offset = *INTEGER(offset_R);

    SEXP ans_R;
    PROTECT(ans_R = duplicate(s_R));

    transferSeason0(ans_R, nSeason, values, offset, iteratorNew_R, iteratorOld_R);
    UNPROTECT(1);
    return ans_R;
}


PREDICTOBJECT_WRAPPER_R(predictBetas);
PREDICTOBJECT_WRAPPER_R(predictComponentWeightMix);
PREDICTOBJECT_WRAPPER_R(predictIndexClassMix);
PREDICTOBJECT_WRAPPER_R(predictLevelComponentWeightMix);
PREDICTOBJECT_WRAPPER_R(predictPriorsBetas);
PREDICTOBJECT_WRAPPER_R(predictSeason);
PREDICTOBJECT_WRAPPER_R(predictUBeta);

/* miscellaneous-functions */

/* create one-off R version wrapper for dpoibin1_R
 * size could be an integer or a double - arrghh*/
SEXP
dpoibin1_R(SEXP x_R, SEXP size_R, SEXP prob_R, SEXP use_log_R)
{
    GetRNGstate();
    double ans = dpoibin1(*INTEGER(x_R), *INTEGER(size_R),
                *REAL(prob_R), *INTEGER(use_log_R));
    PutRNGstate();
    return ScalarReal(ans);
}

/* create one-off R version wrapper for invlogit1_R */
SEXP
invlogit1_R(SEXP x_R)
{
    double ans = invlogit1(*REAL(x_R));
    return ScalarReal(ans);
}

/* create one-off R version wrapper for rcateg1_R */
SEXP
rcateg1_R(SEXP cumProb_R)
{
    GetRNGstate();
    int ans = rcateg1( REAL(cumProb_R) );
    PutRNGstate();
    return ScalarInteger(ans);
}

/* create one-off R version wrapper for rhbinomTrunc1_R */
SEXP
rbinomTrunc1_R(SEXP size_R, SEXP prob_R, SEXP lower_R, SEXP upper_R, SEXP maxAttempt_R)
{
    GetRNGstate();
    int ans = rbinomTrunc1(*INTEGER(size_R),
                                *REAL(prob_R),
                                *INTEGER(lower_R),
                                *INTEGER(upper_R),
                                *INTEGER(maxAttempt_R));
    PutRNGstate();
    return ScalarInteger(ans);
}

/* create one-off R version wrapper for rhalftTrunc1_R */
SEXP
rhalftTrunc1_R(SEXP df_R, SEXP scale_R, SEXP max_R)
{
    GetRNGstate();
    double ans = rhalftTrunc1(*REAL(df_R), *REAL(scale_R), *REAL(max_R));
    PutRNGstate();
    return ScalarReal(ans);
}

/* create one-off R version wrapper for rinvchisq1_R */
SEXP
rinvchisq1_R(SEXP df_R, SEXP scaleSq_R)
{
    GetRNGstate();
    double ans = rinvchisq1(*REAL(df_R), *REAL(scaleSq_R));
    PutRNGstate();
    return ScalarReal(ans);
}

/* wrap filter and sampling operations */

/* wrap iterator operations */
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceA);
UPDATEOBJECT_NOPRNG_WRAPPER_R(resetA);
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceB);
UPDATEOBJECT_NOPRNG_WRAPPER_R(resetB);
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceD);
UPDATEOBJECT_NOPRNG_WRAPPER_R(resetD);
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceM);
UPDATEOBJECT_NOPRNG_WRAPPER_R(resetM);
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceS);
UPDATEOBJECT_NOPRNG_WRAPPER_R(resetS);
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceCA);
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceCP);
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceCC);
UPDATEOBJECT_NOPRNG_WRAPPER_R(advanceCODPCP);

/* one-off wrapper for centre RandomWalk
 * duplicates the iterator, which should be unchanged */
SEXP
centerA_R(SEXP vec_R, SEXP iterator_R)
{
    SEXP iteratorArg_R;
    PROTECT(iteratorArg_R = duplicate(iterator_R));
    SEXP ans;
    PROTECT(ans = centerA(vec_R, iteratorArg_R) );
    UNPROTECT(2);
    return ans;
}

/* create one-off R version wrapper for makeIOther_R */
SEXP
makeIOther_R(SEXP i_R, SEXP transform_R)
{
    GetRNGstate();
    int i = *INTEGER(i_R);
    int ans = makeIOther(i, transform_R);
    PutRNGstate();
    return ScalarInteger(ans);

}

/* one off wrapper for updateDataModelsCounts_R */
SEXP updateDataModelsCounts_R(SEXP y_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP transforms_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(dataModels_R));
    GetRNGstate();
    updateDataModelsCounts(y_R, ans_R,
                        datasets_R, transforms_R);
    PutRNGstate();

    UNPROTECT(1); /* ans_R */

    return ans_R;
}

/* one off wrapper for updateDataModelsAccount_R */
SEXP updateDataModelsAccount_R(SEXP combined_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(combined_R));
    GetRNGstate();
    updateDataModelsAccount(ans_R);
    PutRNGstate();

    UNPROTECT(1); /* ans_R */

    return ans_R;
}

/* wrap generic update functions for priors */

SEXP updateSDNorm_R(SEXP sigma_R,
                    SEXP A_R, SEXP nu_R, SEXP V_R, SEXP n_R, SEXP max_R)
{

    double sigma = *REAL(sigma_R);
    double A = *REAL(A_R);
    double nu = *REAL(nu_R);
    double V = *REAL(V_R);
    int n = *INTEGER(n_R);
    double max = *REAL(max_R);
    GetRNGstate();
    double ans = updateSDNorm(sigma, A, nu, V, n, max);
    PutRNGstate();
    return ScalarReal(ans);

}

SEXP updateSDRobust_R(SEXP sigma_R,
              SEXP A_R, SEXP nuBeta_R, SEXP nuTau_R, SEXP V_R, SEXP n_R, SEXP max_R)
{

    double sigma = *REAL(sigma_R);
    double A = *REAL(A_R);
    double nuBeta = *REAL(nuBeta_R);
    double nuTau = *REAL(nuTau_R);
    double V = *REAL(V_R);
    int n = *INTEGER(n_R);
    double max = *REAL(max_R);
    GetRNGstate();
    double ans = updateSDRobust(sigma, A, nuBeta, nuTau, V, n, max);
    PutRNGstate();
    return ScalarReal(ans);

}

UPDATEOBJECT_NOPRNG_WRAPPER_R(updateAlphaMix);
UPDATEPRIORWITHBETA_WRAPPER_R(updateEta);
UPDATEOBJECT_WRAPPER_R(updateComponentWeightMix);

UPDATEPRIORWITHBETA_WRAPPER_R(updateAlphaDLMNoTrend);
UPDATEPRIORWITHBETA_WRAPPER_R(updateAlphaDeltaDLMWithTrend);
UPDATEPRIORWITHBETA_WRAPPER_R(updateSeason);

/* updating betas */
UPDATEOBJECT_WRAPPER_R(updateBetas);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateLogPostBetas);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateMeansBetas);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateVariancesBetas);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateMu);
UPDATEOBJECT_WRAPPER_R(updatePriorsBetas);

/* wrap update sigma generic functions */
UPDATEOBJECT_WRAPPER_R(updateSigma_Varying);
UPDATEOBJECT_WRAPPER_R(updateSigmaLN2);

/* wrap update varsigma and theta generic functions */
UPDATEOBJECT_NOEXP_WRAPPER_R(updateVarsigma);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateVarsigmaLN2);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateTheta_BinomialVarying);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateTheta_BinomialVaryingAgCertain);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateThetaAndValueAgFun_Binomial);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateThetaAndValueAgNormal_Binomial);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateTheta_PoissonVaryingNotUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateTheta_PoissonVaryingUseExp);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateTheta_PoissonVaryingNotUseExpAgCertain);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateTheta_PoissonVaryingUseExpAgCertain);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateThetaAndValueAgNormal_PoissonNotUseExp);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateThetaAndValueAgPoisson_PoissonNotUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateThetaAndValueAgNormal_PoissonUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateThetaAndValueAgPoisson_PoissonUseExp);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateTheta_NormalVarying);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateTheta_NormalVaryingAgCertain);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateThetaAndValueAgNormal_Normal);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateThetaAndValueAgFun_Normal);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateThetaAndValueAgFun_PoissonNotUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateThetaAndValueAgFun_PoissonUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateThetaAndValueAgLife_PoissonUseExp);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateThetaAndNu_CMPVaryingNotUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateThetaAndNu_CMPVaryingUseExp);

/* wrap update Counts functions */
UPDATECOUNTS_NOEXP_WRAPPER_R(updateCountsPoissonNotUseExp);
UPDATECOUNTS_WITHEXP_WRAPPER_R(updateCountsPoissonUseExp);
UPDATECOUNTS_WITHEXP_WRAPPER_R(updateCountsBinomial);

/* wrap  transfer param model functions*/
TRANSFERPARAM_WRAPPER_R(transferParamModel);
TRANSFERPARAM_WRAPPER_R(transferParamModel_NormalVaryingVarsigmaKnownPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_NormalVaryingVarsigmaUnknownPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_PoissonVaryingNotUseExpPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_BinomialVaryingPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_PoissonVaryingUseExpPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_PoissonBinomialMixture);
TRANSFERPARAM_WRAPPER_R(transferParamModel_Round3);
TRANSFERPARAM_WRAPPER_R(transferParamModel_NormalFixedNotUseExpPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_NormalFixedUseExpPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_TFixedNotUseExpPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_TFixedUseExpPredict);
TRANSFERPARAM_WRAPPER_R(transferParamModel_LN2Predict);

/* wrap predict model functions */
UPDATEOBJECT_NOEXP_WRAPPER_R(predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict);
UPDATEOBJECT_NOEXP_WRAPPER_R(predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict);
UPDATEOBJECT_NOEXP_WRAPPER_R(predictModelNotUseExp_PoissonVaryingNotUseExpPredict);
UPDATEOBJECT_NOEXP_WRAPPER_R(predictModelNotUseExp_NormalFixedNotUseExpPredict);
UPDATEOBJECT_NOEXP_WRAPPER_R(predictModelNotUseExp_TFixedNotUseExpPredict);
UPDATEOBJECT_NOEXP_WRAPPER_R(predictModelNotUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(predictModelUseExp_BinomialVaryingPredict);
UPDATEOBJECT_WITHEXP_WRAPPER_R(predictModelUseExp_PoissonVaryingUseExpPredict);
UPDATEOBJECT_WITHEXP_WRAPPER_R(predictModelUseExp_PoissonBinomialMixturePredict);
UPDATEOBJECT_WITHEXP_WRAPPER_R(predictModelUseExp_Round3Predict);
UPDATEOBJECT_WITHEXP_WRAPPER_R(predictModelUseExp_NormalFixedUseExpPredict);
UPDATEOBJECT_WITHEXP_WRAPPER_R(predictModelUseExp_TFixedUseExpPredict);
UPDATEOBJECT_WITHEXP_WRAPPER_R(predictModelUseExp_LN2Predict);
UPDATEOBJECT_WITHEXP_WRAPPER_R(predictModelUseExp);


/* wrap update model functions */
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_CMPVaryingNotUseExp);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalVaryingVarsigmaKnown);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalVaryingVarsigmaUnknown);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_PoissonVaryingNotUseExp);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_PoissonVaryingNotUseExpAgCertain);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgFun);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_PoissonVaryingNotUseExpAgNormal);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_PoissonVaryingNotUseExpAgFun);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_PoissonVaryingNotUseExpAgPoisson);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_NormalFixedNotUseExp);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp_TFixedNotUseExp);
UPDATEOBJECT_NOEXP_WRAPPER_R(updateModelNotUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_CMPVaryingUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_BinomialVarying);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_PoissonVarying);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_PoissonBinomialMixture);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_Round3);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_BinomialVaryingAgCertain);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_BinomialVaryingAgNormal);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_BinomialVaryingAgFun);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_PoissonVaryingUseExpAgCertain);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_PoissonVaryingUseExpAgNormal);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_PoissonVaryingUseExpAgFun);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_PoissonVaryingUseExpAgPoisson);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_PoissonVaryingUseExpAgLife);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_NormalFixedUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_TFixedUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp_LN2);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateModelUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(updateAlphaLN2);


/* wrap draw model functions */
UPDATEOBJECT_NOEXP_WRAPPER_R(drawModelNotUseExp_NormalVaryingVarsigmaKnown);
UPDATEOBJECT_NOEXP_WRAPPER_R(drawModelNotUseExp_NormalVaryingVarsigmaUnknown);
UPDATEOBJECT_NOEXP_WRAPPER_R(drawModelNotUseExp_PoissonVarying);
UPDATEOBJECT_NOEXP_WRAPPER_R(drawModelNotUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(drawModelUseExp_BinomialVarying);
UPDATEOBJECT_WITHEXP_WRAPPER_R(drawModelUseExp_PoissonVarying);
UPDATEOBJECT_WITHEXP_WRAPPER_R(drawModelUseExp_PoissonBinomialMixture);
UPDATEOBJECT_WITHEXP_WRAPPER_R(drawModelUseExp_NormalFixedUseExp);
UPDATEOBJECT_WITHEXP_WRAPPER_R(drawModelUseExp_LN2);
UPDATEOBJECT_WITHEXP_WRAPPER_R(drawModelUseExp);

/* wrap draw combined model functions */
UPDATECOMBINEDOBJECT_WRAPPER_R(drawCombined_CombinedModelBinomial);
UPDATECOMBINEDOBJECT_WRAPPER_R(drawCombined_CombinedAccountMovements);
UPDATECOMBINEDOBJECT_WRAPPER_R(drawCombined);

/* wrap draw data models combined functions */
UPDATEOBJECT_WRAPPER_R(drawDataModels_CombinedAccountMovements);
UPDATEOBJECT_WRAPPER_R(drawDataModels);

/* wrap draw system models combined functions */
UPDATEOBJECT_WRAPPER_R(drawSystemModels_CombinedAccountMovements);
UPDATEOBJECT_WRAPPER_R(drawSystemModels);

/* wrap predict combined model functions */
PREDICTCOMBINEDOBJECT_WRAPPER_R(predictCombined_CombinedModelNormal);
PREDICTCOMBINEDOBJECT_WRAPPER_R(predictCombined_CombinedModelPoissonNotHasExp);
PREDICTCOMBINEDOBJECT_WRAPPER_R(predictCombined_CombinedModelBinomial);
PREDICTCOMBINEDOBJECT_WRAPPER_R(predictCombined_CombinedModelPoissonHasExp);
PREDICTCOMBINEDOBJECT_WRAPPER_R(predictCombined_CombinedCountsPoissonNotHasExp);
PREDICTCOMBINEDOBJECT_WRAPPER_R(predictCombined_CombinedCountsPoissonHasExp);
PREDICTCOMBINEDOBJECT_WRAPPER_R(predictCombined);

/* wrap repetitive update of combined model functions */
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedModelBinomial);
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedModelNormal);
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedModelPoissonNotHasExp);
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedModelPoissonHasExp);
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedModelCMPNotHasExp);
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedModelCMPHasExp);

/*  update combined CombinedAccount method */
SEXP
updateCombined_CombinedAccount_R(SEXP object_R, SEXP nUpdate_R)
{
    int nUpdate = *INTEGER(nUpdate_R);

    SEXP ans_R;
    PROTECT(ans_R = duplicate(object_R));
    int nProtected = 1;


    SEXP iteratorPopnDup_R = NULL;
    int hasIteratorPopn = 0;
    if(R_has_slot(object_R, iteratorPopn_sym)) {
        SEXP iteratorPopn_R = GET_SLOT(object_R, iteratorPopn_sym);

        PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
        hasIteratorPopn = 1;
        ++nProtected;
    }

    SEXP iteratorExposureDup_R = NULL;
    int hasIteratorExposure = 0;
    if(R_has_slot(object_R, iteratorExposure_sym)) {
        SEXP iteratorExposure_R = GET_SLOT(object_R, iteratorExposure_sym);

        PROTECT(iteratorExposureDup_R = duplicate(iteratorExposure_R));
        hasIteratorExposure = 1;
        ++nProtected;
    }

    SEXP iteratorsCompDup_R = NULL;
    int hasIteratorsComp = 0;
    if(R_has_slot(object_R, iteratorsComp_sym)) {
        SEXP iteratorsComp_R = GET_SLOT(object_R, iteratorsComp_sym);

        PROTECT(iteratorsCompDup_R = duplicate(iteratorsComp_R));
        hasIteratorsComp = 1;
        ++nProtected;
    }

    SEXP iteratorAccDup_R = NULL;
    int hasIteratorAcc = 0;
    if(R_has_slot(object_R, iteratorAcc_sym)) {
        SEXP iteratorAcc_R = GET_SLOT(object_R, iteratorAcc_sym);

        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
        hasIteratorAcc = 1;
        ++nProtected;
    }

    GetRNGstate();
    updateCombined_CombinedAccount(ans_R, nUpdate);
    PutRNGstate();

    if (hasIteratorPopn) {
        SET_SLOT(ans_R, iteratorPopn_sym, iteratorPopnDup_R);
    }
    if (hasIteratorExposure) {
        SET_SLOT(ans_R, iteratorExposure_sym, iteratorExposureDup_R);
    }
    if (hasIteratorsComp) {
        SET_SLOT(ans_R, iteratorsComp_sym, iteratorsCompDup_R);
    }
    if (hasIteratorAcc) {
        SET_SLOT(ans_R, iteratorAcc_sym, iteratorAccDup_R);
    }

    UNPROTECT(nProtected);
    return ans_R;
}


/* generic update combined object method */
SEXP
updateCombined_R(SEXP object_R, SEXP nUpdate_R)
{
    int nUpdate = *INTEGER(nUpdate_R);

    SEXP ans_R;
    PROTECT(ans_R = duplicate(object_R));
    int nProtected = 1;

    int i_method_combined = *(INTEGER(GET_SLOT(
                                    object_R, iMethodCombined_sym)));

    if ( (i_method_combined == 9) || (i_method_combined == 10) ) {
        SEXP iteratorPopnDup_R = NULL;
        int hasIteratorPopn = 0;
        if(R_has_slot(object_R, iteratorPopn_sym)) {
            SEXP iteratorPopn_R = GET_SLOT(object_R, iteratorPopn_sym);

            PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
            hasIteratorPopn = 1;
            ++nProtected;
        }

        SEXP iteratorExposureDup_R = NULL;
        int hasIteratorExposure = 0;
        if(R_has_slot(object_R, iteratorExposure_sym)) {
            SEXP iteratorExposure_R = GET_SLOT(object_R, iteratorExposure_sym);

            PROTECT(iteratorExposureDup_R = duplicate(iteratorExposure_R));
            hasIteratorExposure = 1;
            ++nProtected;
        }

        SEXP iteratorsCompDup_R = NULL;
        int hasIteratorsComp = 0;
        if(R_has_slot(object_R, iteratorsComp_sym)) {
            SEXP iteratorsComp_R = GET_SLOT(object_R, iteratorsComp_sym);

            PROTECT(iteratorsCompDup_R = duplicate(iteratorsComp_R));
            hasIteratorsComp = 1;
            ++nProtected;
        }

        SEXP iteratorAccDup_R = NULL;
        int hasIteratorAcc = 0;
        if(R_has_slot(object_R, iteratorAcc_sym)) {
            SEXP iteratorAcc_R = GET_SLOT(object_R, iteratorAcc_sym);

            PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
            hasIteratorAcc = 1;
            ++nProtected;
        }

        GetRNGstate();
        updateCombined(ans_R, nUpdate);
        PutRNGstate();

        if (hasIteratorPopn) {
            SET_SLOT(ans_R, iteratorPopn_sym, iteratorPopnDup_R);
        }
        if (hasIteratorExposure) {
            SET_SLOT(ans_R, iteratorExposure_sym, iteratorExposureDup_R);
        }
        if (hasIteratorsComp) {
            SET_SLOT(ans_R, iteratorsComp_sym, iteratorsCompDup_R);
        }
        if (hasIteratorAcc) {
            SET_SLOT(ans_R, iteratorAcc_sym, iteratorAccDup_R);
        }
    }
    else {
        GetRNGstate();
        updateCombined(ans_R, nUpdate);
        PutRNGstate();
    }

    UNPROTECT(nProtected);
    return ans_R;
}


/* combined objects with update-accounts functions */
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensAccount_CombinedAccountMovements);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensAccount);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogLikAccount_CombinedAccountMovements);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogLikAccount);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccount_CombinedAccountMovements);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccount);

/* generic update account values object method */
SEXP
updateValuesAccount_R(SEXP object_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(object_R));
    int nProtected = 1;

    SEXP iteratorPopnDup_R = NULL;
    int hasIteratorPopn = 0;
    if(R_has_slot(object_R, iteratorPopn_sym)) {
        SEXP iteratorPopn_R = GET_SLOT(object_R, iteratorPopn_sym);

        PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
        hasIteratorPopn = 1;
        ++nProtected;
    }

    SEXP iteratorExposureDup_R = NULL;
    int hasIteratorExposure = 0;
    if(R_has_slot(object_R, iteratorExposure_sym)) {
        SEXP iteratorExposure_R = GET_SLOT(object_R, iteratorExposure_sym);

        PROTECT(iteratorExposureDup_R = duplicate(iteratorExposure_R));
        hasIteratorExposure = 1;
        ++nProtected;
    }

    SEXP iteratorAccDup_R = NULL;
    int hasIteratorAcc = 0;
    if(R_has_slot(object_R, iteratorAcc_sym)) {
        SEXP iteratorAcc_R = GET_SLOT(object_R, iteratorAcc_sym);

        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
        hasIteratorAcc = 1;
        ++nProtected;
    }

    updateValuesAccount(ans_R);

    if (hasIteratorPopn) {
        SET_SLOT(ans_R, iteratorPopn_sym, iteratorPopnDup_R);
    }
    if (hasIteratorExposure) {
        SET_SLOT(ans_R, iteratorExposure_sym, iteratorExposureDup_R);
    }
    if (hasIteratorAcc) {
        SET_SLOT(ans_R, iteratorAcc_sym, iteratorAccDup_R);
    }

    UNPROTECT(nProtected);
    return ans_R;
}


SEXP
updateValuesAccount_CombinedAccountMovements_R(SEXP object_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(object_R));
    SEXP iteratorPopn_R = GET_SLOT(object_R, iteratorPopn_sym);
    SEXP iteratorPopnDup_R = NULL;
    PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
    SEXP iteratorExposure_R = GET_SLOT(object_R, iteratorExposure_sym);
    SEXP iteratorExposureDup_R = NULL;
    PROTECT(iteratorExposureDup_R = duplicate(iteratorExposure_R));

    int hasAge = *LOGICAL(GET_SLOT(object_R, hasAge_sym));
    int nProtected = 3;

    SEXP iteratorAccDup_R = NULL;
    if (hasAge) {
        SEXP iteratorAcc_R = GET_SLOT(object_R, iteratorAcc_sym);
        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
        ++nProtected;
    }

    updateValuesAccount_CombinedAccountMovements(ans_R);

    SET_SLOT(ans_R, iteratorPopn_sym, iteratorPopnDup_R);
    SET_SLOT(ans_R, iteratorExposure_sym, iteratorExposureDup_R);

    if (hasAge) {
        SET_SLOT(ans_R, iteratorAcc_sym, iteratorAccDup_R);
    }

    UNPROTECT(nProtected);
    return ans_R;
}

UPDATEOBJECT_NOPRNG_WRAPPER_R(updateExpectedExposure);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateExpectedExposure_CombinedAccountMovements);
UPDATEOBJECT_WRAPPER_R(updateSystemModels);
UPDATEOBJECT_WRAPPER_R(updateSystemModels_CombinedAccountMovements);

/* wrap repetitive update of combined counts functions */
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedCountsPoissonNotHasExp);
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedCountsPoissonHasExp);
UPDATECOMBINEDOBJECT_WRAPPER_R(updateCombined_CombinedCountsBinomial);

/* wrap loglikelihood functions */
LOGLIKELIHOOD_WRAPPER_R(logLikelihood_Binomial);
LOGLIKELIHOOD_WRAPPER_R(logLikelihood_CMP);
LOGLIKELIHOOD_WRAPPER_R(logLikelihood_Poisson);
LOGLIKELIHOOD_WRAPPER_R(logLikelihood_PoissonBinomialMixture);
LOGLIKELIHOOD_WRAPPER_R(logLikelihood_Round3);
LOGLIKELIHOOD_WRAPPER_R(logLikelihood_NormalFixedUseExp);
LOGLIKELIHOOD_WRAPPER_R(logLikelihood_TFixedUseExp);
LOGLIKELIHOOD_WRAPPER_R(logLikelihood_LN2);
LOGLIKELIHOOD_WRAPPER_R(logLikelihood);

/* wrap predict prior functions */
PREDICTOBJECT_WRAPPER_R(predictPrior);
PREDICTOBJECT_WRAPPER_R(predictPrior_ExchFixed);
PREDICTOBJECT_WRAPPER_R(predictPrior_ExchNormZero);
PREDICTOBJECT_WRAPPER_R(predictPrior_ExchNormCov);
PREDICTOBJECT_WRAPPER_R(predictPrior_ExchRobustZero);
PREDICTOBJECT_WRAPPER_R(predictPrior_ExchRobustCov);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMNoTrendNormZeroNoSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMWithTrendNormZeroNoSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMNoTrendNormZeroWithSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMWithTrendNormZeroWithSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMNoTrendNormCovNoSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMWithTrendNormCovNoSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMNoTrendNormCovWithSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMWithTrendNormCovWithSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMNoTrendRobustZeroNoSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMWithTrendRobustZeroNoSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMNoTrendRobustZeroWithSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMWithTrendRobustZeroWithSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMNoTrendRobustCovNoSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMWithTrendRobustCovNoSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMNoTrendRobustCovWithSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_DLMWithTrendRobustCovWithSeasonPredict);
PREDICTOBJECT_WRAPPER_R(predictPrior_KnownCertain);
PREDICTOBJECT_WRAPPER_R(predictPrior_KnownUncertain);
PREDICTOBJECT_WRAPPER_R(predictPrior_MixNormZero);
PREDICTOBJECT_WRAPPER_R(predictPrior_Zero);

/* wrap draw prior functions */
UPDATEOBJECT_WRAPPER_R(drawPrior);
UPDATEOBJECT_WRAPPER_R(drawPrior_ExchFixed);
UPDATEOBJECT_WRAPPER_R(drawPrior_ExchNormZero);
UPDATEOBJECT_WRAPPER_R(drawPrior_ExchNormCov);
UPDATEOBJECT_WRAPPER_R(drawPrior_ExchRobustZero);
UPDATEOBJECT_WRAPPER_R(drawPrior_ExchRobustCov);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMNoTrendNormZeroNoSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMWithTrendNormZeroNoSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMNoTrendNormZeroWithSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMWithTrendNormZeroWithSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMNoTrendNormCovNoSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMWithTrendNormCovNoSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMNoTrendNormCovWithSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMWithTrendNormCovWithSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMNoTrendRobustZeroNoSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMWithTrendRobustZeroNoSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMNoTrendRobustZeroWithSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMWithTrendRobustZeroWithSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMNoTrendRobustCovNoSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMWithTrendRobustCovNoSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMNoTrendRobustCovWithSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_DLMWithTrendRobustCovWithSeason);
UPDATEOBJECT_WRAPPER_R(drawPrior_KnownCertain);
UPDATEOBJECT_WRAPPER_R(drawPrior_KnownUncertain);
UPDATEOBJECT_WRAPPER_R(drawPrior_MixNormZero);
UPDATEOBJECT_WRAPPER_R(drawPrior_Zero);


TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_ExchNormZero);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_ExchNormCov);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_ExchRobustZero);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_ExchRobustCov);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMNoTrendNormZeroNoSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMWithTrendNormZeroNoSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMNoTrendNormZeroWithSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMWithTrendNormZeroWithSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMNoTrendNormCovNoSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMWithTrendNormCovNoSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMNoTrendNormCovWithSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMWithTrendNormCovWithSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMNoTrendRobustZeroNoSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMWithTrendRobustZeroNoSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMNoTrendRobustZeroWithSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMWithTrendRobustZeroWithSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMNoTrendRobustCovNoSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMWithTrendRobustCovNoSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMNoTrendRobustCovWithSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_DLMWithTrendRobustCovWithSeasonPredict);
TRANSFERPARAMPRIOR_WRAPPER_R(transferParamPrior_MixNormZeroPredict);

/* CMP functions */

/* one off wrapper for logDensCMPUnnormalised1_R */
SEXP logDensCMPUnnormalised1_R(SEXP x_R, SEXP gamma_R, SEXP nu_R)
{
    int x = *INTEGER(x_R);
    double gam = *REAL(gamma_R);
    double nu = *REAL(nu_R);

    double ans = logDensCMPUnnormalised1(x, gam, nu);

    return ScalarReal(ans);
}


RCMP_WRAPPER_R(rcmpUnder);
RCMP_WRAPPER_R(rcmpOver);
RCMP_WRAPPER_R(rcmp1);

/* *************************** update-account -------------------------- */


/* generic update account object method */
SEXP
updateAccount_R(SEXP object_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(object_R));
    int nProtected = 1;

    SEXP iteratorPopnDup_R = NULL;
    int hasIteratorPopn = 0;
    if(R_has_slot(object_R, iteratorPopn_sym)) {
        SEXP iteratorPopn_R = GET_SLOT(object_R, iteratorPopn_sym);

        PROTECT(iteratorPopnDup_R = duplicate(iteratorPopn_R));
        hasIteratorPopn = 1;
        ++nProtected;
    }

    SEXP iteratorExposureDup_R = NULL;
    int hasIteratorExposure = 0;
    if(R_has_slot(object_R, iteratorExposure_sym)) {
        SEXP iteratorExposure_R = GET_SLOT(object_R, iteratorExposure_sym);

        PROTECT(iteratorExposureDup_R = duplicate(iteratorExposure_R));
        hasIteratorExposure = 1;
        ++nProtected;
    }

    SEXP iteratorsCompDup_R = NULL;
    int hasIteratorsComp = 0;
    if(R_has_slot(object_R, iteratorsComp_sym)) {
        SEXP iteratorsComp_R = GET_SLOT(object_R, iteratorsComp_sym);

        PROTECT(iteratorsCompDup_R = duplicate(iteratorsComp_R));
        hasIteratorsComp = 1;
        ++nProtected;
    }

    SEXP iteratorAccDup_R = NULL;
    int hasIteratorAcc = 0;
    if(R_has_slot(object_R, iteratorAcc_sym)) {
        SEXP iteratorAcc_R = GET_SLOT(object_R, iteratorAcc_sym);

        PROTECT(iteratorAccDup_R = duplicate(iteratorAcc_R));
        hasIteratorAcc = 1;
        ++nProtected;
    }

    GetRNGstate();
    updateAccount(ans_R);
    PutRNGstate();

    if (hasIteratorPopn) {
        SET_SLOT(ans_R, iteratorPopn_sym, iteratorPopnDup_R);
    }
    if (hasIteratorExposure) {
        SET_SLOT(ans_R, iteratorExposure_sym, iteratorExposureDup_R);
    }
    if (hasIteratorsComp) {
        SET_SLOT(ans_R, iteratorsComp_sym, iteratorsCompDup_R);
    }
    if (hasIteratorAcc) {
        SET_SLOT(ans_R, iteratorAcc_sym, iteratorAccDup_R);
    }

    UNPROTECT(nProtected);
    return ans_R;
}


UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMovePopn);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMoveBirths);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMoveBirthsSmall);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMoveOrigDest);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMoveOrigDestSmall);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMovePool);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMoveNet);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMoveComp);
UPDATEOBJECT_WRAPPER_UPDATEPROPOSAL_R(updateProposalAccountMoveCompSmall);

/* wrapper for diffLogLikAccountMovePopn */
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogLikAccountMovePopn);

/* one-off wrapper for diffLogLikPopn */
SEXP
diffLogLikPopn_R(SEXP diff_R, SEXP iFirst_R, SEXP iterator_R,
                            SEXP population_R, SEXP dataModels_R,
                            SEXP datasets_R, SEXP seriesIndices_R,
                            SEXP transforms_R)
{
    int diff = *INTEGER(diff_R);
    int iFirst_r = *INTEGER(iFirst_R);
    SEXP iteratornew_R;
    PROTECT(iteratornew_R = duplicate(iterator_R));
    double ans = diffLogLikPopn(diff, iFirst_r, iteratornew_R,
                                    population_R, dataModels_R,
                                    datasets_R, seriesIndices_R,
                                    transforms_R);
    UNPROTECT(1);
    return ScalarReal(ans);
}

/* one-off wrapper for diffLogLikPopnOneDataset */
SEXP
diffLogLikPopnOneDataset_R(SEXP diff_R, SEXP iFirst_R, SEXP iterator_R,
                            SEXP population_R, SEXP model_R,
                            SEXP dataset_R, SEXP transform_R)
{
    int diff = *INTEGER(diff_R);
    int iFirst_r = *INTEGER(iFirst_R);
    SEXP iteratornew_R;
    PROTECT(iteratornew_R = duplicate(iterator_R));
    double ans = diffLogLikPopnOneDataset(diff, iFirst_r, iteratornew_R,
                                    population_R, model_R,
                                    dataset_R, transform_R);
    UNPROTECT(1);
    return ScalarReal(ans);
}

/* one-off wrapper for diffLogLikPopnOneCell */
SEXP
diffLogLikPopnOneCell_R(SEXP iAfter_R, SEXP diff_R, SEXP population_R,
                SEXP model_R, SEXP dataset_R, SEXP transform_R)
{
    int iAfter_r = *INTEGER(iAfter_R);
    int diff = *INTEGER(diff_R);
    double ans = diffLogLikPopnOneCell(iAfter_r, diff, population_R,
                                    model_R, dataset_R, transform_R);
    return ScalarReal(ans);
}

/* wrapper for diffLogLikAccountMoveOrigDest */
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogLikAccountMoveOrigDest);


/* one-off wrapper for diffLogLikCellComp */
SEXP
diffLogLikCellComp_R(SEXP diff_R, SEXP iComp_R, SEXP iCell_R,
                            SEXP component_R, SEXP dataModels_R,
                            SEXP datasets_R, SEXP seriesIndices_R,
                            SEXP transforms_R)
{
    int diff = *INTEGER(diff_R);
    int iComp_r = *INTEGER(iComp_R);
    int iCell_r = *INTEGER(iCell_R);
    double ans = diffLogLikCellComp(diff, iComp_r, iCell_r,
                                    component_R, dataModels_R,
                                    datasets_R, seriesIndices_R,
                                    transforms_R);
    return ScalarReal(ans);
}

/* one-off wrapper for diffLogLikCellOneDataset */
SEXP
diffLogLikCellOneDataset_R(SEXP diff_R, SEXP iCell_R, SEXP component_R,
                SEXP model_R, SEXP dataset_R, SEXP transform_R)
{
    int iCell_r = *INTEGER(iCell_R);
    int diff = *INTEGER(diff_R);
    double ans = diffLogLikCellOneDataset(diff, iCell_r, component_R,
                                    model_R, dataset_R, transform_R);
    return ScalarReal(ans);
}

/* one-off wrapper for diffLogLikPopnPair */
SEXP
diffLogLikPopnPair_R(SEXP diff_R, SEXP iPopnOrig_R, SEXP iPopnDest_R,
                            SEXP iterator_R,
                            SEXP population_R, SEXP dataModels_R,
                            SEXP datasets_R, SEXP seriesIndices_R,
                            SEXP transforms_R)
{
    int diff = *INTEGER(diff_R);
    int iPopnOrig_r = *INTEGER(iPopnOrig_R);
    int iPopnDest_r = *INTEGER(iPopnDest_R);
    SEXP iteratornew_R;
    PROTECT(iteratornew_R = duplicate(iterator_R));
    double ans = diffLogLikPopnPair(diff, iPopnOrig_r, iPopnDest_r,
                                    iteratornew_R,
                                    population_R, dataModels_R,
                                    datasets_R, seriesIndices_R,
                                    transforms_R);
    UNPROTECT(1);
    return ScalarReal(ans);
}

/* wrapper for diffLogLikAccountMovePool */
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogLikAccountMovePool);


/* one-off wrapper for diffLogLikCellsPool */
SEXP
diffLogLikCellsPool_R(SEXP diff_R, SEXP iComp_R,
                            SEXP iCellOut_R, SEXP iCellIn_R,
                            SEXP component_R, SEXP dataModels_R,
                            SEXP datasets_R, SEXP seriesIndices_R,
                            SEXP transforms_R)
{
    int diff = *INTEGER(diff_R);
    int iComp_r = *INTEGER(iComp_R);
    int iCellOut_r = *INTEGER(iCellOut_R);
    int iCellIn_r = *INTEGER(iCellIn_R);
    double ans = diffLogLikCellsPool(diff, iComp_r, iCellOut_r, iCellIn_r,
                                    component_R, dataModels_R,
                                    datasets_R, seriesIndices_R,
                                    transforms_R);
    return ScalarReal(ans);
}

/* wrapper for diffLogLikAccountMoveNet */
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogLikAccountMoveNet);


/* one-off wrapper for diffLogLikCellsNet */
SEXP
diffLogLikCellsNet_R(SEXP diff_R, SEXP iComp_R,
                            SEXP iCellAdd_R, SEXP iCellSub_R,
                            SEXP component_R, SEXP dataModels_R,
                            SEXP datasets_R, SEXP seriesIndices_R,
                            SEXP transforms_R)
{
    int diff = *INTEGER(diff_R);
    int iComp_r = *INTEGER(iComp_R);
    int iCellAdd_r = *INTEGER(iCellAdd_R);
    int iCellSub_r = *INTEGER(iCellSub_R);
    double ans = diffLogLikCellsNet(diff, iComp_r, iCellAdd_r, iCellSub_r,
                                    component_R, dataModels_R,
                                    datasets_R, seriesIndices_R,
                                    transforms_R);
    return ScalarReal(ans);
}


/* wrapper for diffLogLikAccountMoveComp */
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogLikAccountMoveComp);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogLikAccountMoveCompSmall);

/* wrapper for diffLogDensPopn */
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensPopn);

/* one-off wrapper for diffLogDensPopnOneCohort */
SEXP
diffLogDensPopnOneCohort_R(SEXP diff_R, SEXP population_R, SEXP i_R,
               SEXP iterator_R, SEXP theta_R, SEXP strucZeroArray_R)
{
    int diff = *INTEGER(diff_R);
    int i_r = *INTEGER(i_R);
    double * theta = REAL(theta_R);
    int * strucZeroArray = INTEGER(strucZeroArray_R);
    SEXP iteratornew_R;
    PROTECT(iteratornew_R = duplicate(iterator_R));
    double ans = diffLogDensPopnOneCohort (diff, population_R, i_r,
                       iteratornew_R, theta, strucZeroArray);
    UNPROTECT(1);
    return ScalarReal(ans);
}

/* wrapper for diffLogDensExpPopn */
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensExpPopn);

/* diffLogDensExpOneOrigDestParChPool */
SEXP diffLogDensExpOneOrigDestParChPool_R(SEXP iCell_R, SEXP hasAge_R,
                        SEXP ageTimeStep_R, SEXP updatedPopn_R,
                        SEXP component_R, SEXP theta_R,
                        SEXP strucZeroArray_R,
                        SEXP iteratorComp_R,
                        SEXP iExpFirst_R, SEXP exposure_R,
                        SEXP iteratorExposure_R,
                        SEXP diff_R)
{
    int diff = *INTEGER(diff_R);
    int iCell_r = *INTEGER(iCell_R);
    int hasAge = *LOGICAL(hasAge_R);
    double ageTimeStep = *REAL(ageTimeStep_R);
    int updatedPopn = *LOGICAL(updatedPopn_R);
    double * theta = REAL(theta_R);
    int * strucZeroArray = INTEGER(strucZeroArray_R);
    double * exposure = REAL(exposure_R);
    int iExpFirst_r = *INTEGER(iExpFirst_R);
    SEXP iteratorCompNew_R;
    PROTECT(iteratorCompNew_R = duplicate(iteratorComp_R));
    SEXP iteratorExpNew_R;
    PROTECT(iteratorExpNew_R = duplicate(iteratorExposure_R));
    double ans = diffLogDensExpOneOrigDestParChPool(iCell_r, hasAge,
                        ageTimeStep, updatedPopn,
                        component_R, theta, strucZeroArray,
                        iteratorCompNew_R,
                        iExpFirst_r, exposure,
                        iteratorExpNew_R,
                        diff);
    UNPROTECT(2);
    return ScalarReal(ans);
}
/*.Call(diffLogDensExpOneComp_R,
              iCell, hasAge, ageTimeStep, updatedPopn,
              component, theta, strucZeroArray, iteratorComp,
              iExpFirst, exposure, iteratorExposure, diff)*/

/* diffLogDensExpOneComp */
SEXP diffLogDensExpOneComp_R(SEXP iCell_R, SEXP hasAge_R,
                        SEXP ageTimeStep_R, SEXP updatedPopn_R,
                        SEXP component_R, SEXP theta_R,
                        SEXP strucZeroArray_R,
                        SEXP iteratorComp_R,
                        SEXP iExpFirst_R, SEXP exposure_R,
                        SEXP iteratorExposure_R,
                        SEXP diff_R)
{
    int diff = *INTEGER(diff_R);
    int iCell_r = *INTEGER(iCell_R);
    int hasAge = *LOGICAL(hasAge_R);
    double ageTimeStep = *REAL(ageTimeStep_R);
    int updatedPopn = *LOGICAL(updatedPopn_R);
    double * theta = REAL(theta_R);
    int * strucZeroArray = INTEGER(strucZeroArray_R);
    double * exposure = REAL(exposure_R);
    int iExpFirst_r = *INTEGER(iExpFirst_R);
    SEXP iteratorCompNew_R;
    PROTECT(iteratorCompNew_R = duplicate(iteratorComp_R));
    SEXP iteratorExpNew_R;
    PROTECT(iteratorExpNew_R = duplicate(iteratorExposure_R));
    double ans = diffLogDensExpOneComp(iCell_r, hasAge,
                        ageTimeStep, updatedPopn,
                        component_R, theta, strucZeroArray,
                        iteratorCompNew_R,
                        iExpFirst_r, exposure,
                        iteratorExpNew_R,
                        diff);
    UNPROTECT(2);
    return ScalarReal(ans);
}

DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensJumpOrigDest);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensExpOrigDestPoolNet);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensJumpPoolWithExpose);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensJumpPoolNoExpose);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensJumpNet);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensJumpComp);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensExpComp);
DIFFLOGLIKCOMBINED_WRAPPER_R(diffLogDensCompSmall);

UPDATEOBJECT_NOPRNG_WRAPPER_R(updateCellMove);
UPDATEOBJECT_NOPRNG_WRAPPER_R(updateAccSmall);

/* duplicates the iterator and replaces the unused iterator when finished
 * used when calling the code directly from R */
SEXP
updateSubsequentPopnMove_R(SEXP combined_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(combined_R));
    SEXP iterator_R = GET_SLOT(combined_R, iteratorPopn_sym);
    SEXP iteratorDup_R = NULL;
    PROTECT(iteratorDup_R = duplicate(iterator_R));

    updateSubsequentPopnMove(ans_R);

    SET_SLOT(ans_R, iteratorPopn_sym, iteratorDup_R);

    UNPROTECT(2);
    return ans_R;
}


/* duplicates the iterator and replaces the unused iterator when finished
 * used when calling the code directly from R */
SEXP
updateSubsequentAccMove_R(SEXP combined_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(combined_R));
    SEXP iterator_R = GET_SLOT(combined_R, iteratorAcc_sym);
    SEXP iteratorDup_R = NULL;
    PROTECT(iteratorDup_R = duplicate(iterator_R));

    updateSubsequentAccMove(ans_R);

    SET_SLOT(ans_R, iteratorAcc_sym, iteratorDup_R);

    UNPROTECT(2);
    return ans_R;
}

/* duplicates the iterator and replaces the unused iterator when finished
 * used when calling the code directly from R */
SEXP
updateSubsequentExpMove_R(SEXP combined_R)
{
    SEXP ans_R;
    PROTECT(ans_R = duplicate(combined_R));
    SEXP iterator_R = GET_SLOT(combined_R, iteratorExposure_sym);
    SEXP iteratorDup_R = NULL;
    PROTECT(iteratorDup_R = duplicate(iterator_R));

    updateSubsequentExpMove(ans_R);

    SET_SLOT(ans_R, iteratorExposure_sym, iteratorDup_R);

    UNPROTECT(2);
    return ans_R;
}


/* helper_simulate */
UPDATEOBJECT_WRAPPER_R(drawAlphaLN2);
UPDATEOBJECT_WRAPPER_R(drawBetas);
UPDATEOBJECT_WRAPPER_R(drawDataModelsAccount);
UPDATEOBJECT_WRAPPER_R(drawDelta0);
UPDATEOBJECT_WRAPPER_R(drawEta);
UPDATEOBJECT_WRAPPER_R(drawOmegaAlpha);
UPDATEOBJECT_WRAPPER_R(drawOmegaComponentWeightMix);
UPDATEOBJECT_WRAPPER_R(drawOmegaDelta);
UPDATEOBJECT_WRAPPER_R(drawOmegaLevelComponentWeightMix);
UPDATEOBJECT_WRAPPER_R(drawOmegaSeason);
UPDATEOBJECT_WRAPPER_R(drawOmegaVectorsMix);
UPDATEOBJECT_WRAPPER_R(drawPhi);
UPDATEOBJECT_WRAPPER_R(drawPhiMix);
UPDATEOBJECT_WRAPPER_R(drawPriors);
UPDATEOBJECT_WRAPPER_R(drawSigma_Varying);
UPDATEOBJECT_WRAPPER_R(drawTau);
UPDATEOBJECT_WRAPPER_R(drawUEtaCoef);
UPDATEOBJECT_WRAPPER_R(drawVarsigma);

/* ******************************************************************************* */
/* Create table describing R-visible versions of C functions ********************* */
/* ******************************************************************************* */

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

static const
R_CallMethodDef callMethods[] = {

  /* update betas */

  CALLDEF(updateGWithTrend_R, 1),
  CALLDEF(updateLatentComponentWeightMix_R, 1),
  CALLDEF(updateLatentWeightMix_R, 1),
  CALLDEF(updateLevelComponentWeightMix_R, 1),
  CALLDEF(updateMeanLevelComponentWeightMix_R, 1),
  CALLDEF(updateIndexClassMaxPossibleMix_R, 1),
  CALLDEF(updateIndexClassMaxUsedMix_R, 1),
  CALLDEF(updateIndexClassMix_R, 2),
  CALLDEF(updateVectorsMixAndProdVectorsMix_R, 2),
  CALLDEF(updateOmegaAlpha_R, 2),
  CALLDEF(updateOmegaComponentWeightMix_R, 1),
  CALLDEF(updateOmegaDelta_R, 1),
  CALLDEF(updateOmegaLevelComponentWeightMix_R, 1),
  CALLDEF(updateOmegaSeason_R, 1),
  CALLDEF(updateOmegaVectorsMix_R, 1),
  CALLDEF(updatePhi_R, 2),
  CALLDEF(updatePhiMix_R, 1),
  CALLDEF(updateUEtaCoef_R, 1),
  CALLDEF(updateWSqrt_R, 1),
  CALLDEF(updateWSqrtInvG_R, 1),
  CALLDEF(updateWeightMix_R, 1),
  CALLDEF(updateTauNorm_R, 2),
  CALLDEF(updateTauRobust_R, 1),

  CALLDEF(updateUBeta_R, 2),

  CALLDEF(updatePriorBeta_R, 4),
  CALLDEF(updatePriorBeta_ExchFixed_R, 4),
  CALLDEF(updatePriorBeta_ExchNormZero_R, 4),
  CALLDEF(updatePriorBeta_ExchNormCov_R, 4),
  CALLDEF(updatePriorBeta_ExchRobustZero_R, 4),
  CALLDEF(updatePriorBeta_ExchRobustCov_R, 4),
  CALLDEF(updatePriorBeta_DLMNoTrendNormZeroNoSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMWithTrendNormZeroNoSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMNoTrendNormZeroWithSeason_R,4),
  CALLDEF(updatePriorBeta_DLMWithTrendNormZeroWithSeason_R,4),
  CALLDEF(updatePriorBeta_DLMNoTrendNormCovNoSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMWithTrendNormCovNoSeason_R,4),
  CALLDEF(updatePriorBeta_DLMNoTrendNormCovWithSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMWithTrendNormCovWithSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMNoTrendRobustZeroNoSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMWithTrendRobustZeroNoSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMNoTrendRobustZeroWithSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMWithTrendRobustZeroWithSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMNoTrendRobustCovNoSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMWithTrendRobustCovNoSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMNoTrendRobustCovWithSeason_R, 4),
  CALLDEF(updatePriorBeta_DLMWithTrendRobustCovWithSeason_R, 4),
  CALLDEF(updatePriorBeta_KnownCertain_R, 4),
  CALLDEF(updatePriorBeta_KnownUncertain_R, 4),
  CALLDEF(updatePriorBeta_MixNormZero_R, 4),
  CALLDEF(updatePriorBeta_Zero_R, 4),

  /* helper-functions */
  CALLDEF(makeMu_R, 3),
  CALLDEF(dpoibin1_R, 4),
  CALLDEF(invlogit1_R, 1),
  CALLDEF(rcateg1_R, 1),
  CALLDEF(rinvchisq1_R, 2),
  CALLDEF(rbinomTrunc1_R, 5),
  CALLDEF(rhalftTrunc1_R, 3),
  CALLDEF(rmvnorm1_R, 2),
  CALLDEF(rmvnorm2_R, 2),
  CALLDEF(rnormTruncated_R, 8),
  CALLDEF(rnormIntTrunc1_R, 4),
  CALLDEF(rtnorm1_R, 4),
  CALLDEF(rpoisTrunc1_R, 4),

  CALLDEF(betaHat_R, 1),
  CALLDEF(betaHatAlphaDLM_R, 1),
  CALLDEF(betaHatCovariates_R, 1),
  CALLDEF(betaHatSeason_R, 1),
  CALLDEF(findOneRootLogPostSigmaNorm_R, 8),
  CALLDEF(findOneRootLogPostSigmaRobust_R, 9),
  CALLDEF(getV_R, 1),

  CALLDEF(makeVBarAndN_R, 2),
  CALLDEF(logPostPhiMix_R, 6),
  CALLDEF(logPostPhiFirstOrderMix_R, 6),
  CALLDEF(logPostPhiSecondOrderMix_R, 6),
  CALLDEF(makeLifeExpBirth_R, 5),
  CALLDEF(modePhiMix_R, 6),
  CALLDEF(safeLogProp_Binomial_R, 7),
  CALLDEF(safeLogProp_Poisson_R, 7),

  CALLDEF(predictAlphaDLMNoTrend_R, 1),
  CALLDEF(predictAlphaDeltaDLMWithTrend_R, 1),
  CALLDEF(predictAlphaLN2_R, 1),
  CALLDEF(predictBeta_R, 1),
  CALLDEF(transferAlphaDelta0_R, 5),
  CALLDEF(transferSeason0_R, 6),

  CALLDEF(predictBetas_R, 1),
  CALLDEF(predictComponentWeightMix_R, 1),
  CALLDEF(predictIndexClassMix_R, 1),
  CALLDEF(predictLevelComponentWeightMix_R, 1),
  CALLDEF(predictPriorsBetas_R, 1),
  CALLDEF(predictSeason_R, 1),
  CALLDEF(predictUBeta_R, 1),

  CALLDEF(transferParamBetas_R, 4),

  CALLDEF(transferParamPriorsBetas_R,4),
  CALLDEF(transferParamSigma_R,4),
  CALLDEF(transferParamVarsigma_R,4),
  CALLDEF(transferLevelComponentWeightOldMix_R, 4),


  CALLDEF(centerA_R, 2),
  CALLDEF(diff_R, 2),
  CALLDEF(logLikelihood_Binomial_R,4),
  CALLDEF(logLikelihood_CMP_R,4),
  CALLDEF(logLikelihood_Poisson_R,4),
  CALLDEF(logLikelihood_PoissonBinomialMixture_R,4),
  CALLDEF(logLikelihood_Round3_R,4),
  CALLDEF(logLikelihood_NormalFixedUseExp_R, 4),
  CALLDEF(logLikelihood_TFixedUseExp_R, 4),
  CALLDEF(logLikelihood_LN2_R, 4),
  CALLDEF(diffLogLik_R,6),
  CALLDEF(makeIOther_R,2),

  /* iterators */
  CALLDEF(advanceA_R, 1),
  CALLDEF(resetA_R, 1),
  CALLDEF(advanceB_R, 1),
  CALLDEF(resetB_R, 1),
  CALLDEF(advanceD_R, 1),
  CALLDEF(resetD_R, 1),
  CALLDEF(advanceM_R, 1),
  CALLDEF(resetM_R, 1),
  CALLDEF(advanceS_R, 1),
  CALLDEF(resetS_R, 1),
  CALLDEF(advanceCA_R, 1),
  CALLDEF(advanceCP_R, 1),
  CALLDEF(advanceCC_R, 1),
  CALLDEF(advanceCODPCP_R, 1),
  CALLDEF(resetCA_R, 2),
  CALLDEF(resetCP_R, 2),
  CALLDEF(resetCC_R, 2),
  CALLDEF(resetCODPCP_R, 2),

  /* update-nongeneric) */
  CALLDEF(updateSDNorm_R, 6),
  CALLDEF(updateSDRobust_R, 7),

  CALLDEF(updateAlphaMix_R, 1),

  CALLDEF(updateEta_R, 2),
  CALLDEF(updateComponentWeightMix_R, 1),

  CALLDEF(updateAlphaDLMNoTrend_R, 2),
  CALLDEF(updateAlphaDeltaDLMWithTrend_R, 2),
  CALLDEF(updateSeason_R, 2),

  CALLDEF(updateBetas_R, 1),
  CALLDEF(updateLogPostBetas_R, 1),
  CALLDEF(updateMeansBetas_R, 1),
  CALLDEF(updateVariancesBetas_R, 1),
  CALLDEF(updateMu_R, 1),

  CALLDEF(updateSigma_Varying_R, 1),
  CALLDEF(updateSigmaLN2_R, 1),

  CALLDEF(updateTheta_BinomialVarying_R, 3),
  CALLDEF(updateTheta_BinomialVaryingAgCertain_R, 3),
  CALLDEF(updateThetaAndValueAgFun_Binomial_R, 3),
  CALLDEF(updateThetaAndValueAgNormal_Binomial_R, 3),
  CALLDEF(updateTheta_PoissonVaryingNotUseExp_R, 2),
  CALLDEF(updateTheta_PoissonVaryingUseExp_R, 3),
  CALLDEF(updateTheta_PoissonVaryingNotUseExpAgCertain_R, 2),
  CALLDEF(updateTheta_PoissonVaryingUseExpAgCertain_R, 3),
  CALLDEF(updateThetaAndValueAgNormal_PoissonNotUseExp_R, 2),
  CALLDEF(updateThetaAndValueAgPoisson_PoissonNotUseExp_R, 2),
  CALLDEF(updateThetaAndValueAgNormal_PoissonUseExp_R, 3),
  CALLDEF(updateThetaAndValueAgPoisson_PoissonUseExp_R,3),
  CALLDEF(updateTheta_NormalVarying_R, 2),
  CALLDEF(updateTheta_NormalVaryingAgCertain_R, 2),
  CALLDEF(updateThetaAndValueAgNormal_Normal_R, 2),
  CALLDEF(updateThetaAndValueAgFun_Normal_R, 2),
  CALLDEF(updateThetaAndValueAgFun_PoissonNotUseExp_R, 2),
  CALLDEF(updateThetaAndValueAgFun_PoissonUseExp_R, 3),
  CALLDEF(updateThetaAndValueAgLife_PoissonUseExp_R, 3),
  CALLDEF(updateThetaAndNu_CMPVaryingNotUseExp_R, 2),
  CALLDEF(updateThetaAndNu_CMPVaryingUseExp_R, 3),

  CALLDEF(updateVarsigma_R, 2),
  CALLDEF(updateVarsigmaLN2_R, 3),

  /* update counts */
  CALLDEF(updateCountsPoissonNotUseExp_R, 5),
  CALLDEF(updateCountsPoissonUseExp_R, 6),
  CALLDEF(updateCountsBinomial_R, 6),

  /* update dataModels and datasets */
  CALLDEF(updateDataModelsCounts_R, 4),
  CALLDEF(updateDataModelsAccount_R, 1),

  /* models */
  CALLDEF(logLikelihood_R, 4),

  CALLDEF(transferParamModel_R, 4),
  CALLDEF(transferParamModel_NormalVaryingVarsigmaKnownPredict_R, 4),
  CALLDEF(transferParamModel_NormalVaryingVarsigmaUnknownPredict_R, 4),
  CALLDEF(transferParamModel_PoissonVaryingNotUseExpPredict_R, 4),
  CALLDEF(transferParamModel_BinomialVaryingPredict_R, 4),
  CALLDEF(transferParamModel_PoissonVaryingUseExpPredict_R, 4),
  CALLDEF(transferParamModel_PoissonBinomialMixture_R, 4),
  CALLDEF(transferParamModel_Round3_R, 4),
  CALLDEF(transferParamModel_NormalFixedNotUseExpPredict_R, 4),
  CALLDEF(transferParamModel_NormalFixedUseExpPredict_R, 4),
  CALLDEF(transferParamModel_TFixedNotUseExpPredict_R, 4),
  CALLDEF(transferParamModel_TFixedUseExpPredict_R, 4),
  CALLDEF(transferParamModel_LN2Predict_R, 4),

  CALLDEF(predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict_R, 2),
  CALLDEF(predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict_R, 2),
  CALLDEF(predictModelNotUseExp_PoissonVaryingNotUseExpPredict_R, 2),
  CALLDEF(predictModelNotUseExp_NormalFixedNotUseExpPredict_R, 2),
  CALLDEF(predictModelNotUseExp_TFixedNotUseExpPredict_R, 2),
  CALLDEF(predictModelNotUseExp_R, 2),

  CALLDEF(predictModelUseExp_BinomialVaryingPredict_R, 3),
  CALLDEF(predictModelUseExp_PoissonVaryingUseExpPredict_R, 3),
  CALLDEF(predictModelUseExp_PoissonBinomialMixturePredict_R, 3),
  CALLDEF(predictModelUseExp_Round3Predict_R, 3),
  CALLDEF(predictModelUseExp_NormalFixedUseExpPredict_R, 3),
  CALLDEF(predictModelUseExp_TFixedUseExpPredict_R, 3),
  CALLDEF(predictModelUseExp_LN2Predict_R, 3),
  CALLDEF(predictModelUseExp_R, 3),

  CALLDEF(updateModelNotUseExp_CMPVaryingNotUseExp_R, 2),
  CALLDEF(updateModelNotUseExp_NormalVaryingVarsigmaKnown_R, 2),
  CALLDEF(updateModelNotUseExp_NormalVaryingVarsigmaUnknown_R, 2),
  CALLDEF(updateModelNotUseExp_PoissonVaryingNotUseExp_R, 2),
  CALLDEF(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain_R, 2),
  CALLDEF(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain_R, 2),
  CALLDEF(updateModelNotUseExp_PoissonVaryingNotUseExpAgCertain_R, 2),
  CALLDEF(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal_R, 2),
  CALLDEF(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal_R, 2),
  CALLDEF(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgFun_R, 2),
  CALLDEF(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun_R, 2),
  CALLDEF(updateModelNotUseExp_PoissonVaryingNotUseExpAgNormal_R, 2),
  CALLDEF(updateModelNotUseExp_PoissonVaryingNotUseExpAgFun_R, 2),
  CALLDEF(updateModelNotUseExp_PoissonVaryingNotUseExpAgPoisson_R, 2),
  CALLDEF(updateModelNotUseExp_NormalFixedNotUseExp_R, 2),
  CALLDEF(updateModelNotUseExp_TFixedNotUseExp_R, 2),
  CALLDEF(updateModelNotUseExp_R, 2),

  CALLDEF(updateModelUseExp_CMPVaryingUseExp_R, 3),
  CALLDEF(updateModelUseExp_BinomialVarying_R, 3),
  CALLDEF(updateModelUseExp_PoissonVarying_R, 3),
  CALLDEF(updateModelUseExp_PoissonBinomialMixture_R, 3),
  CALLDEF(updateModelUseExp_Round3_R, 3),
  CALLDEF(updateModelUseExp_BinomialVaryingAgCertain_R, 3),
  CALLDEF(updateModelUseExp_BinomialVaryingAgNormal_R, 3),
  CALLDEF(updateModelUseExp_BinomialVaryingAgFun_R, 3),
  CALLDEF(updateModelUseExp_PoissonVaryingUseExpAgCertain_R, 3),
  CALLDEF(updateModelUseExp_PoissonVaryingUseExpAgNormal_R, 3),
  CALLDEF(updateModelUseExp_PoissonVaryingUseExpAgFun_R, 3),
  CALLDEF(updateModelUseExp_PoissonVaryingUseExpAgPoisson_R, 3),
  CALLDEF(updateModelUseExp_PoissonVaryingUseExpAgLife_R, 3),
  CALLDEF(updateModelUseExp_NormalFixedUseExp_R, 3),
  CALLDEF(updateModelUseExp_TFixedUseExp_R, 3),
  CALLDEF(updateModelUseExp_LN2_R, 3),
  CALLDEF(updateModelUseExp_R, 3),

  CALLDEF(updateAlphaLN2_R, 3),

  CALLDEF(updatePriorsBetas_R, 1),

  CALLDEF(drawModelNotUseExp_NormalVaryingVarsigmaKnown_R, 2),
  CALLDEF(drawModelNotUseExp_NormalVaryingVarsigmaUnknown_R, 2),
  CALLDEF(drawModelNotUseExp_PoissonVarying_R, 2),
  CALLDEF(drawModelNotUseExp_R, 2),

  CALLDEF(drawModelUseExp_BinomialVarying_R, 3),
  CALLDEF(drawModelUseExp_PoissonVarying_R, 3),
  CALLDEF(drawModelUseExp_PoissonBinomialMixture_R, 3),
  CALLDEF(drawModelUseExp_NormalFixedUseExp_R, 3),
  CALLDEF(drawModelUseExp_LN2_R, 3),
  CALLDEF(drawModelUseExp_R, 3),

  /* draw combined */
  CALLDEF(drawCombined_CombinedModelBinomial_R, 2),
  CALLDEF(drawCombined_CombinedAccountMovements_R, 2),
  CALLDEF(drawCombined_R, 2),

  /* draw data models*/
  CALLDEF(drawDataModels_CombinedAccountMovements_R, 1),
  CALLDEF(drawDataModels_R, 1),

  /* draw system models*/
  CALLDEF(drawSystemModels_CombinedAccountMovements_R, 1),
  CALLDEF(drawSystemModels_R, 1),

  /* predict combined */
  CALLDEF(predictCombined_CombinedModelNormal_R, 4),
  CALLDEF(predictCombined_CombinedModelPoissonHasExp_R, 4),
  CALLDEF(predictCombined_CombinedModelBinomial_R, 4),
  CALLDEF(predictCombined_CombinedModelPoissonNotHasExp_R, 4),
  CALLDEF(predictCombined_CombinedCountsPoissonNotHasExp_R, 4),
  CALLDEF(predictCombined_CombinedCountsPoissonHasExp_R, 4),
  CALLDEF(predictCombined_R, 4),

  /* update combined */
  CALLDEF(updateCombined_CombinedModelBinomial_R, 2),
  CALLDEF(updateCombined_CombinedModelNormal_R, 2),
  CALLDEF(updateCombined_CombinedModelPoissonNotHasExp_R, 2),
  CALLDEF(updateCombined_CombinedModelPoissonHasExp_R, 2),
  CALLDEF(updateCombined_CombinedModelCMPNotHasExp_R, 2),
  CALLDEF(updateCombined_CombinedModelCMPHasExp_R, 2),
  CALLDEF(updateCombined_CombinedAccount_R, 2),
  CALLDEF(updateCombined_R, 2),

  CALLDEF(updateCombined_CombinedCountsPoissonNotHasExp_R, 2),
  CALLDEF(updateCombined_CombinedCountsPoissonHasExp_R, 2),
  CALLDEF(updateCombined_CombinedCountsBinomial_R, 2),

  CALLDEF(diffLogDensAccount_CombinedAccountMovements_R, 1),
  CALLDEF(diffLogDensAccount_R, 1),
  CALLDEF(diffLogLikAccount_CombinedAccountMovements_R, 1),
  CALLDEF(diffLogLikAccount_R, 1),
  CALLDEF(updateProposalAccount_CombinedAccountMovements_R, 1),
  CALLDEF(updateProposalAccount_R, 1),
  CALLDEF(updateValuesAccount_CombinedAccountMovements_R, 1),
  CALLDEF(updateValuesAccount_R, 1),
  CALLDEF(updateExpectedExposure_CombinedAccountMovements_R, 1),
  CALLDEF(updateExpectedExposure_R, 1),
  CALLDEF(updateSystemModels_CombinedAccountMovements_R, 1),
  CALLDEF(updateSystemModels_R, 1),

  CALLDEF(estimateOneChain_R, 6),

  CALLDEF(getOneIterFromFile_R, 5),
  CALLDEF(getDataFromFile_R, 5),
  CALLDEF(overwriteValuesOnFile_R, 5),

  /* description helpers */
  CALLDEF(chooseICellComp_R, 1),
  CALLDEF(chooseICellCompUpperTri_R, 1),
  CALLDEF(chooseICellOutInPool_R, 1),
  CALLDEF(chooseICellPopn_R, 1),
  CALLDEF(chooseICellSubAddNet_R, 1),
  CALLDEF(isLowerTriangle_R, 2),
  CALLDEF(getIAccNextFromPopn_R, 2),
  CALLDEF(getIPopnNextFromPopn_R, 2),
  CALLDEF(getIExpFirstFromPopn_R, 2),
  CALLDEF(getMinValCohortAccession_R, 3),
  CALLDEF(getMinValCohortPopulation_R, 3),

  /* mapping functions */
  CALLDEF(getIPopnNextFromComp_R, 2),
  CALLDEF(getIPopnNextFromOrigDest_R, 2),

  CALLDEF(getIAccNextFromComp_R, 2),
  CALLDEF(getIAccNextFromOrigDest_R, 2),
  CALLDEF(getIExposureFromComp_R, 2),
  CALLDEF(getIExposureFromBirths_R, 2),
  CALLDEF(getIExposureFromOrigDest_R, 2),
  CALLDEF(getIExpFirstFromComp_R, 2),
  CALLDEF(getIExpFirstFromBirths_R, 2),
  CALLDEF(getIExpFirstPairFromOrigDest_R, 2),
  CALLDEF(getICellCompFromExp_R, 2),
  CALLDEF(getICellBirthsFromExp_R, 2),
  CALLDEF(getICellLowerTriFromComp_R, 2),
  CALLDEF(getICellLowerTriNextFromComp_R, 2),

  /*predict priors*/
  CALLDEF(predictPrior_R, 1),
  CALLDEF(predictPrior_ExchFixed_R, 1),
  CALLDEF(predictPrior_ExchNormZero_R, 1),
  CALLDEF(predictPrior_ExchNormCov_R, 1),
  CALLDEF(predictPrior_ExchRobustZero_R, 1),
  CALLDEF(predictPrior_ExchRobustCov_R, 1),
  CALLDEF(predictPrior_DLMNoTrendNormZeroNoSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMWithTrendNormZeroNoSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMNoTrendNormZeroWithSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMWithTrendNormZeroWithSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMNoTrendNormCovNoSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMWithTrendNormCovNoSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMNoTrendNormCovWithSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMWithTrendNormCovWithSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMNoTrendRobustZeroNoSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMWithTrendRobustZeroNoSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMNoTrendRobustZeroWithSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMWithTrendRobustZeroWithSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMNoTrendRobustCovNoSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMWithTrendRobustCovNoSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMNoTrendRobustCovWithSeasonPredict_R, 1),
  CALLDEF(predictPrior_DLMWithTrendRobustCovWithSeasonPredict_R, 1),
  CALLDEF(predictPrior_KnownCertain_R, 1),
  CALLDEF(predictPrior_KnownUncertain_R, 1),
  CALLDEF(predictPrior_MixNormZero_R, 1),
  CALLDEF(predictPrior_Zero_R, 1),

  /*draw priors*/
  CALLDEF(drawPrior_R, 1),
  CALLDEF(drawPrior_ExchFixed_R, 1),
  CALLDEF(drawPrior_ExchNormZero_R, 1),
  CALLDEF(drawPrior_ExchNormCov_R, 1),
  CALLDEF(drawPrior_ExchRobustZero_R, 1),
  CALLDEF(drawPrior_ExchRobustCov_R, 1),
  CALLDEF(drawPrior_DLMNoTrendNormZeroNoSeason_R, 1),
  CALLDEF(drawPrior_DLMWithTrendNormZeroNoSeason_R, 1),
  CALLDEF(drawPrior_DLMNoTrendNormZeroWithSeason_R, 1),
  CALLDEF(drawPrior_DLMWithTrendNormZeroWithSeason_R, 1),
  CALLDEF(drawPrior_DLMNoTrendNormCovNoSeason_R, 1),
  CALLDEF(drawPrior_DLMWithTrendNormCovNoSeason_R, 1),
  CALLDEF(drawPrior_DLMNoTrendNormCovWithSeason_R, 1),
  CALLDEF(drawPrior_DLMWithTrendNormCovWithSeason_R, 1),
  CALLDEF(drawPrior_DLMNoTrendRobustZeroNoSeason_R, 1),
  CALLDEF(drawPrior_DLMWithTrendRobustZeroNoSeason_R, 1),
  CALLDEF(drawPrior_DLMNoTrendRobustZeroWithSeason_R, 1),
  CALLDEF(drawPrior_DLMWithTrendRobustZeroWithSeason_R, 1),
  CALLDEF(drawPrior_DLMNoTrendRobustCovNoSeason_R, 1),
  CALLDEF(drawPrior_DLMWithTrendRobustCovNoSeason_R, 1),
  CALLDEF(drawPrior_DLMNoTrendRobustCovWithSeason_R, 1),
  CALLDEF(drawPrior_DLMWithTrendRobustCovWithSeason_R, 1),
  CALLDEF(drawPrior_KnownCertain_R, 1),
  CALLDEF(drawPrior_KnownUncertain_R, 1),
  CALLDEF(drawPrior_MixNormZero_R, 1),
  CALLDEF(drawPrior_Zero_R, 1),

  CALLDEF(transferParamPrior_R, 2),
  CALLDEF(transferParamPrior_ExchNormZero_R, 2),
  CALLDEF(transferParamPrior_ExchNormCov_R, 2),
  CALLDEF(transferParamPrior_ExchRobustZero_R, 2),
  CALLDEF(transferParamPrior_ExchRobustCov_R, 2),
  CALLDEF(transferParamPrior_DLMNoTrendNormZeroNoSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMWithTrendNormZeroNoSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMNoTrendNormZeroWithSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMWithTrendNormZeroWithSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMNoTrendNormCovNoSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMWithTrendNormCovNoSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMNoTrendNormCovWithSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMWithTrendNormCovWithSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMNoTrendRobustZeroNoSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMWithTrendRobustZeroNoSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMNoTrendRobustZeroWithSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMWithTrendRobustZeroWithSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMNoTrendRobustCovNoSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMWithTrendRobustCovNoSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMNoTrendRobustCovWithSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_DLMWithTrendRobustCovWithSeasonPredict_R, 2),
  CALLDEF(transferParamPrior_MixNormZeroPredict_R, 2),

  /* CMP */
  CALLDEF(logDensCMPUnnormalised1_R, 3),
  CALLDEF(rcmpUnder_R, 3),
  CALLDEF(rcmpOver_R, 3),
  CALLDEF(rcmp1_R, 3),

  /* update-account */
  CALLDEF(updateAccount_R, 1),
  CALLDEF(updateProposalAccountMovePopn_R, 1),
  CALLDEF(updateProposalAccountMoveBirths_R, 1),
  CALLDEF(updateProposalAccountMoveBirthsSmall_R, 1),
  CALLDEF(updateProposalAccountMoveOrigDest_R, 1),
  CALLDEF(updateProposalAccountMoveOrigDestSmall_R, 1),
  CALLDEF(updateProposalAccountMovePool_R, 1),
  CALLDEF(updateProposalAccountMoveNet_R, 1),
  CALLDEF(updateProposalAccountMoveComp_R, 1),
  CALLDEF(updateProposalAccountMoveCompSmall_R, 1),

  CALLDEF(diffLogLikAccountMovePopn_R, 1),
  CALLDEF(diffLogLikPopn_R, 8),
  CALLDEF(diffLogLikPopnOneDataset_R, 7),
  CALLDEF(diffLogLikPopnOneCell_R, 6),
  CALLDEF(diffLogLikAccountMoveOrigDest_R, 1),
  CALLDEF(diffLogLikCellComp_R, 8),
  CALLDEF(diffLogLikCellOneDataset_R,6),
  CALLDEF(diffLogLikPopnPair_R, 9),
  CALLDEF(diffLogLikAccountMovePool_R, 1),
  CALLDEF(diffLogLikCellsPool_R, 9),
  CALLDEF(diffLogLikAccountMoveNet_R, 1),
  CALLDEF(diffLogLikCellsNet_R, 9),
  CALLDEF(diffLogLikAccountMoveComp_R, 1),
  CALLDEF(diffLogLikAccountMoveCompSmall_R, 1),

  CALLDEF(diffLogDensPopn_R, 1),
  CALLDEF(diffLogDensPopnOneCohort_R, 6),
  CALLDEF(diffLogDensExpPopn_R, 1),
  CALLDEF(diffLogDensExpOneOrigDestParChPool_R, 12),
  CALLDEF(diffLogDensExpOneComp_R, 12),
  CALLDEF(diffLogDensJumpOrigDest_R, 1),
  CALLDEF(diffLogDensExpOrigDestPoolNet_R, 1),
  CALLDEF(diffLogDensJumpPoolWithExpose_R, 1),
  CALLDEF(diffLogDensJumpPoolNoExpose_R, 1),
  CALLDEF(diffLogDensJumpNet_R, 1),
  CALLDEF(diffLogDensJumpComp_R, 1),
  CALLDEF(diffLogDensExpComp_R, 1),
  CALLDEF(diffLogDensCompSmall_R, 1),

  CALLDEF(updateAccSmall_R, 1),
  CALLDEF(updateCellMove_R, 1),
  CALLDEF(updateSubsequentPopnMove_R, 1),
  CALLDEF(updateSubsequentAccMove_R, 1),
  CALLDEF(updateSubsequentExpMove_R, 1),

  /* helper-simulate */
  CALLDEF(drawAlphaLN2_R, 1),
  CALLDEF(drawBetas_R, 1),
  CALLDEF(drawDataModelsAccount_R, 1),
  CALLDEF(drawDelta0_R, 1),
  CALLDEF(drawEta_R, 1),
  CALLDEF(drawOmegaAlpha_R, 1),
  CALLDEF(drawOmegaComponentWeightMix_R, 1),
  CALLDEF(drawOmegaDelta_R, 1),
  CALLDEF(drawOmegaLevelComponentWeightMix_R, 1),
  CALLDEF(drawOmegaSeason_R, 1),
  CALLDEF(drawOmegaVectorsMix_R, 1),
  CALLDEF(drawPhi_R, 1),
  CALLDEF(drawPhiMix_R, 1),
  CALLDEF(drawPriors_R, 1),
  CALLDEF(drawSigma_Varying_R, 1),
  CALLDEF(drawTau_R, 1),
  CALLDEF(drawUEtaCoef_R, 1),
  CALLDEF(drawVarsigma_R, 1),

  {NULL}
};


/* ******************************************************************************* */
/* Register R-visible versions of C functions, plus symbols ********************** */
/* ******************************************************************************* */

void
R_init_demest(DllInfo *info)
{

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE); /* can't find any documentation on this */
    /* see R Programming for Bioinformatics, 2009, pg 189 -
     * "The call to R_useDynamicSymbols indicates that if the correct C entry point
     * is not found in the shared library, then an error should be signaled.
     * Currently, the default behavior in R is to search all other loaded shared
     * libraries for the symbol, which is fairly dangerous behavior. If you have
     *  registered all routines in your library, then you should set this to FALSE
     * as is done in the stats package." */

  /* populate pointers declared in header file demest.h
   * see http://tolstoy.newcastle.edu.au/R/e5/devel/08/11/0646.html */
  dembase_Collapse_R = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("dembase", "collapse_R");
  dembase_Extend_R = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("dembase", "extend_R");

  dembase_getIAfter = (int(*)(int, SEXP)) R_GetCCallable("dembase", "getIAfter");
  dembase_getIBefore = (SEXP(*)(int, SEXP)) R_GetCCallable("dembase", "getIBefore");
  dembase_getIShared = (SEXP(*)(int, SEXP)) R_GetCCallable("dembase", "getIShared");


  Data_sym = install(".Data");

/* everything here must be declared in "x_sym" form in demest.h */
#define ADD_SYM(name) name##_sym = install(#name)
  /* betas */
  ADD_SYM(beta);
  /* priors */
  ADD_SYM(iMethodPrior);
  ADD_SYM(Z);
  ADD_SYM(eta);
  ADD_SYM(gamma);
  ADD_SYM(lower);
  ADD_SYM(tau);
  ADD_SYM(tauMax);
  ADD_SYM(upper);
  /* random walk priors */
  ADD_SYM(order);
  ADD_SYM(iteratorBeta);
  /* iterators */
  ADD_SYM(iWithin);
  ADD_SYM(nWithin);
  ADD_SYM(iBetween);
  ADD_SYM(nBetween);
  ADD_SYM(incrementBetween);
  ADD_SYM(indices);
  ADD_SYM(initial);
  ADD_SYM(dimIterators);
  ADD_SYM(strideLengths);
  ADD_SYM(nStrides);
  ADD_SYM(dimBefore);
  ADD_SYM(dimAfter);
  ADD_SYM(posDim);
  ADD_SYM(lengthDim);
  /* models */
  ADD_SYM(logPostPriorsBetas);
  ADD_SYM(logPostBetas);
  ADD_SYM(logPostSigma);
  ADD_SYM(logPostTheta);
  ADD_SYM(logPostVarsigma);
  ADD_SYM(iMethodModel);
  ADD_SYM(priorsBetas);
  ADD_SYM(theta);
  ADD_SYM(thetaTransformed);
  ADD_SYM(cellInLik);
  ADD_SYM(mu);
  ADD_SYM(sigma);
  ADD_SYM(sigmaMax);
  ADD_SYM(ASigma);
  ADD_SYM(nuSigma);
  ADD_SYM(varsigma);
  ADD_SYM(varsigmaMax);
  ADD_SYM(varsigmaSetToZero);
  ADD_SYM(AVarsigma);
  ADD_SYM(nuVarsigma);
  ADD_SYM(w);
  ADD_SYM(scaleTheta);
  ADD_SYM(scaleThetaMultiplier);
  ADD_SYM(nAcceptTheta);
  ADD_SYM(betas);
  ADD_SYM(iteratorBetas);
  ADD_SYM(dims);
  ADD_SYM(prob);
  ADD_SYM(meansBetas);
  ADD_SYM(variancesBetas);
  ADD_SYM(betaEqualsMean);
  ADD_SYM(acceptBeta);
  ADD_SYM(tolerance);
  ADD_SYM(betaIsPredicted);
  ADD_SYM(mean);
  ADD_SYM(sd);

  /* ag */
  ADD_SYM(maxAttempt);
  ADD_SYM(nFailedPropTheta);
  ADD_SYM(nFailedPropYStar);
  ADD_SYM(valueAg);
  ADD_SYM(weightAg);
  ADD_SYM(transformAg);
  ADD_SYM(meanAg);
  ADD_SYM(sdAg);
  ADD_SYM(scaleAg);
  ADD_SYM(nAcceptAg);
  ADD_SYM(nFailedPropValueAg);
  ADD_SYM(funAg);
  ADD_SYM(xArgsAg);
  ADD_SYM(weightsArgsAg);
  ADD_SYM(mxAg);
  ADD_SYM(axAg);
  ADD_SYM(nxAg);
  ADD_SYM(nAgeAg);
  ADD_SYM(transformThetaToMxAg);

  /* y */
  ADD_SYM(subtotals);
  ADD_SYM(transformSubtotals);
  ADD_SYM(subtotalsNet);
  /* combined */
  ADD_SYM(slotsToExtract);
  ADD_SYM(iMethodCombined);
  ADD_SYM(model);
  ADD_SYM(exposure);
  ADD_SYM(y);
  ADD_SYM(dataModels);
  ADD_SYM(datasets);
  ADD_SYM(transforms);
  ADD_SYM(seriesIndices);
  ADD_SYM(updateComponent);
  ADD_SYM(updateDataModel);
  ADD_SYM(updateSystemModel);

  ADD_SYM(J);

  ADD_SYM(UC);
  ADD_SYM(DC);
  ADD_SYM(UR);
  ADD_SYM(DCInv);
  ADD_SYM(DRInv);
  ADD_SYM(CC);
  ADD_SYM(a);
  ADD_SYM(R);
  ADD_SYM(priorsW);
  ADD_SYM(v);
  ADD_SYM(forward);
  ADD_SYM(offsetsPriorsBetas);
  ADD_SYM(offsetsSigma);
  ADD_SYM(offsetsVarsigma);
  ADD_SYM(offsetsBetas);
  ADD_SYM(m0);
  ADD_SYM(C0);
  ADD_SYM(CC);
  ADD_SYM(phi);
  ADD_SYM(w);
  ADD_SYM(iteratorGamma);
  ADD_SYM(iteratorV);
  ADD_SYM(delta);
  ADD_SYM(phiKnown);
/* description */
  ADD_SYM(nTime);
  ADD_SYM(stepTime);
  ADD_SYM(hasAge);
  ADD_SYM(hasSex);
  ADD_SYM(nAge);
  ADD_SYM(stepAge);
  ADD_SYM(length);
  ADD_SYM(stepTriangle);
  ADD_SYM(stepDirection);
  ADD_SYM(nBetweenVec);
  ADD_SYM(stepBetweenVec);
  ADD_SYM(nWithinVec);
  ADD_SYM(stepWithinVec);
  /* cohort iterators */
  ADD_SYM(i);
  ADD_SYM(iTime);
  ADD_SYM(iAge);
  ADD_SYM(iTriangle);
  ADD_SYM(finished);
  ADD_SYM(lastAgeGroupOpen);
  /* mappings */
  ADD_SYM(isOneToOne);
  ADD_SYM(nSharedVec);
  ADD_SYM(stepSharedCurrentVec);
  ADD_SYM(stepSharedCurrentExposureVec);
  ADD_SYM(stepSharedTargetVec);
  ADD_SYM(nTimeCurrent);
  ADD_SYM(stepTimeCurrent);
  ADD_SYM(stepTimeTarget);
  ADD_SYM(nAgeCurrent);
  ADD_SYM(nAgeTarget);
  ADD_SYM(stepAgeCurrent);
  ADD_SYM(stepAgeTarget);
  ADD_SYM(stepSexCurrent);
  ADD_SYM(stepSexTarget);
  ADD_SYM(iSexDominant);
  ADD_SYM(stepTriangleCurrent);
  ADD_SYM(stepTriangleTarget);
  ADD_SYM(nOrigDestVec);
  ADD_SYM(stepOrigCurrentVec);
  ADD_SYM(stepDestCurrentVec);
  ADD_SYM(stepOrigDestTargetVec);
  ADD_SYM(iVec);
  ADD_SYM(lengthVec);
  ADD_SYM(increment);
  ADD_SYM(iMinAge);

  /*new priors */
  ADD_SYM(ATau);
  ADD_SYM(nuTau);
  ADD_SYM(hasAlphaDLM);
  ADD_SYM(hasAlphaICAR);
  ADD_SYM(hasAlphaMix);
  ADD_SYM(hasCovariates);
  ADD_SYM(hasAlphaKnown);
  ADD_SYM(hasMean);
  ADD_SYM(hasSeason);
  ADD_SYM(isKnownUncertain);
  ADD_SYM(isNorm);
  ADD_SYM(isRobust);
  ADD_SYM(isZeroVar);
  ADD_SYM(alphaDLM);
  ADD_SYM(alphaICAR);
  ADD_SYM(alphaMix);
  ADD_SYM(iteratorState);
  ADD_SYM(iteratorStateOld);
  ADD_SYM(K);
  ADD_SYM(L);
  ADD_SYM(s);
  ADD_SYM(UBeta);
  ADD_SYM(nuBeta);
  ADD_SYM(isSaturated);
  ADD_SYM(allStrucZero);
  ADD_SYM(alongAllStrucZero);
  ADD_SYM(strucZeroArray);
  ADD_SYM(mNoTrend);
  ADD_SYM(m0NoTrend);
  ADD_SYM(CNoTrend);
  ADD_SYM(aNoTrend);
  ADD_SYM(RNoTrend);
  ADD_SYM(GWithTrend);
  ADD_SYM(mWithTrend);
  ADD_SYM(m0WithTrend);
  ADD_SYM(CWithTrend);
  ADD_SYM(aWithTrend);
  ADD_SYM(hasLevel);
  ADD_SYM(omegaAlpha);
  ADD_SYM(omegaAlphaMax);
  ADD_SYM(AAlpha);
  ADD_SYM(nuAlpha);
  ADD_SYM(deltaDLM);
  ADD_SYM(nuDelta);
  ADD_SYM(ADelta);
  ADD_SYM(omegaDelta);
  ADD_SYM(omegaDeltaMax);
  ADD_SYM(minPhi);
  ADD_SYM(maxPhi);
  ADD_SYM(shape1Phi);
  ADD_SYM(shape2Phi);
  ADD_SYM(WSqrt);
  ADD_SYM(WSqrtInvG);
  ADD_SYM(exposureAg);
  ADD_SYM(P);
  ADD_SYM(AEtaIntercept);
  ADD_SYM(AEtaCoef);
  ADD_SYM(meanEtaCoef);
  ADD_SYM(UEtaCoef);
  ADD_SYM(nuEtaCoef);
  ADD_SYM(nSeason);
  ADD_SYM(ASeason);
  ADD_SYM(omegaSeason);
  ADD_SYM(omegaSeasonMax);
  ADD_SYM(nuSeason);
  ADD_SYM(mSeason);
  ADD_SYM(m0Season);
  ADD_SYM(CSeason);
  ADD_SYM(aSeason);
  ADD_SYM(RSeason);
  ADD_SYM(JOld);
  ADD_SYM(sumsWeightsMix);
  ADD_SYM(weightMix);
  ADD_SYM(latentWeightMix);
  ADD_SYM(componentWeightMix);
  ADD_SYM(latentComponentWeightMix);
  ADD_SYM(levelComponentWeightMix);
  ADD_SYM(levelComponentWeightOldMix);
  ADD_SYM(meanLevelComponentWeightMix);
  ADD_SYM(indexClassMix);
  ADD_SYM(indexClassMaxMix);
  ADD_SYM(indexClassMaxUsedMix);
  ADD_SYM(foundIndexClassMaxPossibleMix);
  ADD_SYM(indexClassMaxPossibleMix);
  ADD_SYM(indexClassProbMix);
  ADD_SYM(omegaComponentWeightMix);
  ADD_SYM(omegaComponentWeightMaxMix);
  ADD_SYM(omegaLevelComponentWeightMix);
  ADD_SYM(omegaLevelComponentWeightMaxMix);
  ADD_SYM(iteratorsDimsMix);
  ADD_SYM(iAlong);
  ADD_SYM(dimBeta);
  ADD_SYM(dimBetaOld);
  ADD_SYM(phiMix);
  ADD_SYM(mMix);
  ADD_SYM(CMix);
  ADD_SYM(aMix);
  ADD_SYM(RMix);
  ADD_SYM(prodVectorsMix);
  ADD_SYM(posProdVectors1Mix);
  ADD_SYM(posProdVectors2Mix);
  ADD_SYM(nBetaNoAlongMix);
  ADD_SYM(vectorsMix);
  ADD_SYM(omegaVectorsMix);
  ADD_SYM(iteratorProdVectorMix);
  ADD_SYM(yXMix);
  ADD_SYM(XXMix);
  ADD_SYM(alphaMix);
  ADD_SYM(priorMeanLevelComponentWeightMix);
  ADD_SYM(priorSDLevelComponentWeightMix);
  ADD_SYM(AComponentWeightMix);
  ADD_SYM(nuComponentWeightMix);
  ADD_SYM(omegaVectorsMix);
  ADD_SYM(omegaVectorsMaxMix);
  ADD_SYM(AVectorsMix);
  ADD_SYM(nuVectorsMix);
  ADD_SYM(minLevelComponentWeight);
  ADD_SYM(maxLevelComponentWeight);
  ADD_SYM(updateSeriesDLM);
  ADD_SYM(alphaKnown);
  ADD_SYM(AKnownVec);
  ADD_SYM(nuCMP);
  ADD_SYM(sdLogNuCMP);
  ADD_SYM(sdLogNuMaxCMP);
  ADD_SYM(meanMeanLogNuCMP);
  ADD_SYM(sdMeanLogNuCMP);
  ADD_SYM(meanLogNuCMP);
  ADD_SYM(ASDLogNuCMP);
  ADD_SYM(nuSDLogNuCMP);
  ADD_SYM(nu);
  ADD_SYM(ADelta0);
  ADD_SYM(meanDelta0);
  ADD_SYM(ALevelComponentWeightMix);
  ADD_SYM(nuLevelComponentWeightMix);
  /* skeleton */
  ADD_SYM(first);
  ADD_SYM(last);
  /* Box-Cox */
  ADD_SYM(boxCoxParam);
  /* accounts and combined accounts*/
  ADD_SYM(account);
  ADD_SYM(population);
  ADD_SYM(accession);
  ADD_SYM(components);
  ADD_SYM(descriptions);
  ADD_SYM(iteratorPopn);
  ADD_SYM(iteratorAcc);
  ADD_SYM(iteratorExposure);
  ADD_SYM(transformsExpToComp);
  ADD_SYM(transformExpToBirths);
  ADD_SYM(iCell);
  ADD_SYM(iCellOther);
  ADD_SYM(iComp);
  ADD_SYM(iPopnNext);
  ADD_SYM(iPopnNextOther);
  ADD_SYM(iAccNext);
  ADD_SYM(iAccNextOther);
  ADD_SYM(iOrigDest);
  ADD_SYM(iPool);
  ADD_SYM(iIntNet);
  ADD_SYM(iBirths);
  ADD_SYM(iParCh);
  ADD_SYM(diffProp);
  ADD_SYM(isIncrement);
  ADD_SYM(isNet);
  ADD_SYM(usePriorPopn);
  ADD_SYM(systemModels);
  ADD_SYM(modelUsesExposure);
  ADD_SYM(mappingsFromExp);
  ADD_SYM(mappingsToExp);
  ADD_SYM(mappingsToPopn);
  ADD_SYM(mappingsToAcc);
  ADD_SYM(iExpFirst);
  ADD_SYM(iExpFirstOther);
  ADD_SYM(ageTimeStep);
  ADD_SYM(iteratorsComp);
  ADD_SYM(expectedExposure);
  ADD_SYM(iExposure);
  ADD_SYM(iExposureOther);
  ADD_SYM(isLowerTriangle);
  ADD_SYM(generatedNewProposal);
  ADD_SYM(probSmallUpdate);
  ADD_SYM(isSmallUpdate);
  ADD_SYM(probPopn);
  ADD_SYM(cumProbComp);
  ADD_SYM(scaleNoise);
  ADD_SYM(nCellAccount);
  ADD_SYM(alphaLN2);
  ADD_SYM(transformLN2);
  ADD_SYM(constraintLN2);
  ADD_SYM(nCellBeforeLN2);


#undef ADD_SYM

}

