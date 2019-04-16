
//#include "iterators-methods.h"
#include "model-methods.h"
#include "helper-functions.h"
#include "demest.h"

//#include "R_ext/BLAS.h" 
/* for BLAS level 2 documention see www.netlib.org/blas/blas2-paper.ps */ 

//#include "R_ext/Lapack.h" 

/* File "helper-simulate.c" contains C versions of 
 * functions from "helper-simulate.R".   */


void
drawBetas(SEXP object_R)
{
    SEXP betas_R = GET_SLOT(object_R, betas_sym);
    int n_betas = LENGTH(betas_R);
    
    SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
    
    int max_J = 0; /* max J so far */
    double *work = NULL;
    
    for (int i = 0; i < n_betas; ++i) {
        
        #ifdef DEBUGGING
        PrintValue(mkString(""));
        PrintValue(mkString("i"));
        PrintValue(ScalarInteger(i));
        #endif
           
        SEXP beta_R = VECTOR_ELT(betas_R, i);
        SEXP prior_R = VECTOR_ELT(priors_R, i);
        double *beta = REAL(beta_R);
        
        int J = *INTEGER(GET_SLOT(prior_R, J_sym));
        
        /* only malloc more space if we need it */
        if(J > max_J) {
            max_J = J;
            work = (double*)R_alloc(2*J, sizeof(double));
        }
                      
        int *all_struc_zero = INTEGER(GET_SLOT(prior_R, allStrucZero_sym));
        
        double *beta_hat = work;
        double *var = work + J;
        betaHat(beta_hat, prior_R, J);
        
        getV_Internal(var, prior_R, J);
        
        for (int j = 0; j < J; ++j) {
            if (!all_struc_zero[j]) {
                beta[j] = rnorm(beta_hat[j], sqrt(var[j]) );
            }
        }
        #ifdef DEBUGGING
        PrintValue(mkString("J"));
        PrintValue(ScalarReal(J));
        PrintValue(mkString("beta_hat"));
        printDblArray(beta_hat, J);
        PrintValue(mkString("var"));
        printDblArray(var, J);
        PrintValue(mkString("beta"));
        printDblArray(beta, J);
        #endif
    }
}

/*
 * ## READY_TO_TRANSLATE
## HAS_TESTS
drawDataModelsAccount <- function(combined, useC = FALSE) {
    stopifnot(methods::validObject(combined))
    if (useC) {
        .Call(drawDataModelsAccount_R, combined)
    }
    else {
        data.models <- combined@dataModels
        datasets <- combined@datasets
        population <- combined@account@population
        components <- combined@account@components
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        for (i in seq_along(data.models)) {
            model <- data.models[[i]]
            dataset <- datasets[[i]]
            transform <- transforms[[i]]
            series.index <- series.indices[i]
            if (series.index == 0L)
                series <- population
            else
                series <- components[[series.index]]
            series.collapsed <- collapse(series, transform = transform)
            if (methods::is(model, "Poisson") || methods::is(model, "CMP"))
                series.collapsed <- toDouble(series.collapsed)
            model <- drawModelUseExp(model,
                                     y = dataset,
                                     exposure = series.collapsed)
            data.models[[i]] <- model
        }
        combined@dataModels <- data.models
        combined
    }
}
*/

void
drawDataModelsAccount(SEXP combined_R)
{
    /*data.models <- combined@dataModels
        datasets <- combined@datasets
        population <- combined@account@population
        components <- combined@account@components
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms*/
        
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP components_R = GET_SLOT(account_R, components_sym);
    
    int* seriesIndices = INTEGER(seriesIndices_R);
    
    int nObs = LENGTH(dataModels_R);

/*    for (i in seq_along(data.models)) {
            model <- data.models[[i]]
            dataset <- datasets[[i]]
            transform <- transforms[[i]]
            series.index <- series.indices[i]
*/
    for (int i = 0; i < nObs; ++i) {
        
        SEXP model_R = VECTOR_ELT(dataModels_R, i);
        SEXP dataset_R = VECTOR_ELT(datasets_R, i);
        SEXP transform_R = VECTOR_ELT(transforms_R, i);
        
        int series_index_r = seriesIndices[i];
        
/*            if (series.index == 0L)
                series <- population
            else
                series <- components[[series.index]]
*/        
        SEXP series_R = population_R;
        if (series_index_r > 0) {
            series_R = VECTOR_ELT(components_R, series_index_r-1);
        }
/*           series.collapsed <- collapse(series, transform = transform)
            if (methods::is(model, "Poisson") || methods::is(model, "CMP"))
                series.collapsed <- toDouble(series.collapsed)
            model <- drawModelUseExp(model,
                                     y = dataset,
                                     exposure = series.collapsed)
            data.models[[i]] <- model
        }
*/        
        SEXP seriesCollapsed_R;

        int nProtect  = 0;
        int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
        
        const char *class_name = CHAR(STRING_ELT(GET_SLOT((model_R), R_ClassSymbol), 0));
        int found = !((strstr(class_name, "Poisson") == NULL) && (strstr(class_name, "CMP") == NULL));
        if (found) {
            
            SEXP seriesCollapsed_tmp_R;
            /* collapse_R in demographic is okay with series_R being integer
             * but type of contents of seriesCollapsed_R will be integer*/
            PROTECT(seriesCollapsed_tmp_R = dembase_Collapse_R(series_R, transform_R));

            PROTECT(seriesCollapsed_R = coerceVector(seriesCollapsed_tmp_R, REALSXP));
            nProtect  = 2;
        }
        else {
            
            PROTECT(seriesCollapsed_R = dembase_Collapse_R(series_R, transform_R));
            nProtect  = 1;
        }
        
        /* seriesCollapsed_R should now be in appropriate state for model */
        //drawModelUseExp_Internal(model_R, dataset_R,
                                    //seriesCollapsed_R, i_method_model);

        UNPROTECT(nProtect); /* seriesCollapsed_R and possibly also series_Collapsed_tmp_R*/
    }    
}

/*
 * 
## READY_TO_TRANSLATE
## HAS_TESTS
drawDelta0 <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM") && methods::is(prior, "WithTrendMixin"))
    if (useC) {
        .Call(drawDelta0_R, prior)
    }
    else {
        L <- prior@L@.Data
        along.all.struc.zero <- prior@alongAllStrucZero
        delta <- prior@deltaDLM@.Data # numeric vector length (K+1)L
        A0 <- prior@ADelta0@.Data # scalar
        mean0 <- prior@meanDelta0@.Data # scalar
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            if (!along.all.struc.zero[l]) {
                indices <- iterator@indices
                i0 <- indices[1L]
                delta[i0] <- stats::rnorm(n = 1L,
                                          mean = mean0,
                                          sd = A0)
            }
            iterator <- advanceA(iterator)
        }
        prior@deltaDLM@.Data <- delta
        prior
    }
}
*/
void
drawDelta0(SEXP prior_R)
{
    /*L <- prior@L@.Data
        along.all.struc.zero <- prior@alongAllStrucZero
        delta <- prior@deltaDLM@.Data # numeric vector length (K+1)L
        A0 <- prior@ADelta0@.Data # scalar
        mean0 <- prior@meanDelta0@.Data # scalar
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        */
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym)); /* vector, length (K+1)L */
    
    double A0 = *REAL(GET_SLOT(prior_R, ADelta0_sym)); 
    
    double mean0 = *REAL(GET_SLOT(prior_R, meanDelta0_sym)); 
    
    SEXP iterator_R = GET_SLOT(prior_R, iteratorState_sym);
    resetA(iterator_R);
    int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym));

    /*for (l in seq_len(L)) {
            if (!along.all.struc.zero[l]) {
                indices <- iterator@indices
                i0 <- indices[1L]
                delta[i0] <- stats::rnorm(n = 1L,
                                          mean = mean0,
                                          sd = A0)
            }
            iterator <- advanceA(iterator)
        }
     */
     for (int l = 0; l < L; ++l) {
    
        if (!alongAllStrucZero[l]) {
            int i0_r = indices[0];
            delta[i0_r - 1] = rnorm(mean0, A0);
        }   
        advanceA(iterator_R);
    }   
}

/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawEta <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawEta_R, prior)
    }
    else {
        eta <- prior@eta@.Data
        P <- prior@P@.Data
        U.eta.coef <- prior@UEtaCoef@.Data
        mean.eta.coef <- prior@meanEtaCoef@.Data
        eta[1L] <- 0
        for (p in seq_len(P - 1L)) {
            mean <- mean.eta.coef[p]
            sd <- sqrt(U.eta.coef[p])
            eta[p + 1L] <- stats::rnorm(n = 1L,
                                        mean = mean,
                                        sd = sd)
        }
        prior@eta@.Data <- eta
        prior
    }
}
*/
void
drawEta(SEXP prior_R)
{
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    double *eta = REAL(GET_SLOT(prior_R, eta_sym)); /* length P */
    double *mean_eta_coef = REAL(GET_SLOT(prior_R, meanEtaCoef_sym)); /* length P-1 */
    double *U_eta_coef = REAL(GET_SLOT(prior_R, UEtaCoef_sym)); /* length P-1 */
    
    eta[0] = 0;
    
    for (int p = 0; p < P-1; ++p) {
        double mean = mean_eta_coef[p];
        double sd = sqrt(U_eta_coef[p]);
        
        eta[p+1] = rnorm(mean, sd);
    }
}
/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaAlpha <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaAlpha_R, prior)
    }
    else {
        A <- prior@AAlpha@.Data
        nu <- prior@nuAlpha@.Data
        max <- prior@omegaAlphaMax@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaAlpha@.Data <- omega
        prior
    }
}
*/

void
drawOmegaAlpha(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaAlphaMax_sym));
        
    double A = *REAL(GET_SLOT(prior_R, AAlpha_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuAlpha_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, omega);    
}


/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaComponentWeightMix <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaComponentWeightMix_R, prior)
    }
    else {
        A <- prior@AComponentWeightMix@.Data
        nu <- prior@nuComponentWeightMix@.Data
        max <- prior@omegaComponentWeightMaxMix@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaComponentWeightMix@.Data <- omega
        prior
    }
}
*/

void
drawOmegaComponentWeightMix(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaComponentWeightMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, AComponentWeightMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuComponentWeightMix_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaComponentWeightMix_sym, omega);   
}

/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaDelta <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaDelta_R, prior)
    }
    else {
        A <- prior@ADelta@.Data
        nu <- prior@nuDelta@.Data
        max <- prior@omegaDeltaMax@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaDelta@.Data <- omega
        prior
    }
}
*/

void
drawOmegaDelta(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaDeltaMax_sym));
        
    double A = *REAL(GET_SLOT(prior_R, ADelta_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuDelta_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, omega);    
}

/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaLevelComponentWeightMix <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaLevelComponentWeightMix_R, prior)
    }
    else {
        A <- prior@ALevelComponentWeightMix@.Data
        nu <- prior@nuLevelComponentWeightMix@.Data
        max <- prior@omegaLevelComponentWeightMaxMix@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaLevelComponentWeightMix@.Data <- omega
        prior
    }
}
*/
void
drawOmegaLevelComponentWeightMix(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaLevelComponentWeightMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, ALevelComponentWeightMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuLevelComponentWeightMix_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaLevelComponentWeightMix_sym, omega);   
}


/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaSeason <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaSeason_R, prior)
    }
    else {
        A <- prior@ASeason@.Data
        nu <- prior@nuSeason@.Data
        max <- prior@omegaSeasonMax@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaSeason@.Data <- omega
        prior
    }
}
*/
void
drawOmegaSeason(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaSeasonMax_sym));
    double A = *REAL(GET_SLOT(prior_R, ASeason_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuSeason_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, omega);   
}

/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaVectorsMix <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaVectorsMix_R, prior)
    }
    else {
        A <- prior@AVectorsMix@.Data
        nu <- prior@nuVectorsMix@.Data
        max <- prior@omegaVectorsMaxMix@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaVectorsMix@.Data <- omega
        prior
    }
}
*/
void
drawOmegaVectorsMix(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaVectorsMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, AVectorsMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuVectorsMix_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaVectorsMix_sym, omega);   
}

/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawPhi <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawPhi_R, prior)
    }
    else {
        phi.known <- prior@phiKnown@.Data
        if (phi.known)
            prior
        else {
            phi.min <- prior@minPhi@.Data
            phi.max <- prior@maxPhi@.Data
            shape1 <- prior@shape1Phi@.Data
            shape2 <- prior@shape2Phi@.Data
            X <- stats::rbeta(n = 1L,
                              shape1 = shape1,
                              shape2 = shape2)
            phi <- phi.min + X * (phi.max - phi.min)
            prior@phi <- phi
            prior
        }
    }
}
*/
void
drawPhi(SEXP prior_R)
{
    double phi_min = *REAL(GET_SLOT(prior_R, minPhi_sym));
    double phi_max = *REAL(GET_SLOT(prior_R, maxPhi_sym));
    double shape1 = *REAL(GET_SLOT(prior_R, shape1Phi_sym));
    double shape2 = *REAL(GET_SLOT(prior_R, shape2Phi_sym));
    
    
    double X = rbeta(shape1, shape2);
    double phi = phi_min + X*(phi_max - phi_min);
    
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, phi);   
}

/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawPhiMix <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawPhiMix_R, prior)
    }
    else {
        phi.known <- prior@phiKnown@.Data
        if (phi.known)
            prior
        else {
            phi.min <- prior@minPhi@.Data
            phi.max <- prior@maxPhi@.Data
            shape1 <- prior@shape1Phi@.Data
            shape2 <- prior@shape2Phi@.Data
            X <- stats::rbeta(n = 1L,
                              shape1 = shape1,
                              shape2 = shape2)
            phi <- phi.min + X * (phi.max - phi.min)
            prior@phiMix <- phi
            prior
        }
    }
}
*/
void
drawPhiMix(SEXP prior_R)
{
    double phi_min = *REAL(GET_SLOT(prior_R, minPhi_sym));
    double phi_max = *REAL(GET_SLOT(prior_R, maxPhi_sym));
    double shape1 = *REAL(GET_SLOT(prior_R, shape1Phi_sym));
    double shape2 = *REAL(GET_SLOT(prior_R, shape2Phi_sym));
    
    
    double X = rbeta(shape1, shape2);
    double phi = phi_min + X*(phi_max - phi_min);
    
    SET_DOUBLESCALE_SLOT(prior_R, phiMix_sym, phi);   
}
/*
## READY_TO_TRANSLATE
## HAS_TESTS
drawPriors <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(drawPriors_R, object)
    }
    else {
        priors <- object@priorsBetas
        for (b in seq_along(priors))
            priors[[b]] <- drawPrior(priors[[b]])
        object@priorsBetas <- priors
        object
    }
}
*/
void
drawPriors(SEXP object_R)
{
    SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
    int nPriors = LENGTH(priors_R);
    
    for (int b = 0; b < nPriors; ++b) {
        
            /*priors[[b]] <- drawPrior(priors[[b]])*/
        
    }
}

void
drawSigma_Varying(SEXP object_R)
{
    double sigma_max = *REAL(GET_SLOT(object_R, sigmaMax_sym));
    
    double A = *REAL(GET_SLOT(object_R, ASigma_sym));
    double nu = *REAL(GET_SLOT(object_R, nuSigma_sym));
    
    double val = rhalftTrunc1(nu, A, sigma_max);
    
    SET_DOUBLESCALE_SLOT(object_R, sigma_sym, val)
}

void
drawTau(SEXP prior_R)
{
    double tau_max = *REAL(GET_SLOT(prior_R, tauMax_sym));
    
    double A = *REAL(GET_SLOT(prior_R, ATau_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuTau_sym));
    
    double tau = rhalftTrunc1(nu, A, tau_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, tau)
}

void
drawUEtaCoef(SEXP prior_R)
{
    double *U = REAL(GET_SLOT(prior_R, UEtaCoef_sym)); /* length P-1 */
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    double *A = REAL(GET_SLOT(prior_R, AEtaCoef_sym));
    double *nu = REAL(GET_SLOT(prior_R, nuEtaCoef_sym));
    
    for (int p = 0; p < P-1; ++p) {
        double df_p = nu[p];
        double ASq_p = A[p] * A[p];
        U[p] = rinvchisq1(df_p, ASq_p);
    }
}

void
drawVarsigma(SEXP object_R)
{
    double varsigma_max = *REAL(GET_SLOT(object_R, varsigmaMax_sym));
    double A = *REAL(GET_SLOT(object_R, AVarsigma_sym));
    double nu = *REAL(GET_SLOT(object_R, nuVarsigma_sym));
    
    double val = rhalftTrunc1(nu, A, varsigma_max);
    
    SET_DOUBLESCALE_SLOT(object_R, varsigma_sym, val)
}
