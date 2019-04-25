
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
    }
}

void
drawDataModelsAccount(SEXP combined_R)
{
        
    SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
    SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
    SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
    SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);
    
    SEXP account_R = GET_SLOT(combined_R, account_sym);
    SEXP population_R = GET_SLOT(account_R, population_sym);
    SEXP components_R = GET_SLOT(account_R, components_sym);
    
    int* seriesIndices = INTEGER(seriesIndices_R);
    
    int nObs = LENGTH(dataModels_R);

    for (int i = 0; i < nObs; ++i) {
        
        SEXP model_R = VECTOR_ELT(dataModels_R, i);
        SEXP dataset_R = VECTOR_ELT(datasets_R, i);
        SEXP transform_R = VECTOR_ELT(transforms_R, i);
        
        int series_index_r = seriesIndices[i];
        
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
        drawModelUseExp_Internal(model_R, dataset_R,
                                    seriesCollapsed_R, i_method_model);

        UNPROTECT(nProtect); /* seriesCollapsed_R and possibly also series_Collapsed_tmp_R*/
    }    
}

void
drawDelta0(SEXP prior_R)
{
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym)); /* vector, length (K+1)L */
    
    double A0 = *REAL(GET_SLOT(prior_R, ADelta0_sym)); 
    
    double mean0 = *REAL(GET_SLOT(prior_R, meanDelta0_sym)); 
    
    SEXP iterator_R = GET_SLOT(prior_R, iteratorState_sym);
    resetA(iterator_R);
    int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym));

     for (int l = 0; l < L; ++l) {
    
        if (!alongAllStrucZero[l]) {
            int i0_r = indices[0];
            delta[i0_r - 1] = rnorm(mean0, A0);
        }   
        advanceA(iterator_R);
    }   
}

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

void
drawOmegaAlpha(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaAlphaMax_sym));
        
    double A = *REAL(GET_SLOT(prior_R, AAlpha_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuAlpha_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, omega);    
}


void
drawOmegaComponentWeightMix(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaComponentWeightMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, AComponentWeightMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuComponentWeightMix_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaComponentWeightMix_sym, omega);   
}


void
drawOmegaDelta(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaDeltaMax_sym));
        
    double A = *REAL(GET_SLOT(prior_R, ADelta_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuDelta_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, omega);    
}


void
drawOmegaLevelComponentWeightMix(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaLevelComponentWeightMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, ALevelComponentWeightMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuLevelComponentWeightMix_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaLevelComponentWeightMix_sym, omega);   
}

void
drawOmegaSeason(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaSeasonMax_sym));
    double A = *REAL(GET_SLOT(prior_R, ASeason_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuSeason_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, omega);   
}

void
drawOmegaVectorsMix(SEXP prior_R)
{
    double omega_max = *REAL(GET_SLOT(prior_R, omegaVectorsMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, AVectorsMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuVectorsMix_sym));
    
    double omega = rhalftTrunc1(nu, A, omega_max);
    
    SET_DOUBLESCALE_SLOT(prior_R, omegaVectorsMix_sym, omega);   
}

void
drawPhi(SEXP prior_R)
{
    int phi_known = *LOGICAL(GET_SLOT(prior_R, phiKnown_sym));
    if (!phi_known) {
        double phi_min = *REAL(GET_SLOT(prior_R, minPhi_sym));
        double phi_max = *REAL(GET_SLOT(prior_R, maxPhi_sym));
        double shape1 = *REAL(GET_SLOT(prior_R, shape1Phi_sym));
        double shape2 = *REAL(GET_SLOT(prior_R, shape2Phi_sym));
        
        
        double X = rbeta(shape1, shape2);
        double phi = phi_min + X*(phi_max - phi_min);
        
        SET_DOUBLESCALE_SLOT(prior_R, phi_sym, phi);  
    }
    /* no change if phi known */ 
}

void
drawPhiMix(SEXP prior_R)
{
    int phi_known = *LOGICAL(GET_SLOT(prior_R, phiKnown_sym));
    if (!phi_known) {
        double phi_min = *REAL(GET_SLOT(prior_R, minPhi_sym));
        double phi_max = *REAL(GET_SLOT(prior_R, maxPhi_sym));
        double shape1 = *REAL(GET_SLOT(prior_R, shape1Phi_sym));
        double shape2 = *REAL(GET_SLOT(prior_R, shape2Phi_sym));
        
        
        double X = rbeta(shape1, shape2);
        double phi = phi_min + X*(phi_max - phi_min);
        
        SET_DOUBLESCALE_SLOT(prior_R, phiMix_sym, phi);   
    }
}

void
drawPriors(SEXP object_R)
{
    SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
    int nPriors = LENGTH(priors_R);
    
    for (int b = 0; b < nPriors; ++b) {
        
            SEXP this_prior = VECTOR_ELT(priors_R, b);
            drawPrior(this_prior);
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
