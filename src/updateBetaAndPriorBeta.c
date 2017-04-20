
#include "update-nongeneric.h"
#include "demest.h"

/* fill in beta, update prior_R */
void
updateBetaAndPriorBeta(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    int i_method_prior = *(INTEGER(GET_SLOT(prior_R, iMethodPrior_sym)));
    
    switch(i_method_prior)
    {
        case 0:
            updateBetaAndPriorBeta_ExchFixed(beta, J, prior_R, vbar, 
                                            n_vec, sigma); 
            break;
        
        case 1:
            updateBetaAndPriorBeta_ExchNormZero(beta, J, prior_R, vbar, 
                                            n_vec, sigma); 
            break;    
        case 2:    
            updateBetaAndPriorBeta_ExchNormCov(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 3:
            updateBetaAndPriorBeta_ExchRobustZero(beta, J, prior_R, vbar, 
                                            n_vec, sigma); 
            break;
        case 4:
            updateBetaAndPriorBeta_ExchRobustCov(beta, J, prior_R, vbar, 
                                            n_vec, sigma); 
            break;
        
        case 5:    
            updateBetaAndPriorBeta_DLMNoTrendNormZeroNoSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 6:    
            updateBetaAndPriorBeta_DLMNoTrendNormZeroWithSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        
        case 7:
            updateBetaAndPriorBeta_DLMNoTrendNormCovNoSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 8:    
            updateBetaAndPriorBeta_DLMNoTrendNormCovWithSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 9:    
            updateBetaAndPriorBeta_DLMNoTrendRobustZeroNoSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 10:
            updateBetaAndPriorBeta_DLMNoTrendRobustZeroWithSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 11:
            updateBetaAndPriorBeta_DLMNoTrendRobustCovNoSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 12:
            updateBetaAndPriorBeta_DLMNoTrendRobustCovWithSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 13:    
            updateBetaAndPriorBeta_DLMWithTrendNormZeroNoSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 14:
            updateBetaAndPriorBeta_DLMWithTrendNormZeroWithSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 15:
            updateBetaAndPriorBeta_DLMWithTrendNormCovNoSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 16:
            updateBetaAndPriorBeta_DLMWithTrendNormCovWithSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 17:    
            updateBetaAndPriorBeta_DLMWithTrendRobustZeroNoSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 18:    
            updateBetaAndPriorBeta_DLMWithTrendRobustZeroWithSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 19:    
            updateBetaAndPriorBeta_DLMWithTrendRobustCovNoSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 20:    
            updateBetaAndPriorBeta_DLMWithTrendRobustCovWithSeason(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 29:    
            updateBetaAndPriorBeta_KnownCertain(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 30:    
            updateBetaAndPriorBeta_KnownUncertain(beta, J, 
                                            prior_R, vbar, n_vec, sigma); 
            break;
        case 31:
            updateBetaAndPriorBeta_MixNormZero(beta, J, 
                                            prior_R, vbar, n_vec, sigma);
            break;
        case 40:
            updateBetaAndPriorBeta_Zero(beta, J, 
                                            prior_R, vbar, n_vec, sigma);
            break;
        default:
            error("unknown i_method_prior: %d", i_method_prior);
            break;
    }
}

void
updateBetaAndPriorBeta_ExchFixed(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    double tau = *REAL(GET_SLOT(prior_R, tau_sym));
    
    double sigmaSq = sigma*sigma;
    double precPrior = 1/(tau*tau);
    
    
    for (int i = 0; i < J; ++i) {
        
        double thisPrecData = n_vec[i]/sigmaSq;
        double thisVar = 1/(thisPrecData + precPrior);
        double thisMean = thisPrecData * vbar[i] * thisVar;
        double thisSd = sqrt(thisVar);
    
        beta[i] = rnorm(thisMean, thisSd);
    }
}

void
updateBetaAndPriorBeta_ExchNormZero(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    updateTauNorm(prior_R, beta, J);
}


void
updateBetaAndPriorBeta_ExchRobustZero(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma); 
    
    updateUBeta(prior_R, beta, J);
    
    updateTauRobust(prior_R, J);
}

void
updateBetaAndPriorBeta_ExchNormCov(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    updateEta(prior_R, beta, J);
    
    updateUEtaCoef(prior_R);
    
    updateTauNorm(prior_R, beta, J);
}

void
updateBetaAndPriorBeta_ExchRobustCov(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    updateUBeta(prior_R, beta, J);
    
    updateTauRobust(prior_R, J);
    
    updateEta(prior_R, beta, J);
    
    updateUEtaCoef(prior_R);
}

void
updateBetaAndPriorBeta_DLMNoTrendNormZeroNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    updateAlphaDLMNoTrend(prior_R, beta, J);
    
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    
    updateTauNorm(prior_R, beta, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
}

void
updateBetaAndPriorBeta_DLMWithTrendNormZeroNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    updateAlphaDeltaDLMWithTrend(prior_R, beta, J);
    
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    
    updateTauNorm(prior_R, beta, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    updateOmegaDelta(prior_R);
    updateGWithTrend(prior_R);
    updateWSqrt(prior_R);
    updateWSqrtInvG(prior_R);
}


void
updateBetaAndPriorBeta_DLMNoTrendNormZeroWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    betaHatSeason(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    
    updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
    
    /* reuse beta_tilde */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    
    updateSeason(prior_R, beta_tilde, J);
    
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    
    updateTauNorm(prior_R, beta, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    updateOmegaSeason(prior_R);
}


void
updateBetaAndPriorBeta_DLMWithTrendNormZeroWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    betaHatSeason(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
    
    /* reuse beta_tilde */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    
    updateSeason(prior_R, beta_tilde, J);
    
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    
    updateTauNorm(prior_R, beta, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    updateOmegaDelta(prior_R);
    updateGWithTrend(prior_R);
    updateWSqrt(prior_R);
    updateWSqrtInvG(prior_R);
    updateOmegaSeason(prior_R);
}


void
updateBetaAndPriorBeta_DLMNoTrendNormCovNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    betaHatCovariates(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
    
    /* reuse beta_tilde */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateEta(prior_R, beta_tilde, J);
    
    updateUEtaCoef(prior_R);
    
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    
    updateTauNorm(prior_R, beta, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
}

void
updateBetaAndPriorBeta_DLMWithTrendNormCovNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    betaHatCovariates(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
    
    /* reuse beta_tilde */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateEta(prior_R, beta_tilde, J);
    
    updateUEtaCoef(prior_R);
    
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    
    updateTauNorm(prior_R, beta, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    
    updateOmegaDelta(prior_R);
    
    updateGWithTrend(prior_R);
    
    updateWSqrt(prior_R);
    
    updateWSqrtInvG(prior_R);
}


void
updateBetaAndPriorBeta_DLMNoTrendNormCovWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(2*J, sizeof(double));
    double *beta_tilde1 = beta_tilde;
    double *beta_tilde2 = beta_tilde + J;
    
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateAlphaDLMNoTrend(prior_R, beta_tilde1, J);
    
    /* reuse beta tildes */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateSeason(prior_R, beta_tilde1, J);
    
    /* reuse beta tildes */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateEta(prior_R, beta_tilde1, J);
    updateUEtaCoef(prior_R);
    
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    
    updateTauNorm(prior_R, beta, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    updateOmegaSeason(prior_R);
}

void
updateBetaAndPriorBeta_DLMWithTrendNormCovWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(2*J, sizeof(double));
    double *beta_tilde1 = beta_tilde;
    double *beta_tilde2 = beta_tilde + J;
    
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde1, J);
    
    /* reuse beta tildes */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateSeason(prior_R, beta_tilde1, J);
    
    /* reuse beta tildes */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateEta(prior_R, beta_tilde1, J);
    updateUEtaCoef(prior_R);
    
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    
    updateTauNorm(prior_R, beta, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    updateOmegaDelta(prior_R);
    updateGWithTrend(prior_R);
    updateWSqrt(prior_R);
    updateWSqrtInvG(prior_R);
    updateOmegaSeason(prior_R);
}

void
updateBetaAndPriorBeta_DLMNoTrendRobustZeroNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    updateAlphaDLMNoTrend(prior_R, beta, J);
    
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    
    updateUBeta(prior_R, beta, J);
    
    updateTauRobust(prior_R, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
}

void
updateBetaAndPriorBeta_DLMWithTrendRobustZeroNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    updateAlphaDeltaDLMWithTrend(prior_R, beta, J);
    
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    
    updateUBeta(prior_R, beta, J);
    updateTauRobust(prior_R, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    updateOmegaDelta(prior_R);
    updateGWithTrend(prior_R);
    updateWSqrt(prior_R);
    updateWSqrtInvG(prior_R);
}

void
updateBetaAndPriorBeta_DLMNoTrendRobustZeroWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    betaHatSeason(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
    
    /* reuse beta_tilde */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateSeason(prior_R, beta_tilde, J);
    
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    
    updateUBeta(prior_R, beta, J);
    
    updateTauRobust(prior_R, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    
    updateOmegaSeason(prior_R);
}

void
updateBetaAndPriorBeta_DLMWithTrendRobustZeroWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    betaHatSeason(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
    
    /* reuse beta_tilde */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateSeason(prior_R, beta_tilde, J);
    
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    
    updateUBeta(prior_R, beta, J);
    
    updateTauRobust(prior_R, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    updateOmegaDelta(prior_R);
    updateGWithTrend(prior_R);
    updateWSqrt(prior_R);
    updateWSqrtInvG(prior_R);
    
    updateOmegaSeason(prior_R);
}

void
updateBetaAndPriorBeta_DLMNoTrendRobustCovNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    betaHatCovariates(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
    
    /* reuse beta_tilde */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateEta(prior_R, beta_tilde, J);
    updateUEtaCoef(prior_R);
    
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    
    updateUBeta(prior_R, beta, J);
    
    updateTauRobust(prior_R, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
}

void
updateBetaAndPriorBeta_DLMWithTrendRobustCovNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    betaHatCovariates(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
    
    /* reuse beta_tilde */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        double bh = beta_tilde[i];
        beta_tilde[i] = beta[i] - bh;
    }
    
    updateEta(prior_R, beta_tilde, J);
    updateUEtaCoef(prior_R);
    
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    
    updateUBeta(prior_R, beta, J);
    
    updateTauRobust(prior_R, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    
    updateOmegaDelta(prior_R);
    updateGWithTrend(prior_R);
    updateWSqrt(prior_R);
    updateWSqrtInvG(prior_R);
}

void
updateBetaAndPriorBeta_DLMNoTrendRobustCovWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(2*J, sizeof(double));
    double *beta_tilde1 = beta_tilde;
    double *beta_tilde2 = beta_tilde + J;
    
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateAlphaDLMNoTrend(prior_R, beta_tilde1, J);

    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateSeason(prior_R, beta_tilde1, J);
    
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateEta(prior_R, beta_tilde1, J);
    updateUEtaCoef(prior_R);
    
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    
    updateUBeta(prior_R, beta, J);
    
    updateTauRobust(prior_R, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);
    
    updateOmegaSeason(prior_R);
}


void
updateBetaAndPriorBeta_DLMWithTrendRobustCovWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    
    double *beta_tilde = (double *)R_alloc(2*J, sizeof(double));
    double *beta_tilde1 = beta_tilde;
    double *beta_tilde2 = beta_tilde + J;
    
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde1, J);

    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    
    updateSeason(prior_R, beta_tilde1, J);
    
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }

    updateEta(prior_R, beta_tilde1, J);
    updateUEtaCoef(prior_R);
    
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    
    updateUBeta(prior_R, beta, J);

    updateTauRobust(prior_R, J);
    
    updateOmegaAlpha(prior_R, isWithTrend);

    updateOmegaDelta(prior_R);
    updateGWithTrend(prior_R);
    updateWSqrt(prior_R);
    updateWSqrtInvG(prior_R);
    
    updateOmegaSeason(prior_R);
}

void
updateBetaAndPriorBeta_KnownCertain(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
	double *alpha = REAL(GET_SLOT(prior_R, alphaKnown_sym));  
    memcpy(beta, alpha, J*sizeof(double));
        
}

void
updateBetaAndPriorBeta_KnownUncertain(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
	double *alpha = REAL(GET_SLOT(prior_R, alphaKnown_sym));  
    double *AKnownVec = REAL(GET_SLOT(prior_R, AKnownVec_sym));
    
    double sigmaSq = sigma * sigma;
    
    for (int i = 0; i < J; ++i) {
		
		double thisPrecData = n_vec[i]/sigmaSq;
		double thisA = AKnownVec[i];
		double thisPrecPrior = 1/(thisA * thisA);
		double thisVar = 1/(thisPrecData + thisPrecPrior);
		double thisMean = (thisPrecData * vbar[i] + 
									thisPrecPrior * alpha[i]) * thisVar;
		double thisSD = sqrt(thisVar);
		beta[i] = rnorm(thisMean, thisSD);
	}
}

void
updateBetaAndPriorBeta_MixNormZero(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
    updateBeta(beta, J, prior_R, vbar, n_vec, sigma);
    updateTauNorm(prior_R, beta, J);
    updateVectorsMixAndProdVectorsMix(prior_R, beta, J);
    updateOmegaVectorsMix(prior_R);
    updateLatentComponentWeightMix(prior_R);
    updateComponentWeightMix(prior_R);
    updateWeightMix(prior_R);
    updateLatentWeightMix(prior_R);
    updateOmegaComponentWeightMix(prior_R);
    updateOmegaLevelComponentWeightMix(prior_R);
    updateIndexClassMaxPossibleMix(prior_R);
    updateIndexClassMix(prior_R, beta, J);
    updateIndexClassMaxUsedMix(prior_R);
    updateLevelComponentWeightMix(prior_R);
    updateMeanLevelComponentWeightMix(prior_R);
    updatePhiMix(prior_R);
    updateAlphaMix(prior_R);
}

void
updateBetaAndPriorBeta_Zero(double *beta, int J, SEXP prior_R, 
                        double *vbar, int *n_vec, double sigma)
{
	memset(beta, 0, J * sizeof(double));
}
