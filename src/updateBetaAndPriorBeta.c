
#include "update-nongeneric.h"
#include "demest.h"

/* fill in beta, update prior_R */
void
updateBetaAndPriorBeta(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
    int i_method_prior = *(INTEGER(GET_SLOT(prior_R, iMethodPrior_sym)));
    
	switch(i_method_prior)
    {
        case 0:
            updateBetaAndPriorBeta_ExchFixed(beta, J, prior_R, vbar, 
                                            n, sigma); 
            break;
        
        case 1:
            updateBetaAndPriorBeta_ExchNormZero(beta, J, prior_R, vbar, 
                                            n, sigma); 
            break;    
        case 2:    
			updateBetaAndPriorBeta_ExchNormCov(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 3:
            updateBetaAndPriorBeta_ExchRobustZero(beta, J, prior_R, vbar, 
                                            n, sigma); 
            break;
        case 4:
            updateBetaAndPriorBeta_ExchRobustCov(beta, J, prior_R, vbar, 
                                            n, sigma); 
            break;
        
        case 5:    
            updateBetaAndPriorBeta_DLMNoTrendNormZeroNoSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 6:    
            updateBetaAndPriorBeta_DLMNoTrendNormZeroWithSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        
        case 7:
            updateBetaAndPriorBeta_DLMNoTrendNormCovNoSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 8:    
            updateBetaAndPriorBeta_DLMNoTrendNormCovWithSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 9:    
            updateBetaAndPriorBeta_DLMNoTrendRobustZeroNoSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
		case 10:
            updateBetaAndPriorBeta_DLMNoTrendRobustZeroWithSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 11:
            updateBetaAndPriorBeta_DLMNoTrendRobustCovNoSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 12:
            updateBetaAndPriorBeta_DLMNoTrendRobustCovWithSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 13:    
            updateBetaAndPriorBeta_DLMWithTrendNormZeroNoSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
		case 14:
            updateBetaAndPriorBeta_DLMWithTrendNormZeroWithSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 15:
            updateBetaAndPriorBeta_DLMWithTrendNormCovNoSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 16:
            updateBetaAndPriorBeta_DLMWithTrendNormCovWithSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
        case 17:    
            updateBetaAndPriorBeta_DLMWithTrendRobustZeroNoSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
		case 18:    
            updateBetaAndPriorBeta_DLMWithTrendRobustZeroWithSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
		case 19:    
            updateBetaAndPriorBeta_DLMWithTrendRobustCovNoSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
		case 20:    
            updateBetaAndPriorBeta_DLMWithTrendRobustCovWithSeason(beta, J, 
											prior_R, vbar, n, sigma); 
            break;
		
 		default:
            error("unknown i_method_prior: %d", i_method_prior);
            break;
    }
}


void
updateBetaAndPriorBeta_ExchFixed(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
    double tau = *REAL(GET_SLOT(prior_R, tau_sym));
    
    double precData = n/(sigma*sigma);
    double precPrior = 1/(tau*tau);
    
    double var = 1/(precData + precPrior);
    double sd = sqrt(var);
    
    for (int i = 0; i < J; ++i) {
        
        double thisMean = precData * vbar[i] * var;
        
        beta[i] = rnorm(thisMean, sd);
    }
}

/* void */
/* updateBetaAndPriorBeta_ExchNormZero(double *beta, int J, SEXP prior_R,  */
/*                         double *vbar, int n, double sigma) */
/* { */
/* 	updateBetaScaled(beta, J, prior_R, vbar, n, sigma); */
	
/* 	updateZetaAndTau(prior_R, J, beta, vbar, n, sigma); */
	
/* 	betaExchZero(beta, J, prior_R); */
/* } */


void
updateBetaAndPriorBeta_ExchNormZero(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
	updateTauNorm(prior_R, beta, J);
}


void
updateBetaAndPriorBeta_ExchRobustZero(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma); /* changed by John 27 May 2016 */
	
	updateUBeta(prior_R, beta, J);
	
	updateTauRobust(prior_R, J);
	
	/* updateBetaScaled(beta, J, prior_R, vbar, n, sigma); */
	
	/* updateZeta(prior_R, J, beta, vbar, n, sigma); */
	
	/* updateUBetaScaled(prior_R, beta, J); */
	
	/* updateTauScaledRobust(prior_R); */
	
	/* updateUBetaExchRobustZero(prior_R); */
	
	/* betaExchZero(beta, J, prior_R); */
	
	/* updateTauExchZero(prior_R); */
	
}

void
updateBetaAndPriorBeta_ExchNormCov(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
	updateEta(prior_R, beta, J);
	
	updateUEtaCoef(prior_R);
	
	updateTauNorm(prior_R, beta, J);
}

void
updateBetaAndPriorBeta_ExchRobustCov(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
	updateUBeta(prior_R, beta, J);
	
	updateTauRobust(prior_R, J);
	
	updateEta(prior_R, beta, J);
	
	updateUEtaCoef(prior_R);
}

void
updateBetaAndPriorBeta_DLMNoTrendNormZeroNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
	updateAlphaDLMNoTrend(prior_R, beta, J);
	
	int isWithTrend = 0;
	updatePhi(prior_R, isWithTrend);
	
	updateTauNorm(prior_R, beta, J);
	
	updateOmegaAlpha(prior_R, isWithTrend);
}

void
updateBetaAndPriorBeta_DLMWithTrendNormZeroNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
    
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
    
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
    
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
    
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
	updateAlphaDLMNoTrend(prior_R, beta, J);
	
	int isWithTrend = 0;
	updatePhi(prior_R, isWithTrend);
	
	updateUBeta(prior_R, beta, J);
	
	updateTauRobust(prior_R, J);
	
	updateOmegaAlpha(prior_R, isWithTrend);
}

void
updateBetaAndPriorBeta_DLMWithTrendRobustZeroNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
    
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
    
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
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
                        double *vbar, int n, double sigma)
{
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
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


/*
## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendRobustCovWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendRobustCovWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior) - betaHatCovariates(prior)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatSeason(prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior <- updateOmegaSeason(prior = prior )
                  list(beta, prior)
              }
          })

*/
void
updateBetaAndPriorBeta_DLMWithTrendRobustCovWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma)
{
/*                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior) - betaHatCovariates(prior)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatSeason(prior)
*/
	updateBeta(beta, J, prior_R, vbar, n, sigma);
	
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

/*                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
*/

    updateEta(prior_R, beta_tilde1, J);
    updateUEtaCoef(prior_R);
    
	int isWithTrend = 1;
	updatePhi(prior_R, isWithTrend);
	
	updateUBeta(prior_R, beta, J);

/*                 prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior <- updateOmegaSeason(prior = prior )
*/
	
	updateTauRobust(prior_R, J);
	
	updateOmegaAlpha(prior_R, isWithTrend);

    updateOmegaDelta(prior_R);
	updateGWithTrend(prior_R);
	updateWSqrt(prior_R);
	updateWSqrtInvG(prior_R);
    
    updateOmegaSeason(prior_R);
}

