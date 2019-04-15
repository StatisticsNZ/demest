
#include "update-nongeneric.h"
#include "demest.h"

/* warning - these functions all have different parameters,
   in a different order, from the R equivalents */
void
updatePriorBeta(double *beta, int J, SEXP prior_R, 
		double *thetaTransformed, double sigma)
{
    int i_method_prior = *(INTEGER(GET_SLOT(prior_R, iMethodPrior_sym)));
    
    switch(i_method_prior)
    {
        case 0:
            updatePriorBeta_ExchFixed(beta, J, prior_R, thetaTransformed, sigma); 
            break;
        
        case 1:
            updatePriorBeta_ExchNormZero(beta, J, prior_R, thetaTransformed, sigma); 
            break;    
        case 2:    
            updatePriorBeta_ExchNormCov(beta, J, prior_R, thetaTransformed,  sigma); 
            break;
        case 3:
            updatePriorBeta_ExchRobustZero(beta, J, prior_R, thetaTransformed, sigma); 
            break;
        case 4:
            updatePriorBeta_ExchRobustCov(beta, J, prior_R, thetaTransformed, sigma); 
            break;
        
        case 5:    
            updatePriorBeta_DLMNoTrendNormZeroNoSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 6:    
            updatePriorBeta_DLMNoTrendNormZeroWithSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        
        case 7:
            updatePriorBeta_DLMNoTrendNormCovNoSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 8:    
            updatePriorBeta_DLMNoTrendNormCovWithSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 9:    
            updatePriorBeta_DLMNoTrendRobustZeroNoSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 10:
            updatePriorBeta_DLMNoTrendRobustZeroWithSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 11:
            updatePriorBeta_DLMNoTrendRobustCovNoSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 12:
            updatePriorBeta_DLMNoTrendRobustCovWithSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 13:    
            updatePriorBeta_DLMWithTrendNormZeroNoSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 14:
            updatePriorBeta_DLMWithTrendNormZeroWithSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 15:
            updatePriorBeta_DLMWithTrendNormCovNoSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 16:
            updatePriorBeta_DLMWithTrendNormCovWithSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 17:    
            updatePriorBeta_DLMWithTrendRobustZeroNoSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 18:    
            updatePriorBeta_DLMWithTrendRobustZeroWithSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 19:    
            updatePriorBeta_DLMWithTrendRobustCovNoSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 20:    
            updatePriorBeta_DLMWithTrendRobustCovWithSeason(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 29:    
            updatePriorBeta_KnownCertain(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 30:    
            updatePriorBeta_KnownUncertain(beta, J, 
                                            prior_R, thetaTransformed,  sigma); 
            break;
        case 31:
            updatePriorBeta_MixNormZero(beta, J, 
                                            prior_R, thetaTransformed,  sigma);
            break;
        case 40:
            updatePriorBeta_Zero(beta, J, 
                                            prior_R, thetaTransformed,  sigma);
            break;
        default:
            error("unknown i_method_prior: %d", i_method_prior);
            break;
    }
}

void
updatePriorBeta_ExchFixed(double *beta, int J, SEXP prior_R, 
			  double *thetaTransformed, double sigma) {
  /* do nothing */
}

void
updatePriorBeta_ExchNormZero(double *beta, int J, SEXP prior_R, 
			     double *thetaTransformed, double sigma) {
  int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
  if (isSaturated) {
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
  }
  else {
    updateTauNorm(prior_R, beta, J);
  }
}

void
updatePriorBeta_ExchRobustZero(double *beta, int J, SEXP prior_R, 
			       double *thetaTransformed, double sigma) {
    updateUBeta(prior_R, beta, J);
    updateTauRobust(prior_R, J);
}

void
updatePriorBeta_ExchNormCov(double *beta, int J, SEXP prior_R, 
			    double *thetaTransformed, double sigma) {
  int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
  if (isSaturated) {
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
    updateEta(prior_R, thetaTransformed, J);
  }
  else {
    updateEta(prior_R, beta, J);
    updateTauNorm(prior_R, beta, J);
  }
  updateUEtaCoef(prior_R);
}

void
updatePriorBeta_ExchRobustCov(double *beta, int J, SEXP prior_R, 
			      double *thetaTransformed, double sigma) {
    updateUBeta(prior_R, beta, J);
    updateTauRobust(prior_R, J);
    updateEta(prior_R, beta, J);
    updateUEtaCoef(prior_R);
}

void
updatePriorBeta_DLMNoTrendNormZeroNoSeason(double *beta, int J, SEXP prior_R, 
					   double *thetaTransformed, double sigma) {
    int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
    if (isSaturated) {
	updateAlphaDLMNoTrend(prior_R, thetaTransformed, J);
	SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
    }
    else {
	updateAlphaDLMNoTrend(prior_R, beta, J);
	updateTauNorm(prior_R, beta, J);
    }
    int isWithTrend = 0;
    updatePhi(prior_R, isWithTrend);
    updateOmegaAlpha(prior_R, isWithTrend);
}

void
updatePriorBeta_DLMWithTrendNormZeroNoSeason(double *beta, int J, SEXP prior_R, 
					     double *thetaTransformed, double sigma) {
    int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
    if (isSaturated) {
	updateAlphaDeltaDLMWithTrend(prior_R, thetaTransformed, J);
	SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
    }
    else {
	updateAlphaDeltaDLMWithTrend(prior_R, beta, J);
	updateTauNorm(prior_R, beta, J);
    }
    int isWithTrend = 1;
    updatePhi(prior_R, isWithTrend);
    updateOmegaAlpha(prior_R, isWithTrend);
    updateOmegaDelta(prior_R);
    updateGWithTrend(prior_R);
    updateWSqrt(prior_R);
    updateWSqrtInvG(prior_R);
}

void
updatePriorBeta_DLMNoTrendNormZeroWithSeason(double *beta, int J, SEXP prior_R, 
					     double *thetaTransformed, double sigma) {
  int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
  double *beta_tilde = (double *)R_alloc(J, sizeof(double));
  if (isSaturated) {
    /* update alpha  */
    betaHatSeason(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = thetaTransformed[i] - beta_tilde[i];
    }
    updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = thetaTransformed[i] - beta_tilde[i];
    }
    updateSeason(prior_R, beta_tilde, J);
    /* update tau */
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
  }
  else {
    /* update alpha */
    betaHatSeason(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    updateSeason(prior_R, beta_tilde, J);
    /* update tau */
    updateTauNorm(prior_R, beta, J);
  }
  int isWithTrend = 0;
  updatePhi(prior_R, isWithTrend);
  updateOmegaAlpha(prior_R, isWithTrend);
  updateOmegaSeason(prior_R);
}


void
updatePriorBeta_DLMWithTrendNormZeroWithSeason(double *beta, int J, SEXP prior_R, 
					       double *thetaTransformed, double sigma) {
  int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
  double *beta_tilde = (double *)R_alloc(J, sizeof(double));
  if (isSaturated) {
    /* update alpha and delta */
    betaHatSeason(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = thetaTransformed[i] - beta_tilde[i];
    }
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = thetaTransformed[i] - beta_tilde[i];
    }
    updateSeason(prior_R, beta_tilde, J);
    /* update tau */
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
  }
  else {
    /* update alpha and delta */
    betaHatSeason(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    updateSeason(prior_R, beta_tilde, J);
    /* update tau */
    updateTauNorm(prior_R, beta, J);
  }
  int isWithTrend = 1;
  updatePhi(prior_R, isWithTrend);
  updateOmegaAlpha(prior_R, isWithTrend);
  updateOmegaDelta(prior_R);
  updateGWithTrend(prior_R);
  updateWSqrt(prior_R);
  updateWSqrtInvG(prior_R);
  updateOmegaSeason(prior_R);
}

void
updatePriorBeta_DLMNoTrendNormCovNoSeason(double *beta, int J, SEXP prior_R, 
					  double *thetaTransformed, double sigma) {
  int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
  double *beta_tilde = (double *)R_alloc(J, sizeof(double));
  if (isSaturated) {
    /* update alpha */
    betaHatCovariates(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = thetaTransformed[i] - beta_tilde[i];
    }
    updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = thetaTransformed[i] - beta_tilde[i];
    }
    updateEta(prior_R, beta_tilde, J);
    /* update tau */
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
  }
  else {
    /* update alpha */
    betaHatCovariates(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    updateEta(prior_R, beta_tilde, J);
    /* update tau */
    updateTauNorm(prior_R, beta, J);
  }
  updateUEtaCoef(prior_R);
  int isWithTrend = 0;
  updatePhi(prior_R, isWithTrend);
  updateOmegaAlpha(prior_R, isWithTrend);
}


void
updatePriorBeta_DLMWithTrendNormCovNoSeason(double *beta, int J, SEXP prior_R, 
					    double *thetaTransformed, double sigma) {
  int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
  double *beta_tilde = (double *)R_alloc(J, sizeof(double));
  if (isSaturated) {
    /* update alpha and delta */
    betaHatCovariates(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = thetaTransformed[i] - beta_tilde[i];
    }
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = thetaTransformed[i] - beta_tilde[i];
    }
    updateEta(prior_R, beta_tilde, J);
    /* update tau */
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
  }
  else {
    /* update alpha and delta */
    betaHatCovariates(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde[i] = beta[i] - beta_tilde[i];
    }
    updateEta(prior_R, beta_tilde, J);
    /* update tau */
    updateTauNorm(prior_R, beta, J);
  }
  updateUEtaCoef(prior_R);
  int isWithTrend = 1;
  updatePhi(prior_R, isWithTrend);
  updateOmegaAlpha(prior_R, isWithTrend);
  updateOmegaDelta(prior_R);
  updateGWithTrend(prior_R);
  updateWSqrt(prior_R);
  updateWSqrtInvG(prior_R);
}


void
updatePriorBeta_DLMNoTrendNormCovWithSeason(double *beta, int J, SEXP prior_R, 
					    double *thetaTransformed, double sigma) {
  int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
  double *beta_tilde = (double *)R_alloc(2*J, sizeof(double));
  double *beta_tilde1 = beta_tilde;
  double *beta_tilde2 = beta_tilde + J;
  if (isSaturated) {
    /* update alpha */
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = thetaTransformed[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateAlphaDLMNoTrend(prior_R, beta_tilde1, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = thetaTransformed[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateSeason(prior_R, beta_tilde1, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = thetaTransformed[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateEta(prior_R, beta_tilde1, J);
    /* update tau */
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
  }
  else {
    /* update alpha */
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateAlphaDLMNoTrend(prior_R, beta_tilde1, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateSeason(prior_R, beta_tilde1, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateEta(prior_R, beta_tilde1, J);
    /* update tau */
    updateTauNorm(prior_R, beta, J);
  }
  updateUEtaCoef(prior_R);
  int isWithTrend = 0;
  updatePhi(prior_R, isWithTrend);
  updateOmegaAlpha(prior_R, isWithTrend);
  updateOmegaSeason(prior_R);
}


void
updatePriorBeta_DLMWithTrendNormCovWithSeason(double *beta, int J, SEXP prior_R, 
					      double *thetaTransformed, double sigma) {
  int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
  double *beta_tilde = (double *)R_alloc(2*J, sizeof(double));
  double *beta_tilde1 = beta_tilde;
  double *beta_tilde2 = beta_tilde + J;
  if (isSaturated) {
    /* update alpha and delta */
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = thetaTransformed[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde1, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = thetaTransformed[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateSeason(prior_R, beta_tilde1, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = thetaTransformed[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateEta(prior_R, beta_tilde1, J);
    /* update tau */
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
  }
  else {
    /* update alpha and delta */
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde1, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateSeason(prior_R, beta_tilde1, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
      beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateEta(prior_R, beta_tilde1, J);
    /* update tau */
    updateTauNorm(prior_R, beta, J);
  }
  updateUEtaCoef(prior_R);
  int isWithTrend = 1;
  updatePhi(prior_R, isWithTrend);
  updateOmegaAlpha(prior_R, isWithTrend);
  updateOmegaDelta(prior_R);
  updateGWithTrend(prior_R);
  updateWSqrt(prior_R);
  updateWSqrtInvG(prior_R);
  updateOmegaSeason(prior_R);
}

void
updatePriorBeta_DLMNoTrendRobustZeroNoSeason(double *beta, int J, SEXP prior_R, 
					     double *thetaTransformed, double sigma) {
  updateAlphaDLMNoTrend(prior_R, beta, J);
  int isWithTrend = 0;
  updatePhi(prior_R, isWithTrend);
  updateUBeta(prior_R, beta, J);
  updateTauRobust(prior_R, J);
  updateOmegaAlpha(prior_R, isWithTrend);
}

void
updatePriorBeta_DLMWithTrendRobustZeroNoSeason(double *beta, int J, SEXP prior_R, 
					       double *thetaTransformed, double sigma) {
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
updatePriorBeta_DLMNoTrendRobustZeroWithSeason(double *beta, int J, SEXP prior_R, 
					       double *thetaTransformed, double sigma)
{
  double *beta_tilde = (double *)R_alloc(J, sizeof(double));
  /* update alpha */
  betaHatSeason(beta_tilde, prior_R, J);
  for (int i = 0; i < J; ++i) {
    double bh = beta_tilde[i];
    beta_tilde[i] = beta[i] - bh;
  }
  updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
  /* update season */
  betaHatAlphaDLM(beta_tilde, prior_R, J);
  for (int i = 0; i < J; ++i) {
    double bh = beta_tilde[i];
    beta_tilde[i] = beta[i] - bh;
  }
  updateSeason(prior_R, beta_tilde, J);
  /* update other */
  int isWithTrend = 0;
  updatePhi(prior_R, isWithTrend);
  updateUBeta(prior_R, beta, J);
  updateTauRobust(prior_R, J);
  updateOmegaAlpha(prior_R, isWithTrend);
  updateOmegaSeason(prior_R);
}

void
updatePriorBeta_DLMWithTrendRobustZeroWithSeason(double *beta, int J, SEXP prior_R, 
						 double *thetaTransformed, double sigma) {
  double *beta_tilde = (double *)R_alloc(J, sizeof(double));
  /* update alpha and delta */
  betaHatSeason(beta_tilde, prior_R, J);
  for (int i = 0; i < J; ++i) {
    double bh = beta_tilde[i];
    beta_tilde[i] = beta[i] - bh;
  }
  updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
  /* update season */
  betaHatAlphaDLM(beta_tilde, prior_R, J);
  for (int i = 0; i < J; ++i) {
    double bh = beta_tilde[i];
    beta_tilde[i] = beta[i] - bh;
  }
  updateSeason(prior_R, beta_tilde, J);
  /* update other */
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
updatePriorBeta_DLMNoTrendRobustCovNoSeason(double *beta, int J, SEXP prior_R, 
					    double *thetaTransformed, double sigma) {
  double *beta_tilde = (double *)R_alloc(J, sizeof(double));
  /* update alpha */
  betaHatCovariates(beta_tilde, prior_R, J);
  for (int i = 0; i < J; ++i) {
    double bh = beta_tilde[i];
    beta_tilde[i] = beta[i] - bh;
  }
  updateAlphaDLMNoTrend(prior_R, beta_tilde, J);
  /* update eta */
  betaHatAlphaDLM(beta_tilde, prior_R, J);
  for (int i = 0; i < J; ++i) {
    double bh = beta_tilde[i];
    beta_tilde[i] = beta[i] - bh;
  }
  updateEta(prior_R, beta_tilde, J);
  /* update other */
  updateUEtaCoef(prior_R);
  int isWithTrend = 0;
  updatePhi(prior_R, isWithTrend);
  updateUBeta(prior_R, beta, J);
  updateTauRobust(prior_R, J);
  updateOmegaAlpha(prior_R, isWithTrend);
}

void
updatePriorBeta_DLMWithTrendRobustCovNoSeason(double *beta, int J, SEXP prior_R, 
					      double *thetaTransformed, double sigma) {
  double *beta_tilde = (double *)R_alloc(J, sizeof(double));
  /* update alpha and delta */
  betaHatCovariates(beta_tilde, prior_R, J);
  for (int i = 0; i < J; ++i) {
    double bh = beta_tilde[i];
    beta_tilde[i] = beta[i] - bh;
  }
  updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde, J);
  /* update eta */
  betaHatAlphaDLM(beta_tilde, prior_R, J);
  for (int i = 0; i < J; ++i) {
    double bh = beta_tilde[i];
    beta_tilde[i] = beta[i] - bh;
  }
  updateEta(prior_R, beta_tilde, J);
  /* update other */
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
updatePriorBeta_DLMNoTrendRobustCovWithSeason(double *beta, int J, SEXP prior_R, 
					      double *thetaTransformed, double sigma) {
  double *beta_tilde = (double *)R_alloc(2*J, sizeof(double));
  double *beta_tilde1 = beta_tilde;
  double *beta_tilde2 = beta_tilde + J;
  /* update alpha */
  betaHatSeason(beta_tilde1, prior_R, J);
  betaHatCovariates(beta_tilde2, prior_R, J);
  for (int i = 0; i < J; ++i) {
    beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
  }
  updateAlphaDLMNoTrend(prior_R, beta_tilde1, J);
  /* update season */
  betaHatAlphaDLM(beta_tilde1, prior_R, J);
  betaHatCovariates(beta_tilde2, prior_R, J);
  for (int i = 0; i < J; ++i) {
    beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
  }
  updateSeason(prior_R, beta_tilde1, J);
  /* update eta */
  betaHatAlphaDLM(beta_tilde1, prior_R, J);
  betaHatSeason(beta_tilde2, prior_R, J);
  for (int i = 0; i < J; ++i) {
    beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
  }
  updateEta(prior_R, beta_tilde1, J);
  /* update other */
  updateUEtaCoef(prior_R);
  int isWithTrend = 0;
  updatePhi(prior_R, isWithTrend);
  updateUBeta(prior_R, beta, J);
  updateTauRobust(prior_R, J);
  updateOmegaAlpha(prior_R, isWithTrend);
  updateOmegaSeason(prior_R);
}


void
updatePriorBeta_DLMWithTrendRobustCovWithSeason(double *beta, int J, SEXP prior_R, 
						double *thetaTransformed, double sigma) {
    double *beta_tilde = (double *)R_alloc(2*J, sizeof(double));
    double *beta_tilde1 = beta_tilde;
    double *beta_tilde2 = beta_tilde + J;
    /* update alpha and delta */
    betaHatSeason(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateAlphaDeltaDLMWithTrend(prior_R, beta_tilde1, J);
    /* update season */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatCovariates(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateSeason(prior_R, beta_tilde1, J);
    /* update eta */
    betaHatAlphaDLM(beta_tilde1, prior_R, J);
    betaHatSeason(beta_tilde2, prior_R, J);
    for (int i = 0; i < J; ++i) {
        beta_tilde1[i] = beta[i] - beta_tilde1[i] - beta_tilde2[i];
    }
    updateEta(prior_R, beta_tilde1, J);
    /* update other */
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
updatePriorBeta_KnownCertain(double *beta, int J, SEXP prior_R, 
			     double *thetaTransformed, double sigma) {
  /* no action */
}

void
updatePriorBeta_KnownUncertain(double *beta, int J, SEXP prior_R, 
			       double *thetaTransformed, double sigma) {
  /* no action */
}

void
updatePriorBeta_MixNormZero(double *beta, int J, SEXP prior_R, 
			    double *thetaTransformed, double sigma) {
    int isSaturated = *INTEGER(GET_SLOT(prior_R, isSaturated_sym));
    double *beta_tilde = (double *)R_alloc(J, sizeof(double));
    
    if (isSaturated) {
	SET_DOUBLESCALE_SLOT(prior_R, tau_sym, sigma);
	beta_tilde = thetaTransformed;
    }
    else {
	updateTauNorm(prior_R, beta, J);
	beta_tilde = beta;
    }
    updateVectorsMixAndProdVectorsMix(prior_R, beta_tilde, J);
    updateOmegaVectorsMix(prior_R);
    updateLatentComponentWeightMix(prior_R);
    updateComponentWeightMix(prior_R);
    updateWeightMix(prior_R);
    updateLatentWeightMix(prior_R);
    updateOmegaComponentWeightMix(prior_R);
    updateOmegaLevelComponentWeightMix(prior_R);
    updateIndexClassMaxPossibleMix(prior_R);
    updateIndexClassMix(prior_R, beta_tilde, J);
    updateIndexClassMaxUsedMix(prior_R);
    updateLevelComponentWeightMix(prior_R);
    updateMeanLevelComponentWeightMix(prior_R);
    updatePhiMix(prior_R);
    updateAlphaMix(prior_R);
}

void
updatePriorBeta_Zero(double *beta, int J, SEXP prior_R, 
		     double *thetaTransformed, double sigma) {
  /* no action */
}
