
#include "update-nongeneric.h"
#include "demest.h"

extern SEXP
  Data_sym,  /* used for .Data slot */
  iMethodPrior_sym,
  Z_sym,
  beta_sym,
  eta_sym,
  gamma_sym,
  lower_sym,
  tau_sym,
  tauMax_sym,
  upper_sym,
  order_sym,
  iteratorBeta_sym,
  iWithin_sym,
  nWithin_sym,
  iBetween_sym,
  nBetween_sym,
  incrementBetween_sym,
  indices_sym,
  initial_sym,
  dimIterators_sym,
  strideLengths_sym,
  nStrides_sym,
  dimBefore_sym,
  dimAfter_sym,
  posDim_sym,
  lengthDim_sym,
  iMethodModel_sym,
  meansBetas_sym,
  variancesBetas_sym,
  betaEqualsMean_sym,
  acceptBeta_sym,
  priorsBetas_sym,
  logPostPriorsBetas_sym,
  logPostBetas_sym,
  logPostSigma_sym,
  logPostTheta_sym,
  logPostVarsigma_sym,
  theta_sym,
  thetaTransformed_sym,
  cellInLik_sym,
  mu_sym,
  sigma_sym,
  sigmaMax_sym,
  ASigma_sym,
  nuSigma_sym,
  varsigma_sym,
  varsigmaMax_sym,
  varsigmaSetToZero_sym,
  AVarsigma_sym,
  nuVarsigma_sym,
  w_sym,
  scaleTheta_sym,
  scaleThetaMultiplier_sym,
  nAcceptTheta_sym,
  betas_sym,
  priors_sym,
  iteratorBetas_sym,
  dims_sym,
  prob_sym,
  ADelta0_sym,
  meanDelta0_sym,

  mean_sym,
  sd_sym,

  tolerance_sym,
  betaIsPredicted_sym,
  nFailedPropTheta_sym,
  nFailedPropYStar_sym,
  maxAttempt_sym,
  valueAg_sym,
  weightAg_sym,
  transformAg_sym,
  meanAg_sym,
  sdAg_sym,
  scaleAg_sym,
  nAcceptAg_sym,
  nFailedPropValueAg_sym,
  funAg_sym,
  xArgsAg_sym,
  weightsArgsAg_sym,
  mxAg_sym,
  axAg_sym,
  nxAg_sym,
  nAgeAg_sym,
  transformThetaToMxAg_sym,
  subtotals_sym,
  transformSubtotals_sym,
  subtotalsNet_sym,
  slotsToExtract_sym,
  iMethodCombined_sym,
  model_sym,
  exposure_sym,
  y_sym,
  dataModels_sym,
  datasets_sym,
  transforms_sym,
  seriesIndices_sym,
  updateComponent_sym,
  updateDataModel_sym,
  updateSystemModel_sym,

  J_sym,

  UC_sym,
  DC_sym,
  UR_sym,
  DCInv_sym,
  DRInv_sym,
  CC_sym,
  a_sym,
  R_sym,
  priorsW_sym,
  v_sym,
  forward_sym,
  offsetsBetas_sym,
  offsetsPriorsBetas_sym,
  offsetsSigma_sym,
  offsetsVarsigma_sym,
  m0_sym,
  C0_sym,
  phi_sym,
  w_sym,
  iteratorGamma_sym,
  iteratorV_sym,
  delta_sym,
  phiKnown_sym,
  /* description */
  nTime_sym,
  stepTime_sym,
  hasAge_sym,
  hasSex_sym,
  iSexDominant_sym,
  stepSexCurrent_sym,
  stepSexTarget_sym,
  nAge_sym,
  stepAge_sym,
  length_sym,
  stepTriangle_sym,
  stepDirection_sym,
  nBetweenVec_sym,
  stepBetweenVec_sym,
  nWithinVec_sym,
  stepWithinVec_sym,
  /* cohort iterators */
  i_sym,
  iTime_sym,
  iAge_sym,
  iTriangle_sym,
  finished_sym,
  iVec_sym,
  lengthVec_sym,
  increment_sym,
  lastAgeGroupOpen_sym,
  /* mappings */
  hasParCh_sym,
  isOneToOne_sym,
  nSharedVec_sym,
  stepSharedCurrentVec_sym,
  stepSharedCurrentExposureVec_sym,
  stepSharedTargetVec_sym,
  nTimeCurrent_sym,
  stepTimeCurrent_sym,
  stepTimeTarget_sym,
  nAgeCurrent_sym,
  nAgeTarget_sym,
  stepAgeCurrent_sym,
  stepAgeTarget_sym,
  stepTriangleCurrent_sym,
  stepTriangleTarget_sym,
  nOrigDestVec_sym,
  stepOrigCurrentVec_sym,
  stepDestCurrentVec_sym,
  stepOrigDestTargetVec_sym,
  iMinAge_sym,

  /*new priors*/
  ATau_sym,
  nuTau_sym,
  hasAlphaDLM_sym,
  hasAlphaICAR_sym,
  hasAlphaMix_sym,
  hasCovariates_sym,
  hasAlphaKnown_sym,
  hasMean_sym,
  hasSeason_sym,
  isKnownUncertain_sym,
  isNorm_sym,
  isRobust_sym,
  isZeroVar_sym,
  alphaDLM_sym,
  alphaICAR_sym,
  alphaMix_sym,
  iteratorState_sym,
  iteratorStateOld_sym,
  K_sym,
  L_sym,
  s_sym,
  UBeta_sym,
  nuBeta_sym,
  isSaturated_sym,
  allStrucZero_sym,
  alongAllStrucZero_sym,
  strucZeroArray_sym,
  mNoTrend_sym,
  m0NoTrend_sym,
  CNoTrend_sym,
  aNoTrend_sym,
  RNoTrend_sym,
  GWithTrend_sym,
  mWithTrend_sym,
  m0WithTrend_sym,
  CWithTrend_sym,
  aWithTrend_sym,
  hasLevel_sym,
  omegaAlpha_sym,
  omegaAlphaMax_sym,
  AAlpha_sym,
  nuAlpha_sym,
  deltaDLM_sym,
  omegaDelta_sym,
  omegaDeltaMax_sym,
  nuDelta_sym,
  ADelta_sym,
  minPhi_sym,
  maxPhi_sym,
  shape1Phi_sym,
  shape2Phi_sym,
  WSqrt_sym,
  WSqrtInvG_sym,
  exposureAg_sym,
  P_sym,
  AEtaIntercept_sym,
  AEtaCoef_sym,
  nuEtaCoef_sym,
  meanEtaCoef_sym,
  UEtaCoef_sym,
  nSeason_sym,
  ASeason_sym,
  omegaSeason_sym,
  omegaSeasonMax_sym,
  nuSeason_sym,
  mSeason_sym,
  m0Season_sym,
  CSeason_sym,
  aSeason_sym,
  RSeason_sym,
  JOld_sym,
  /* new priors Jan 2017 */
  sumsWeightsMix_sym,
  weightMix_sym,
  latentWeightMix_sym,
  foundIndexClassMaxPossibleMix_sym,
  indexClassMaxPossibleMix_sym,
  indexClassProbMix_sym,
  componentWeightMix_sym,
  latentComponentWeightMix_sym,
  levelComponentWeightMix_sym,
  levelComponentWeightOldMix_sym,
  meanLevelComponentWeightMix_sym,
  indexClassMix_sym,
  indexClassMaxMix_sym,
  indexClassMaxUsedMix_sym,
  omegaComponentWeightMix_sym,
  omegaComponentWeightMaxMix_sym,
  omegaLevelComponentWeightMix_sym,
  omegaLevelComponentWeightMaxMix_sym,
  iteratorsDimsMix_sym,
  iAlong_sym,
  dimBeta_sym,
  dimBetaOld_sym,
  phiMix_sym,
  mMix_sym,
  CMix_sym,
  aMix_sym,
  RMix_sym,
  prodVectorsMix_sym,
  posProdVectors1Mix_sym,
  posProdVectors2Mix_sym,
  nBetaNoAlongMix_sym,
  vectorsMix_sym,
  omegaVectorsMix_sym,
  iteratorProdVectorMix_sym,
  yXMix_sym,
  XXMix_sym,
  priorMeanLevelComponentWeightMix_sym,
  priorSDLevelComponentWeightMix_sym,
  AComponentWeightMix_sym,
  nuComponentWeightMix_sym,
  omegaVectorsMix_sym,
  omegaVectorsMaxMix_sym,
  AVectorsMix_sym,
  nuVectorsMix_sym,
  minLevelComponentWeight_sym,
  maxLevelComponentWeight_sym,
  updateSeriesDLM_sym,
  ALevelComponentWeightMix_sym,
  nuLevelComponentWeightMix_sym,

  nuCMP_sym,
  sdLogNuCMP_sym,
  sdLogNuMaxCMP_sym,
  meanMeanLogNuCMP_sym,
  sdMeanLogNuCMP_sym,
  meanLogNuCMP_sym,
  ASDLogNuCMP_sym,
  nuSDLogNuCMP_sym,
  nu_sym,

  alphaKnown_sym,
  AKnownVec_sym,

  /* skeleton */
  first_sym,
  last_sym,

  /* Box-Cox */
  boxCoxParam_sym,

  /* accounts  and combined accounts*/
  account_sym,
  population_sym,
  accession_sym,
  components_sym,
  descriptions_sym,
  iteratorPopn_sym,
  iteratorAcc_sym,
  iteratorExposure_sym,
  transformsExpToComp_sym,
  transformExpToBirths_sym,
  iCell_sym,
  iCellOther_sym,
  iComp_sym,
  iPopnNext_sym,
  iPopnNextOther_sym,
  iAccNext_sym,
  iAccNextOther_sym,
  iOrigDest_sym,
  iPool_sym,
  iIntNet_sym,
  iBirths_sym,
  iParCh_sym,
  diffProp_sym,
  isIncrement_sym,
  isNet_sym,
  scaleNoise_sym,
  usePriorPopn_sym,
  systemModels_sym,
  modelUsesExposure_sym,
  mappingsFromExp_sym,
  mappingsToExp_sym,
  mappingsToPopn_sym,
  mappingsToAcc_sym,
  iExpFirst_sym,
  iExpFirstOther_sym,
  ageTimeStep_sym,
  iteratorsComp_sym,
  expectedExposure_sym,
  iExposure_sym,
  iExposureOther_sym,
  isLowerTriangle_sym,
  isOldestAgeGroup_sym,
  generatedNewProposal_sym,
  probSmallUpdate_sym,
  isSmallUpdate_sym,
  isSmallUpdateFinal_sym,
  probPopn_sym,
  cumProbComp_sym,
  nCellAccount_sym,
  /* LN@ */
  alphaLN2_sym,
  transformLN2_sym,
  constraintLN2_sym,
  nCellBeforeLN2_sym;

extern SEXP (*dembase_Collapse_R)(SEXP ,SEXP);
extern SEXP (*dembase_Extend_R)(SEXP ,SEXP);
extern int (*dembase_getIAfter)(int, SEXP);
extern SEXP (*dembase_getIBefore)(int, SEXP);
extern SEXP (*dembase_getIShared)(int, SEXP);


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
