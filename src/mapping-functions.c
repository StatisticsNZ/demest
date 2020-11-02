#include "mapping-functions.h"
#include "demest.h"


/* File "mapping-function.c" contains C versions of functions 
 * from "mapping-functions.R". */

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

  
/* mappings to population */

int
getIPopnNextFromComp(int i, SEXP mapping_R)
{
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedPopnVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nTimeComp  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimePopn  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iPopnNext_r = 1;
    int iMinus1 = i-1;
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedPopn = stepSharedPopnVec[d];
        int iShared = (iMinus1/stepSharedComp) % nShared; 
        iPopnNext_r += iShared * stepSharedPopn;
    }
    
    int iTime = (iMinus1/stepTimeComp) % nTimeComp; 
    ++iTime; 
    iPopnNext_r += iTime * stepTimePopn;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgePopn = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int iAge = iMinus1/stepAgeComp % nAge;
        
        if (iAge < nAge - 1) {
            int stepTriangleCurrent = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
            int iTriangle = iMinus1/stepTriangleCurrent % 2;
            int isUpper = (iTriangle == 1);
            if (isUpper) {
                ++iAge;
            }
        }
        
        iPopnNext_r += iAge * stepAgePopn;
    }
    
    return iPopnNext_r;
}

SEXP
getIPopnNextFromOrigDest(int i, SEXP mapping_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, 2));
    int *ans = INTEGER(ans_R);
    
    getIPopnNextFromOrigDestInternal(ans, i, mapping_R);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}

/* ans must have 2 elements */
void
getIPopnNextFromOrigDestInternal(int *ans, int i, SEXP mapping_R)
{
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedPopnVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nTimeComp  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimePopn  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepDestCompVec  = INTEGER(GET_SLOT(mapping_R, stepDestCurrentVec_sym));
    int *stepOrigDestPopnVec  = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    
    int iPopnNextOrig_r = 1;
    int iMinus1 = i - 1;
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedPopn = stepSharedPopnVec[d];
        int iShared = (iMinus1/stepSharedComp) % nShared; 
        iPopnNextOrig_r += iShared * stepSharedPopn;
    }
    
    int iTime = (iMinus1/stepTimeComp) % nTimeComp; 
    ++iTime; 
    iPopnNextOrig_r += iTime * stepTimePopn;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgePopn = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int iAge = iMinus1/stepAgeComp % nAge;
        
        if (iAge < nAge - 1) {
            int stepTriangleCurrent = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
            int iTriangle = (iMinus1/stepTriangleCurrent) % 2;
            int isUpper = (iTriangle == 1);
            if (isUpper) {
                ++iAge;
            }
        }
        
        iPopnNextOrig_r += iAge * stepAgePopn;
    }
    
    int iPopnNextDest_r = iPopnNextOrig_r;
    
    for (int d = 0; d < nDimOrigDest; ++d) {
        int nOrigDest = nOrigDestVec[d];
        int stepOrigComp = stepOrigCompVec[d];
        int stepDestComp = stepDestCompVec[d];
        int stepOrigDestPopn = stepOrigDestPopnVec[d];
        
        int iOrig = (iMinus1/stepOrigComp) % nOrigDest; 
        int iDest = (iMinus1/stepDestComp) % nOrigDest; 
        iPopnNextOrig_r += iOrig * stepOrigDestPopn;
        iPopnNextDest_r += iDest * stepOrigDestPopn;
    }
    
    ans[0] = iPopnNextOrig_r;
    ans[1] = iPopnNextDest_r;
}

int
getIAccNextFromComp(int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeAcc  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedAccVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    int isBirths = 1-hasAge;
    
    int nDimShared = LENGTH(nSharedVec_R);
    int iMinus1 = i - 1;
    
    int iTimeComp = (iMinus1 / stepTimeComp) % nTime;
    
    int iAccNext_r = 1;
    int iTimeAcc = 0;
    int returnZero = 0;
    
    int iTimeCompLessNTimeMinusOne = (iTimeComp < (nTime - 1));
    
    if (isBirths) {
        if ( iTimeCompLessNTimeMinusOne ) {
            iTimeAcc = iTimeComp + 1;
        }
        else {
            iAccNext_r = 0;
            returnZero = 1;
        }
    }
    
    else { /* not isBirths */
        int nAgeComp = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeAcc = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, 
                                                stepTriangleCurrent_sym));
        
        int iTriangleComp = ( iMinus1 / stepTriangleComp ) % 2;
        int isLower = (iTriangleComp == 0);
        
        iTimeAcc = iTimeComp;
        if (isLower) {
            if ( iTimeCompLessNTimeMinusOne ) {
                iTimeAcc = iTimeComp + 1;
            }
            else {
                iAccNext_r = 0;
                returnZero = 1;
            }
        }
        
        if (!returnZero) {
            int iAgeComp  = ( iMinus1 / stepAgeComp ) % nAgeComp;
	    iAccNext_r += iAgeComp * stepAgeAcc;
        }
    }

    if (!returnZero) {

        iAccNext_r += iTimeAcc * stepTimeAcc;
        
        for (int d = 0; d < nDimShared; ++d) {
            int nShared = nSharedVec[d];
            int stepSharedComp = stepSharedCompVec[d];
            int stepSharedAcc = stepSharedAccVec[d];
            int iShared = (iMinus1 / stepSharedComp ) % nShared;
            iAccNext_r += iShared * stepSharedAcc;
        }
    }
    
    return iAccNext_r;
}


SEXP
getIAccNextFromOrigDest(int i, SEXP mapping_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, 2));
    int *ans = INTEGER(ans_R);
    
    getIAccNextFromOrigDestInternal(ans, i, mapping_R);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}

/* ans must have 2 elements */
void
getIAccNextFromOrigDestInternal(int *ans, int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeAcc  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
    int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
    int stepAgeAcc = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
    int stepTriangle = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedAccVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepDestCompVec  = INTEGER(GET_SLOT(mapping_R, stepDestCurrentVec_sym));
    int *stepOrigDestAccVec = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    int iMinus1 = i - 1;

    int iTimeComp = (iMinus1 / stepTimeComp) % nTime;
    int iTriangle = (iMinus1 / stepTriangle) % 2;
    int isLower = (iTriangle == 0);
    
    int iAccNextOrig_r = 0;
    int iAccNextDest_r = 0;
    int returnZeros = 0;
    int iTimeCompLessThanNTimeMinus1 = (iTimeComp < (nTime - 1));
    
    int iTimeAcc = iTimeComp;
    if (isLower) {
        if (iTimeCompLessThanNTimeMinus1) {
            iTimeAcc = iTimeComp + 1;
        }
        else {
            returnZeros = 1;
        }
    }
    
    if (!returnZeros) {
        int iAccNext = 1 + iTimeAcc * stepTimeAcc;
        int iAge = (iMinus1 / stepAgeComp) % nAge;
 
	iAccNext += iAge * stepAgeAcc;
            
	for (int d = 0; d < nDimShared; ++d) {
	  int nShared = nSharedVec[d];
	  int stepSharedComp = stepSharedCompVec[d];
	  int stepSharedAcc = stepSharedAccVec[d];
	  int iShared = (iMinus1/stepSharedComp) % nShared; 
	  iAccNext += iShared * stepSharedAcc;
	}
            
	iAccNextOrig_r = iAccNext;
	iAccNextDest_r = iAccNext;
            
	for (int d = 0; d < nDimOrigDest; ++d) {
	  int nOrigDest = nOrigDestVec[d];
	  int stepOrigComp = stepOrigCompVec[d];
	  int stepDestComp = stepDestCompVec[d];
	  int stepOrigDestAcc = stepOrigDestAccVec[d];
                
	  int iOrig = (iMinus1/stepOrigComp) % nOrigDest; 
	  int iDest = (iMinus1/stepDestComp) % nOrigDest; 
	  iAccNextOrig_r += iOrig * stepOrigDestAcc;
	  iAccNextDest_r += iDest * stepOrigDestAcc;
	}
    }

    ans[0] = iAccNextOrig_r;
    ans[1] = iAccNextDest_r;
}

/* *********** MAPPINGS TO EXPOSURE - iExposure *********** */

int
getIExposureFromComp(int i, SEXP mapping_R)
{
    int isOneToOne = *INTEGER(GET_SLOT(mapping_R, isOneToOne_sym));
    
    int returnValue = i;
    if (!isOneToOne) {
        returnValue = getIExposureFromCompNotOneToOne(i, mapping_R);
    }
    return returnValue;
}

/* does the work for getIExposureFromComp if mapping is not one-to-one */
int
getIExposureFromCompNotOneToOne(int i, SEXP mapping_R)
{    
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iMinus1 = i - 1;
    
    int iTime = ( iMinus1 / stepTimeComp ) % nTime;
    int iExp_r = 1 + iTime * stepTimeExp;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        
        int iAge = iMinus1/stepAgeComp % nAge;
        int iTriangle = iMinus1/stepTriangleComp % 2;
        
        iExp_r += iAge * stepAgeExp;
        iExp_r += iTriangle * stepTriangleExp;
    }
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1/stepSharedComp) % nShared; 
        iExp_r += iShared * stepSharedExp;
    }
    
    return iExp_r;
}

int
getIExposureFromBirths(int i, SEXP mapping_R)
{   
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeBirths  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    int hasSex  = *LOGICAL(GET_SLOT(mapping_R, hasSex_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedBirthsVec = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentExposureVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iMinus1 = i - 1;
    
    int iTime = ( iMinus1 / stepTimeBirths ) % nTime;
    int iExp_r = 1 + iTime * stepTimeExp;
    
    if (hasAge) {
        int nAgeBirths = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeBirths = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleBirths = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        
        int iMinAge = *INTEGER(GET_SLOT(mapping_R, iMinAge_sym));
        int iAgeBirths = iMinus1/stepAgeBirths % nAgeBirths;
        int iAgeExp = iAgeBirths + iMinAge - 1;
        
        int iTriangle = iMinus1/stepTriangleBirths % 2;
        
        iExp_r += iAgeExp * stepAgeExp;
        iExp_r += iTriangle * stepTriangleExp;
    }

    if (hasSex) {
        int iSexDominant = *INTEGER(GET_SLOT(mapping_R, iSexDominant_sym));
        int stepSexExp = *INTEGER(GET_SLOT(mapping_R, stepSexTarget_sym));
        iExp_r += iSexDominant * stepSexExp;
    }

    for (int d = 0; d < nDimShared; ++d) {
      int nShared = nSharedVec[d];
      int stepSharedBirths = stepSharedBirthsVec[d];
      int stepSharedExp = stepSharedExpVec[d];
      int iShared = (iMinus1/stepSharedBirths) % nShared; 
      iExp_r += iShared * stepSharedExp;
    }
    
    return iExp_r;
}


int
getIExposureFromOrigDest(int i, SEXP mapping_R)
{
    int isOneToOne = *INTEGER(GET_SLOT(mapping_R, isOneToOne_sym));
    
    int returnValue = i;
    if (!isOneToOne) {
        returnValue = getIExposureFromOrigDestNotOneToOne(i, mapping_R);
    }
    return returnValue;
}

/* does the work for getIExposureFromOrigDest if mapping is not one-to-one */
int
getIExposureFromOrigDestNotOneToOne(int i, SEXP mapping_R)
{    
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepOrigDestAccVec  = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    
    int iMinus1 = i - 1;
    
    int iTime = ( iMinus1 / stepTimeComp ) % nTime;
    int iExp_r = 1 + iTime * stepTimeExp;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        
        int iAge = iMinus1/stepAgeComp % nAge;
        int iTriangle = iMinus1/stepTriangleComp % 2;
        
        iExp_r += iAge * stepAgeExp;
        iExp_r += iTriangle * stepTriangleExp;
    }
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1/stepSharedComp) % nShared; 
        iExp_r += iShared * stepSharedExp;
    }
    
    for (int d = 0; d < nDimOrigDest; ++d) {
        int nOrigDest = nOrigDestVec[d];
        int stepOrigComp = stepOrigCompVec[d];
        int stepOrigDestAcc = stepOrigDestAccVec[d];
        
        int iOrig = (iMinus1/stepOrigComp) % nOrigDest;
        
        iExp_r += iOrig * stepOrigDestAcc;
    }
    
    return iExp_r;
    
}


/* *************** MAPPINGS TO EXPOSURE - iExpFirst **************** */


int
getIExpFirstFromComp(int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int iMinus1 = i - 1;
    
    int iExp_r = 1; 

    int iTimeComp = (iMinus1 / stepTimeComp) % nTime;
    iExp_r += iTimeComp * stepTimeExp;
    
    if (hasAge) { 
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, 
                                                stepTriangleCurrent_sym));
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, 
                                                stepTriangleTarget_sym));
        int iAgeComp = ( iMinus1 / stepAgeComp ) % nAge;
        int iTriangleComp = ( iMinus1 / stepTriangleComp ) % 2;
        iExp_r += iAgeComp * stepAgeExp + iTriangleComp * stepTriangleExp;
    }
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1 / stepSharedComp ) % nShared;
        iExp_r += iShared * stepSharedExp;
    }
                
    return iExp_r;
}

int
getIExpFirstFromBirths(int i, SEXP mapping_R)
{   

    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeBirths  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));

    int hasSex = *LOGICAL(GET_SLOT(mapping_R, hasSex_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedBirthsVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iMinus1 = i - 1;
    
    int iTime = ( iMinus1 / stepTimeBirths ) % nTime;
    int iExp_r = 1 + iTime * stepTimeExp;

    if (hasSex) {
        int stepSexBirths  = *INTEGER(GET_SLOT(mapping_R, stepSexCurrent_sym));
        int stepSexExp  = *INTEGER(GET_SLOT(mapping_R, stepSexTarget_sym));
        int iSex = ( iMinus1 / stepSexBirths ) % 2;
        iExp_r += iSex * stepSexExp;
    }
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedBirths = stepSharedBirthsVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1/stepSharedBirths) % nShared; 
        iExp_r += iShared * stepSharedExp;
    }
    
    return iExp_r;
}

SEXP
getIExpFirstPairFromOrigDest(int i, SEXP mapping_R)
{
    SEXP ans_R;
    PROTECT(ans_R = allocVector(INTSXP, 2));
    int *ans = INTEGER(ans_R);
    
    getIExpFirstPairFromOrigDestInternal(ans, i, mapping_R);
    
    UNPROTECT(1); /* ans_R */
    return ans_R;
}



/* ans must have 2 elements */
void
getIExpFirstPairFromOrigDestInternal(int *ans, int i, SEXP mapping_R)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeComp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int hasAge = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    
    SEXP nOrigDestVec_R = GET_SLOT(mapping_R, nOrigDestVec_sym);
    int *nOrigDestVec  = INTEGER(nOrigDestVec_R);
    int *stepOrigCompVec  = INTEGER(GET_SLOT(mapping_R, stepOrigCurrentVec_sym));
    int *stepDestCompVec  = INTEGER(GET_SLOT(mapping_R, stepDestCurrentVec_sym));
    int *stepOrigDestExpVec = INTEGER(GET_SLOT(mapping_R, stepOrigDestTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int nDimOrigDest = LENGTH(nOrigDestVec_R);
    int iMinus1 = i - 1;

    int iTimeComp = (iMinus1 / stepTimeComp) % nTime;
    
    int iExp = 1;
    int iExpOrig_r = 0;
    int iExpDest_r = 0;
    iExp += iTimeComp * stepTimeExp;
    
    if (hasAge) {
        int nAge = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int stepAgeComp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        int stepTriangleComp = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        int iAgeComp = (iMinus1 / stepAgeComp) % nAge;
        int iTriangleComp = (iMinus1 / stepTriangleComp) % 2;
        iExp += iAgeComp * stepAgeExp + iTriangleComp * stepTriangleExp;
    }
            
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int iShared = (iMinus1 / stepSharedComp ) % nShared;
        iExp += iShared * stepSharedExp;
    }
        
    iExpOrig_r = iExp;
    iExpDest_r = iExp;
        
    for (int d = 0; d < nDimOrigDest; ++d) {
        int nOrigDest = nOrigDestVec[d];
        int stepOrigComp = stepOrigCompVec[d];
        int stepDestComp = stepDestCompVec[d];
        int stepOrigDestExp = stepOrigDestExpVec[d];
                
        int iOrig = (iMinus1/stepOrigComp) % nOrigDest; 
        int iDest = (iMinus1/stepDestComp) % nOrigDest; 
                
        iExpOrig_r += iOrig * stepOrigDestExp;
        iExpDest_r += iDest * stepOrigDestExp;
    }

    ans[0] = iExpOrig_r;
    ans[1] = iExpDest_r;    
}


int
getICellCompFromExp(int i, SEXP mapping_R)
{
    int isOneToOne = *INTEGER(GET_SLOT(mapping_R, isOneToOne_sym));
    
    int returnValue = i;
    if (!isOneToOne) {
        returnValue = getICellCompFromExpNotOneToOne(i, mapping_R);
    }
    return returnValue;
}

/* does the work for getICellCompFromExp if mapping is not one-to-one */
int
getICellCompFromExpNotOneToOne(int i, SEXP mapping_R)
{    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedCompVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    
    int iMinus1 = i - 1;
    
    int iComp_r = 1;
    
    for (int d = 0; d < nDimShared; ++d) {
        int nShared = nSharedVec[d];
        int stepSharedExp = stepSharedExpVec[d];
        int stepSharedComp = stepSharedCompVec[d];
        int iShared = (iMinus1/stepSharedExp) % nShared; 
        iComp_r += iShared * stepSharedComp;
    }
    
    return iComp_r;
}

int
getICellBirthsFromExp(int i, SEXP mapping_R, int ageForward)
{
    int nTime  = *INTEGER(GET_SLOT(mapping_R, nTimeCurrent_sym));
    int stepTimeExp  = *INTEGER(GET_SLOT(mapping_R, stepTimeCurrent_sym));
    int stepTimeBirths  = *INTEGER(GET_SLOT(mapping_R, stepTimeTarget_sym));
    int hasAge  = *LOGICAL(GET_SLOT(mapping_R, hasAge_sym));
    int hasSex  = *LOGICAL(GET_SLOT(mapping_R, hasSex_sym));
    
    SEXP nSharedVec_R = GET_SLOT(mapping_R, nSharedVec_sym);
    int *nSharedVec  = INTEGER(nSharedVec_R);
    int *stepSharedExpVec  = INTEGER(GET_SLOT(mapping_R, stepSharedCurrentVec_sym));
    int *stepSharedBirthsVec  = INTEGER(GET_SLOT(mapping_R, stepSharedTargetVec_sym));
    
    int nDimShared = LENGTH(nSharedVec_R);
    int iMinus1 = i - 1;
        
    int iBirths_r = 1;
    int iTimeExp = (iMinus1 / stepTimeExp) % nTime;
    int iTimeBirths = iTimeExp;
    int returnZero = 0;

    if (hasSex) {

        int iSexDominant = *INTEGER(GET_SLOT(mapping_R, iSexDominant_sym));
        int stepSexExp = *INTEGER(GET_SLOT(mapping_R, stepSexCurrent_sym));
	int iSexExp = iMinus1/stepSexExp % 2;
	if (iSexExp != iSexDominant) {
	  iBirths_r = 0;
	  returnZero = 1;
	}

    } /* end hasSex */
    if (!returnZero && hasAge) {

        int nAgeExp = *INTEGER(GET_SLOT(mapping_R, nAgeCurrent_sym));
        int nAgeBirths = *INTEGER(GET_SLOT(mapping_R, nAgeTarget_sym));
        
        int stepAgeExp = *INTEGER(GET_SLOT(mapping_R, stepAgeCurrent_sym));
        int stepAgeBirths = *INTEGER(GET_SLOT(mapping_R, stepAgeTarget_sym));
        
        int stepTriangleExp = *INTEGER(GET_SLOT(mapping_R, stepTriangleCurrent_sym));
        int stepTriangleBirths = *INTEGER(GET_SLOT(mapping_R, stepTriangleTarget_sym));
        
        int iMinAge = *INTEGER(GET_SLOT(mapping_R, iMinAge_sym));
        
        int iAgeExp = iMinus1/stepAgeExp % nAgeExp;
        int iTriangleExp = iMinus1/stepTriangleExp % 2;
        
        iTimeBirths = iTimeExp + iMinAge - iAgeExp - 2;

	int ageLTMin = iAgeExp < (iMinAge - 1);
	int ageLEMax = iAgeExp < (iMinAge + nAgeBirths - 1);
	  

        if (ageLTMin) {
            if (iTriangleExp == 0) {
                iTimeBirths = iTimeExp + iMinAge - iAgeExp - 1;
            }
            if ((iTimeBirths >= nTime) || !ageForward) {
                iBirths_r = 0;
                returnZero = 1;
            }
        }
        else if (!ageLTMin && ageLEMax) {
            int iAgeBirths = iAgeExp - iMinAge + 1;
            int iTriangleBirths = iTriangleExp;
            iTimeBirths = iTimeExp;
            iBirths_r += iAgeBirths * stepAgeBirths;
            iBirths_r += iTriangleBirths * stepTriangleBirths;
        }
        else {
                iBirths_r = 0;
                returnZero = 1;
        }
    } /* end hasAge */
    
    /* else iTimeBirths = iTimeExp as default */
   
    if (!returnZero) {
        iBirths_r += iTimeBirths * stepTimeBirths;
        
        for (int d = 0; d < nDimShared; ++d) {
            int nShared = nSharedVec[d];
            int stepSharedExp = stepSharedExpVec[d];
            int stepSharedBirths = stepSharedBirthsVec[d];
            int iShared = (iMinus1/stepSharedExp) % nShared; 
            iBirths_r += iShared * stepSharedBirths;
        }
    }
    
    return iBirths_r;
}

