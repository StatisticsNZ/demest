
#include "Prior-methods.h"
#include "helper-functions.h"
#include "iterators-methods.h"
#include "demest.h"
#include "R_ext/BLAS.h" /* dtrmv */
/* for BLAS level 2 documention see www.netlib.org/blas/blas2-paper.ps */ 
#include "R_ext/Linpack.h" /* dqrsl dtrsl */

/* File "Prior-methods.c" contains C versions of functions 
 * from "Prior-methods.R". */

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


/* ******************************************************************************** */
/* Functions for updating priors. ************************************************* */
/* ******************************************************************************** */

/* Note that these functions modify the priors in place, 
   unlike the R versions, or the R-visible C versions
   created in init.c. */

static __inline__ void
predictPrior_ExchFixed_i(SEXP prior_R) 
{
    /* null op */
}

static __inline__ void
predictPrior_ExchNormZero_i(SEXP prior_R) 
{
    /* null op */
}

static __inline__ void
predictPrior_ExchNormCov_i(SEXP prior_R) 
{
    /* null op */
}

static __inline__ void
predictPrior_ExchRobustZero_i(SEXP prior_R) 
{
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_ExchRobustCov_i(SEXP prior_R) 
{
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_DLMNoTrendNormZeroNoSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
predictPrior_DLMWithTrendNormZeroNoSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
predictPrior_DLMNoTrendNormZeroWithSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDLMNoTrend(prior_R);
    predictSeason(prior_R);
}

static __inline__ void
predictPrior_DLMWithTrendNormZeroWithSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDeltaDLMWithTrend(prior_R);
    predictSeason(prior_R);
}

static __inline__ void
predictPrior_DLMNoTrendNormCovNoSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
predictPrior_DLMWithTrendNormCovNoSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
predictPrior_DLMNoTrendNormCovWithSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDLMNoTrend(prior_R);
    predictSeason(prior_R);
}

static __inline__ void
predictPrior_DLMWithTrendNormCovWithSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDeltaDLMWithTrend(prior_R);
    predictSeason(prior_R);
}

static __inline__ void
predictPrior_DLMNoTrendRobustZeroNoSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDLMNoTrend(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_DLMWithTrendRobustZeroNoSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDeltaDLMWithTrend(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_DLMNoTrendRobustZeroWithSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDLMNoTrend(prior_R);
    predictSeason(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_DLMWithTrendRobustZeroWithSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDeltaDLMWithTrend(prior_R);
    predictSeason(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_DLMNoTrendRobustCovNoSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDLMNoTrend(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_DLMWithTrendRobustCovNoSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDeltaDLMWithTrend(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_DLMNoTrendRobustCovWithSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDLMNoTrend(prior_R);
    predictSeason(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_DLMWithTrendRobustCovWithSeasonPredict_i(SEXP prior_R) 
{
    predictAlphaDeltaDLMWithTrend(prior_R);
    predictSeason(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
predictPrior_KnownCertain_i(SEXP prior_R) 
{
    /* null op */
}

static __inline__ void
predictPrior_KnownUncertain_i(SEXP prior_R) 
{
    /* null op */
}


static __inline__ void
predictPrior_MixNormZero_i(SEXP prior_R) 
{
    
    predictLevelComponentWeightMix(prior_R);
    predictComponentWeightMix(prior_R);
    updateWeightMix(prior_R);
    predictIndexClassMix(prior_R);
    updateIndexClassMaxUsedMix(prior_R);
    updateAlphaMix(prior_R);
    
}

static __inline__ void
predictPrior_Zero_i(SEXP prior_R) 
{
    /* null op */
}


void
predictPrior(SEXP prior_R) 
{
    int i_method_prior = *(INTEGER(GET_SLOT(prior_R, iMethodPrior_sym)));
    
    switch(i_method_prior)
    {
        case 0:
            predictPrior_ExchFixed_i(prior_R);
            break;
        case 1:
            predictPrior_ExchNormZero_i(prior_R);
            break;
        case 2:
            predictPrior_ExchNormCov_i(prior_R);
            break;
        case 3:
            predictPrior_ExchRobustZero_i(prior_R);
            break;
        case 4:
            predictPrior_ExchRobustCov_i(prior_R);
            break;
        case 29:
            predictPrior_KnownCertain_i(prior_R);
            break;
        case 30:
            predictPrior_KnownUncertain_i(prior_R);
            break;
        case 40:
            predictPrior_Zero_i(prior_R);
            break;
        case 105:
            predictPrior_DLMNoTrendNormZeroNoSeasonPredict_i(prior_R);
            break;
        case 106:
            predictPrior_DLMNoTrendNormZeroWithSeasonPredict_i(prior_R);
            break;
        case 107:
            predictPrior_DLMNoTrendNormCovNoSeasonPredict_i(prior_R);
            break;
        case 108:
            predictPrior_DLMNoTrendNormCovWithSeasonPredict_i(prior_R);
            break;
        case 109:
            predictPrior_DLMNoTrendRobustZeroNoSeasonPredict_i(prior_R);
            break;
        case 110:
            predictPrior_DLMNoTrendRobustZeroWithSeasonPredict_i(prior_R);
            break;
        case 111:
            predictPrior_DLMNoTrendRobustCovNoSeasonPredict_i(prior_R);
            break;
        case 112:
            predictPrior_DLMNoTrendRobustCovWithSeasonPredict_i(prior_R);
            break;
        case 113:
            predictPrior_DLMWithTrendNormZeroNoSeasonPredict_i(prior_R);
            break;
        case 114:
            predictPrior_DLMWithTrendNormZeroWithSeasonPredict_i(prior_R);
            break;
        case 115:
            predictPrior_DLMWithTrendNormCovNoSeasonPredict_i(prior_R);
            break;
        case 116:
            predictPrior_DLMWithTrendNormCovWithSeasonPredict_i(prior_R);
            break;
        case 117:
            predictPrior_DLMWithTrendRobustZeroNoSeasonPredict_i(prior_R);
            break;
        case 118:
            predictPrior_DLMWithTrendRobustZeroWithSeasonPredict_i(prior_R);
            break;
        case 119:
            predictPrior_DLMWithTrendRobustCovNoSeasonPredict_i(prior_R);
            break;
        case 120:
            predictPrior_DLMWithTrendRobustCovWithSeasonPredict_i(prior_R);
            break;
        case 131:
            predictPrior_MixNormZero_i(prior_R);
            break;
        default:
            error("unknown i_method_prior for predictPrior: %d", i_method_prior);
            break;
    }
}

void
predictPrior_ExchFixed(SEXP prior_R) 
{
    predictPrior_ExchFixed_i(prior_R);
}

void
predictPrior_ExchNormZero(SEXP prior_R) 
{
    predictPrior_ExchNormZero_i(prior_R);
}

void
predictPrior_ExchNormCov(SEXP prior_R) 
{
    predictPrior_ExchNormCov_i(prior_R);
}

void
predictPrior_ExchRobustZero(SEXP prior_R) 
{
    predictPrior_ExchRobustZero_i(prior_R);
}

void
predictPrior_ExchRobustCov(SEXP prior_R) 
{
    predictPrior_ExchRobustCov_i(prior_R);
}

void
predictPrior_DLMNoTrendNormZeroNoSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMNoTrendNormZeroNoSeasonPredict_i(prior_R);
}

void
predictPrior_DLMWithTrendNormZeroNoSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMWithTrendNormZeroNoSeasonPredict_i(prior_R);
}

void
predictPrior_DLMNoTrendNormZeroWithSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMNoTrendNormZeroWithSeasonPredict_i(prior_R);
}

void
predictPrior_DLMWithTrendNormZeroWithSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMWithTrendNormZeroWithSeasonPredict_i(prior_R);
}

void
predictPrior_DLMNoTrendNormCovNoSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMNoTrendNormCovNoSeasonPredict_i(prior_R);
}

void
predictPrior_DLMWithTrendNormCovNoSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMWithTrendNormCovNoSeasonPredict_i(prior_R);
}


void
predictPrior_DLMNoTrendNormCovWithSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMNoTrendNormCovWithSeasonPredict_i(prior_R);
}

void
predictPrior_DLMWithTrendNormCovWithSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMWithTrendNormCovWithSeasonPredict_i(prior_R);
}

void
predictPrior_DLMNoTrendRobustZeroNoSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMNoTrendRobustZeroNoSeasonPredict_i(prior_R);
}

void
predictPrior_DLMWithTrendRobustZeroNoSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMWithTrendRobustZeroNoSeasonPredict_i(prior_R);
}

void
predictPrior_DLMNoTrendRobustZeroWithSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMNoTrendRobustZeroWithSeasonPredict_i(prior_R);
}

void
predictPrior_DLMWithTrendRobustZeroWithSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMWithTrendRobustZeroWithSeasonPredict_i(prior_R);
}

void
predictPrior_DLMNoTrendRobustCovNoSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMNoTrendRobustCovNoSeasonPredict_i(prior_R);
}

void
predictPrior_DLMWithTrendRobustCovNoSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMWithTrendRobustCovNoSeasonPredict_i(prior_R);
}

void
predictPrior_DLMNoTrendRobustCovWithSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMNoTrendRobustCovWithSeasonPredict_i(prior_R);
}

void
predictPrior_DLMWithTrendRobustCovWithSeasonPredict(SEXP prior_R) 
{
    predictPrior_DLMWithTrendRobustCovWithSeasonPredict_i(prior_R);
}

void
predictPrior_KnownCertain(SEXP prior_R) 
{
    predictPrior_KnownCertain_i(prior_R);
}

void
predictPrior_KnownUncertain(SEXP prior_R) 
{
    predictPrior_KnownUncertain_i(prior_R);
}

void
predictPrior_MixNormZero(SEXP prior_R) 
{
    predictPrior_MixNormZero_i(prior_R);
}

void
predictPrior_Zero(SEXP prior_R) 
{
    predictPrior_Zero_i(prior_R);
}

/* ******************************************************************************** */
/* Functions for drawing priors. ************************************************* */
/* ******************************************************************************** */

/* Note that these functions modify the priors in place, 
   unlike the R versions, or the R-visible C versions
   created in init.c. */

static __inline__ void
drawPrior_ExchFixed_i(SEXP prior_R) 
{
    /* null op */
}

static __inline__ void
drawPrior_ExchNormZero_i(SEXP prior_R) 
{
    drawTau(prior_R);
}

static __inline__ void
drawPrior_ExchNormCov_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
}

static __inline__ void
drawPrior_ExchRobustZero_i(SEXP prior_R) 
{
    drawTau(prior_R);
    predictUBeta(prior_R);
}

static __inline__ void
drawPrior_ExchRobustCov_i(SEXP prior_R) 
{
    drawTau(prior_R);
    predictUBeta(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
}

static __inline__ void
drawPrior_DLMNoTrendNormZeroNoSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawPhi(prior_R);
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
drawPrior_DLMWithTrendNormZeroNoSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaDelta(prior_R);
    drawPhi(prior_R);
    drawDelta0(prior_R);
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
drawPrior_DLMNoTrendNormZeroWithSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaSeason(prior_R);
    drawPhi(prior_R);
    predictSeason(prior_R);
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
drawPrior_DLMWithTrendNormZeroWithSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaDelta(prior_R);
    drawOmegaSeason(prior_R);
    drawPhi(prior_R);
    predictSeason(prior_R);
    drawDelta0(prior_R);
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
drawPrior_DLMNoTrendNormCovNoSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawPhi(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
drawPrior_DLMWithTrendNormCovNoSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaDelta(prior_R);
    drawPhi(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
    drawDelta0(prior_R);
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
drawPrior_DLMNoTrendNormCovWithSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaSeason(prior_R);
    drawPhi(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
    predictSeason(prior_R);
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
drawPrior_DLMWithTrendNormCovWithSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaDelta(prior_R);
    drawOmegaSeason(prior_R);
    drawPhi(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
    predictSeason(prior_R);
    drawDelta0(prior_R);
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
drawPrior_DLMNoTrendRobustZeroNoSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    predictUBeta(prior_R);
    drawPhi(prior_R);
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
drawPrior_DLMWithTrendRobustZeroNoSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaDelta(prior_R);
    predictUBeta(prior_R);
    drawPhi(prior_R);
    drawDelta0(prior_R);
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
drawPrior_DLMNoTrendRobustZeroWithSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaSeason(prior_R);
    predictUBeta(prior_R);
    drawPhi(prior_R);
    predictSeason(prior_R);
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
drawPrior_DLMWithTrendRobustZeroWithSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaDelta(prior_R);
    drawOmegaSeason(prior_R);
    predictUBeta(prior_R);
    drawPhi(prior_R);
    predictSeason(prior_R);
    drawDelta0(prior_R);
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
drawPrior_DLMNoTrendRobustCovNoSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    predictUBeta(prior_R);
    drawPhi(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
drawPrior_DLMWithTrendRobustCovNoSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaDelta(prior_R);
    predictUBeta(prior_R);
    drawPhi(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
    drawDelta0(prior_R);
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
drawPrior_DLMNoTrendRobustCovWithSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaSeason(prior_R);
    predictUBeta(prior_R);
    drawPhi(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
    predictSeason(prior_R);
    predictAlphaDLMNoTrend(prior_R);
}

static __inline__ void
drawPrior_DLMWithTrendRobustCovWithSeason_i(SEXP prior_R) 
{
    drawTau(prior_R);
    drawOmegaAlpha(prior_R);
    drawOmegaDelta(prior_R);
    drawOmegaSeason(prior_R);
    predictUBeta(prior_R);
    drawPhi(prior_R);
    drawUEtaCoef(prior_R);
    drawEta(prior_R);
    predictSeason(prior_R);
    drawDelta0(prior_R);
    predictAlphaDeltaDLMWithTrend(prior_R);
}

static __inline__ void
drawPrior_KnownCertain_i(SEXP prior_R) 
{
    /* null op */
}

static __inline__ void
drawPrior_KnownUncertain_i(SEXP prior_R) 
{
    /* null op */
}

#if(0)
/* John not finished yet */
static __inline__ void
drawPrior_MixNormZero_i(SEXP prior_R) 
{
    /* null op */
    
}
#endif

static __inline__ void
drawPrior_Zero_i(SEXP prior_R) 
{
    /* null op */
}


void
drawPrior(SEXP prior_R) 
{
    int i_method_prior = *(INTEGER(GET_SLOT(prior_R, iMethodPrior_sym)));
    
    switch(i_method_prior)
    {
        case 0:
            drawPrior_ExchFixed_i(prior_R);
            break;
        case 1:
            drawPrior_ExchNormZero_i(prior_R);
            break;
        case 2:
            drawPrior_ExchNormCov_i(prior_R);
            break;
        case 3:
            drawPrior_ExchRobustZero_i(prior_R);
            break;
        case 4:
            drawPrior_ExchRobustCov_i(prior_R);
            break;
        case 5:
            drawPrior_DLMNoTrendNormZeroNoSeason_i(prior_R);
            break;
        case 6:
            drawPrior_DLMNoTrendNormZeroWithSeason_i(prior_R);
            break;
        case 7:
            drawPrior_DLMNoTrendNormCovNoSeason_i(prior_R);
            break;
        case 8:
            drawPrior_DLMNoTrendNormCovWithSeason_i(prior_R);
            break;
                case 9:
            drawPrior_DLMNoTrendRobustZeroNoSeason_i(prior_R);
            break;
        case 10:
            drawPrior_DLMNoTrendRobustZeroWithSeason_i(prior_R);
            break;
        case 11:
            drawPrior_DLMNoTrendRobustCovNoSeason_i(prior_R);
            break;
        case 12:
            drawPrior_DLMNoTrendRobustCovWithSeason_i(prior_R);
            break;
        case 13:
            drawPrior_DLMWithTrendNormZeroNoSeason_i(prior_R);
            break;
        case 14:
            drawPrior_DLMWithTrendNormZeroWithSeason_i(prior_R);
            break;
        case 15:
            drawPrior_DLMWithTrendNormCovNoSeason_i(prior_R);
            break;
        case 16:
            drawPrior_DLMWithTrendNormCovWithSeason_i(prior_R);
            break;
        case 17:
            drawPrior_DLMWithTrendRobustZeroNoSeason_i(prior_R);
            break;
        case 18:
            drawPrior_DLMWithTrendRobustZeroWithSeason_i(prior_R);
            break;
        case 19:
            drawPrior_DLMWithTrendRobustCovNoSeason_i(prior_R);
            break;
        case 20:
            drawPrior_DLMWithTrendRobustCovWithSeason_i(prior_R);
            break;
        case 29:
            drawPrior_KnownCertain_i(prior_R);
            break;
        case 30:
            drawPrior_KnownUncertain_i(prior_R);
            break;
        case 40:
            drawPrior_Zero_i(prior_R);
            break;
        #if(0)
        case 131:
            drawPrior_MixNormZero_i(prior_R);
            break;
        #endif
        default:
            error("unknown i_method_prior for drawPrior: %d", i_method_prior);
            break;
    }
}

void
drawPrior_ExchFixed(SEXP prior_R) 
{
    drawPrior_ExchFixed_i(prior_R);
}

void
drawPrior_ExchNormZero(SEXP prior_R) 
{
    drawPrior_ExchNormZero_i(prior_R);
}

void
drawPrior_ExchNormCov(SEXP prior_R) 
{
    drawPrior_ExchNormCov_i(prior_R);
}

void
drawPrior_ExchRobustZero(SEXP prior_R) 
{
    drawPrior_ExchRobustZero_i(prior_R);
}

void
drawPrior_ExchRobustCov(SEXP prior_R) 
{
    drawPrior_ExchRobustCov_i(prior_R);
}

void
drawPrior_DLMNoTrendNormZeroNoSeason(SEXP prior_R) 
{
    drawPrior_DLMNoTrendNormZeroNoSeason_i(prior_R);
}

void
drawPrior_DLMWithTrendNormZeroNoSeason(SEXP prior_R) 
{
    drawPrior_DLMWithTrendNormZeroNoSeason_i(prior_R);
}

void
drawPrior_DLMNoTrendNormZeroWithSeason(SEXP prior_R) 
{
    drawPrior_DLMNoTrendNormZeroWithSeason_i(prior_R);
}

void
drawPrior_DLMWithTrendNormZeroWithSeason(SEXP prior_R) 
{
    drawPrior_DLMWithTrendNormZeroWithSeason_i(prior_R);
}

void
drawPrior_DLMNoTrendNormCovNoSeason(SEXP prior_R) 
{
    drawPrior_DLMNoTrendNormCovNoSeason_i(prior_R);
}

void
drawPrior_DLMWithTrendNormCovNoSeason(SEXP prior_R) 
{
    drawPrior_DLMWithTrendNormCovNoSeason_i(prior_R);
}


void
drawPrior_DLMNoTrendNormCovWithSeason(SEXP prior_R) 
{
    drawPrior_DLMNoTrendNormCovWithSeason_i(prior_R);
}

void
drawPrior_DLMWithTrendNormCovWithSeason(SEXP prior_R) 
{
    drawPrior_DLMWithTrendNormCovWithSeason_i(prior_R);
}

void
drawPrior_DLMNoTrendRobustZeroNoSeason(SEXP prior_R) 
{
    drawPrior_DLMNoTrendRobustZeroNoSeason_i(prior_R);
}

void
drawPrior_DLMWithTrendRobustZeroNoSeason(SEXP prior_R) 
{
    drawPrior_DLMWithTrendRobustZeroNoSeason_i(prior_R);
}

void
drawPrior_DLMNoTrendRobustZeroWithSeason(SEXP prior_R) 
{
    drawPrior_DLMNoTrendRobustZeroWithSeason_i(prior_R);
}

void
drawPrior_DLMWithTrendRobustZeroWithSeason(SEXP prior_R) 
{
    drawPrior_DLMWithTrendRobustZeroWithSeason_i(prior_R);
}

void
drawPrior_DLMNoTrendRobustCovNoSeason(SEXP prior_R) 
{
    drawPrior_DLMNoTrendRobustCovNoSeason_i(prior_R);
}

void
drawPrior_DLMWithTrendRobustCovNoSeason(SEXP prior_R) 
{
    drawPrior_DLMWithTrendRobustCovNoSeason_i(prior_R);
}

void
drawPrior_DLMNoTrendRobustCovWithSeason(SEXP prior_R) 
{
    drawPrior_DLMNoTrendRobustCovWithSeason_i(prior_R);
}

void
drawPrior_DLMWithTrendRobustCovWithSeason(SEXP prior_R) 
{
    drawPrior_DLMWithTrendRobustCovWithSeason_i(prior_R);
}

void
drawPrior_KnownCertain(SEXP prior_R) 
{
    drawPrior_KnownCertain_i(prior_R);
}

void
drawPrior_KnownUncertain(SEXP prior_R) 
{
    drawPrior_KnownUncertain_i(prior_R);
}

void
drawPrior_MixNormZero(SEXP prior_R) 
{
    #if(0)
    /* John not finished yet */
    drawPrior_MixNormZero_i(prior_R);
    #endif
}

void
drawPrior_Zero(SEXP prior_R) 
{
    drawPrior_Zero_i(prior_R);
}


/* ********************** transferParamPrior ******************* */

void
transferParamPrior_ExchNormZero(SEXP prior_R,  double *values, 
                     int nValues) 
{
    double val = values[0]; /* should have just one value in it */

    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, val);
}

void
transferParamPrior_ExchNormCov(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    
    /* first P values from values into eta */
    memcpy(eta, values, P*sizeof(double));
    
    /* (P+1)th value in values into tau */
    double val = values[P]; 

    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, val);
}

void
transferParamPrior_ExchRobustZero(SEXP prior_R,  double *values, 
                     int nValues) 
{
    double val = values[0];     /* should have just one value in it */

    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, val);
}


void
transferParamPrior_ExchRobustCov(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));

    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    
    /* first P values from values into eta */
    memcpy(eta, values, P*sizeof(double));
    
    /* (P+1)th (should be last) value in values into tau */
    double val = values[P]; 

    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, val);
}

void
transferParamPrior_DLMNoTrendNormZeroNoSeasonPredict(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* vector length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMWithTrendNormZeroNoSeasonPredict(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
    
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);
    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset;
    transferAlphaDelta0(delta, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaDelta */
    double od = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, od);

    ++offset;
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMNoTrendNormZeroWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* vector length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    
    SEXP s_R = GET_SLOT(prior_R, s_sym); /* list */
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
        
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    transferSeason0(s_R, nSeason, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L*nSeason; 
    /* (offset)th value in values into omegaSeason */
    double os = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, os);
    
    ++offset;
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMWithTrendNormZeroWithSeasonPredict(SEXP prior_R,
                    double *values, int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
    
    SEXP s_R = GET_SLOT(prior_R, s_sym); /* list */
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
        
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);
    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset;
    transferAlphaDelta0(delta, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaDelta */
    double od = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, od);

    ++offset;
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    transferSeason0(s_R, nSeason, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L*nSeason; 
    /* (offset)th value in values into omegaSeason */
    double os = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, os);

    ++offset; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMNoTrendNormCovNoSeasonPredict(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    
    /* vector length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    double *eta_src = values + offset - 1;
    
    /* P values from values[offset-1] onwards into eta */
    memcpy(eta, eta_src, P*sizeof(double));
    
    offset += P; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMWithTrendNormCovNoSeasonPredict(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
    
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset;
    transferAlphaDelta0(delta, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaDelta */
    double od = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, od);

    ++offset;
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    double *eta_src = values + offset - 1; 
    
    /* P values from values[offset-1] onwards into eta */
    memcpy(eta, eta_src, P*sizeof(double));
    
    offset += P; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMNoTrendNormCovWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    
    SEXP s_R = GET_SLOT(prior_R, s_sym); /* list */
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
        
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    transferSeason0(s_R, nSeason, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L*nSeason; 
    /* (offset)th value in values into omegaSeason */
    double os = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, os);

    ++offset; 
    
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    double *eta_src = values + offset - 1; 
    
    /* P values from values[offset-1] onwards into eta */
    memcpy(eta, eta_src, P*sizeof(double));
    
    offset += P; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMWithTrendNormCovWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
    
    SEXP s_R = GET_SLOT(prior_R, s_sym); /* list */
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
        
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 

    transferAlphaDelta0(delta, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaDelta */
    double od = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, od);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    transferSeason0(s_R, nSeason, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L*nSeason; 
    /* (offset)th value in values into omegaSeason */
    double os = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, os);

    ++offset; 
    
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    double *eta_src = values + offset - 1; 
    
    /* P values from values[offset-1] onwards into eta */
    memcpy(eta, eta_src, P*sizeof(double));
    
    offset += P; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMNoTrendRobustZeroNoSeasonPredict(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* vector length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMWithTrendRobustZeroNoSeasonPredict(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
    
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset;
    transferAlphaDelta0(delta, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaDelta */
    double od = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, od);

    ++offset;
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMNoTrendRobustZeroWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* vector length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    
    SEXP s_R = GET_SLOT(prior_R, s_sym); /* list */
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
        
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    transferSeason0(s_R, nSeason, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L*nSeason; 
    /* (offset)th value in values into omegaSeason */
    double os = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, os);
    
    ++offset;
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMWithTrendRobustZeroWithSeasonPredict(SEXP prior_R,
                    double *values, int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
    
    SEXP s_R = GET_SLOT(prior_R, s_sym); /* list */
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
        
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);
    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset;
    transferAlphaDelta0(delta, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaDelta */
    double od = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, od);

    ++offset;
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    transferSeason0(s_R, nSeason, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L*nSeason; 
    /* (offset)th value in values into omegaSeason */
    double os = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, os);

    ++offset; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}


void
transferParamPrior_DLMNoTrendRobustCovNoSeasonPredict(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    
    /* vector length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    double *eta_src = values + offset - 1;
    
    /* P values from values[offset-1] onwards into eta */
    memcpy(eta, eta_src, P*sizeof(double));
    
    offset += P; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMWithTrendRobustCovNoSeasonPredict(SEXP prior_R,  double *values, 
                     int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
    
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset;
    transferAlphaDelta0(delta, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaDelta */
    double od = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, od);

    ++offset;
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    double *eta_src = values + offset - 1; 
    
    /* P values from values[offset-1] onwards into eta */
    memcpy(eta, eta_src, P*sizeof(double));
    
    offset += P; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMNoTrendRobustCovWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    
    SEXP s_R = GET_SLOT(prior_R, s_sym); /* list */
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
        
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    transferSeason0(s_R, nSeason, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L*nSeason; 
    /* (offset)th value in values into omegaSeason */
    double os = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, os);

    ++offset; 
    
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    double *eta_src = values + offset - 1; 
    
    /* P values from values[offset-1] onwards into eta */
    memcpy(eta, eta_src, P*sizeof(double));
    
    offset += P; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}

void
transferParamPrior_DLMWithTrendRobustCovWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues) 
{
    int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    
    /* vectors length (K+1)L */
    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym));
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
    
    SEXP s_R = GET_SLOT(prior_R, s_sym); /* list */
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));
        
    SEXP iteratorNew_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iteratorOld_R = GET_SLOT(prior_R, iteratorStateOld_sym);
    
    int K_old = J_old/L; /* integer division */
    
    int offset = 1;
    
    transferAlphaDelta0(alpha, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaAlpha */
    double oa = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, oa);

    ++offset; 

    transferAlphaDelta0(delta, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L;
    /* (offset)th value in values into omegaDelta */
    double od = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, od);

    ++offset; 
    /* (offset)th value in values into phi */
    double p = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, phi_sym, p);

    ++offset; 
    transferSeason0(s_R, nSeason, values, offset,
                    iteratorNew_R, iteratorOld_R);

    offset += (K_old + 1)*L*nSeason; 
    /* (offset)th value in values into omegaSeason */
    double os = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, os);

    ++offset; 
    
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));
    double *eta_src = values + offset - 1; 
    
    /* P values from values[offset-1] onwards into eta */
    memcpy(eta, eta_src, P*sizeof(double));
    
    offset += P; 
    /* (offset)th value in values into tau */
    double t = values[offset - 1]; 
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, t);
}


void
transferParamPrior_MixNormZeroPredict(SEXP prior_R,
                        double *values, int nValues) 
{
    int *dimBetaOld = INTEGER(GET_SLOT(prior_R, dimBetaOld_sym));
    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));  
    int iAlong_c = iAlong_r -1;
    
    int indexClassMax = *INTEGER(GET_SLOT(prior_R, indexClassMaxMix_sym));
    int nBetaNoAlong = *INTEGER(GET_SLOT(prior_R, nBetaNoAlongMix_sym));
    /* int J_old = *INTEGER(GET_SLOT(prior_R, JOld_sym)); DELETED BY JB 17 FEB 17*/ 
    
    double *prodVectors = REAL(GET_SLOT(prior_R, prodVectorsMix_sym));
    double *levelComponentWeightOld = REAL(GET_SLOT(prior_R,
                                            levelComponentWeightOldMix_sym));
    
    int nAlongOld = dimBetaOld[iAlong_c];
    int nAlongOldTimesIndexClassMax = nAlongOld * indexClassMax;
    int offset = 1;
    
    /* offset += J_old; /\* alphaMix *\/ DELETED BY JB 17 FEB 17*/
    
    int nProd = nBetaNoAlong * indexClassMax;
    /* prodVectorsMix */
    memcpy(prodVectors, values + offset - 1, nProd * sizeof(double));
    offset += nProd;
    
    /* omegaVectorsMix */
    SET_DOUBLESCALE_SLOT(prior_R, omegaVectorsMix_sym, values[offset -1]);
    ++offset;
    
    offset += nAlongOldTimesIndexClassMax; /* weightMix */
    offset += nAlongOldTimesIndexClassMax; /* componentWeightMix */
    
    /* omegaComponentWeightMix */
    SET_DOUBLESCALE_SLOT(prior_R, omegaComponentWeightMix_sym, values[offset -1]);
    ++offset;
    
    /* levelComponentWeightOldMix */              
    transferLevelComponentWeightOldMix(levelComponentWeightOld, values, offset,
                                            nAlongOld, indexClassMax);  
    offset += nAlongOldTimesIndexClassMax;
    
    /* meanLevelComponentWeightMix */
    SET_DOUBLESCALE_SLOT(prior_R, meanLevelComponentWeightMix_sym, 
                                            values[offset -1]);
    ++offset;
    
    /* phiMix */
    SET_DOUBLESCALE_SLOT(prior_R, phiMix_sym, 
                                            values[offset -1]);
    ++offset;
    
    /* omegaLevelComponentWeightMix */
    SET_DOUBLESCALE_SLOT(prior_R, omegaLevelComponentWeightMix_sym, 
                                            values[offset -1]);
    ++offset;
    
    /* tau */
    SET_DOUBLESCALE_SLOT(prior_R, tau_sym, values[offset -1]);
}


void
transferParamPrior(SEXP prior_R, double *values, int nValues) 
{
    int i_method_prior = *(INTEGER(GET_SLOT(prior_R, iMethodPrior_sym)));
    
    switch(i_method_prior)
    {
        case 1:
            transferParamPrior_ExchNormZero(prior_R, values, nValues);
            break;
        case 2: 
            transferParamPrior_ExchNormCov(prior_R, values, nValues);
            break;
        case 3:
            transferParamPrior_ExchRobustZero(prior_R, values, nValues);
            break;
        case 4:
            transferParamPrior_ExchRobustCov(prior_R, values, nValues);
            break;
        case 105:
            transferParamPrior_DLMNoTrendNormZeroNoSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 106:
            transferParamPrior_DLMNoTrendNormZeroWithSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 107:
            transferParamPrior_DLMNoTrendNormCovNoSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 108:
            transferParamPrior_DLMNoTrendNormCovWithSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 109:
            transferParamPrior_DLMNoTrendRobustZeroNoSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 110:
            transferParamPrior_DLMNoTrendRobustZeroWithSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 111:
            transferParamPrior_DLMNoTrendRobustCovNoSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 112:
            transferParamPrior_DLMNoTrendRobustCovWithSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 113:
            transferParamPrior_DLMWithTrendNormZeroNoSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 114:
            transferParamPrior_DLMWithTrendNormZeroWithSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 115:
            transferParamPrior_DLMWithTrendNormCovNoSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 116:
            transferParamPrior_DLMWithTrendNormCovWithSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 117:
            transferParamPrior_DLMWithTrendRobustZeroNoSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 118:
            transferParamPrior_DLMWithTrendRobustZeroWithSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 119:
            transferParamPrior_DLMWithTrendRobustCovNoSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 120:
            transferParamPrior_DLMWithTrendRobustCovWithSeasonPredict(prior_R,
                                                    values, nValues);
            break;
        case 131:
            transferParamPrior_MixNormZeroPredict(prior_R,
                                                    values, nValues);
            break;
            
        default:
            error("unknown i_method_prior for transferParamPrior: %d", i_method_prior);
            break;
    }
}
