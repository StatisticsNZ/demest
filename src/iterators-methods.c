
#include "demest.h"

/* File "iterators-methods.c" contains C versions of functions 
 * from "iterators-methods.R". */

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


/* *************************************************************************** */
/*** Along iterators ********************************************************* */
/* *************************************************************************** */


/* advance random walk iterator one 'step' 
 * and if advanced from final position, return to initial position*/
void
advanceA(SEXP iterator_R)
{
    /* iWithin changed in-place */
    int *iWithin = INTEGER(GET_SLOT(iterator_R, iWithin_sym));
    /* nWithin unchanged so treat by value */
    int nWithin = *(INTEGER(GET_SLOT(iterator_R, nWithin_sym)));
    
    SEXP indices_R = GET_SLOT(iterator_R, indices_sym);
    int indices_len =  LENGTH(indices_R);
    int *indices = INTEGER(indices_R);
    
    if (*iWithin < nWithin) {
        *iWithin += 1;
        for (int i = 0; i < indices_len; ++i) {
            indices[i] += 1;
        }
    }
    else { /* assume iWithin == nWithin */
        /* iBetween changed in-place */
        int *iBetween = INTEGER(GET_SLOT(iterator_R, iBetween_sym));
        /* nBetween unchanged so treat by value */
        int nBetween = *(INTEGER(GET_SLOT(iterator_R, nBetween_sym)));
        
        *iWithin = 1;
        if (*iBetween < nBetween) {
            *iBetween += 1;
            int incrementBetween = 
                *(INTEGER(GET_SLOT(iterator_R, incrementBetween_sym)));
            for (int i = 0; i < indices_len; ++i) {
                indices[i] += incrementBetween;
            }
        }
        else {
            *iBetween = 1;
            
            int *initial = INTEGER(GET_SLOT(iterator_R, initial_sym));
            memcpy( indices, initial, indices_len * sizeof(int) );
        }
    }
}



/* reset along iterator */
void
resetA(SEXP iterator_R)
{
    int *iWithin = INTEGER(GET_SLOT(iterator_R, iWithin_sym));
    int *iBetween = INTEGER(GET_SLOT(iterator_R, iBetween_sym));
    SEXP indices_R = GET_SLOT(iterator_R, indices_sym);
    int indices_len =  LENGTH(indices_R);
    int *indices = INTEGER(indices_R);
    int *initial = INTEGER(GET_SLOT(iterator_R, initial_sym));
    
    /* object@iWithin <- 1L; object@iBetween <- 1L */
    *iWithin = 1;
    *iBetween = 1;
    
    /* object@indices <- object@initial */
    /* fast copy from initial to indices provided both same size */
    memcpy( indices, initial, indices_len * sizeof(int) );
    
}



/* *************************************************************************** */
/*** Beta iterators ********************************************************* */
/* *************************************************************************** */

/* advance beta iterator one 'step' */
void
advanceB(SEXP iterator_R)
{
    SEXP indices_R = GET_SLOT(iterator_R, indices_sym);
    int n_beta =  LENGTH(indices_R);
    
    if (n_beta > 1) {
        
        int *indices = INTEGER(indices_R);
        
        /* dimIterators is a list of dimIterators */
        SEXP dim_iterators_R = GET_SLOT(iterator_R, dimIterators_sym); /*VECSXP */  
        
        /* strideLengths is a list containing an integer vector 
         * for each beta other than the intercept.   */
        SEXP stride_lengths_R = GET_SLOT(iterator_R, strideLengths_sym); /*VECSXP */  
        
        int nDimIterators =  LENGTH(dim_iterators_R);
        
        /* make space to store iterator strides */
        int dimIteratorStrides[nDimIterators];
        
        for (int d = 0; d < nDimIterators; ++d) {
            SEXP dIterator_R = VECTOR_ELT(dim_iterators_R, d);
            advanceD(dIterator_R);
            /* and get the nStride at the same time */
            dimIteratorStrides[d] 
                    = *(INTEGER(GET_SLOT(dIterator_R, nStrides_sym)));
        }
        
        for (int b = 1; b < n_beta; ++b) {
            
            int *this_vec = INTEGER(VECTOR_ELT(stride_lengths_R, b-1));
            
            for (int d = 0; d < nDimIterators; ++d) {
                int n_strides = dimIteratorStrides[d];
                int stride_length = this_vec[d];
                indices[b] += n_strides * stride_length;
            }
        }
    }
}



/* reset beta iterator */
void
resetB(SEXP iterator_R)
{
    SEXP indices_R = GET_SLOT(iterator_R, indices_sym);
    int n_beta =  LENGTH(indices_R);
    
    if(n_beta > 1) {
        
        int *indices = INTEGER(indices_R);
        
        for (int b = 1; b < n_beta; ++b) {
            indices[b] = 1;
        }
        
        /* dimIterators is a list of dimIterators */
        SEXP dim_iterators_R = GET_SLOT(iterator_R, dimIterators_sym); /*VECSXP */  
        int nDimIterators =  LENGTH(dim_iterators_R);
        
        /* for (d in seq_along(object@dimIterators))
            object@dimIterators[[d]] <- resetD(object@dimIterators[[d]]) */
        for (int d = 0; d < nDimIterators; ++d) {
            resetD(VECTOR_ELT(dim_iterators_R, d));
        
        }
    }
}




/* *************************************************************************** */
/*** Dim iterators ********************************************************* */
/* *************************************************************************** */



/* advance dim iterator */
void
advanceD(SEXP iterator_R)
{
    /* iWithin and nStrides changed in-place */
    int *iWithin = INTEGER(GET_SLOT(iterator_R, iWithin_sym));
    int *nStrides = INTEGER(GET_SLOT(iterator_R, nStrides_sym));
    /* nWithin by value */
    int nWithin = *(INTEGER(GET_SLOT(iterator_R, nWithin_sym)));
    
    if (*iWithin < nWithin) {
        *iWithin += 1;
        *nStrides = 0;
        
    }
    else { 
        
        int *iBetween = INTEGER(GET_SLOT(iterator_R, iBetween_sym));
        int nBetween = *(INTEGER(GET_SLOT(iterator_R, nBetween_sym)));
        
        *iWithin = 1;
        
        if (*iBetween < nBetween) {
            *iBetween += 1;
            *nStrides = 1;
        }
        else {
            *iBetween = 1;
            *nStrides = 1 - nBetween;
            
        }
    }
}



void
resetD(SEXP iterator_R)
{
    int *nStrides = INTEGER(GET_SLOT(iterator_R, nStrides_sym));
    int *iWithin = INTEGER(GET_SLOT(iterator_R, iWithin_sym));
    int *iBetween = INTEGER(GET_SLOT(iterator_R, iBetween_sym));
    int nBetween = *INTEGER(GET_SLOT(iterator_R, nBetween_sym));
   
    *nStrides = 1 - nBetween;
    *iWithin = 1;
    *iBetween = 1;
}


/* ********* CohortIterators ****************** */

/* advance cohort iterator */
void
advanceCA(SEXP iterator_R)
{
    int i = *INTEGER(GET_SLOT(iterator_R, i_sym));
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
    int * iTime_ptr = INTEGER(GET_SLOT(iterator_R, iTime_sym));
    int * iAge_ptr = INTEGER(GET_SLOT(iterator_R, iAge_sym));
    int iTime = *iTime_ptr;
    int iAge = *iAge_ptr;
    
    ++iTime;
    i += stepTime;    
        
    if (iAge < nAge) {
      ++iAge;
      i += stepAge;
    }

    *iAge_ptr = iAge;
    *iTime_ptr = iTime;
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    *i_ptr = i;
    
    SEXP finished_R = GET_SLOT(iterator_R, finished_sym);
    LOGICAL(finished_R)[0] = (iTime >= nTime);

}

/* advance cohort iterator */
void
advanceCP(SEXP iterator_R)
{
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    int i = *i_ptr;
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int * iTime_ptr = INTEGER(GET_SLOT(iterator_R, iTime_sym));
    int iTime = *iTime_ptr;
    int hasAge = *LOGICAL(GET_SLOT(iterator_R, hasAge_sym));
   
    ++iTime;
    i += stepTime;
    
        
    if (hasAge) {
        int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
        int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
        int * iAge_ptr = INTEGER(GET_SLOT(iterator_R, iAge_sym));
        int iAge = *iAge_ptr;
        
        if (iAge < nAge) {
            ++iAge;
            i += stepAge;
        }
        *iAge_ptr = iAge;
    }
    
    *i_ptr = i;
    *iTime_ptr = iTime;
    
    SEXP finished_R = GET_SLOT(iterator_R, finished_sym);
    LOGICAL(finished_R)[0] = (iTime >= nTime);
}


void
advanceCC(SEXP iterator_R)
{
  int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
  int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
  int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
  int * iTime_ptr = INTEGER(GET_SLOT(iterator_R, iTime_sym));
  int hasAge = *LOGICAL(GET_SLOT(iterator_R, hasAge_sym));
  int i = *i_ptr;
  int iTime = *iTime_ptr;
  int finished = 0;
  if (hasAge) {
    int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
    int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
    int lastAgeGroupOpen = *INTEGER(GET_SLOT(iterator_R, lastAgeGroupOpen_sym));
    int stepTriangle = *INTEGER(GET_SLOT(iterator_R, stepTriangle_sym));
    int * iAge_ptr = INTEGER(GET_SLOT(iterator_R, iAge_sym));
    int * iTriangle_ptr = INTEGER(GET_SLOT(iterator_R, iTriangle_sym));
    int iAge = *iAge_ptr;
    int iTriangle = *iTriangle_ptr;
    int isLowerBefore = iTriangle == 1;
    int isOldestAgeBefore = iAge == nAge;
    if (isLowerBefore) {
      ++iTime;
      i += stepTime;
      ++iTriangle;
      i += stepTriangle;
      if (lastAgeGroupOpen)
	finished = (iTime == nTime) && isOldestAgeBefore;
      else
	finished = isOldestAgeBefore;
    }
    else {
      if (isOldestAgeBefore) {
	++iTime;
	i += stepTime;
      }
      else {
	++iAge;
        i += stepAge;
        --iTriangle;
        i -= stepTriangle;
      }
      finished = (iTime == nTime);
    }
    *iAge_ptr = iAge;
    *iTriangle_ptr = iTriangle;
  }
  else {
    ++iTime;
    i += stepTime;
    finished = (iTime == nTime);
  }   
  *i_ptr = i;
  *iTime_ptr = iTime;
  SEXP finished_R = GET_SLOT(iterator_R, finished_sym);
  LOGICAL(finished_R)[0] = (finished != 0);
}


/* advance CODPCP iterator */
void
advanceCODPCP(SEXP iterator_R)
{
    advanceCC(iterator_R);
    int iter_i = *INTEGER(GET_SLOT(iterator_R, i_sym));
    int *iVec = INTEGER(GET_SLOT(iterator_R, iVec_sym));
    int length = *INTEGER(GET_SLOT(iterator_R, lengthVec_sym));
    int *increment = INTEGER(GET_SLOT(iterator_R, increment_sym));
    
    for(int j = 0; j < length; ++j) {
        
        iVec[j] = iter_i + increment[j];
    
    }

}

/* reset cohort iterator */
void
resetCA(SEXP iterator_R, int i)
{
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));    

    int iTime_R = (((i - 1) / stepTime) % nTime) + 1; /* R-style */
    int iAge_R = (((i - 1) / stepAge) % nAge) + 1; /* R-style */

    int * iTime_ptr = INTEGER(GET_SLOT(iterator_R, iTime_sym));
    int * iAge_ptr = INTEGER(GET_SLOT(iterator_R, iAge_sym));
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
        
    *iTime_ptr = iTime_R;
    *iAge_ptr = iAge_R;
    *i_ptr = i;
    
    SEXP finished_R = GET_SLOT(iterator_R, finished_sym);
    LOGICAL(finished_R)[0] = (iTime_R >= nTime);
}

/* reset cohort iterator */
void
resetCP(SEXP iterator_R, int i)
{
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int hasAge = *LOGICAL(GET_SLOT(iterator_R, hasAge_sym));
    
    int iTime_R = (((i - 1) / stepTime) % nTime) + 1; /* R-style */
    
    if (hasAge) {
        int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
        int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
        int iAge_R = (((i - 1) / stepAge) % nAge) + 1; /* R-style */
        
        int * iAge_ptr = INTEGER(GET_SLOT(iterator_R, iAge_sym));
        *iAge_ptr = iAge_R;
    }
    
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    int * iTime_ptr = INTEGER(GET_SLOT(iterator_R, iTime_sym));
        
    *i_ptr = i;
    *iTime_ptr = iTime_R;
    
    SEXP finished_R = GET_SLOT(iterator_R, finished_sym);
    LOGICAL(finished_R)[0] = (iTime_R >= nTime);
}


void
resetCC(SEXP iterator_R, int i)
{
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int hasAge = *LOGICAL(GET_SLOT(iterator_R, hasAge_sym));
    
    int iTime_R = ((i - 1)/stepTime) % nTime  + 1;

    int finished = 0;
    
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    int * iTime_ptr = INTEGER(GET_SLOT(iterator_R, iTime_sym));
        
    *i_ptr = i;
    *iTime_ptr = iTime_R;
    
    if (hasAge) {
    
        int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
        int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
        int lastAgeGroupOpen = *INTEGER(GET_SLOT(iterator_R, lastAgeGroupOpen_sym));
        int stepTriangle = *INTEGER(GET_SLOT(iterator_R, stepTriangle_sym));
        int iAge_R = (((i - 1) / stepAge) % nAge) + 1; /* R-style */
        int iTriangle_R = (((i - 1) / stepTriangle) % 2) + 1; /* R-style */
        
        int * iAge_ptr = INTEGER(GET_SLOT(iterator_R, iAge_sym));
        int * iTriangle_ptr = INTEGER(GET_SLOT(iterator_R, iTriangle_sym));
        
        *iAge_ptr = iAge_R;
        *iTriangle_ptr = iTriangle_R;

        if (iTriangle_R == 1) {
	  finished = (iTime_R == nTime);
        }
        else {
	  if (lastAgeGroupOpen)
            finished = (iTime_R == nTime) && (iAge_R == nAge);
	  else
            finished = (iAge_R == nAge);
        }
    }
    else {
        finished = (iTime_R == nTime);
    }
    
    SEXP finished_R = GET_SLOT(iterator_R, finished_sym);
    LOGICAL(finished_R)[0] = (finished != 0);
}

void
resetCODPCP(SEXP iterator_R, int i)
{
    
    resetCC(iterator_R, i);
    
    int iter_i = *INTEGER(GET_SLOT(iterator_R, i_sym));
    int length = *INTEGER(GET_SLOT(iterator_R, lengthVec_sym));
    
    int *iVec = INTEGER(GET_SLOT(iterator_R, iVec_sym));
    int *increment = INTEGER(GET_SLOT(iterator_R, increment_sym));
    
    for(int j = 0; j < length; ++j) {
        
        iVec[j] = iter_i + increment[j];
    
    }
}


/* ************** Margin iterators *************** */

/* advance margin iterator */
/* dimIterators is a list, indices is an integer vector */
void
advanceM(SEXP object_R)
{
    SEXP iterators_R = GET_SLOT(object_R, dimIterators_sym);
    int n_its = LENGTH(iterators_R);
    
    int *indices = INTEGER(GET_SLOT(object_R, indices_sym));
    
    for (int i = 0; i < n_its; ++i) {
        SEXP iterator_R = VECTOR_ELT(iterators_R, i);
        advanceD(iterator_R);
        indices[i] = *INTEGER(GET_SLOT(iterator_R, iBetween_sym));
    }
     
}

/* reset margin iterator */
/* dimIterators is a list, indices is an integer vector */
void
resetM(SEXP object_R)
{
    SEXP iterators_R = GET_SLOT(object_R, dimIterators_sym);
    int n_its = LENGTH(iterators_R);
    
    int *indices = INTEGER(GET_SLOT(object_R, indices_sym));
    
    for (int i = 0; i < n_its; ++i) {
        SEXP iterator_R = VECTOR_ELT(iterators_R, i);
        indices[i] = 1;
        
        resetD(iterator_R);
        
    }
     
}

/* ********************* slice iterators ********************** */

void
advanceS(SEXP object_R)
{
    int posDim = *INTEGER(GET_SLOT(object_R, posDim_sym));
    int lengthDim = *INTEGER(GET_SLOT(object_R, lengthDim_sym));
    int increment = *INTEGER(GET_SLOT(object_R, increment_sym));
    
    SEXP indices_R = GET_SLOT(object_R, indices_sym);
    int nIndices = LENGTH(indices_R);
    int *indices = INTEGER(indices_R);
    
    if (posDim < lengthDim) {
        for (int i = 0; i < nIndices; ++i) {
            indices[i] += increment;
        }
        posDim += 1;
    }
    else {
        int combinedIncrement = (lengthDim - 1) * increment;
        for (int i = 0; i < nIndices; ++i) {
            indices[i] -= combinedIncrement;
        }
        posDim = 1;
    }
    SET_INTSCALE_SLOT(object_R, posDim_sym, posDim)    
}

void
resetS(SEXP object_R)
{
    int posDim = *INTEGER(GET_SLOT(object_R, posDim_sym));
    int increment = *INTEGER(GET_SLOT(object_R, increment_sym));
    
    SEXP indices_R = GET_SLOT(object_R, indices_sym);
    int nIndices = LENGTH(indices_R);
    int *indices = INTEGER(indices_R);
    
    int combinedIncrement = (posDim - 1) * increment;
    for (int i = 0; i < nIndices; ++i) {
        indices[i] -= combinedIncrement;
    }
    posDim = 1;
    SET_INTSCALE_SLOT(object_R, posDim_sym, posDim)    
}
