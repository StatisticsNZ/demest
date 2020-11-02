
//#include "iterators-methods.h"
#include "model-methods.h"
#include "helper-functions.h"
#include "demest.h"

//#include "R_ext/BLAS.h"
/* for BLAS level 2 documention see www.netlib.org/blas/blas2-paper.ps */

//#include "R_ext/Lapack.h"

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


/* File "helper-simulate.c" contains C versions of
 * functions from "helper-simulate.R".   */

void
drawAlphaLN2(SEXP object_R)
{
    SEXP alpha_R = GET_SLOT(object_R, alphaLN2_sym);
    double * alpha = REAL(alpha_R);
    int n_alphas = LENGTH(alpha_R);

    int * constraint = INTEGER(GET_SLOT(object_R, constraintLN2_sym));
    double sigma = *REAL(GET_SLOT(object_R, sigma_sym));

    for (int j = 0; j < n_alphas; ++j) {

        int constraint_j = constraint[j];

        if ((constraint_j == NA_INTEGER) ||(constraint_j != 0)) {
            double x = rnorm(0, sigma);

            if (constraint_j == NA_INTEGER) {
                alpha[j] = x;
            }
            else if (constraint_j == -1) {
                alpha[j] = (-1) * fabs(x);
            }
            else if (constraint_j == 1) {
                alpha[j] = fabs(x);
            }
            else {
                error("invalid value for 'constraint'");
            }
        }
    }
}

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
        SEXP seriesCollapsed_R;

        int nProtect  = 0;
        int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
        const char *class_name = CHAR(STRING_ELT(GET_SLOT((model_R), R_ClassSymbol), 0));
        int contains_pois = strstr(class_name, "Poisson") != NULL;
        int contains_poisbin = strstr(class_name, "PoissonBinomial") != NULL;
        int contains_cmp = strstr(class_name, "CMP") != NULL;
        int contains_normal = strstr(class_name, "Normal") != NULL;
        int need_to_coerce = (contains_pois && !contains_poisbin) || contains_cmp || contains_normal;
        if (need_to_coerce) {
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
