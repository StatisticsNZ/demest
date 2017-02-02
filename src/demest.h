
#ifndef __DEMEST_H__
#define __DEMEST_H__

#ifndef __GNUC__
#define __inline__ inline
#endif


#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

//#define DEBUGGING
//#define DEBUGGING_EXTRA
//#define DEBUGNANS /* debugging NaNs */
//#define DEBUGFILEREAD 

#define USE_LOG 1 /* macro for indicator to use log for dbinom etc */
#define NOT_USE_LOG 0 /* macro for indicator to use log for dbinom etc */

#define SET_DOUBLESCALE_SLOT(object, slotname, value) { \
    double *ptr = REAL(GET_SLOT(object, slotname));     \
    *ptr = (value);}
    
#define SET_INTSCALE_SLOT(object, slotname, value) {    \
    int *ptr = INTEGER(GET_SLOT(object, slotname));     \
    *ptr = (value);}


/* everything in "x_sym" form here must be macro defined in init.c */
SEXP
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
  priorsBetas_sym, 
  theta_sym,
  mu_sym,
  sigma_sym,
  sigmaMax_sym,
  ASigma_sym,
  nuSigma_sym,
  varsigma_sym,
  varsigmaMax_sym,
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
  
  tolerance_sym,
  betaIsPredicted_sym,
  nFailedPropTheta_sym,
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
  observation_sym,
  datasets_sym,
  transforms_sym,
  
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
  /* mappings */
  isOneToOne_sym,
  nSharedVec_sym,
  stepSharedCurrentVec_sym,
  stepSharedTargetVec_sym,
  nTimeCurrent_sym,
  stepTimeCurrent_sym,
  stepTimeTarget_sym,
  stepAgeCurrent_sym,
  stepAgeTarget_sym,
  stepTriangleCurrent_sym,
  nOrigDestVec_sym,
  stepOrigCurrentVec_sym,
  stepDestCurrentVec_sym,
  stepOrigDestTargetVec_sym,
  
  /*new priors*/
  ATau_sym,
  nuTau_sym,
  hasAlphaMove_sym,
  hasAlphaDLM_sym,
  hasAlphaICAR_sym,
  hasCovariates_sym,
  hasSeason_sym,
  alphaCross_sym,
  alphaDLM_sym,
  alphaICAR_sym,
  indicesCross_sym,
  iteratorState_sym,
  iteratorStateOld_sym,
  K_sym,
  L_sym,
  s_sym,
  UBeta_sym,
  nuBeta_sym,
  isRobust_sym,
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
  
  WSqrt_sym,
  WSqrtInvG_sym,
  exposureAg_sym,
  P_sym,
  AEtaIntercept_sym,
  AEtaCoef_sym,
  nuEtaCoef_sym,
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
  JOld_sym;
  
  
/* Priors-methods */

void updateBeta(double *beta, int J, SEXP prior, double *vbar, 
                int n, double sigma);

void updateGWithTrend(SEXP prior_R);
void updateOmegaAlpha(SEXP prior_R, int isWithTrend);
void updateOmegaDelta(SEXP prior_R);
void updateOmegaSeason(SEXP prior_R);
void updatePhi(SEXP prior_R, int isWithTrend);
void updateUEtaCoef(SEXP prior_R);
void updateWSqrt(SEXP prior_R);
void updateWSqrtInvG(SEXP prior_R);
void updateTauNorm(SEXP prior_R, double *betaScaled, int J);
void updateTauRobust(SEXP prior_R, int J);

void updateUBeta(SEXP prior_R, double *beta, int J);

void
updateBetaAndPriorBeta(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_ExchFixed(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_ExchNormZero(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_ExchNormCov(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_ExchRobustZero(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_ExchRobustCov(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMNoTrendNormZeroNoSeason(double *beta, int J, 
                    SEXP prior_R, double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMWithTrendNormZeroNoSeason(double *beta, int J, 
                    SEXP prior_R, double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMNoTrendNormZeroWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMWithTrendNormZeroWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMNoTrendNormCovNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMWithTrendNormCovNoSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMNoTrendNormCovWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMWithTrendNormCovWithSeason(double *beta, int J, SEXP prior_R, 
                        double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMNoTrendRobustZeroNoSeason(double *beta, int J, 
                    SEXP prior_R, double *vbar, int n, double sigma);                   
void
updateBetaAndPriorBeta_DLMWithTrendRobustZeroNoSeason(double *beta, int J, 
                    SEXP prior_R, double *vbar, int n, double sigma);       
void
updateBetaAndPriorBeta_DLMNoTrendRobustZeroWithSeason(double *beta, int J, 
                    SEXP prior_R, double *vbar, int n, double sigma);                   
void
updateBetaAndPriorBeta_DLMWithTrendRobustZeroWithSeason(double *beta, int J,
                    SEXP prior_R, double *vbar, int n, double sigma);
void
updateBetaAndPriorBeta_DLMNoTrendRobustCovNoSeason(double *beta, int J, 
                    SEXP prior_R, double *vbar, int n, double sigma);                   
void
updateBetaAndPriorBeta_DLMWithTrendRobustCovNoSeason(double *beta, int J, 
                    SEXP prior_R, double *vbar, int n, double sigma);       
void
updateBetaAndPriorBeta_DLMNoTrendRobustCovWithSeason(double *beta, int J, 
                    SEXP prior_R, double *vbar, int n, double sigma);                   
void
updateBetaAndPriorBeta_DLMWithTrendRobustCovWithSeason(double *beta, int J,
                    SEXP prior_R, double *vbar, int n, double sigma);
                    
/* helper-functions */
SEXP makeMu(int n, SEXP betas_R, SEXP iterator_R);
double dpoibin1(int x, int size, double prob, int use_log);
double invlogit1(double x);
int rcateg1(double* cumProb);
double rinvchisq1(double df, double scale);

SEXP rmvnorm1(SEXP mean_R, SEXP var_R);
SEXP rmvnorm2(SEXP mean_R, SEXP var_R);
SEXP rnormTruncated(int n, SEXP mean_R, SEXP sd_R, 
                double lower, double upper, double tolerance,
                int maxAttempt,
                int uniform);
double rtnorm1(double mean, double sd, double lower, double upper);
int rpoisTrunc1(double lambda, int lower, int upper, int maxAttempt);
                
void betaHat(double *beta_hat, SEXP prior_R, int J);
void betaHatAlphaDLM(double *beta_hat, SEXP prior_R, int J);
void betaHatCovariates(double *beta_hat, SEXP prior_R, int J);
void betaHatSeason(double *beta_hat, SEXP prior_R, int J);

double findOneRootLogPostSigmaNorm(double sigma0, double z, double A, double nu,
                            double V, int n, double min, double max);
double findOneRootLogPostSigmaRobust(double sigma0, double z, double A, 
                            double nuBeta, double nuTau,
                            double V, int n, double min, double max);

SEXP getV_R(SEXP prior_R);

SEXP makeVBar_R(SEXP object, SEXP iBeta);
double logPostPhiMix(double phi, double *level, double meanLevel, 
                int nAlong, int indexClassMax_r, double omega);
double logPostPhiFirstOrderMix(double phi, double *level, double meanLevel, 
                int nAlong, int indexClassMax_r, double omega);
double makeLifeExpBirth(double *mx, double *nx, double *ax, 
                        int iAge0_r, int nAge);

double safeLogProp_Binomial(double logit_th_new, 
                            double logit_th_other_new,
                            double logit_th_old, 
                            double logit_th_other_old, 
                            double scale,
                            double weight,
                            double weight_other);
double safeLogProp_Poisson(double log_th_new, 
                            double log_th_other_new,
                            double log_th_old, 
                            double log_th_other_old, 
                            double scale,
                            double weight,
                            double weight_other);

void predictAlphaDLMNoTrend(SEXP prior_R);
void predictAlphaDeltaDLMWithTrend(SEXP prior_R);
void predictBeta(double* beta, SEXP prior_R, int J);
void predictBetas(SEXP object_R);


void predictPriorsBetas(SEXP object_R);
void predictSeason(SEXP prior_R);
void predictUBeta(SEXP prior_R);

void transferAlphaDelta0(double *state, double *values, int offset,
                    SEXP iteratorValues_R, SEXP iteratorState_R);
void transferSeason0(SEXP s_R, int nSeason, double *values, int offset,
                    SEXP iteratorState_R, SEXP iteratorValues_R);
                    
void transferParamBetas(SEXP model_R, const char *filename, 
                                        int lengthIter, int iteration);


void transferParamPriorsBetas(SEXP model_R, const char *filename,
                                int lengthIter, int iteration);

void transferParamSigma(SEXP model_R, const char *filename, 
                                        int lengthIter, int iteration);
void transferParamVarsigma(SEXP model_R, const char *filename, 
                                        int lengthIter, int iteration);
                                                        
SEXP centerA(SEXP vec_R, SEXP iterator_R);
SEXP diff_R(SEXP vec_R, SEXP order_R);
int makeIOther(int i, SEXP transform_R);


/* loglikelihood */
double logLikelihood_Binomial(SEXP model_R, int count, 
                                SEXP dataset_R, int i);
double logLikelihood_Poisson(SEXP model_R, int count, 
                                SEXP dataset_R, int i);
double logLikelihood_PoissonBinomialMixture(SEXP model_R, int count, 
                                SEXP dataset_R, int i);
double logLikelihood(SEXP model_R, int count, 
                                SEXP dataset_R, int i);
SEXP diffLogLik_R(SEXP yProp_R, SEXP y_R, SEXP indicesY_R, 
        SEXP observation_R, SEXP datasets_R, SEXP transforms_R);

/* iterators */
void advanceA(SEXP iterator_R);
void resetA(SEXP iterator_R);
void advanceB(SEXP iterator_R);
void resetB(SEXP iterator_R);
void advanceD(SEXP iterator_R);
void resetD(SEXP iterator_R);
void advanceM(SEXP object_R);
void resetM(SEXP object_R);
void advanceS(SEXP object_R);
void resetS(SEXP object_R);
void advanceCAP(SEXP iterator_R);
void advanceCC(SEXP iterator_R);
void resetCAP(SEXP iterator_R, int i);
void resetCC(SEXP iterator_R, int i);
void resetCODPCP(SEXP iterator_R, int i);

/* priors */
void predictPrior(SEXP prior_R);
void predictPrior_ExchNormZero(SEXP prior_R);
void predictPrior_ExchNormCov(SEXP prior_R);
void predictPrior_ExchRobustZero(SEXP prior_R);
void predictPrior_ExchRobustCov(SEXP prior_R);
void predictPrior_DLMNoTrendNormZeroNoSeasonPredict(SEXP prior_R);
void predictPrior_DLMWithTrendNormZeroNoSeasonPredict(SEXP prior_R);
void predictPrior_DLMNoTrendNormZeroWithSeasonPredict(SEXP prior_R);
void predictPrior_DLMWithTrendNormZeroWithSeasonPredict(SEXP prior_R);
void predictPrior_DLMNoTrendNormCovNoSeasonPredict(SEXP prior_R);
void predictPrior_DLMWithTrendNormCovNoSeasonPredict(SEXP prior_R);
void predictPrior_DLMNoTrendNormCovWithSeasonPredict(SEXP prior_R);
void predictPrior_DLMWithTrendNormCovWithSeasonPredict(SEXP prior_R);
void predictPrior_DLMNoTrendRobustZeroNoSeasonPredict(SEXP prior_R);
void predictPrior_DLMWithTrendRobustZeroNoSeasonPredict(SEXP prior_R);
void predictPrior_DLMNoTrendRobustZeroWithSeasonPredict(SEXP prior_R);
void predictPrior_DLMWithTrendRobustZeroWithSeasonPredict(SEXP prior_R);
void predictPrior_DLMNoTrendRobustCovNoSeasonPredict(SEXP prior_R);
void predictPrior_DLMWithTrendRobustCovNoSeasonPredict(SEXP prior_R);
void predictPrior_DLMNoTrendRobustCovWithSeasonPredict(SEXP prior_R);
void predictPrior_DLMWithTrendRobustCovWithSeasonPredict(SEXP prior_R);

void transferParamPrior(SEXP prior_R, double *values, int nValues);
void transferParamPrior_ExchNormZero(SEXP prior_R, double *values, 
                      int nValues);
void transferParamPrior_ExchNormCov(SEXP prior_R, double *values, 
                                            int nValues);
void transferParamPrior_ExchRobustZero(SEXP prior_R, double *values, 
                                            int nValues);
void transferParamPrior_ExchRobustCov(SEXP prior_R, double *values, 
                                            int nValues);
void transferParamPrior_DLMNoTrendNormZeroNoSeasonPredict(SEXP prior_R,
                                double *values, int nValues);
void transferParamPrior_DLMWithTrendNormZeroNoSeasonPredict(SEXP prior_R,
                                double *values, int nValues);
void transferParamPrior_DLMNoTrendNormZeroWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues);
void transferParamPrior_DLMWithTrendNormZeroWithSeasonPredict(SEXP prior_R,
                    double *values, int nValues);
void transferParamPrior_DLMNoTrendNormCovNoSeasonPredict(SEXP prior_R,
                                double *values, int nValues);
void transferParamPrior_DLMWithTrendNormCovNoSeasonPredict(SEXP prior_R,
                                double *values, int nValues);
void transferParamPrior_DLMNoTrendNormCovWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues); 
void transferParamPrior_DLMWithTrendNormCovWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues);
void transferParamPrior_DLMNoTrendRobustZeroNoSeasonPredict(SEXP prior_R,
                                double *values, int nValues);
void transferParamPrior_DLMWithTrendRobustZeroNoSeasonPredict(SEXP prior_R,
                                double *values, int nValues);
void transferParamPrior_DLMNoTrendRobustZeroWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues);
void transferParamPrior_DLMWithTrendRobustZeroWithSeasonPredict(SEXP prior_R,
                    double *values, int nValues);
void transferParamPrior_DLMNoTrendRobustCovNoSeasonPredict(SEXP prior_R,
                                double *values, int nValues);
void transferParamPrior_DLMWithTrendRobustCovNoSeasonPredict(SEXP prior_R,
                                double *values, int nValues);
void transferParamPrior_DLMNoTrendRobustCovWithSeasonPredict(SEXP prior_R,
                        double *values, int nValues);
void transferParamPrior_DLMWithTrendRobustCovWithSeasonPredict(SEXP prior_R,
                    double *values, int nValues);

/* update-nongeneric */
double updateSDNorm(double sigma, double A, double nu, double V, int n, double max);
double updateSDRobust(double sigma, double A, double nuBeta, 
              double nuTau, double V, int n, double max);

void updateEta(SEXP prior_R, double* beta, int J);

void
updateAlphaDLMNoTrend(SEXP prior_R, double *betaTilde, int J);
void
updateAlphaDeltaDLMWithTrend(SEXP prior_R, double *betaTilde, int J);
void
updateSeason(SEXP prior_R, double *betaTilde, int J);

void updateSigma_Varying(SEXP object);

void updateTheta_BinomialVarying(SEXP object, SEXP y_R, SEXP exposure_R);
void updateTheta_BinomialVaryingAgCertain(SEXP object, SEXP y_R, SEXP exposure_R);
void updateThetaAndValueAgFun_Binomial(SEXP object, SEXP y_R, SEXP exposure_R);
void updateThetaAndValueAgNormal_Binomial(SEXP object, SEXP y_R, SEXP exposure_R);
void updateTheta_PoissonVaryingNotUseExp(SEXP object, SEXP y_R);
void updateTheta_PoissonVaryingUseExp(SEXP object, SEXP y_R, SEXP exposure_R);
void updateTheta_PoissonVaryingNotUseExpAgCertain(SEXP object, SEXP y_R);
void updateTheta_PoissonVaryingUseExpAgCertain(SEXP object, SEXP y_R, SEXP exposure_R);
void updateThetaAndValueAgNormal_PoissonNotUseExp(SEXP object, SEXP y_R);
void updateThetaAndValueAgPoisson_PoissonNotUseExp(SEXP object, SEXP y_R);
void updateThetaAndValueAgNormal_PoissonUseExp(SEXP object, SEXP y_R, SEXP exposure_R);
void updateThetaAndValueAgPoisson_PoissonUseExp(SEXP object, SEXP y_R, SEXP exposure_R);
void updateTheta_NormalVarying(SEXP object, SEXP y_R);
void updateTheta_NormalVaryingAgCertain(SEXP object, SEXP y_R);
void updateThetaAndValueAgNormal_Normal(SEXP object, SEXP y_R);
void updateThetaAndValueAgFun_Normal(SEXP object, SEXP y_R);
void updateThetaAndValueAgFun_PoissonNotUseExp(SEXP object, SEXP y_R);
void updateThetaAndValueAgFun_PoissonUseExp(SEXP object, SEXP y_R, SEXP exposure_R);
void updateThetaAndValueAgLife_PoissonUseExp(SEXP object, SEXP y_R, SEXP exposure_R);

void updateVarsigma(SEXP object, SEXP y_R);

/* update counts */
void updateCountsPoissonNotUseExp(SEXP y_R, SEXP model_R, 
            SEXP observation_R, SEXP datasets_R, SEXP transforms_R);
void updateCountsPoissonUseExp(SEXP y_R, SEXP model_R, 
                        SEXP exposure_R, SEXP observation_R, 
                        SEXP datasets_R, SEXP transforms_R);
void updateCountsBinomial(SEXP y_R, SEXP model_R, 
                        SEXP exposure_R, SEXP observation_R, 
                        SEXP datasets_R, SEXP transforms_R);
void 
updateObservationCounts(SEXP y_R, SEXP observation_R, 
                        SEXP datasets_R, SEXP transforms_R);

/* transfer param model */
void transferParamModel(SEXP model_R, const char *filename,
                                int lengthIter, int iteration);
void transferParamModel_NormalVaryingVarsigmaKnownPredict(SEXP model_R, 
        const char *filename, int lengthIter, int iteration);
void transferParamModel_NormalVaryingVarsigmaUnknownPredict(SEXP model_R, 
        const char *filename, int lengthIter, int iteration);
void transferParamModel_PoissonVaryingNotUseExpPredict(SEXP model_R, 
        const char *filename, int lengthIter, int iteration);
void transferParamModel_BinomialVaryingPredict(SEXP model_R, 
        const char *filename, int lengthIter, int iteration);
void transferParamModel_PoissonVaryingUseExpPredict(SEXP model_R, 
        const char *filename, int lengthIter, int iteration);


/* predict models not using exposure*/
void predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict(SEXP object, SEXP y_R);
void predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict(SEXP object, SEXP y_R);
void predictModelNotUseExp_PoissonVaryingNotUseExpPredict(SEXP object, SEXP y_R);
void predictModelNotUseExp(SEXP object, SEXP y_R);
/* predict models using exposure*/
void predictModelUseExp_BinomialVaryingPredict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp_PoissonVaryingUseExpPredict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp_PoissonBinomialMixturePredict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp(SEXP object, SEXP y_R, SEXP exposure_R);


/* update models not using exposure*/
void updateModelNotUseExp_NormalVaryingVarsigmaKnown(SEXP object, SEXP y_R);
void updateModelNotUseExp_NormalVaryingVarsigmaUnknown(SEXP object, SEXP y_R);
void updateModelNotUseExp_PoissonVaryingNotUseExp(SEXP object, SEXP y_R);
void updateModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain(SEXP object, SEXP y_R);
void updateModelNotUseExp_PoissonVaryingNotUseExpAgCertain(SEXP object, SEXP y_R);
void updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain(SEXP object, SEXP y_R);
void updateModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal(SEXP object, SEXP y_R);
void updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal(SEXP object, SEXP y_R);
void updateModelNotUseExp_NormalVaryingVarsigmaKnownAgFun(SEXP object, SEXP y_R);
void updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun(SEXP object, SEXP y_R);
void updateModelNotUseExp_PoissonVaryingNotUseExpAgNormal(SEXP object, SEXP y_R);
void updateModelNotUseExp_PoissonVaryingNotUseExpAgFun(SEXP object, SEXP y_R);
void updateModelNotUseExp_PoissonVaryingNotUseExpAgPoisson(SEXP object, SEXP y_R);
void updateModelNotUseExp(SEXP object, SEXP y_R);
/* update models using exposure*/
void updateModelUseExp_BinomialVarying(SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonVarying(SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonBinomialMixture
                                (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_BinomialVaryingAgCertain
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_BinomialVaryingAgNormal
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_BinomialVaryingAgFun
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonVaryingUseExpAgCertain
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonVaryingUseExpAgNormal
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonVaryingUseExpAgFun
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonVaryingUseExpAgPoisson
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonVaryingUseExpAgLife
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp(SEXP object, SEXP y_R, SEXP exposure_R);

void updateBetasAndPriorsBetas(SEXP object_R);

/* predict combined models*/
void predictCombined_CombinedModelNormal(SEXP object_R, 
                    const char *filename, int lengthIter, int iteration);
void predictCombined_CombinedModelPoissonNotHasExp(SEXP object_R, 
                    const char *filename, int lengthIter, int iteration);
void predictCombined_CombinedModelBinomial(SEXP object_R, 
                    const char *filename, int lengthIter, int iteration);
void predictCombined_CombinedModelPoissonHasExp(SEXP object_R, 
                    const char *filename, int lengthIter, int iteration);
void predictCombined(SEXP object_R, 
                    const char *filename, int lengthIter, int iteration);

/* update combined models*/
void updateCombined_CombinedModelBinomial(SEXP object_R, int nUpdate);
void updateCombined_CombinedModelNormal(SEXP object_R, int nUpdate);
void updateCombined_CombinedModelPoissonNotHasExp(SEXP object_R, int nUpdate);
void updateCombined_CombinedModelPoissonHasExp(SEXP object_R, int nUpdate);
void updateCombined(SEXP object_R, int nUpdate);

/* update combined counts */
void updateCombined_CombinedCountsPoissonNotHasExp(SEXP object_R,
                                                        int nUpdate);
void updateCombined_CombinedCountsPoissonHasExp(SEXP object_R,
                                                        int nUpdate);
void updateCombined_CombinedCountsBinomial(SEXP object_R,
                                                        int nUpdate);

/* estimate one chain */
void estimateOneChain(SEXP object_R, SEXP filename_R, 
                SEXP nBurnin_R, SEXP nSim_R, SEXP nThin_R,
                SEXP continuing);

/* get data from file */
SEXP getOneIterFromFile_R(SEXP filename_R, 
                        SEXP first_R, SEXP last_R, 
                        SEXP lengthIter_R, SEXP iteration_R);

SEXP getDataFromFile_R(SEXP filename_R, 
                        SEXP first_R, SEXP last_R, 
                        SEXP lengthIter_R, SEXP iterations_R);

/* description helpers */
int chooseICellComp(SEXP description_R);
SEXP chooseICellOutInPool(SEXP description_R);
int chooseICellPopn(SEXP description_R);
int getIAccNextFromPopn(int i, SEXP description_R);
int getIPopnNextFromPopn(int i, SEXP description_R);
int getMinValCohort(int i, SEXP series_R, SEXP iterator_R);

/* mapping functions */
int getIPopnNextFromComp(int i, SEXP mapping_R);
SEXP getIPopnNextFromOrigDest(int i, SEXP mapping_R);


/* pointers for routines from dembase package 
 * 
 * these have to be populated with R_GetCCallable in the initialisation function */
SEXP (*dembase_Collapse_R)(SEXP ,SEXP); 
int (*dembase_getIAfter)(int, SEXP);
SEXP (*dembase_getIBefore)(int, SEXP);
SEXP (*dembase_getIShared)(int, SEXP);

#endif
