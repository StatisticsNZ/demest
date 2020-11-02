
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

#define USE_LOG 1 /* macro for indicator to use log for dbinom etc */
#define NOT_USE_LOG 0 /* macro for indicator to use log for dbinom etc */

#define SET_DOUBLESCALE_SLOT(object, slotname, value) { \
    double *ptr = REAL(GET_SLOT(object, slotname));     \
    *ptr = (value);}

#define SET_INTSCALE_SLOT(object, slotname, value) {    \
    int *ptr = INTEGER(GET_SLOT(object, slotname));     \
    *ptr = (value);}

#define SET_LOGICALSCALE_SLOT(object, slotname, value) {    \
    int *ptr = LOGICAL(GET_SLOT(object, slotname));     \
    *ptr = (value);}


/* everything in "x_sym" form here must be macro defined in init.c */



/* Priors-methods */

void updateGWithTrend(SEXP prior_R);
void updateLatentComponentWeightMix(SEXP prior_R);
void updateLatentWeightMix(SEXP prior_R);
void updateLevelComponentWeightMix(SEXP prior_R);
void updateMeanLevelComponentWeightMix(SEXP prior_R);
void updateIndexClassMaxPossibleMix(SEXP prior_R);
void updateIndexClassMaxUsedMix(SEXP prior_R);
void updateIndexClassMix(SEXP prior_R, double * betaTilde, int J);
void updateVectorsMixAndProdVectorsMix(SEXP prior_R, double * betaTilde, int J);
void updateOmegaAlpha(SEXP prior_R, int isWithTrend);
void updateOmegaComponentWeightMix(SEXP prior_R);
void updateOmegaDelta(SEXP prior_R);
void updateOmegaLevelComponentWeightMix(SEXP prior_R);
void updateOmegaSeason(SEXP prior_R);
void updateOmegaVectorsMix(SEXP prior_R);
void updatePhi(SEXP prior_R, int isWithTrend);
void updatePhiMix(SEXP prior_R);
void updateUEtaCoef(SEXP prior_R);
void updateWSqrt(SEXP prior_R);
void updateWSqrtInvG(SEXP prior_R);
void updateWeightMix(SEXP prior_R);
void updateTauNorm(SEXP prior_R, double *betaScaled, int J);
void updateTauRobust(SEXP prior_R, int J);

void updateUBeta(SEXP prior_R, double *beta, int J);

void
updatePriorBeta(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_ExchFixed(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_ExchNormZero(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_ExchNormCov(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_ExchRobustZero(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_ExchRobustCov(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMNoTrendNormZeroNoSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMWithTrendNormZeroNoSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMNoTrendNormZeroWithSeason(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMWithTrendNormZeroWithSeason(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMNoTrendNormCovNoSeason(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMWithTrendNormCovNoSeason(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMNoTrendNormCovWithSeason(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMWithTrendNormCovWithSeason(double *beta, int J, SEXP prior_R,
                        double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMNoTrendRobustZeroNoSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMWithTrendRobustZeroNoSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMNoTrendRobustZeroWithSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMWithTrendRobustZeroWithSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMNoTrendRobustCovNoSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMWithTrendRobustCovNoSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMNoTrendRobustCovWithSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_DLMWithTrendRobustCovWithSeason(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_KnownCertain(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_KnownUncertain(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_MixNormZero(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
void
updatePriorBeta_Zero(double *beta, int J,
                    SEXP prior_R, double *thetaTransformed, double sigma);
/* helper-functions */
SEXP makeMu(int n, SEXP betas_R, SEXP iterator_R);
double dpoibin1(int x, int size, double prob, int use_log);
double invlogit1(double x);
int rcateg1(double* cumProb);
int rbinomTrunc1(int size, double prob, int lower, int upper, int maxAttempt);
double rhalftTrunc1(double df, double scale, double max);
double rinvchisq1(double df, double scaleSq);

SEXP rmvnorm1(SEXP mean_R, SEXP var_R);
SEXP rmvnorm2(SEXP mean_R, SEXP var_R);
SEXP rnormTruncated(int n, SEXP mean_R, SEXP sd_R,
                double lower, double upper, double tolerance,
                int maxAttempt,
                int uniform);
int rnormIntTrunc1(double mean, double sd,
                int lower, int upper);
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

SEXP makeVBarAndN_R(SEXP object, SEXP iBeta_R);
double logPostPhiMix(double phi, double *level, double meanLevel,
                int nAlong, int indexClassMax_r, double omega);
double logPostPhiFirstOrderMix(double phi, double *level, double meanLevel,
                int nAlong, int indexClassMax_r, double omega);
double logPostPhiSecondOrderMix(double phi, double *level, double meanLevel,
                int nAlong, int indexClassMax_r, double omega);
double makeLifeExpBirth(double *mx, double *nx, double *ax,
                        int iAge0_r, int nAge);
double modePhiMix (double * level, double meanLevel, int nAlong,
              int indexClassMax, double omega, double tolerance);

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
void predictAlphaLN2(SEXP prior_R);
void predictBeta(double* beta, SEXP prior_R, int J);
void predictBetas(SEXP object_R);
void predictComponentWeightMix(SEXP prior_R);
void predictIndexClassMix(SEXP prior_R);
void predictLevelComponentWeightMix(SEXP prior_R);

void predictPriorsBetas(SEXP object_R);
void predictSeason(SEXP prior_R);
void predictUBeta(SEXP prior_R);

void transferAlphaDelta0(double *state, double *values, int offset,
                    SEXP iteratorValues_R, SEXP iteratorState_R);
void transferSeason0(SEXP s_R, int nSeason, double *values, int offset,
                    SEXP iteratorState_R, SEXP iteratorValues_R);
void transferLevelComponentWeightOldMix(double * ans, double * values, int offset,
                                int nAlongOld, int indexClassMax);
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

/* helper-simulate */
void drawAlphaLN2(SEXP object_R);
void drawBetas(SEXP object_R);
void drawDataModelsAccount(SEXP combined_R);
void drawDelta0(SEXP prior_R);
void drawEta(SEXP prior_R);
void drawOmegaAlpha(SEXP prior_R);
void drawOmegaComponentWeightMix(SEXP prior_R);
void drawOmegaDelta(SEXP prior_R);
void drawOmegaLevelComponentWeightMix(SEXP prior_R);
void drawOmegaSeason(SEXP prior_R);
void drawOmegaVectorsMix(SEXP prior_R);
void drawPhi(SEXP prior_R);
void drawPhiMix(SEXP prior_R);
void drawPriors(SEXP object_R);
void drawSigma_Varying(SEXP object_R);
void drawTau(SEXP prior_R);
void drawUEtaCoef(SEXP prior_R);
void drawVarsigma(SEXP object_R);


/* loglikelihood */
double logLikelihood_Binomial(SEXP model_R, int count,
                                SEXP dataset_R, int i);
double logLikelihood_CMP(SEXP model_R, int count,
                                SEXP dataset_R, int i);
double logLikelihood_Poisson(SEXP model_R, int count,
                                SEXP dataset_R, int i);
double logLikelihood_PoissonBinomialMixture(SEXP model_R, int count,
                                SEXP dataset_R, int i);
double logLikelihood_Round3(SEXP model_R, int count,
                SEXP dataset_R, int i);
double logLikelihood_NormalFixedUseExp(SEXP model_R, int count,
                                SEXP dataset_R, int i);
double logLikelihood_TFixedUseExp(SEXP model_R, int count,
                                SEXP dataset_R, int i);
double logLikelihood_LN2(SEXP model_R, int count,
                                SEXP dataset_R, int i);
double logLikelihood(SEXP model_R, int count,
                                SEXP dataset_R, int i);
SEXP diffLogLik_R(SEXP yProp_R, SEXP y_R, SEXP indicesY_R,
        SEXP dataModels_R, SEXP datasets_R, SEXP transforms_R);

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
void advanceCA(SEXP iterator_R);
void advanceCP(SEXP iterator_R);
void advanceCC(SEXP iterator_R);
void advanceCODPCP(SEXP iterator_R);
void resetCA(SEXP iterator_R, int i);
void resetCP(SEXP iterator_R, int i);
void resetCC(SEXP iterator_R, int i);
void resetCODPCP(SEXP iterator_R, int i);

/* priors */
void predictPrior(SEXP prior_R);
void predictPrior_ExchFixed(SEXP prior_R);
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
void predictPrior_KnownCertain(SEXP prior_R);
void predictPrior_KnownUncertain(SEXP prior_R);
void predictPrior_MixNormZero(SEXP prior_R);
void predictPrior_Zero(SEXP prior_R);

void drawPrior(SEXP prior_R);
void drawPrior_ExchFixed(SEXP prior_R);
void drawPrior_ExchNormZero(SEXP prior_R);
void drawPrior_ExchNormCov(SEXP prior_R);
void drawPrior_ExchRobustZero(SEXP prior_R);
void drawPrior_ExchRobustCov(SEXP prior_R);
void drawPrior_DLMNoTrendNormZeroNoSeason(SEXP prior_R);
void drawPrior_DLMWithTrendNormZeroNoSeason(SEXP prior_R);
void drawPrior_DLMNoTrendNormZeroWithSeason(SEXP prior_R);
void drawPrior_DLMWithTrendNormZeroWithSeason(SEXP prior_R);
void drawPrior_DLMNoTrendNormCovNoSeason(SEXP prior_R);
void drawPrior_DLMWithTrendNormCovNoSeason(SEXP prior_R);
void drawPrior_DLMNoTrendNormCovWithSeason(SEXP prior_R);
void drawPrior_DLMWithTrendNormCovWithSeason(SEXP prior_R);
void drawPrior_DLMNoTrendRobustZeroNoSeason(SEXP prior_R);
void drawPrior_DLMWithTrendRobustZeroNoSeason(SEXP prior_R);
void drawPrior_DLMNoTrendRobustZeroWithSeason(SEXP prior_R);
void drawPrior_DLMWithTrendRobustZeroWithSeason(SEXP prior_R);
void drawPrior_DLMNoTrendRobustCovNoSeason(SEXP prior_R);
void drawPrior_DLMWithTrendRobustCovNoSeason(SEXP prior_R);
void drawPrior_DLMNoTrendRobustCovWithSeason(SEXP prior_R);
void drawPrior_DLMWithTrendRobustCovWithSeason(SEXP prior_R);
void drawPrior_KnownCertain(SEXP prior_R);
void drawPrior_KnownUncertain(SEXP prior_R);
void drawPrior_MixNormZero(SEXP prior_R);
void drawPrior_Zero(SEXP prior_R);

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
void transferParamPrior_MixNormZeroPredict(SEXP prior_R,
                    double *values, int nValues);

/* update-nongeneric */

void updateBetas(SEXP object);
void updateLogPostBetas(SEXP object);
void updateMeansBetas(SEXP object);
void updateVariancesBetas(SEXP object);
void updateMu(SEXP object);

double updateSDNorm(double sigma, double A, double nu, double V, int n, double max);
double updateSDRobust(double sigma, double A, double nuBeta,
              double nuTau, double V, int n, double max);

void updateAlphaMix(SEXP prior_R);

void updateEta(SEXP prior_R, double* beta, int J);
void updateComponentWeightMix(SEXP prior_R);

void
updateAlphaDLMNoTrend(SEXP prior_R, double *betaTilde, int J);
void
updateAlphaDeltaDLMWithTrend(SEXP prior_R, double *betaTilde, int J);
void
updateSeason(SEXP prior_R, double *betaTilde, int J);

void updateSigma_Varying(SEXP object);
void updateSigmaLN2(SEXP object_R);

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
void updateThetaAndNu_CMPVaryingNotUseExp(SEXP object_R, SEXP y_R);
void updateThetaAndNu_CMPVaryingUseExp(SEXP object_R, SEXP y_R, SEXP exposure_R);

void updateVarsigma(SEXP object, SEXP y_R);
void updateVarsigmaLN2(SEXP object_R, SEXP y_R, SEXP exposure_R);

/* update counts */
void updateCountsPoissonNotUseExp(SEXP y_R, SEXP model_R,
            SEXP dataModels_R, SEXP datasets_R, SEXP transforms_R);
void updateCountsPoissonUseExp(SEXP y_R, SEXP model_R,
                        SEXP exposure_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP transforms_R);
void updateCountsBinomial(SEXP y_R, SEXP model_R,
                        SEXP exposure_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP transforms_R);
void
updateDataModelsCounts(SEXP y_R, SEXP dataModels_R,
               SEXP datasets_R, SEXP transforms_R);
void
updateDataModelsAccount(SEXP combined_R);

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
void transferParamModel_PoissonBinomialMixture(SEXP model_R,
        const char *filename, int lengthIter, int iteration);
void transferParamModel_NormalFixedNotUseExpPredict(SEXP model_R,
        const char *filename, int lengthIter, int iteration);
void transferParamModel_NormalFixedUseExpPredict(SEXP model_R,
        const char *filename, int lengthIter, int iteration);
void transferParamModel_Round3(SEXP model_R,
        const char *filename, int lengthIter, int iteration);
void transferParamModel_TFixedNotUseExpPredict(SEXP model_R,
        const char *filename, int lengthIter, int iteration);
void transferParamModel_TFixedUseExpPredict(SEXP model_R,
        const char *filename, int lengthIter, int iteration);
void transferParamModel_LN2Predict(SEXP model_R,
        const char *filename, int lengthIter, int iteration);


/* predict models not using exposure*/
void predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict(SEXP object, SEXP y_R);
void predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict(SEXP object, SEXP y_R);
void predictModelNotUseExp_PoissonVaryingNotUseExpPredict(SEXP object, SEXP y_R);
void predictModelNotUseExp_NormalFixedNotUseExpPredict(SEXP object, SEXP y_R);
void predictModelNotUseExp_TFixedNotUseExpPredict(SEXP object, SEXP y_R);
void predictModelNotUseExp(SEXP object, SEXP y_R);
/* predict models using exposure*/
void predictModelUseExp_BinomialVaryingPredict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp_PoissonVaryingUseExpPredict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp_PoissonBinomialMixturePredict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp_Round3Predict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp_NormalFixedUseExpPredict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp_TFixedUseExpPredict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp_LN2Predict(SEXP object, SEXP y_R, SEXP exposure_R);
void predictModelUseExp(SEXP object, SEXP y_R, SEXP exposure_R);


/* update models not using exposure*/
void updateModelNotUseExp_CMPVaryingNotUseExp(SEXP object, SEXP y_R);
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
void updateModelNotUseExp_NormalFixedNotUseExp(SEXP object, SEXP y_R);
void updateModelNotUseExp_TFixedNotUseExp(SEXP object, SEXP y_R);
void updateModelNotUseExp(SEXP object, SEXP y_R);
/* update models using exposure*/
void updateModelUseExp_CMPVaryingUseExp(SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_BinomialVarying(SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonVarying(SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_PoissonBinomialMixture
                                (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_Round3
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
void updateModelUseExp_NormalFixedUseExp
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_TFixedUseExp
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp_LN2(SEXP object, SEXP y_R, SEXP exposure_R);
void updateModelUseExp(SEXP object, SEXP y_R, SEXP exposure_R);
void updateAlphaLN2(SEXP object_R, SEXP y_R, SEXP exposure_R);
void updatePriorsBetas(SEXP object_R);

/* update models not using exposure*/
void drawModelNotUseExp_NormalVaryingVarsigmaKnown(SEXP object, SEXP y_R);
void drawModelNotUseExp_NormalVaryingVarsigmaUnknown(SEXP object, SEXP y_R);
void drawModelNotUseExp_PoissonVarying(SEXP object, SEXP y_R);
void drawModelNotUseExp(SEXP object, SEXP y_R);
/* update models using exposure*/
void drawModelUseExp_BinomialVarying(SEXP object, SEXP y_R, SEXP exposure_R);
void drawModelUseExp_PoissonVarying(SEXP object, SEXP y_R, SEXP exposure_R);
void drawModelUseExp_PoissonBinomialMixture
                                (SEXP object, SEXP y_R, SEXP exposure_R);
void drawModelUseExp_NormalFixedUseExp
                            (SEXP object, SEXP y_R, SEXP exposure_R);
void drawModelUseExp_LN2(SEXP object, SEXP y_R, SEXP exposure_R);
void drawModelUseExp(SEXP object, SEXP y_R, SEXP exposure_R);



/* draw combined models*/
void drawCombined_CombinedModelBinomial(SEXP object_R, int nUpdate);
void drawCombined_CombinedAccountMovements(SEXP object_R, int nUpdate);
void drawCombined(SEXP object_R, int nUpdate);

/* draw data models*/
void drawDataModels_CombinedAccountMovements(SEXP combined_R);
void drawDataModels(SEXP combined_R);

/* draw system models*/
void drawSystemModels_CombinedAccountMovements(SEXP combined_R);
void drawSystemModels(SEXP combined_R);

/* predict combined models*/
void predictCombined_CombinedModelNormal(SEXP object_R,
                    const char *filename, int lengthIter, int iteration);
void predictCombined_CombinedModelPoissonNotHasExp(SEXP object_R,
                    const char *filename, int lengthIter, int iteration);
void predictCombined_CombinedModelBinomial(SEXP object_R,
                    const char *filename, int lengthIter, int iteration);
void predictCombined_CombinedModelPoissonHasExp(SEXP object_R,
                    const char *filename, int lengthIter, int iteration);
void predictCombined_CombinedCountsPoissonNotHasExp(SEXP object_R,
                    const char *filename, int lengthIter, int iteration);
void predictCombined_CombinedCountsPoissonHasExp(SEXP object_R,
                    const char *filename, int lengthIter, int iteration);
void predictCombined(SEXP object_R,
                    const char *filename, int lengthIter, int iteration);

/* update combined models*/
void updateCombined_CombinedModelBinomial(SEXP object_R, int nUpdate);
void updateCombined_CombinedModelNormal(SEXP object_R, int nUpdate);
void updateCombined_CombinedModelPoissonNotHasExp(SEXP object_R, int nUpdate);
void updateCombined_CombinedModelPoissonHasExp(SEXP object_R, int nUpdate);
void updateCombined_CombinedModelCMPNotHasExp(SEXP object_R, int nUpdate);
void updateCombined_CombinedModelCMPHasExp(SEXP object_R, int nUpdate);
void updateCombined_CombinedAccount(SEXP object_R, int nUpdate);
void updateCombined(SEXP object_R, int nUpdate);

void updateProposalAccount_CombinedAccountMovements(SEXP object_R);
void updateProposalAccount(SEXP object_R);
double diffLogLikAccount_CombinedAccountMovements(SEXP object_R);
double diffLogLikAccount(SEXP object_R);
double diffLogDensAccount_CombinedAccountMovements(SEXP object_R);
double diffLogDensAccount(SEXP object_R);
void updateValuesAccount_CombinedAccountMovements(SEXP object_R);
void updateValuesAccount(SEXP object_R);
void updateExpectedExposure_CombinedAccountMovements(SEXP object_R);
void updateExpectedExposure(SEXP object_R);
void updateSystemModels_CombinedAccountMovements(SEXP combined_R);
void updateSystemModels(SEXP object_R);

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

SEXP overwriteValuesOnFile_R(SEXP object_R, SEXP skeleton_R,
              SEXP filename_R, SEXP nIteration_R, SEXP lengthIter_R);

/* description helpers */
int chooseICellComp(SEXP description_R);
int chooseICellCompUpperTri(SEXP description_R);
SEXP chooseICellOutInPool(SEXP description_R);
int chooseICellPopn(SEXP description_R);
SEXP chooseICellSubAddNet(SEXP description_R);
int getICellLowerTriFromComp(int i_cell_up_r, SEXP description_R);
int getICellLowerTriNextFromComp(int i_cell_up_r, SEXP description_R);
int isLowerTriangle(int i, SEXP description_R);
int isOldestAgeGroup(int i, SEXP description_R);
int getIAccNextFromPopn(int i, SEXP description_R);
int getIPopnNextFromPopn(int i, SEXP description_R);
int getIExpFirstFromPopn(int i, SEXP description_R);
int getMinValCohortAccession(int i, SEXP series_R, SEXP iterator_R);
int getMinValCohortPopulationHasAge(int i, SEXP population_R, SEXP accession_R,
				    SEXP iterator_R);
int getMinValCohortPopulationNoAge(int i, SEXP series_R, SEXP iterator_R);

/* mapping functions */
int getIPopnNextFromComp(int i, SEXP mapping_R);
SEXP getIPopnNextFromOrigDest(int i, SEXP mapping_R);

int getIAccNextFromComp(int i, SEXP mapping_R);
SEXP getIAccNextFromOrigDest(int i, SEXP mapping_R);
int getIExposureFromComp(int i, SEXP mapping_R);
int getIExposureFromBirths(int i, SEXP mapping_R);
int getIExposureFromOrigDest(int i, SEXP mapping_R);
int getIExpFirstFromComp(int i, SEXP mapping_R);
int getIExpFirstFromBirths(int i, SEXP mapping_R);
SEXP getIExpFirstPairFromOrigDest(int i, SEXP mapping_R);
int getICellCompFromExp(int i, SEXP mapping_R);
int getICellBirthsFromExp(int i, SEXP mapping_R, int ageForward);

/* CMP */
double logDensCMPUnnormalised1(int x, double gamma, double nu);
double rcmpUnder(double mu, double nu, int maxAttempt);
double rcmpOver(double mu, double nu, int maxAttempt);
double rcmp1(double mu, double nu, int maxAttempt);

/* update-account */
void updateAccount(SEXP combined_R);
void updateProposalAccountMovePopn(SEXP combined_R);
void updateProposalAccountMoveBirths(SEXP combined_R);
void updateProposalAccountMoveBirthsSmall(SEXP combined_R);
void updateProposalAccountMoveOrigDest(SEXP combined_R);
void updateProposalAccountMoveOrigDestSmall(SEXP combined_R);
void updateProposalAccountMovePool(SEXP combined_R);
void updateProposalAccountMoveNet(SEXP combined_R);
void updateProposalAccountMoveComp(SEXP combined_R);
void updateProposalAccountMoveCompSmall(SEXP combined_R);

double diffLogLikAccountMovePopn(SEXP combined_R);
double diffLogLikPopn(int diff, int iFirst_r, SEXP iterator_R,
                        SEXP population_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R);
double diffLogLikPopnOneDataset(int diff, int iFirst_r, SEXP iterator_R,
                        SEXP population_R, SEXP model_R,
                        SEXP dataset_R, SEXP transform_R);
double diffLogLikPopnOneCell(int iAfter_r, int diff, SEXP population_R,
                        SEXP model_R, SEXP dataset_R, SEXP transform_R);
double diffLogLikAccountMoveOrigDest(SEXP combined_R);
double diffLogLikCellComp(int diff, int iComp_r, int iCell_r,
                        SEXP component_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R);
double diffLogLikCellOneDataset(int diff, int iCell_r, SEXP component_R,
                        SEXP model_R, SEXP dataset_R, SEXP transform_R);
double diffLogLikPopnPair(int diffOrig, int diffDest,
			  int iPopnOrig_r, int iPopnDest_r,
                        SEXP iterator_R,
                        SEXP population_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R);
double diffLogLikAccountMovePool(SEXP combined_R);
double diffLogLikCellsPool(int diff, int iComp_r, int iCellOut_r, int iCellIn_r,
                        SEXP component_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R);
double diffLogLikAccountMoveNet(SEXP combined_R);
double diffLogLikCellsNet(int diff, int iComp_r,
                        int iCellAdd_r, int iCellSub_r,
                        SEXP component_R, SEXP dataModels_R,
                        SEXP datasets_R, SEXP seriesIndices_R,
                        SEXP transforms_R);
double diffLogLikAccountMoveComp(SEXP combined_R);
double diffLogLikAccountMoveCompSmall(SEXP combined_R);

double diffLogDensPopn(SEXP combined_R);
double diffLogDensPopnOneCohort (int diff, SEXP population_R, int i_r,
                 SEXP iterator_R, double * theta, int * strucZeroArray);
double diffLogDensExpPopn(SEXP combined_R);
double diffLogDensExpOneOrigDestParChPool(int iCell_r, int hasAge,
					  double ageTimeStep, int updatedPopn, int updatedBirths,
                        SEXP component_R, double * theta,
                        int * strucZeroArray,
                        SEXP iteratorComp_R,
                        int iExpFirst_r, double * exposure,
                        SEXP iteratorExposure_R,
					  int diff, int firstOnly,
					  int isSmallUpdateFinal);
double diffLogDensExpOneComp(int iCell_r, int hasAge,
			     double ageTimeStep, int updatedPopn, int updatedBirths,
                        SEXP component_R, double * theta, int * strucZeroArray,
                        SEXP iteratorComp_R,
                        int iExpFirst_r, double * exposure,
                        SEXP iteratorExposure_R,
			     int diff, int firstOnly, int isSmallUpdateFinal);
double diffLogDensJumpOrigDest(SEXP combined_R);
double diffLogDensExpOrigDestPoolNet(SEXP combined_R);
double diffLogDensExpOrigDestSmall(SEXP combined_R);
double diffLogDensJumpPoolWithExpose(SEXP combined_R);
double diffLogDensJumpPoolNoExpose(SEXP combined_R);
double diffLogDensJumpNet(SEXP combined_R);
double diffLogDensJumpComp(SEXP combined_R);
double diffLogDensExpComp(SEXP combined_R);
double diffLogDensExpCompSmall(SEXP combined_R);
double diffLogDensJumpBirthsSmall(SEXP combined_R);
double diffLogDensJumpOrigDestSmall(SEXP combined_R);
double diffLogDensJumpCompSmall(SEXP combined_R);

void updateAccSmall(SEXP combined_R);
void updateExpSmall(SEXP combined_R);
void updateCellMove(SEXP combined_R);
void updateSubsequentPopnMove(SEXP combined_R);
void updateSubsequentAccMove(SEXP combined_R);
void updateSubsequentExpMove(SEXP combined_R);
void updateSubsequentExpMoveOneCohortNoAge(SEXP combined_R);


/* pointers for routines from dembase package
 *
 * these have to be populated with R_GetCCallable in the initialisation function */

#endif
