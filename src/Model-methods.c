#include "model-methods.h"
#include "update-nongeneric.h"
#include "helper-functions.h"
#include "demest.h"


/* File "Model-methods.c" contains C versions of functions 
 * from "Model-methods.R". */

/* functions for model log likelihoods */



double
logLikelihood(SEXP model_R, int count, SEXP dataset_R, int i) 
{
    int iMethodModel = *INTEGER(GET_SLOT(model_R, iMethodModel_sym));
    double ans = 0;
    
    switch(iMethodModel)
    {
        case 9: case 18: case 19: case 118: case 119:/* Binomial */
            ans = logLikelihood_Binomial(model_R, count, dataset_R, i);
            break;
        case 10: case 20: case 21: case 120: case 121:/* Poisson */
            ans = logLikelihood_Poisson(model_R, count, dataset_R, i);
            break;
        case 11: /* PoissonBinomialMixture */
            ans = logLikelihood_PoissonBinomialMixture(
                                        model_R, count, dataset_R, i);
            break;
         default:
            error("unknown iMethodModel: %d", iMethodModel);
            break;
    }
    return ans;
}


/* ************************* transferParamModel functions ************************ */




static __inline__ void
transferParamModel_NormalVaryingVarsigmaKnownPredict_i(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamBetas(model_R, filename, lengthIter, iteration);
    transferParamPriorsBetas(model_R, filename, lengthIter, iteration);
    transferParamSigma(model_R, filename, lengthIter, iteration);
}


static __inline__ void
transferParamModel_NormalVaryingVarsigmaUnknownPredict_i(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamBetas(model_R, filename, lengthIter, iteration);
    transferParamPriorsBetas(model_R, filename, lengthIter, iteration);
    transferParamVarsigma(model_R, filename, lengthIter, iteration);
    transferParamSigma(model_R, filename, lengthIter, iteration);
}


static __inline__ void
transferParamModel_PoissonVaryingNotUseExpPredict_i(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamBetas(model_R, filename, lengthIter, iteration);
    transferParamPriorsBetas(model_R, filename, lengthIter, iteration);
    transferParamSigma(model_R, filename, lengthIter, iteration);
}


static __inline__ void
transferParamModel_BinomialVaryingPredict_i(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamBetas(model_R, filename, lengthIter, iteration);
    transferParamPriorsBetas(model_R, filename, lengthIter, iteration);
    transferParamSigma(model_R, filename, lengthIter, iteration);
}


static __inline__ void
transferParamModel_PoissonVaryingUseExpPredict_i(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamBetas(model_R, filename, lengthIter, iteration);
    transferParamPriorsBetas(model_R, filename, lengthIter, iteration);
    transferParamSigma(model_R, filename, lengthIter, iteration);
}

void
transferParamModel(SEXP model_R, const char *filename,
                                int lengthIter, int iteration) 
{
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));
    
    switch(i_method_model)
    {
        case 104: 
            transferParamModel_NormalVaryingVarsigmaKnownPredict_i(model_R, 
                        filename, lengthIter, iteration);
            break;
        case 105: 
            transferParamModel_NormalVaryingVarsigmaUnknownPredict_i(model_R, 
                        filename, lengthIter, iteration);
            break;
        case 106: 
            transferParamModel_PoissonVaryingNotUseExpPredict_i(model_R, 
                        filename, lengthIter, iteration);
            break;
        case 109: 
            transferParamModel_BinomialVaryingPredict_i(model_R, 
                        filename, lengthIter, iteration);
            break;
        case 110: 
            transferParamModel_PoissonVaryingUseExpPredict_i(model_R, 
                        filename, lengthIter, iteration);
            break;
        default:
            error("unknown i_method_model in transferParamModel: %d", 
                                                        i_method_model);
            break;
    }
}

/* these functions only exists so that function can be tested from R with useSpecific = TRUE */
void
transferParamModel_NormalVaryingVarsigmaKnownPredict(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamModel_NormalVaryingVarsigmaKnownPredict_i(model_R, filename, 
                                lengthIter, iteration);
}
void
transferParamModel_NormalVaryingVarsigmaUnknownPredict(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamModel_NormalVaryingVarsigmaUnknownPredict_i(model_R, filename, 
                                lengthIter, iteration);
}
void
transferParamModel_PoissonVaryingNotUseExpPredict(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamModel_PoissonVaryingNotUseExpPredict_i(model_R, filename, 
                                lengthIter, iteration);
}
void
transferParamModel_BinomialVaryingPredict(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamModel_BinomialVaryingPredict_i(model_R, filename, 
                                lengthIter, iteration);
}
void
transferParamModel_PoissonVaryingUseExpPredict(SEXP model_R, const char *filename,
                                int lengthIter, int iteration)
{
    transferParamModel_PoissonVaryingUseExpPredict_i(model_R, filename, 
                                lengthIter, iteration);
}


/* ******************************************************************************** */
/* Functions for predicting models. ************************************************* */
/* ******************************************************************************** */

/* Note that these functions modify the models in place, 
   unlike the R versions, or the R-visible C versions
   created in init.c. */

/* inline functions */


static __inline__ void
predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict_i(SEXP object, SEXP y_R)
{
    /*  object <- predictPriorsBetas(object)
        object <- predictBetas(object)
        object <- updateTheta_NormalVarying(object, y = y) */
    predictPriorsBetas(object);
    predictBetas(object);
    updateTheta_NormalVarying(object, y_R);
}

static __inline__ void
predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict_i(SEXP object, SEXP y_R)
{
    /*  object <- predictPriorsBetas(object)
        object <- predictBetas(object)
        object <- updateTheta_NormalVarying(object, y = y) */
    predictPriorsBetas(object);
    predictBetas(object);
    updateTheta_NormalVarying(object, y_R);
}

static __inline__ void
predictModelNotUseExp_PoissonVaryingNotUseExpPredict_i(SEXP object, SEXP y_R)
{
    /*  object <- predictPriorsBetas(object)
        object <- predictBetas(object)
        object <- updateTheta_PoissonVaryingNotUseExp(object, y = y) */
    predictPriorsBetas(object);
    predictBetas(object);
    updateTheta_PoissonVaryingNotUseExp(object, y_R);
}

/* models using exposure */

static __inline__ void
predictModelUseExp_BinomialVaryingPredict_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    /*  object <- predictPriorsBetas(object)
        object <- predictBetas(object)
        object <- updateTheta_BinomialVarying(object, y = y, exposure = exposure) */
    predictPriorsBetas(object);
    predictBetas(object);
    updateTheta_BinomialVarying(object, y_R, exposure_R);
}

static __inline__ void
predictModelUseExp_PoissonVaryingUseExpPredict_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    /*  object <- predictPriorsBetas(object)
        object <- predictBetas(object)
        object <- updateTheta_PoissonVaryingUseExp(object, y = y, exposure = exposure) */
    predictPriorsBetas(object);
    predictBetas(object);
    updateTheta_PoissonVaryingUseExp(object, y_R, exposure_R);
}

static __inline__ void
predictModelUseExp_PoissonBinomialMixturePredict_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    /*  do nothing */
}

void
predictModelNotUseExp(SEXP object, SEXP y_R) 
{
    int i_method_model = *(INTEGER(GET_SLOT(object, iMethodModel_sym)));
    
    predictModelNotUseExp_Internal(object, y_R, i_method_model);

}

void
predictModelNotUseExp_Internal(SEXP object, SEXP y_R, int i_method_model) 
{
    switch(i_method_model)
    {
        case 104:
            predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict_i(object, y_R);
            break;
        case 105:
            predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict_i(object, y_R);
            break;
        case 106:
            predictModelNotUseExp_PoissonVaryingNotUseExpPredict_i(object, y_R);
            break;
        default:
            error("unknown i_method_model: %d", i_method_model);
            break;
   }
}


void
predictModelUseExp(SEXP object, SEXP y_R, SEXP exposure_R) 
{
    int i_method_model = *(INTEGER(GET_SLOT(object, iMethodModel_sym)));
        
    predictModelUseExp_Internal(object, y_R, exposure_R, i_method_model);
}

void
predictModelUseExp_Internal(SEXP object, SEXP y_R, SEXP exposure_R,
                            int i_method_model) 
{
    switch(i_method_model)
    {
        case 109: 
            predictModelUseExp_BinomialVaryingPredict_i(object, y_R, exposure_R);
            break;
        case 110: 
            predictModelUseExp_PoissonVaryingUseExpPredict_i(object, y_R, exposure_R);
            break;
        case 111: 
            predictModelUseExp_PoissonBinomialMixturePredict_i(object, y_R, exposure_R);
            break;
            
        default:
            error("unknown i_method_model: %d", i_method_model);
            break;
    }
}

/* specific functions for models not using exposure */
void
predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict(SEXP object, SEXP y_R) 
{
    predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict_i(object, y_R);
    
}

void
predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict(SEXP object, SEXP y_R) 
{
    predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict_i(object, y_R);
    
}

void
predictModelNotUseExp_PoissonVaryingNotUseExpPredict(SEXP object, SEXP y_R) 
{
    predictModelNotUseExp_PoissonVaryingNotUseExpPredict_i(object, y_R);
    
}

/* specific functions for models using exposure */

void
predictModelUseExp_BinomialVaryingPredict(SEXP object, SEXP y_R, SEXP exposure_R)
{
    predictModelUseExp_BinomialVaryingPredict_i(object, y_R, exposure_R);
    
}

void
predictModelUseExp_PoissonVaryingUseExpPredict(SEXP object, SEXP y_R, SEXP exposure_R)
{
    predictModelUseExp_PoissonVaryingUseExpPredict_i(object, y_R, exposure_R);
    
}

void
predictModelUseExp_PoissonBinomialMixturePredict(SEXP object, SEXP y_R, SEXP exposure_R)
{
    predictModelUseExp_PoissonBinomialMixturePredict_i(object, y_R, exposure_R);
    
}

/* ******************************************************************************** */
/* Functions for updating models. ************************************************* */
/* ******************************************************************************** */

/* Note that these functions modify the models in place, 
   unlike the R versions, or the R-visible C versions
   created in init.c. */

/* inline functions */

/* models not using exposure */


static __inline__ void
updateModelNotUseExp_NormalVaryingVarsigmaKnown_i(SEXP object, SEXP y_R)
{
    updateTheta_NormalVarying(object, y_R);
    updateSigma_Varying_General(object, identity);
    updateBetasAndPriorsBetas_General(object, identity);
}

static __inline__ void
updateModelNotUseExp_NormalVaryingVarsigmaUnknown_i(SEXP object, SEXP y_R)
{
    updateTheta_NormalVarying(object, y_R);
    updateVarsigma(object, y_R);
    updateSigma_Varying_General(object, identity);
    updateBetasAndPriorsBetas_General(object, identity);
}

static __inline__ void
updateModelNotUseExp_PoissonVaryingNotUseExp_i(SEXP object, SEXP y_R)
{
    updateTheta_PoissonVaryingNotUseExp(object, y_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}

static __inline__ void
updateModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain_i(SEXP object, SEXP y_R)
{
    updateTheta_NormalVaryingAgCertain(object, y_R);
    updateSigma_Varying_General(object, identity);
    updateBetasAndPriorsBetas_General(object, identity);
}

static __inline__ void
updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain_i(SEXP object, SEXP y_R)
{
    updateTheta_NormalVaryingAgCertain(object, y_R);
    updateVarsigma(object, y_R);
    updateSigma_Varying_General(object, identity);
    updateBetasAndPriorsBetas_General(object, identity);
}

static __inline__ void
updateModelNotUseExp_PoissonVaryingNotUseExpAgCertain_i(SEXP object, SEXP y_R)
{
    updateTheta_PoissonVaryingNotUseExpAgCertain(object, y_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}

static __inline__ void
updateModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal_i(SEXP object, SEXP y_R)
{
    updateTheta_NormalVaryingAgCertain(object, y_R);
    updateThetaAndValueAgNormal_Normal(object, y_R);
    updateSigma_Varying_General(object, identity);
    updateBetasAndPriorsBetas_General(object, identity);
}

static __inline__ void
updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal_i(SEXP object, SEXP y_R)
{
    updateTheta_NormalVaryingAgCertain(object, y_R);
    updateThetaAndValueAgNormal_Normal(object, y_R);
    updateVarsigma(object, y_R);
    updateSigma_Varying_General(object, identity);
    updateBetasAndPriorsBetas_General(object, identity);
}

static __inline__ void
updateModelNotUseExp_NormalVaryingVarsigmaKnownAgFun_i(SEXP object, SEXP y_R)
{
    updateThetaAndValueAgFun_Normal(object, y_R);
    updateSigma_Varying_General(object, identity);
    updateBetasAndPriorsBetas_General(object, identity);
}

static __inline__ void
updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun_i(SEXP object, SEXP y_R)
{
    updateThetaAndValueAgFun_Normal(object, y_R);
    updateVarsigma(object, y_R);
    updateSigma_Varying_General(object, identity);
    updateBetasAndPriorsBetas_General(object, identity);
}

static __inline__ void
updateModelNotUseExp_PoissonVaryingNotUseExpAgNormal_i(SEXP object, SEXP y_R)
{
    updateTheta_PoissonVaryingNotUseExpAgCertain(object, y_R);
    updateThetaAndValueAgNormal_PoissonNotUseExp(object, y_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}

/* problem ScaleVec non positive on tests with n.test <- 20 */
static __inline__ void
updateModelNotUseExp_PoissonVaryingNotUseExpAgFun_i(SEXP object, SEXP y_R)
{
    updateThetaAndValueAgFun_PoissonNotUseExp(object, y_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}

static __inline__ void
updateModelNotUseExp_PoissonVaryingNotUseExpAgPoisson_i(SEXP object, SEXP y_R)
{
    updateTheta_PoissonVaryingNotUseExpAgCertain(object, y_R);
    updateThetaAndValueAgPoisson_PoissonNotUseExp(object, y_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}

/* models using exposure */




static __inline__ void
updateModelUseExp_BinomialVarying_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateTheta_BinomialVarying(object, y_R, exposure_R);
    updateSigma_Varying_General(object, logit);
    updateBetasAndPriorsBetas_General(object, logit);
}


static __inline__ void
updateModelUseExp_PoissonVarying_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateTheta_PoissonVaryingUseExp(object, y_R, exposure_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}

static __inline__ void
updateModelUseExp_PoissonBinomialMixture_i
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    /* do nothing */
}

static __inline__ void
updateModelUseExp_BinomialVaryingAgCertain_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateTheta_BinomialVaryingAgCertain(object, y_R, exposure_R);
    updateSigma_Varying_General(object, logit);
    updateBetasAndPriorsBetas_General(object, logit);
}

static __inline__ void
updateModelUseExp_BinomialVaryingAgNormal_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateTheta_BinomialVaryingAgCertain(object, y_R, exposure_R);
    updateThetaAndValueAgNormal_Binomial(object, y_R, exposure_R);
    updateSigma_Varying_General(object, logit);
    updateBetasAndPriorsBetas_General(object, logit);
}

static __inline__ void
updateModelUseExp_BinomialVaryingAgFun_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateThetaAndValueAgFun_Binomial(object, y_R, exposure_R);
    updateSigma_Varying_General(object, logit);
    updateBetasAndPriorsBetas_General(object, logit);
}

static __inline__ void
updateModelUseExp_PoissonVaryingUseExpAgCertain_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateTheta_PoissonVaryingUseExpAgCertain(object, y_R, exposure_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}

static __inline__ void
updateModelUseExp_PoissonVaryingUseExpAgNormal_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateTheta_PoissonVaryingUseExpAgCertain(object, y_R, exposure_R);
    updateThetaAndValueAgNormal_PoissonUseExp(object, y_R, exposure_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}

/* problem ScaleVec non positive on tests with n.test <- 20 */
static __inline__ void
updateModelUseExp_PoissonVaryingUseExpAgFun_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateThetaAndValueAgFun_PoissonUseExp(object, y_R, exposure_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}


static __inline__ void
updateModelUseExp_PoissonVaryingUseExpAgPoisson_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateTheta_PoissonVaryingUseExpAgCertain(object, y_R, exposure_R);
    updateThetaAndValueAgPoisson_PoissonUseExp(object, y_R, exposure_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}


static __inline__ void
updateModelUseExp_PoissonVaryingUseExpAgLife_i(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateThetaAndValueAgLife_PoissonUseExp(object, y_R, exposure_R);
    updateSigma_Varying_General(object, log);
    updateBetasAndPriorsBetas_General(object, log);
}


void
updateModelNotUseExp(SEXP object, SEXP y_R) 
{
    int i_method_model = *(INTEGER(GET_SLOT(object, iMethodModel_sym)));
    
    updateModelNotUseExp_Internal(object, y_R, i_method_model);
}

void
updateModelNotUseExp_Internal(SEXP object, SEXP y_R, int i_method_model) 
{
    switch(i_method_model)
    {
        case 4:
            updateModelNotUseExp_NormalVaryingVarsigmaKnown_i(object, y_R);
            break;
        case 5:
            updateModelNotUseExp_NormalVaryingVarsigmaUnknown_i(object, y_R);
            break;
        case 6:
            updateModelNotUseExp_PoissonVaryingNotUseExp_i(object, y_R);
            break;
        case 12:    
            updateModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain_i(object, y_R);
            break;
        case 13:    
            updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain_i(object, y_R);
            break;
        case 14:    
            updateModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal_i(object, y_R);
            break;  
        case 15:    
            updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal_i(object, y_R);
            break;
        case 16:    
            updateModelNotUseExp_PoissonVaryingNotUseExpAgCertain_i(object, y_R);
            break;    
        case 17:    
            updateModelNotUseExp_PoissonVaryingNotUseExpAgNormal_i(object, y_R);
            break;    
        case 22:    
            updateModelNotUseExp_PoissonVaryingNotUseExpAgPoisson_i(object, y_R);
            break; 
        case 24:    
            updateModelNotUseExp_NormalVaryingVarsigmaKnownAgFun_i(object, y_R);
            break; 
        case 25:    
            updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun_i(object, y_R);
            break; 
        case 26:    
            updateModelNotUseExp_PoissonVaryingNotUseExpAgFun_i(object, y_R);
            break; 
        default:
            error("unknown i_method_model: %d", i_method_model);
            break;
    }
}


void
updateModelUseExp(SEXP object, SEXP y_R, SEXP exposure_R) 
{
    int i_method_model = *(INTEGER(GET_SLOT(object, iMethodModel_sym)));
        
    updateModelUseExp_Internal(object, y_R, exposure_R, i_method_model);
}

void
updateModelUseExp_Internal(SEXP object, SEXP y_R, SEXP exposure_R,
                            int i_method_model) 
{
    switch(i_method_model)
    {
        case 9:
            updateModelUseExp_BinomialVarying_i(object, y_R, exposure_R);
            break;
        case 10:
            updateModelUseExp_PoissonVarying_i(object, y_R, exposure_R);
            break;
        case 11:
            updateModelUseExp_PoissonBinomialMixture_i(object, y_R, exposure_R);
            break;
        case 18: 
        updateModelUseExp_BinomialVaryingAgCertain_i(object, y_R, exposure_R);
            break;
        case 19:    
            updateModelUseExp_BinomialVaryingAgNormal_i(object, y_R, exposure_R);
            break;
        case 20:    
            updateModelUseExp_PoissonVaryingUseExpAgCertain_i(object, y_R, exposure_R);
            break;
        case 21:    
            updateModelUseExp_PoissonVaryingUseExpAgNormal_i(object, y_R, exposure_R);
            break;
        case 23:    
            updateModelUseExp_PoissonVaryingUseExpAgPoisson_i(object, y_R, exposure_R);
            break;
        case 27:    
            updateModelUseExp_BinomialVaryingAgFun_i(object, y_R, exposure_R);
            break;
        case 28:    
            updateModelUseExp_PoissonVaryingUseExpAgFun_i(object, y_R, exposure_R);
            break;
        case 29:    
            updateModelUseExp_PoissonVaryingUseExpAgLife_i(object, y_R, exposure_R);
            break;
        default:
            error("unknown i_method_model: %d", i_method_model);
            break;
    }
}

/* specific functions for models not using exposure */
void
updateModelNotUseExp_NormalVaryingVarsigmaKnown(SEXP object, SEXP y_R) 
{
    updateModelNotUseExp_NormalVaryingVarsigmaKnown_i(object, y_R);
    
}

void
updateModelNotUseExp_NormalVaryingVarsigmaUnknown(SEXP object, SEXP y_R) 
{
    updateModelNotUseExp_NormalVaryingVarsigmaUnknown_i(object, y_R);
    
}

void
updateModelNotUseExp_PoissonVaryingNotUseExp(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_PoissonVaryingNotUseExp_i(object, y_R);
}

void
updateModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain_i(object, y_R);
}

void
updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain_i(object, y_R);
}

void
updateModelNotUseExp_PoissonVaryingNotUseExpAgCertain(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_PoissonVaryingNotUseExpAgCertain_i(object, y_R);
}

void
updateModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal_i(object, y_R);
}

void
updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal_i(object, y_R);
}

void
updateModelNotUseExp_NormalVaryingVarsigmaKnownAgFun(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_NormalVaryingVarsigmaKnownAgFun_i(object, y_R);
}

void
updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun_i(object, y_R);
}

void
updateModelNotUseExp_PoissonVaryingNotUseExpAgNormal(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_PoissonVaryingNotUseExpAgNormal_i(object, y_R);
}

void
updateModelNotUseExp_PoissonVaryingNotUseExpAgFun(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_PoissonVaryingNotUseExpAgFun_i(object, y_R);
}


void
updateModelNotUseExp_PoissonVaryingNotUseExpAgPoisson(SEXP object, SEXP y_R)
{
    updateModelNotUseExp_PoissonVaryingNotUseExpAgPoisson_i(object, y_R);
}

/* specific functions for models using exposure */
void
updateModelUseExp_BinomialVarying(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_BinomialVarying_i(object, y_R, exposure_R);
    
}


void
updateModelUseExp_PoissonVarying(SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_PoissonVarying_i(object, y_R, exposure_R);
}

void
updateModelUseExp_PoissonBinomialMixture
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_PoissonBinomialMixture_i(object, y_R, exposure_R);
}

void
updateModelUseExp_BinomialVaryingAgCertain
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_BinomialVaryingAgCertain_i(object, y_R, exposure_R);
}

void
updateModelUseExp_BinomialVaryingAgNormal
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_BinomialVaryingAgNormal_i(object, y_R, exposure_R);
}

void
updateModelUseExp_BinomialVaryingAgFun
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_BinomialVaryingAgFun_i(object, y_R, exposure_R);
}

void
updateModelUseExp_PoissonVaryingUseExpAgCertain
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_PoissonVaryingUseExpAgCertain_i(object, y_R, exposure_R);
}

void
updateModelUseExp_PoissonVaryingUseExpAgNormal
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_PoissonVaryingUseExpAgNormal_i(object, y_R, exposure_R);
}

void
updateModelUseExp_PoissonVaryingUseExpAgFun
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_PoissonVaryingUseExpAgFun_i(object, y_R, exposure_R);
}

void
updateModelUseExp_PoissonVaryingUseExpAgPoisson
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_PoissonVaryingUseExpAgPoisson_i(object, y_R, exposure_R);
}

void
updateModelUseExp_PoissonVaryingUseExpAgLife
                                (SEXP object, SEXP y_R, SEXP exposure_R)
{
    updateModelUseExp_PoissonVaryingUseExpAgLife_i(object, y_R, exposure_R);
}
