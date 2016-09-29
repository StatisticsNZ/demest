
#include "Prior-methods.h"
#include "helper-functions.h"
#include "iterators-methods.h"
#include "demest.h"
#include "R_ext/BLAS.h" /* dtrmv */
/* for BLAS level 2 documention see www.netlib.org/blas/blas2-paper.ps */ 
#include "R_ext/Linpack.h" /* dqrsl dtrsl */

/* File "Prior-methods.c" contains C versions of functions 
 * from "Prior-methods.R". */



/* ******************************************************************************** */
/* Functions for updating priors. ************************************************* */
/* ******************************************************************************** */

/* Note that these functions modify the priors in place, 
   unlike the R versions, or the R-visible C versions
   created in init.c. */

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
    /* predictUBetaScaled(prior_R); changed by John 27 May 2016 */
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

void
predictPrior(SEXP prior_R) 
{
    int i_method_prior = *(INTEGER(GET_SLOT(prior_R, iMethodPrior_sym)));
    
    switch(i_method_prior)
    {
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
        default:
            error("unknown i_method_prior for predictPrior: %d", i_method_prior);
            break;
    }
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
    double val = values[0];  	/* should have just one value in it */

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
        default:
            error("unknown i_method_prior for transferParamPrior: %d", i_method_prior);
            break;
    }
}
