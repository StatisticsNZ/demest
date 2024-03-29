
#include "update-nongeneric.h"
#include "model-methods.h"
#include "Prior-methods.h"
#include "helper-functions.h"
#include "demest.h"


/* for BLAS level 2 documention see www.netlib.org/blas/blas2-paper.ps */
#include "R_ext/Linpack.h" /* dqrdc dqrsl dtrsl */
#include "R_ext/Lapack.h"


/* File "update-nongeneric.c" contains C versions of functions
 * from "update-nongeneric.R". */

/* ****************************************************************** */
/* *********** UPDATING STANDARD DEVIATION (VIA SLICE SAMPLING) ****** */
/* ****************************************************************** */

double
updateSDNorm(double sigma, double A, double nu, double V, int n, double max)
{
    double ans = -99;       /* default answer */

    double sigmaSq = sigma * sigma;
    double ASq = A * A;

    double nuPlusOne;
    double nPlusNuPlusOne;
    double nuASq;
    double n_nuASq;
    double twoASq;
    double f0;
    double numerator;
    double denominator;
    double fmax;

    int nu_finite = R_finite(nu);

    if (nu_finite) {
      nuPlusOne = nu + 1;
      nPlusNuPlusOne = n + nuPlusOne;
      nuASq = nu * ASq;
      n_nuASq = n * nuASq;
    }
    else {
      twoASq = 2 * ASq;
    }

    if (nu_finite)
      f0 = -n*log(sigma) - V/(2*sigmaSq) - nuPlusOne/2 * log(sigmaSq + nuASq);
    else
      f0 = -n*log(sigma) - V/(2*sigmaSq) - sigmaSq/twoASq;

    double e = rexp(1.0);
    double z = f0 - e;
    if (nu_finite) {
      numerator = V - n_nuASq + sqrt((V - n_nuASq)*(V - n_nuASq) + 4*nPlusNuPlusOne*V*nuASq);
      denominator = 2*nPlusNuPlusOne;
    }
    else {
      numerator = -n * ASq + sqrt(n * n * ASq * ASq + 4 * ASq * V);
      denominator = 2;
    }
    double sigma_star = sqrt(numerator/denominator);

    double sigma0_left = 0.5*sigma_star;
    double rootLeft = findOneRootLogPostSigmaNorm(sigma0_left,
                          z, A, nu, V, n, 0, sigma_star);
    int foundRootLeft = (rootLeft > 0);

    if (foundRootLeft) {

        int rootLessThanMax = 1;
        double sigma0_right = 1.5*sigma_star;

        if (R_finite(max)) {
            double maxSq = max * max;
        if (nu_finite)
          fmax = -n*log(max) - V/(2*maxSq) - nuPlusOne/2 * log(maxSq + nuASq);
        else
          fmax = -n*log(max) - V/(2*maxSq) - maxSq/twoASq;
            rootLessThanMax = (z > fmax);
            if (rootLessThanMax) {
                sigma0_right = 1.5*sigma_star;
                if (sigma0_right > max) {
                    sigma0_right = 0.5*sigma_star + 0.5*max;
                }
            }
        }
        /* else rootLessThanMax and sigma0_right stay as default values */

        double limitRight = max;
        int foundLimitRight = 1;

        if (rootLessThanMax) {
            double rootRight = findOneRootLogPostSigmaNorm(sigma0_right,
                                   z, A, nu, V, n, sigma_star, max);
            int foundRootRight = (rootRight > 0);

            if (foundRootRight) {
                limitRight = rootRight;
                foundLimitRight = 1;
            }
            else {
                foundLimitRight = 0;
            }
        }
        /* else limitRight and foundLimitRight stay as default values */

        if (foundLimitRight) {
            ans = runif(rootLeft, limitRight);
        }
    }
    else {
        double near_sigma_star = (rootLeft > -2);
        if (near_sigma_star) {
            ans = sigma_star;
        }
    }
    return ans;
}

double
updateSDRobust(double sigma, double A, double nuBeta, double nuTau,
                            double V, int n, double max)
{
    double ans = -99;       /* default answer */

    double ASq = A * A;
    double sigmaSq = sigma * sigma;
    double n_nuBeta = n*nuBeta;
    double nuTauPlusOne;
    double nuTauASq;

    int nuFinite = R_finite(nuTau);
    if (nuFinite) {
      nuTauPlusOne = nuTau + 1;
      nuTauASq = nuTau * ASq;
    }

    double f0;
    if (nuFinite)
      f0 = n_nuBeta*log(sigma) - nuBeta/2 *sigmaSq* V - nuTauPlusOne/2 * log(sigmaSq + nuTauASq);
    else
      f0 = n_nuBeta*log(sigma) - nuBeta/2 *sigmaSq* V - sigmaSq/(2*ASq);
    double e = rexp(1.0);
    double z = f0 - e;
    double sigma_star;
    if (nuFinite) {
      double H1 = nuBeta*V;
      double H2 = H1 * nuTauASq + nuTauPlusOne - n_nuBeta;
      double H3 = -n_nuBeta*nuTauASq;
      sigma_star = sqrt((-H2 +sqrt(H2*H2 - 4*H1*H3))/(2*H1));
    }
    else
      sigma_star = sqrt((n * nuBeta) / (nuBeta * V + 1/ASq));
    double sigma0_left = 0.5*sigma_star;
    double rootLeft = findOneRootLogPostSigmaRobust(sigma0_left,
                            z, A, nuBeta, nuTau, V, n, 0, sigma_star);
    int foundRootLeft = (rootLeft > 0);

    if (foundRootLeft) {
        int rootLessThanMax = 1;
        double sigma0_right = 1.5*sigma_star;

        if (R_finite(max)) {
            double maxSq = max * max;
        double fmax;
        if (nuFinite)
          fmax = n_nuBeta*log(max) - nuBeta/2 *maxSq* V - nuTauPlusOne/2 * log(maxSq + nuTauASq);
        else
          fmax = n_nuBeta*log(max) - nuBeta/2 *maxSq* V - maxSq/(2*ASq);
            rootLessThanMax = (z > fmax);

            if (rootLessThanMax) {
                sigma0_right = 1.5 * sigma_star;
                if (sigma0_right > max) {
                    sigma0_right = 0.5*sigma_star + 0.5*max;
                }
            }
        }
        /* else rootLessThanMax and sigma0_right stay as default values */

        double limitRight = max;
        int foundLimitRight = 1;

        if (rootLessThanMax) {
            double rootRight = findOneRootLogPostSigmaRobust(sigma0_right,
                            z, A, nuBeta, nuTau, V, n, sigma_star, max);
            int foundRootRight = (rootRight > 0);
            if (foundRootRight) {
                limitRight = rootRight;
                foundLimitRight = 1;
            }
            else {
                foundLimitRight = 0;
            }
        }
        /* else limitRight and foundLimitRight stay as default values */

        if (foundLimitRight) {
            ans = runif(rootLeft, limitRight);
        }
    }
    else {
        double near_sigma_star = (rootLeft > -2);
        if (near_sigma_star) {
            ans = sigma_star;
        }
    }


    return ans;
}


/* *************************************************************************** */
/*** UPDATING PRIORS ********************************************************* */
/* *************************************************************************** */

void
updateAlphaMix(SEXP prior_R)
{
    double *alpha = REAL(GET_SLOT(prior_R, alphaMix_sym));
    int nBeta = *INTEGER(GET_SLOT(prior_R, J_sym));
    int *indexClass = INTEGER(GET_SLOT(prior_R, indexClassMix_sym));

    double *prodVectors = REAL(GET_SLOT(prior_R, prodVectorsMix_sym));
    int pos1 = *INTEGER(GET_SLOT(prior_R, posProdVectors1Mix_sym));
    int pos2 = *INTEGER(GET_SLOT(prior_R, posProdVectors2Mix_sym));
    int nBetaNoAlong = *INTEGER(GET_SLOT(prior_R, nBetaNoAlongMix_sym));

    for (int iBeta = 0; iBeta < nBeta; ++iBeta) {
        int iClass = indexClass[iBeta] - 1;
        int iBetaNoAlong = (iBeta/pos1) * pos2 + iBeta%pos2;
        int iProd = iClass * nBetaNoAlong + iBetaNoAlong;
        alpha[iBeta] = prodVectors[iProd];
    }
}

void
updateAlphaDeltaDLMWithTrend(SEXP prior_R, double *betaTilde, int J)
{
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));

    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym)); /* vector, length (K+1)L */
    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym)); /* vector, length (K+1)L */

    double *G = REAL(GET_SLOT(prior_R, GWithTrend_sym)); /* 2x2 matrix */

    /* m a list of vector of doubles, len K+1, each vector length 2 */
    SEXP m_R;
    PROTECT(m_R = duplicate(GET_SLOT(prior_R, mWithTrend_sym)));
    /* m0 a list of vector of doubles, len L, each vector length 2 */
    SEXP m0_R = GET_SLOT(prior_R, m0WithTrend_sym);
    /* C a list of matrices of doubles, len K+1, each 2x2 */
    SEXP C_R;
    PROTECT(C_R = duplicate(GET_SLOT(prior_R, CWithTrend_sym)));
    /* a a list of vector of doubles, len K, each vector length 2 */
    SEXP a_R;
    PROTECT(a_R = duplicate(GET_SLOT(prior_R, aWithTrend_sym)));

    double *WSqrt = REAL(GET_SLOT(prior_R, WSqrt_sym)); /* 2x2 matrix */
    double *WSqrtInvG = REAL(GET_SLOT(prior_R, WSqrtInvG_sym)); /* 2x2 matrix */

    /* UC a list of matrices of doubles, len K+1, each matrix 2x2 */
    SEXP UC_R;
    PROTECT(UC_R = duplicate(GET_SLOT(prior_R, UC_sym)));
    /* DC a list of matrices of doubles, len K+1, each matrix 2x2 */
    SEXP DC_R;
    PROTECT(DC_R = duplicate(GET_SLOT(prior_R, DC_sym)));
    /* DCInv a list of matrices of doubles, len K+1, each matrix 2x2 */
    SEXP DCInv_R;
    PROTECT(DCInv_R = duplicate(GET_SLOT(prior_R, DCInv_sym)));
    /* DRInv a list of matrices of doubles, len K, each matrix 2x2 */
    SEXP DRInv_R;
    PROTECT(DRInv_R = duplicate(GET_SLOT(prior_R, DRInv_sym)));
    /* UR a list of matrices of doubles, len K, each matrix 2x2 */
    SEXP UR_R;
    PROTECT(UR_R = duplicate(GET_SLOT(prior_R, UR_sym)));

    int hasLevel = *LOGICAL(GET_SLOT(prior_R, hasLevel_sym));

    double phi = *REAL(GET_SLOT(prior_R, phi_sym));
    double omegaAlpha = *REAL(GET_SLOT(prior_R, omegaAlpha_sym));
    double omegaDelta = *REAL(GET_SLOT(prior_R, omegaDelta_sym));

    double *v = (double *)R_alloc(J, sizeof(double));
    getV_Internal(v, prior_R, J);

    SEXP iterator_ad_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iterator_v_R = GET_SLOT(prior_R, iteratorV_sym);

    resetA(iterator_ad_R);
    resetA(iterator_v_R);

    int *indices_ad = INTEGER(GET_SLOT(iterator_ad_R, indices_sym));
    int *indices_v = INTEGER(GET_SLOT(iterator_v_R, indices_sym));

    int q = 2; /* dimensions */

    /* space for 2 vectors length q
     * and 4 matrices q  x q
     * and svd work  q*(5q + 7)
     * = total  9*q + 9*q*q*/
    int nWorkspace = 9*q*(1+q);
    double *workspace = (double *)R_alloc(nWorkspace, sizeof(double));

    /* divvie up the workspace */
    double *singulars = workspace; /* q */
    double *diag = workspace + q; /* q */
    double *work1 = workspace + 2*q; /* q x q checked */
    double *work2 = workspace + 2*q + q*q; /* q x q checked*/
    double *work3 = workspace + 2*q + 2*q*q; /* 2*q x q checked */
    double *work_svd = workspace + 2*q + 4* q*q; /* q*(5q + 7) */
    int n_work_svd = q*(5*q+7);
    int n_iwork_svd = 8*q;
    /* allocate 8q of int space for iwork in svd */
    int *iwork_svd = (int *)R_alloc(n_iwork_svd, sizeof(int));

    /* stuff needed for fortran routines */
    int info = 0;
    int lwork = q*(5*q+7); /* q*(5q + 7) */

    char jobz = 'O';
    char transN = 'N';
    char transT = 'T';

    int dim_n = q;

    double dummyU = 0; /* U not used */
    int ldu = 1;

    double alpha_blas_one = 1.0;
    double beta_blas_zero = 0.0;
    double beta_blas_one = 1.0;

    int inc_blas = 1;

    /* referenced when drawing final gamma, delta
     * the contents get changed in forward filter but
     * no need to set up the pointer each time */
    double *lastUC = REAL(VECTOR_ELT(UC_R, K));
    double *lastDC = REAL(VECTOR_ELT(DC_R, K));
    double *last_m = REAL(VECTOR_ELT(m_R, K));

    for (int l = 0; l < L; ++l) {

    if (!alongAllStrucZero[l]) {

        double *m0_l = REAL(VECTOR_ELT(m0_R, l));
        double *m_first = REAL(VECTOR_ELT(m_R, 0));
        memcpy(m_first, m0_l, q*sizeof(double));

        /* forward filter */
        for (int i = 0; i < K; ++i) {
        /*zero workspaces */
        memset(workspace, 0, nWorkspace*sizeof(double));
        memset(iwork_svd, 0, n_iwork_svd * sizeof(int));

        int iv = indices_v[i] - 1;
        double this_v = v[iv];

        double *thisDC = REAL(VECTOR_ELT(DC_R, i));
        double *thisUC = REAL(VECTOR_ELT(UC_R, i));
        double *thisUR = REAL(VECTOR_ELT(UR_R, i));
        double *thisDRInv = REAL(VECTOR_ELT(DRInv_R, i));

        /* t(UC[[i]]) %*% t(G)*/
        F77_CALL(dgemm)(&transT, &transT, &q, &q, &q,
                &alpha_blas_one, thisUC, &q, G, &q,
                &beta_blas_zero, work1, &q FCONE FCONE);
        /* after call, work1 contains t(UC[[i]]) %*% t(G) */
        /* DC[[i]] %*% t(UC[[i]]) %*% t(G)*/
        F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                &alpha_blas_one, thisDC, &q, work1, &q,
                &beta_blas_zero, work2, &q FCONE FCONE);
        /* after call, work2 contains DC[[i]] %*% t(UC[[i]]) %*% t(G) */

        /*  M.R <- rbind(DC[[i]] %*% t(UC[[i]]) %*% t(G), W.sqrt) */
        for (int colj = 0; colj < q; ++colj) {
            int sourceCol = q * colj;
            int destCol = 2*sourceCol;
            for (int rowi = 0; rowi < q; ++rowi) {
            int sourceIndex = sourceCol + rowi;
            int baseDestIndex = destCol + rowi;
            work3 [ baseDestIndex ]
                = work2[sourceIndex];
            work3 [ baseDestIndex+q ]
                = WSqrt[sourceIndex];
            }
        }
        /* work3 should now contain M.R <- rbind(DC[[i]] %*% t(UC[[i]]) %*% t(G),
           W.sqrt) */

        /* MR (A for dgesdd) in work 3 */
        /* svd.R <- svd(M.R, nu = 0)
         * provide work1 for VT */
        {
            int dim_m = (2*q);

            F77_CALL(dgesdd)(&jobz, &dim_m, &dim_n, work3,
                     &dim_m, singulars, &dummyU, &ldu, /* U not used */
                     work1, &dim_n, /* work1 for VT */
                     work_svd, &lwork,
                     iwork_svd, &info FCONE);
            if (info) error("error in dgesdd in updateAlphaDeltaDLMWithTrend: %d", info);
        }
        /* after call, work1 contains V**T */

        for (int rowi = 0; rowi < q; ++rowi) {

            double tmp = 1/singulars[rowi];
            diag[rowi] = ( R_finite( tmp ) ? tmp : 0.0 );

            for (int colj = 0; colj < q; ++colj) {
            int index = q*colj + rowi;
            thisUR[index] = work1[q*rowi + colj];
            if (rowi == colj) {
                thisDRInv[index] = diag[rowi];
            }
            }
        }

        /*M.C <- rbind(UR[[i]][c(1L, 3L)] / sqrt(v[indices.v[i]]),
          DR.inv[[i]]) */
        double sqrt_v = sqrt(this_v);
        memset(work3, 0, 2*q*q * sizeof(double));
        for (int colj = 0; colj < q; ++colj) {

            for (int rowi = 0; rowi < q; ++rowi) {
            int index = (q+1)*colj + rowi;
            if (rowi == 0) {
                work3 [ index ]
                = thisUR[q*colj + rowi]/sqrt_v;
            }
            if (rowi == colj) {
                work3 [ index + 1]
                = diag[rowi];
            }
            }
        }
        /* work3 now contains M.C (dimensions (q+1)*q */
        memset(work_svd, 0, n_work_svd*sizeof(double));
        memset(iwork_svd, 0, n_iwork_svd*(sizeof(int)));

        /* M.C (A for dgesdd) in work3 ((q+1) x q) */
        /* svd.C <- svd(M.C, nu = 0)
         * provide work1 for VT */
        {
            int dim_m = (q+1);
            F77_CALL(dgesdd)(&jobz, &dim_m, &dim_n, work3,
                     &dim_m, singulars, &dummyU, &ldu, /* U not used */
                     work1, &dim_n, /* work1 for VT */
                     work_svd, &lwork,
                     iwork_svd, &info FCONE);
            if (info) error("error in dgesdd in updateAlphaDeltaDLMWithTrend: %d", info);
        }

        double *newUC = REAL(VECTOR_ELT(UC_R, i+1));
        double *newDC = REAL(VECTOR_ELT(DC_R, i+1));
        double *newDCInv = REAL(VECTOR_ELT(DCInv_R, i+1));

        /* UC[[i + 1L]] <- UR[[i]] %*% svd.C$v*/
        F77_CALL(dgemm)(&transN, &transT, &q, &q, &q,
                &alpha_blas_one, thisUR, &q,
                work1, &q, /* work1 is t(svd.C$v)) */
                &beta_blas_zero, newUC, &q FCONE FCONE);
        /* after call, newUC contains UR[[i]] %*% svd.C$v */

        for (int rowi = 0; rowi < q; ++rowi) {
            double s = singulars[rowi];
            double tmp = 1/s;
            diag[rowi] = ( R_finite( tmp ) ? tmp : 0.0 );

            int colj = rowi;
            int index = q*colj + rowi;

            newDC[index] = diag[rowi];
            newDCInv[index] = s;
        }

        double *this_m = REAL(VECTOR_ELT(m_R, i)); /* vec len 2 */
        double *this_a = REAL(VECTOR_ELT(a_R, i)); /* vec len 2 */
        double *new_C = REAL(VECTOR_ELT(C_R, i+1)); /* matrix 2x2 */
        double *new_m = REAL(VECTOR_ELT(m_R, i+1)); /* vec len 2 */

        /* a[[i]] <- drop(G %*% m[[i]])*/
        F77_CALL(dgemv)(&transN, &q, &q, &alpha_blas_one, G,
                &q, this_m, &inc_blas, &beta_blas_zero,
                this_a, &inc_blas FCONE);
        /* this_a should have new a[[i]] */

        /* e <- betaTilde[indices.v[i]] - a[[i]][1L]*/
        double e = betaTilde[iv] - this_a[0];

        /*   C[[i + 1L]] <- UC[[i + 1L]] %*% DC[[i + 1L]] %*% DC[[i + 1L]] %*% t(UC[[i + 1L]]) */
        F77_CALL(dgemm)(&transN, &transT, &q, &q, &q,
                &alpha_blas_one, newDC, &q,
                newUC, &q,
                &beta_blas_zero, work1, &q FCONE FCONE);
        F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                &alpha_blas_one, newDC, &q,
                work1, &q,
                &beta_blas_zero, work2, &q FCONE FCONE);
        F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                &alpha_blas_one, newUC, &q,
                work2, &q,
                &beta_blas_zero, new_C, &q FCONE FCONE);

        /*  A <- C[[i + 1L]][1:2] / v[indices.v[i]]
            m[[i + 1L]] <- a[[i]] + A * e */
        for (int mi = 0; mi < q; ++mi) {
            new_m[mi] = this_a[mi] + e * new_C[mi] / this_v;
        }
        }
        /* draw final gamma, delta*/
        F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                &alpha_blas_one, lastUC, &q,
                lastDC, &q,
                &beta_blas_zero, work1, &q FCONE FCONE);
        /* sqrtC in work1 */

        {
        /* use diag to store z */
        double *z = diag;
        for (int zi = 0; zi < q; ++zi) {
            z[zi] = rnorm(0,1);
            /* use first q elements in work 2for theta and put
             * last_m into theta to start with*/
            work2[zi] = last_m[zi];
        }
        /* theta <- m[[K + 1L]] + drop(sqrt.C %*% z)*/
        F77_CALL(dgemv)(&transN, &q, &q, &alpha_blas_one, work1,
                &q, z, &inc_blas, &beta_blas_one,
                work2, &inc_blas FCONE);
        /* theta is in first q elements of work2 */
        }

        int index_ad = indices_ad[K] - 1;
        alpha[index_ad] = work2[0];
        delta[index_ad] = work2[1];

        /* use work_svd for the star spaces in backwards smooth*/
        double *UCstar = work_svd; /* qxq */
        double *DCstar = work_svd + q*q; /* qxq */
        double *sqrtCstar = work_svd + 2*q*q; /* qxq */
        double *theta_prev_minus_a = work_svd + 3*q*q; /* q */
        double *m_star = work_svd + 3*q*q + q; /* q */
        double *z = work_svd + 3*q*q + 2*q; /* q */
        double *theta_curr = work_svd + 3*q*q + 3*q; /* q */

        /* backward smooth */
        for (int i = K-1; i >= 0; --i) {

        int index_ad = indices_ad[i+1] - 1;
        int index_ad_now = indices_ad[i] - 1;

        /*zero workspaces */
        memset(workspace, 0, nWorkspace*sizeof(double));
        memset(iwork_svd, 0, n_iwork_svd * sizeof(int));

        double *testDCInv = REAL(VECTOR_ELT(DCInv_R, 0));
        double testDCInvFirst = testDCInv[0];

        double *this_m = REAL(VECTOR_ELT(m_R, i));

        if (!hasLevel) {
            if ( ( i == 0 ) && (!R_finite(testDCInvFirst)) ) {
            delta[index_ad_now] = alpha[index_ad] - alpha[index_ad_now];
            }
            else {
            double alphaIndexAd = alpha[index_ad];
            double *thisUC = REAL(VECTOR_ELT(UC_R, i));
            double *thisDCInv = REAL(VECTOR_ELT(DCInv_R, i));

            /* C.inv <- UC[[i + 1L]] %*% DC.inv[[i + 1L]]
               %*% DC.inv[[i + 1L]] %*% t(UC[[i + 1L]]) */
            F77_CALL(dgemm)(&transN, &transT, &q, &q, &q,
                    &alpha_blas_one, thisDCInv, &q,
                    thisUC, &q,
                    &beta_blas_zero, work1, &q FCONE FCONE);
            F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                    &alpha_blas_one, thisDCInv, &q,
                    work1, &q,
                    &beta_blas_zero, work2, &q FCONE FCONE);
            F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                    &alpha_blas_one, thisUC, &q,
                    work2, &q,
                    &beta_blas_zero, work1, &q FCONE FCONE);
            /* CInv in work1 */

            /* sigma.inv.1 <- C.inv[1L] */
            /* sigma.inv.2 <- C.inv[2L] */
            /* sigma.inv.3 <- C.inv[3L] */
            /* sigma.inv.4 <- C.inv[4L] + phi^2 / omega.delta^2 */
            double sigma_inv_1 = work1[0];
            double sigma_inv_2 = work1[1];
            double sigma_inv_3 = work1[2];
            double sigma_inv_4 = work1[3] + phi * phi / (omegaDelta * omegaDelta);

            /* determinant <- sigma.inv.1 * sigma.inv.4 - sigma.inv.2 * sigma.inv.3 */
            /* sigma.1 <- sigma.inv.4 / determinant */
            /* sigma.2 <- -1 * sigma.inv.3 / determinant */
            /* sigma.3 <- -1 * sigma.inv.2 / determinant */
            /* sigma.4 <- sigma.inv.1 / determinant */
            double determinant = sigma_inv_1 * sigma_inv_4 - sigma_inv_2 * sigma_inv_3;
            double sigma_1 = sigma_inv_4 / determinant;
            double sigma_2 = -1 * sigma_inv_3 / determinant;
            double sigma_3 = -1 * sigma_inv_2 / determinant;
            double sigma_4 = sigma_inv_1 / determinant;

            /* mu.inner.1 <- C.inv[1L] * m[[i + 1L]][1L] + C.inv[3L] * m[[i + 1L]][2L] */
            /* mu.inner.2 <- (C.inv[2L] * m[[i + 1L]][1L] + C.inv[4L] * m[[i + 1L]][2L] */
            /*         + phi * delta[indices.ad[i + 2L]] / omega.delta^2) */
            double *this_m = REAL(VECTOR_ELT(m_R, i));
            double mu_inner_1 = work1[0] * this_m[0] + work1[2] * this_m[1];
            double mu_inner_2 = (work1[1] * this_m[0] + work1[3] * this_m[1]
                         + phi * delta[index_ad] / (omegaDelta * omegaDelta));

            /* mu.1 <- sigma.1 * mu.inner.1 + sigma.3 * mu.inner.2 */
            /* mu.2 <- sigma.2 * mu.inner.1 + sigma.4 * mu.inner.2 */
            double mu_1 = sigma_1 * mu_inner_1 + sigma_3 * mu_inner_2;
            double mu_2 = sigma_2 * mu_inner_1 + sigma_4 * mu_inner_2;

            /* mu.star.1 <- mu.1 */
            /* mu.star.2 <- mu.1 + mu.2 */
            double mu_star_1 = mu_1;
            double mu_star_2 = mu_1 + mu_2;

            /* sigma.star.1 <- sigma.1 */
            /* sigma.star.2 <- sigma.1 + sigma.2 */
            /* sigma.star.3 <- sigma.1 + sigma.3 */
            /* sigma.star.4 <- sigma.1 + sigma.2 + sigma.3 + sigma.4 */
            double sigma_star_1 = sigma_1;
            double sigma_star_2 = sigma_1 + sigma_2;
            double sigma_star_3 = sigma_1 + sigma_3;
            double sigma_star_4 = sigma_1 + sigma_2 + sigma_3 + sigma_4;

            /* rho.star.sq <- sigma.star.2 * sigma.star.3 / (sigma.star.1 * sigma.star.4) */
            double rho_star_sq = sigma_star_2 * sigma_star_3 / (sigma_star_1 * sigma_star_4);

            /* mean.alpha <- (mu.star.1 + sqrt(rho.star.sq * sigma.star.1 / sigma.star.4) */
            /*         * (alpha[indices.ad[i + 2L]] - mu.star.2)) */
            /* var.alpha <- (1 - rho.star.sq) * sigma.star.1 */
            double mean_alpha = (mu_star_1 + sqrt(rho_star_sq * sigma_star_1 / sigma_star_4)
                         * (alpha[index_ad] - mu_star_2));
            double var_alpha = (1 - rho_star_sq) * sigma_star_1;

            /* alpha.curr <- stats::rnorm(n = 1L, */
            /*                 mean = mean.alpha, */
            /*                 sd = sqrt(var.alpha)) */
            double alpha_curr = rnorm(mean_alpha, sqrt(var_alpha));

            /* delta.curr <- alpha[indices.ad[i + 2L]] - alpha.curr */
            /* alpha[indices.ad[i + 1L]] <- alpha.curr */
            /* delta[indices.ad[i + 1L]] <- delta.curr */
            double delta_curr = alphaIndexAd - alpha_curr;
            alpha[index_ad_now] = alpha_curr;
            delta[index_ad_now] = delta_curr;
            } /* end if (i == 0) */
        } /* end if !hasLevel */
        else {

            double *thisDCInv = REAL(VECTOR_ELT(DCInv_R, i));

            if ( ( i == 0 ) && (!R_finite(testDCInvFirst)) ) {
            /* prec.delta.0 <- DC.inv[[1L]][4L]  */
            /* prec.alpha <- 1 / omega.alpha^2 */
            /* prec.delta.1 <- phi^2 / omega.delta^2 */
            double precDelta0 = thisDCInv[3];
            double precAlpha = 1 / (omegaAlpha * omegaAlpha);
            double precDelta1 = phi * phi / (omegaDelta * omegaDelta);

            /* var.delta.curr <- 1 / (prec.delta.0 + prec.alpha + prec.delta.1) */
            /* mean.delta.curr <- var.delta.curr * (prec.delta.0 * m[[1L]][2L] + prec.alpha * alpha[indices.ad[2L]] */
            /*       + prec.delta.1 * delta[indices.ad[2L]] / phi) */
            /* delta.curr <- rnorm(n = 1L, */
            /*                     mean = mean, */
            /*                     sd = sqrt(var)) */
            /* delta[indices.ad[1L]] <- delta.curr */
            double varDeltaCurr = 1 / (precDelta0 + precAlpha + precDelta1);
            double meanDeltaCurr = varDeltaCurr * (precDelta0 * this_m[1]
                                   + precAlpha * alpha[index_ad]
                                   + precDelta1 * delta[index_ad] / phi);
            delta[index_ad_now] = rnorm(meanDeltaCurr, sqrt(varDeltaCurr));
            }
            else {
            double *thisUR = REAL(VECTOR_ELT(UR_R, i));
            double *thisDRInv = REAL(VECTOR_ELT(DRInv_R, i));
            double *this_C = REAL(VECTOR_ELT(C_R, i));
            double *thisUC = REAL(VECTOR_ELT(UC_R, i));
            double *this_a = REAL(VECTOR_ELT(a_R, i));

            /*R.inv <- (UR[[i + 1L]] %*% DR.inv[[i + 1L]]
              %*% DR.inv[[i + 1L]] %*% t(UR[[i + 1L]]))   */
            F77_CALL(dgemm)(&transN, &transT, &q, &q, &q,
                    &alpha_blas_one, thisDRInv, &q,
                    thisUR, &q,
                    &beta_blas_zero, work1, &q FCONE FCONE);
            F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                    &alpha_blas_one, thisDRInv, &q,
                    work1, &q,
                    &beta_blas_zero, work2, &q FCONE FCONE);
            F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                    &alpha_blas_one, thisUR, &q,
                    work2, &q,
                    &beta_blas_zero, work1, &q FCONE FCONE);
            /* RInv in work1 */

            /*B <- C[[i + 1L]] %*% t(G) %*% R.inv*/
            F77_CALL(dgemm)(&transT, &transN, &q, &q, &q,
                    &alpha_blas_one, G, &q,
                    work1, &q,
                    &beta_blas_zero, work2, &q FCONE FCONE);
            F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                    &alpha_blas_one, this_C, &q,
                    work2, &q,
                    &beta_blas_zero, work1, &q FCONE FCONE);
            /* B in work1 */

            /*M.C.star <- rbind(W.sqrt.inv.G,
              DC.inv[[i + 1L]] %*% t(UC[[i + 1L]])) */
            F77_CALL(dgemm)(&transN, &transT, &q, &q, &q,
                    &alpha_blas_one, thisDCInv, &q,
                    thisUC, &q,
                    &beta_blas_zero, work2, &q FCONE FCONE);
            /* DC.inv[[i + 1L]] %*% t(UC[[i + 1L]]) in work 2*/

            /* put M.C.star into work 3 ( dimensions 2q x q ) */
            for (int colj = 0; colj < q; ++colj) {
                int sourceCol = q * colj;
                int destCol = 2*sourceCol;
                for (int rowi = 0; rowi < q; ++rowi) {
                int sourceIndex = sourceCol + rowi;
                int baseDestIndex = destCol + rowi;
                work3 [ baseDestIndex ]
                    = WSqrtInvG[sourceIndex];
                work3 [ baseDestIndex+q ]
                    = work2[sourceIndex];
                }
            }
            /* work3 should now contain M.C.star <- rbind(W.sqrt.inv.G,
               DC.inv[[i + 1L]] %*% t(UC[[i + 1L]])) */

            /*svd.C.star <- svd(M.C.star, nu = 0)*/
            {
                int dim_m = (2*q);

                F77_CALL(dgesdd)(&jobz, &dim_m, &dim_n, work3,
                         &dim_m, singulars, &dummyU, &ldu, /* U not used */
                         work2, &dim_n, /* work2 for VT */
                         work_svd, &lwork,
                         iwork_svd, &info FCONE);
                if (info) error("error in dgesdd in updateAlphaDeltaDLMWithTrend: %d", info);
            }
            /* after call, work2 contains V**T */

            memset(DCstar, 0, q*q * sizeof(double));
            for (int rowi = 0; rowi < q; ++rowi) {

                double tmp = 1/singulars[rowi];
                diag[rowi] = ( R_finite( tmp ) ? tmp : 0.0 );

                for (int colj = 0; colj < q; ++colj) {
                int index = q*colj + rowi;
                UCstar[index] = work2[q*rowi + colj];
                if (rowi == colj) {
                    DCstar[index] = diag[rowi];
                }
                }
            }

            /* sqrt.C.star <- UC.star %*% DC.star */
            F77_CALL(dgemm)(&transN, &transN, &q, &q, &q,
                    &alpha_blas_one, UCstar, &q,
                    DCstar, &q,
                    &beta_blas_zero, sqrtCstar, &q FCONE FCONE);


            theta_prev_minus_a[0] = alpha[index_ad] - this_a[0];
            theta_prev_minus_a[1] = delta[index_ad] - this_a[1];

            /* store z */
            for (int zi = 0; zi < q; ++zi) {
                z[zi] = rnorm(0,1);
                /* put this_m into m_star to start with */
                m_star[zi] = this_m[zi];
            }
            /* m.star <- m[[i + 1L]] + drop(B %*% (theta.prev - a[[i + 1L]]))*/
            F77_CALL(dgemv)(&transN, &q, &q, &alpha_blas_one, work1, /* B in work1 */
                    &q, theta_prev_minus_a, &inc_blas, &beta_blas_one,
                    m_star, &inc_blas FCONE);
            /* m_star complete */

            /*theta.curr <- m.star + drop(sqrt.C.star %*% z)*/
            /* put m_star into theta_curr to start with */
            memcpy(theta_curr, m_star, q*sizeof(double));
            F77_CALL(dgemv)(&transN, &q, &q, &alpha_blas_one, sqrtCstar,
                    &q, z, &inc_blas, &beta_blas_one,
                    theta_curr, &inc_blas FCONE);
            /* theta_curr complete */

            int index_ad_now = indices_ad[i] - 1;
            alpha[index_ad_now] = theta_curr[0];
            delta[index_ad_now] = theta_curr[1];
            } /* end else (i == 0) */
        } /* end else !hasLevel */
        }
        } /* end !alongAllStructuralZero[l] */
        advanceA(iterator_ad_R);
        advanceA(iterator_v_R);
    }
    /* only alphaDLM and deltaDLM get updated in the prior */
    UNPROTECT(8);
}

void
updateAlphaDLMNoTrend(SEXP prior_R, double *betaTilde, int J)
{
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));

    double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym)); /* vector, length (K+1)L */
    /* m0 a list of vector of doubles, len L, each vector length 1 */
    SEXP m0_R = GET_SLOT(prior_R, m0NoTrend_sym);
    /* C a list of vector of doubles, len K+1, each vector length 1 */
    SEXP C_R = GET_SLOT(prior_R, CNoTrend_sym);

    double phi = *REAL(GET_SLOT(prior_R, phi_sym));
    double omega = *REAL(GET_SLOT(prior_R, omegaAlpha_sym));

    double phiSq = phi * phi;
    double omegaSq = omega * omega;

    double *v = (double *)R_alloc(J, sizeof(double));
    getV_Internal(v, prior_R, J);

    double tolerance = *REAL(GET_SLOT(prior_R, tolerance_sym));

    SEXP iterator_a_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iterator_v_R = GET_SLOT(prior_R, iteratorV_sym);

    resetA(iterator_a_R);
    resetA(iterator_v_R);

    double *m = (double *)R_alloc((K+1), sizeof(double));
    double *C = (double *)R_alloc((K+1), sizeof(double));
    double *a = (double *)R_alloc((K), sizeof(double));
    double *R = (double *)R_alloc(K, sizeof(double));

    /* just need first element for C */
    C[0] = *REAL(VECTOR_ELT(C_R, 0));

    int *indices_a = INTEGER(GET_SLOT(iterator_a_R, indices_sym));
    int *indices_v = INTEGER(GET_SLOT(iterator_v_R, indices_sym));

    for (int l = 0; l < L; ++l) {

    if (!alongAllStrucZero[l]) {

        m[0] = *REAL(VECTOR_ELT(m0_R, l));

        /* forward filter */
        for (int i = 0; i < K; ++i) {
        int index_v = indices_v[i] - 1;
        double this_a = phi * m[i];
        a[i] = this_a;
        double this_R = phiSq * C[i] + omegaSq;
        R[i] = this_R;
        double q = this_R + v[index_v];
        double e = betaTilde[index_v] - this_a;
        double A = this_R/q;
        m[i+1] = this_a + A*e;
        C[i+1] = this_R - A*A*q;
        }

        int index_a = indices_a[K] - 1;
        double last_alpha = rnorm( m[K], sqrt(C[K]) );
        alpha[index_a] = last_alpha;

        /* backward sample */
        for (int i = K-1; i >= 0; --i) {
        if ((i > 0) || (C[0] > tolerance)) {
            double B = C[i] * phi / R[i];
            double mStar = m[i] + B * (last_alpha - a[i]);
            double CStar = C[i] - B*B*R[i];
            index_a = indices_a[i] - 1;
            last_alpha = rnorm( mStar, sqrt(CStar) );
            alpha[index_a] = last_alpha;
        }
        }
    } /* end if (!alongAllStrucZero[l]) */
    advanceA(iterator_a_R);
    advanceA(iterator_v_R);
    } /* end for (int l = 0; l < L; l++) */

    /* only alphaDLM gets updated in the prior */
}


void
updateEta(SEXP prior_R, double* beta, int J)
{
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    double *z = REAL(GET_SLOT(prior_R, Z_sym)); /* J x P */

    double *eta = REAL(GET_SLOT(prior_R, eta_sym)); /* length P */

    double AEtaIntercept = *REAL(GET_SLOT(prior_R, AEtaIntercept_sym));

    double *meanEtaCoef = REAL(GET_SLOT(prior_R, meanEtaCoef_sym)); /* length P-1 */

    double *UEtaCoef = REAL(GET_SLOT(prior_R, UEtaCoef_sym)); /* length P-1 */

    double *v = (double *)R_alloc(J, sizeof(double));

    getV_Internal(v, prior_R, J); /* fill in v */

    /* one malloc for all space at once
     * need JP + 2* PP + 3P = P(J+2*P+3)*/
    double *work = (double *)R_alloc(P*(J+2*P+3), sizeof(double));
    double *work1 = work; /* J*P */
    double *work2 = work + J*P; /* P*P */
    double *work3 = work + J*P + P*P; /* P*P */
    double *qraux = work + J*P + 2*P*P; /* P */
    double *b = work + J*P + 2*P*P + P;  /* P */
    double *qty_and_g = work + J*P + 2*P*P + 2*P;  /* P */

    for (int rowj = 0; rowj < J; ++rowj) {
        double v_j = v[rowj];

        for (int colp = 0; colp < P; ++colp) {

            work1[rowj * P + colp] = z[colp * J + rowj]/ v_j;

        }
    }
    /*crossprod(Z, diag(1 / v)) in work1
     * keep work1 for b later */
    /* stuff needed for mm and mv multiplication fortran routines */
    char transN = 'N';

    double alpha_blas_one = 1.0;
    double beta_blas_zero = 0.0;
    int inc_blas = 1;

    /*crossprod(Z, diag(1 / v)) %*% Z*/
    F77_CALL(dgemm)(&transN, &transN, &P, &P, &J,
                            &alpha_blas_one, work1, &P, z, &J,
                            &beta_blas_zero, work2, &P FCONE FCONE);
    /* PxP  result is in work2 */

    /* U.eta <- c(A.eta.intercept^2, U.eta.coef)
    var.inv <- crossprod(Z, diag(1 / v)) %*% Z + diag(1 / U.eta)*/
    work2[0] += 1/(AEtaIntercept*AEtaIntercept);

    for (int p = 1; p < P; ++p) {

        work2[p * P + p] += 1/UEtaCoef[p-1];
    }
    /* var.inv in work2 */
    /* keep a copy of var.inv in work3 for Choleski factorisation later */
    memcpy(work3, work2, P*P*sizeof(double));

    /* stuff for dqrdc for qr decomposition */
    int jpvt = 0; /* not pivoting */
    double work_qr = 0; /* work not needed since we are not pivoting */
    int job_qr = 0; /* no pivoting */

    F77_CALL(dqrdc)(work2, &P, &P, &P, qraux, &jpvt,
                            &work_qr, &job_qr);
    /* after the call, work2 is qr and contains R in its upper triangle and below
     * the diagonal work2=qr contains info from which the orthogonal part
     * of the decomposition can be recovered,
     * and qraux contains the information required to be able to do this */

    /*b <- crossprod(Z, diag(1 / v)) %*% beta */
    F77_CALL(dgemv)(&transN, &P, &J, &alpha_blas_one, work1,
                        &P, beta, &inc_blas, &beta_blas_zero,
                        b, &inc_blas FCONE);

    /*eta.hat <- qr.solve(qr, b)
     * qr.solve(a,b) does same as qr.coef(a,b) when a is a qr
     * ie solves using least squares, so C can use dqrsl
     * https://docs.tibco.com/pub/enterprise-runtime-for-R/1.5.0_may_2013/TERR_1.5.0_LanguageRef/base/qr.qy.html*/

    /* in the call parameters, parameter x = our qr and y = b */
    double qy = 0; /* q*y results if requested - not needed */

    /* transpose(q)*y results - needed since we want eta_hat (b),
     * in the call parameters, parameter b = eta_hat*/
    double rsd = 0; /* residual y  - z*b if req - not needed */
    double xb = 0; /* x*b if req - not needed */
    int job_sl = 100; /* set to compute b (eta_hat) only */
    int info = 0; /* could be changed by call */

    F77_CALL(dqrsl)(work2, &P, &P, &P, qraux, b,
                    &qy, qty_and_g,
                    eta, /* use eta for eta_hat */
                    &rsd, &xb, &job_sl, &info);
    if (info) error("error in dqrsl in updateEta: %d", info);

    /* eta.hat[-1L] <- eta.hat[-1L] + mean.eta.coef / U.eta.coef */
    /* Added by JB 24 December 2018                              */
    for (int p = 0; p < P - 1; ++p) {

      eta[p + 1] += meanEtaCoef[p] / UEtaCoef[p];

    }

    /* g <- rnorm(n = P) */
    for (int p = 0; p < P; ++p) {

        qty_and_g[p] = rnorm(0,1);
    }

    /*R <- chol(var.inv)
     * var.inv is in work3
     *
     * use dpotrf: compute the Cholesky factorization of a real sym-
      metric positive definite matrix A
      * UPLO, N, A, LDA, INFO */
    char uplo = 'U';

    F77_CALL(dpotrf)(&uplo, &P, work3, &P, &info FCONE);
    if (info) error("error in dpotrf in updateEta: %d", info);
    /* on exit, work3 contains U from factorisation var.inv = U**T*U
     * ie work3 contains R from R <- chol(var.inv)*/

    /* epsilon <- backsolve(R, g) */

    /* use dtrsl from linpack */
    int job_bsl = 01; /* solve t*x = b, t upper triangular */

    F77_CALL(dtrsl)(work3, /* R */
                    &P, &P, qty_and_g,
                    &job_bsl, &info);
    if (info) error("error in dtrsl in updateEta: %d", info);
    /* after the call, qty_and_g contains the solution, epsilon */

    /* prior@eta@.Data <- eta.hat + epsilon */
    for (int p = 0; p < P; ++p) {
            eta[p] += qty_and_g[p];
    }
}

void
updateComponentWeightMix(SEXP prior_R)
{
    double *compWeight = REAL(GET_SLOT(prior_R, componentWeightMix_sym));
    double *latentCompWeight = REAL(GET_SLOT(prior_R, latentComponentWeightMix_sym));
    double *levelCompWeight = REAL(GET_SLOT(prior_R, levelComponentWeightMix_sym));
    int *indexClass = INTEGER(GET_SLOT(prior_R, indexClassMix_sym));
    int indexClassMax = *INTEGER(GET_SLOT(prior_R, indexClassMaxMix_sym));
    double omega = *REAL(GET_SLOT(prior_R, omegaComponentWeightMix_sym));
    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;
    SEXP dimBeta_R = GET_SLOT(prior_R, dimBeta_sym);
    int *dimBeta = INTEGER(dimBeta_R);
    int  nDimBeta = LENGTH(dimBeta_R);
    SEXP iteratorsDims_R = GET_SLOT(prior_R, iteratorsDimsMix_sym);
    SEXP iteratorBeta_R = VECTOR_ELT(iteratorsDims_R, iAlong_c);
    int nAlong = dimBeta[iAlong_c];

    double min = *REAL(GET_SLOT(prior_R, minLevelComponentWeight_sym));
    double max = *REAL(GET_SLOT(prior_R, maxLevelComponentWeight_sym));

    int nBeta = 1;
    for (int i = 0; i < nDimBeta; ++i) {
        nBeta *= dimBeta[i];
    }

    double invOmegaSq = 1/(omega * omega);

    resetS(iteratorBeta_R);

    SEXP indicesBeta_R = GET_SLOT(iteratorBeta_R, indices_sym);
    int *indicesBeta = INTEGER(indicesBeta_R);
    int nIndicesBeta = LENGTH(indicesBeta_R);

    for (int iAlong = 0; iAlong < nAlong; ++iAlong) {

        for (int iClass = 0; iClass < indexClassMax; ++iClass) {

            int iW = iClass * nAlong + iAlong;
            int sumIsComp = 0;
            double sumLatentCompWeight = 0;

            for (int iB = 0; iB < nIndicesBeta; ++iB) {
                int iBeta = indicesBeta[iB] - 1;
                int class = indexClass[iBeta]-1;
                int isComp = (iClass <= class);

                if (isComp) {
                    ++sumIsComp;
                    int iZ = iClass * nBeta + iBeta;
                    sumLatentCompWeight += latentCompWeight[iZ];
                }

            }

            double level = levelCompWeight[iW];
            double var = 1 / (invOmegaSq + sumIsComp);
            double mean = var * (level * invOmegaSq + sumLatentCompWeight);
            double sd = sqrt(var);

            compWeight[iW] = rtnorm1(mean, sd, min, max);
        }

        advanceS(iteratorBeta_R);
    }

}


void
updateGWithTrend(SEXP prior_R)
{
    /* GWithTrend is a 2x2 matrix, R uses column-major ordering*/
    double *GWithTrend = REAL(GET_SLOT(prior_R, GWithTrend_sym));
    double phi = *REAL(GET_SLOT(prior_R, phi_sym));

    GWithTrend[3] = phi;
}

void
updateIndexClassMaxPossibleMix(SEXP prior_R)
{
    SEXP latentWeight_R = GET_SLOT(prior_R, latentWeightMix_sym);
    double *latentWeight = REAL(latentWeight_R);
    int nLatentWeight = LENGTH(latentWeight_R);

    double *weight = REAL(GET_SLOT(prior_R, weightMix_sym));
    int indexClassMax = *INTEGER(GET_SLOT(prior_R, indexClassMaxMix_sym));
    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    int nAlong = dimBeta[iAlong_c];

    double minLatentWeight = 1; /* max latent weight is 1 */
    for (int j = 0; j < nLatentWeight; ++j) {
        if (latentWeight[j] < minLatentWeight) {
            minLatentWeight = latentWeight[j];
        }
    }
    double oneMinusMinLatentWeight = 1- minLatentWeight;

    /* make our own sumsWeights and zero it*/
    double sumsWeights[nAlong];
    memset(sumsWeights, 0, nAlong * sizeof(double));

    int foundAns = 0;
    int indexClassMaxPoss = 0;

    while (!foundAns && (indexClassMaxPoss < indexClassMax)) {

        ++indexClassMaxPoss;

        int offset = (indexClassMaxPoss - 1) * nAlong;

        for (int iAlong = 0; iAlong < nAlong; ++iAlong) {
            sumsWeights[iAlong] += weight[offset + iAlong];
        }
        for (int i = 0; i < nAlong; ++i) {
            if (sumsWeights[i] <= oneMinusMinLatentWeight) {
                break;
            }
            else if (i == (nAlong - 1)) {
                foundAns = 1;
            }
        }
    }

    SET_INTSCALE_SLOT(prior_R, indexClassMaxPossibleMix_sym, indexClassMaxPoss);
    SET_LOGICALSCALE_SLOT(prior_R, foundIndexClassMaxPossibleMix_sym, foundAns);
}


void
updateIndexClassMaxUsedMix(SEXP prior_R)
{
    int *indexClass = INTEGER(GET_SLOT(prior_R, indexClassMix_sym));
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));

    int indexClassMaxUsed = 0;
    for (int j = 0; j < J; ++j) {
        if ( indexClass[j] > indexClassMaxUsed ) {
            indexClassMaxUsed = indexClass[j];
        }
    }

    SET_INTSCALE_SLOT(prior_R, indexClassMaxUsedMix_sym, indexClassMaxUsed);
}


void
updateIndexClassMix(SEXP prior_R, double* betaTilde, int J)
{
    int *indexClass = INTEGER(GET_SLOT(prior_R, indexClassMix_sym));
    int indexClassMaxPoss = *INTEGER(GET_SLOT(prior_R, indexClassMaxPossibleMix_sym));

    double *indexClassProbOriginal = REAL(GET_SLOT(prior_R, indexClassProbMix_sym));

    double *weight = REAL(GET_SLOT(prior_R, weightMix_sym));
    double *latentWeight = REAL(GET_SLOT(prior_R, latentWeightMix_sym));

    double *prodVectors = REAL(GET_SLOT(prior_R, prodVectorsMix_sym));

    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    int nAlong = dimBeta[iAlong_c];

    int pos1 = *INTEGER(GET_SLOT(prior_R, posProdVectors1Mix_sym));
    int pos2 = *INTEGER(GET_SLOT(prior_R, posProdVectors2Mix_sym));
    int nBetaNoAlong = *INTEGER(GET_SLOT(prior_R, nBetaNoAlongMix_sym));

    SEXP iteratorsDims_R = GET_SLOT(prior_R, iteratorsDimsMix_sym);
    SEXP iteratorBeta_R = VECTOR_ELT(iteratorsDims_R, iAlong_c);

    resetS(iteratorBeta_R);

    /* space for v and indexClassProb, both length J */
    double *work = (double*)R_alloc(2*J, sizeof(double));
    double *v = work;
    getV_Internal(v, prior_R, J);

    /* copy the original indexClassProbs so we can change them */
    double *indexClassProb = work + J;
    memcpy(indexClassProb, indexClassProbOriginal, J*sizeof(double));

    SEXP indicesBeta_R = GET_SLOT(iteratorBeta_R, indices_sym);
    int *indicesBeta = INTEGER(indicesBeta_R);
    int nIndicesBeta = LENGTH(indicesBeta_R);

    for (int iAlong = 0; iAlong < nAlong; ++iAlong) {

        for (int iB = 0; iB < nIndicesBeta; ++iB) {
            int iBeta = indicesBeta[iB] - 1;
            double thisLatentWeight = latentWeight[iBeta];
            double thisBetaTilde = betaTilde[iBeta];
            double thisV = v[iBeta];

            int iBetaNoAlong = (iBeta/pos1)* pos2 + (iBeta%pos2);

            for (int iClass = 0; iClass < indexClassMaxPoss; ++iClass) {

                int iW = iClass * nAlong + iAlong;
                double thisWeight = weight[iW];
                int includeClass = (thisLatentWeight < thisWeight);
                if (includeClass) {
                    int iProd = iClass * nBetaNoAlong + iBetaNoAlong;
                    double valProdVector = prodVectors[iProd];
                    double tmp = thisBetaTilde - valProdVector;
                    double logProb = -0.5 * (tmp * tmp) / thisV;
                    indexClassProb[iClass] = logProb;
                }
                else {
                    indexClassProb[iClass] = R_PosInf;
                }

            }

            double maxLogProb = R_NegInf;
            for (int iClass = 0; iClass < indexClassMaxPoss; ++iClass) {

                double logProb = indexClassProb[iClass];
                int includeClass = !(logProb > 0);

                if (includeClass && (logProb > maxLogProb)) {
                    maxLogProb = logProb;
                }


            }

            double sumProb = 0;
            for (int iClass = 0; iClass < indexClassMaxPoss; ++iClass) {

                double logProb = indexClassProb[iClass];
                int includeClass = !(logProb > 0);

                if (includeClass) {
                    logProb -= maxLogProb;
                    double prob = exp(logProb);
                    indexClassProb[iClass] = prob;
                    sumProb += prob;
                }
            }

            double U = runif(0,1) * sumProb;
            double cumSum = 0;

            int iClassStays = 0;
            for (int iClass = 0; iClass < indexClassMaxPoss; ++iClass) {

                iClassStays = iClass;

                double prob = indexClassProb[iClass];
                int includeClass = !(prob > 1);

                if (includeClass) {
                    cumSum += prob;

                    if (!(U > cumSum)) {
                        break;
                    }
                }
            }

            indexClass[iBeta] = iClassStays+1;

        } /* end beta index loop */

        advanceS(iteratorBeta_R);
    }
}

void
updateLatentComponentWeightMix(SEXP prior_R)
{
    double *latentCompWeight = REAL(GET_SLOT(prior_R, latentComponentWeightMix_sym));
    double *compWeight = REAL(GET_SLOT(prior_R, componentWeightMix_sym));
    int *indexClass = INTEGER(GET_SLOT(prior_R, indexClassMix_sym));

    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r - 1;

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    int  nBeta = *INTEGER(GET_SLOT(prior_R, J_sym));
    SEXP iteratorsDims_R = GET_SLOT(prior_R, iteratorsDimsMix_sym);
    SEXP iteratorBeta_R = VECTOR_ELT(iteratorsDims_R, iAlong_c);
    int nAlong = dimBeta[iAlong_c];

    resetS(iteratorBeta_R);

    SEXP indicesBeta_R = GET_SLOT(iteratorBeta_R, indices_sym);
    int *indicesBeta = INTEGER(indicesBeta_R);
    int nIndicesBeta = LENGTH(indicesBeta_R);

    for (int iAlong = 0; iAlong < nAlong; ++iAlong) {

        for (int iB = 0; iB < nIndicesBeta; ++iB) {

            int iBeta = indicesBeta[iB] - 1;
            int class_iBeta_r = indexClass[iBeta];
            int class_iBeta_c = class_iBeta_r - 1;

            for (int iClass = 0; iClass < class_iBeta_r; ++iClass) {

                int iz = iClass * nBeta + iBeta;
                int iw = iClass * nAlong + iAlong;
                double compWeight_iw = compWeight[iw];

                if (iClass < class_iBeta_c) {
                    latentCompWeight[iz] = rtnorm1(compWeight_iw,
                                                        1, R_NegInf, 0);
                }
                else {
                    latentCompWeight[iz] = rtnorm1(compWeight_iw,
                                                        1, 0, R_PosInf);
                }
            }
        }
        advanceS(iteratorBeta_R);
    }
}

void
updateLatentWeightMix(SEXP prior_R)
{
    double *latentWeight = REAL(GET_SLOT(prior_R, latentWeightMix_sym));
    double *weight = REAL(GET_SLOT(prior_R, weightMix_sym));
    int *indexClass = INTEGER(GET_SLOT(prior_R, indexClassMix_sym));

    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r - 1;

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    SEXP iteratorsDims_R = GET_SLOT(prior_R, iteratorsDimsMix_sym);
    SEXP iteratorBeta_R = VECTOR_ELT(iteratorsDims_R, iAlong_c);
    int nAlong = dimBeta[iAlong_c];

    resetS(iteratorBeta_R);

    SEXP indicesBeta_R = GET_SLOT(iteratorBeta_R, indices_sym);
    int *indicesBeta = INTEGER(indicesBeta_R);
    int nIndicesBeta = LENGTH(indicesBeta_R);

    for (int iAlong = 0; iAlong < nAlong; ++iAlong) {

        for (int iB = 0; iB < nIndicesBeta; ++iB) {

            int iBeta = indicesBeta[iB] - 1;
            int iClass = indexClass[iBeta] - 1;

            int iw = iClass * nAlong + iAlong;
            double weight_iw = weight[iw];

            latentWeight[iBeta] = runif(0, weight_iw);
        }
        advanceS(iteratorBeta_R);
    }
}



void
updateLevelComponentWeightMix(SEXP prior_R)
{
    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;
    int nAlong = dimBeta[iAlong_c];

    int indexClassMax = *INTEGER(GET_SLOT(prior_R, indexClassMaxMix_sym));
    int indexClassMaxPossible = *INTEGER(GET_SLOT(prior_R, indexClassMaxPossibleMix_sym));

    double *comp = REAL(GET_SLOT(prior_R, componentWeightMix_sym));
    double *level = REAL(GET_SLOT(prior_R, levelComponentWeightMix_sym));
    double meanLevel = *REAL(GET_SLOT(prior_R, meanLevelComponentWeightMix_sym));

    double *mOriginal = REAL(GET_SLOT(prior_R, mMix_sym)); /* len nAlong */
    double *COriginal = REAL(GET_SLOT(prior_R, CMix_sym)); /* len nAlong */
    double *aOriginal = REAL(GET_SLOT(prior_R, aMix_sym)); /* len nAlong - 1 */
    double *ROriginal = REAL(GET_SLOT(prior_R, RMix_sym)); /* len nAlong - 1 */

    double phi = *REAL(GET_SLOT(prior_R, phiMix_sym));
    double phiSq = phi * phi;

    double omegaComp = *REAL(GET_SLOT(prior_R, omegaComponentWeightMix_sym));
    double omegaCompSq = omegaComp * omegaComp;
    double omegaLevel = *REAL(GET_SLOT(prior_R, omegaLevelComponentWeightMix_sym));
    double omegaLevelSq = omegaLevel * omegaLevel;

    double priorMeanFirst = meanLevel / (1 - phi);
    double priorVarFirst = omegaLevelSq / (1 - phi*phi);
    double priorSdFirst = sqrt(priorVarFirst);

    /* malloc space to copy things we do not want to change into,
     * do not want to change m, C, a, R */
    int sz1 = nAlong;
    int sz2 = nAlong - 1;
    int spaceNeeded = 2*sz1 + 2*sz2;
    double *copies = (double *)R_alloc(spaceNeeded, sizeof(double));
    double *m = copies;
    double *C = copies + sz1;
    double *a = copies + 2*sz1;
    double *R = copies + 2*sz1 + sz2;
    memcpy(m, mOriginal, sz1*sizeof(double));
    memcpy(C, COriginal, sz1*sizeof(double));
    memcpy(a, aOriginal, sz2*sizeof(double));
    memcpy(R, ROriginal, sz2*sizeof(double));

    for (int iClass = 0; iClass < indexClassMaxPossible; ++iClass) {

        m[0] = priorMeanFirst;
        C[0] = priorVarFirst;

        /* forward filter */
        for (int iAlong = 0; iAlong < nAlong-1; ++iAlong) {

            int iWt = iClass * nAlong + iAlong + 1;
            double this_a = meanLevel + phi * m[iAlong];
            a[iAlong] = this_a;
            double this_R = phiSq * C[iAlong] + omegaLevelSq;
            R[iAlong] = this_R;

            double q = this_R + omegaCompSq;
            double e = comp[iWt] - this_a;
            double A = this_R / q;
            int next_iAlong = iAlong+1;
            m[next_iAlong] = this_a + A * e;
            C[next_iAlong] = this_R - A*A * q;
        }

        /* draw final values */
        int final_iWt = (iClass + 1) * nAlong - 1;
        level[final_iWt] = rnorm( m[nAlong-1], sqrt(C[nAlong-1]) );

        /* backwards smooth */
        for (int iAlong = nAlong-2; iAlong >= 0; --iAlong) {

            int iWtCurr = iClass * nAlong + iAlong;
            int iWtNext = iWtCurr + 1;

            double this_C = C[iAlong];
            double this_R = R[iAlong];
            double B = this_C * phi / this_R;
            double mStar = m[iAlong] + B * ( level[iWtNext] - a[iAlong]);
            double CStar = this_C - B*B * this_R;

            level[iWtCurr] = rnorm( mStar, sqrt(CStar) );
        }
    }

    if ( indexClassMaxPossible < indexClassMax) {

        for (int iClass = indexClassMaxPossible; iClass < indexClassMax; ++iClass) {

            int iWt = iClass * nAlong;
            level[iWt] = rnorm( priorMeanFirst, priorSdFirst );

            for (int iAlong = 1; iAlong < nAlong; ++iAlong) {

                int iWtCurr = iClass * nAlong + iAlong;
                int iWtPrev = iWtCurr - 1;
                double mean = meanLevel + phi * level[iWtPrev];
                level[iWtCurr] = rnorm( mean, omegaLevel);
            }
        }
    }
}

void
updateMeanLevelComponentWeightMix(SEXP prior_R)
{
    double *level = REAL(GET_SLOT(prior_R, levelComponentWeightMix_sym));
    double omega = *REAL(GET_SLOT(prior_R, omegaLevelComponentWeightMix_sym));
    double meanPrior = *REAL(GET_SLOT(prior_R, priorMeanLevelComponentWeightMix_sym));
    double sdPrior = *REAL(GET_SLOT(prior_R, priorSDLevelComponentWeightMix_sym));
    double phi = *REAL(GET_SLOT(prior_R, phiMix_sym));

    int indexClassMaxUsed = *INTEGER(GET_SLOT(prior_R, indexClassMaxUsedMix_sym));

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;
    int nAlong = dimBeta[iAlong_c];

    double invOmegaSq = 1/(omega * omega);
    double precPrior = 1/(sdPrior * sdPrior);
    double onePlusPhi = 1 + phi;
    double meanData = 0;

    for (int iClass = 0; iClass < indexClassMaxUsed; ++iClass) {

        int iw = iClass * nAlong;

        double levelCurr = level[iw];
        meanData += levelCurr * onePlusPhi;

        int iCurr = iw;

        for (int iAlong = 1; iAlong < nAlong; ++iAlong) {

            ++iCurr; /* iClass * nAlong + iAlong */
            double levelPrev = levelCurr;
            levelCurr = level[iCurr];

            meanData += levelCurr - phi * levelPrev;
        }
    }

    double nObs = indexClassMaxUsed * (nAlong - 1 + onePlusPhi / (1 - phi) );
    meanData /= nObs;
    double precData = nObs * invOmegaSq;
    double var = 1 / (precData + precPrior);
    double mean = var * (precData * meanData + precPrior * meanPrior);
    double sd = sqrt(var);
    double newMean = rnorm(mean, sd);

    SET_DOUBLESCALE_SLOT(prior_R, meanLevelComponentWeightMix_sym, newMean);
}

void
updateOmegaAlpha(SEXP prior_R, int isWithTrend)
{
    int returnUnchanged = 0;
    if (isWithTrend) {
        int hasLevel = *LOGICAL(GET_SLOT(prior_R, hasLevel_sym));
        if (!hasLevel) {
            returnUnchanged = 1;
        }
    }

    if (!returnUnchanged) {

        int K = *INTEGER(GET_SLOT(prior_R, K_sym));
        int L = *INTEGER(GET_SLOT(prior_R, L_sym));
        int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));

        double *alpha = REAL(GET_SLOT(prior_R, alphaDLM_sym)); /* vector, length (K+1)L */
        double omega = *REAL(GET_SLOT(prior_R, omegaAlpha_sym));
        double omegaMax = *REAL(GET_SLOT(prior_R, omegaAlphaMax_sym));

        double A = *REAL(GET_SLOT(prior_R, AAlpha_sym));
        double nu = *REAL(GET_SLOT(prior_R, nuAlpha_sym));

        SEXP iterator_R = GET_SLOT(prior_R, iteratorState_sym);

        resetA(iterator_R);
        int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym));

        double *delta = NULL;
        double phi = 0;

        if (isWithTrend) {
            delta = REAL(GET_SLOT(prior_R, deltaDLM_sym));
        }
        else {
            phi = *REAL(GET_SLOT(prior_R, phi_sym));
        }

        double V = 0;
        int n = 0;

        for (int l = 0; l < L; ++l) {

        if (!alongAllStrucZero[l]) {

        for (int i = 0; i < K; ++i) {
            int k_curr = indices[i + 1] - 1; /* C style indices */
            int k_prev = indices[i] - 1;

            double alpha_k_curr = alpha[k_curr];
            double alpha_k_prev = alpha[k_prev];

            double toSq = 0;

            if (isWithTrend) {
            toSq = alpha_k_curr - alpha_k_prev - delta[k_prev];
            }
            else {
            toSq = alpha_k_curr - phi * alpha_k_prev;
            }
            V += toSq*toSq;
            n += 1;
        }

        } /* end if (!alongAllStrucZero[l]) */
            advanceA(iterator_R);
        }

        omega = updateSDNorm(omega, A, nu, V, n, omegaMax);

        int successfullyUpdated = (omega > 0);
        if(successfullyUpdated) {
            SET_DOUBLESCALE_SLOT(prior_R, omegaAlpha_sym, omega);
        }
    } /* end if !returnUnchanged */
}

void
updateOmegaComponentWeightMix(SEXP prior_R)
{
    double omega = *REAL(GET_SLOT(prior_R, omegaComponentWeightMix_sym));
    double omegaMax = *REAL(GET_SLOT(prior_R, omegaComponentWeightMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, AComponentWeightMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuComponentWeightMix_sym));

    double *compWeight = REAL(GET_SLOT(prior_R, componentWeightMix_sym));
    double *levelCompWeight = REAL(GET_SLOT(prior_R, levelComponentWeightMix_sym));
    int indexClassMaxUsed = *INTEGER(GET_SLOT(prior_R, indexClassMaxUsedMix_sym));

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;
    int nAlong = dimBeta[iAlong_c];

    int n = nAlong * indexClassMaxUsed;
    double V = 0;

    for (int iClass = 0; iClass < indexClassMaxUsed; ++iClass) {

        for (int iAlong = 0; iAlong < nAlong; ++iAlong) {

            int iWt = iClass * nAlong + iAlong;
            double tmp = compWeight[iWt] - levelCompWeight[iWt];
            V += tmp * tmp;
        }
    }

    omega = updateSDNorm(omega, A, nu, V, n, omegaMax);

    int successfullyUpdated = (omega > 0);
    if(successfullyUpdated) {
    SET_DOUBLESCALE_SLOT(prior_R, omegaComponentWeightMix_sym, omega);
    }
}

void
updateOmegaDelta(SEXP prior_R)
{
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));

    double *delta = REAL(GET_SLOT(prior_R, deltaDLM_sym)); /* vector, length (K+1)L */
    double phi = *REAL(GET_SLOT(prior_R, phi_sym));
    double omega = *REAL(GET_SLOT(prior_R, omegaDelta_sym));
    double omegaMax = *REAL(GET_SLOT(prior_R, omegaDeltaMax_sym));

    double A = *REAL(GET_SLOT(prior_R, ADelta_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuDelta_sym));

    SEXP iterator_R = GET_SLOT(prior_R, iteratorState_sym);

    resetA(iterator_R);
    int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym));

    double V = 0;
    int n = 0;

    for (int l = 0; l < L; ++l) {

    if (!alongAllStrucZero[l]) {

        for (int i = 0; i < K; ++i) {
        int k_curr = indices[i + 1] - 1; /* C style indices */
        int k_prev = indices[i] - 1;

        double toSq = delta[k_curr] - phi * delta[k_prev];
        V += toSq*toSq;
        n += 1;
        }

    }
        advanceA(iterator_R);
    }

    omega = updateSDNorm(omega, A, nu, V, n, omegaMax);

    int successfullyUpdated = (omega > 0);
    if(successfullyUpdated) {
        SET_DOUBLESCALE_SLOT(prior_R, omegaDelta_sym, omega);
    }
}

void
updateOmegaLevelComponentWeightMix(SEXP prior_R)
{
    double omega = *REAL(GET_SLOT(prior_R, omegaLevelComponentWeightMix_sym));
    double omegaMax = *REAL(GET_SLOT(prior_R, omegaLevelComponentWeightMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, AComponentWeightMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuComponentWeightMix_sym));

    double *level = REAL(GET_SLOT(prior_R, levelComponentWeightMix_sym));
    double meanLevel = *REAL(GET_SLOT(prior_R, meanLevelComponentWeightMix_sym));

    double phi = *REAL(GET_SLOT(prior_R, phiMix_sym));

    double oneMinusPhiSq = 1 - phi*phi;
    double meanOverOneMinusPhi = meanLevel/(1 - phi);

    int indexClassMaxUsed = *INTEGER(GET_SLOT(prior_R, indexClassMaxUsedMix_sym));

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;
    int nAlong = dimBeta[iAlong_c];

    int n = nAlong * indexClassMaxUsed;
    double V = 0;

    for (int iClass = 0; iClass < indexClassMaxUsed; ++iClass) {

        int iWt = iClass * nAlong;
        double levelCurr = level[iWt];

        double tmp = levelCurr - meanOverOneMinusPhi;
        V += oneMinusPhiSq * tmp * tmp;

        int iWtCurr = iWt;

        for (int iAlong = 1; iAlong < nAlong; ++iAlong) {

            ++iWtCurr; /* iClass * nAlong + iAlong */
            double levelPrev = levelCurr;
            levelCurr = level[iWtCurr];

            tmp = levelCurr - meanLevel - phi * levelPrev;
            V += tmp * tmp;
        }
    }

    omega = updateSDNorm(omega, A, nu, V, n, omegaMax);

    int successfullyUpdated = (omega > 0);
    if(successfullyUpdated) {
        SET_DOUBLESCALE_SLOT(prior_R, omegaLevelComponentWeightMix_sym, omega);
    }
}

void
updateOmegaSeason(SEXP prior_R)
{
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));

    /* s is FFBS list */
    SEXP s_R = GET_SLOT(prior_R, s_sym);
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));

    double omega = *REAL(GET_SLOT(prior_R, omegaSeason_sym));
    double omegaMax = *REAL(GET_SLOT(prior_R, omegaSeasonMax_sym));

    double A = *REAL(GET_SLOT(prior_R, ASeason_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuSeason_sym));

    SEXP iterator_R = GET_SLOT(prior_R, iteratorState_sym);

    resetA(iterator_R);
    int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym));

    double V = 0;

    for (int l = 0; l < L; ++l) {

    if (!alongAllStrucZero[l]) {
        for (int i = 0; i < K; ++i) {
        int i_curr = indices[i + 1] - 1; /* C style indices */
        int i_prev = indices[i] - 1;

        double *s_curr = REAL(VECTOR_ELT(s_R, i_curr));
        double *s_prev = REAL(VECTOR_ELT(s_R, i_prev));
        double curr = s_curr[0];
        double prev = s_prev[nSeason-1];
        double toSq = curr - prev;
        V += toSq*toSq;
        }
        }

        advanceA(iterator_R);
    }

    omega = updateSDNorm(omega, A, nu, V, J, omegaMax);

    int successfullyUpdated = (omega > 0);
    if(successfullyUpdated) {
        SET_DOUBLESCALE_SLOT(prior_R, omegaSeason_sym, omega);
    }
}

void
updateOmegaVectorsMix(SEXP prior_R)
{
    double omega = *REAL(GET_SLOT(prior_R, omegaVectorsMix_sym));
    double omegaMax = *REAL(GET_SLOT(prior_R, omegaVectorsMaxMix_sym));
    double A = *REAL(GET_SLOT(prior_R, AVectorsMix_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuVectorsMix_sym));

    SEXP vectors_R = GET_SLOT(prior_R, vectorsMix_sym); /* list */
    int nVectors = LENGTH(vectors_R);

    int indexClassMaxUsed = *INTEGER(GET_SLOT(prior_R, indexClassMaxUsedMix_sym));

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;

    int n = 0;
    double V = 0;

    for (int i = 0; i < nVectors; ++i) {

        if (i != iAlong_c) {
            double *vector = REAL(VECTOR_ELT(vectors_R, i));
            int dim = dimBeta[i];
            int nCells = dim * indexClassMaxUsed;

            for (int iVector = 0; iVector < nCells; ++iVector) {

                double tmp = vector[iVector];
                V += tmp * tmp;
            }

            n += nCells;
        }
    }

    omega = updateSDNorm(omega, A, nu, V, n, omegaMax);

    int successfullyUpdated = (omega > 0);
    if(successfullyUpdated) {
        SET_DOUBLESCALE_SLOT(prior_R, omegaVectorsMix_sym, omega);
    }
}

void
updatePhi(SEXP prior_R, int isWithTrend) {

    int isPhiKnown = *LOGICAL(GET_SLOT(prior_R, phiKnown_sym));

    if (!isPhiKnown) {

        double phiCurr = *REAL(GET_SLOT(prior_R, phi_sym));

        int K = *INTEGER(GET_SLOT(prior_R, K_sym));
        int L = *INTEGER(GET_SLOT(prior_R, L_sym));
        int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));

        double *state = NULL;
        double omega = 0;

        if (isWithTrend) {
            state = REAL(GET_SLOT(prior_R, deltaDLM_sym)); /* vector, length (K+1)L */
            omega = *REAL(GET_SLOT(prior_R, omegaDelta_sym));
        }
        else {
            state = REAL(GET_SLOT(prior_R, alphaDLM_sym)); /* vector, length (K+1)L */
            omega = *REAL(GET_SLOT(prior_R, omegaAlpha_sym));
        }

        double minPhi = *REAL(GET_SLOT(prior_R, minPhi_sym));
        double maxPhi = *REAL(GET_SLOT(prior_R, maxPhi_sym));

        double shape1 = *REAL(GET_SLOT(prior_R, shape1Phi_sym));
        double shape2 = *REAL(GET_SLOT(prior_R, shape2Phi_sym));

        SEXP iterator_R = GET_SLOT(prior_R, iteratorState_sym);

        resetA(iterator_R);
        int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym));

        double numerator = 0;
        double denominator = 0;

        for (int l = 0; l < L; ++l) {

            if (!alongAllStrucZero[l]) {

                for (int i = 0; i < K; ++i) {
                    int k_curr = indices[i + 1] - 1; /* C style indices */
                    int k_prev = indices[i] - 1;

                    double state_k_prev = state[k_prev];
                    numerator += state[k_curr] * state_k_prev;
                    denominator += state_k_prev * state_k_prev;
                }
            }

            advanceA(iterator_R);
        }

        double mean = numerator/denominator;
        double sd = omega/sqrt(denominator);

        double phiProp = rtnorm1(mean, sd, minPhi, maxPhi);

        double phiPropTr = (phiProp - minPhi) / (maxPhi - minPhi);
        double phiCurrTr = (phiCurr - minPhi) / (maxPhi - minPhi);

        double logDensProp = dbeta(phiPropTr, shape1, shape2, USE_LOG);
        double logDensCurr = dbeta(phiCurrTr, shape1, shape2, USE_LOG);

        double logDiff = logDensProp - logDensCurr;

        int accept = (!(logDiff < 0) || (runif(0, 1) < exp(logDiff)));
        if (accept) {
            SET_DOUBLESCALE_SLOT(prior_R, phi_sym, phiProp);
        }

    }/* end !isPhiKnown */

    /* prior unchanged if phi known or !foundValue */
}


void
updatePhiMix(SEXP prior_R)

{
    int isPhiKnown = *LOGICAL(GET_SLOT(prior_R, phiKnown_sym));

    if (!isPhiKnown) {

        double phiCurr = *REAL(GET_SLOT(prior_R, phiMix_sym));

        double minPhi = *REAL(GET_SLOT(prior_R, minPhi_sym));
        double maxPhi = *REAL(GET_SLOT(prior_R, maxPhi_sym));

        double shape1 = *REAL(GET_SLOT(prior_R, shape1Phi_sym));
        double shape2 = *REAL(GET_SLOT(prior_R, shape2Phi_sym));

        double *level = REAL(GET_SLOT(prior_R, levelComponentWeightMix_sym));
        double meanLevel = *REAL(GET_SLOT(prior_R, meanLevelComponentWeightMix_sym));

        int indexClassMaxUsed = *INTEGER(GET_SLOT(prior_R, indexClassMaxUsedMix_sym));

        double omega = *REAL(GET_SLOT(prior_R, omegaLevelComponentWeightMix_sym));

        int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));
        int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
        int iAlong_c = iAlong_r -1;
        int nAlong = dimBeta[iAlong_c];

        double tolerance = *REAL(GET_SLOT(prior_R, tolerance_sym));

        double phiMax = modePhiMix(level, meanLevel, nAlong,
                       indexClassMaxUsed, omega, tolerance);

        double logPostPhiFirst = logPostPhiFirstOrderMix(phiMax, level, meanLevel,
                                 nAlong, indexClassMaxUsed, omega);

        double logPostPhiSecond = logPostPhiSecondOrderMix(phiMax, level, meanLevel,
                                   nAlong, indexClassMaxUsed, omega);

        double varProp = -1/logPostPhiSecond;
        double meanProp = phiMax + varProp * logPostPhiFirst;
        double sdProp = sqrt(varProp);

        double phiProp = rtnorm1(meanProp, sdProp, minPhi, maxPhi);

        double logLikProp = logPostPhiMix(phiProp, level, meanLevel,
                          nAlong, indexClassMaxUsed, omega);

        double logLikCurr = logPostPhiMix(phiCurr, level, meanLevel,
                          nAlong, indexClassMaxUsed, omega);

        double phiPropTr = (phiProp - minPhi) / (maxPhi - minPhi);
        double phiCurrTr = (phiCurr - minPhi) / (maxPhi - minPhi);

        double logDensProp = dbeta(phiPropTr, shape1, shape2, USE_LOG);
        double logDensCurr = dbeta(phiCurrTr, shape1, shape2, USE_LOG);

        double logPropProp = dnorm(phiProp, meanProp, sdProp, USE_LOG);
        double logPropCurr = dnorm(phiCurr, meanProp, sdProp, USE_LOG);

        double logDiff = logLikProp - logLikCurr +
            logDensProp - logDensCurr +
            logPropCurr - logPropProp;

        int accept = (!(logDiff < 0) || (runif(0, 1) < exp(logDiff)));
        if (accept) {
            SET_DOUBLESCALE_SLOT(prior_R, phiMix_sym, phiProp);
        }

    }
}


void
updateSeason(SEXP prior_R, double *betaTilde, int J)
{
    int K = *INTEGER(GET_SLOT(prior_R, K_sym));
    int L = *INTEGER(GET_SLOT(prior_R, L_sym));
    int *alongAllStrucZero = INTEGER(GET_SLOT(prior_R, alongAllStrucZero_sym));
    int nSeason = *INTEGER(GET_SLOT(prior_R, nSeason_sym));

    /* s is length (K+1)L list of vectors of length nSeason*/
    SEXP s_R = GET_SLOT(prior_R, s_sym);

    /* m a list of vector of doubles, len K+1, each vector length nSeason */
    SEXP m_R;
    PROTECT(m_R = duplicate(GET_SLOT(prior_R, mSeason_sym)));
    /* m0 a list of vector of doubles, len L, each vector length nSeason */
    SEXP m0_R = GET_SLOT(prior_R, m0Season_sym);
    /* C a list of vector of doubles, len K+1, each vector length nSeason */
    SEXP C_R;
    PROTECT(C_R = duplicate(GET_SLOT(prior_R, CSeason_sym)));
    /* a a list of vector of doubles, len K, each vector length nSeason */
    SEXP a_R;
    PROTECT(a_R = duplicate(GET_SLOT(prior_R, aSeason_sym)));
    /* R a list of vector of doubles, len K, each vector length nSeason */
    SEXP R_R;
    PROTECT(R_R = duplicate(GET_SLOT(prior_R, RSeason_sym)));

    double omega = *REAL(GET_SLOT(prior_R, omegaSeason_sym));
    double omegaSq = omega * omega;

    double *v = (double *)R_alloc(J, sizeof(double));
    getV_Internal(v, prior_R, J);

    SEXP iterator_s_R = GET_SLOT(prior_R, iteratorState_sym);
    SEXP iterator_v_R = GET_SLOT(prior_R, iteratorV_sym);

    resetA(iterator_s_R);
    resetA(iterator_v_R);

    int *indices_s = INTEGER(GET_SLOT(iterator_s_R, indices_sym));
    int *indices_v = INTEGER(GET_SLOT(iterator_v_R, indices_sym));


    /* referenced when drawing final s
     * the contents get changed in forward filter but
     * no need to set up the pointer each time */
    double *last_m = REAL(VECTOR_ELT(m_R, K));
    double *last_C = REAL(VECTOR_ELT(C_R, K));

    for (int l = 0; l < L; ++l) {

        if (!alongAllStrucZero[l]) {

            /*m[[1L]] <- m0[[l]]*/
            double *m0_l = REAL(VECTOR_ELT(m0_R, l));
            double *m_first = REAL(VECTOR_ELT(m_R, 0));
            memcpy(m_first, m0_l, nSeason*sizeof(double));

            /* forward filter */
            for (int i = 0; i < K; ++i) {

                int index_j = indices_v[i] - 1;

                double *this_m = REAL(VECTOR_ELT(m_R, i));
                double *this_C = REAL(VECTOR_ELT(C_R, i));
                double *this_a = REAL(VECTOR_ELT(a_R, i));
                double *this_R = REAL(VECTOR_ELT(R_R, i));

                double *next_m = REAL(VECTOR_ELT(m_R, i+1));
                double *next_C = REAL(VECTOR_ELT(C_R, i+1));

                for (int i_n = 0; i_n < nSeason-1; ++i_n) {
                    this_a[i_n + 1] = this_m[i_n];
                    this_R[i_n + 1] = this_C[i_n];
                }

                double curr_a = this_m[nSeason-1];
                double curr_R = this_C[nSeason-1] + omegaSq;
                this_a[0] = curr_a;
                this_R[0] = curr_R;

                double q = curr_R + v[index_j];
                double e = betaTilde[index_j] - curr_a;

                double Ae1 = curr_R * e/q;
                memcpy(next_m, this_a, nSeason*sizeof(double));
                next_m[0] += Ae1;

                double AAq1 = curr_R * curr_R/q;
                memcpy(next_C, this_R, nSeason*sizeof(double));
                next_C[0] -= AAq1;
            }

            int i_curr = indices_s[K] - 1;
            double *this_s = REAL(VECTOR_ELT(s_R, i_curr));

            for (int i_n = 0; i_n < nSeason; ++i_n) {
                double mean = last_m[i_n];
                double sd = sqrt(last_C[i_n]);
                double s = rnorm( mean, sd);
                this_s[i_n] = s;
            }

            /* backward smooth */
            for (int i = K-1; i >= 0; --i) {

                int i_prev = indices_s[i+1] - 1;
                int i_curr = indices_s[i] - 1;

                double *this_C = REAL(VECTOR_ELT(C_R, i));
                double thisC_last = this_C[nSeason-1];
                double *this_m = REAL(VECTOR_ELT(m_R, i));
                double thism_last = this_m[nSeason-1];

                double *s_prev = REAL(VECTOR_ELT(s_R, i_prev));
                double *s_curr = REAL(VECTOR_ELT(s_R, i_curr));

                /*s[[i.curr]][-n.season] <- s[[i.prev]][-1L]
                 * copy from last nSeason-1 elements of s_prev
                 * into first nSeason-1 elements of s_curr */
                memcpy(s_curr, (s_prev+1), (nSeason-1)*sizeof(double));

                double lambda = thisC_last/(thisC_last + omegaSq);
                double s_prev_first = s_prev[0];

                double mean = lambda * s_prev_first + (1 - lambda)*thism_last;
                double sd = sqrt(lambda) * omega;
                s_curr[nSeason-1] = rnorm(mean, sd);

            }

        } /* end if (!alongAllStrucZero[l]) */
        advanceA(iterator_s_R);
        advanceA(iterator_v_R);
    }
    /* only s gets updated in the prior */

    UNPROTECT(4);
}


void
updateUEtaCoef(SEXP prior_R)
{
    int P = *INTEGER(GET_SLOT(prior_R, P_sym));
    double *mean = REAL(GET_SLOT(prior_R, meanEtaCoef_sym)); /* length P-1 */
    double *U = REAL(GET_SLOT(prior_R, UEtaCoef_sym)); /* length P-1 */
    double *nu = REAL(GET_SLOT(prior_R, nuEtaCoef_sym));
    double *A = REAL(GET_SLOT(prior_R, AEtaCoef_sym));
    double *eta = REAL(GET_SLOT(prior_R, eta_sym));

    for (int p = 0; p < P-1; ++p) {
        double df_p = nu[p] + 1;
        double nuTimesASq_p = nu[p] * A[p] * A[p];
        double diff_p = eta[p+1] - mean[p];
        double scaleSq_p = (nuTimesASq_p + diff_p * diff_p) / df_p;
        U[p] = rinvchisq1(df_p, scaleSq_p);
    }
}

void
updateVectorsMixAndProdVectorsMix(SEXP prior_R, double * betaTilde, int J)
{
    SEXP vectors_R = GET_SLOT(prior_R, vectorsMix_sym); /* list */
    int nVectors = LENGTH(vectors_R);
    double omegaVectors = *REAL(GET_SLOT(prior_R, omegaVectorsMix_sym));

    SEXP iteratorsDims_R = GET_SLOT(prior_R, iteratorsDimsMix_sym);

    double *prodVectors = REAL(GET_SLOT(prior_R, prodVectorsMix_sym));

    SEXP iteratorProd_R = GET_SLOT(prior_R, iteratorProdVectorMix_sym);

    int *indexClass = INTEGER(GET_SLOT(prior_R, indexClassMix_sym));
    int indexClassMaxUsed = *INTEGER(GET_SLOT(prior_R, indexClassMaxUsedMix_sym));
    size_t sizeToZero = indexClassMaxUsed * sizeof(double);

    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;

    int *dimBeta = INTEGER(GET_SLOT(prior_R, dimBeta_sym));

    int pos1 = *INTEGER(GET_SLOT(prior_R, posProdVectors1Mix_sym));
    int pos2 = *INTEGER(GET_SLOT(prior_R, posProdVectors2Mix_sym));
    int nBetaNoAlong = *INTEGER(GET_SLOT(prior_R, nBetaNoAlongMix_sym));

    double precPrior = 1/(omegaVectors * omegaVectors);

    /* space for v length J and two lots of indexClassMaxUsed*/
    double *work = (double*)R_alloc(J + 2*indexClassMaxUsed, sizeof(double));
    double *v = work;
    getV_Internal(v, prior_R, J);

    /* make our own yX and XX from the remaining work so we can change them */
    double *yX = work + J;
    double *XX = work + J + indexClassMaxUsed;

    /* only need to get the iteratorProd indices once */
    int *indicesVectors = INTEGER(GET_SLOT(iteratorProd_R, indices_sym));

    /* loop through vectors, skipping position occupied by "along" dimension*/
    for (int i = 0; i < nVectors; ++i) {

        if (i == iAlong_c) {
            continue;
        }

        double *thisVector = REAL(VECTOR_ELT(vectors_R, i));
        int nElementVector = dimBeta[i];

        SEXP iteratorBeta_R = VECTOR_ELT(iteratorsDims_R, i);
        resetS(iteratorBeta_R);

        SEXP indicesBeta_R = GET_SLOT(iteratorBeta_R, indices_sym);
        int *indicesBeta = INTEGER(indicesBeta_R);
        int nIndicesBeta = LENGTH(indicesBeta_R);

        /* update i'th vector*/
        for (int iElement = 0; iElement < nElementVector; ++iElement) {

            memset(yX, 0, sizeToZero);
            memset(XX, 0, sizeToZero);

            for (int iB = 0; iB < nIndicesBeta; ++iB) {

                int iBeta = indicesBeta[iB] - 1;
                double thisBetaTilde = betaTilde[iBeta];
                double thisV = v[iBeta];

                int thisIndexClass = indexClass[iBeta] - 1;
                int iVector = thisIndexClass * nElementVector + iElement;
                double valVector = thisVector[iVector];

                int iBetaNoAlong = (iBeta/pos1)* pos2 + (iBeta%pos2);
                int iProd = thisIndexClass * nBetaNoAlong + iBetaNoAlong;
                double valProdVector = prodVectors[iProd];
                double X = valProdVector/valVector;
                double tmp = X / thisV;

                yX[thisIndexClass] += thisBetaTilde * tmp;
                XX[thisIndexClass] += X * tmp;
            }

            advanceS(iteratorBeta_R);

            for (int iClass = 0; iClass < indexClassMaxUsed; ++iClass) {
                int iVector = iClass * nElementVector + iElement;
                double precData = XX[iClass];
                double var = 1/ (precData + precPrior);
                double sd = sqrt(var);
                double mean = var * yX[iClass];
                thisVector[iVector] = rnorm(mean, sd);
            }

        } /* end iElement loop */


        for (int iClass = 0; iClass < indexClassMaxUsed; ++iClass) {

            resetM(iteratorProd_R);

            for (int iBetaNoAlong = 0; iBetaNoAlong < nBetaNoAlong; ++iBetaNoAlong) {

                int iProdVectors = iClass * nBetaNoAlong + iBetaNoAlong;
                double prodValues = 1;

                for (int iV = 0; iV < nVectors; ++iV) {

                    if (iV == iAlong_c) {
                        continue;
                    }

                    double *vector_iV = REAL(VECTOR_ELT(vectors_R, iV));
                    int nElement_iV = dimBeta[iV];
                    int iElement_iV = indicesVectors[iV] - 1;
                    int iVector_iV = iClass * nElement_iV + iElement_iV;
                    double value = vector_iV[iVector_iV];
                    prodValues *= value;
                }

                prodVectors[iProdVectors] = prodValues;
                advanceM(iteratorProd_R);
            }
        }
    } /* end main loop through vectors */
}


void
updateWSqrt(SEXP prior_R)
{
    double *WSqrt = REAL(GET_SLOT(prior_R, WSqrt_sym));
    double omegaAlpha = *REAL(GET_SLOT(prior_R, omegaAlpha_sym));
    double omegaDelta = *REAL(GET_SLOT(prior_R, omegaDelta_sym));

    /* WSqrt[0] = omegaAlpha*omegaAlpha; */
    /* WSqrt[3] = omegaDelta*omegaDelta; */
    WSqrt[0] = omegaAlpha;
    WSqrt[3] = omegaDelta;
}

void
updateWSqrtInvG(SEXP prior_R)
{
    double *WSqrtInvG = REAL(GET_SLOT(prior_R, WSqrtInvG_sym));
    double omegaAlpha = *REAL(GET_SLOT(prior_R, omegaAlpha_sym));
    double omegaDelta = *REAL(GET_SLOT(prior_R, omegaDelta_sym));
    double phi = *REAL(GET_SLOT(prior_R, phi_sym));

    WSqrtInvG[0] = 1/omegaAlpha;
    WSqrtInvG[2] = 1/omegaAlpha;
    WSqrtInvG[3] = phi/omegaDelta;
}


void
updateWeightMix(SEXP prior_R)
{
    double *weight = REAL(GET_SLOT(prior_R, weightMix_sym));
    double *compWeight = REAL(GET_SLOT(prior_R, componentWeightMix_sym));

    int indexClassMax = *INTEGER(GET_SLOT(prior_R, indexClassMaxMix_sym));

    int iAlong_r = *INTEGER(GET_SLOT(prior_R, iAlong_sym));
    int iAlong_c = iAlong_r -1;
    SEXP dimBeta_R = GET_SLOT(prior_R, dimBeta_sym);
    int *dimBeta = INTEGER(dimBeta_R);
    int nAlong = dimBeta[iAlong_c];

    int nWt = nAlong * indexClassMax;

    for (int iWt = 0; iWt < nWt; ++iWt) {

        weight[iWt] = pnorm(compWeight[iWt], 0, 1, 1, 0);

    }

    for (int iAlong = 0; iAlong < nAlong; ++iAlong) {

        double multiplierNext = 1;

        for (int iClass = 0; iClass < indexClassMax; ++iClass) {

            int iWtCurr = iClass * nAlong + iAlong;
            double multiplierCurr = multiplierNext;
            double thisWeight = weight[iWtCurr];
            multiplierNext = multiplierCurr * (1 - thisWeight);
            weight[iWtCurr] *= multiplierCurr;
        }
    }
}

void
updateTauNorm(SEXP prior_R, double *beta, int J)
{
    double A = *REAL(GET_SLOT(prior_R, ATau_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuTau_sym));
    double tau = *REAL(GET_SLOT(prior_R, tau_sym));
    double tauMax = *REAL(GET_SLOT(prior_R, tauMax_sym));
    int *allStrucZero = INTEGER(GET_SLOT(prior_R, allStrucZero_sym));

    double *beta_hat = (double *)R_alloc(J, sizeof(double));
    betaHat(beta_hat, prior_R, J);

    int n = 0;
    double V = 0;

    for (int j = 0; j < J; ++j) {
        if (!allStrucZero[j]) {
            n += 1;
            double diff = beta[j] - beta_hat[j];
            V += diff*diff;
        }
    }
    tau = updateSDNorm(tau, A, nu, V, n, tauMax);

    int successfullyUpdated = (tau > 0);

    if (successfullyUpdated) {

        SET_DOUBLESCALE_SLOT(prior_R, tau_sym, tau);
    }
}

void
updateTauRobust(SEXP prior_R, int J)
{
    double *UBeta = REAL(GET_SLOT(prior_R, UBeta_sym));
    double nuBeta = *REAL(GET_SLOT(prior_R, nuBeta_sym));
    double tau = *REAL(GET_SLOT(prior_R, tau_sym));
    double tauMax = *REAL(GET_SLOT(prior_R, tauMax_sym));
    int *allStrucZero = INTEGER(GET_SLOT(prior_R, allStrucZero_sym));

    double A = *REAL(GET_SLOT(prior_R, ATau_sym));
    double nuTau = *REAL(GET_SLOT(prior_R, nuTau_sym));

    int n = 0;
    double V = 0;

    for (int i = 0; i < J; ++i) {
        if (!allStrucZero[i]) {
            n += 1;
            V += 1/UBeta[i];
        }
    }

    tau = updateSDRobust(tau, A, nuBeta, nuTau, V, n, tauMax);
    int successfullyUpdated = (tau > 0);

    if (successfullyUpdated) {

        SET_DOUBLESCALE_SLOT(prior_R, tau_sym, tau);
    }
}

void
updateUBeta(SEXP prior_R, double *beta, int J)
{
    double *U = REAL(GET_SLOT(prior_R, UBeta_sym));
    double nu = *REAL(GET_SLOT(prior_R, nuBeta_sym));
    double tau = *REAL(GET_SLOT(prior_R, tau_sym));
    int *allStrucZero = INTEGER(GET_SLOT(prior_R, allStrucZero_sym));

    double *beta_hat = (double *)R_alloc(J, sizeof(double));
    betaHat(beta_hat, prior_R, J);

    double df = nu + 1;

    double nuTimesTauSq = nu * tau * tau;
    double thisScaleSq = 0;

    for (int j = 0; j < J; ++j) {
        if (!allStrucZero[j]) {
            double diff = beta[j] - beta_hat[j];
            thisScaleSq = (nuTimesTauSq + diff*diff)/df;
            U[j] = rinvchisq1(df, thisScaleSq);
        }
    }
}


/* *******************************************************
 * UPDATING MODELS
 ****************************************************** */

/* helper functions to calculate sums over logs of densities */
static __inline__ double
get_log_beta_dens(int n, double theta[],
                double shape1, double shape2)
{
    double result = 0.0;
    for (int i = 0; i < (n); ++i) {
        result += dbeta(theta[i], shape1, shape2, USE_LOG);
    }
    return result;
}

static __inline__ double
get_log_gamma_dens(int n, double theta[],
                double shape, double scale)
{
    double result = 0.0;
    for (int i = 0; i < (n); ++i) {
        result += dgamma(theta[i], shape, scale, USE_LOG);
    }
    return result;
}

void
updateAlphaLN2(SEXP object_R, SEXP y_R, SEXP exposure_R)
{
    int add1 = *INTEGER(GET_SLOT(object_R, add1_sym));
    SEXP alpha_R = GET_SLOT(object_R, alphaLN2_sym);
    double * alpha = REAL(alpha_R);
    int n_alphas = LENGTH(alpha_R);

    int * cell_in_lik = INTEGER(GET_SLOT(object_R, cellInLik_sym));
    int * n_cell_vec = INTEGER(GET_SLOT(object_R, nCellBeforeLN2_sym));

    int * constraint = INTEGER(GET_SLOT(object_R, constraintLN2_sym));

    SEXP transform_R = GET_SLOT(object_R, transformLN2_sym);

    double varsigma = *REAL(GET_SLOT(object_R, varsigma_sym));
    double sigma = *REAL(GET_SLOT(object_R, sigma_sym));
    double varsigma_sq = varsigma * varsigma;
    double sigma_sq = sigma * sigma;

    int n_y = LENGTH(y_R);
    int * y = INTEGER(y_R);
    int * exposure = INTEGER(exposure_R);
    double resid_vec[n_alphas]; /* array of doubles */
    for (int j = 0; j < n_alphas; ++j) { /* initialise to 0s */
        resid_vec[j] = 0.0;
    }

    double resid;
    
    for (int i = 0; i < n_y; ++i) {

        if (cell_in_lik[i]) {
	  if (add1) {
            resid = log1p(y[i]) - log1p(exposure[i]);
	  }
	  else {
	    resid = log(y[i]) - log(exposure[i]);
	  }
	  int j_r = dembase_getIAfter(i+1, transform_R);
	  resid_vec[j_r - 1] += resid;
        }
    }

    for (int j = 0; j < n_alphas; ++j) {

        int constraint_j = constraint[j];
        int update = ((constraint_j == NA_INTEGER) ||(constraint_j != 0));

        if (update) {
            int n_cell = n_cell_vec[j];
            double prec_data = n_cell/varsigma_sq;
            double prec_prior = 1/sigma_sq;

            double V = 1/(prec_data + prec_prior);

            double resid = resid_vec[j];
            double mean_post = prec_data* V * resid / n_cell;
            double sd_post = sqrt(V);

            if (constraint_j == NA_INTEGER) {
                alpha[j] = rnorm(mean_post, sd_post);
            }
            else if (constraint_j == -1) {
                alpha[j] = rtnorm1(mean_post, sd_post, R_NegInf, 0);
            }
            else if (constraint_j == 1) {
                alpha[j] = rtnorm1(mean_post, sd_post, 0, R_PosInf);
            }
            else {
                error("invalid value for 'constraint'");
            }
        }
    }
}

void
updatePriorsBetas(SEXP object_R)
{
  SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
  SEXP betas_R = GET_SLOT(object_R, betas_sym);
  int n_beta =  LENGTH(betas_R);
  double *thetaTransformed = REAL(GET_SLOT(object_R, thetaTransformed_sym));
  double sigma = *REAL(GET_SLOT(object_R, sigma_sym));
  for (int i_beta = 0; i_beta < n_beta; ++i_beta) {
    double *beta = REAL(VECTOR_ELT(betas_R, i_beta));
    SEXP prior_R = VECTOR_ELT(priors_R, i_beta);
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));
    updatePriorBeta(beta, J, prior_R, thetaTransformed, sigma);
  }
}


void
updateBetas(SEXP object_R)
{
  SEXP betas_R = GET_SLOT(object_R, betas_sym);
  SEXP meansBetas_R = GET_SLOT(object_R, meansBetas_sym);
  SEXP variancesBetas_R = GET_SLOT(object_R, variancesBetas_sym);
  SEXP priorsBetas_R = GET_SLOT(object_R, priorsBetas_sym);
  int *betaEqualsMean = LOGICAL(GET_SLOT(object_R, betaEqualsMean_sym));
  SEXP theta_R = GET_SLOT(object_R, theta_sym);
  double *theta = REAL(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object_R, thetaTransformed_sym));
  int *cellInLik = LOGICAL(GET_SLOT(object_R, cellInLik_sym));
  SEXP iteratorBetas_R = GET_SLOT(object_R, iteratorBetas_sym);
  double sigma = *REAL(GET_SLOT(object_R, sigma_sym));
  double sigma_sq = sigma * sigma;
  int n_beta =  LENGTH(betas_R);
  int n_theta =  LENGTH(theta_R);
  double *beta_ptr[n_beta];
  double *mean_ptr[n_beta];
  double *var_ptr[n_beta];
  int *struc_zero_ptr[n_beta];
  int J_vec[n_beta];
  int max_J = 0;
  for (int i_beta = 0; i_beta < n_beta; ++i_beta) {
    beta_ptr[i_beta] = REAL(VECTOR_ELT(betas_R, i_beta));
    mean_ptr[i_beta] = REAL(VECTOR_ELT(meansBetas_R, i_beta));
    var_ptr[i_beta] = REAL(VECTOR_ELT(variancesBetas_R, i_beta));
    SEXP prior_R = VECTOR_ELT(priorsBetas_R, i_beta);
    struc_zero_ptr[i_beta] = LOGICAL(GET_SLOT(prior_R, allStrucZero_sym));
    int J = *INTEGER(GET_SLOT(prior_R, J_sym));
    J_vec[i_beta] = J;
    if (J > max_J)
      max_J = J;
  }
  double *vbar = (double *)R_alloc(max_J, sizeof(double));
  int *n_vec = (int *)R_alloc(max_J, sizeof(int));
  for (int i_beta = 0; i_beta < n_beta; ++i_beta) {
    int J = J_vec[i_beta];
    if (betaEqualsMean[i_beta]) {
      for (int j = 0; j < J; ++j) {
    beta_ptr[i_beta][j] = mean_ptr[i_beta][j];
      }
    }
    else {
      getVBarAndN(vbar, n_vec,
          J, cellInLik,
          betas_R, iteratorBetas_R,
          theta, n_theta,
          thetaTransformed,
          n_beta, i_beta);
      for (int j = 0; j < J; ++j) {
    int all_struc_zero = struc_zero_ptr[i_beta][j];
    if (!all_struc_zero) {
      double mean_prior = mean_ptr[i_beta][j];
      double var_prior = var_ptr[i_beta][j];
      double val_post;
      if (var_prior > 0) {
        double prec_data = n_vec[j] / sigma_sq;
        double prec_prior = 1 / var_prior;
        double var_post = 1 / (prec_prior + prec_data);
        double mean_data = vbar[j];
        double mean_post = (prec_data * mean_data + prec_prior * mean_prior) * var_post;
        double sd_post = sqrt(var_post);
        val_post = rnorm(mean_post, sd_post);
      }
      else
        val_post = mean_prior;
      beta_ptr[i_beta][j] = val_post;
    }
      }
    }
  }
}


void
updateLogPostBetas(SEXP object_R)
{
  /* likelihood */
  SEXP thetaTransformed_R = GET_SLOT(object_R, thetaTransformed_sym);
  double *thetaTransformed = REAL(thetaTransformed_R);
  double *mu = REAL(GET_SLOT(object_R, mu_sym));
  int *cellInLik = LOGICAL(GET_SLOT(object_R, cellInLik_sym));
  double sigma = *REAL(GET_SLOT(object_R, sigma_sym));
  int n_theta = LENGTH(thetaTransformed_R);
  double log_likelihood = 0;
  for (int i_theta = 0; i_theta < n_theta; ++i_theta) {
    if (cellInLik[i_theta]) {
      log_likelihood += dnorm(thetaTransformed[i_theta],
                  mu[i_theta],
                  sigma,
                  USE_LOG);
    }
  }
  /* prior */
  SEXP betas_R = GET_SLOT(object_R, betas_sym);
  SEXP means_R = GET_SLOT(object_R, meansBetas_sym);
  SEXP variances_R = GET_SLOT(object_R, variancesBetas_sym);
  int *betaEqualsMean = LOGICAL(GET_SLOT(object_R, betaEqualsMean_sym));
  SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
  int n_beta = LENGTH(betas_R);
  double log_prior = 0;
  for (int i_beta = 0; i_beta < n_beta; ++i_beta) {
    if (!betaEqualsMean[i_beta]) {
      double *beta = REAL(VECTOR_ELT(betas_R, i_beta));
      double *mean = REAL(VECTOR_ELT(means_R, i_beta));
      double *variance = REAL(VECTOR_ELT(variances_R, i_beta));
      SEXP prior_R = VECTOR_ELT(priors_R, i_beta);
      int *allStrucZero = LOGICAL(GET_SLOT(prior_R, allStrucZero_sym));
      int J = *INTEGER(GET_SLOT(prior_R, J_sym));
      for (int j = 0; j < J; ++j) {
    if (!allStrucZero[j]) {
      log_prior += dnorm(beta[j], mean[j], sqrt(variance[j]), USE_LOG);
    }
      }
    }
  }
  /* combine */
  double logPostBetas = log_likelihood + log_prior;
  SET_DOUBLESCALE_SLOT(object_R, logPostBetas_sym, logPostBetas);
}



void
updateMeansBetas(SEXP object_R)
{
  SEXP means_R = GET_SLOT(object_R, meansBetas_sym);
  SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
  int n_beta =  LENGTH(means_R);

  for (int i = 0; i < n_beta; ++i) {
    SEXP mean_R = VECTOR_ELT(means_R, i);
    double *mean = REAL(mean_R);
    int J = LENGTH(mean_R);
    SEXP prior_R = VECTOR_ELT(priors_R, i);
    betaHat(mean, prior_R, J);
  }
}

void
updateMu(SEXP object_R)
{

  SEXP mu_R = GET_SLOT(object_R, mu_sym);
  double *mu = REAL(mu_R);
  int n_mu = LENGTH(mu_R);

  SEXP betas_R = GET_SLOT(object_R, betas_sym);
  int n_beta =  LENGTH(betas_R);

  SEXP iterator_R = GET_SLOT(object_R, iteratorBetas_sym);
  resetB(iterator_R);

  int *indices = INTEGER(GET_SLOT(iterator_R, indices_sym));

  double* beta_ptr[n_beta]; /* array of pointers */
  for (int b = 0; b < n_beta; ++b) {
    beta_ptr[b] = REAL(VECTOR_ELT(betas_R, b));
  }

  for (int i = 0; i < n_mu; ++i) {
    mu[i] = 0;
    for (int b = 0; b < n_beta; ++b) {
      mu[i] += beta_ptr[b][indices[b]-1];
    }
    advanceB(iterator_R);
  }

}



void
updateSigma_Varying(SEXP object)
{
  SEXP sigma_R = GET_SLOT(object, sigma_sym);
  double sigma = *REAL(GET_SLOT(sigma_R, Data_sym));
  double sigmaMax = *REAL(GET_SLOT(object, sigmaMax_sym));

  double A = *REAL(GET_SLOT(object, ASigma_sym));
  double nu = *REAL(GET_SLOT(object, nuSigma_sym));

  SEXP theta_R = GET_SLOT(object, theta_sym);
  int n_theta = LENGTH(theta_R);

  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));

  double *mu = REAL(GET_SLOT(object, mu_sym));

  int *cellInLik = LOGICAL(GET_SLOT(object, cellInLik_sym));

  double V = 0.0;
  int n = 0;

  for (int i = 0; i < n_theta; ++i) {
    if (cellInLik[i]) {
      double tmp = thetaTransformed[i] - mu[i];
      V += (tmp * tmp);
      n += 1;
    }
  }

  sigma = updateSDNorm(sigma, A, nu, V, n, sigmaMax);

  int successfullyUpdated = (sigma > 0);

  if (successfullyUpdated) {

    SET_DOUBLESCALE_SLOT(object, sigma_sym, sigma);
  }
}


void
updateSigmaLN2(SEXP object_R)
{
    SEXP alpha_R = GET_SLOT(object_R, alphaLN2_sym);
    double * alpha = REAL(alpha_R);
    int n_alphas = LENGTH(alpha_R);

    int * constraint = INTEGER(GET_SLOT(object_R, constraintLN2_sym));

    double sigma = *REAL(GET_SLOT(object_R, sigma_sym));
    double sigma_max = *REAL(GET_SLOT(object_R, sigmaMax_sym));
    double A = *REAL(GET_SLOT(object_R, ASigma_sym));
    double nu = *REAL(GET_SLOT(object_R, nuSigma_sym));
    double V = 0.0;
    int n = 0;

    for (int j = 0; j < n_alphas; ++j) {

        int constraint_j = constraint[j];

        if ((constraint_j == NA_INTEGER) ||(constraint_j != 0)) {
            double alpha_j = alpha[j];
            V += (alpha_j * alpha_j);
            n++;
        }
    }
    if (n > 0) {
        sigma = updateSDNorm(sigma, A, nu, V, n, sigma_max);
        int successfully_updated = (sigma > 0);

        if (successfully_updated) {
            SET_DOUBLESCALE_SLOT(object_R, sigma_sym, sigma);
        }
    }
    else {
        SET_DOUBLESCALE_SLOT(object_R, sigma_sym, 0.0);
    }
}

/* y_R and exposure_R are both Counts objects, g'teed to be integer  */
void
updateTheta_BinomialVarying(SEXP object, SEXP y_R, SEXP exposure_R)
{
  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R and exposure_R are all identical */

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  double scale_multiplier = *REAL(GET_SLOT(object, scaleThetaMultiplier_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));
  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));
  int *cellInLik = LOGICAL(GET_SLOT(object, cellInLik_sym));
  int *strucZeroArray = INTEGER(GET_SLOT(object, strucZeroArray_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  int *y = INTEGER(y_R);
  int *exposure = INTEGER(exposure_R);

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  scale = scale * scale_multiplier;

  for (int i = 0; i < n_theta; ++i) {
    
    int isStrucZero = strucZeroArray[i] == 0;
    if (!isStrucZero) {
      
      int this_y = y[i];
      int this_exposure = exposure[i];

      double mean = 0;
      double sd = 0;
      double theta_curr = theta[i]; /* only used if y not missing */
      double logit_th_curr = thetaTransformed[i]; /* only used if y not missing */

      int drawFromPrior = !cellInLik[i] || (this_y == NA_INTEGER);
      if (drawFromPrior) {
	mean = mu[i];
	sd = sigma;
      }
      else {
	mean = logit_th_curr;
	sd = scale * sqrt((this_exposure - this_y + 0.5) / ((this_exposure + 0.5) * (this_y + 0.5)));
      }

      int attempt = 0;
      int found_prop = 0;

      double logit_th_prop = 0.0;

      while( (!found_prop) && (attempt < maxAttempt) ) {

	++attempt;
	logit_th_prop = rnorm(mean, sd);

	found_prop = ( (logit_th_prop > lower + tolerance) &&
		       (logit_th_prop < upper - tolerance));
      } /* end while loop */

      if (found_prop) {

	double theta_prop = 0.0;

	if (logit_th_prop > 0) {
	  theta_prop = 1/(1+exp(-logit_th_prop));
	}
	else {
	  theta_prop = exp(logit_th_prop) / (1 + exp(logit_th_prop));
	}

	if (drawFromPrior) {
	  theta[i] = theta_prop;
	  thetaTransformed[i] = logit_th_prop;
	}
	else {
	  double loglik_prop = dbinom(this_y, this_exposure,
				      theta_prop, USE_LOG);
	  double loglik_curr = dbinom(this_y, this_exposure,
				      theta_curr, USE_LOG);
	  /* Calculate log of (prior density * proposal density). The Jacobians
	     from the transformation of variables cancel, as do the normal
	     densitites in the proposal distributions.*/
	  double log_dens_prop = dnorm(logit_th_prop, mu[i], sigma, USE_LOG);
	  double log_dens_curr = dnorm(logit_th_curr, mu[i], sigma, USE_LOG);

	  double log_diff = loglik_prop + log_dens_prop
	    - loglik_curr - log_dens_curr;

	  int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));
	  /* acceptance */
	  if (accept) {
	    ++n_accept_theta;
	    theta[i] = theta_prop;
	    thetaTransformed[i] = logit_th_prop;
	  }
	}
      }
      else  {
	++n_failed_prop_theta;
      }
    } /* end isStrucZero */
  } /* end loop through thetas */

    /* theta updated in place */
  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
}

/* y_R is g'teed to be integer
 * exposure_R is g'teed to be integer */
void
updateTheta_BinomialVaryingAgCertain(SEXP object, SEXP y_R, SEXP exposure_R)
{
  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R and exposure_R are all identical */

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double scale_theta = *REAL(GET_SLOT(object, scaleTheta_sym));
  double scale_theta_multiplier = *REAL(GET_SLOT(object, scaleThetaMultiplier_sym));
  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *mu = REAL(GET_SLOT(object, mu_sym));

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int *y = INTEGER(y_R);
  int *exposure = INTEGER(exposure_R);

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  scale_theta = scale_theta * scale_theta_multiplier;

  for (int i = 0; i < n_theta; ++i) {

    double scale_theta_i = scale_theta / sqrt(1 + log(1 + exposure[i]));

    int ir = i+1; /* R style index */

    int ir_other = makeIOther(ir, transformAg_R);
    int i_other = ir_other - 1;

    int is_in_delta = (ir_other >= 0);
    int is_has_other = 0;
    int is_weight_positive = 0;
    int is_weight_other_positive = 0;
    double weight = 0.0;
    double weight_other = 0.0;

    if (is_in_delta) {

      is_has_other = (ir_other > 0);

      weight = weightAg[i];
      is_weight_positive = (weight > 0);

      if (is_has_other) {
    weight_other = weightAg[i_other];
    is_weight_other_positive = (weight_other > 0);
      }
    }

    int value_fixed = (is_in_delta && is_weight_positive
               && !(is_has_other && is_weight_other_positive) );

    if (value_fixed) {
      /* skip to next value of theta */
      continue;
    }

    /* value is not fixed */
    int is_update_pair = (is_in_delta && is_weight_positive
              && is_has_other && is_weight_other_positive);

    double theta_curr = theta[i];
    double logit_th_curr = thetaTransformed[i];

    double theta_other_curr = (is_update_pair ? theta[i_other] : 0.0);

    double theta_prop = 0.0;
    double theta_other_prop = 0.0;
    double logit_th_prop = 0.0;
    double logit_th_other_prop = 0.0;
    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      logit_th_prop = rnorm(logit_th_curr, scale_theta_i);
      ++attempt;

      int inside_limits = ((logit_th_prop > lower + tolerance) &&
               (logit_th_prop < upper - tolerance));

      if (inside_limits) {

    if (logit_th_prop > 0) {
      theta_prop = 1/( 1 + exp(-logit_th_prop));
    }
    else {
      theta_prop =
        exp(logit_th_prop)/(1 + exp(logit_th_prop));
    }

    if (is_update_pair) {

      theta_other_prop = (theta_curr - theta_prop)
        * weight / weight_other + theta_other_curr;

      int is_theta_other_prop_valid = ( (theta_other_prop > 0.0)
                        && (theta_other_prop < 1.0) );

      if (is_theta_other_prop_valid) {
        logit_th_other_prop
          = log(theta_other_prop/(1- theta_other_prop));

        found_prop = ((logit_th_other_prop > lower + tolerance)
              && (logit_th_other_prop < upper - tolerance));
      }
    }
    else {
      found_prop = 1;
    }
      } /* end if inside limits */
    } /* end while loop */

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */

      ++n_failed_prop_theta;

      continue; /* go on to next theta */
    }

    int this_y = y[i];
    double this_mu = mu[i];

    int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

    double log_diff = 0;

    if (!y_is_missing) {

      int this_exposure = exposure[i];

      log_diff = dbinom(this_y, this_exposure,
            theta_prop, USE_LOG)
    - dbinom(this_y, this_exposure,
         theta_curr, USE_LOG);
    }

    if (is_update_pair) {

      int this_y_other = y[i_other];

      int y_other_is_missing = ( this_y_other == NA_INTEGER
                 || ISNA(this_y_other) );

      if (!y_other_is_missing) {
    int this_exposure_other = exposure[i_other];

    log_diff += dbinom(this_y_other, this_exposure_other,
               theta_other_prop, USE_LOG)
      - dbinom(this_y_other, this_exposure_other,
           theta_other_curr, USE_LOG);
      }

      double mu_other = mu[i_other];

      double logit_th_other_curr = thetaTransformed[i_other];

      double log_diff_prior =
    -log(theta_prop*(1-theta_prop))
    + dnorm(logit_th_prop, this_mu, sigma, USE_LOG)
    - log(theta_other_prop*(1-theta_other_prop))
    + dnorm(logit_th_other_prop, mu_other, sigma, USE_LOG)
    + log(theta_curr*(1-theta_curr))
    - dnorm(logit_th_curr, this_mu, sigma, USE_LOG)
    + log(theta_other_curr*(1-theta_other_curr))
    - dnorm(logit_th_other_curr, mu_other, sigma, USE_LOG);

      double log_diff_prop = safeLogProp_Binomial(logit_th_curr,
                          logit_th_other_curr,
                          logit_th_prop,
                          logit_th_other_prop,
                          scale_theta_i,
                          weight,
                          weight_other)
    - safeLogProp_Binomial(logit_th_prop,
                   logit_th_other_prop,
                   logit_th_curr,
                   logit_th_other_curr,
                   scale_theta_i,
                   weight,
                   weight_other);

      log_diff += log_diff_prior + log_diff_prop;

    }
    else {
      log_diff += dnorm(logit_th_prop, this_mu, sigma, USE_LOG)
    - dnorm(logit_th_curr, this_mu, sigma, USE_LOG);
    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));

    if (accept) {
      ++n_accept_theta;
      theta[i] = theta_prop;
      thetaTransformed[i] = logit_th_prop;

      if (is_update_pair) {
    theta[i_other] = theta_other_prop;
    thetaTransformed[i_other] = logit_th_other_prop;
      }
    }
    else {
    }
  } /* end for each theta */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
}


/* y_R is g'teed to be integer
 * exposure_R is g'teed to be integer */
void
updateThetaAndValueAgNormal_Binomial(SEXP object, SEXP y_R, SEXP exposure_R)
{

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R and exposure_R are all identical */

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  SEXP valueAg_R = GET_SLOT(object, valueAg_sym);
  int nValueAg = LENGTH(valueAg_R);
  double *valueAg = REAL(valueAg_R);
  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));
  double scaleAg = *REAL(GET_SLOT(object, scaleAg_sym));

  int n_accept_ag = 0;
  int n_failed_prop_value_ag = 0;

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int *y = INTEGER(y_R);
  int *exposure = INTEGER(exposure_R);

  /* malloc one (overlarge) space for all the 4 vecs */
  double *allVecs = (double *)R_alloc(4*n_theta, sizeof(double));
  double *vec_th_curr = allVecs;
  double *vec_logit_th_curr = allVecs + n_theta;
  double *vec_th_prop = allVecs + 2*n_theta;
  double *vec_logit_th_prop = allVecs + 3*n_theta;

  for (int k = 0; k < nValueAg; ++k) {

    int kr = k+1; /* R style index */

    SEXP iAg_R = dembase_getIBefore(kr, transformAg_R);
    int nAg = LENGTH(iAg_R);
    int *iAg = INTEGER(iAg_R);

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;
      double th_curr = theta[index];
      double logit_th_curr = thetaTransformed[index];
      vec_th_curr[i] = th_curr;
      vec_logit_th_curr[i] = logit_th_curr;
    }

    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      double increment = 0.0;

      for (int i = 0; i < nAg; ++i) {

    increment = rnorm(0, scaleAg);
    double logit_th_prop = vec_logit_th_curr[i] + increment;

    int inside_limits = ((logit_th_prop > lower + tolerance)
                 && (logit_th_prop < upper - tolerance));

    if (!inside_limits) { /* not in range */
      break; /* break out of the i-loop */
    }
    else {

      double theta_prop = 0.0;
      int valid = 0;
      if (logit_th_prop > 0) {
        theta_prop = 1/(1 + exp(-logit_th_prop));
        valid = (theta_prop < 1.0);
      }
      else {
        theta_prop = exp(logit_th_prop)/(1 + exp(logit_th_prop));
        valid = (theta_prop > 0.0);
      }

      if (!valid) break;
      else {
        vec_logit_th_prop[i] = logit_th_prop;
        vec_th_prop[i] = theta_prop;
        found_prop = (i == (nAg - 1));
      }
    }
      }
      /* found_prop is 0 if we had to break out of the loop */
    }

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */
      ++n_failed_prop_value_ag;

      continue; /* go on to next value ag */
    }

    double ag_prop = 0.0;
    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      ag_prop += vec_th_prop[i] * weightAg[index];
    }

    double ag_curr = valueAg[k];
    double mean_k = meanAg[k];
    double sd_k = sdAg[k];

    double log_diff_lik = 0.0;
    double log_diff_prior = 0.0;

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      int this_y = y[index];

      int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

      if (!y_is_missing) { /* does the is.observed bit */
    int this_exp = exposure[index];

    log_diff_lik += dbinom(this_y, this_exp, vec_th_prop[i], USE_LOG);
    log_diff_lik -= dbinom(this_y, this_exp, vec_th_curr[i], USE_LOG);
      }

      double this_mu = mu[index];

      log_diff_prior += dnorm(vec_logit_th_prop[i], this_mu, sigma, USE_LOG);
      log_diff_prior -= dnorm(vec_logit_th_curr[i], this_mu, sigma, USE_LOG);

    }

    double log_diff_ag = dnorm(mean_k, ag_prop, sd_k, USE_LOG)
      - dnorm(mean_k, ag_curr, sd_k, USE_LOG);

    double log_diff = log_diff_lik + log_diff_prior + log_diff_ag;

    if (!(log_diff < 0) || (runif(0, 1) < exp(log_diff))) {

      ++n_accept_ag;
      valueAg[k] = ag_prop;
      for (int i = 0; i < nAg; ++i) {
    int index = iAg[i] - 1;
    theta[index] = vec_th_prop[i];
    thetaTransformed[index] = vec_logit_th_prop[i];
      }
    }
  } /* end for each value ag and set of thetas */

  SET_INTSCALE_SLOT(object, nAcceptAg_sym, n_accept_ag);
  SET_INTSCALE_SLOT(object, nFailedPropValueAg_sym, n_failed_prop_value_ag);
}

void
updateThetaAndValueAgFun_Binomial(SEXP object, SEXP y_R, SEXP exposure_R)
{
  int *y = INTEGER(y_R);
  int *exposure = INTEGER(exposure_R);

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R are all identical */

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  double scale_multiplier = *REAL(GET_SLOT(object, scaleThetaMultiplier_sym));
  double sigma = *REAL(GET_SLOT(object, sigma_sym));
  scale *= scale_multiplier;

  double *valueAg = REAL(GET_SLOT(object, valueAg_sym));

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));

  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  SEXP funAg_R = GET_SLOT(object, funAg_sym);

  /* set up to be able to call the R function from C */
  SEXP call_R = NULL;
  /* call_R will be the final called object */
  PROTECT(call_R = allocList(3));
  SET_TYPEOF(call_R, LANGSXP);
  SETCAR(call_R, funAg_R); /* sets first value in list to this function*/

  SEXP xArgsAg_R = GET_SLOT(object, xArgsAg_sym);
  SEXP weightsArgsAg_R = GET_SLOT(object, weightsArgsAg_sym);
  double *tmp_x = NULL;
  int length_x_args_list = LENGTH(xArgsAg_R);
  int n_xs = 0;
  if (length_x_args_list > 0) {
    SEXP first_R = VECTOR_ELT(xArgsAg_R, 0);
    n_xs = length(first_R);
    tmp_x = (double *)R_alloc(n_xs, sizeof(double));
  }

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    int ir = i+1; /* R style index */

    int i_ag_r = dembase_getIAfter(ir, transformAg_R);
    int i_ag = i_ag_r - 1;

    int contributes_to_ag = (i_ag_r > 0);

    int this_y = y[i];
    int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

    int draw_straight_from_prior = (y_is_missing && !contributes_to_ag);

    int this_exposure = exposure[i];

    double theta_curr = theta[i];
    double logit_th_curr = thetaTransformed[i];

    double mean = mu[i];
    double sd = sigma;

    if (!y_is_missing) {

      mean = logit_th_curr;
      sd = scale * sqrt((this_exposure - this_y + 0.5) / ((this_exposure + 0.5) * (this_y + 0.5)));

    }

    int attempt = 0;
    int found_prop = 0;

    double logit_th_prop = 0.0;

#ifdef DEBUGGING
    PrintValue(mkString(""));
    PrintValue(mkString("--------------------------------"));
    PrintValue(mkString("i_r"));
    PrintValue(ScalarInteger(ir));
    PrintValue(mkString("contributes_to_ag"));
    PrintValue(ScalarInteger(contributes_to_ag));
    PrintValue(mkString("y_is_missing"));
    PrintValue(ScalarInteger(y_is_missing));
    PrintValue(mkString("draw straight_from_prior"));
    PrintValue(ScalarInteger(draw_straight_from_prior));
    PrintValue(mkString("this_y"));
    PrintValue(ScalarInteger(this_y));
    PrintValue(mkString("this_exposure"));
    PrintValue(ScalarInteger(this_exposure));
    PrintValue(mkString("theta_curr"));
    PrintValue(ScalarReal(theta_curr));
    PrintValue(mkString("logit_th_curr"));
    PrintValue(ScalarReal(logit_th_curr));
    PrintValue(mkString("mean"));
    PrintValue(ScalarReal(mean));
    PrintValue(mkString("sd"));
    PrintValue(ScalarReal(sd));
#endif


    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      logit_th_prop = rnorm(mean, sd);
      found_prop = ( (logit_th_prop > lower + tolerance) &&
             (logit_th_prop < upper - tolerance));

    }

#ifdef DEBUGGING
    PrintValue(mkString(""));
    PrintValue(mkString("after while loop"));
    PrintValue(mkString("found_prop"));
    PrintValue(ScalarInteger(found_prop));
    PrintValue(mkString("logit_th_prop"));
    PrintValue(ScalarReal(logit_th_prop));
#endif

    if (found_prop) {

      double exp_logit_theta_prop = exp(logit_th_prop);

      double theta_prop = exp_logit_theta_prop / ( 1 + exp_logit_theta_prop);
      if (logit_th_prop > 0) {
    theta_prop = 1 / ( 1 + 1/exp_logit_theta_prop );
      }

#ifdef DEBUGGING
      PrintValue(mkString(""));
      PrintValue(mkString("in if found prop"));
      PrintValue(mkString("theta_prop"));
      PrintValue(ScalarReal(theta_prop));
#endif

      if (draw_straight_from_prior) {
    theta[i] = theta_prop;
    thetaTransformed[i] = logit_th_prop;
#ifdef DEBUGGING
    PrintValue(mkString("drawing straight from prior"));
    PrintValue(mkString("theta[i]"));
    PrintValue(ScalarReal(theta[i]));
#endif
      }
      else {

    SEXP x_R = NULL;
    SEXP weight_R = NULL;
    double *x = NULL;

    if (contributes_to_ag) {

      x_R = VECTOR_ELT(xArgsAg_R, i_ag);
      weight_R = VECTOR_ELT(weightsArgsAg_R, i_ag);
      x = REAL(x_R);
      /* store these xs in case we need to restore them*/
      memcpy(tmp_x, x, n_xs*sizeof(double));
    }

    double log_diff = 0;

#ifdef DEBUGGING
    PrintValue(mkString("NOT drawing straight from prior"));

#endif

    if (!y_is_missing) {

      double log_lik_prop = dbinom(this_y, this_exposure, theta_prop, USE_LOG);
      double log_lik_curr = dbinom(this_y, this_exposure, theta_curr, USE_LOG);
      log_diff = log_lik_prop - log_lik_curr;

#ifdef DEBUGGING
      PrintValue(mkString("NOT y_is_missing"));
      PrintValue(mkString("theta_prop"));
      PrintValue(ScalarReal(theta_prop));
      PrintValue(mkString("theta_curr"));
      PrintValue(ScalarReal(theta_curr));
      PrintValue(mkString("log_lik_prop"));
      PrintValue(ScalarReal(log_lik_prop));
      PrintValue(mkString("log_lik_curr"));
      PrintValue(ScalarReal(log_lik_curr));
      PrintValue(mkString("log_diff"));
      PrintValue(ScalarReal(log_diff));

#endif
    }

    double log_dens_prop = dnorm(logit_th_prop, mu[i], sigma, USE_LOG);
    double log_dens_curr = dnorm(logit_th_curr, mu[i], sigma, USE_LOG);
    log_diff += (log_dens_prop - log_dens_curr);

#ifdef DEBUGGING
    PrintValue(mkString("log_dens_prop"));
    PrintValue(ScalarReal(log_dens_prop));
    PrintValue(mkString("log_dens_curr"));
    PrintValue(ScalarReal(log_dens_curr));
    PrintValue(mkString("log_diff"));
    PrintValue(ScalarReal(log_diff));
#endif


    double ag_prop = 0;

    if (contributes_to_ag) {

      double ag_curr = valueAg[i_ag];
      double mean_ag = meanAg[i_ag];
      double sd_ag = sdAg[i_ag];

      SEXP ir_shared_R;
      PROTECT( ir_shared_R
           = dembase_getIShared(ir, transformAg_R) );
      int n_ir_shared = LENGTH(ir_shared_R);
      int *ir_shared = INTEGER(ir_shared_R);

      for (int j = 0; j < n_ir_shared; ++j) {
        if ( i == (ir_shared[j] - 1) ) {
          x[j] = theta_prop;
          /* alters this x in the original R SEXP object */
        }
      }

      /* set 2nd and 3rd values in the function call object */
      SETCADR(call_R, x_R);
      SETCADDR(call_R, weight_R);

      /* call the supplied function */
      SEXP prop_R = PROTECT(eval(call_R, R_GlobalEnv));
      ag_prop = *REAL(prop_R);

      UNPROTECT(2); /* ir_shared_r, current prop_R */

      double log_dens_ag_prop = dnorm(mean_ag, ag_prop, sd_ag, USE_LOG);
      double log_dens_ag_curr = dnorm(mean_ag, ag_curr, sd_ag, USE_LOG);
      log_diff += log_dens_ag_prop - log_dens_ag_curr;

#ifdef DEBUGGING
      PrintValue(mkString("contributes to ag"));
      PrintValue(mkString("ag_prop"));
      PrintValue(ScalarReal(ag_prop));
      PrintValue(mkString("log_diff"));
      PrintValue(ScalarReal(log_diff));
#endif
    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));
    if (accept) {
      ++n_accept_theta;
      theta[i] = theta_prop;
      thetaTransformed[i] = logit_th_prop;
      if (contributes_to_ag) {
        /* x will have been updated in place already*/
        valueAg[i_ag] = ag_prop;
      }
    }
    else if (contributes_to_ag) {
      /* unmodify the x_ags */
      memcpy(x, tmp_x, n_xs*sizeof(double));
    }

#ifdef DEBUGGING
    PrintValue(mkString(""));
    PrintValue(mkString("accept"));
    PrintValue(ScalarInteger(accept));
    PrintValue(mkString("theta[i]"));
    PrintValue(ScalarReal(theta[i]));
    PrintValue(mkString("valueAg[i_ag]"));
    PrintValue(ScalarReal(valueAg[i_ag]));
    PrintValue(mkString("xArgsAg_R"));
    PrintValue(xArgsAg_R);

#endif
      }
    }
    else { /* not found prop */
      ++n_failed_prop_theta;
    }

  } /* end i-loop through thetas */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
  UNPROTECT(1); /* call_R */
}



void
updateThetaAndNu_CMPVaryingNotUseExp(SEXP object_R, SEXP y_R)
{
  SEXP theta_R = GET_SLOT(object_R, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object_R, thetaTransformed_sym));
  int *cellInLik = LOGICAL(GET_SLOT(object_R, cellInLik_sym));

  double *mu = REAL(GET_SLOT(object_R, mu_sym));

  double boxCoxParam = *REAL(GET_SLOT(object_R, boxCoxParam_sym));
  int usesBoxCoxTransform = (boxCoxParam > 0) ? 1 : 0;
  double oneOverBoxCoxParam = 1/boxCoxParam;

  double scale = *REAL(GET_SLOT(object_R, scaleTheta_sym));
  double scaleMultiplier = *REAL(GET_SLOT(object_R, scaleThetaMultiplier_sym));
  scale *= scaleMultiplier;

  double lower = *REAL(GET_SLOT(object_R, lower_sym));
  double upper = *REAL(GET_SLOT(object_R, upper_sym));
  double tolerance = *REAL(GET_SLOT(object_R, tolerance_sym));
  double sigma = *REAL(GET_SLOT(object_R, sigma_sym));
  double * nu = REAL(GET_SLOT(object_R, nuCMP_sym));
  double meanLogNu = *REAL(GET_SLOT(object_R, meanLogNuCMP_sym));
  double sdLogNu = *REAL(GET_SLOT(object_R, sdLogNuCMP_sym));

  int maxAttempt = *INTEGER(GET_SLOT(object_R, maxAttempt_sym));

  int *y = INTEGER(y_R);

  int n_failed_prop_y_star = 0;
  int n_failed_prop_theta = 0;
  int n_accept_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    int this_y = y[i];
    int y_is_missing = ( this_y == NA_INTEGER );

    int is_struc_zero = !cellInLik[i] && !y_is_missing && (this_y == 0);

    if (!is_struc_zero) {

      double mean = 0;
      double sd = 0;

      double th_curr = 0;
      double tr_th_curr = 0;

      if (y_is_missing) {
    mean = mu[i];
    sd = sigma;
      }
      else {
    th_curr = theta[i];
    tr_th_curr = thetaTransformed[i];
    mean = tr_th_curr;
    if (y_is_missing) {
      sd = scale / scaleMultiplier;
    }
    else {
      sd = scale / sqrt(1 + this_y);
    }
      }

      int attempt = 0;
      int found_prop_theta = 0;
      double tr_th_prop = 0;

      while( (!found_prop_theta) && (attempt < maxAttempt) ) {

    ++attempt;

    tr_th_prop = rnorm(mean, sd);

    found_prop_theta = ( ( tr_th_prop > (lower + tolerance) )
                 && ( tr_th_prop < (upper - tolerance) ) );
      }

      if (found_prop_theta) {

    double th_prop = 0;

    if (usesBoxCoxTransform) {
      th_prop = pow(boxCoxParam * tr_th_prop + 1, oneOverBoxCoxParam);
    }
    else {
      th_prop = exp(tr_th_prop);
    }

    if (y_is_missing) {
      theta[i] = th_prop;
      thetaTransformed[i] = tr_th_prop;
      nu[i] = rlnorm(meanLogNu, sdLogNu);
    }
    else {

      double nu_curr = nu[i];
      double log_nu_curr = log(nu_curr);
      double log_nu_prop = rnorm(log_nu_curr, sdLogNu);

      double nu_prop = exp(log_nu_prop);
      double y_star = rcmp1(th_prop, nu_prop, maxAttempt);

      int found_y_star = R_finite(y_star);

      if (found_y_star) {
        double logLikCurr = logDensCMPUnnormalised1(this_y, th_curr, nu_curr);
        double logLikProp = logDensCMPUnnormalised1(this_y, th_prop, nu_prop);
        double logLikCurrStar = logDensCMPUnnormalised1(y_star, th_curr, nu_curr);
        double logLikPropStar = logDensCMPUnnormalised1(y_star, th_prop, nu_prop);

        double logDensThCurr = dnorm(tr_th_curr, mu[i], sigma, USE_LOG);
        double logDensThProp = dnorm(tr_th_prop, mu[i], sigma, USE_LOG);
        double logDensNuCurr = dnorm(log_nu_curr, meanLogNu, sdLogNu, USE_LOG);
        double logDensNuProp = dnorm(log_nu_prop, meanLogNu, sdLogNu, USE_LOG);

        double logDiff = logLikProp - logLikCurr + logLikCurrStar - logLikPropStar
          + logDensThProp - logDensThCurr + logDensNuProp - logDensNuCurr;

        int accept = ( !(logDiff < 0) || ( runif(0,1) < exp(logDiff) ) );

#ifdef DEBUGGING
        PrintValue(mkString(""));
        PrintValue(mkString("logLikCurr"));
        PrintValue(ScalarReal(logLikCurr));
        PrintValue(mkString("logLikProp"));
        PrintValue(ScalarReal(logLikProp));
        PrintValue(mkString("logLikCurrStar"));
        PrintValue(ScalarReal(logLikCurrStar));
        PrintValue(mkString("logLikPropStar"));
        PrintValue(ScalarReal(logLikPropStar));
        PrintValue(mkString("logDensThCurr"));
        PrintValue(ScalarReal(logDensThCurr));
        PrintValue(mkString("logDensThProp"));
        PrintValue(ScalarReal(logDensThProp));
        PrintValue(mkString("logDensNuCurr"));
        PrintValue(ScalarReal(logDensNuCurr));
        PrintValue(mkString("logDensNuProp"));
        PrintValue(ScalarReal(logDensNuProp));

        PrintValue(mkString("logDiff"));
        PrintValue(ScalarReal(logDiff));
        PrintValue(mkString("accept"));
        PrintValue(ScalarInteger(accept));
#endif

        if (accept) {
          ++n_accept_theta;
          theta[i] = th_prop;
          thetaTransformed[i] = tr_th_prop;
          nu[i] = nu_prop;
        }
      }
      else {
        ++n_failed_prop_y_star;
      }
    }
      } /* end if found_prop_theta */
      else {
    ++n_failed_prop_theta;
      }
    } /* end if (!is_struc_zero) */

  } /* end loop through theta */

  SET_INTSCALE_SLOT(object_R, nFailedPropYStar_sym, n_failed_prop_y_star);
  SET_INTSCALE_SLOT(object_R, nFailedPropTheta_sym, n_failed_prop_theta);
  SET_INTSCALE_SLOT(object_R, nAcceptTheta_sym, n_accept_theta);
}


void
updateThetaAndNu_CMPVaryingUseExp(SEXP object_R, SEXP y_R, SEXP exposure_R)
{
  SEXP theta_R = GET_SLOT(object_R, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object_R, thetaTransformed_sym));
  int *cellInLik = LOGICAL(GET_SLOT(object_R, cellInLik_sym));

  double *mu = REAL(GET_SLOT(object_R, mu_sym));

  double boxCoxParam = *REAL(GET_SLOT(object_R, boxCoxParam_sym));
  int usesBoxCoxTransform = (boxCoxParam > 0) ? 1 : 0;
  double oneOverBoxCoxParam = 1/boxCoxParam;

  double scale = *REAL(GET_SLOT(object_R, scaleTheta_sym));
  double scaleMultiplier = *REAL(GET_SLOT(object_R, scaleThetaMultiplier_sym));
  scale *= scaleMultiplier;

  double lower = *REAL(GET_SLOT(object_R, lower_sym));
  double upper = *REAL(GET_SLOT(object_R, upper_sym));
  double tolerance = *REAL(GET_SLOT(object_R, tolerance_sym));
  double sigma = *REAL(GET_SLOT(object_R, sigma_sym));
  double * nu = REAL(GET_SLOT(object_R, nuCMP_sym));
  double meanLogNu = *REAL(GET_SLOT(object_R, meanLogNuCMP_sym));
  double sdLogNu = *REAL(GET_SLOT(object_R, sdLogNuCMP_sym));

  int maxAttempt = *INTEGER(GET_SLOT(object_R, maxAttempt_sym));

  int *y = INTEGER(y_R);
  double *exposure = REAL(exposure_R);

  int n_failed_prop_y_star = 0;
  int n_failed_prop_theta = 0;
  int n_accept_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    int this_y = y[i];
    int y_is_missing = ( this_y == NA_INTEGER );

    int is_struc_zero = !cellInLik[i] && !y_is_missing && (this_y == 0);

    if (!is_struc_zero) {

      double mean = 0;
      double sd = 0;

      double th_curr = 0;
      double tr_th_curr = 0;

      if (y_is_missing) {
    mean = mu[i];
    sd = sigma;
      }
      else {
    th_curr = theta[i];
    tr_th_curr = thetaTransformed[i];
    mean = tr_th_curr;
    sd = scale / sqrt(1 + this_y);
      }

      int attempt = 0;
      int found_prop_theta = 0;
      double tr_th_prop = 0;

      while( (!found_prop_theta) && (attempt < maxAttempt) ) {

    ++attempt;

    tr_th_prop = rnorm(mean, sd);

    found_prop_theta = ( ( tr_th_prop > (lower + tolerance) )
                 && ( tr_th_prop < (upper - tolerance) ) );
      }

      if (found_prop_theta) {

    double th_prop = 0;

    if (usesBoxCoxTransform) {
      th_prop = pow(boxCoxParam * tr_th_prop + 1, oneOverBoxCoxParam);
    }
    else {
      th_prop = exp(tr_th_prop);
    }

    if (y_is_missing) {
      theta[i] = th_prop;
      thetaTransformed[i] = tr_th_prop;
      nu[i] = rlnorm(meanLogNu, sdLogNu);
    }
    else {

      double nu_curr = nu[i];
      double log_nu_curr = log(nu_curr);
      double log_nu_prop = rnorm(log_nu_curr, sdLogNu);

      double nu_prop = exp(log_nu_prop);
      double this_exposure = exposure[i];
      double gamma_curr = th_curr * this_exposure;
      double gamma_prop = th_prop * this_exposure;
      double y_star = rcmp1(gamma_prop, nu_prop, maxAttempt);

      int found_y_star = R_finite(y_star);

      if (found_y_star) {

        double logLikCurr = logDensCMPUnnormalised1(this_y, gamma_curr, nu_curr);
        double logLikProp = logDensCMPUnnormalised1(this_y, gamma_prop, nu_prop);
        double logLikCurrStar = logDensCMPUnnormalised1(y_star, gamma_curr, nu_curr);
        double logLikPropStar = logDensCMPUnnormalised1(y_star, gamma_prop, nu_prop);

        double logDensThCurr = dnorm(tr_th_curr, mu[i], sigma, USE_LOG);
        double logDensThProp = dnorm(tr_th_prop, mu[i], sigma, USE_LOG);
        double logDensNuCurr = dnorm(log_nu_curr, meanLogNu, sdLogNu, USE_LOG);
        double logDensNuProp = dnorm(log_nu_prop, meanLogNu, sdLogNu, USE_LOG);

        double logDiff = logLikProp - logLikCurr + logLikCurrStar - logLikPropStar
          + logDensThProp - logDensThCurr + logDensNuProp - logDensNuCurr;

        int accept = ( !(logDiff < 0) || ( runif(0,1) < exp(logDiff) ) );

        if (accept) {
          ++n_accept_theta;
          theta[i] = th_prop;
          thetaTransformed[i] = tr_th_prop;
          nu[i] = nu_prop;
        }
      }
      else {
        ++n_failed_prop_y_star;
      }
    }
      } /* end if found_prop_theta */
      else {
    ++n_failed_prop_theta;
      }
    } /* end if !is_struc_zero */

  } /* end loop through theta */

  SET_INTSCALE_SLOT(object_R, nFailedPropYStar_sym, n_failed_prop_y_star);
  SET_INTSCALE_SLOT(object_R, nFailedPropTheta_sym, n_failed_prop_theta);
  SET_INTSCALE_SLOT(object_R, nAcceptTheta_sym, n_accept_theta);

}


/* y_R is a demographic array, g'teed to be doubles */
void
updateTheta_NormalVarying(SEXP object, SEXP y_R)
{

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  double *w = REAL(GET_SLOT(object, w_sym));
  /* n_theta and length of y_R and w are all identical */

  double sigma = *REAL(GET_SLOT(object, sigma_sym));
  double varsigma = *REAL(GET_SLOT(object, varsigma_sym));

  double *y = REAL(y_R);

  double prec_prior = 1/(sigma*sigma);
  double varsigma_sq = varsigma*varsigma;

  int n_failed_prop_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    double this_y = y[i];

    int y_is_missing = ( this_y == NA_REAL || ISNA(this_y) );

    double mean = 0;
    double sd = 0;

    if (y_is_missing) {
      mean = mu[i];
      sd = sigma;
    }
    else {
      double prec_data = w[i] / varsigma_sq;
      double var = 1/ (prec_data + prec_prior);
      mean = var * ( prec_prior * mu[i] + prec_data * this_y);
      sd = sqrt(var);
    }

    int attempt = 0;
    int found_prop = 0;
    double theta_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      theta_prop = rnorm(mean, sd);

      found_prop = ( ( theta_prop > (lower + tolerance) )
             && ( theta_prop < (upper - tolerance) ) );
    }

    if (found_prop) {
      theta[i] = theta_prop;
      thetaTransformed[i] = theta_prop;
    }
    else {
      ++n_failed_prop_theta;
    }

  } /* end loop through thetas */

  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
}

/* y_R is g'teed to be real */
void
updateTheta_NormalVaryingAgCertain(SEXP object, SEXP y_R)
{

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  int n_theta = LENGTH(theta_R);
  double * w = REAL(GET_SLOT(object, w_sym));
  /* n_theta and length of w and y_R and exposure_R are all identical */

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double varsigma = *REAL(GET_SLOT(object, varsigma_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *mu = REAL(GET_SLOT(object, mu_sym));

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

#ifdef DEBUGGING
  PrintValue(mkString("theta"));
  PrintValue(theta_R);
  SEXP mu_R = GET_SLOT(object, mu_sym);
  PrintValue(mkString("mu"));
  PrintValue(mu_R);
#endif

  double *y = REAL(y_R);

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    int ir = i+1; /* R style index */

    int ir_other = makeIOther(ir, transformAg_R);
    int i_other = ir_other - 1;

    int is_in_delta = (ir_other >= 0);
    int is_has_other = 0;
    int is_weight_positive = 0;
    int is_weight_other_positive = 0;
    double weight = 0.0;
    double weight_other = 0.0;

    if (is_in_delta) {

      is_has_other = (ir_other > 0);

      weight = weightAg[i];
      is_weight_positive = (weight > 0);

      if (is_has_other) {
    weight_other = weightAg[i_other];
    is_weight_other_positive = (weight_other > 0);
      }
    }

    int value_fixed = (is_in_delta && is_weight_positive
               && !(is_has_other && is_weight_other_positive) );

    if (value_fixed) {
      /* skip to next value of theta */
      continue;
    }

    /* value is not fixed */
    int is_update_pair = (is_in_delta && is_weight_positive
              && is_has_other && is_weight_other_positive);

    double th_curr = theta[i];

#ifdef DEBUGGING
    PrintValue(mkString(""));
    PrintValue(mkString("ir"));
    PrintValue(ScalarInteger(ir));
    PrintValue(mkString("th_curr"));
    PrintValue(ScalarReal(th_curr));
    PrintValue(mkString("ir_other"));
    PrintValue(ScalarInteger(ir_other));
#endif

    double th_prop = 0.0;
    double th_other_curr = 0.0;
    double th_other_prop = 0.0;
    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {
      ++attempt;

      th_prop = rnorm(th_curr, scale);

      int prop_in_range = ((th_prop > lower + tolerance) &&
               (th_prop < upper - tolerance));

      if (!prop_in_range) {

    continue; /* get another proposal */
      }

      /* prop is in range */

      if(is_update_pair) { /* constrained update of pair */

    th_other_curr = theta[i_other];

    th_other_prop = (th_curr - th_prop)
      * weight / weight_other + th_other_curr;

    found_prop = ( (th_other_prop > lower + tolerance)
               && (th_other_prop < upper - tolerance) );

      }
      else {
    found_prop = 1;
      }

    }    /* end while loop */

#ifdef DEBUGGING
    PrintValue(mkString("after while loop"));
    PrintValue(mkString("th_prop"));
    PrintValue(ScalarReal(th_prop));
    PrintValue(mkString("update pair"));
    PrintValue(ScalarInteger(is_update_pair));
    PrintValue(mkString("th_other_curr"));
    PrintValue(ScalarReal(th_other_curr));
    PrintValue(mkString("th_other_prop"));
    PrintValue(ScalarReal(th_other_prop));
    PrintValue(mkString("found_prop"));
    PrintValue(ScalarInteger(found_prop));
#endif

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */
      ++n_failed_prop_theta;

#ifdef DEBUGGING
      PrintValue(mkString("!found_prop"));
      PrintValue(ScalarInteger(n_failed_prop_theta));
#endif

      continue; /* go on to next theta */
    }

    double this_y = y[i];
    double this_mu = mu[i];

    int y_is_missing = ( this_y == NA_REAL || ISNA(this_y) );

#ifdef DEBUGGING
    PrintValue(mkString("this_y"));
    PrintValue(ScalarReal(this_y));
    PrintValue(mkString("y_is_missing"));
    PrintValue(ScalarInteger(y_is_missing));
#endif


    double log_diff = 0;

    if (!y_is_missing) {
      double this_sd = varsigma/sqrt(w[i]);

      log_diff += dnorm(this_y, th_prop,
            this_sd, USE_LOG)
    - dnorm(this_y, th_curr,
        this_sd, USE_LOG);
    }

#ifdef DEBUGGING
    PrintValue(mkString("after !y_is_missing. log_diff"));
    PrintValue(ScalarReal(log_diff));
#endif

    if(is_update_pair) {

      double this_y_other = y[i_other];
      double mu_other = mu[i_other];

      int y_other_is_missing = ( this_y_other == NA_REAL
                 || ISNA(this_y_other) );

      if (!y_other_is_missing) {
    double this_sd_other = varsigma/sqrt(w[i_other]);

    log_diff += dnorm(this_y_other, th_other_prop,
              this_sd_other, USE_LOG)
      - dnorm(this_y_other, th_other_curr,
          this_sd_other, USE_LOG);
      }

#ifdef DEBUGGING
      PrintValue(mkString("this_y_other"));
      PrintValue(ScalarReal(this_y_other));
      PrintValue(mkString("after y_other_is_missing"));
      PrintValue(mkString("log_diff"));
      PrintValue(ScalarReal(log_diff));
      PrintValue(mkString("this_mu"));
      PrintValue(ScalarReal(this_mu));
      PrintValue(mkString("mu_other"));
      PrintValue(ScalarReal(mu_other));
#endif

      double log_diff_prior =
    dnorm(th_prop, this_mu, sigma, USE_LOG)
    + dnorm(th_other_prop, mu_other, sigma, USE_LOG)
    - dnorm(th_curr, this_mu, sigma, USE_LOG)
    - dnorm(th_other_curr, mu_other, sigma, USE_LOG);

      double weight_ratio = fabs(weight/weight_other);
      double log_diff_prop = log( dnorm(th_curr, th_prop, scale, NOT_USE_LOG)
                  + weight_ratio*dnorm(th_other_curr, th_other_prop,  scale, NOT_USE_LOG) )
    - log( dnorm(th_prop,  th_curr, scale, NOT_USE_LOG)
           + weight_ratio*dnorm(th_other_prop,  th_other_curr, scale, NOT_USE_LOG) );

      log_diff += log_diff_prior + log_diff_prop;

#ifdef DEBUGGING
      PrintValue(mkString("final log_diff if is_update_pair"));
      PrintValue(mkString("weight_ratior"));
      PrintValue(ScalarReal(weight_ratio));
      PrintValue(mkString("log_diff_prior"));
      PrintValue(ScalarReal(log_diff_prior));
      PrintValue(mkString("log_diff_prop"));
      PrintValue(ScalarReal(log_diff_prop));
      PrintValue(mkString("final log_diff"));
      PrintValue(ScalarReal(log_diff));
#endif

    }
    else {

      /* proposal densities cancel */
      log_diff +=
    dnorm(th_prop, this_mu, sigma, USE_LOG)
    - dnorm(th_curr, this_mu, sigma, USE_LOG);

#ifdef DEBUGGING
      PrintValue(mkString("final log_diff if not is_update_pair"));
      PrintValue(mkString("log_diff"));
      PrintValue(ScalarReal(log_diff));
#endif

    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));

    if (accept) {

      ++n_accept_theta;

      theta[i] = th_prop;
      thetaTransformed[i] = th_prop;

#ifdef DEBUGGING
      PrintValue(mkString("accept"));
      PrintValue(mkString("n_accept_theta"));
      PrintValue(ScalarInteger(n_accept_theta));
      PrintValue(mkString("theta[i]"));
      PrintValue(ScalarReal(theta[i]));
#endif

      if(is_update_pair) {

    theta[i_other] = th_other_prop;
    thetaTransformed[i_other] = th_other_prop;

#ifdef DEBUGGING
    PrintValue(mkString("theta[i_other"));
    PrintValue(ScalarReal(theta[i_other]));
#endif
      }
    }
    else {
#ifdef DEBUGGING
      PrintValue(mkString("not accepted"));

#endif

    }
  } /* end for each theta */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
}

/* y_R is g'teed to be real
 * betas is a list, length same as length of the iteratorBetas' indices */
void
updateThetaAndValueAgNormal_Normal(SEXP object, SEXP y_R)
{
  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *w = REAL(GET_SLOT(object, w_sym));
  /* n_theta and length of w and y_R are all identical */
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));

  double varsigma = *REAL(GET_SLOT(object, varsigma_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  SEXP valueAg_R = GET_SLOT(object, valueAg_sym);
  int nValueAg = LENGTH(valueAg_R);
  double *valueAg = REAL(valueAg_R);
  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));
  double scaleAg = *REAL(GET_SLOT(object, scaleAg_sym));

  int n_accept_ag = 0;
  int n_failed_prop_value_ag = 0;

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  double *y = REAL(y_R);

  /* malloc one (overlarge) space for all the 2 vecs */
  double *allVecs = (double *)R_alloc(2*n_theta, sizeof(double));
  double *vec_th_curr = allVecs;
  double *vec_th_prop = allVecs + n_theta;

  for (int k = 0; k < nValueAg; ++k) {

    int kr = k+1; /* R style index */

    SEXP iAg_R = dembase_getIBefore(kr, transformAg_R);
    int nAg = LENGTH(iAg_R);
    int *iAg = INTEGER(iAg_R);

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;
      double th_curr = theta[index];
      vec_th_curr[i] = th_curr;
    }

    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      double increment = 0.0;

      for (int i = 0; i < nAg; ++i) {

    increment = rnorm(0, scaleAg);
    double th_prop = vec_th_curr[i] + increment;

    int inside_limits = ((th_prop > lower + tolerance)
                 && (th_prop < upper - tolerance));

    if (!inside_limits) { /* not in range */
      break; /* break out of the i-loop through benchmarked indices */
    }
    else {

      vec_th_prop[i] = th_prop;
      found_prop = (i == (nAg -1));
    }
      }
    }
    /* found_prop is 0 if we had to break out of the loop */

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */
      ++n_failed_prop_value_ag;

      continue; /* go on to next value benchmark */
    }

    double ag_prop = 0.0;
    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      ag_prop += vec_th_prop[i] * weightAg[index];
    }

    double ag_curr = valueAg[k];
    double mean_k = meanAg[k];
    double sd_k = sdAg[k];

    double log_diff_lik = 0.0;
    double log_diff_prior = 0.0;

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      double this_y = y[index];
      double this_sd = varsigma/sqrt(w[index]);
      double this_mu = mu[index];

      int y_is_missing = ( this_y == NA_REAL || ISNA(this_y) );

      if (!y_is_missing) { /* does the is.observed bit */

    log_diff_lik += dnorm(this_y, vec_th_prop[i], this_sd, USE_LOG);
    log_diff_lik -= dnorm(this_y, vec_th_curr[i], this_sd, USE_LOG);
      }

      log_diff_prior += dnorm(vec_th_prop[i], this_mu,
                  sigma, USE_LOG);
      log_diff_prior -= dnorm(vec_th_curr[i], this_mu,
                  sigma, USE_LOG);

    }

    double log_diff_ag = dnorm(mean_k, ag_prop, sd_k, USE_LOG)
      - dnorm(mean_k, ag_curr, sd_k, USE_LOG);

    double log_diff = log_diff_lik + log_diff_prior + log_diff_ag;

    if ( !(log_diff < 0) || (runif(0, 1) < exp(log_diff))) {

      ++n_accept_ag;
      valueAg[k] = ag_prop;
      for (int i = 0; i < nAg; ++i) {
    int index = iAg[i] - 1;
    theta[index] = vec_th_prop[i];
    thetaTransformed[index] = vec_th_prop[i];
      }
    }
  } /* end for each value benchmark and set of thetas */

  SET_INTSCALE_SLOT(object, nAcceptAg_sym, n_accept_ag);
  SET_INTSCALE_SLOT(object, nFailedPropValueAg_sym, n_failed_prop_value_ag);
}

void
updateThetaAndValueAgFun_Normal(SEXP object, SEXP y_R)
{
  double *y = REAL(y_R);

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  double *mu = REAL(GET_SLOT(object, mu_sym));
  int n_theta = LENGTH(theta_R);
  double *w = REAL(GET_SLOT(object, w_sym));
  /* n_theta and length of w and y_R are all identical */

  double varsigma = *REAL(GET_SLOT(object, varsigma_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  double *valueAg = REAL(GET_SLOT(object, valueAg_sym));

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));

  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  SEXP funAg_R = GET_SLOT(object, funAg_sym);

  /* set up to be able to call the R function from C */
  SEXP call_R = NULL;
  /* call_R will be the final called object */
  PROTECT(call_R = allocList(3));
  SET_TYPEOF(call_R, LANGSXP);
  SETCAR(call_R, funAg_R); /* sets first value in list to this function*/

  SEXP xArgsAg_R = GET_SLOT(object, xArgsAg_sym);
  SEXP weightsArgsAg_R = GET_SLOT(object, weightsArgsAg_sym);
  double *tmp_x = NULL;
  int length_x_args_list = LENGTH(xArgsAg_R);
  int n_xs = 0;
  if (length_x_args_list > 0) {
    SEXP first_R = VECTOR_ELT(xArgsAg_R, 0);
    n_xs = length(first_R);
    tmp_x = (double *)R_alloc(n_xs, sizeof(double));
  }

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    int ir = i+1; /* R style index */

    int i_ag_r = dembase_getIAfter(ir, transformAg_R);
    int i_ag = i_ag_r - 1;

    int contributes_to_ag = (i_ag_r > 0);

    double this_y = y[i];
    int y_is_missing = ( this_y == NA_REAL || ISNA(this_y) );

    int draw_straight_from_prior = (y_is_missing && !contributes_to_ag);

    double th_curr = theta[i];

    double mean = mu[i];
    double sd = sigma;

    double this_w = w[i];
    double this_varsigma_over_sqrtw = varsigma/sqrt(this_w);

    if (!y_is_missing) {

      mean = th_curr;
      sd = scale/sqrt(this_w);
    }

    int attempt = 0;
    int found_prop = 0;

    double th_prop = 0.0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      th_prop = rnorm(mean, sd);
      found_prop = ( (th_prop > lower + tolerance) &&
             (th_prop < upper - tolerance));

    }

    if (found_prop) {

      if (draw_straight_from_prior) {
    theta[i] = th_prop;
    thetaTransformed[i] = th_prop;
      }
      else {

    SEXP x_R = NULL;
    SEXP weight_R = NULL;
    double *x = NULL;

    if (contributes_to_ag) {

      x_R = VECTOR_ELT(xArgsAg_R, i_ag);
      weight_R = VECTOR_ELT(weightsArgsAg_R, i_ag);
      x = REAL(x_R);
      /* store these xs in case we need to restore them*/
      memcpy(tmp_x, x, n_xs*sizeof(double));
    }

    double log_diff = 0;

    if (!y_is_missing) {

      double log_lik_prop = dnorm(this_y, th_prop,
                      this_varsigma_over_sqrtw, USE_LOG);
      double log_lik_curr = dnorm(this_y, th_curr,
                      this_varsigma_over_sqrtw, USE_LOG);
      log_diff = log_lik_prop - log_lik_curr;
    }

    double log_dens_prop = dnorm(th_prop, mu[i], sigma, USE_LOG);
    double log_dens_curr = dnorm(th_curr, mu[i], sigma, USE_LOG);
    log_diff += (log_dens_prop - log_dens_curr);

    double ag_prop = 0;

    if (contributes_to_ag) {

      double ag_curr = valueAg[i_ag];
      double mean_ag = meanAg[i_ag];
      double sd_ag = sdAg[i_ag];

      SEXP ir_shared_R;
      PROTECT( ir_shared_R
           = dembase_getIShared(ir, transformAg_R) );
      int n_ir_shared = LENGTH(ir_shared_R);
      int *ir_shared = INTEGER(ir_shared_R);

      for (int j = 0; j < n_ir_shared; ++j) {
        if ( i == (ir_shared[j] - 1) ) {
          x[j] = th_prop;
          /* alters this x in the original R SEXP object */
        }
      }

      /* set 2nd and 3rd values in the function call object */
      SETCADR(call_R, x_R);
      SETCADDR(call_R, weight_R);

      /* call the supplied function */
      SEXP prop_R = PROTECT(eval(call_R, R_GlobalEnv));
      ag_prop = *REAL(prop_R);

      UNPROTECT(2); /* ir_shared_r, current prop_R */

      double log_dens_ag_prop = dnorm(mean_ag, ag_prop, sd_ag, USE_LOG);
      double log_dens_ag_curr = dnorm(mean_ag, ag_curr, sd_ag, USE_LOG);
      log_diff += log_dens_ag_prop - log_dens_ag_curr;
    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));
    if (accept) {
      ++n_accept_theta;
      theta[i] = th_prop;
      thetaTransformed[i] = th_prop;
      if (contributes_to_ag) {
        /* x will have been updated in place already*/
        valueAg[i_ag] = ag_prop;
      }
    }
    else if (contributes_to_ag) {
      /* unmodify the x_ags */
      memcpy(x, tmp_x, n_xs*sizeof(double));
    }
      }
    }
    else { /* not found prop */
      ++n_failed_prop_theta;
    }

  } /* end i-loop through thetas */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
  UNPROTECT(1); /* call_R */
}

/* y_R is a  Counts object, g'teed to be integer */
void
updateTheta_PoissonVaryingNotUseExp(SEXP object, SEXP y_R)
{
  double boxCoxParam = *REAL(GET_SLOT(object, boxCoxParam_sym));
  int usesBoxCoxTransformation = (boxCoxParam > 0);

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  int *cellInLik = LOGICAL(GET_SLOT(object, cellInLik_sym));
  /* n_theta and length of y_R and cellInLik are all identical */

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));
  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  double scale_multiplier = *REAL(GET_SLOT(object, scaleThetaMultiplier_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  int *y = INTEGER(y_R); /* length n_theta */

  /* make an array that has 0's if y is there, 1s if missing */
  int *yMissing = (int *)R_alloc(n_theta, sizeof(int));
  memset(yMissing, 0, n_theta * sizeof(int));

  for (int i = 0; i < n_theta; ++i) {
    int this_y = y[i];
    if ( this_y == NA_INTEGER || ISNA(this_y) ) {
      yMissing[i] = 1;
    }
  }

  SEXP transformSubtotals_R = NULL;

  int has_subtotals = ( R_has_slot(y_R, subtotals_sym) );
  if (has_subtotals) {

    transformSubtotals_R = GET_SLOT(y_R, transformSubtotals_sym);
  }

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  scale = scale * scale_multiplier;

  for (int i = 0; i < n_theta; ++i) {

    int this_y = y[i];
    int y_is_missing = yMissing[i];

        int is_struc_zero = !cellInLik[i] && !y_is_missing && (this_y == 0);

    if (!is_struc_zero) {

      int ir = i + 1; /* R index */

      double mean = 0;
      double sd = 0;
      double theta_curr = theta[i];
      double transformedThetaCurr = thetaTransformed[i];

      int use_subtotal = 0;
      int ir_after = 0;
      if (y_is_missing && has_subtotals) {
    ir_after = dembase_getIAfter(ir, transformSubtotals_R);
    use_subtotal = (ir_after > 0);
      }

      int draw_straight_from_prior = (y_is_missing && !use_subtotal);

      if (draw_straight_from_prior) {

    mean = mu[i];
    sd = sigma;

      }
      else {

    mean = transformedThetaCurr;

    if (y_is_missing) {
      sd = scale / scale_multiplier;
    }
    else {
      sd = scale / sqrt(1 + this_y);
    }

      }

      double transformedThetaProp = 0.0;

      int attempt = 0;
      int found_prop = 0;

      while( (!found_prop) && (attempt < maxAttempt) ) {

    ++attempt;

        transformedThetaProp = rnorm(mean, sd);
        found_prop = ( (transformedThetaProp > lower + tolerance) &&
               (transformedThetaProp < upper - tolerance));
      }

      if (found_prop) {

        double theta_prop = 0;
        if (usesBoxCoxTransformation) {
      theta_prop = pow(boxCoxParam * transformedThetaProp + 1, 1/boxCoxParam);
        }
        else {
      theta_prop = exp(transformedThetaProp);
        }

        if (draw_straight_from_prior) {
      theta[i] = theta_prop;
      thetaTransformed[i] = transformedThetaProp;
        }
        else {

      double log_lik_prop = 0;
      double log_lik_curr = 0;

      if (use_subtotal) {

        int *subtotals = INTEGER(GET_SLOT(y_R, subtotalsNet_sym));
        int i_after = ir_after -1;
        int subtotal = subtotals[i_after];

        SEXP ir_shared_R;
        PROTECT( ir_shared_R
             = dembase_getIShared(ir, transformSubtotals_R) );

        int n_ir_shared = LENGTH(ir_shared_R);
        int *ir_shared = INTEGER(ir_shared_R);

        double lambda_curr = 0;
        for (int j = 0; j < n_ir_shared; ++j) {
          int shared_index = ir_shared[j] - 1;
          if (yMissing[shared_index]) {
        lambda_curr += theta[shared_index];
          }
        }

        UNPROTECT(1); /* ir_shared_R */

        double lambda_prop = lambda_curr + theta_prop - theta_curr;
        log_lik_prop = dpois(subtotal, lambda_prop, USE_LOG);
        log_lik_curr = dpois(subtotal, lambda_curr, USE_LOG);

      }
      else {

        log_lik_prop = dpois(this_y, theta_prop, USE_LOG);
        log_lik_curr = dpois(this_y, theta_curr, USE_LOG);
      }

      double log_dens_prop = dnorm(transformedThetaProp, mu[i], sigma, USE_LOG);
      double log_dens_curr = dnorm(transformedThetaCurr, mu[i], sigma, USE_LOG);
      double log_diff = (log_lik_prop + log_dens_prop
                 - log_lik_curr - log_dens_curr);

      int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));
      if (accept) {
        ++n_accept_theta;
        theta[i] = theta_prop;
        thetaTransformed[i] = transformedThetaProp;
      }
        }
      }
      else { /* not found prop */
    ++n_failed_prop_theta;
      }

    } /* end if (!is_struc_zero) */
  } /* end loop through thetas */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);

}

/* y_R and exposure_R are both Counts objects,
 * y_R is integer,
 * exposure_R is doubles,
 * exposure is g'teed not to be null */
void
updateTheta_PoissonVaryingUseExp(SEXP object, SEXP y_R, SEXP exposure_R)
{
  double boxCoxParam = *REAL(GET_SLOT(object, boxCoxParam_sym));
  int usesBoxCoxTransformation = (boxCoxParam > 0);

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  int *cellInLik = LOGICAL(GET_SLOT(object, cellInLik_sym));
  /* n_theta and length of y_R, cellInLik, and exposure_R are all identical */

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));
  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  double scale_multiplier = *REAL(GET_SLOT(object, scaleThetaMultiplier_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  int *y = INTEGER(y_R);

  /* make an array that has 0's if y is there, 1s if missing */
  int *yMissing = (int *)R_alloc(n_theta, sizeof(int));
  memset(yMissing, 0, n_theta * sizeof(int));

  for (int i = 0; i < n_theta; ++i) {
    int this_y = y[i];
    if ( this_y == NA_INTEGER || ISNA(this_y) ) {
      yMissing[i] = 1;
    }
  }

  double *exposure = REAL(exposure_R);

  SEXP transformSubtotals_R = NULL;

  int has_subtotals = ( R_has_slot(y_R, subtotals_sym) );
  if (has_subtotals) {
    transformSubtotals_R = GET_SLOT(y_R, transformSubtotals_sym);
  }

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  scale = scale * scale_multiplier;

  for (int i = 0; i < n_theta; ++i) {

    int this_y = y[i];
    int y_is_missing = yMissing[i];

    int is_struc_zero = !cellInLik[i] && !y_is_missing && (this_y == 0);

    if (!is_struc_zero) {

      int ir = i + 1; /* R index */

      double mean = 0;
      double sd = 0;
      double theta_curr = theta[i];
      double transformedThetaCurr = thetaTransformed[i];

      int use_subtotal = 0;
      int ir_after = 0;
      if (y_is_missing && has_subtotals) {
    ir_after = dembase_getIAfter(ir, transformSubtotals_R);
    use_subtotal = (ir_after > 0);
      }

      int draw_straight_from_prior = (y_is_missing && !use_subtotal);

      if (draw_straight_from_prior) {
    mean = mu[i];
    sd = sigma;
      }
      else {
        mean = transformedThetaCurr;
        if (y_is_missing) {
      sd = scale / scale_multiplier;
        }
        else {
      sd = scale / sqrt(1 + this_y);
        }
      }

      int attempt = 0;
      int found_prop = 0;

      double transformedThetaProp = 0.0;

      while( (!found_prop) && (attempt < maxAttempt) ) {

    attempt++;

    transformedThetaProp = rnorm(mean, sd);
        found_prop = ( (transformedThetaProp > lower + tolerance) &&
               (transformedThetaProp < upper - tolerance));

      }

      if (found_prop) {

        double theta_prop = 0;
        if (usesBoxCoxTransformation) {
      theta_prop = pow(boxCoxParam * transformedThetaProp + 1, 1/boxCoxParam);
        }
        else {
      theta_prop = exp(transformedThetaProp);
        }

        if (draw_straight_from_prior) {
      theta[i] = theta_prop;
      thetaTransformed[i] = transformedThetaProp;
        }
        else {

      double log_lik_prop = 0;
      double log_lik_curr = 0;

      double this_exposure = exposure[i];

      if (use_subtotal) {

            int *subtotals = INTEGER(GET_SLOT(y_R, subtotalsNet_sym));
            int i_after = ir_after -1;
            int subtotal = subtotals[i_after];

            SEXP ir_shared_R;
            PROTECT( ir_shared_R
             = dembase_getIShared(ir, transformSubtotals_R) );
            int n_ir_shared = LENGTH(ir_shared_R);
            int *ir_shared = INTEGER(ir_shared_R);

            double lambda_curr = 0;
            for (int j = 0; j < n_ir_shared; ++j) {
          int shared_index = ir_shared[j] - 1;
          if (yMissing[shared_index]) {
                lambda_curr += theta[shared_index]
          * exposure[ shared_index ];
          }
            }

            UNPROTECT(1); /* ir_shared_R */

            double lambda_prop = lambda_curr
          + (theta_prop - theta_curr) * this_exposure;
            log_lik_prop = dpois(subtotal, lambda_prop, USE_LOG);
            log_lik_curr = dpois(subtotal, lambda_curr, USE_LOG);

      }
      else {

            log_lik_prop = dpois(this_y, theta_prop*this_exposure, USE_LOG);
            log_lik_curr = dpois(this_y, theta_curr*this_exposure, USE_LOG);
      }

      double log_dens_prop = dnorm(transformedThetaProp, mu[i], sigma, USE_LOG);
      double log_dens_curr = dnorm(transformedThetaCurr, mu[i], sigma, USE_LOG);

      double log_diff = (log_lik_prop + log_dens_prop
                 - log_lik_curr - log_dens_curr);

      int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));
      if (accept) {
            ++n_accept_theta;
            theta[i] = theta_prop;
        thetaTransformed[i] = transformedThetaProp;
      }
        }
      }
      else { /* not found prop */
        ++n_failed_prop_theta;
      }

    } /* end if (!is_struc_zero) */
  } /* end loop through thetas */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
}



/* y_R a Counts object,
 * y_R is integer */
void
updateTheta_PoissonVaryingNotUseExpAgCertain(SEXP object, SEXP y_R)
{
  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R are all identical */

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double scale_theta = *REAL(GET_SLOT(object, scaleTheta_sym));
  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *mu = REAL(GET_SLOT(object, mu_sym));

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int *y = INTEGER(y_R);

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    int ir = i+1; /* R style index */

    int ir_other = makeIOther(ir, transformAg_R);
    int i_other = ir_other - 1;

    int is_in_delta = (ir_other >= 0);
    int is_has_other = 0;
    int is_weight_positive = 0;
    int is_weight_other_positive = 0;
    double weight = 0.0;
    double weight_other = 0.0;

    if (is_in_delta) {

      is_has_other = (ir_other > 0);

      weight = weightAg[i];
      is_weight_positive = (weight > 0);

      if (is_has_other) {
    weight_other = weightAg[i_other];
    is_weight_other_positive = (weight_other > 0);
      }
    }

    int value_fixed = (is_in_delta && is_weight_positive
               && !(is_has_other && is_weight_other_positive) );

#ifdef DEBUGGING
    PrintValue(mkString(""));
    PrintValue(mkString("ir"));
    PrintValue(ScalarInteger(ir));
    PrintValue(mkString("ir_other"));
    PrintValue(ScalarInteger(ir_other));
    PrintValue(mkString("is_in_delta"));
    PrintValue(ScalarInteger(is_in_delta));
    PrintValue(mkString("is_has_other"));
    PrintValue(ScalarInteger(is_has_other));
    PrintValue(mkString("weight"));
    PrintValue(ScalarReal(weight));
    PrintValue(mkString("weight_other"));
    PrintValue(ScalarReal(weight_other));
    PrintValue(mkString("value_fixed"));
    PrintValue(ScalarInteger(value_fixed));
#endif

    if (value_fixed) {
      /* skip to next value of theta */
      continue;
    }

    /* value is not fixed */
    int is_update_pair = (is_in_delta && is_weight_positive
              && is_has_other && is_weight_other_positive);

    double theta_curr = theta[i];
    double log_th_curr = thetaTransformed[i];

    double theta_other_curr = (is_update_pair ? theta[i_other] : 0.0);

#ifdef DEBUGGING
    PrintValue(mkString("is_update_pair"));
    PrintValue(ScalarInteger(is_update_pair));
    PrintValue(mkString("theta_curr"));
    PrintValue(ScalarReal(theta_curr));
    PrintValue(mkString("log_th_curr"));
    PrintValue(ScalarReal(log_th_curr));
    PrintValue(mkString("theta_other_curr"));
    PrintValue(ScalarReal(theta_other_curr));
#endif

    double theta_prop = 0.0;
    double theta_other_prop = 0.0;
    double log_th_prop = 0.0;
    double log_th_other_prop = 0.0;
    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      log_th_prop = rnorm(log_th_curr, scale_theta);

      int prop_in_range = ((log_th_prop > lower + tolerance) &&
               (log_th_prop < upper - tolerance));

      if (prop_in_range) {

    theta_prop = exp(log_th_prop);

    if (is_update_pair) {

      theta_other_prop = (theta_curr - theta_prop)
        * weight / weight_other + theta_other_curr;

      int is_positive = (theta_other_prop > 0.0);

      if (is_positive) {
        log_th_other_prop = log(theta_other_prop);

        found_prop = ((log_th_other_prop > lower + tolerance)
              && (log_th_other_prop < upper - tolerance));
      }
    }
    else {
      found_prop = 1;
    }
      } /* end if inside limits */
    } /* end while loop */

#ifdef DEBUGGING
    PrintValue(mkString("outside while loop"));
    PrintValue(mkString("attempt"));
    PrintValue(ScalarInteger(attempt));
    PrintValue(mkString("found_prop"));
    PrintValue(ScalarInteger(found_prop));
    PrintValue(mkString("theta_prop"));
    PrintValue(ScalarReal(theta_prop));
    PrintValue(mkString("theta_other_prop"));
    PrintValue(ScalarReal(theta_other_prop));
    PrintValue(mkString("is_update_pair"));
    PrintValue(ScalarInteger(is_update_pair));
#endif

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */

      ++n_failed_prop_theta;

#ifdef DEBUGGING
      PrintValue(mkString("!found_prop"));
      PrintValue(ScalarInteger(n_failed_prop_theta));
#endif

      continue; /* go on to next theta */
    }

    int this_y = y[i];
    double this_mu = mu[i];

    int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

    double log_diff = 0;

#ifdef DEBUGGING
    PrintValue(mkString("y_is_missing"));
    PrintValue(ScalarInteger(y_is_missing));
#endif

    if (!y_is_missing) {

      log_diff = dpois(this_y, theta_prop, USE_LOG)
    - dpois(this_y, theta_curr, USE_LOG);
#ifdef DEBUGGING
      PrintValue(mkString("this_y"));
      PrintValue(ScalarInteger(this_y));
      PrintValue(mkString("theta_prop"));
      PrintValue(ScalarReal(theta_prop));
      PrintValue(mkString("theta_curr"));
      PrintValue(ScalarReal(theta_curr));
#endif
    }

#ifdef DEBUGGING
    PrintValue(mkString("log_diff so far"));
    PrintValue(ScalarReal(log_diff));
#endif

    if (is_update_pair) {

      int this_y_other = y[i_other];

      int y_other_is_missing = ( this_y_other == NA_INTEGER
                 || ISNA(this_y_other) );
#ifdef DEBUGGING
      PrintValue(mkString("updating pair"));
      PrintValue(mkString("this_y_other"));
      PrintValue(ScalarInteger(this_y_other));
      PrintValue(mkString("y_other_is_missing"));
      PrintValue(ScalarInteger(y_other_is_missing));
#endif

      if (!y_other_is_missing) {
    log_diff += dpois(this_y_other, theta_other_prop, USE_LOG)
      - dpois(this_y_other, theta_other_curr, USE_LOG);
#ifdef DEBUGGING
    PrintValue(mkString("this_y_other"));
    PrintValue(ScalarInteger(this_y_other));
    PrintValue(mkString("theta_other_prop"));
    PrintValue(ScalarReal(theta_other_prop));
    PrintValue(mkString("theta_other_curr"));
    PrintValue(ScalarReal(theta_other_curr));
#endif
      }

#ifdef DEBUGGING
      PrintValue(mkString("log_diff so far"));
      PrintValue(ScalarReal(log_diff));
#endif
      double mu_other = mu[i_other];

      double log_th_other_curr
    = log(theta_other_curr);

      double log_diff_prior =
    + dlnorm(theta_prop, this_mu, sigma, USE_LOG)
    + dlnorm(theta_other_prop, mu_other, sigma, USE_LOG)
    - dlnorm(theta_curr, this_mu, sigma, USE_LOG)
    - dlnorm(theta_other_curr, mu_other, sigma, USE_LOG);

      double log_diff_prop = safeLogProp_Poisson(log_th_curr,
                         log_th_other_curr,
                         log_th_prop,
                         log_th_other_prop,
                         scale_theta,
                         weight,
                         weight_other)
    - safeLogProp_Poisson(log_th_prop,
                  log_th_other_prop,
                  log_th_curr,
                  log_th_other_curr,
                  scale_theta,
                  weight,
                  weight_other);

      log_diff += log_diff_prior + log_diff_prop;

#ifdef DEBUGGING
      PrintValue(mkString("updating pairs"));
      PrintValue(mkString("this_mu"));
      PrintValue(ScalarReal(this_mu));
      PrintValue(mkString("mu_other"));
      PrintValue(ScalarReal(mu_other));

      PrintValue(mkString("log_th_prop"));
      PrintValue(ScalarReal(log_th_prop));
      PrintValue(mkString("log_th_other_prop"));
      PrintValue(ScalarReal(log_th_other_prop));
      PrintValue(mkString("log_th_curr"));
      PrintValue(ScalarReal(log_th_curr));
      PrintValue(mkString("log_th_other_curr"));
      PrintValue(ScalarReal(log_th_other_curr));

      PrintValue(mkString("log_diff_prior"));
      PrintValue(ScalarReal(log_diff_prior));
      PrintValue(mkString("log_diff_prop"));
      PrintValue(ScalarReal(log_diff_prop));
      PrintValue(mkString("log_diff"));
      PrintValue(ScalarReal(log_diff));
#endif
    }
    else {
      log_diff += dnorm(log_th_prop, this_mu, sigma, USE_LOG)
    - dnorm(log_th_curr, this_mu, sigma, USE_LOG);

#ifdef DEBUGGING
      PrintValue(mkString("not updating pairs"));
      PrintValue(mkString("log_th_prop"));
      PrintValue(ScalarReal(log_th_prop));
      PrintValue(mkString("log_th_curr"));
      PrintValue(ScalarReal(log_th_curr));
      PrintValue(mkString("this_mu"));
      PrintValue(ScalarReal(this_mu));
      PrintValue(mkString("log_diff"));
      PrintValue(ScalarReal(log_diff));
#endif
    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));

    if (accept) {
      ++n_accept_theta;
      theta[i] = theta_prop;
      thetaTransformed[i] = log_th_prop;

#ifdef DEBUGGING
      PrintValue(mkString("accept"));
      PrintValue(mkString("n_accept_theta"));
      PrintValue(ScalarInteger(n_accept_theta));
      PrintValue(mkString("theta[i]"));
      PrintValue(ScalarReal(theta[i]));
      PrintValue(mkString("theta[i_other"));
      PrintValue(ScalarReal(theta[i_other]));
#endif

      if (is_update_pair) {
    theta[i_other] = theta_other_prop;
    thetaTransformed[i_other] = log_th_other_prop;

#ifdef DEBUGGING
    PrintValue(mkString("theta[i_other"));
    PrintValue(ScalarReal(theta[i_other]));
#endif
      }
    }
    else {
#ifdef DEBUGGING
      PrintValue(mkString("not accepted"));

#endif

    }
  } /* end for each theta */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
}

/* y_R a Counts object,
 * y_R is integer,
 * exposure_R is g'teed to be doubles */
void
updateTheta_PoissonVaryingUseExpAgCertain(SEXP object, SEXP y_R, SEXP exposure_R)
{
  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R and exposure_R are all identical */

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double scale_theta = *REAL(GET_SLOT(object, scaleTheta_sym));
  double scale_theta_multiplier = *REAL(GET_SLOT(object, scaleThetaMultiplier_sym));
  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int *y = INTEGER(y_R);
  double *exposure = REAL(exposure_R);

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  scale_theta = scale_theta * scale_theta_multiplier;

  for (int i = 0; i < n_theta; ++i) {

    double scale_theta_i = scale_theta / sqrt(1 + log(1 + exposure[i]));

    int ir = i+1; /* R style index */

    int ir_other = makeIOther(ir, transformAg_R);
    int i_other = ir_other - 1;

    int is_in_delta = (ir_other >= 0);
    int is_has_other = 0;
    int is_weight_positive = 0;
    int is_weight_other_positive = 0;
    double weight = 0.0;
    double weight_other = 0.0;

    if (is_in_delta) {

      is_has_other = (ir_other > 0);

      weight = weightAg[i];
      is_weight_positive = (weight > 0);

      if (is_has_other) {
    weight_other = weightAg[i_other];
    is_weight_other_positive = (weight_other > 0);
      }
    }

    int value_fixed = (is_in_delta && is_weight_positive
               && !(is_has_other && is_weight_other_positive) );

    if (value_fixed) {
      /* skip to next value of theta */
      continue;
    }

    /* value is not fixed */
    int is_update_pair = (is_in_delta && is_weight_positive
              && is_has_other && is_weight_other_positive);

    double theta_curr = theta[i];
    double log_th_curr = thetaTransformed[i];

    double theta_other_curr = (is_update_pair ? theta[i_other] : 0.0);
    double theta_prop = 0.0;
    double theta_other_prop = 0.0;
    double log_th_prop = 0.0;
    double log_th_other_prop = 0.0;
    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      log_th_prop = rnorm(log_th_curr, scale_theta_i);  /* changed by John 21 May 2016 */

      int prop_in_range = ((log_th_prop > lower + tolerance) &&
               (log_th_prop < upper - tolerance));

      if (prop_in_range) {

    theta_prop = exp(log_th_prop);

    if (is_update_pair) {

      theta_other_prop = (theta_curr - theta_prop)
        * weight / weight_other + theta_other_curr;

      int is_positive = (theta_other_prop > 0.0);

      if (is_positive) {
        log_th_other_prop = log(theta_other_prop);

        found_prop = ((log_th_other_prop > lower + tolerance)
              && (log_th_other_prop < upper - tolerance));
      }
    }
    else {
      found_prop = 1;
    }
      } /* end if inside limits */
    } /* end while loop */

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */

      ++n_failed_prop_theta;

      continue; /* go on to next theta */
    }

    int this_y = y[i];
    double this_mu = mu[i];

    int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

    double log_diff = 0;

    if (!y_is_missing) {

      double this_exposure = exposure[i];

      log_diff = dpois(this_y, theta_prop * this_exposure, USE_LOG)
    - dpois(this_y, theta_curr * this_exposure, USE_LOG);
    }

    if (is_update_pair) {

      int this_y_other = y[i_other];

      int y_other_is_missing = ( this_y_other == NA_INTEGER
                 || ISNA(this_y_other) );
      if (!y_other_is_missing) {

    double other_exposure = exposure[i_other];

    log_diff += dpois(this_y_other,
              theta_other_prop * other_exposure, USE_LOG)
      - dpois(this_y_other,
          theta_other_curr * other_exposure, USE_LOG);
      }

      double mu_other = mu[i_other];

      double log_th_other_curr = thetaTransformed[i_other];

      double log_diff_prior =
    + dlnorm(theta_prop, this_mu, sigma, USE_LOG)
    + dlnorm(theta_other_prop, mu_other, sigma, USE_LOG)
    - dlnorm(theta_curr, this_mu, sigma, USE_LOG)
    - dlnorm(theta_other_curr, mu_other, sigma, USE_LOG);

      double log_diff_prop = safeLogProp_Poisson(log_th_curr,
                         log_th_other_curr,
                         log_th_prop,
                         log_th_other_prop,
                         scale_theta_i,
                         weight,
                         weight_other)
    - safeLogProp_Poisson(log_th_prop,
                  log_th_other_prop,
                  log_th_curr,
                  log_th_other_curr,
                  scale_theta_i,
                  weight,
                  weight_other);

      log_diff += log_diff_prior + log_diff_prop;

    }
    else {
      log_diff += dnorm(log_th_prop, this_mu, sigma, USE_LOG)
    - dnorm(log_th_curr, this_mu, sigma, USE_LOG);

    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));

    if (accept) {
      ++n_accept_theta;
      theta[i] = theta_prop;
      thetaTransformed[i] = log_th_prop;
      if (is_update_pair) {
    theta[i_other] = theta_other_prop;
    thetaTransformed[i_other] = log_th_other_prop;
      }
    }
    else {

    }
  } /* end for each theta */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
}

/* y_R is g'teed to be integer */
void
updateThetaAndValueAgNormal_PoissonNotUseExp(SEXP object, SEXP y_R)
{

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R are all identical */

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  SEXP valueAg_R = GET_SLOT(object, valueAg_sym);
  int nValueAg = LENGTH(valueAg_R);
  double *valueAg = REAL(valueAg_R);
  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));
  double scaleAg = *REAL(GET_SLOT(object, scaleAg_sym));

  int n_accept_ag = 0;
  int n_failed_prop_value_ag = 0;

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int *y = INTEGER(y_R);

  /* malloc one (overlarge) space for all the 4 vecs */
  double *allVecs = (double *)R_alloc(4*n_theta, sizeof(double));
  double *vec_th_curr = allVecs;
  double *vec_log_th_curr = allVecs + n_theta;
  double *vec_th_prop = allVecs + 2*n_theta;
  double *vec_log_th_prop = allVecs + 3*n_theta;

  for (int k = 0; k < nValueAg; ++k) {

    int kr = k+1; /* R style index */

    SEXP iAg_R = dembase_getIBefore(kr, transformAg_R);
    int nAg = LENGTH(iAg_R);
    int *iAg = INTEGER(iAg_R);

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;
      vec_th_curr[i] = theta[index];
      vec_log_th_curr[i] = thetaTransformed[index];
    }

    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      double increment = 0.0;

      for (int i = 0; i < nAg; ++i) {

    increment = rnorm(0, scaleAg);
    double log_th_prop = vec_log_th_curr[i] + increment;

    int inside_limits = ((log_th_prop > lower + tolerance)
                 && (log_th_prop < upper - tolerance));

    if (!inside_limits) { /* not in range */
      break; /* break out of the i-loop through benchmarked indices */
    }
    else {

      double theta_prop = exp(log_th_prop);
      int valid = 0;
      if (log_th_prop > 0) {
        valid = R_finite(theta_prop);
      }
      else {
        valid = (theta_prop > 0);
      }

      if (!valid) break; /* break out of the i-loop through benchmarked indices */
      else {
        vec_log_th_prop[i] = log_th_prop;
        vec_th_prop[i] = theta_prop;
        found_prop = (i == (nAg - 1));
      }
    }
      }
      /* found_prop is 0 if we had to break out of the loop */
    }

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */
      ++n_failed_prop_value_ag;

      continue; /* go on to next value benchmark */
    }

    double ag_prop = 0.0;
    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      ag_prop += vec_th_prop[i] * weightAg[index];
    }

    double ag_curr = valueAg[k];
    double mean_k = meanAg[k];
    double sd_k = sdAg[k];

    double log_diff_lik = 0.0;
    double log_diff_prior = 0.0;

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      int this_y = y[index];

      int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

      if (!y_is_missing) { /* does the is.observed bit */

    log_diff_lik += dpois(this_y, vec_th_prop[i], USE_LOG);
    log_diff_lik -= dpois(this_y, vec_th_curr[i], USE_LOG);
      }

      double this_mu = mu[index];

      log_diff_prior += dnorm(vec_log_th_prop[i], this_mu, sigma, USE_LOG);
      log_diff_prior -= dnorm(vec_log_th_curr[i], this_mu, sigma, USE_LOG);

    }

    double log_diff_ag = dnorm(mean_k, ag_prop, sd_k, USE_LOG)
      - dnorm(mean_k, ag_curr, sd_k, USE_LOG);

    double log_diff = log_diff_lik + log_diff_prior + log_diff_ag;

    if (!(log_diff < 0) || (runif(0, 1) < exp(log_diff))) {

      ++n_accept_ag;
      valueAg[k] = ag_prop;
      for (int i = 0; i < nAg; ++i) {
    int index = iAg[i] - 1;
    theta[index] = vec_th_prop[i];
    thetaTransformed[index] = vec_log_th_prop[i];
      }
    }
  } /* end for each value benchmark and set of thetas */

  SET_INTSCALE_SLOT(object, nAcceptAg_sym, n_accept_ag);
  SET_INTSCALE_SLOT(object, nFailedPropValueAg_sym, n_failed_prop_value_ag);
}



/* y_R is g'teed to be integer
 * exposure_R is g'teed to be doubles */
void
updateThetaAndValueAgNormal_PoissonUseExp(SEXP object, SEXP y_R, SEXP exposure_R)
{

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R and exposure_R are all identical */

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  SEXP valueAg_R = GET_SLOT(object, valueAg_sym);
  int nValueAg = LENGTH(valueAg_R);
  double *valueAg = REAL(valueAg_R);
  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));
  double scaleAg = *REAL(GET_SLOT(object, scaleAg_sym));

  int n_accept_ag = 0;
  int n_failed_prop_value_ag = 0;

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int *y = INTEGER(y_R);
  double *exposure = REAL(exposure_R);

  /* malloc one (overlarge) space for all the 4 vecs */
  double *allVecs = (double *)R_alloc(4*n_theta, sizeof(double));
  double *vec_th_curr = allVecs;
  double *vec_log_th_curr = allVecs + n_theta;
  double *vec_th_prop = allVecs + 2*n_theta;
  double *vec_log_th_prop = allVecs + 3*n_theta;

  for (int k = 0; k < nValueAg; ++k) {

    int kr = k+1; /* R style index */

    SEXP iAg_R = dembase_getIBefore(kr, transformAg_R);
    int nAg = LENGTH(iAg_R);
    int *iAg = INTEGER(iAg_R);

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;
      vec_th_curr[i] = theta[index];
      vec_log_th_curr[i] = thetaTransformed[index];
    }

    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      double increment = 0.0;

      for (int i = 0; i < nAg; ++i) {

    increment = rnorm(0, scaleAg);
    double log_th_prop = vec_log_th_curr[i] + increment;

    int inside_limits = ((log_th_prop > lower + tolerance)
                 && (log_th_prop < upper - tolerance));

    if (!inside_limits) { /* not in range */
      break; /* break out of the i-loop through agmarked indices */
    }
    else {

      double theta_prop = exp(log_th_prop);
      int valid = 0;
      if (log_th_prop > 0) {
        valid = R_finite(theta_prop);
      }
      else {
        valid = (theta_prop > 0);
      }

      if (!valid) break; /* break out of the i-loop through agmarked indices */
      else {
        vec_log_th_prop[i] = log_th_prop;
        vec_th_prop[i] = theta_prop;
        found_prop = (i == (nAg - 1));
      }
    }
      }
      /* found_prop is 0 if we had to break out of the loop */
    }

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */
      ++n_failed_prop_value_ag;

      continue; /* go on to next value agmark */
    }

    double ag_prop = 0.0;
    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      ag_prop += vec_th_prop[i] * weightAg[index];
    }

    double ag_curr = valueAg[k];
    double mean_k = meanAg[k];
    double sd_k = sdAg[k];

    double log_diff_lik = 0.0;
    double log_diff_prior = 0.0;

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      int this_y = y[index];

      int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

      if (!y_is_missing) { /* does the is.observed bit */
    double this_exp = exposure[index];

    log_diff_lik += dpois(this_y, vec_th_prop[i] * this_exp, USE_LOG);
    log_diff_lik -= dpois(this_y, vec_th_curr[i] * this_exp, USE_LOG);
      }

      double this_mu = mu[index];

      log_diff_prior += dnorm(vec_log_th_prop[i], this_mu, sigma, USE_LOG);
      log_diff_prior -= dnorm(vec_log_th_curr[i], this_mu, sigma, USE_LOG);

    }

    double log_diff_ag = dnorm(mean_k, ag_prop, sd_k, USE_LOG)
      - dnorm(mean_k, ag_curr, sd_k, USE_LOG);

    double log_diff = log_diff_lik + log_diff_prior + log_diff_ag;

    if (!(log_diff < 0) || (runif(0, 1) < exp(log_diff))) {

      ++n_accept_ag;
      valueAg[k] = ag_prop;
      for (int i = 0; i < nAg; ++i) {
    int index = iAg[i] - 1;
    theta[index] = vec_th_prop[i];
    thetaTransformed[index] = vec_log_th_prop[i];
      }
    }
  } /* end for each value agmark and set of thetas */

  SET_INTSCALE_SLOT(object, nAcceptAg_sym, n_accept_ag);
  SET_INTSCALE_SLOT(object, nFailedPropValueAg_sym, n_failed_prop_value_ag);
}



void
updateThetaAndValueAgPoisson_PoissonNotUseExp(SEXP object, SEXP y_R)
{
  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R are all identical */

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  SEXP valueAg_R = GET_SLOT(object, valueAg_sym);
  int nValueAg = LENGTH(valueAg_R);
  double *valueAg = REAL(valueAg_R);
  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double scaleAg = *REAL(GET_SLOT(object, scaleAg_sym));

  double *exposureAg = REAL(GET_SLOT(object, exposureAg_sym));

  int n_accept_ag = 0;
  int n_failed_prop_value_ag = 0;

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int *y = INTEGER(y_R);

  /* malloc one (overlarge) space for all the 4 vecs */
  double *allVecs = (double *)R_alloc(4*n_theta, sizeof(double));
  double *vec_th_curr = allVecs;
  double *vec_log_th_curr = allVecs + n_theta;
  double *vec_th_prop = allVecs + 2*n_theta;
  double *vec_log_th_prop = allVecs + 3*n_theta;

  for (int k = 0; k < nValueAg; ++k) {
    int kr = k+1; /* R style index */

    SEXP iAg_R = dembase_getIBefore(kr, transformAg_R);
    int nAg = LENGTH(iAg_R);
    int *iAg = INTEGER(iAg_R);

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;
      vec_th_curr[i] = theta[index];
      vec_log_th_curr[i] = thetaTransformed[index];
    }

    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      double increment = 0.0;

      for (int i = 0; i < nAg; ++i) {

    increment = rnorm(0, scaleAg);
    double log_th_prop = vec_log_th_curr[i] + increment;

    int inside_limits = ((log_th_prop > lower + tolerance)
                 && (log_th_prop < upper - tolerance));

    if (!inside_limits) { /* not in range */
      break; /* break out of the i-loop through benchmarked indices */
    }
    else {

      double theta_prop = exp(log_th_prop);
      int valid = 0;
      if (log_th_prop > 0) {
        valid = R_finite(theta_prop);
      }
      else {
        valid = (theta_prop > 0);
      }

      if (!valid) break; /* break out of the i-loop through benchmarked indices */
      else {
        vec_log_th_prop[i] = log_th_prop;
        vec_th_prop[i] = theta_prop;
        found_prop = (i == (nAg - 1));
      }
    }
      }
      /* found_prop is 0 if we had to break out of the loop */
    }

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */
      ++n_failed_prop_value_ag;

      continue; /* go on to next value benchmark */
    }

    double ag_prop = 0.0;
    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      ag_prop += vec_th_prop[i] * weightAg[index];
    }

    double ag_curr = valueAg[k];
    double mean_k = meanAg[k];
    double exposure_k = exposureAg[k];

    double log_diff_lik = 0.0;
    double log_diff_prior = 0.0;

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      int this_y = y[index];

      int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

      if (!y_is_missing) { /* does the is.observed bit */

    log_diff_lik += dpois(this_y, vec_th_prop[i], USE_LOG);
    log_diff_lik -= dpois(this_y, vec_th_curr[i], USE_LOG);
      }

      double this_mu = mu[index];

      log_diff_prior += dnorm(vec_log_th_prop[i], this_mu, sigma, USE_LOG);
      log_diff_prior -= dnorm(vec_log_th_curr[i], this_mu, sigma, USE_LOG);

    }

    double log_diff_ag = (exposure_k*(ag_curr - ag_prop)
              + mean_k * exposure_k *(log(ag_prop) - log(ag_curr)));

    double log_diff = log_diff_lik + log_diff_prior + log_diff_ag;

    if (!(log_diff < 0) || (runif(0, 1) < exp(log_diff))) {

      ++n_accept_ag;
      valueAg[k] = ag_prop;
      for (int i = 0; i < nAg; ++i) {
    int index = iAg[i] - 1;
    theta[index] = vec_th_prop[i];
    thetaTransformed[index] = vec_log_th_prop[i];
      }
    }
  } /* end for each value benchmark and set of thetas */

  SET_INTSCALE_SLOT(object, nAcceptAg_sym, n_accept_ag);
  SET_INTSCALE_SLOT(object, nFailedPropValueAg_sym, n_failed_prop_value_ag);
}




void
updateThetaAndValueAgPoisson_PoissonUseExp(SEXP object, SEXP y_R, SEXP exposure_R)
{

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R and exposure_R are all identical */

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  SEXP valueAg_R = GET_SLOT(object, valueAg_sym);
  int nValueAg = LENGTH(valueAg_R);
  double *valueAg = REAL(valueAg_R);
  double *weightAg = REAL(GET_SLOT(object, weightAg_sym));
  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double scaleAg = *REAL(GET_SLOT(object, scaleAg_sym));

  double *exposureAg = REAL(GET_SLOT(object, exposureAg_sym));

  int n_accept_ag = 0;
  int n_failed_prop_value_ag = 0;

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int *y = INTEGER(y_R);
  double *exposure = REAL(exposure_R);

  /* malloc one (overlarge) space for all the 4 vecs */
  double *allVecs = (double *)R_alloc(4*n_theta, sizeof(double));
  double *vec_th_curr = allVecs;
  double *vec_log_th_curr = allVecs + n_theta;
  double *vec_th_prop = allVecs + 2*n_theta;
  double *vec_log_th_prop = allVecs + 3*n_theta;

  for (int k = 0; k < nValueAg; ++k) {

    int kr = k+1; /* R style index */

    SEXP iAg_R = dembase_getIBefore(kr, transformAg_R);
    int nAg = LENGTH(iAg_R);
    int *iAg = INTEGER(iAg_R);

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;
      vec_th_curr[i] = theta[index];
      vec_log_th_curr[i] = thetaTransformed[index];
    }

    int attempt = 0;
    int found_prop = 0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      double increment = 0.0;

      for (int i = 0; i < nAg; ++i) {

    increment = rnorm(0, scaleAg);
    double log_th_prop = vec_log_th_curr[i] + increment;

    int inside_limits = ((log_th_prop > lower + tolerance)
                 && (log_th_prop < upper - tolerance));

    if (!inside_limits) { /* not in range */
      break; /* break out of the i-loop through agmarked indices */
    }
    else {

      double theta_prop = exp(log_th_prop);
      int valid = 0;
      if (log_th_prop > 0) {
        valid = R_finite(theta_prop);
      }
      else {
        valid = (theta_prop > 0);
      }

      if (!valid) break; /* break out of the i-loop through agmarked indices */
      else {
        vec_log_th_prop[i] = log_th_prop;
        vec_th_prop[i] = theta_prop;
        found_prop = (i == (nAg - 1));
      }
    }
      }
      /* found_prop is 0 if we had to break out of the loop */
    }

    if (!found_prop) {  /* reached 'maxAttempt' without generating proposal */
      ++n_failed_prop_value_ag;

      continue; /* go on to next value agmark */
    }

    double ag_prop = 0.0;
    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      ag_prop += vec_th_prop[i] * weightAg[index];
    }

    double ag_curr = valueAg[k];
    double mean_k = meanAg[k];
    double exposure_k = exposureAg[k];

    double log_diff_lik = 0.0;
    double log_diff_prior = 0.0;

    for (int i = 0; i < nAg; ++i) {
      int index = iAg[i] - 1;

      int this_y = y[index];

      int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

      if (!y_is_missing) { /* does the is.observed bit */
    double this_exp = exposure[index];

    log_diff_lik += dpois(this_y, vec_th_prop[i] * this_exp, USE_LOG);
    log_diff_lik -= dpois(this_y, vec_th_curr[i] * this_exp, USE_LOG);
      }

      double this_mu = mu[index];

      log_diff_prior += dnorm(vec_log_th_prop[i], this_mu, sigma, USE_LOG);
      log_diff_prior -= dnorm(vec_log_th_curr[i], this_mu, sigma, USE_LOG);

    }

    double log_diff_ag = (exposure_k*(ag_curr - ag_prop)
              + mean_k * exposure_k *(log(ag_prop) - log(ag_curr)));

    double log_diff = log_diff_lik + log_diff_prior + log_diff_ag;

    if (!(log_diff < 0) || (runif(0, 1) < exp(log_diff))) {

      ++n_accept_ag;
      valueAg[k] = ag_prop;
      for (int i = 0; i < nAg; ++i) {
    int index = iAg[i] - 1;
    theta[index] = vec_th_prop[i];
    thetaTransformed[index] = vec_log_th_prop[i];
      }
    }
  } /* end for each value agmark and set of thetas */

  SET_INTSCALE_SLOT(object, nAcceptAg_sym, n_accept_ag);
  SET_INTSCALE_SLOT(object, nFailedPropValueAg_sym, n_failed_prop_value_ag);
}




void
updateThetaAndValueAgFun_PoissonNotUseExp(SEXP object, SEXP y_R)
{
  int *y = INTEGER(y_R);

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R are all identical */

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  double sigma = *REAL(GET_SLOT(object, sigma_sym));

  double *valueAg = REAL(GET_SLOT(object, valueAg_sym));

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));

  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  SEXP funAg_R = GET_SLOT(object, funAg_sym);

  /* set up to be able to call the R function from C */
  SEXP call_R = NULL;
  /* call_R will be the final called object */
  PROTECT(call_R = allocList(3));
  SET_TYPEOF(call_R, LANGSXP);
  SETCAR(call_R, funAg_R); /* sets first value in list to this function*/

  SEXP xArgsAg_R = GET_SLOT(object, xArgsAg_sym);
  SEXP weightsArgsAg_R = GET_SLOT(object, weightsArgsAg_sym);
  double *tmp_x = NULL;
  int length_x_args_list = LENGTH(xArgsAg_R);
  int n_xs = 0;
  if (length_x_args_list > 0) {
    SEXP first_R = VECTOR_ELT(xArgsAg_R, 0);
    n_xs = length(first_R);
    tmp_x = (double *)R_alloc(n_xs, sizeof(double));
  }

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    int ir = i+1; /* R style index */

    int i_ag_r = dembase_getIAfter(ir, transformAg_R);
    int i_ag = i_ag_r - 1;

    int contributes_to_ag = (i_ag_r > 0);

    int this_y = y[i];
    int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

    int draw_straight_from_prior = (y_is_missing && !contributes_to_ag);

    double theta_curr = theta[i];
    double log_th_curr = thetaTransformed[i];

    double mean = mu[i];
    double sd = sigma;

    if (!y_is_missing) {

      mean = log_th_curr;
      sd = scale;
    }

    int attempt = 0;
    int found_prop = 0;

    double log_th_prop = 0.0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      log_th_prop = rnorm(mean, sd);
      found_prop = ( (log_th_prop > lower + tolerance) &&
             (log_th_prop < upper - tolerance));

    }

    if (found_prop) {

      double theta_prop = exp(log_th_prop);

      if (draw_straight_from_prior) {
    theta[i] = theta_prop;
    thetaTransformed[i] = log_th_prop;
      }
      else {

    SEXP x_R = NULL;
    SEXP weight_R = NULL;
    double *x = NULL;

    if (contributes_to_ag) {

      x_R = VECTOR_ELT(xArgsAg_R, i_ag);
      weight_R = VECTOR_ELT(weightsArgsAg_R, i_ag);
      x = REAL(x_R);
      /* store these xs in case we need to restore them*/
      memcpy(tmp_x, x, n_xs*sizeof(double));
    }

    double log_diff = 0;

    if (!y_is_missing) {

      double log_lik_prop = 0;
      double log_lik_curr = 0;

      log_lik_prop = dpois(this_y, theta_prop, USE_LOG);
      log_lik_curr = dpois(this_y, theta_curr, USE_LOG);
      log_diff = log_lik_prop - log_lik_curr;
    }

    double log_dens_prop = dnorm(log_th_prop, mu[i], sigma, USE_LOG);
    double log_dens_curr = dnorm(log_th_curr, mu[i], sigma, USE_LOG);
    log_diff += (log_dens_prop - log_dens_curr);

    double ag_prop = 0;

    if (contributes_to_ag) {

      double ag_curr = valueAg[i_ag];
      double mean_ag = meanAg[i_ag];
      double sd_ag = sdAg[i_ag];

      SEXP ir_shared_R;
      PROTECT( ir_shared_R
           = dembase_getIShared(ir, transformAg_R) );
      int n_ir_shared = LENGTH(ir_shared_R);
      int *ir_shared = INTEGER(ir_shared_R);

      for (int j = 0; j < n_ir_shared; ++j) {
        if ( i == (ir_shared[j] - 1) ) {
          x[j] = theta_prop;
          /* alters this x in the original R SEXP object */
        }
      }

      /* set 2nd and 3rd values in the function call object */
      SETCADR(call_R, x_R);
      SETCADDR(call_R, weight_R);

      /* call the supplied function */
      SEXP prop_R = PROTECT(eval(call_R, R_GlobalEnv));
      ag_prop = *REAL(prop_R);

      UNPROTECT(2); /* ir_shared_r, current prop_R */

      double log_dens_ag_prop = dnorm(mean_ag, ag_prop, sd_ag, USE_LOG);
      double log_dens_ag_curr = dnorm(mean_ag, ag_curr, sd_ag, USE_LOG);
      log_diff += log_dens_ag_prop - log_dens_ag_curr;
    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));
    if (accept) {
      ++n_accept_theta;
      theta[i] = theta_prop;
      thetaTransformed[i] = log_th_prop;
      if (contributes_to_ag) {
        /* x will have been updated in place already*/
        valueAg[i_ag] = ag_prop;
      }
    }
    else if (contributes_to_ag) {
      /* unmodify the x_ags */
      memcpy(x, tmp_x, n_xs*sizeof(double));
    }
      }
    }
    else { /* not found prop */
      ++n_failed_prop_theta;
    }

  } /* end i-loop through thetas */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
  UNPROTECT(1); /* call_R */
}



void
updateThetaAndValueAgFun_PoissonUseExp(SEXP object, SEXP y_R, SEXP exposure_R)
{
  int *y = INTEGER(y_R);
  double *exposure = REAL(exposure_R);

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R are all identical */

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  double sigma = *REAL(GET_SLOT(object, sigma_sym));
  double scale_theta_multiplier
    = *REAL(GET_SLOT(object, scaleThetaMultiplier_sym));
  scale *= scale_theta_multiplier;

  double *valueAg = REAL(GET_SLOT(object, valueAg_sym));

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));

  SEXP transformAg_R = GET_SLOT(object, transformAg_sym);

  SEXP funAg_R = GET_SLOT(object, funAg_sym);

  /* set up to be able to call the R function from C */
  SEXP call_R = NULL;
  /* call_R will be the final called object */
  PROTECT(call_R = allocList(3));
  SET_TYPEOF(call_R, LANGSXP);
  SETCAR(call_R, funAg_R); /* sets first value in list to this function*/

  SEXP xArgsAg_R = GET_SLOT(object, xArgsAg_sym);
  SEXP weightsArgsAg_R = GET_SLOT(object, weightsArgsAg_sym);
  double *tmp_x = NULL;
  int length_x_args_list = LENGTH(xArgsAg_R);
  int n_xs = 0;
  if (length_x_args_list > 0) {
    SEXP first_R = VECTOR_ELT(xArgsAg_R, 0);
    n_xs = length(first_R);
    tmp_x = (double *)R_alloc(n_xs, sizeof(double));
  }

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  for (int i = 0; i < n_theta; ++i) {

    int ir = i+1; /* R style index */

    int i_ag_r = dembase_getIAfter(ir, transformAg_R);
    int i_ag = i_ag_r - 1;

    int contributes_to_ag = (i_ag_r > 0);

    int this_y = y[i];
    int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

    int draw_straight_from_prior = (y_is_missing && !contributes_to_ag);

    double theta_curr = theta[i];
    double log_th_curr = thetaTransformed[i];

    double mean = mu[i];
    double sd = sigma;

    double this_exp = exposure[i];

    if (!y_is_missing) {

      mean = log_th_curr;
      sd = scale / sqrt(1 + this_y);
    }

    int attempt = 0;
    int found_prop = 0;

    double log_th_prop = 0.0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      log_th_prop = rnorm(mean, sd);
      found_prop = ( (log_th_prop > lower + tolerance) &&
             (log_th_prop < upper - tolerance));

    }

    if (found_prop) {

      double theta_prop = exp(log_th_prop);

      if (draw_straight_from_prior) {
    theta[i] = theta_prop;
    thetaTransformed[i] = log_th_prop;
      }
      else {

    SEXP x_R = NULL;
    SEXP weight_R = NULL;
    double *x = NULL;

    if (contributes_to_ag) {

      x_R = VECTOR_ELT(xArgsAg_R, i_ag);
      weight_R = VECTOR_ELT(weightsArgsAg_R, i_ag);
      x = REAL(x_R);
      /* store these xs in case we need to restore them*/
      memcpy(tmp_x, x, n_xs*sizeof(double));
    }

    double log_diff = 0;

    if (!y_is_missing) {

      double log_lik_prop = 0;
      double log_lik_curr = 0;

      log_lik_prop = dpois(this_y, theta_prop*this_exp, USE_LOG);
      log_lik_curr = dpois(this_y, theta_curr*this_exp, USE_LOG);
      log_diff = log_lik_prop - log_lik_curr;
    }

    double log_dens_prop = dnorm(log_th_prop, mu[i], sigma, USE_LOG);
    double log_dens_curr = dnorm(log_th_curr, mu[i], sigma, USE_LOG);
    log_diff += (log_dens_prop - log_dens_curr);

    double ag_prop = 0;

    if (contributes_to_ag) {

      double ag_curr = valueAg[i_ag];
      double mean_ag = meanAg[i_ag];
      double sd_ag = sdAg[i_ag];

      SEXP ir_shared_R;
      PROTECT( ir_shared_R
           = dembase_getIShared(ir, transformAg_R) );
      int n_ir_shared = LENGTH(ir_shared_R);
      int *ir_shared = INTEGER(ir_shared_R);

      for (int j = 0; j < n_ir_shared; ++j) {
        if ( i == (ir_shared[j] - 1) ) {
          x[j] = theta_prop;
          /* alters this x in the original R SEXP object */
        }
      }

      /* set 2nd and 3rd values in the function call object */
      SETCADR(call_R, x_R);
      SETCADDR(call_R, weight_R);

      /* call the supplied function */
      SEXP prop_R = PROTECT(eval(call_R, R_GlobalEnv));
      ag_prop = *REAL(prop_R);

      UNPROTECT(2); /* ir_shared_r, current prop_R */

      double log_dens_ag_prop = dnorm(mean_ag, ag_prop, sd_ag, USE_LOG);
      double log_dens_ag_curr = dnorm(mean_ag, ag_curr, sd_ag, USE_LOG);
      log_diff += log_dens_ag_prop - log_dens_ag_curr;
    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));
    if (accept) {
      ++n_accept_theta;
      theta[i] = theta_prop;
      thetaTransformed[i] = log_th_prop;
      if (contributes_to_ag) {
        /* x will have been updated in place already*/
        valueAg[i_ag] = ag_prop;
      }
    }
    else if (contributes_to_ag) {
      /* unmodify the x_ags */
      memcpy(x, tmp_x, n_xs*sizeof(double));
    }
      }
    }
    else { /* not found prop */
      ++n_failed_prop_theta;
    }

  } /* end i-loop through thetas */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
  UNPROTECT(1); /* call_R */
}

void
updateThetaAndValueAgLife_PoissonUseExp(SEXP object, SEXP y_R, SEXP exposure_R)
{

  int *y = INTEGER(y_R);
  double *exposure = REAL(exposure_R);

  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *thetaTransformed = REAL(GET_SLOT(object, thetaTransformed_sym));
  /* n_theta and length of y_R are all identical */

  double *mu = REAL(GET_SLOT(object, mu_sym));

  double lower = *REAL(GET_SLOT(object, lower_sym));
  double upper = *REAL(GET_SLOT(object, upper_sym));
  double tolerance = *REAL(GET_SLOT(object, tolerance_sym));

  double scale = *REAL(GET_SLOT(object, scaleTheta_sym));
  double sigma = *REAL(GET_SLOT(object, sigma_sym));
  double scale_theta_multiplier
    = *REAL(GET_SLOT(object, scaleThetaMultiplier_sym));
  scale *= scale_theta_multiplier;

  double *mx = REAL(GET_SLOT(object, mxAg_sym));

  double *ax = REAL(GET_SLOT(object, axAg_sym));
  double *nx = REAL(GET_SLOT(object, nxAg_sym));

  int nAge = *INTEGER(GET_SLOT(object, nAgeAg_sym));

  double *valueAg = REAL(GET_SLOT(object, valueAg_sym));

  double *meanAg = REAL(GET_SLOT(object, meanAg_sym));
  double *sdAg = REAL(GET_SLOT(object, sdAg_sym));

  SEXP transformAg_R = GET_SLOT(object, transformThetaToMxAg_sym);

  int maxAttempt = *INTEGER(GET_SLOT(object, maxAttempt_sym));

  int n_accept_theta = 0;
  int n_failed_prop_theta = 0;

  SEXP exposureMx_tmp_R;
  SEXP exposureMx_R;
  /* collapse_R in demographic is okay with y_R being integer
   * but type of contents of yCollapsed_R will be integer*/
  PROTECT(exposureMx_tmp_R = dembase_Collapse_R(exposure_R, transformAg_R));
  PROTECT(exposureMx_R = coerceVector(exposureMx_tmp_R, REALSXP));
  double *exposureMx = REAL(exposureMx_R);

  for (int i = 0; i < n_theta; ++i) {

    int ir = i+1; /* R style index */

    int i_mx_r = dembase_getIAfter(ir, transformAg_R);
    int i_mx = i_mx_r - 1;

    int contributes_to_ag = (i_mx_r > 0);

    int this_y = y[i];
    int y_is_missing = ( this_y == NA_INTEGER || ISNA(this_y) );

    int draw_straight_from_prior = (y_is_missing && !contributes_to_ag);

    double theta_curr = theta[i];
    double log_th_curr = thetaTransformed[i];

    double mean = mu[i];
    double sd = sigma;

    double this_exp = exposure[i]; /* exposure, not exposureMx */

    if (!y_is_missing) {

      mean = log_th_curr;
      sd = scale / sqrt(1 + this_y);
    }

    int attempt = 0;
    int found_prop = 0;

    double log_th_prop = 0.0;

    while( (!found_prop) && (attempt < maxAttempt) ) {

      ++attempt;

      log_th_prop = rnorm(mean, sd);
      found_prop = ( (log_th_prop > lower + tolerance) &&
             (log_th_prop < upper - tolerance));

    }

    if (found_prop) {

      double theta_prop = exp(log_th_prop);

      if (draw_straight_from_prior) {
    theta[i] = theta_prop;
    thetaTransformed[i] = log_th_prop;
      }
      else {

    double log_diff = 0;

    if (!y_is_missing) {

      double log_lik_prop = 0;
      double log_lik_curr = 0;

      log_lik_prop = dpois(this_y, theta_prop*this_exp, USE_LOG);
      log_lik_curr = dpois(this_y, theta_curr*this_exp, USE_LOG);
      log_diff = log_lik_prop - log_lik_curr;
    }

    double log_dens_prop = dnorm(log_th_prop, mu[i], sigma, USE_LOG);
    double log_dens_curr = dnorm(log_th_curr, mu[i], sigma, USE_LOG);
    log_diff += (log_dens_prop - log_dens_curr);

    double ag_prop = 0;

    double increment_mx = 0;

    if (contributes_to_ag) {

      increment_mx = (theta_prop - theta_curr)* this_exp/exposureMx[i_mx];
      mx[i_mx] += increment_mx;

      int iAge0_r = (i_mx/nAge) * nAge + 1; /* i_mx = i_mx_r - 1 */

      ag_prop = makeLifeExpBirth(mx, nx, ax, iAge0_r, nAge);

      int i_ag = i_mx / nAge; /* 1 less than the r style index */

      double ag_curr = valueAg[i_ag];
      double mean_ag = meanAg[i_ag];
      double sd_ag = sdAg[i_ag];

      double log_dens_ag_prop = dnorm(mean_ag, ag_prop, sd_ag, USE_LOG);
      double log_dens_ag_curr = dnorm(mean_ag, ag_curr, sd_ag, USE_LOG);
      log_diff += log_dens_ag_prop - log_dens_ag_curr;
    }

    int accept = (!(log_diff < 0) || (runif(0, 1) < exp(log_diff)));
    if (accept) {
      ++n_accept_theta;
      theta[i] = theta_prop;
      thetaTransformed[i] = log_th_prop;
      if (contributes_to_ag) {
            int i_ag = i_mx / nAge; /* 1 less than the r style index */
        valueAg[i_ag] = ag_prop;
      }
    }
    else if (contributes_to_ag) {
      /* unmodify the mx */
      mx[i_mx] -= increment_mx;
    }
      }
    }
    else { /* not found prop */
      ++n_failed_prop_theta;
    }

  } /* end i-loop through thetas */

  SET_INTSCALE_SLOT(object, nAcceptTheta_sym, n_accept_theta);
  SET_INTSCALE_SLOT(object, nFailedPropTheta_sym, n_failed_prop_theta);
  UNPROTECT(2); /* exposureMx_R and tmp of same */
}


void
updateVariancesBetas(SEXP object_R)
{
  SEXP variances_R = GET_SLOT(object_R, variancesBetas_sym);
  SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
  int n_beta =  LENGTH(variances_R);

  for (int i = 0; i < n_beta; ++i) {
    SEXP variance_R = VECTOR_ELT(variances_R, i);
    double *variance = REAL(variance_R);
    int J = LENGTH(variance_R);
    SEXP prior_R = VECTOR_ELT(priors_R, i);
    getV_Internal(variance, prior_R, J);
  }
}


/* y_R is a demographic array, g'teed to be doubles */
void
updateVarsigma(SEXP object, SEXP y_R)
{
  SEXP varsigma_R = GET_SLOT(object, varsigma_sym);
  double varsigma = *REAL(GET_SLOT(varsigma_R, Data_sym));
  double varsigmaMax = *REAL(GET_SLOT(object, varsigmaMax_sym));
  double A = *REAL(GET_SLOT(object, AVarsigma_sym));
  double nu = *REAL(GET_SLOT(object, nuVarsigma_sym));
  SEXP theta_R = GET_SLOT(object, theta_sym);
  double *theta = REAL(theta_R);
  int n_theta = LENGTH(theta_R);
  double *w = REAL(GET_SLOT(object, w_sym));
  double *y = REAL(y_R);
  /* n_theta and length of y_R and w are all identical */

  double V = 0.0;
  int n_obs = 0;
  for (int i = 0; i < n_theta; ++i) {
    double this_y = y[i];
    int y_is_missing = (this_y == NA_REAL || ISNA(y[i]));
    if ( !y_is_missing ) {
      ++n_obs;
      double y_minus_theta = this_y - theta[i];
      V += w[i] * y_minus_theta * y_minus_theta;
    }
  }
  varsigma = updateSDNorm(varsigma, A, nu, V, n_obs, varsigmaMax);
  int successfullyUpdated = (varsigma > 0);
  if (successfullyUpdated) {
    SET_DOUBLESCALE_SLOT(object, varsigma_sym, varsigma);
  }
}


void
updateVarsigmaLN2(SEXP object_R, SEXP y_R, SEXP exposure_R)
{

  int update = *INTEGER(GET_SLOT(object_R, updateVarsigmaLN2_sym));
  if (update) {
    int hasHalfT = *INTEGER(GET_SLOT(object_R, varsigmaLN2HasHalfT_sym));
    double varsigma = *REAL(GET_SLOT(object_R, varsigma_sym));
    double varsigma_max = *REAL(GET_SLOT(object_R, varsigmaMax_sym));
    double A = *REAL(GET_SLOT(object_R, AVarsigma_sym));
    int add1 = *INTEGER(GET_SLOT(object_R, add1_sym));
    double nu = *REAL(GET_SLOT(object_R, nuVarsigma_sym));
    double * alpha = REAL(GET_SLOT(object_R, alphaLN2_sym));
    int * cell_in_lik = INTEGER(GET_SLOT(object_R, cellInLik_sym));
    SEXP transform_R = GET_SLOT(object_R, transformLN2_sym);
    double V = 0.0;
    int n = 0;
    int n_y = LENGTH(y_R);
    int * y = INTEGER(y_R);
    int * exposure = INTEGER(exposure_R);
    double tmp;
    for (int i = 0; i < n_y; ++i) {
      if (cell_in_lik[i]) {
	int j_r = dembase_getIAfter(i+1, transform_R);
	double alpha_j = alpha[j_r - 1];
	if (add1)
	  tmp = log1p(y[i]) - log1p(exposure[i]) - alpha_j;
	else
	  tmp = log(y[i]) - log(exposure[i]) - alpha_j;
	V += tmp * tmp;
	n++;
      }
    }
    if (n > 0) {
      int successfully_updated = 0;
      if (hasHalfT) {
	varsigma = updateSDNorm(varsigma, A, nu, V, n, varsigma_max);
	successfully_updated = (varsigma > 0);
      }
      else {			/* inv-chi-sq prior */
	int maxAttempt = *INTEGER(GET_SLOT(object_R, maxAttempt_sym));
	int  df = nu + n;
        double scaleSq = (nu * A + V) / (nu + n);
	double varsigma_sq;
	for (int i = 0; i < maxAttempt; i++) {
	  varsigma_sq = rinvchisq1(df, scaleSq);
	  varsigma = sqrt(varsigma_sq);
	  if (varsigma < varsigma_max) {
	    successfully_updated = 1;
	    break;
	  }
	}
      }
      if (successfully_updated) {
	SET_DOUBLESCALE_SLOT(object_R, varsigma_sym, varsigma);
      }
    }
    else {
      SET_DOUBLESCALE_SLOT(object_R, varsigma_sym, 0.0);
    }
  }
}




/* *************************** updating counts *************************** */


void
updateCountsPoissonNotUseExp(SEXP y_R, SEXP model_R, SEXP dataModels_R,
                 SEXP datasets_R, SEXP transforms_R)
{

#ifdef DEBUGGING
  PrintValue(y_R);
  PrintValue(model_R);
  PrintValue(dataModels_R);
  PrintValue(datasets_R);
  PrintValue(transforms_R);
  PrintValue(GET_SLOT(model_R, theta_sym));
#endif

  double *theta = REAL(GET_SLOT(model_R, theta_sym));
  int n_y = LENGTH(y_R);
  int *y = INTEGER(y_R);
  int *strucZeroArray = INTEGER(GET_SLOT(model_R, strucZeroArray_sym));

  int has_subtotals = 0;

  SEXP transformSubtotals_R = NULL;
  if (R_has_slot(y_R, transformSubtotals_sym)) {
    has_subtotals = 1;
    transformSubtotals_R = GET_SLOT(y_R, transformSubtotals_sym);
  }


#ifdef DEBUGGING
  PrintValue(ScalarInteger(100));
  PrintValue(ScalarInteger(has_subtotals));
  if(has_subtotals) {
    PrintValue(ScalarInteger(110));
    PrintValue(transformSubtotals_R);
  }
#endif

  for (int ir = 1; ir <= n_y; ++ir) {

        if (strucZeroArray[ir - 1] != 0) {

#ifdef DEBUGGING
      PrintValue(ScalarInteger(200));
      PrintValue(ScalarInteger(ir));
#endif

      int nInd = 2; /* number of indices (proposals) to deal with */
      int yProp[nInd]; /* make space > 1 (may only need one) */
      int indices[nInd]; /* make space > 1 (may only need one) */
      /* put ir into first pos in indices
       * if nInd=2 second pos will be filled later */
      indices[0] = ir;

      if (has_subtotals) {
    int ir_other = makeIOther(ir, transformSubtotals_R);

#ifdef DEBUGGING
    PrintValue(ScalarInteger(300));
    PrintValue(ScalarInteger(ir_other));
#endif

    if (ir_other > 0) { /* found other cell with same subtotal */

      getTwoMultinomialProposalsNoExp(yProp,
                      y, theta, ir, ir_other);

      indices[1] = ir_other; /* only need if nInd = 2 */

    }

    else if (ir_other < 0) { /* cell not included in any subtotal */
      nInd = 1;

#ifdef DEBUGGING
      PrintValue(ScalarInteger(700));
      PrintValue(ScalarReal(theta[ir-1]));
#endif

      /* the R code for R 3.0.0 onwards seems to just use
       * a cast to an int, so that's what I have done. */
      yProp[0] = (int) rpois(theta[ir-1]);

#ifdef DEBUGGING
      PrintValue(ScalarInteger(750));
      PrintValue(ScalarReal(yProp[0]));
#endif
    }
    else { /* ir_other == 0, subtotal refers to single cell */
#ifdef DEBUGGING
      PrintValue(ScalarInteger(600));
#endif
      continue; /* next ir in for loop */
    }
      }
      else { /* no subtotals */
    nInd = 1;
    /* the R code for R 3.0.0 onwards seems to just use
     * a cast to an int, so that's what I have done. */
    yProp[0] = (int) rpois(theta[ir-1]);
#ifdef DEBUGGING
    PrintValue(ScalarInteger(800));
    PrintValue(ScalarReal(yProp[0]));
#endif
      }

      double diffLL = diffLogLik(yProp, y_R, indices, nInd,
                 dataModels_R, datasets_R, transforms_R);

#ifdef DEBUGGING
      PrintValue(ScalarInteger(900));
      PrintValue(ScalarReal(diffLL));
#endif

#ifndef DEBUGGING
      if ( !( diffLL < 0.0) || ( runif(0.0, 1.0) < exp(diffLL) ) ) {
    /* accept proposals */
    for (int i = 0; i < nInd; ++i) {
      y[ indices[i] - 1] = yProp[i];
    }
      }
#endif

#ifdef DEBUGGING
      int accept = 0;
      if (!(diffLL < 0.0)) {
    PrintValue(ScalarInteger(950));
    accept = 1;
      }
      else {
    double ru = runif(0.0,1.0);
    double edll = exp(diffLL);
    PrintValue(ScalarInteger(960));
    PrintValue(ScalarReal(ru));
    PrintValue(ScalarInteger(970));
    PrintValue(ScalarReal(edll));
    int tmp = (ru < edll);
    PrintValue(ScalarInteger(980));
    PrintValue(ScalarInteger(tmp));
    if (tmp) {
      accept = 1;
    }
      }
      PrintValue(ScalarInteger(1000));
      PrintValue(ScalarInteger(accept));

      if ( accept ) {
    /* accept proposals */
    for (int i = 0; i < nInd; ++i) {
      y[ indices[i] - 1] = yProp[i];
    }
      }
#endif

#ifdef DEBUGGING
      PrintValue(ScalarInteger(10000));
      PrintValue(y_R);
#endif
    }
  }

}


/* ONLY tested without subtotals*/
void
updateCountsPoissonUseExp(SEXP y_R, SEXP model_R,
              SEXP exposure_R, SEXP dataModels_R,
              SEXP datasets_R, SEXP transforms_R)
{
  double *theta = REAL(GET_SLOT(model_R, theta_sym));
  double *exposure = REAL(exposure_R);
  int n_y = LENGTH(y_R);
  int *y = INTEGER(y_R);
  int *strucZeroArray = INTEGER(GET_SLOT(model_R, strucZeroArray_sym));

  int has_subtotals = 0;
  SEXP transformSubtotals_R = NULL;
  if (R_has_slot(y_R, transformSubtotals_sym)) {
    has_subtotals = 1;
    transformSubtotals_R = GET_SLOT(y_R, transformSubtotals_sym);
  }

  for (int ir = 1; ir <= n_y; ++ir) {

    if (strucZeroArray[ir - 1] != 0) {

      int nInd = 2; /* number of indices (proposals) to deal with */
      int yProp[nInd]; /* make space > 1 (may only need one)  */
      int indices[nInd]; /* make space > 1 (may only need one) */
      /* put ir into first pos in indices
       * if nInd=2 second pos will be filled later */
      indices[0] = ir;

      if (has_subtotals) {
    int ir_other = makeIOther(ir, transformSubtotals_R);

    if (ir_other > 0) { /* found other cell with same subtotal */

      getTwoMultinomialProposalsWithExp(yProp,
                        y, theta, exposure, ir, ir_other);

      indices[1] = ir_other; /* only need if nInd = 2 */

    }

    else if (ir_other < 0) { /* cell not included in any subtotal */
      nInd = 1;
      /* the R code for R 3.0.0 onwards seems to just use
       * a cast to an int, so that's what I have done. */
      yProp[0] = (int) rpois(theta[ir-1]*exposure[ir-1]);
    }
    else { /* ir_other == 0, subtotal refers to single cell */
      continue; /* next ir in for loop */
    }
      }
      else { /* no subtotals */

    nInd = 1;
    /* the R code for R 3.0.0 onwards seems to just use
     * a cast to an int, so that's what I have done. */
    yProp[0] = (int) rpois(theta[ir-1]*exposure[ir-1]);

      }

      double diffLL = diffLogLik(yProp, y_R, indices, nInd,
                 dataModels_R, datasets_R, transforms_R);


      if (!( diffLL < 0.0) || ( runif(0.0, 1.0) < exp(diffLL) )) {
    /* accept proposals */

    for (int i = 0; i < nInd; ++i) {
      y[ indices[i] - 1] = yProp[i];
    }
      }

    }

  }

}

void
updateCountsBinomial(SEXP y_R, SEXP model_R,
             SEXP exposure_R, SEXP dataModels_R,
             SEXP datasets_R, SEXP transforms_R)
{

  double *theta = REAL(GET_SLOT(model_R, theta_sym));
  int *exposure = INTEGER(exposure_R);
  int nY = LENGTH(y_R);
  int *y = INTEGER(y_R);

  for(int i = 0; i < nY; ++i) {

    int ir = i+1; /* R style index */

    int yProp = rbinom(exposure[i], theta[i]);
    /* cast to int */

    double diffLL = diffLogLik(&yProp, y_R,
                   &ir, 1,
                   dataModels_R, datasets_R, transforms_R);


    int accept =  ( !( diffLL < 0.0)
            || ( runif(0.0, 1.0) < exp(diffLL) ) );
    if (accept) {
      /* accept proposals */
      y[i] = yProp;
    }
  }
}



void
updateCountsAndThetaBinomial(SEXP object_R)
{

  SEXP y_R = GET_SLOT(object_R, y_sym);
  SEXP model_R = GET_SLOT(object_R, model_sym);
  SEXP dataModels_R = GET_SLOT(object_R, dataModels_sym);
  SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
  SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
  double *theta = REAL(GET_SLOT(model_R, theta_sym));
  double *thetaTransformed = REAL(GET_SLOT(model_R, thetaTransformed_sym));
  double *mu = REAL(GET_SLOT(model_R, mu_sym));
  double sigma = *REAL(GET_SLOT(model_R, sigma_sym));
  double scale = *REAL(GET_SLOT(model_R, scaleTheta_sym));
  int *exposure = INTEGER(GET_SLOT(object_R, exposure_sym));
  int *strucZeroArray = INTEGER(GET_SLOT(model_R, strucZeroArray_sym));
  int *cellInLik = INTEGER(GET_SLOT(model_R, cellInLik_sym));
  double lower = *REAL(GET_SLOT(model_R, lower_sym));
  double upper = *REAL(GET_SLOT(model_R, upper_sym));
  double tolerance = *REAL(GET_SLOT(model_R, tolerance_sym));
  int maxAttempt = *INTEGER(GET_SLOT(model_R, maxAttempt_sym));
  int nY = LENGTH(y_R);
  int *y = INTEGER(y_R);
  int nFailedPropTheta = 0;
  int nAcceptTheta = 0;
  double logitThProp;
  double thetaProp;
  for (int i = 0; i < nY; ++i) {
    int isStrucZero = strucZeroArray[i] == 0;
    if (!isStrucZero) {
      int ir = i+1; /* R style index */
      int inLik = cellInLik[i];
      int foundProp = 0;
      int attempt = 0;
      double sdJump = sigma;
      if (inLik)
	sdJump *= scale;
      while (!foundProp && (attempt < maxAttempt)) {
	attempt++;
	logitThProp = rnorm(mu[i], sdJump);
	foundProp = ((logitThProp > lower + tolerance)
		     && (logitThProp < upper + tolerance));
      }
      if (foundProp) {
	if (logitThProp > 0)
	  thetaProp = 1 / (1 + exp(-logitThProp));
	else
	  thetaProp = exp(logitThProp) / (1 + exp(logitThProp));
	int yProp = rbinom(exposure[i], thetaProp);
	int accept = 1;
	if (inLik) {
	  double logitThCurr = thetaTransformed[i];
	  double diffLL = diffLogLik(&yProp, y_R,
				     &ir, 1, /* 1 is value for 'n_element_indices_y' */
				     dataModels_R, datasets_R, transforms_R);
	  double diffLogDens = (dnorm(logitThProp, mu[i], sigma, USE_LOG)
				- dnorm(logitThCurr, mu[i], sigma, USE_LOG));
	  double diffLogJump = (dnorm(logitThCurr, mu[i], sdJump, USE_LOG)
				- dnorm(logitThProp, mu[i], sdJump, USE_LOG));
	  double logR = diffLL + diffLogDens + diffLogJump;
	  accept =  ( !( logR < 0.0) || ( runif(0.0, 1.0) < exp(logR) ) );
	  if (accept)
	    ++nAcceptTheta;
	}
	if (accept) {
	  y[i] = yProp;
	  theta[i] = thetaProp;
	  thetaTransformed[i] = logitThProp;
	}
      }
      else {
	++nFailedPropTheta;
      }
    }
  }
  SET_INTSCALE_SLOT(model_R, nAcceptTheta_sym, nAcceptTheta);
  SET_INTSCALE_SLOT(model_R, nFailedPropTheta_sym, nFailedPropTheta);
}


void
updateCountsAndThetaPoissonNotUseExp(SEXP object_R)
{
  SEXP y_R = GET_SLOT(object_R, y_sym);
  SEXP model_R = GET_SLOT(object_R, model_sym);
  SEXP dataModels_R = GET_SLOT(object_R, dataModels_sym);
  SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
  SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
  double *theta = REAL(GET_SLOT(model_R, theta_sym));
  double *thetaTransformed = REAL(GET_SLOT(model_R, thetaTransformed_sym));
  double boxCoxParam = *REAL(GET_SLOT(model_R, boxCoxParam_sym));
  int usesBoxCoxTransformation = (boxCoxParam > 0);
  double *mu = REAL(GET_SLOT(model_R, mu_sym));
  double sigma = *REAL(GET_SLOT(model_R, sigma_sym));
  double scale = *REAL(GET_SLOT(model_R, scaleTheta_sym));
  int *strucZeroArray = INTEGER(GET_SLOT(model_R, strucZeroArray_sym));
  int *cellInLik = INTEGER(GET_SLOT(model_R, cellInLik_sym));
  double lower = *REAL(GET_SLOT(model_R, lower_sym));
  double upper = *REAL(GET_SLOT(model_R, upper_sym));
  double tolerance = *REAL(GET_SLOT(model_R, tolerance_sym));
  int maxAttempt = *INTEGER(GET_SLOT(model_R, maxAttempt_sym));
  int nY = LENGTH(y_R);
  int *y = INTEGER(y_R);
  int nFailedPropTheta = 0;
  int nAcceptTheta = 0;
  double trThProp;
  double thProp;
  for (int i = 0; i < nY; ++i) {
    int isStrucZero = strucZeroArray[i] == 0;
    if (!isStrucZero) {
      int ir = i+1; /* R style index */
      int inLik = cellInLik[i];
      int foundProp = 0;
      int attempt = 0;
      double sdJump = sigma;
      if (inLik)
	sdJump *= scale;
      while (!foundProp && (attempt < maxAttempt)) {
	attempt++;
	trThProp = rnorm(mu[i], sdJump);
	foundProp = ((trThProp > lower + tolerance)
		     && (trThProp < upper + tolerance));
      }
      if (foundProp) {
        if (usesBoxCoxTransformation) {
	  thProp = pow(boxCoxParam * trThProp + 1, 1/boxCoxParam);
        }
        else {
	  thProp = exp(trThProp);
        }
	int yProp = rpois(thProp);
	int accept = 1;
	if (inLik) {
	  double trThCurr = thetaTransformed[i];
	  double diffLL = diffLogLik(&yProp, y_R,
				     &ir, 1, /* 1 is value for 'n_element_indices_y' */
				     dataModels_R, datasets_R, transforms_R);
	  double diffLogDens = (dnorm(trThProp, mu[i], sigma, USE_LOG)
				- dnorm(trThCurr, mu[i], sigma, USE_LOG));
	  double diffLogJump = (dnorm(trThCurr, mu[i], sdJump, USE_LOG)
				- dnorm(trThProp, mu[i], sdJump, USE_LOG));
	  double logR = diffLL + diffLogDens + diffLogJump;
	  accept =  ( !( logR < 0.0) || ( runif(0.0, 1.0) < exp(logR) ) );
	  if (accept)
	    ++nAcceptTheta;
	}
	if (accept) {
	  y[i] = yProp;
	  theta[i] = thProp;
	  thetaTransformed[i] = trThProp;
	}
      }
      else {
	++nFailedPropTheta;
      }
    }
  }
  SET_INTSCALE_SLOT(model_R, nAcceptTheta_sym, nAcceptTheta);
  SET_INTSCALE_SLOT(model_R, nFailedPropTheta_sym, nFailedPropTheta);
}


void
updateCountsAndThetaPoissonUseExp(SEXP object_R)
{
  SEXP y_R = GET_SLOT(object_R, y_sym);
  double *exposure = REAL(GET_SLOT(object_R, exposure_sym));
  SEXP model_R = GET_SLOT(object_R, model_sym);
  SEXP dataModels_R = GET_SLOT(object_R, dataModels_sym);
  SEXP datasets_R = GET_SLOT(object_R, datasets_sym);
  SEXP transforms_R = GET_SLOT(object_R, transforms_sym);
  double *theta = REAL(GET_SLOT(model_R, theta_sym));
  double *thetaTransformed = REAL(GET_SLOT(model_R, thetaTransformed_sym));
  double boxCoxParam = *REAL(GET_SLOT(model_R, boxCoxParam_sym));
  int usesBoxCoxTransformation = (boxCoxParam > 0);
  double *mu = REAL(GET_SLOT(model_R, mu_sym));
  double sigma = *REAL(GET_SLOT(model_R, sigma_sym));
  double scale = *REAL(GET_SLOT(model_R, scaleTheta_sym));
  int *strucZeroArray = INTEGER(GET_SLOT(model_R, strucZeroArray_sym));
  int *cellInLik = INTEGER(GET_SLOT(model_R, cellInLik_sym));
  double lower = *REAL(GET_SLOT(model_R, lower_sym));
  double upper = *REAL(GET_SLOT(model_R, upper_sym));
  double tolerance = *REAL(GET_SLOT(model_R, tolerance_sym));
  int maxAttempt = *INTEGER(GET_SLOT(model_R, maxAttempt_sym));
  int nY = LENGTH(y_R);
  int *y = INTEGER(y_R);
  int nFailedPropTheta = 0;
  int nAcceptTheta = 0;
  double trThProp;
  double thProp;
  for (int i = 0; i < nY; ++i) {
    int isStrucZero = strucZeroArray[i] == 0;
    if (!isStrucZero) {
      int ir = i+1; /* R style index */
      int inLik = cellInLik[i];
      int foundProp = 0;
      int attempt = 0;
      double sdJump = sigma;
      if (inLik)
	sdJump *= scale;
      while (!foundProp && (attempt < maxAttempt)) {
	attempt++;
	trThProp = rnorm(mu[i], sdJump);
	foundProp = ((trThProp > lower + tolerance)
		     && (trThProp < upper + tolerance));
      }
      if (foundProp) {
        if (usesBoxCoxTransformation) {
	  thProp = pow(boxCoxParam * trThProp + 1, 1/boxCoxParam);
        }
        else {
	  thProp = exp(trThProp);
        }
	int yProp = rpois(thProp * exposure[i]);
	int accept = 1;
	if (inLik) {
	  double trThCurr = thetaTransformed[i];
	  double diffLL = diffLogLik(&yProp, y_R,
				     &ir, 1, /* 1 is value for 'n_element_indices_y' */
				     dataModels_R, datasets_R, transforms_R);
	  double diffLogDens = (dnorm(trThProp, mu[i], sigma, USE_LOG)
				- dnorm(trThCurr, mu[i], sigma, USE_LOG));
	  double diffLogJump = (dnorm(trThCurr, mu[i], sdJump, USE_LOG)
				- dnorm(trThProp, mu[i], sdJump, USE_LOG));
	  double logR = diffLL + diffLogDens + diffLogJump;
	  accept =  ( !( logR < 0.0) || ( runif(0.0, 1.0) < exp(logR) ) );
	  if (accept)
	    ++nAcceptTheta;
	}
	if (accept) {
	  y[i] = yProp;
	  theta[i] = thProp;
	  thetaTransformed[i] = trThProp;
	}
      }
      else {
	++nFailedPropTheta;
      }
    }
  }
  SET_INTSCALE_SLOT(model_R, nAcceptTheta_sym, nAcceptTheta);
  SET_INTSCALE_SLOT(model_R, nFailedPropTheta_sym, nFailedPropTheta);
}



void
updateDataModelsCounts(SEXP y_R, SEXP dataModels_R,
               SEXP datasets_R, SEXP transforms_R)
{
  int nObs = LENGTH(dataModels_R);

  for (int i = 0; i < nObs; ++i) {

    SEXP model_R = VECTOR_ELT(dataModels_R, i);
    SEXP dataset_R = VECTOR_ELT(datasets_R, i);
    SEXP transform_R = VECTOR_ELT(transforms_R, i);

    SEXP yCollapsed_R;

    int nProtect  = 0;
    int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));

    const char *class_name = CHAR(STRING_ELT(GET_SLOT((model_R), R_ClassSymbol), 0));
    int found = !((strstr(class_name, "Poisson") == NULL) && (strstr(class_name, "CMP") == NULL));
    if (found) {

      SEXP yCollapsed_tmp_R;
      /* collapse_R in demographic is okay with y_R being integer
       * but type of contents of yCollapsed_R will be integer*/
      PROTECT(yCollapsed_tmp_R = dembase_Collapse_R(y_R, transform_R));

      PROTECT(yCollapsed_R = coerceVector(yCollapsed_tmp_R, REALSXP));
      nProtect  = 2;
    }
    else {
      PROTECT(yCollapsed_R = dembase_Collapse_R(y_R, transform_R));
      nProtect  = 1;
    }

    /* yCollapsed_R should now be in appropriate state for model */
    updateModelUseExp_Internal(model_R, dataset_R,
                   yCollapsed_R, i_method_model);

    UNPROTECT(nProtect); /* yCollapsed_R and possibly also y_Collapsed_tmp_R*/

  }
}


void
updateDataModelsAccount(SEXP combined_R)
{

  SEXP dataModels_R = GET_SLOT(combined_R, dataModels_sym);
  SEXP datasets_R = GET_SLOT(combined_R, datasets_sym);
  SEXP seriesIndices_R = GET_SLOT(combined_R, seriesIndices_sym);
  SEXP transforms_R = GET_SLOT(combined_R, transforms_sym);

  int *updateDataModel = LOGICAL(GET_SLOT(combined_R, updateDataModel_sym));

  SEXP account_R = GET_SLOT(combined_R, account_sym);
  SEXP population_R = GET_SLOT(account_R, population_sym);
  SEXP components_R = GET_SLOT(account_R, components_sym);

  int* seriesIndices = INTEGER(seriesIndices_R);

  int nObs = LENGTH(dataModels_R);

  for (int i = 0; i < nObs; ++i) {

        if (updateDataModel[i]) {

            SEXP model_R = VECTOR_ELT(dataModels_R, i);
            SEXP dataset_R = VECTOR_ELT(datasets_R, i);
            SEXP transform_R = VECTOR_ELT(transforms_R, i);

            int seriesIndex_r = seriesIndices[i];
            SEXP series_R = population_R;
            if (seriesIndex_r > 0) {
                series_R = VECTOR_ELT(components_R, seriesIndex_r-1);
            }

            SEXP seriesCollapsed_R;

            int nProtect  = 0;
            int i_method_model = *(INTEGER(GET_SLOT(model_R, iMethodModel_sym)));

            const char *class_name = CHAR(STRING_ELT(GET_SLOT((model_R), R_ClassSymbol), 0));
            int found = !((strstr(class_name, "Poisson") == NULL) && (strstr(class_name, "CMP") == NULL));
            if (found) {

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
            updateModelUseExp_Internal(model_R, dataset_R,
                     seriesCollapsed_R, i_method_model);

            UNPROTECT(nProtect); /* seriesCollapsed_R and possibly also series_Collapsed_tmp_R*/

        }

    }
}
