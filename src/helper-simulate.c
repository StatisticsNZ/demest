
//#include "iterators-methods.h"
#include "helper-functions.h"
#include "demest.h"

//#include "R_ext/BLAS.h" 
/* for BLAS level 2 documention see www.netlib.org/blas/blas2-paper.ps */ 

//#include "R_ext/Lapack.h" 

/* File "helper-simulate.c" contains C versions of 
 * functions from "helper-simulate.R".   */


void
drawBetas(SEXP object_R)
{
    SEXP betas_R = GET_SLOT(object_R, betas_sym);
    int n_betas = LENGTH(betas_R);
    
    SEXP priors_R = GET_SLOT(object_R, priorsBetas_sym);
    
    //int max_J = 0; /* max J so far */
    //double *work = NULL;
    
    for (int i = 0; i < n_betas; ++i) {
		
		#ifdef DEBUGGING
		PrintValue(mkString(""));
		PrintValue(mkString("i"));
		PrintValue(ScalarInteger(i));
		#endif
           
		SEXP beta_R = VECTOR_ELT(betas_R, i);
		SEXP prior_R = VECTOR_ELT(priors_R, i);
		double *beta = REAL(beta_R);
		
		int J = *INTEGER(GET_SLOT(prior_R, J_sym));
		
		/* only malloc more space if we need it */
		//if(J > max_J) {
			//max_J = J;
			double *work = (double*)R_alloc(2*J, sizeof(double));
		//}
					  
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
		#ifdef DEBUGGING
		PrintValue(mkString("J"));
        PrintValue(ScalarReal(J));
		PrintValue(mkString("beta_hat"));
		printDblArray(beta_hat, J);
		PrintValue(mkString("var"));
		printDblArray(var, J);
		PrintValue(mkString("beta"));
		printDblArray(beta, J);
		#endif
    }
}
