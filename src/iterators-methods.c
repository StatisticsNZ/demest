
#include "demest.h"

/* File "iterators-methods.c" contains C versions of functions 
 * from "iterators-methods.R". */


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
    
    /* if (object@iWithin < object@nWithin) {
            object@iWithin <- object@iWithin + 1L
            object@indices <- object@indices + 1L
        } */
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
        
        /*  object@iWithin <- 1L */
        *iWithin = 1;
        /* if (object@iBetween < object@nBetween) {
                object@iBetween <- object@iBetween + 1L
                object@indices <- object@indices + object@incrementBetween
         } */
        if (*iBetween < nBetween) {
            *iBetween += 1;
            int incrementBetween = 
                *(INTEGER(GET_SLOT(iterator_R, incrementBetween_sym)));
            for (int i = 0; i < indices_len; ++i) {
                indices[i] += incrementBetween;
            }
        }
        /* else {
                object@iBetween <- 1L
                object@indices <- object@initial
            } */
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
        
        /* for (d in seq_along(dim.iterators))
                dim.iterators[[d]] <- advanceD(dim.iterators[[d]]) */
        for (int d = 0; d < nDimIterators; ++d) {
            SEXP dIterator_R = VECTOR_ELT(dim_iterators_R, d);
            advanceD(dIterator_R);
            /* and get the nStride at the same time */
            dimIteratorStrides[d] 
                    = *(INTEGER(GET_SLOT(dIterator_R, nStrides_sym)));
        }
        
        /*for (b in seq.int(from = 2L, to = n.beta)) {
                for (d in seq_along(dim.iterators)) {
                    n.strides <- dim.iterators[[d]]@nStrides
                    stride.length <- stride.lengths[[b - 1L]][d]
                    indices[[b]] <- indices[[b]] + as.integer(n.strides * stride.length)
                }
            } */
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
    
    /* if (object@iWithin < object@nWithin) {
            object@iWithin <- object@iWithin + 1L
            object@nStrides <- 0L
        } */
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
advanceCAP(SEXP iterator_R)
{
    int i = *INTEGER(GET_SLOT(iterator_R, i_sym));
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int iTime = *INTEGER(GET_SLOT(iterator_R, iTime_sym));
    int hasAge = *INTEGER(GET_SLOT(iterator_R, hasAge_sym));
   
    ++iTime;
    i += stepTime;
    
        
    if (hasAge) {
        int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
        int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
        int iAge = *INTEGER(GET_SLOT(iterator_R, iAge_sym));
        
        if (iAge < nAge) {
            ++iAge;
            i += stepAge;
        }
        SET_SLOT(iterator_R, iAge_sym, ScalarInteger(iAge));
    }
    
    SET_SLOT(iterator_R, i_sym, ScalarInteger(i));
    SET_SLOT(iterator_R, iTime_sym, ScalarInteger(iTime));
    
    int finished = (iTime >= nTime);
    SET_SLOT(iterator_R, finished_sym, ScalarLogical(finished));
}

void
advanceCC(SEXP iterator_R)
{
	int i = *INTEGER(GET_SLOT(iterator_R, i_sym));
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int iTime = *INTEGER(GET_SLOT(iterator_R, iTime_sym));
    int hasAge = *INTEGER(GET_SLOT(iterator_R, hasAge_sym));
   
    int finished = 0;
            
    if (hasAge) {
        int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
        int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
        int iAge = *INTEGER(GET_SLOT(iterator_R, iAge_sym));
        int stepTriangle = *INTEGER(GET_SLOT(iterator_R, stepTriangle_sym));
        int iTriangle = *INTEGER(GET_SLOT(iterator_R, iTriangle_sym));
        
        if (iTriangle == 1) {
            ++iTime;
            iTriangle = 2;
            i += stepTime + stepTriangle;
        }
        
        else {
			if (iAge < nAge) {
				++iAge;
				iTriangle = 1;
				i += stepAge - stepTriangle;
			}
			else {
				++iTime;
				i += stepTime;
			}
			finished = ((iTriangle == 1) && (iTime == nTime));
		}
		
		SET_SLOT(iterator_R, iAge_sym, ScalarInteger(iAge));
		SET_SLOT(iterator_R, iTriangle_sym, ScalarInteger(iTriangle));
    
    }
    else {
		++iTime;
		i += stepTime;
		finished = (iTime == nTime);
	}	
    
    SET_SLOT(iterator_R, i_sym, ScalarInteger(i));
    SET_SLOT(iterator_R, iTime_sym, ScalarInteger(iTime));
    
    SET_SLOT(iterator_R, finished_sym, ScalarLogical(finished));
}


/* reset cohort iterator */
void
resetCAP(SEXP iterator_R, int i)
{
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int hasAge = *INTEGER(GET_SLOT(iterator_R, hasAge_sym));
    
    int iTime_R = (((i - 1) / stepTime) % nTime) + 1; /* R-style */
    
    if (hasAge) {
    
        int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
        int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
        int iAge_R = (((i - 1) / stepAge) % nAge) + 1; /* R-style */
        SET_SLOT(iterator_R, iAge_sym, ScalarInteger(iAge_R));
    }
    
    SET_SLOT(iterator_R, i_sym, ScalarInteger(i));
    SET_SLOT(iterator_R, iTime_sym, ScalarInteger(iTime_R));
    
    int finished = (iTime_R >= nTime);
    SET_SLOT(iterator_R, finished_sym, ScalarLogical(finished));
}

void
resetCC(SEXP iterator_R, int i)
{
    int stepTime = *INTEGER(GET_SLOT(iterator_R, stepTime_sym));
    int nTime = *INTEGER(GET_SLOT(iterator_R, nTime_sym));
    int hasAge = *INTEGER(GET_SLOT(iterator_R, hasAge_sym));
    
    int iTime_R = ((i - 1)/stepTime) % nTime  + 1;
    
    SET_SLOT(iterator_R, i_sym, ScalarInteger(i));
    SET_SLOT(iterator_R, iTime_sym, ScalarInteger(iTime_R));
        
    if (hasAge) {
    
        int stepAge = *INTEGER(GET_SLOT(iterator_R, stepAge_sym));
        int stepTriangle = *INTEGER(GET_SLOT(iterator_R, stepTriangle_sym));
        int nAge = *INTEGER(GET_SLOT(iterator_R, nAge_sym));
        int iAge_R = (((i - 1) / stepAge) % nAge) + 1; /* R-style */
        int iTriangle_R = (((i - 1) / stepTriangle) % 2) + 1; /* R-style */
        
        SET_SLOT(iterator_R, iAge_sym, ScalarInteger(iAge_R));
        SET_SLOT(iterator_R, iTriangle_sym, ScalarInteger(iTriangle_R));
    }
    
    int finished = (iTime_R >= nTime);
    SET_SLOT(iterator_R, finished_sym, ScalarLogical(finished));
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
