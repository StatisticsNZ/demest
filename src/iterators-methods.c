
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
        
    ++iAge;
    i += stepAge;

    *iAge_ptr = iAge;
    *iTime_ptr = iTime;
    int * i_ptr = INTEGER(GET_SLOT(iterator_R, i_sym));
    *i_ptr = i;
    
    SEXP finished_R = GET_SLOT(iterator_R, finished_sym);
    LOGICAL(finished_R)[0] = ((iTime >= nTime) || (iAge >= nAge));

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
        
        if (iTriangle == 1) {
            ++iTime;
	    i += stepTime;
            ++iTriangle;
            i += stepTriangle;
	    finished = !lastAgeGroupOpen && (iAge == nAge);
        }
        
        else {
	    --iTriangle;
	    i -= stepTriangle;
            if (iAge < nAge) {
                ++iAge;
		i += stepAge;
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
    LOGICAL(finished_R)[0] = ((iTime_R >= nTime) || (iAge_R >= nAge));
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
            finished = !lastAgeGroupOpen && (iAge_R == nAge);
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
