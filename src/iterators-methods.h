
#ifndef __ITERATORS_METHODS_H__
#define __ITERATORS_METHODS_H__


    #include <Rinternals.h>

    /* advance random walk iterator one 'step' 
     * and if advanced from final position, return to initial position*/
    void advanceA(SEXP iterator_R);

    /* reset random walk iterator */
    void resetA(SEXP iterator_R);
    
    /* advance beta iterator */
    void advanceB(SEXP iterator_R);

    /* reset beta iterator */
    void resetB(SEXP iterator_R);
    
    /* advance dims iterator one step */
    void advanceD(SEXP iterator_R);

    /* reset dims iterator */
    void resetD(SEXP iterator_R);

    void advanceCP(SEXP iterator_R);

    void advanceCA(SEXP iterator_R);

    void resetCA(SEXP iterator_R, int i);

    void resetCP(SEXP iterator_R, int i);

    

#endif
