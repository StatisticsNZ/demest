
#ifndef __MAPPING_FUNCIONS_H__
#define __MAPPING_FUNCIONS_H__

    #include <Rinternals.h>

    void getIPopnNextFromOrigDestInternal(int *ans, int i, SEXP mapping_R);
    void getIAccNextFromOrigDestInternal(int *ans, int i, SEXP mapping_R);
    int getIExposureFromCompNotOneToOne(int i, SEXP mapping_R);
    int getIExposureFromOrigDestNotOneToOne(int i, SEXP mapping_R);
    void getIExpFirstFromOrigDestInternal(int *ans, int i, SEXP mapping_R);
    int getICellCompFromExpNotOneToOne(int i, SEXP mapping_R);
    
#endif
