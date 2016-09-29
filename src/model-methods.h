
#ifndef __MODEL_METHODS_H__
#define __MODEL_METHODS_H__


    #include <Rinternals.h>

    void predictModelNotUseExp_Internal(SEXP object, 
                                        SEXP y_R, 
                                        int i_method_model);
    
    void predictModelUseExp_Internal(SEXP object,
                                    SEXP y_R,
                                    SEXP exposure_R,
                                    int i_method_model);
                                    
    void updateModelNotUseExp_Internal(SEXP object, 
                                        SEXP y_R, 
                                        int i_method_model);
    
    void updateModelUseExp_Internal(SEXP object,
                                    SEXP y_R,
                                    SEXP exposure_R,
                                    int i_method_model);

    
#endif
