
## AllClasses-12-Model-mixin-specific.R

                   gradientBetas = "list",
                   momentumBetas = "list",
                   betasOld = "list",
                   useHMCToUpdateBeta = "logical",
                   sizeStep = "Scale",
                   nStep = "Length",
                   acceptBeta = "integer",
                   useHMCBetas = "LogicalFlag",

useHMCToUpdateBeta <- object@useHMCToUpdateBeta
acceptBeta <- object@acceptBeta

## 'useHMCToUpdateBeta' has no missing values
             if (any(is.na(useHMCToUpdateBeta)))
                 return(gettextf("'%s' has missing values",
                                 "useHMCToUpdateBeta"))

## 'acceptBeta' has length 1
if (!identical(length(acceptBeta), 1L))
    stop(gettext("'%s' does not have length %d",
                 "acceptBeta", 1L))
## 'acceptBeta' is 0 or 1
if (!(acceptBeta %in% 0:1))
    stop(gettextf("'%s' is not %d or %d",
                  "acceptBeta", 0L, 1L))             

             ## 'sizeStep' positive
             if (!(sizeStep > 0))
                 return(gettextf("'%s' is non-positive",
                                 "sizeStep"))
             ## gradientBetas, momentumBetas, meansBetas, variancesBetas, betasOld:
             for (name in c("gradientBetas", "momentumBetas",
                            "meansBetas", "variancesBetas",
                            "betasOld")) {
                 value <- methods::slot(object, name)
                 ## 'betas' and slot have same length
                 if (!identical(length(betas), length(value)))
                     return(gettextf("'%s' and '%s' have different lengths",
                                     "betas", name))
                 ## corresponding elements of slot and 'betas' have same length
                 for (i in seq_along(value))
                     if (!identical(length(value[[i]]), length(betas[[i]])))
                         return(gettextf("element %d of '%s' and element %d of '%s' have different lengths",
                                         i, name, i, "betas"))
                 ## slot has missing values iff beta has missing values
                 for (i in seq_along(value))
                     if (!identical(is.na(value[[i]]), is.na(betas[[i]])))
                         return(gettextf("element %d of '%s' and element %d of '%s' have different patterns of missingness",
                                         i, name, i, "betas"))
             }

             ## 'betas' and 'useHMCToUpdateBeta' have same length
             if (!identical(length(betas), length(useHMCToUpdateBeta)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "betas", "useHMCToUpdateBeta"))


## NO_TESTS
setClass("HMCBetaMixin",
         slots = c(useHMC = "LogicalFlag",
                   sizeStep = "Scale",
                   nStep = "Length"),
         contains = "VIRTUAL")





## AllClasses-14-Model-hierarchy-spec.R

setClass("SpecVarying",
         contains = c("VIRTUAL",
                      "SpecModel",
                      "ScaleThetaMixin",
                      "SpecASigmaMixin",
                      "SpecAggregateMixin",
                      "FormulaMuMixin",
                      "HMCBetaMixin",
                      "LowerUpperMixin",
                      "MaxAttemptMixin",
                      "SpecSigmaMaxMixin",
                      "SpecsPriorsMixin",
                      "NuSigmaMixin",
                      "SpecSeriesMixin",
                      "SpecAggregate"))




## AllClasses-15-Model-hierarchy.R

useHMCBetas = methods::new("LogicalFlag", TRUE),



## update-nongeneric


## TRANSLATED
## HAS_TESTS (comparing R and C versions)
updateBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateBetas_R, object)
    }
    else {
        object <- updateBetasGibbs(object)
        object <- updateBetasHMC(object) # updates 'logPostBeta', even when no betas updated with HMC
        object
    }
}
