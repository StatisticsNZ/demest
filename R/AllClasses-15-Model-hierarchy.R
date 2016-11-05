

## Virtual classes ###########################################################

setClass("NotUseExposure",
         contains = "VIRTUAL")

setClass("UseExposure",
         contains = "VIRTUAL")


setClass("Model",
         slots = c(call = "call"),
         contains = c("VIRTUAL",
             "MetadataY",
             "SlotsToExtract",
             "IMethodModel"))

## HAS_TESTS
setClass("Binomial",
         contains = c("VIRTUAL",
             "UseExposure",
             "ScaleThetaMultiplierMixin"),
         validity = function(object) {
             theta <- object@theta
             ## 'theta' is non-negative
             if (any(theta < 0))
                 return(gettextf("'%s' has negative values", "theta"))
             ## 'theta' is less than or equal to 1
             if (any(theta > 1))
                 return(gettextf("'%s' has values greater than %d",
                                 "theta", 1L))
             TRUE
         })

## HAS_TESTS
setClass("Normal",
         contains = c("VIRTUAL",
             "NotUseExposure"))

## HAS_TESTS
setClass("Poisson",
         contains = c("VIRTUAL",
             "ScaleThetaMultiplierMixin"),
         validity = function(object) {
             theta <- object@theta
             ## 'theta' is non-negative
             if (any(theta < 0))
                 return(gettextf("'%s' has negative values", "theta"))
             TRUE
         })


## Basic Models ##################################################################

## HAS_TESTS
## HAS_UPDATE
setClass("BinomialVarying",
         contains = c("Model",
             "Binomial",
             "Varying"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta",
                                   "nAcceptTheta",
                                   "betas",
                                   "sigma",
                                   "priorsBetas"),
             iMethodModel = 9L,
             nuSigma = methods::new("DegreesFreedom", 7)),
         validity = function(object) {
             theta <- object@theta
             lower <- object@lower
             upper <- object@upper
             tolerance <- object@tolerance
             ## 'theta' greater than or equal to invlogit(lower)
             if (any(theta < invlogit1(lower) - tolerance))
                 return(gettextf("'%s' has values that are less than '%s'",
                                 "theta", "lower"))
             ## 'theta' less than or equal to invlogit(upper)
             if (any(theta > invlogit1(upper) + tolerance))
                 return(gettextf("'%s' has values that are greater than '%s'",
                                 "theta", "upper"))
             TRUE
         })

setClass("NormalVarying",
         contains = c("VIRTUAL",
             "Model",
             "Normal",
             "Varying",
             "WNormalMixin"))
         
## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaKnown",
         contains = c("NormalVarying",
             "VarsigmaKnown"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta",
                                   "betas",
                                   "sigma",
                                   "priorsBetas"),
             iMethodModel = 4L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaUnknown",
         contains = c("NormalVarying",
             "VarsigmaUnknown"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta",
                                   "varsigma",
                                   "betas",
                                   "sigma",
                                   "priorsBetas"),
             iMethodModel = 5L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
setClass("PoissonVarying",
         contains = c("VIRTUAL",
             "Model",
             "Poisson",
             "Varying"),
         validity = function(object) {
             theta <- object@theta
             lower <- object@lower
             upper <- object@upper
             tolerance <- object@tolerance
             ## 'theta' greater than or equal to exp(lower)
             if (any(theta < exp(lower) - tolerance))
                 return(gettextf("'%s' has values that are less than '%s'",
                                 "theta", "lower"))
             ## 'theta' less than or equal to exp(upper)
             if (any(theta > exp(upper) + tolerance))
                 return(gettextf("'%s' has values that are greater than '%s'",
                                 "theta", "upper"))
             TRUE
         })

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingNotUseExp",
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta",
                                   "nAcceptTheta",
                                   "betas",
                                   "sigma",
                                   "priorsBetas"),
             iMethodModel = 6L,
             nuSigma = methods::new("DegreesFreedom", 7)),
        contains = c("PoissonVarying",
             "NotUseExposure"))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingUseExp",
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas"),
             iMethodModel = 10L,
             nuSigma = methods::new("DegreesFreedom", 7)),
         contains = c("PoissonVarying",
             "UseExposure"))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonBinomialMixture",
         slots = c(prob = "numeric"),
         contains = c("Model", "Prob", "UseExposure"),
         prototype = prototype(slotsToExtract = character(),
             iMethodModel = 11L))


## Models With Aggregate ##################################################################

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaKnownAgCertain",
         contains = c("NormalVaryingVarsigmaKnown",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas"),
             iMethodModel = 12L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaUnknownAgCertain",
         contains = c("NormalVaryingVarsigmaUnknown",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "varsigma", "betas", "sigma",
                                   "priorsBetas"),
             iMethodModel = 13L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaKnownAgNormal",
         contains = c("NormalVaryingVarsigmaKnown",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
             iMethodModel = 14L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaUnknownAgNormal",
         contains = c("NormalVaryingVarsigmaUnknown",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "varsigma", "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
             iMethodModel = 15L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingNotUseExpAgCertain",
         contains = c("PoissonVaryingNotUseExp",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas"),
             iMethodModel = 16L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingNotUseExpAgNormal",
         contains = c("PoissonVaryingNotUseExp",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas",
                                   "valueAg", "nFailedPropValueAg", "nAcceptAg"),
             iMethodModel = 17L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("BinomialVaryingAgCertain",
         contains = c("BinomialVarying",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas"),
             iMethodModel = 18L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("BinomialVaryingAgNormal",
         contains = c("BinomialVarying",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas",
                                   "valueAg", "nFailedPropValueAg", "nAcceptAg"),
             iMethodModel = 19L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingUseExpAgCertain",
         contains = c("PoissonVaryingUseExp",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas"),
             iMethodModel = 20L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingUseExpAgNormal",
         contains = c("PoissonVaryingUseExp",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas",
                                   "valueAg", "nFailedPropValueAg", "nAcceptAg"),
             iMethodModel = 21L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingNotUseExpAgPoisson",
         contains = c("PoissonVaryingNotUseExp",
             "AgPoisson"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
             iMethodModel = 22L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingUseExpAgPoisson",
         contains = c("PoissonVaryingUseExp",
             "AgPoisson"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
             iMethodModel = 23L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaKnownAgFun",
         contains = c("NormalVaryingVarsigmaKnown",
             "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas",
                                   "valueAg"),
             iMethodModel = 24L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaUnknownAgFun",
         contains = c("NormalVaryingVarsigmaUnknown",
             "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "varsigma", "betas", "sigma",
                                   "priorsBetas", "valueAg"),
             iMethodModel = 25L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
setClass("PoissonVaryingNotUseExpAgFun",
         contains = c("PoissonVaryingNotUseExp",
             "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg"),
             iMethodModel = 26L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
setClass("BinomialVaryingAgFun",
         contains = c("BinomialVarying",
             "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas",
                                   "valueAg"),
             iMethodModel = 27L,
             nuSigma = methods::new("DegreesFreedom", 7)))

## HAS_TESTS
setClass("PoissonVaryingUseExpAgFun",
         contains = c("PoissonVaryingUseExp",
             "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas",
                                   "valueAg"),
             iMethodModel = 28L,
             nuSigma = methods::new("DegreesFreedom", 7)))



## Predicted Models - Basic #############################################################

## iMethodModel for 'Predict' classes equals iMethodModel for
## base class plus 100

## HAS_TESTS
setClass("BinomialVaryingPredict",
         prototype = prototype(iMethodModel = 109L),
         contains = c("BinomialVarying", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))

## HAS_TESTS
setClass("NormalVaryingVarsigmaKnownPredict",
         prototype = prototype(iMethodModel = 104L),
         contains = c("NormalVaryingVarsigmaKnown", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))

## HAS_TESTS
setClass("NormalVaryingVarsigmaUnknownPredict",
         prototype = prototype(iMethodModel = 105L),
         contains = c("NormalVaryingVarsigmaUnknown", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsVarsigma", "OffsetsSigma"))

## HAS_TESTS
setClass("PoissonVaryingNotUseExpPredict",
         prototype = prototype(iMethodModel = 106L),
         contains = c("PoissonVaryingNotUseExp", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))

## HAS_TESTS
setClass("PoissonVaryingUseExpPredict",
         prototype = prototype(iMethodModel = 110L),
         contains = c("PoissonVaryingUseExp", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))

## HAS_TESTS
setClass("PoissonBinomialMixturePredict",
         prototype = prototype(iMethodModel = 111L),
         contains = "PoissonBinomialMixture")


## Predicted Models - Aggregate #############################################################

## MIGHT NEED TO DELETE THESE CLASSES

## HAS_TESTS
setClass("BinomialVaryingPredictAgCertain",
         contains = c("BinomialVaryingPredict",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas"),
         iMethodModel = 118L))

## HAS_TESTS
setClass("BinomialVaryingPredictAgNormal",
         contains = c("BinomialVaryingPredict",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
         iMethodModel = 119L))

## HAS_TESTS
setClass("NormalVaryingVarsigmaKnownPredictAgCertain",
         contains = c("NormalVaryingVarsigmaKnownPredict",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas"),
             iMethodModel = 112L))

## HAS_TESTS
setClass("NormalVaryingVarsigmaUnknownPredictAgCertain",
         contains = c("NormalVaryingVarsigmaUnknownPredict",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "varsigma", "betas", "sigma",
                                   "priorsBetas"),
             iMethodModel = 113L))

## HAS_TESTS
setClass("NormalVaryingVarsigmaKnownPredictAgNormal",
         contains = c("NormalVaryingVarsigmaKnownPredict",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
         iMethodModel = 114L))

## HAS_TESTS
setClass("NormalVaryingVarsigmaUnknownPredictAgNormal",
         contains = c("NormalVaryingVarsigmaUnknownPredict",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "varsigma", "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
         iMethodModel = 115L))

## HAS_TESTS
setClass("PoissonVaryingNotUseExpPredictAgCertain",
         contains = c("PoissonVaryingNotUseExpPredict",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas"),
         iMethodModel = 116L))

## HAS_TESTS
setClass("PoissonVaryingNotUseExpPredictAgNormal",
         contains = c("PoissonVaryingNotUseExpPredict",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
         iMethodModel = 117L))


## HAS_TESTS
setClass("PoissonVaryingUseExpPredictAgCertain",
         contains = c("PoissonVaryingUseExpPredict",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas"),
         iMethodModel = 120L))

## HAS_TESTS
setClass("PoissonVaryingUseExpPredictAgNormal",
         contains = c("PoissonVaryingUseExpPredict",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
         iMethodModel = 121L))

## NO_TESTS
setClass("PoissonVaryingNotUseExpPredictAgPoisson",
         contains = c("PoissonVaryingNotUseExpPredict",
             "AgPoisson"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
         iMethodModel = 122L))

## NO_TESTS
setClass("PoissonVaryingUseExpPredictAgPoisson",
         contains = c("PoissonVaryingUseExpPredict",
             "AgPoisson"),
         prototype = prototype(slotsToExtract = c("theta",
                               "nFailedPropTheta", "nAcceptTheta",
                               "betas", "sigma", "priorsBetas",
                               "valueAg", "nFailedPropValueAg", "nAcceptAg"),
         iMethodModel = 123L))


