

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
                      "BoxCoxParamMixin",
                      "ScaleThetaMultiplierMixin",
                      "StrucZeroArrayMixin"),
         validity = function(object) {
             theta <- object@theta
             ## 'theta' is non-negative
             if (any(theta[!is.na(theta)] < 0))
                 return(gettextf("'%s' has negative values", "theta"))
             TRUE
         })

## NO_TESTS
setClass("CMP",
         contains = c("VIRTUAL",
                      "BoxCoxParamMixin",
                      "SDLogNuCMPMixin",
                      "MeanLogNuCMPMixin",
                      "NuCMPMixin",
                      "ScaleThetaMultiplierMixin",
                      "NFailedPropYStarMixin",
                      "StrucZeroArrayMixin"),
         validity = function(object) {
             theta <- object@theta
             ## 'theta' is non-negative
             if (any(theta[!is.na(theta)] < 0))
                 return(gettextf("'%s' has negative values", "theta"))
             TRUE
         })

## HAS_TESTS
setClass("NormalFixed",
         contains = c("Model", "MeanSDMixin", "MeanSDMetadataAllMixin"),
         validity = function(object) {
             mean <- object@mean@.Data
             metadataY <- object@metadataY
             ## 'metadataY' and 'mean' consistent
             if (!identical(length(mean), as.integer(prod(dim(metadataY)))))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "mean", "metadataY"))
             TRUE
         })

## NO_TESTS
setClass("TFixed",
         contains = c("VIRTUAL",
                      "Model",
                      "MeanSDMixin",
                      "MeanSDMetadataAllMixin",
                      "NuMixin"),
         validity = function(object) {
             mean <- object@mean@.Data
             metadataY <- object@metadataY
             ## 'metadataY' and 'mean' consistent
             if (!identical(length(mean), as.integer(prod(dim(metadataY)))))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "mean", "metadataY"))
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
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 9L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)),
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
         prototype = prototype(varsigmaSetToZero = methods::new("LogicalFlag", FALSE),
                               slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 4L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)),
         validity = function(object) {
             varsigmaSetToZero <- object@varsigmaSetToZero@.Data
             if (varsigmaSetToZero) {
                 ## if varsigma is 0, lower, upper not specified
                 for (name in c("lower", "upper")) {
                     value <- methods::slot(object, name)
                     if (is.finite(value))
                         return(gettextf("'%s' is %d but '%s' is finite",
                                         "varsigma", 0L, name))
                 }
             }
             TRUE
         })


## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaUnknown",
         contains = c("NormalVarying",
                      "VarsigmaUnknown"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "varsigma",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 5L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostVarsigma = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

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
             boxCoxParam <- object@boxCoxParam
             if (boxCoxParam > 0) {
                 lower.back.tr <- (boxCoxParam * lower + 1) ^ (1 / boxCoxParam)
                 upper.back.tr <- (boxCoxParam * upper + 1) ^ (1 / boxCoxParam)
             }
             else {
                 lower.back.tr <- exp(lower)
                 upper.back.tr <- exp(upper)
             }
             ## 'theta' greater than or equal to back-transformed 'lower'
             if (any(theta[!is.na(theta)] < lower.back.tr - tolerance))
                 return(gettextf("'%s' has values that are less than '%s'",
                                 "theta", "lower"))
             ## 'theta' less than or equal to back-transformed 'upper'
             if (any(theta[!is.na(theta)] > upper.back.tr + tolerance))
                 return(gettextf("'%s' has values that are greater than '%s'",
                                 "theta", "upper"))
             TRUE
         })

## NO_TESTS
setClass("CMPVarying",
         contains = c("VIRTUAL",
                      "Model",
                      "CMP",
                      "Varying"),
         validity = function(object) {
             theta <- object@theta
             lower <- object@lower
             upper <- object@upper
             tolerance <- object@tolerance
             boxCoxParam <- object@boxCoxParam
             if (boxCoxParam > 0) {
                 lower.back.tr <- (boxCoxParam * lower + 1) ^ (1 / boxCoxParam)
                 upper.back.tr <- (boxCoxParam * upper + 1) ^ (1 / boxCoxParam)
             }
             else {
                 lower.back.tr <- exp(lower)
                 upper.back.tr <- exp(upper)
             }
             ## 'theta' greater than or equal to back-transformed 'lower'
             if (any(theta[!is.na(theta)] < lower.back.tr - tolerance))
                 return(gettextf("'%s' has values that are less than '%s'",
                                 "theta", "lower"))
             ## 'theta' less than or equal to back-transformed 'upper'
             if (any(theta[!is.na(theta)] > upper.back.tr + tolerance))
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
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 6L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)),
         contains = c("PoissonVarying",
                      "NotUseExposure"))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingUseExp",
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 10L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)),
         contains = c("PoissonVarying",
                      "UseExposure"))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonBinomialMixture",
         contains = c("Model", "Prob", "UseExposure"),
         prototype = prototype(slotsToExtract = character(),
                               iMethodModel = 11L))


## HAS_TESTS
setClass("NormalFixedNotUseExp",
         contains = c("NormalFixed",
                      "NotUseExposure"),
         prototype = prototype(slotsToExtract = character(),

                               iMethodModel = 30L))

## HAS_TESTS
setClass("NormalFixedUseExp",
         contains = c("NormalFixed",
                      "UseExposure"),
         prototype = prototype(slotsToExtract = character(),
                               iMethodModel = 31L))

## NO_TESTS
setClass("CMPVaryingNotUseExp",
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nFailedPropYStar",
                                                  "nAcceptTheta",
                                                  "nuCMP",
                                                  "betas",
                                                  "acceptBeta",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 32L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)),
         contains = c("CMPVarying",
                      "NotUseExposure"))

## NO_TESTS
setClass("CMPVaryingUseExp",
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nFailedPropYStar",
                                                  "nAcceptTheta",
                                                  "nuCMP",
                                                  "betas",
                                                  "acceptBeta",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 33L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)),
         contains = c("CMPVarying",
                      "UseExposure"))


## HAS_TESTS
setClass("Round3",
         contains = c("Model",
                      "UseExposure"),
         prototype = prototype(slotsToExtract = character(),
                               iMethodModel = 34L))


## HAS_TESTS
setClass("TFixedNotUseExp",
         contains = c("TFixed",
                      "NotUseExposure"),
         prototype = prototype(nu = methods::new("DegreesFreedom", 7),
                               slotsToExtract = character(),
                               iMethodModel = 35L))

## HAS_TESTS
setClass("TFixedUseExp",
         contains = c("TFixed",
                      "UseExposure"),
         prototype = prototype(nu = methods::new("DegreesFreedom", 7),
                               slotsToExtract = character(),
                               iMethodModel = 36L))


## Models With Aggregate ##################################################################

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaKnownAgCertain",
         contains = c("NormalVaryingVarsigmaKnown",
                      "AgCertain"),
         prototype = prototype(varsigmaSetToZero = methods::new("LogicalFlag", FALSE),
                               slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 12L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaUnknownAgCertain",
         contains = c("NormalVaryingVarsigmaUnknown",
                      "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "varsigma",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 13L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostVarsigma = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaKnownAgNormal",
         contains = c("NormalVaryingVarsigmaKnown",
                      "AgNormal"),
         prototype = prototype(varsigmaSetToZero = methods::new("LogicalFlag", FALSE),
                               slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 14L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaUnknownAgNormal",
         contains = c("NormalVaryingVarsigmaUnknown",
                      "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "varsigma",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 15L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostVarsigma = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingNotUseExpAgCertain",
         contains = c("PoissonVaryingNotUseExp",
                      "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 16L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingNotUseExpAgNormal",
         contains = c("PoissonVaryingNotUseExp",
                      "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 17L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("BinomialVaryingAgCertain",
         contains = c("BinomialVarying",
                      "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 18L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("BinomialVaryingAgNormal",
         contains = c("BinomialVarying",
                      "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 19L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingUseExpAgCertain",
         contains = c("PoissonVaryingUseExp",
                      "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 20L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingUseExpAgNormal",
         contains = c("PoissonVaryingUseExp",
                      "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 21L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingNotUseExpAgPoisson",
         contains = c("PoissonVaryingNotUseExp",
                      "AgPoisson"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 22L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("PoissonVaryingUseExpAgPoisson",
         contains = c("PoissonVaryingUseExp",
                      "AgPoisson"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 23L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaKnownAgFun",
         contains = c("NormalVaryingVarsigmaKnown",
                      "AgFun"),
         prototype = prototype(varsigmaSetToZero = methods::new("LogicalFlag", FALSE),
                               slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg"),
                               iMethodModel = 24L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
## HAS_UPDATE
setClass("NormalVaryingVarsigmaUnknownAgFun",
         contains = c("NormalVaryingVarsigmaUnknown",
                      "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "varsigma",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg"),
                               iMethodModel = 25L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
setClass("PoissonVaryingNotUseExpAgFun",
         contains = c("PoissonVaryingNotUseExp",
                      "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg"),
                               iMethodModel = 26L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
setClass("BinomialVaryingAgFun",
         contains = c("BinomialVarying",
                      "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg"),
                               iMethodModel = 27L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## HAS_TESTS
setClass("PoissonVaryingUseExpAgFun",
         contains = c("PoissonVaryingUseExp",
                      "AgFun"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg"),
                               iMethodModel = 28L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))

## NO_TESTS
setClass("PoissonVaryingUseExpAgLife",
         contains = c("PoissonVaryingUseExp",
                      "AgLife"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "mxAg"),
                               iMethodModel = 29L,
                               nuSigma = methods::new("DegreesFreedom", 7),
                               stepSize = methods::new("Scale", 0.1),
                               nStep = methods::new("Length", 10L),
                               acceptBeta = 0L,
                               useHMC = methods::new("LogicalFlag", TRUE),
                               logPostTheta = methods::new("Parameter", 0),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostBetas = methods::new("Parameter", 0),
                               logPostPriorsBetas = methods::new("Parameter", 0)))



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

## HAS_TESTS
setClass("NormalFixedNotUseExpPredict",
         prototype = prototype(iMethodModel = 130L),
         contains = "NormalFixedNotUseExp")

## HAS_TESTS
setClass("NormalFixedUseExpPredict",
         prototype = prototype(iMethodModel = 131L,
                               slotsToExtract = character()),
         contains = "NormalFixedUseExp")

## NO_TESTS
setClass("CMPVaryingNotUseExpPredict",
         prototype = prototype(iMethodModel = 132L),
         contains = c("CMPVaryingNotUseExp", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))

## NO_TESTS
setClass("CMPVaryingUseExpPredict",
         prototype = prototype(iMethodModel = 133L),
         contains = c("CMPVaryingUseExp", "BetaIsPredicted", "OffsetsBetas",
                      "OffsetsPriorsBetas", "OffsetsSigma"))

## HAS_TESTS
setClass("Round3Predict",
         contains = "Round3",
         prototype = prototype(slotsToExtract = character(),
                               iMethodModel = 134L))

## HAS_TESTS
setClass("TFixedNotUseExpPredict",
         prototype = prototype(nu = methods::new("DegreesFreedom", 7),
                               slotsToExtract = character(),
                               iMethodModel = 135L),
         contains = "TFixedNotUseExp")

## HAS_TESTS
setClass("TFixedUseExpPredict",
         prototype = prototype(nu = methods::new("DegreesFreedom", 7),
                               slotsToExtract = character(),
                               iMethodModel = 136L),
         contains = "TFixedUseExp")

## Predicted Models - Aggregate #############################################################

## MIGHT NEED TO DELETE THESE CLASSES

## HAS_TESTS
setClass("BinomialVaryingPredictAgCertain",
         contains = c("BinomialVaryingPredict",
                      "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 118L))

## HAS_TESTS
setClass("BinomialVaryingPredictAgNormal",
         contains = c("BinomialVaryingPredict",
                      "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 119L))

## HAS_TESTS
setClass("NormalVaryingVarsigmaKnownPredictAgCertain",
         contains = c("NormalVaryingVarsigmaKnownPredict",
                      "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 112L))

## HAS_TESTS
setClass("NormalVaryingVarsigmaUnknownPredictAgCertain",
         contains = c("NormalVaryingVarsigmaUnknownPredict",
             "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "varsigma",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
             iMethodModel = 113L))

## HAS_TESTS
setClass("NormalVaryingVarsigmaKnownPredictAgNormal",
         contains = c("NormalVaryingVarsigmaKnownPredict",
                      "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 114L))

## HAS_TESTS
setClass("NormalVaryingVarsigmaUnknownPredictAgNormal",
         contains = c("NormalVaryingVarsigmaUnknownPredict",
                      "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "varsigma",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 115L))

## HAS_TESTS
setClass("PoissonVaryingNotUseExpPredictAgCertain",
         contains = c("PoissonVaryingNotUseExpPredict",
                      "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 116L))

## HAS_TESTS
setClass("PoissonVaryingNotUseExpPredictAgNormal",
         contains = c("PoissonVaryingNotUseExpPredict",
                      "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 117L))


## HAS_TESTS
setClass("PoissonVaryingUseExpPredictAgCertain",
         contains = c("PoissonVaryingUseExpPredict",
                      "AgCertain"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas"),
                               iMethodModel = 120L))

## HAS_TESTS
setClass("PoissonVaryingUseExpPredictAgNormal",
         contains = c("PoissonVaryingUseExpPredict",
             "AgNormal"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
         iMethodModel = 121L))

## NO_TESTS
setClass("PoissonVaryingNotUseExpPredictAgPoisson",
         contains = c("PoissonVaryingNotUseExpPredict",
             "AgPoisson"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
         iMethodModel = 122L))

## NO_TESTS
setClass("PoissonVaryingUseExpPredictAgPoisson",
         contains = c("PoissonVaryingUseExpPredict",
                      "AgPoisson"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "nFailedPropValueAg",
                                                  "nAcceptAg"),
                               iMethodModel = 123L))

## NO_TESTS
setClass("PoissonVaryingUseExpPredictAgLife",
         contains = c("PoissonVaryingUseExp",
                      "AgLife"),
         prototype = prototype(slotsToExtract = c("theta",
                                                  "nFailedPropTheta",
                                                  "nAcceptTheta",
                                                  "betas",
                                                  "acceptBeta",
                                                  "sigma",
                                                  "priorsBetas",
                                                  "valueAg",
                                                  "mxAg"),
                               iMethodModel = 129L))


