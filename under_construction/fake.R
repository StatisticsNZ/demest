model <- Model(y ~ Poisson(mean ~ age * sex),
               `(Intercept)` ~ ExchFixed(sd = 10), 
               age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
               sex ~ ExchFixed(sd = 0.1),
               age:sex ~ ExchFixed(sd = 0.05),
               priorSD = HalfT(scale = 0.2))

model <- Model(y ~ Poisson(mean ~ age * sex),
               `(Intercept)` ~ ExchFixed(sd = 10), 
               age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
               sex ~ ExchFixed(sd = 0.1),
               age:sex ~ ExchFixed(sd = 0.05),
               priorSD = HalfT(sc = 0.2))





setMethod("checkAndTidySimulatedYExposureWeights",
          signature(model = "SpecCMPVarying")
          function(model, y, exposure, weights) {
              warnSimulateModelIgnoresArg(arg = weights,
                                          argname = "weights",
                                          model = model)
              if (is.null(exposure)) {
                  checkYNoExposure(y)
                  y <- makeCountsY(exposure)
              }
              else if (methods::is(exposure, "Counts")) {
                  warnSimulateModelExposureAndYSupplied(y = y,
                                                        model = model)
                  checkSimulatedExposure(exposure)
                  y <- makeCountsY(exposure)              
              }
              else
                  stop(gettextf("'%s' has class \"%s\" but '%s' has class \"%s\"",
                                "model", class(model), "exposure", class(exposure)))
              list(y = y,
                   exposure = exposure)
          })

## leave checking of weights to 'checkAndTidyWeights'
setMethod("checkAndTidySimulatedYExposureWeights",
          signature(model = "SpecNormalVarying")
          function(model, y, exposure, weights) {
              if (!methods::is(y, "DemographicArray"))
                  stop(gettextf("'%s' has class \"%s\"",
                                "y", class(y)))
              warnSimulateModelIgnoresArg(arg = exposure,
                                          argname = "exposure",
                                          model = model)
              list(y = y,
                   exposure = NULL)
          })

setMethod("checkAndTidySimulatedYExposureWeights",
          signature(model = "SpecPoissonVarying")
          function(model, y, exposure, weights) {
              warnSimulateModelIgnoresArg(arg = weights,
                                          argname = "weights",
                                          model = model)
              if (is.null(exposure)) {
                  checkYNoExposure(y)
                  y <- makeCountsY(exposure)
              }
              else if (methods::is(exposure, "Counts")) {
                  warnSimulateModelExposureAndYSupplied(y = y,
                                                        model = model)
                  checkSimulatedExposure(exposure)
                  y <- makeCountsY(exposure)              
              }
              else
                  stop(gettextf("'%s' has class \"%s\" but '%s' has class \"%s\"",
                                "model", class(model), "exposure", class(exposure)))
              list(y = y,
                   exposure = exposure)
          })




setMethod("initialCombinedModelSimulate",
          signature(object = "SpecPoissonVarying"),
          function(object, y, exposure, weights) {
              model <- initialModel(object,
                                    y = y,
                                    exposure = exposure)
              model <- drawHyperParam(model)
              y <- setYToMissing(y)
              methods::new("CombinedModelPoisson",
                           model = model,
                           y = y,
                           exposure = exposure)
          })




                          







kainga_data <- data.frame(kainga = letters[1:10],
                          pc_pakeha = runif(n = 10, max = 0.2))

model <- Model(y ~ Binomial(mean ~ kainga),
               `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.1),
               kainga ~ Exch(covariates = Covariates(mean ~ pc_pakeha, data = kainga_data),
                             error = Error(scale = HalfT(scale = 0.1))),
               priorSD = HalfT(scale = 0.1))



setMethod("drawModelNotUseExp",
          signature(object = "CMPVaryingNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_CMPVaryingNotUseExp_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  object <- drawBetas(object)
                  object <- drawSigma_Varying(object)
                  object <- updateThetaAndNu_CMPVaryingNotUseExp(object, y = y)
                  object
              }
          })


setMethod("drawModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnown"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalVaryingVarsigmaKnown_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  varsigmaSetToZero <- object@varsigmaSetToZero@.Data
                  object <- drawBetas(object)
                  object <- drawSigma_Varying(object)
                  if (varsigmaSetToZero)
                      object <- updateThetaVarsigmaSetToZero(object)
                  else
                      object <- updateTheta_NormalVarying(object, y = y)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknown"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalVaryingVarsigmaUnknown_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVarying(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_PoissonVaryingNotUseExp_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgCertain"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgCertain"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgCertain"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_PoissonVaryingNotUseExpAgCertain_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgNormal"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_Normal(object = object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgNormal"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_Normal(object = object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgFun"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalVaryingVarsigmaKnownAgFun_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateThetaAndValueAgFun_Normal(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgFun"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateThetaAndValueAgFun_Normal(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgNormal"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_PoissonVaryingNotUseExpAgNormal_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgFun"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_PoissonVaryingNotUseExpAgFun_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateThetaAndValueAgFun_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgPoisson"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_PoissonVaryingNotUseExpAgPoisson_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateThetaAndValueAgPoisson_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "NormalFixedNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_NormalFixedNotUseExp_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  ## object is not updated
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawModelNotUseExp",
          signature(object = "TFixedNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_TFixedNotUseExp_R, object, y)
                  else
                      .Call(drawModelNotUseExp_R, object, y)
              }
              else {
                  ## object is not updated
                  object
              }
          })





## drawModelUseExp #################################################################


## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "CMPVaryingUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data) & (exposure@.Data == 0L)] == 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_CMPVaryingUseExp_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateThetaAndNu_CMPVaryingUseExp(object, y = y, exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "PoissonVaryingUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data) & (exposure@.Data == 0L)] == 0))
              stopifnot(all(y@.Data[!is.na(y@.Data) & (exposure@.Data == 0L)] == 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_PoissonVarying_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "PoissonBinomialMixture"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_PoissonBinomialMixture_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "Round3"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(dataset[!is.na(dataset)] %% 3L == 0L))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_Round3_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "BinomialVaryingAgCertain"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_BinomialVaryingAgCertain_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateTheta_BinomialVaryingAgCertain(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "PoissonVaryingUseExpAgCertain"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_PoissonVaryingUseExpAgCertain_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "BinomialVaryingAgNormal"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_BinomialVaryingAgNormal_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateTheta_BinomialVaryingAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgNormal_Binomial(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "BinomialVaryingAgFun"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_BinomialVaryingAgFun_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateThetaAndValueAgFun_Binomial(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "PoissonVaryingUseExpAgNormal"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_PoissonVaryingUseExpAgNormal_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgNormal_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "PoissonVaryingUseExpAgFun"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_PoissonVaryingUseExpAgFun_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateThetaAndValueAgFun_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "PoissonVaryingUseExpAgLife"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_PoissonVaryingUseExpAgLife_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateThetaAndValueAgLife_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "PoissonVaryingUseExpAgPoisson"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_PoissonVaryingUseExpAgPoisson_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgPoisson_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "NormalFixedUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_NormalFixedUseExp_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "Round3"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_Round3_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawModelUseExp",
          signature(object = "TFixedUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_TFixedUseExp_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })
