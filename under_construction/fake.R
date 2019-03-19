


## All the functions below are needed, as of 2019-03-14

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




                          





