

## checkAndTidySimulatedYExposureWeights ##################################################

## HAS_TESTS
setMethod("checkAndTidySimulatedYExposureWeights",
          signature(model = "SpecBinomialVarying"),
          function(model, y, exposure, weights) {
              warnSimulateModelIgnoresArg(arg = y,
                                          argname = "y",
                                          model = model)
              warnSimulateModelIgnoresArg(arg = weights,
                                          argname = "weights",
                                          model = model)
              checkSimulatedExposure(exposure)
              y <- makeCountsY(exposure)
              list(y = y,
                   exposure = exposure)
          })


## modelUsesWeights  ######################################################################

## HAS_TESTS
setMethod("modelUsesWeights",
          signature(object = "SpecNormalVarying"),
          function(object) TRUE)


## printSpecAgAccuracyEqns ################################################################

setMethod("printSpecAgAccuracyEqns",
          signature(object = "SpecAgPlaceholder"),
          function(object) invisible())

setMethod("printSpecAgAccuracyEqns",
          signature(object = "SpecAgCertain"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("        value[a] = aggregate[a]\n")
              else
                  cat("           value = aggregate\n")
          })

setMethod("printSpecAgAccuracyEqns",
          signature(object = "SpecAgNormal"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("        value[a] ~ N(aggregate[a], sd[a]^2)\n")
              else
                  cat("           value ~ N(aggregate, sd^2)\n")
          })

setMethod("printSpecAgAccuracyEqns",
          signature(object = "SpecAgFun"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("        value[a] ~ N(aggregate[a], sd[a]^2)\n")
              else
                  cat("           value ~ N(aggregate, sd^2)\n")
          })

setMethod("printSpecAgAccuracyEqns",
          signature(object = "SpecAgLife"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("        value[a] ~ N(aggregate[a], sd[a]^2)\n")
              else
                  cat("           value ~ N(aggregate, sd^2)\n")
          })

setMethod("printSpecAgAccuracyEqns",
          signature(object = "SpecAgPoisson"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("exposure[a] * value[a] ~ Poisson(exposure[a] * aggregate[a])\n")
              else
                  cat("exposure * value ~ Poisson(aggregate * value)\n")
          })


## printSpecAgValEqns ##############################################################

setMethod("printSpecAgValEqns",
          signature(object = "SpecNormalVarying",
                    aggregate = "SpecAgPlaceholder"),
          function(object, aggregate) invisible())

setMethod("printSpecAgValEqns",
          signature(object = "SpecNormalVarying",
                    aggregate = "SpecAggregate"),
          function(object, aggregate) {
              cat("\n")
              cat("       aggregate = sum(mean * weight)")
          })

setMethod("printSpecAgValEqns",
          signature(object = "SpecNormalVarying",
                    aggregate = "SpecAgFun"),
          function(object, aggregate) {
              cat("\n")
              cat("       aggregate = f(mean, weight)")
          })

setMethod("printSpecAgValEqns",
          signature(object = "SpecBinomialVarying",
                    aggregate = "SpecAgPlaceholder"),
          function(object, aggregate) invisible())

setMethod("printSpecAgValEqns",
          signature(object = "SpecCMPVarying",
                    aggregate = "SpecAgPlaceholder"),
          function(object, aggregate) invisible())

setMethod("printSpecAgValEqns",
          signature(object = "SpecBinomialVarying",
                    aggregate = "SpecAggregate"),
          function(object, aggregate) {
              cat("\n")
              cat("       aggregate = sum(prob * weight)")
          })

setMethod("printSpecAgValEqns",
          signature(object = "SpecBinomialVarying",
                    aggregate = "SpecAgFun"),
          function(object, aggregate) {
              cat("\n")
              cat("       aggregate = f(prob, weight)")
          })

setMethod("printSpecAgValEqns",
          signature(object = "SpecPoissonVarying",
                    aggregate = "SpecAgPlaceholder"),
          function(object, aggregate) invisible())

setMethod("printSpecAgValEqns",
          signature(object = "SpecPoissonVarying",
                    aggregate = "SpecAggregate"),
          function(object, aggregate) {
              use.expose <- object@useExpose@.Data
              cat("\n")
              if (use.expose)
                  cat("       aggregate = sum(rate * weight)\n")
              else
                  cat("       aggregate = sum(count * weight)")
          })

setMethod("printSpecAgValEqns",
          signature(object = "SpecPoissonVarying",
                    aggregate = "SpecAgFun"),
          function(object, aggregate) {
              use.expose <- object@useExpose@.Data
              cat("\n")
              if (use.expose)
                  cat("       aggregate = f(rate, weight)\n")
              else
                  cat("       aggregate = f(count * weight)")
          })

setMethod("printSpecAgValEqns",
          signature(object = "SpecPoissonVarying",
                    aggregate = "SpecAgLife"),
          function(object, aggregate) {
              cat("\n")
              cat("         rate.ag = sum(rate * weight)\n")
              cat("       aggregate = LifeExp(rate.ag)\n")
          })



## show ##########################################################################

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "DampKnown"),
          function(object) {
              phi <- object@phi
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("damp =", phi, "\n")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "DampUnknown"),
          function(object) {
              min <- object@minPhi
              max <- object@maxPhi
              shape1 <- object@shape1Phi@.Data
              shape2 <- object@shape2Phi@.Data
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("dampTransform = (damp-",
                  format(min, digits = 4),
                  ")/(",
                  format(max, digits = 4),
                  "-",
                  format(min, digits = 4),
                  ")\n",
                  sep = "")
              cat("dampTransform ~ Beta(",
                  format(shape1, digits = 4),
                  ",",
                  format(shape2, digits = 4),
                  ")\n",
                  sep = "")
          })                        

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Dispersion"),
          function(object) {
              mean <- object@meanLogNuCMP@.Data
              sd <- object@sdLogNuCMP@.Data
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("log(dispersion[i]) ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecAgPlaceholder"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecAgCertain"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printSpecAgAccuracyEqns(object)
              printValueAg(object)
              printWeightAg(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecAgNormal"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printSpecAgAccuracyEqns(object)
              printValueAg(object)
              printSDAg(object)
              printWeightAg(object)
              printJumpAg(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecAgFun"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printSpecAgAccuracyEqns(object)
              printValueAg(object)
              printSDAg(object)
              printWeightAg(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecAgPoisson"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printSpecAgAccuracyEqns(object)
              printValueAg(object)
              printJumpAg(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodBinomial"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printBinomialLikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodCMP"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printCMPLikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodNormalVarsigmaKnown"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printNormalVarsigmaKnownLikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodNormalVarsigmaUnknown"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printNormalVarsigmaUnknownLikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodPoisson"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPoissonLikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodPoissonBinomialMixture"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPoissonBinomialLikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodRound3"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printRound3LikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodNormalFixed"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printNormalFixedLikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodTFixed"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printTFixedLikEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecBinomialVarying"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printBinomialSpecEqns(object)
              cat("\n")
              printSpecsPriorsEqns(object)
              printSDEqns(object)
              printSpecAggregateEqns(object)
              printJump(object)
              printHMC(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecCMPVarying"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printCMPSpecEqns(object)
              cat("\n")
              printSpecsPriorsEqns(object)
              printSDEqns(object)
              printSpecAggregateEqns(object)
              printJump(object)
              printHMC(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecNormalVaryingVarsigmaKnown"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printNormalVarsigmaKnownSpecEqns(object)
              cat("\n")
              printSpecsPriorsEqns(object)
              printSDEqns(object)
              printSpecAggregateEqns(object)
              printJump(object)
              printHMC(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecNormalVaryingVarsigmaUnknown"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printNormalVarsigmaUnknownSpecEqns(object)
              cat("\n")
              printSpecsPriorsEqns(object)
              printSDEqns(object)
              printSpecAggregateEqns(object)
              printJump(object)
              printHMC(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecPoissonVarying"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPoissonSpecEqns(object)
              cat("\n")
              printSpecsPriorsEqns(object)
              printSDEqns(object)
              printSpecAggregateEqns(object)
              printJump(object)
              printHMC(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecPoissonBinomialMixture"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPoissonBinomialSpecEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecRound3"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printRound3SpecEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecNormalFixed"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printNormalFixedSpecEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecTFixed"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printTFixedSpecEqns(object)
          })




## stringScaleAg ##############################################################################

setMethod("stringScaleAg",
          signature(object = "ScaleAgMixin"),
          function(object) {
              scale <- object@scaleAg@.Data
              value <- object@valueAg@.Data
              n.value <- length(value)
              if (n.value > 1L)
                  sprintf("    aggregate[a]: %s\n", scale)
              else
                  sprintf("    aggregate: %s\n", scale)
          })


## stringScaleTheta ###########################################################################

setMethod("stringScaleTheta",
          signature(object = "SpecBinomialVarying"),
          function(object) {
              scale <- object@scaleTheta@.Data
              sprintf("    prob[i]: %s\n", scale)
          })


setMethod("stringScaleTheta",
          signature(object = "SpecCMPVarying"),
          function(object) {
              scale <- object@scaleTheta@.Data
              series <- object@series@.Data
              use.expose <- object@useExpose@.Data
              has.series <- !is.na(series)
              if (use.expose || has.series)
                  sprintf("    rate[i]: %s\n", scale)
              else
                  sprintf("    count[i]: %s\n", scale)
          })

setMethod("stringScaleTheta",
          signature(object = "SpecNormalVarying"),
          function(object) {
              scale <- object@scaleTheta@.Data
              lower <- object@lower
              upper <- object@upper
              aggregate <- object@aggregate
              uses.scale <- (is.finite(lower)
                             || is.finite(upper)
                             || !methods::is(aggregate, "SpecAgPlaceholder"))
              if (uses.scale)
                  sprintf("    mean[i]: %s\n", scale)
              else
                  ""
          })

setMethod("stringScaleTheta",
          signature(object = "SpecPoissonVarying"),
          function(object) {
              scale <- object@scaleTheta@.Data
              series <- object@series@.Data
              use.expose <- object@useExpose@.Data
              has.series <- !is.na(series)
              if (use.expose || has.series)
                  sprintf("    rate[i]: %s\n", scale)
              else
                  sprintf("    count[i]: %s\n", scale)
          })

setMethod("stringScaleTheta",
          signature(object = "SpecPoissonBinomialMixture"),
          function(object) "")


setMethod("stringScaleTheta",
          signature(object = "SpecNormalFixed"),
          function(object) "")

setMethod("stringScaleTheta",
          signature(object = "SpecTFixed"),
          function(object) "")

setMethod("stringScaleTheta",
          signature(object = "SpecRound3"),
          function(object) "")
