
## printSpecAggregateEqns ################################################################

setMethod("printSpecAggregateEqns",
          signature(object = "SpecAgPlaceholder"),
          function(object) invisible())

setMethod("printSpecAggregateEqns",
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

setMethod("printSpecAggregateEqns",
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

setMethod("printSpecAggregateEqns",
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
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("damp ~ Unif(", min, ", ", max, ")\n", sep = "")
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
              printSpecAggregateEqns(object)
              printValueAg(object)
              printWeightAg(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecAgNormal"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printSpecAggregateEqns(object)
              printValueAg(object)
              printSDAg(object)
              printWeightAg(object)
              printJumpAg(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecAgPoisson"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printSpecAggregateEqns(object)
              printValueAg(object)
              printWeightAg(object)
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
          signature(object = "SpecBinomialVarying"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printBinomialSpecEqns(object)
              cat("\n")
              printSpecsPriorsEqns(object)
              cat("\n")
              printSDEqns(object)
              printAggregateSpecEqns(object)
              printJump(object)
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
              cat("\n")
              printSDEqns(object)
              printAggregateSpecEqns(object)
              printJump(object)
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
              cat("\n")
              printSDEqns(object)
              printAggregateSpecEqns(object)
              printJump(object)
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
              cat("\n")
              printSDEqns(object)
              printAggregateSpecEqns(object)
              printJump(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecPoissonBinomialMixture"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPoissonBinomialSpecEqns(object)
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
              has.series <- !is.na(series)
              if (has.series)
                  sprintf("    rate[i]: %s\n", scale)
              else
                  sprintf("    rate[i] or count[i]: %s\n", scale)
          })

setMethod("stringScaleTheta",
          signature(object = "SpecPoissonBinomialMixture"),
          function(object) "")
