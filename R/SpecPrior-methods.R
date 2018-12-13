

## checkPriorIsInformative ###########################################################


setGeneric("checkPriorIsInformative",
           function(object)
               standardGeneric("checkPriorIsInformative"))


checkPriorInform_multTau <- function(object, nameArg, nameFun) {
    multTau <- object@multTau@.Data
    if (!identical(multTau, 1))
        stop(gettextf("'%s' argument not allowed in call to '%s' when specifying model for fake data",
                      nameArg, nameFun))
    NULL
}


checkPriorInform_tau <- function(object, nameArg, nameFun) {
    tau <- object@tau@.Data
    if (is.na(tau))
        stop(gettextf("'%s' argument required in call to '%s' when specifying model for fake data",
                      nameArg, nameFun))
    NULL
}

checkPriorInform_ATau <- function(object, nameArg, nameFun) {
    ATau <- object@ATau@.Data
    if (is.na(ATau))
        stop(gettextf("'%s' argument required in call to '%s' when specifying model for fake data",
                      nameArg, nameFun))
    NULL
}




setMethod("checkPriorIsInformative",
          signature(object = "SpecExchFixed"),
          function(object) {
              checkPriorInform_multTau(object = object,
                                       nameArg = "mult",
                                       nameFun = "ExchFixed")
              checkPriorInform_tau(object = object,
                                   nameArg = "sd",
                                   nameFun = "ExchFixed")
          })


setMethod("checkPriorIsInformative",
          signature(object = "SpecExch"),
          function(object) {
              checkPriorInform_multTau(object = object,
                                       nameArg = "mult",
                                       nameFun = "HalfT")
              checkPriorInform_Atau(object = object,
                                    nameArg = "scale",
                                    nameFun = "HalfT")
          })


setMethod("checkPriorIsInformative",
          signature(object = "SpecExchRobustZero"),
          function(object) {
              checkPriorInform_multTau(object = object,
                                       nameArg = "mult",
                                       nameFun = "HalfT")
              checkPriorInform_Atau(object = object,
                                    nameArg = "scale",
                                    nameFun = "HalfT")
          })




## printPriorEqns ####################################################################


setMethod("printPriorEqns",
          signature(object = "SpecDLM"),
          function(object, name = NULL, order = 1L) {
              has.trend <- methods::is(object, "SpecWithTrendMixin")
              has.season <- methods::is(object, "SpecSeasonMixin")
              has.covariates <- methods::is(object, "SpecCovariatesMixin")
              printDLMEqns(object = object,
                           name = name,
                           order = order,
                           hasTrend = has.trend,
                           hasSeason = has.season,
                           hasCovariates = has.covariates)
          })

setMethod("printPriorEqns",
          signature(object = "SpecExch"),
          function(object, name = NULL, order = 1L) {
              has.covariates <- methods::is(object, "SpecCovariatesMixin")
              printExchEqns(object = object,
                            name = name,
                            hasCovariates = has.covariates)
          })

setMethod("printPriorEqns",
          signature(object = "SpecExchFixed"),
          function(object, name = NULL, order = 1L) {
              printExchFixedEqns(object = object,
                                 name = name)
          })


setMethod("printPriorEqns",
          signature(object = "SpecKnown"),
          function(object, name = NULL, order = 1L) {
              printKnownEqns(object = object,
                             name = name)
          })

setMethod("printPriorEqns",
          signature(object = "SpecMix"),
          function(object, name = NULL, order = 1L) {
              has.covariates <- methods::is(object, "SpecCovariatesMixin")
              printMixEqns(object = object,
                           name = name,
                           hasCovariates = has.covariates)
          })

setMethod("printPriorEqns",
          signature(object = "SpecZero"),
          function(object, name = NULL, order = 1L) {
              printZeroEqns(name = name)
          })



## show ###############################################################################

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Components"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              nu <- object@nuVectorsMix@.Data
              A <- object@AVectorsMix@.Data
              max <- object@omegaVectorsMaxMix@.Data
              cat("scaleComponent ~ trunc-half-t(", nu, ", ", sep = "")
              cat(squaredOrNA(A), ", ", max, ")\n", sep = "")
          })    

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Covariates"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              contrastsArg <- object@contrastsArg
              formula <- object@formula
              data <- object@data
              if (length(formula) > 0L) {
                  cat("formula:\n")
                  cat(deparse(object@formula), "\n\n", sep = "")
              }
              cat("priors:\n")
              printCovariatesEqns(object)
              if (length(contrastsArg) > 0L) {
                  cat("\ncontrasts:\n")
                  print(contrastsArg)
              }
              else
                  cat("\n")
              if (length(data) > 0L) {
                  cat("data:\n")
                  print(data)
              }
          })    

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Error"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printErrorEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "HalfT"),
          function(object) {
              nu <- object@nu
              A <- object@A
              max <- object@scaleMax
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("trunc-half-t(", nu, ", ", sep = "")
              cat(squaredOrNA(A), ", ", max, ")\n", sep = "")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Initial"),
          function(object) {
              mean <- object@mean@.Data
              sd <- object@A@.Data
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Level"),
          function(object) {
              nu <- object@nuAlpha
              A <- object@AAlpha
              max <- object@omegaAlphaMax
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("  level[0] ~ N(0, NA)\n")
              cat("scaleLevel ~ trunc-half-t(", nu, ", ", sep = "")
              cat(squaredOrNA(A), ", ", max, ")\n", sep = "")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Norm"),
          function(object) {
              mean <- object@mean@.Data
              A <- object@A@.Data
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("N(", mean, ", ", squaredOrNA(A), ")\n", sep = "")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "TDist"),
          function(object) {
              nu <- object@nuEtaCoef@.Data
              mean <- object@meanEtaCoef@.Data
              A <- object@AEtaCoef@.Data
              n <- length(nu)
              cat("An object of class \"", class(object), "\"\n", sep = "")
              if (n == 1L)
                  cat("t(", nu, ", ", mean, ", ", squaredOrNA(A), ")\n", sep = "")
              else
                  cat("t([", paste(nu, collapse = ","), "], [",
                      paste(mean, collapse = ","), "], [",
                      paste(sapply(A, squaredOrNA), collapse = ","), "])\n",
                      sep = "")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Trend"),
          function(object) {
              mean <- object@meanDelta0
              sd <- object@ADelta0
              nu <- object@nuDelta
              A <- object@ADelta
              max <- object@omegaDeltaMax
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("  trend[0] ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
              cat("scaleTrend ~ trunc-half-t(", nu, ", ", sep = "")
              cat(squaredOrNA(A), ", ", max, ")\n", sep = "")
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Season"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printSeasonEqns(object = object,
                              isMain = TRUE)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecDLM"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPriorEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecExch"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPriorEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecExchFixed"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPriorEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecKnown"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printPriorEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecMix"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printPriorEqns(object)
          })

#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "Weights"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              mean <- object@priorMeanLevelComponentWeightMix@.Data
              sd <- object@priorSDLevelComponentWeightMix@.Data
              nuComp <- object@nuComponentWeightMix@.Data
              AComp <- object@AComponentWeightMix@.Data
              maxComp <- object@omegaComponentWeightMaxMix@.Data
              nuLevel <- object@nuLevelComponentWeightMix@.Data
              ALevel <- object@ALevelComponentWeightMix@.Data
              maxLevel <- object@omegaLevelComponentWeightMaxMix@.Data
              cat("         mean ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
              cat("       scale1 ~ trunc-half-t(", nuComp, ", ", sep = "")
              cat(squaredOrNA(AComp), ", ", maxComp, ")\n", sep = "")
              cat("       scale2 ~ trunc-half-t(", nuLevel, ", ", sep = "")
              cat(squaredOrNA(ALevel), ", ", maxLevel, ")\n", sep = "")
              phi <- object@phi
              phi.known <- object@phiKnown
              min.phi <- object@minPhi
              max.phi <- object@maxPhi
              shape1 <- object@shape1Phi@.Data
              shape2 <- object@shape2Phi@.Data
              if (phi.known)
                  cat("         damp =",
                      format(phi, digits = 4),
                      "\n")
              else {
                  cat("dampTransform = (damp-",
                      format(min.phi, digits = 4),
                      ")/(",
                      format(max.phi, digits = 4),
                      "-",
                      format(min.phi, digits = 4),
                      ")\n",
                      sep = "")
                  cat("dampTransform ~ Beta(",
                      format(shape1, digits = 4),
                      ",",
                      format(shape2, digits = 4),
                      ")\n",
                      sep = "")
              }
          })






#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecZero"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n", sep = "")
              printPriorEqns(object)
          })
