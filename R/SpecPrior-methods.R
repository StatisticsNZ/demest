

## checkPriorIsInformative ###########################################################

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecExchFixed"),
          function(object) {
              checkPriorInform_ExchFixed(object)
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecExchNormZero"),
          function(object) {
              value <- checkPriorInform_Error(object)
              if (!is.null(value))
                  return(gettextf("%s in call to '%s'",
                                  value, "Exch"))
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecExchRobustZero"),
          function(object) {
              value <- checkPriorInform_Error(object)
              if (!is.null(value))
                  return(gettextf("%s in call to '%s'",
                                  value, "Exch"))
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecExchNormCov"),
          function(object) {
              value.covariates <- checkPriorInform_Covariates(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.covariates, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "Exch"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecExchRobustCov"),
          function(object) {
              value.covariates <- checkPriorInform_Covariates(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.covariates, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "Exch"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMNoTrendNormZeroNoSeason"),
          function(object) {
              value.level <- checkPriorInform_Level(object)              
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMWithTrendNormZeroNoSeason"),
          function(object) {
              has.level <- object@hasLevel@.Data
              if (has.level)
                  value.level <- checkPriorInform_Level(object)
              else
                  value.level <- NULL
              value.trend <- checkPriorInform_Trend(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.trend, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMNoTrendNormZeroWithSeason"),
          function(object) {
              value.level <- checkPriorInform_Level(object)
              value.season <- checkPriorInform_Season(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.season, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMWithTrendNormZeroWithSeason"),
          function(object) {
              has.level <- object@hasLevel@.Data
              if (has.level)
                  value.level <- checkPriorInform_Level(object)
              else
                  value.level <- NULL
              value.trend <- checkPriorInform_Trend(object)
              value.season <- checkPriorInform_Season(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.trend,
                                 value.season, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMNoTrendNormCovNoSeason"),
          function(object) {
              value.level <- checkPriorInform_Level(object)              
              value.covariates <- checkPriorInform_Covariates(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.covariates, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMWithTrendNormCovNoSeason"),
          function(object) {
              has.level <- object@hasLevel@.Data
              if (has.level)
                  value.level <- checkPriorInform_Level(object)
              else
                  value.level <- NULL
              value.trend <- checkPriorInform_Trend(object)
              value.covariates <- checkPriorInform_Covariates(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.trend,
                                 value.covariates, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMNoTrendNormCovWithSeason"),
          function(object) {
              value.level <- checkPriorInform_Level(object)
              value.covariates <- checkPriorInform_Covariates(object)
              value.season <- checkPriorInform_Season(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.season,
                                 value.covariates, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMWithTrendNormCovWithSeason"),
          function(object) {
              has.level <- object@hasLevel@.Data
              if (has.level)
                  value.level <- checkPriorInform_Level(object)
              else
                  value.level <- NULL
              value.trend <- checkPriorInform_Trend(object)
              value.covariates <- checkPriorInform_Covariates(object)
              value.season <- checkPriorInform_Season(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.trend, value.covariates,
                                 value.season, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

















## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMNoTrendRobustZeroNoSeason"),
          function(object) {
              value.level <- checkPriorInform_Level(object)              
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMWithTrendRobustZeroNoSeason"),
          function(object) {
              has.level <- object@hasLevel@.Data
              if (has.level)
                  value.level <- checkPriorInform_Level(object)
              else
                  value.level <- NULL
              value.trend <- checkPriorInform_Trend(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.trend, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMNoTrendRobustZeroWithSeason"),
          function(object) {
              value.level <- checkPriorInform_Level(object)
              value.season <- checkPriorInform_Season(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.season, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMWithTrendRobustZeroWithSeason"),
          function(object) {
              has.level <- object@hasLevel@.Data
              if (has.level)
                  value.level <- checkPriorInform_Level(object)
              else
                  value.level <- NULL
              value.trend <- checkPriorInform_Trend(object)
              value.season <- checkPriorInform_Season(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.trend,
                                 value.season, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMNoTrendRobustCovNoSeason"),
          function(object) {
              value.level <- checkPriorInform_Level(object)              
              value.covariates <- checkPriorInform_Covariates(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.covariates, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMWithTrendRobustCovNoSeason"),
          function(object) {
              has.level <- object@hasLevel@.Data
              if (has.level)
                  value.level <- checkPriorInform_Level(object)
              else
                  value.level <- NULL
              value.trend <- checkPriorInform_Trend(object)
              value.covariates <- checkPriorInform_Covariates(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.trend,
                                 value.covariates, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMNoTrendRobustCovWithSeason"),
          function(object) {
              value.level <- checkPriorInform_Level(object)
              value.covariates <- checkPriorInform_Covariates(object)
              value.season <- checkPriorInform_Season(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.season,
                                 value.covariates, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecDLMWithTrendRobustCovWithSeason"),
          function(object) {
              has.level <- object@hasLevel@.Data
              if (has.level)
                  value.level <- checkPriorInform_Level(object)
              else
                  value.level <- NULL
              value.trend <- checkPriorInform_Trend(object)
              value.covariates <- checkPriorInform_Covariates(object)
              value.season <- checkPriorInform_Season(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.level, value.trend, value.covariates,
                                 value.season, value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "DLM"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecKnown"),
          function(object) {
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecMixNormZero"),
          function(object) {
              value.components <- checkPriorInform_Components(object)
              value.weights <- checkPriorInform_Weights(object)
              value.error <- checkPriorInform_Error(object)
              for (value in list(value.components, value.weights,
                                 value.error)) {
                  if (!is.null(value))
                      return(gettextf("%s in call to '%s'",
                                      value, "Mix"))
              }
              NULL
          })

## HAS_TESTS
setMethod("checkPriorIsInformative",
          signature(object = "SpecZero"),
          function(object) {
              NULL
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
          signature(object = "InvChiSq"),
          function(object) {
              nu <- object@nu
              A <- object@A
              max <- object@scaleMax
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("trunc-inv-chi-sq(", nu, ", ", sep = "")
              cat(A, ", ", max, ")\n", sep = "")
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
