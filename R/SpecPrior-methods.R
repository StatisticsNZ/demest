
## ## SpecUnknownTau

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecUnknownTau",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               stop(gettextf("priors must have known variance when used in function '%s'",
##                             "fakeData"))
##           })


## ## SpecCovariates

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecCovariates",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               stop(gettextf("priors must not have covariates when used in function '%s'",
##                             "fakeData"))
##           })


## ## Exchangeable Normal

## ## HAS_TESTS
## ## Include to avoid ambiguous inheritance from UnknownTau and Covariates
## setMethod("fakeBeta",
##           signature(object = "SpecExchNormCovUnknown",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               stop(gettextf("priors must not have covariates when used in function '%s'",
##                             "fakeData"))
##           })


## ## Exchangeable Robust

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecExchRobustZeroKnown",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               tau <- object@tau
##               nu <- object@nu
##               dim <- dim(metadata)
##               n <- prod(dim)
##               ans <- numeric(n)
##               for (i in seq_len(n)) {
##                   U <- rinvchisq1(df = nu, scale = tau^2)
##                   ans[i] <- stats::rnorm(n = 1L, mean = 0, sd = sqrt(U))
##               }
##               ans <- array(ans, dim = dim)
##               ans <- sweepAllMargins(ans)
##               as.double(ans)
##           })

## ## Include to avoid ambiguous inheritance from UnknownTau and Covariates
## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecExchRobustCovUnknown",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               stop(gettextf("priors must not have covariates when used in function '%s'",
##                             "fakeData"))
##           })


## ## Uniform

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecUniform",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               stop(gettextf("uniform prior may not be used in function '%s'",
##                             "fakeData"))
##           })

## ## Known

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecKnownCertain",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               values <- object@values
##               metadata.prior <- object@metadata
##               .Data.prior <- array(values,
##                                     dim = dim(metadata.prior),
##                                     dimnames = dimnames(metadata.prior))
##               values <- methods::new("Values", .Data = .Data.prior, metadata = metadata.prior)
##               .Data.beta <- array(0,
##                                   dim = dim(metadata),
##                                   dimnames = dimnames(metadata))
##               beta.tmp <- methods::new("Values", .Data = .Data.beta, metadata = metadata)
##               values <- tryCatch(dembase::makeCompatible(x = values,
##                                                 y = beta.tmp,
##                                                 subset = TRUE),
##                                  error = function(e) e)
##               if (methods::is(values, "error"))
##                   stop(gettextf("prior incompatible with '%s' : %s",
##                                 "y", values$message))
##               values <- as.double(values)
##               dim <- dim(metadata)
##               ans <- array(values, dim = dim)
##               ans <- sweepAllMargins(ans)
##               as.double(ans)
##           })

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecKnownUncertain",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               mean <- object@mean
##               sd <- object@sd
##               metadata.prior <- object@metadata
##               .Data.mean <- array(mean,
##                                   dim = dim(metadata.prior),
##                                   dimnames = dimnames(metadata.prior))
##               mean <- methods::new("Values", .Data = .Data.mean, metadata = metadata.prior)
##               .Data.sd <- array(sd,
##                                 dim = dim(metadata.prior),
##                                 dimnames = dimnames(metadata.prior))
##               sd <- methods::new("Values", .Data = .Data.sd, metadata = metadata.prior)
##               .Data.beta <- array(0,
##                                   dim = dim(metadata),
##                                   dimnames = dimnames(metadata))
##               beta.tmp <- methods::new("Values", .Data = .Data.beta, metadata = metadata)
##               mean <- tryCatch(dembase::makeCompatible(x = mean, y = beta.tmp, subset = TRUE),
##                                error = function(e) e)
##               if (methods::is(mean, "error"))
##                   stop(gettextf("prior incompatible with '%s' : %s",
##                                 "y", mean$message))
##               sd <- dembase::makeCompatible(x = sd, y = beta.tmp, subset = TRUE)
##               mean <- as.numeric(mean)
##               sd <- as.numeric(sd)
##               ans <- stats::rnorm(n = length(mean), mean = mean, sd = sd)
##               ans <- array(ans, dim = dim(metadata))
##               ans <- sweepAllMargins(ans)
##               as.double(ans)
##           })

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecPoly",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               trend <- object@trend
##               covariates <- object@covariates
##               seasonal <- object@seasonal
##               priorV <- object@priorV
##               J <- dim(metadata)
##               if (!is.null(covariates))
##                   stop(gettextf("priors must not have covariates when used in function '%s'",
##                                 "fakeData"))
##               ans <- fakeBeta(object = trend, metadata = metadata)
##               if (!is.null(seasonal))
##                   ans <- ans + fakeBeta(object = seasonal, metadata = metadata)
##               ans <- ans + fakeDLMErrors(spec = priorV, J = J)
##               ans - mean(ans)
##           })

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecPolyComponentTrend",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               q <- object@q
##               priorsW <- object@priorsW
##               m0 <- object@m0
##               C0 <- object@C0
##               J <- dim(metadata)
##               G <- diag(nrow = q)
##               G[row(G) == col(G) - 1L] <- 1L
##               W <- matrix(nrow = q, ncol = J)
##               for (i in seq_len(q))
##                   W[i, ] <- fakeDLMErrors(spec = priorsW[[i]], J = J)
##               gamma <- matrix(nrow = q, ncol = J + 1L)
##               gamma[ , 1L] <- rmvnorm1(mean = m0, var = C0, useC = TRUE)
##               for (j in seq_len(J))
##                   gamma[ , j + 1L] <- G %*% gamma[ , j] + W[ , j]
##               ans <- gamma[1L, -1L]
##               ans <- ans - mean(ans)
##               ans
##           })

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecPolyComponentSeasonal",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               q <- object@q
##               priorsW <- object@priorsW
##               m0 <- object@m0
##               C0 <- object@C0
##               J <- dim(metadata)
##               G <- matrix(0, nrow = q, ncol = q)
##               G[1, ] <- 1
##               G[row(G) == col(G) + 1L] <- -1
##               W <- matrix(nrow = q, ncol = J)
##               for (i in seq_len(q))
##                   W[i, ] <- fakeDLMErrors(spec = priorsW[[i]], J = J)
##               gamma <- matrix(nrow = q, ncol = J + 1L)
##               gamma[ , 1L] <- rmvnorm1(mean = m0, var = C0, useC = TRUE)
##               for (j in seq_len(J))
##                   gamma[ , j + 1L] <- G %*% gamma[ , j] + W[ , j]
##               ans <- gamma[1L, -1L]
##               ans <- ans - mean(ans)
##               ans
##           })


## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecAR10",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               phi <- object@phi
##               m0 <- object@m0
##               C0 <- object@C0
##               priorV <- object@priorV
##               priorW <- object@priorW
##               along <- object@along
##               if (is.null(phi))
##                   stop(gettextf("AR1 prior must have value for '%s' when used in function '%s'",
##                                 "coef", "fakeData"))
##               if (!(abs(phi) < 1))
##                   stop(gettextf("'%s' must have absolute value less than 1 when AR1 prior used in function '%s'",
##                                 "coef", "fakeData"))
##               along <- dembase::checkAndTidyAlong(along = along,
##                                          metadata = metadata,
##                                          numericDimScales = FALSE)
##               dim.beta <- dim(metadata)
##               J <- as.integer(prod(dim.beta))
##               K <- dim.beta[along]
##               L <- J %/% K
##               dim.gamma <- replace(dim.beta, list = along, values = K + 1L)
##               gamma <- array(dim = dim.gamma)
##               indices <- slice.index(x = gamma, MARGIN = along)
##               gamma[indices == 1L] <- stats::rnorm(n = L, mean = m0, sd = sqrt(C0))
##               for (k in seq_len(K)) {
##                   gamma.hat <- phi * gamma[indices == k]
##                   errors.state <- fakeDLMErrors(spec = priorW, J = L)
##                   gamma[indices == k + 1L] <- gamma.hat + errors.state
##               }
##               gamma.no.initial <- gamma[indices != 1L]
##               errors.obs <- fakeDLMErrors(spec = priorV, J = J)
##               ans <- gamma.no.initial + errors.obs
##               ans <- array(ans, dim = dim.beta)
##               ans <- sweepAllMargins(ans)
##               as.double(ans)
##           })

## ## HAS_TESTS
## setMethod("fakeBeta",
##           signature(object = "SpecAR11",
##                     metadata = "MetaData"),
##           function(object, metadata) {
##               phi <- object@phi
##               m0 <- object@m0
##               C0 <- object@C0
##               priorV <- object@priorV
##               priorW <- object@priorW
##               along <- object@along
##               if (is.null(phi))
##                   stop(gettextf("AR1 prior must have value for '%s' when used in function '%s'",
##                                 "coef", "fakeData"))
##               if (!(abs(phi) < 1))
##                   stop(gettextf("'%s' must have absolute value less than 1 when AR1 prior used in function '%s'",
##                                 "coef", "fakeData"))
##               along <- dembase::checkAndTidyAlong(along = along,
##                                          metadata = metadata,
##                                          numericDimScales = FALSE)
##               dim.beta <- dim(metadata)
##               J <- as.integer(prod(dim.beta))
##               K <- dim.beta[along]
##               L <- J %/% K
##               dim.gamma.delta <- replace(dim.beta, list = along, values = K + 1L)
##               gamma <- array(dim = dim.gamma.delta)
##               delta <- array(dim = dim.gamma.delta)
##               indices <- slice.index(x = gamma, MARGIN = along)
##               for (l in seq_len(L)) {
##                   initial <- rmvnorm2(mean = m0, var = C0)
##                   gamma[indices == 1L][l] <- initial[1L]
##                   delta[indices == 1L][l] <- initial[2L]
##               }
##               for (k in seq_len(K)) {
##                   delta.hat <- phi * delta[indices == k]
##                   errors.state <- fakeDLMErrors(spec = priorW, J = L)
##                   delta[indices == k + 1L] <- delta.hat + errors.state
##                   gamma[indices == k + 1L] <- (gamma[indices == k] +
##                                                    delta[indices == k])
##               }
##               gamma.no.initial <- gamma[indices != 1L]
##               errors.obs <- fakeDLMErrors(spec = priorV, J = J)
##               ans <- gamma.no.initial + errors.obs
##               ans <- array(ans, dim = dim.beta)
##               ans <- sweepAllMargins(ans)
##               as.double(ans)
##           })



## ## fakeDLMErrors ###################################################################

## ## HAS_TESTS
## setMethod("fakeDLMErrors",
##           signature(spec = "SpecPriorVarDLMNormKnown",
##                     J = "integer"),
##           function(spec, J) {
##               tau <- spec@tau
##               stats::rnorm(n = J, mean = 0, sd = tau)
##           })

## ## HAS_TESTS
## setMethod("fakeDLMErrors",
##           signature(spec = "SpecPriorVarDLMNormUnknown",
##                     J = "integer"),
##           function(spec, J) {
##               stop(gettextf("priors with unknown variance terms are not permitted when used in function '%s'",
##                             "fakeData"))
##           })

## ## HAS_TESTS
## setMethod("fakeDLMErrors",
##           signature(spec = "SpecPriorVarDLMRobustKnown",
##                     J = "integer"),
##           function(spec, J) {
##               nu <- spec@nu
##               tau <- spec@tau
##               v <- nu * tau^2 / rchisq(n = J, df = nu)
##               stats::rnorm(n = J, mean = 0, sd = sqrt(v))
##           })

## ## HAS_TESTS
## setMethod("fakeDLMErrors",
##           signature(spec = "SpecPriorVarDLMRobustUnknown",
##                     J = "integer"),
##           function(spec, J) {
##               stop(gettextf("priors with unknown variance terms are not permitted when used in function '%s'",
##                             "fakeData"))
##           })

## ## HAS_TESTS
## setMethod("fakeDLMErrors",
##           signature(spec = "SpecPriorVarDLMZero",
##                     J = "integer"),
##           function(spec, J) {
##               rep(0, times = J)
##           })


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
