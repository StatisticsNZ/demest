
## Code that was translated into C that is no longer used


## DELETED JANUARY 2017 ##########################################

## TRANSLATED
## HAS_TESTS
betaExchZero <- function(betaScaled, prior, useC = FALSE) {
    ## betaScaled
    stopifnot(is.double(betaScaled))
    stopifnot(!any(is.na(betaScaled)))
    ## prior
    stopifnot(methods::is(prior, "Exch"))
    stopifnot(methods::is(prior, "ZeroMixin"))
    stopifnot(methods::validObject(prior))
    ## betaScaled and prior
    stopifnot(identical(length(betaScaled), as.integer(prior@J)))
    if (useC) {
        .Call(betaExchZero_R, betaScaled, prior)
    }
    else {
        A <- prior@ATau@.Data
        zeta <- prior@zeta
        A * zeta * betaScaled
    }
}


## TRANSLATED
## HAS_TESTS
updateBetaScaled <- function(prior, vbar, n, sigma, useC = FALSE) {
    checkUpdateBetaAndPriorBeta(prior = prior,
                                vbar = vbar,
                                n = n,
                                sigma = sigma)
    stopifnot(methods::is(prior, "Exch"))
    stopifnot(methods::is(prior, "ZeroMixin"))
    if (useC) {
        .Call(updateBetaScaled_R, prior, vbar, n, sigma)
    }
    else {
        J <- prior@J@.Data
        A <- prior@ATau@.Data
        zeta <- prior@zeta
        K <- vbar / (A * zeta)
        v <- getV(prior)
        prec.data <- (n * A^2 * zeta^2) / sigma^2
        prec.prior <- 1 / v
        var <- 1 / (prec.data + prec.prior)
        mean <- prec.data * K * var
        sd <- sqrt(var)
        stats::rnorm(n = J, mean = mean, sd = sd)
    }
}


## TRANSLATED
## HAS_TESTS
updateZetaAndTau <- function(prior, betaScaled, vbar, n, sigma, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Exch"))
    stopifnot(methods::is(prior, "ZeroMixin"))
    stopifnot(methods::validObject(prior))
    ## vbar
    stopifnot(is.double(vbar))
    stopifnot(identical(length(vbar), as.integer(prior@J)))
    stopifnot(!any(is.na(vbar)))
    ## n
    stopifnot(is.integer(n))
    stopifnot(identical(length(n), 1L))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## sigma
    stopifnot(is.double(sigma))
    stopifnot(identical(length(sigma), 1L))
    stopifnot(!is.na(sigma))
    stopifnot(sigma > 0)
    if (useC) {
        .Call(updateZetaAndTau_R, prior, betaScaled, vbar, n, sigma)
    }
    else {
        J <- prior@J@.Data
        A <- prior@ATau@.Data
        nu <- prior@nuTau@.Data
        tau.max <- prior@tauMax@.Data
        L <- sum(vbar / betaScaled) / (J * A)
        prec.data <- (n * J^2 * A^2)  / (sigma^2 * sum(1/betaScaled^2))
        var <- 1 / (prec.data + 1)
        mean <- prec.data * L * var
        sd <- sqrt(var)
        zeta <- stats::rnorm(n = 1L, mean = mean, sd = sd)
        df <- nu + J
        scale <- (nu + sum(betaScaled^2)) / df
        tau.sq.scaled <- rinvchisq1(df = df, scale = scale)
        tau.scaled <- sqrt(tau.sq.scaled)
        tau <- A * abs(zeta) * tau.scaled
        if (tau < tau.max) {
            prior@zeta <- zeta
            prior@tauScaled@.Data <- tau.scaled
            prior@tau@.Data <- tau
        }
        prior
    }
}



## I have done a new version of this (the new version is pretty trivial) but did
## not want to delete the old one - JB
## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "ExchNormZero"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_ExchNormZero_R, prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
              }
              else {
                  beta.scaled <- updateBetaScaled(prior = prior,
                                                  vbar = vbar,
                                                  n = n,
                                                  sigma = sigma)
                  prior <- updateZetaAndTau(prior = prior,
                                            betaScaled = beta.scaled,
                                            vbar = vbar,
                                            n = n,
                                            sigma = sigma)
                  beta <- betaExchZero(betaScaled = beta.scaled,
                                       prior = prior)
                  list(beta, prior)
              }
          })


## Model-methods ####################################################

## TRANSLATED ## removed iMethodModel number from C switch
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
setMethod("logLikelihood",
          signature(model = "BinomialFixed",
                    count = "integer",
                    dataset = "Counts",
                    i = "integer"),
          function(model, count, dataset, i, useC = FALSE, useSpecific = FALSE) {
              ## count
              stopifnot(identical(length(count), 1L))
              stopifnot(!is.na(count))
              stopifnot(count >= 0)
              ## dataset
              stopifnot(is.integer(dataset))
              stopifnot(all(dataset[!is.na(dataset)] >= 0))
              ## i
              stopifnot(identical(length(i), 1L))
              stopifnot(!is.na(i))
              stopifnot(i >= 1L)
              ## dataset and i
              stopifnot(i <= length(dataset))
              stopifnot(!is.na(dataset@.Data[i]))
              ## model and dataset
              stopifnot(identical(length(model@theta), length(dataset)))
              ## model and i
              stopifnot(i <= length(model@theta))
              ## dataset and i
              if (useC) {
                  if (useSpecific)
                      .Call(logLikelihood_Binomial_R, model, count, dataset, i)
                  else
                      .Call(logLikelihood_R, model, count, dataset, i)
              }
              else {
                  logLikelihood_Binomial(model = model,
                                         count = count,
                                         dataset = dataset,
                                         i = i)
              }
          })

## TRANSLATED ## removed iMethodModel number from C switch
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
setMethod("logLikelihood",
          signature(model = "PoissonFixedUseExp",
                    count = "integer",
                    dataset = "Counts",
                    i = "integer"),
          function(model, count, dataset, i, useC = FALSE, useSpecific = FALSE) {
              ## count
              stopifnot(identical(length(count), 1L))
              stopifnot(!is.na(count))
              stopifnot(count >= 0)
              ## dataset
              stopifnot(all(dataset[!is.na(dataset)] >= 0))
              ## i
              stopifnot(identical(length(i), 1L))
              stopifnot(!is.na(i))
              stopifnot(i >= 1L)
              ## dataset and i
              stopifnot(i <= length(dataset))
              stopifnot(!is.na(dataset@.Data[i]))
              ## model and dataset
              stopifnot(identical(length(model@theta), length(dataset)))
              ## model and i
              stopifnot(i <= length(model@theta))
              if (useC) {
                  if (useSpecific)
                      .Call(logLikelihood_Poisson_R, model, count, dataset, i)
                  else
                      .Call(logLikelihood_R, model, count, dataset, i)
              }
              else {
                  logLikelihood_Poisson(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i)
              }
          })


## TRANSLATED ## removed from C
## HAS_TESTS
setMethod("predictModelNotUseExp",
          signature(object = "NormalFixedVarsigmaKnownPredict",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelNotUseExp_NormalFixedVarsigmaKnownPredict_R, object, y)
                  else
                      .Call(predictModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_NormalFixed(object, y = y)
                  object
              }
          })


## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("predictModelNotUseExp",
          signature(object = "NormalFixedVarsigmaUnknownPredict",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelNotUseExp_NormalFixedVarsigmaUnknownPredict_R, object, y)
                  else
                      .Call(predictModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_NormalFixed(object, y = y)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictModelNotUseExp",
          signature(object = "PoissonFixedNotUseExpPredict",
                    y = "Counts"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelNotUseExp_PoissonFixedNotUseExpPredict_R,
                            object, y)
                  else
                      .Call(predictModelNotUseExp_R,
                            object, y)
              }
              else {
                  updateTheta_PoissonFixedNotUseExp(object, y = y)
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("predictModelUseExp",
          signature(object = "BinomialFixedPredict",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelUseExp_BinomialFixedPredict_R,
                            object, y, exposure)
                  else
                      .Call(predictModelUseExp_R,
                            object, y, exposure)
              }
              else {
                  updateTheta_BinomialFixed(object, y = y, exposure = exposure)
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("predictModelUseExp",
          signature(object = "PoissonFixedUseExpPredict",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(all(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelUseExp_PoissonFixedUseExpPredict_R,
                            object, y, exposure)
                  else
                      .Call(predictModelUseExp_R,
                            object, y, exposure)
              }
              else {
                  updateTheta_PoissonFixedUseExp(object, y = y, exposure = exposure)
              }
          })
## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "NormalFixedVarsigmaKnownPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_NormalFixedVarsigmaKnownPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  transferParamSigma(model = model,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = iteration)
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "NormalFixedVarsigmaUnknownPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_NormalFixedVarsigmaUnknownPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  transferParamVarsigma(model = model,
                                        filename = filename,
                                        lengthIter = lengthIter,
                                        iteration = iteration)
                  transferParamSigma(model = model,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = iteration)
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "PoissonFixedNotUseExpPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_PoissonFixedNotUseExpPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  transferParamSigma(model = model,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = iteration)
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "BinomialFixedPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_BinomialFixedPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  transferParamSigma(model = model,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = iteration)
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "PoissonFixedUseExpPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_PoissonFixedUseExpPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  transferParamSigma(model = model,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = iteration)
              }
          })


## TRANSLATED ## removed from C code
## HAS_TESTS
## Function should never be passed an exposure argument.
setMethod("updateModelNotUseExp",
          signature(object = "NormalFixedVarsigmaKnown",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalFixedVarsigmaKnown_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_NormalFixed(object, y = y)
                  object <- updateSigma_NormalFixed(object)
                  object
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
## Function should never be passed an exposure argument.
setMethod("updateModelNotUseExp",
          signature(object = "NormalFixedVarsigmaUnknown",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalFixedVarsigmaUnknown_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_NormalFixed(object = object, y = y)
                  object <- updateVarsigma(object = object, y = y)
                  object <- updateSigma_NormalFixed(object)
                  object
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
## Function should never be passed an exposure argument.
setMethod("updateModelNotUseExp",
          signature(object = "PoissonFixedNotUseExp",
                    y = "Counts"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y[!is.na(y)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_PoissonFixedNotUseExp_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonFixedNotUseExp(object, y = y)
                  object <- updateSigma_PoissonFixed(object)
                  object
              }
          })


## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "BinomialFixed",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0))
              ## exposure
              stopifnot(!any(is.na(exposure)))
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y[!is.na(y)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_BinomialFixed_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_BinomialFixed(object, y = y, exposure = exposure)
                  object <- updateSigma_BinomialFixed(object)
                  object
              }
          })

## TRANSLATED ## removed from C code
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "PoissonFixedUseExp",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y[!is.na(y)][exposure[!is.na(y)] == 0] == 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_PoissonFixedUseExp_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonFixedUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_PoissonFixed(object)
                  object
              }
          })



## update-nongeneric ####################################################

## TRANSLATED ## removed from C code
## HAS_TESTS
updateDeltaHatAR11 <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "AR11"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateDeltaHatAR11_R, prior)
    }
    else {
        K <- prior@K
        L <- prior@L
        delta <- prior@delta
        delta.hat <- prior@deltaHat
        iter.delta <- prior@iteratorGamma
        iter.delta.hat <- prior@iteratorV
        phi <- prior@phi
        iter.delta <- resetA(iter.delta)
        iter.delta.hat <- resetA(iter.delta.hat)
        for (l in seq_len(L)) {
            indices.delta <- iter.delta@indices
            indices.delta.hat <- iter.delta.hat@indices
            for (k in seq_len(K)) {
                i <- indices.delta[k]
                ih <- indices.delta.hat[k]
                delta.hat[ih] <- phi * delta[i]
            }
            iter.delta <- advanceA(iter.delta)
            iter.delta.hat <- advanceA(iter.delta.hat)
        }
        prior@deltaHat <- delta.hat
        prior
    }
}

## TRANSLATED ## removed from R code
## HAS_TESTS
updateDeltaNoInitialAR11 <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "AR11"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateDeltaNoInitialAR11_R, prior)
    }
    else {
        K <- prior@K
        L <- prior@L
        delta.has <- prior@delta
        delta.not <- prior@deltaNoInitial
        iter.has <- prior@iteratorGamma
        iter.not <- prior@iteratorV
        iter.has <- resetA(iter.has)
        iter.not <- resetA(iter.not)
        for (l in seq_len(L)) {
            indices.has <- iter.has@indices
            indices.not <- iter.not@indices
            for (k in seq_len(K)) {
                i.has <- indices.has[k + 1L]
                i.not <- indices.not[k]
                delta.not[i.not] <- delta.has[i.has]
            }
            iter.has <- advanceA(iter.has)
            iter.not <- advanceA(iter.not)
        }
        prior@deltaNoInitial <- delta.not
        prior
    }
}


## TRANSLATED ## removed from C code
## HAS_TESTS
updateEtaNorm <- function(prior, beta, useC = FALSE) {
    validObject(prior)
    stopifnot(is(prior, "Norm"))
    stopifnot(is(prior, "Covariates"))
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    Z <- prior@Z
    stopifnot(identical(length(beta), nrow(Z)))
    if (useC) {
        .Call(updateEtaNorm_R, prior, beta)
    }
    else {
        Z <- prior@Z
        tau <- prior@tau
        QR <- qr(Z)
        R <- qr.R(QR)
        eta.hat <- qr.coef(QR, beta)
        P <- length(eta.hat)
        g <- rnorm(n = P)
        eta <- eta.hat + tau * backsolve(R, g)
        prior@eta <- eta
        prior
    }
}

## TRANSLATED # removed from C code
## HAS_TESTS
updateEtaRobust <- function(prior, beta, useC = FALSE) {
    validObject(prior)
    stopifnot(is(prior, "Robust"))
    stopifnot(is(prior, "Covariates"))
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    Z <- prior@Z
    stopifnot(identical(length(beta), nrow(Z)))
    if (useC) {
        .Call(updateEtaRobust_R, prior, beta)
    }
    else {
        Z <- prior@Z
        U <- prior@U
        D <- diag(1 / sqrt(U))
        Z.scaled <- D %*% Z
        beta.scaled <- drop(D %*% beta)
        QR <- qr(Z.scaled)
        R <- qr.R(QR)
        eta.hat <- qr.coef(QR, beta.scaled)
        P <- length(eta.hat)
        g <- rnorm(n = P)
        eta <- eta.hat + backsolve(R, g)
        prior@eta <- eta
        prior
    }
}

## ## TRANSLATED ## removed C code
## ## HAS_TESTS
## updateFinalGamma <- function(object, useC = FALSE) {
##     stopifnot(is(object, "PolyComponent"))
##     stopifnot(validObject(object))
##     if (useC) {
##         .Call(updateFinalGamma_R, object)
##     }
##     else {
##         J <- object@J
##         gamma <- object@gamma
##         m <- object@m
##         C <- object@CC
##         mean <- m[[J + 1L]]
##         var <- C[[J + 1L]]
##         gamma[[J + 1L]] <- rmvnorm1(mean = mean, var = var)
##         object@gamma <- gamma
##         object
##     }
## }

## TRANSLATED ## removed C code
## HAS_TESTS
updateFinalGamma <- function(object, forward = TRUE, useC = FALSE) {
    ## object
    stopifnot(is(object, "PolyComponent"))
    stopifnot(validObject(object))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    if (useC) {
        .Call(updateFinalGamma_R, object, forward)
    }
    else {
        J <- object@J
        q <- object@q
        m <- object@m
        UC <- object@UC
        DC <- object@DC
        z <- rnorm(n = q)
        j <- if (forward) J + 1L else 1L
        sqrt.C <- UC[[j]] %*% diag(DC[[j]], nrow = q)
        object@gamma[[j]] <- m[[j]] + drop(sqrt.C %*% z)
        object
    }
}

## TRANSLATED ## removed C function
## HAS_TESTS
updateGAR11 <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "AR11"))
     if (useC) {
        .Call(updateGAR11_R, prior)
    }
    else {
        phi <- prior@phi
        G <- prior@G
        G[4L] <- phi
        prior@G <- G
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateGammaHatAR10 <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "AR1"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateGammaHatAR10_R, prior)
    }
    else {
        K <- prior@K
        L <- prior@L
        gamma <- prior@gamma
        gamma.hat <- prior@gammaHat
        iter.gamma <- prior@iteratorGamma
        iter.gamma.hat <- prior@iteratorV
        phi <- prior@phi
        iter.gamma <- resetA(iter.gamma)
        iter.gamma.hat <- resetA(iter.gamma.hat)
        for (l in seq_len(L)) {
            indices.gamma <- iter.gamma@indices
            indices.gamma.hat <- iter.gamma.hat@indices
            for (k in seq_len(K)) {
                i <- indices.gamma[k]
                ih <- indices.gamma.hat[k]
                gamma.hat[ih] <- phi * gamma[i]
            }
            iter.gamma <- advanceA(iter.gamma)
            iter.gamma.hat <- advanceA(iter.gamma.hat)
        }
        prior@gammaHat <- gamma.hat
        prior
    }
}

## TRANSLATED ## C function deleted
## HAS_TESTS
updateGammaNorm <- function(prior, beta, useC = FALSE) {
    validObject(prior)
    stopifnot(is(prior, "Norm"))
    stopifnot(is(prior, "Zero"))
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    if (useC) {
        .Call(updateGammaNorm_R, prior, beta)
    }
    else {
        tau <- prior@tau
        J <- length(beta)
        mean <- mean(beta)
        sd <- tau / sqrt(J)
        gamma <- rnorm(n = 1L, mean = mean, sd = sd)
        prior@gamma <- gamma
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateGammaDeltaAR11 <- function(prior, beta, forward, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR11"))
    stopifnot(validObject(prior))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(!any(is.na(beta)))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    if (useC) {
        .Call(updateGammaDeltaAR11_R, prior, beta, forward)
    }
    else {    
        K <- prior@K
        L <- prior@L
        G <- prior@G  ## 2x2 matrix
        gamma <- prior@gamma  ## numeric length K+1
        delta <- prior@delta  ## numeric length K+1
        phi <- prior@phi  ## numeric length 1
        v <- prior@v  ## numeric length KL
        w <- prior@w  ## numeric length KL
        m <- prior@m  ## list length K+1
        UC <- prior@UC  ## list length K+1
        DC <- prior@DC  ## list length K+1
        DC.inv <- prior@DCInv
        a <- prior@a  ## list length K
        iter.gamma <- prior@iteratorGamma
        iter.v <- prior@iteratorV
        iter.gamma <- resetA(iter.gamma)
        iter.v <- resetA(iter.v)
        for (l in seq_len(L)) {
            indices.gamma <- iter.gamma@indices
            indices.v <- iter.v@indices
            ## forward filter
            for (k in seq_len(K)) {
                if (forward) {
                    k0 <- k
                    k1 <- k
                    k2 <- k + 1L
                }
                else {
                    k0 <- K - k + 2L
                    k1 <- K - k + 1L
                    k2 <- K - k + 1L
                }
                iv <- indices.v[k1]
                v.inv <- 1 / v[iv]
                if (is.infinite(v.inv))
                    v.inv <- 0
                M.R <- rbind(diag(DC[[k0]], nrow = 2L) %*% t(UC[[k0]]) %*% t(G),
                             sqrt(w[iv]))
                svd.R <- svd(M.R, nu = 0)
                UR <- svd.R$v
                DR.inv <- 1 / svd.R$d
                DR.inv[is.infinite(DR.inv)] <- 0
                M.C <- rbind(sqrt(v.inv) * UR[c(1L, 3L)],
                             diag(DR.inv, nrow = 2L))
                svd.C <- svd(M.C, nu = 0)
                UC[[k2]] <- UR %*% svd.C$v
                DC[[k2]] <- 1 / svd.C$d
                DC[[k2]][is.infinite(DC[[k2]])] <- 0
                DC.inv[[k2]] <- svd.C$d
                a[[k1]] <- drop(G %*% m[[k0]])
                e <- beta[iv] - a[[k1]][1L]
                C <- UC[[k2]] %*% diag(DC[[k2]]^2) %*% t(UC[[k2]])
                A <- C[1:2] * v.inv
                m[[k2]] <- a[[k1]] + A * e
            }
            ## draw final gamma, delta
            k <- if (forward) K + 1L else 1L
            ig <- indices.gamma[k]
            sqrt.C <- UC[[k]] %*% diag(DC[[k]], nrow = 2L)
            z <- rnorm(n = 2L)
            gamma.delta <- m[[k]] + drop(sqrt.C %*% z)
            gamma[ig] <- gamma.delta[1L]
            delta[ig] <- gamma.delta[2L]
            ## backward smooth
            for (k in seq.int(from = K, to = 1L)) {
                if (forward) {
                    k0 <- k + 1L
                    k1 <- k
                    k2 <- k
                }
                else {
                    k0 <- K - k + 1L
                    k1 <- K - k + 1L
                    k2 <- K - k + 2L
                }
                iv <- indices.v[k1]
                ig0 <- indices.gamma[k0]
                ig1 <- indices.gamma[k1]
                ig2 <- indices.gamma[k2]
                C.inv <- UC[[k2]] %*% diag(DC.inv[[k2]]^2) %*% t(UC[[k2]])
                var.inv <- phi^2 / w[iv] + C.inv[1L] - 2 * C.inv[2L] + C.inv[4L]
                var <- 1 / var.inv
                mean <- (phi * delta[ig0] / w[iv]
                         + (C.inv[1L] - C.inv[2L]) * (gamma[ig0] - m[[k2]][1L])
                         + (C.inv[4L] - C.inv[2L]) * m[[k2]][2L]) * var
                delta[ig2] <- rnorm(n = 1L, mean = mean, sd = sqrt(var))
                gamma[ig2] <- gamma[ig0] - delta[ig2]
            }
            iter.gamma <- advanceA(iter.gamma)
            iter.v <- advanceA(iter.v)
        }
        prior@gamma <- gamma
        prior@delta <- delta
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateGammaAR10 <- function(prior, beta, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR10"))
    stopifnot(validObject(prior))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(!any(is.na(beta)))
    if (useC) {
        .Call(updateGammaAR10_R, prior, beta)
    }
    else {    
        K <- prior@K
        L <- prior@L
        gamma <- prior@gamma
        m <- prior@m
        C <- prior@CC
        R <- prior@R
        phi <- prior@phi
        v <- prior@v
        w <- prior@w
        iter.gamma <- prior@iteratorGamma
        iter.v <- prior@iteratorV
        iter.gamma <- resetA(iter.gamma)
        iter.v <- resetA(iter.v)
        for (l in seq_len(L)) {
            indices.gamma <- iter.gamma@indices
            indices.v <- iter.v@indices
            ## forward filter
            for (k in seq_len(K)) {
                iv <- indices.v[k]
                R[k] <- phi^2 * C[k] + w[iv]
                A <- v[iv] / (R[k] + v[iv])
                m[k + 1L] <- A * phi * m[k] + (1 - A) * beta[iv]
                C[k + 1L] <- A * R[k]
            }
            ## draw gamma_K
            ig <- indices.gamma[K + 1L]
            gamma[ig] <- rnorm(n = 1L, mean = m[K + 1L], sd = sqrt(C[K + 1L]))
            ## backward smooth
            for (k in seq.int(from = K, to = 1L)) {
                iv <- indices.v[k]
                ig0 <- indices.gamma[k]
                ig1 <- indices.gamma[k + 1L]
                B <- w[iv] / R[k]
                m.star <- B * m[k] + (1 - B) * gamma[ig1] / phi
                C.star <- B * C[k]
                gamma[ig0] <- rnorm(n = 1L, mean = m.star, sd = sqrt(C.star))
            }
            iter.gamma <- advanceA(iter.gamma)
            iter.v <- advanceA(iter.v)
        }
        prior@gamma <- gamma
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateGammaNoInitialAR1 <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "AR1"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateGammaNoInitialAR1_R, prior)
    }
    else {
        K <- prior@K
        L <- prior@L
        gamma.has <- prior@gamma
        gamma.not <- prior@gammaNoInitial
        iter.has <- prior@iteratorGamma
        iter.not <- prior@iteratorV
        iter.has <- resetA(iter.has)
        iter.not <- resetA(iter.not)
        for (l in seq_len(L)) {
            indices.has <- iter.has@indices
            indices.not <- iter.not@indices
            for (k in seq_len(K)) {
                i.has <- indices.has[k + 1L]
                i.not <- indices.not[k]
                gamma.not[i.not] <- gamma.has[i.has]
            }
            iter.has <- advanceA(iter.has)
            iter.not <- advanceA(iter.not)
        }
        prior@gammaNoInitial <- gamma.not
        prior
    }
}



## TRANSLATED ## removed C code
## HAS_TESTS
updateGammaRobust <- function(prior, beta, useC = FALSE) {
    validObject(prior)
    stopifnot(is(prior, "Robust"))
    stopifnot(is(prior, "Zero"))
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    if (useC) {
        .Call(updateGammaRobust_R, prior, beta)
    }
    else {
        U <- prior@U
        J <- length(beta)
        mean <- mean(beta)
        sd <- sqrt(sum(U)) / J
        gamma <- rnorm(n = 1L, mean = mean, sd = sd)
        prior@gamma <- gamma
        prior
    }
}

## no C equivalent
updateM0AR10 <- function(prior, forward, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR10Predict"))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    if (useC) {
        .Call(updateM0Poly_R, component, forward)
    }
    else {
        J <- component@J
        gamma <- component@gamma
        m <- component@m
        if (forward)
            m[[1L]] <- gamma[[1L]]
        else
            m[[J + 1L]] <- gamma[[J + 1L]]
        component@m <- m
        component
    }
}


## TRANSLATED ## removed C code
## HAS_TESTS
updateM0Poly <- function(component, forward, useC = FALSE) {
    ## component
    validObject(component)
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    if (useC) {
        .Call(updateM0Poly_R, component, forward)
    }
    else {
        J <- component@J
        gamma <- component@gamma
        m <- component@m
        if (forward)
            m[[1L]] <- gamma[[1L]]
        else
            m[[J + 1L]] <- gamma[[J + 1L]]
        component@m <- m
        component
    }
}

## changed name, and no longer use zeta
## TRANSLATED ## removed C code
## HAS_TESTS
updatePhiAR10 <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "AR10"))
    stopifnot(validObject(prior))
    if (useC) {
        ## new name, no zeta
        .Call(updatePhiAR10_R, prior)
    }
    else {
        phi.known <- prior@phiKnown
        if (!phi.known) {
            K <- prior@K
            L <- prior@L
            phi.restricted <- prior@phiRestricted
            gamma <- prior@gamma
            w <- prior@w
            iter.gamma <- prior@iteratorGamma
            iter.w <- prior@iteratorV
            iter.gamma <- resetA(iter.gamma)
            iter.w <- resetA(iter.w)
            num <- 0
            den <- 0
            for (l in seq_len(L)) {
                indices.gamma <- iter.gamma@indices ## length K+1
                indices.w <- iter.w@indices ## length K
                for (k in seq_len(K)) {
                    i1 <- indices.gamma[k + 1L]
                    i0 <- indices.gamma[k]
                    iw <- indices.w[k]
                    num <- num + gamma[i0] * gamma[i1] / w[iw]
                    den <- den + gamma[i0]^2 / w[iw]
                }
                iter.gamma <- advanceA(iter.gamma)
                iter.w <- advanceA(iter.w)
            }
            mean <- num / den
            sd <- 1 / sqrt(den)
            phi.prop <- rnorm(n = 1L, mean = mean, sd = sd)
            if (!phi.restricted || ((phi.prop >= 0) && (phi.prop <= 1)))
                prior@phi <- phi.prop
        }
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updatePhiAR11 <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "AR11"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updatePhiAR11_R, prior)
    }
    else {
        phi.known <- prior@phiKnown
        if (!phi.known) {
            K <- prior@K
            L <- prior@L
            phi.restricted <- prior@phiRestricted
            delta <- prior@delta
            w <- prior@w
            iter.delta <- prior@iteratorGamma
            iter.w <- prior@iteratorV
            iter.delta <- resetA(iter.delta)
            iter.w <- resetA(iter.w)
            num <- 0
            den <- 0
            for (l in seq_len(L)) {
                indices.delta <- iter.delta@indices ## length K+1
                indices.w <- iter.w@indices ## length K
                for (k in seq_len(K)) {
                    i1 <- indices.delta[k + 1L]
                    i0 <- indices.delta[k]
                    iw <- indices.w[k]
                    num <- num + delta[i0] * delta[i1] / w[iw]
                    den <- den + delta[i0]^2 / w[iw]
                }
                iter.delta <- advanceA(iter.delta)
                iter.w <- advanceA(iter.w)
            }
            mean <- num / den
            sd <- 1 / sqrt(den)
            phi.prop <- rnorm(n = 1L, mean = mean, sd = sd)
            if (!phi.restricted || ((phi.prop >= 0) && (phi.prop <= 1)))
                prior@phi <- phi.prop
        }
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateTauKnown <- function(prior, zeta, useC = FALSE) {
    ## prior
    validObject(prior)
    stopifnot(is(prior, "KnownTau"))
    ## zeta
    stopifnot(is.double(zeta))
    stopifnot(identical(length(zeta), 1L))
    stopifnot(!is.na(zeta))
    if (useC) {
        .Call(updateTauKnown_R, prior, zeta)
    }
    else {
        tau.unscaled <- prior@tauUnscaled
        tau <- tau.unscaled / abs(zeta)
        prior@tau <- tau
        prior
    }
}

## ADDED TEST FOR TAU
## TRANSLATED ## removed C code
## HAS_TESTS
## ADDED zeta ARGUMENT
updateTauNormZeroUnknown <- function(prior, beta, zeta, useC = FALSE) {
    ## prior
    validObject(prior)
    stopifnot(is(prior, "Norm"))
    stopifnot(is(prior, "Zero"))
    stopifnot(is(prior, "UnknownTau"))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    stopifnot(length(beta) > 2L)
    ## zeta
    stopifnot(is.double(zeta))
    stopifnot(identical(length(zeta), 1L))
    stopifnot(!is.na(zeta))
    if (useC) {
        ## ADDED zeta ARGUMENT
        .Call(updateTauNormZeroUnknown_R, prior, beta, zeta)
    }
    else {
        gamma <- prior@gamma
        ## THE FOLLOWING 2 LINES ARE NEW
        min.tau <- prior@minTau
        max.tau <- prior@maxTau
        J <- length(beta)
        s.sq <- sum((beta - mean(beta))^2) / (J - 2)
        tau.sq <- rinvchisq1(J - 2, s.sq)
        ## THE FOLLOWING 4 LINES ARE NEW
        tau.prop <- sqrt(tau.sq)
        tau.prop.scaled <- abs(zeta) * tau.prop
        if ((min.tau <= tau.prop.scaled) && (tau.prop.scaled <= max.tau))
            prior@tau <- tau.prop
        prior
    }
}

## ADDED TEST FOR TAU
## TRANSLATED ## removed C code
## HAS_TESTS
## ADDED zeta ARGUMENT
updateTauNormCovUnknown <- function(prior, beta, zeta, useC = FALSE) {
    ## prior
    validObject(prior)
    stopifnot(is(prior, "Norm"))
    stopifnot(is(prior, "Covariates"))
    stopifnot(is(prior, "UnknownTau"))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    Z <- prior@Z
    stopifnot(identical(length(beta), nrow(Z)))
    ## zeta
    stopifnot(is.double(zeta))
    stopifnot(identical(length(zeta), 1L))
    stopifnot(!is.na(zeta))
    if (useC) {
        ## ADDED zeta ARGUMENT
        .Call(updateTauNormCovUnknown_R, prior, beta, zeta)
    }
    else {
        ## THE FOLLOWING 2 LINES ARE NEW
        min.tau <- prior@minTau
        max.tau <- prior@maxTau
        Z <- prior@Z
        QR <- qr(Z)
        beta.hat <- qr.fitted(QR, beta)
        J <- length(beta)
        P <- length(prior@eta)
        s.sq <- sum((beta - beta.hat)^2) / (J - P - 1)
        tau.sq <- rinvchisq1(J - P - 1, s.sq)
        ## THE FOLLOWING 4 LINES ARE NEW
        tau.prop <- sqrt(tau.sq)
        tau.prop.scaled <- abs(zeta) * tau.prop
        if ((min.tau <= tau.prop.scaled) && (tau.prop.scaled <= max.tau))
            prior@tau <- tau.prop
        prior
    }
}

## ADDED TEST FOR TAU
## TRANSLATED ## removed C code
## HAS_TESTS
## ADDED zeta ARGUMENT
updateTauRobustUnknown <- function(prior, beta, zeta, useC = FALSE) {
    ## prior
    validObject(prior)
    stopifnot(is(prior, "Robust"))
    stopifnot(is(prior, "UnknownTau"))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    U <- prior@U
    stopifnot(identical(length(beta), length(U)))
    ## zeta
    stopifnot(is.double(zeta))
    stopifnot(identical(length(zeta), 1L))
    stopifnot(!is.na(zeta))
    if (useC) {
        ## ADDED zeta ARGUMENT
        .Call(updateTauRobustUnknown_R, prior, beta, zeta)
    }
    else {
        ## THE FOLLOWING 2 LINES ARE NEW
        min.tau <- prior@minTau
        max.tau <- prior@maxTau
        U <- prior@U
        nu <- prior@nu
        J <- length(beta)
        tau.sq <- rgamma(n = 1L,
                         shape = (J * nu + 1) / 2,
                         rate = (nu / 2) * sum(1 / U))
        ## THE FOLLOWING 4 LINES ARE NEW
        tau.prop <- sqrt(tau.sq)
        tau.prop.scaled <- abs(zeta) * tau.prop
        if ((min.tau <= tau.prop.scaled) && (tau.prop.scaled <= max.tau))
            prior@tau <- tau.prop
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateUCov <- function(prior, beta, useC = FALSE) {
    validObject(prior)
    stopifnot(is(prior, "Robust"))
    stopifnot(is(prior, "Covariates"))
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    Z <- prior@Z
    stopifnot(identical(length(beta), nrow(Z)))
    if (useC) {
        .Call(updateUCov_R, prior, beta)
    }
    else {
        nu <- prior@nu
        tau <- prior@tau
        beta.hat <- betaHat(prior)
        s.sq <- (nu * tau^2 + (beta - beta.hat)^2) / (nu + 1)  ## vector
        U <- numeric(length = length(s.sq))
        for (i in seq_along(U))
            U[i] <- rinvchisq1(df = nu + 1, scale = s.sq[i])
        prior@U <- U
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateUZero <- function(prior, beta, useC = FALSE) {
    validObject(prior)
    stopifnot(is(prior, "Robust"))
    stopifnot(is(prior, "Zero"))
    stopifnot(is.double(beta))
    stopifnot(all(!is.na(beta)))
    U <- prior@U
    stopifnot(identical(length(beta), length(U)))
    if (useC) {
        .Call(updateUZero_R, prior, beta)
    }
    else {
        gamma <- prior@gamma
        nu <- prior@nu
        tau <- prior@tau
        s.sq <- (nu * tau^2 + (beta - gamma)^2) / (nu + 1L)  ## vector
        U <- numeric(length = length(s.sq))
        for (i in seq_along(U))
            U[i] <- rinvchisq1(df = nu + 1, scale = s.sq[i])
        prior@U <- U
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateVPriorVAR1 <- function(prior, beta, zeta, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR1"))
    stopifnot(validObject(prior))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(!any(is.na(beta)))
    ## zeta
    stopifnot(is.double(zeta))
    stopifnot(identical(length(zeta), 1L))
    stopifnot(!is.na(zeta))
    if (useC) {
        .Call(updateVPriorVAR1_R, prior, beta, zeta)
    }
    else {    
        gamma.no.initial <- prior@gammaNoInitial
        v <- prior@v
        priorV <- prior@priorV
        priorV <- updatePriorVarDLM(prior = priorV,
                                    var = v,
                                    observed = beta,
                                    expected = gamma.no.initial,
                                    zeta = zeta)
        v <- updateVarDLM(prior = priorV,
                          observed = beta,
                          expected = gamma.no.initial)
        prior@v <- v
        prior@priorV <- priorV
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateWPriorWAR10 <- function(prior, zeta, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR1"))
    stopifnot(validObject(prior))
    ## zeta
    stopifnot(is.double(zeta))
    stopifnot(identical(length(zeta), 1L))
    stopifnot(!is.na(zeta))
    if (useC) {
        .Call(updateWPriorWAR10_R, prior, zeta)
    }
    else {    
        gamma.no.initial <- prior@gammaNoInitial
        gamma.hat <- prior@gammaHat
        w <- prior@w
        priorW <- prior@priorW
        priorW <- updatePriorVarDLM(prior = priorW,
                                    var = w,
                                    observed = gamma.no.initial,
                                    expected = gamma.hat,
                                    zeta = zeta)
        w <- updateVarDLM(prior = priorW,
                          observed = gamma.no.initial,
                          expected = gamma.hat)
        prior@w <- w
        prior@priorW <- priorW
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateWPriorWAR11 <- function(prior, zeta, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR11"))
    stopifnot(validObject(prior))
    ## zeta
    stopifnot(is.double(zeta))
    stopifnot(identical(length(zeta), 1L))
    stopifnot(!is.na(zeta))
    if (useC) {
        .Call(updateWPriorWAR11_R, prior, zeta)
    }
    else {    
        delta.no.initial <- prior@deltaNoInitial
        delta.hat <- prior@deltaHat
        w <- prior@w
        priorW <- prior@priorW
        priorW <- updatePriorVarDLM(prior = priorW,
                                    var = w,
                                    observed = delta.no.initial,
                                    expected = delta.hat,
                                    zeta = zeta)
        w <- updateVarDLM(prior = priorW,
                          observed = delta.no.initial,
                          expected = delta.hat)
        prior@w <- w
        prior@priorW <- priorW
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateWAndPriorsW <- function(object, zeta, useC = FALSE) {
    stopifnot(is(object, "PolyComponent"))
    stopifnot(validObject(object))
    if (useC) {
        .Call(updateWAndPriorsW_R, object, zeta)
    }
    else {
        J <- object@J
        q <- object@q
        gamma <- object@gamma         ## length J+1 (starts at t = 0)
        gamma.hat <- object@gammaHat  ## length J (starts at t = 1)
        G <- object@G
        W <- object@W
        priorsW <- object@priorsW
        for (j in seq_len(J))
            gamma.hat[[j]] <- drop(G %*% gamma[[j]])
        observed <- double(J)
        expected <- double(J)
        var <- double(J)
        for (i in seq_len(q)) {
            for (j in seq_len(J)) {
                observed[j] <- gamma[[j + 1L]][i]
                expected[j] <- gamma.hat[[j]][i]
                var[j] <- W[[j]][i, i]
            }
            priorsW[[i]] <- updatePriorVarDLM(prior = priorsW[[i]],
                                              var = var,
                                              observed = observed,
                                              expected = expected,
                                              zeta = zeta)
            var <- updateVarDLM(prior = priorsW[[i]],
                                observed = observed,
                                expected = expected)
            for (j in seq_len(J))
                W[[j]][i, i] <- var[j]
        }
        object@W <- W
        object@priorsW <- priorsW
        object@gammaHat <- gamma.hat
        object
    }
}




## TRANSLATED ## removed C code
## HAS_TESTS
updateTheta_BinomialFixed <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(is(object, "BinomialFixed"))
    stopifnot(validObject(object))
    ## y
    stopifnot(is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(is(exposure, "Counts"))
    stopifnot(is.integer(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0L))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y[!is.na(y)] <= exposure[!is.na(y)]))
    if (useC) {
        .Call(updateTheta_BinomialFixed_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        mu <- object@mu
        sigma <- object@sigma
        nu <- mu * (1 - mu)
        prior.shape1 <- mu * (nu / sigma^2 - 1)
        prior.shape2 <- (1 - mu) * (nu / sigma^2 - 1)
        for (i in seq_along(theta)) {
            if (is.na(y[i])) {
                shape1 <- prior.shape1
                shape2 <- prior.shape2
            }
            else {
                shape1 <- prior.shape1 + y[i]
                shape2 <- prior.shape2 + exposure[i] - y[i]
            }
            theta[i] <- rbeta(n = 1L, shape1 = shape1, shape2 = shape2)
        }
        object@theta <- theta
        object
    }
}


## TRANSLATED subject to my changes in the tests - JAH ## removed C code
## HAS_TESTS
updateTheta_NormalFixed <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(is(object, "NormalFixed"))
    stopifnot(validObject(object))
    ## y
    stopifnot(is(y, "DemographicArray"))
    stopifnot(is.double(y))
    stopifnot(identical(length(y), length(object@theta)))
    if (useC) {
        .Call(updateTheta_NormalFixed_R, object, y)
    }
    else {
        theta <- object@theta ## JAH added this line
        varsigma <- object@varsigma
        w <- object@w
        mu <- object@mu
        sigma <- object@sigma
        prec.prior <- 1 / sigma^2
        for (i in seq_along(y)) {
            if (is.na(y[i])) {
                mean <- mu
                sd <- sigma
            }
            else {
                prec.data <- w[i] / varsigma^2
                var <- 1 / (prec.prior + prec.data)
                mean <- (prec.prior * mu + prec.data * y[i]) * var
                sd <- sqrt(var)
            }
            theta[i] <- rnorm(n = 1L, mean = mean, sd = sd)
        }
        object@theta <- theta
        object
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateTheta_PoissonFixedNotUseExp <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(is(object, "PoissonFixedNotUseExp"))
    stopifnot(validObject(object))
    ## y
    stopifnot(is(y, "Counts"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y[!is.na(y)] >= 0))
    if (useC) {
        .Call(updateTheta_PoissonFixedNotUseExp_R, object, y)
    }
    else {
        theta <- object@theta
        mu <- object@mu
        sigma <- object@sigma
        prior.shape <- mu^2 / sigma^2
        prior.rate <- mu / sigma^2
        for (i in seq_along(theta)) {
            if (is.na(y[i])) {
                shape <- prior.shape
                rate <- prior.rate
            }
            else {
                shape <- prior.shape + y[i]
                rate <- prior.rate + 1.0
            }
            theta[i] <- rgamma(n = 1L, shape = shape, rate = rate)
        }
        object@theta <- theta
        object
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
updateTheta_PoissonFixedUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(is(object, "PoissonFixedUseExp"))
    stopifnot(validObject(object))
    ## y
    stopifnot(is(y, "Counts"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(is(exposure, "Counts"))
    stopifnot(is.double(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0L))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y[!is.na(y)][exposure[!is.na(y)] == 0] == 0))
    if (useC) {
        .Call(updateTheta_PoissonFixedUseExp_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        mu <- object@mu
        sigma <- object@sigma
        prior.shape <- mu^2 / sigma^2
        prior.rate <- mu / sigma^2
        for (i in seq_along(theta)) {
            if (is.na(y[i])) {
                shape <- prior.shape
                rate <- prior.rate
            }
            else {
                shape <- prior.shape + y[i]
                rate <- prior.rate + exposure[i]
            }
            theta[i] <- rgamma(n = 1L, shape = shape, rate = rate)
        }
        object@theta <- theta
        object
    }
}


## TRANSLATED ## removed C code
## HAS_TESTS
updateSigma_PoissonFixed <- function(object, useC = FALSE) {
    stopifnot(is(object, "PoissonFixed"))
    stopifnot(validObject(object))
    if (useC) {
        .Call(updateSigma_PoissonFixed_R, object)
    }
    else {
        theta <- object@theta
        mu <- object@mu
        sigma.curr <- object@sigma
        scaleSigma <- object@scaleSigma
        df.prior.sigma <- object@dfPriorSigma
        has.informative.prior <- df.prior.sigma != -1
        sigma.prop <- rnorm(n = 1L, mean = sigma.curr, sd = scaleSigma)
        if (sigma.prop > 0) {
            log.dens.curr <- sum(dgamma(x = theta,
                                        shape = mu^2 / sigma.curr^2,
                                        rate = mu / sigma.curr^2,
                                        log = TRUE))
            log.dens.prop <- sum(dgamma(x = theta,
                                        shape = mu^2 / sigma.prop^2,
                                        rate = mu / sigma.prop^2,
                                        log = TRUE))
            log.diff <- log.dens.prop - log.dens.curr
            if (has.informative.prior) {
                scale.prior.sigma <- object@scalePriorSigma # equals s in Gelman et al
                log.diff.prior <- ((df.prior.sigma / 2 + 1) *
                                   (log(sigma.curr) - log(sigma.prop)) +
                                   (df.prior.sigma * scale.prior.sigma^2 / 2) *
                                   ((1 / sigma.curr) - (1 / sigma.prop)))
                log.diff <- log.diff + log.diff.prior
            }
            accept <- (log.diff >= 0) || (runif(n = 1L) < exp(log.diff))
        }
        else
            accept <- FALSE
        if (accept) {
            object@sigma <- sigma.prop
            object@acceptSigma <- 1L
        }
        else
            object@acceptSigma <- 0L
        object
    }
}



## TRANSLATED ## removed C code
## HAS_TESTS
updateSigma_BinomialFixed <- function(object, useC = FALSE) {
    stopifnot(is(object, "BinomialFixed"))
    stopifnot(validObject(object))
    if (useC) {
        .Call(updateSigma_BinomialFixed_R, object)
    }
    else {
        theta <- object@theta
        mu <- object@mu
        sigma.curr <- object@sigma
        scaleSigma <- object@scaleSigma
        df.prior.sigma <- object@dfPriorSigma
        has.informative.prior <- df.prior.sigma != -1
        nu <- mu * (1 - mu)
        sigma.prop <- rnorm(n = 1L, mean = sigma.curr, sd = scaleSigma)
        if ((sigma.prop > 0) && (sigma.prop < nu)) {
            log.dens.curr <- sum(dbeta(x = theta,
                                       shape1 = mu * (nu / sigma.curr^2 - 1),
                                       shape2 = (1 - mu) * (nu / sigma.curr^2 - 1),
                                       log = TRUE))
            log.dens.prop <- sum(dbeta(x = theta,
                                       shape1 = mu * (nu / sigma.prop^2 - 1),
                                       shape2 = (1 - mu) * (nu / sigma.prop^2 - 1),
                                       log = TRUE))
            log.diff <- log.dens.prop - log.dens.curr
            if (has.informative.prior) {
                scale.prior.sigma <- object@scalePriorSigma # equals s in Gelman et al
                log.diff.prior <- ((df.prior.sigma / 2 + 1) *
                                   (log(sigma.curr) - log(sigma.prop)) +
                                   (df.prior.sigma * scale.prior.sigma^2 / 2) *
                                   ((1 / sigma.curr) - (1 / sigma.prop)))
                log.diff <- log.diff + log.diff.prior
            }
            accept <- (log.diff >= 0) || (runif(n = 1L) < exp(log.diff))
        }
        else
            accept <- FALSE
        if (accept) {
            object@sigma <- sigma.prop
            object@acceptSigma <- 1L
        }
        else
            object@acceptSigma <- 0L
        object
    }
}


## TRANSLATED ## removed C code
## HAS_TESTS
updateSigma_NormalFixed <- function(object, useC = FALSE) {
    stopifnot(is(object, "NormalFixed"))
    stopifnot(validObject(object))
    if (useC) {
        .Call(updateSigma_NormalFixed_R, object)
    }
    else {
        theta <- object@theta
        mu <- object@mu
        df <- object@dfPriorSigma # -1 if non-informative prior
        scale <- object@scalePriorSigma # 0 if non-informative prior; equals s in Gelman et al
        I <- length(theta)
        SS <- sum((theta - mu)^2)
        sigma.sq <- rinvchisq1(I + df, (SS + df * scale^2) / (I + df))
        sigma <- sqrt(sigma.sq)
        object@sigma <- sigma
        object
    }
}








## PRIOR METHODS ##############################################################


## predictBeta #######################################################################

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "ExchNormZeroKnown"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_ExchNormZeroKnown_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  tau <- prior@tau
                  gamma <- prior@gamma
                  rnorm(n = J, mean = gamma, sd = tau)
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "ExchNormZeroUnknown"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_ExchNormZeroUnknown_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  tau <- prior@tau
                  gamma <- prior@gamma
                  rnorm(n = J, mean = gamma, sd = tau)
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "ExchNormCovKnown"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_ExchNormCovKnown_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  tau <- prior@tau
                  beta.hat <- betaHat(prior)
                  rnorm(n = J, mean = beta.hat, sd = tau)
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "ExchNormCovUnknown"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_ExchNormCovUnknown_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  tau <- prior@tau
                  beta.hat <- betaHat(prior)
                  rnorm(n = J, mean = beta.hat, sd = tau)
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "ExchRobustZeroKnown"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              ## J and prior
              stopifnot(identical(J, length(prior@U)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_ExchRobustZeroKnown_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  gamma <- prior@gamma
                  U <- prior@U
                  rnorm(n = J, mean = gamma, sd = sqrt(U))
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "ExchRobustZeroUnknown"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              ## J and prior
              stopifnot(identical(J, length(prior@U)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_ExchRobustZeroUnknown_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  gamma <- prior@gamma
                  U <- prior@U
                  rnorm(n = J, mean = gamma, sd = sqrt(U))
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "ExchRobustCovKnown"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              ## J and prior
              stopifnot(identical(J, length(prior@U)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_ExchRobustCovKnown_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  U <- prior@U
                  beta.hat <- betaHat(prior)
                  rnorm(n = J, mean = beta.hat, sd = sqrt(U))
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "ExchRobustCovUnknown"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              ## J and prior
              stopifnot(identical(J, length(prior@U)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_ExchRobustCovUnknown_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  U <- prior@U
                  beta.hat <- betaHat(prior)
                  rnorm(n = J, mean = beta.hat, sd = sqrt(U))
              }
          })

## 'betaHatPoly' uses 'forward' argument
## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "PolyPredict"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              ## J and prior
              stopifnot(identical(J, length(prior@v)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_PolyPredict_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  components <- prior@components
                  v <- prior@v
                  forward <- prior@forward
                  ## THE FOLLOWING LINE HAS CHANGED
                  beta.hat <- betaHatPoly(components, forward = forward)
                  rnorm(n = J, mean = beta.hat, sd = sqrt(v))
              }
          })

## name of C function has changed
## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "AR10Predict"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              ## J and prior
              stopifnot(identical(J, as.integer(prior@K * prior@L)))
              if (useC) {
                  if (useSpecific)
                     .Call(predictBeta_AR10Predict_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  gamma.no.initial <- prior@gammaNoInitial
                  v <- prior@v
                  rnorm(n = J, mean = gamma.no.initial, sd = sqrt(v))
              }
          })

## identical to method for AR10Predict - just a different name
## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictBeta",
          signature(prior = "AR11Predict"),
          function(prior, J, useC = FALSE, useSpecific = FALSE) {
              ## prior
              validObject(prior)
              ## J
              stopifnot(is.integer(J))
              stopifnot(identical(length(J), 1L))
              stopifnot(!is.na(J))
              stopifnot(J > 0L)
              ## J and prior
              stopifnot(identical(J, as.integer(prior@K * prior@L)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictBeta_AR11Predict_R, prior, J)
                  else
                      .Call(predictBeta_R, prior, J)
              }
              else {
                  gamma.no.initial <- prior@gammaNoInitial
                  v <- prior@v
                  rnorm(n = J, mean = gamma.no.initial, sd = sqrt(v))
              }
          })



## predictPrior ######################################################################

## Update parts of prior that are not (i) fixed or 
## (ii) copied from historical estimates.  
## Includes rescaling of 'Known' tau, which cannot
## be done via 'transferParamPrior', since rescaled
## value not extracted via 'extractValues'

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchNormZeroKnown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchNormZeroKnown_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  tau.unscaled <- prior@tauUnscaled
                  prior@tau <- tau.unscaled / abs(zeta)
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchNormZeroUnknown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchNormZeroUnknown_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchNormCovKnown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchNormCovKnown_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  tau.unscaled <- prior@tauUnscaled
                  prior@tau <- tau.unscaled / abs(zeta)
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchNormCovUnknown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchNormCovUnknown_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchRobustZeroKnown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchRobustZeroKnown_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  tau.unscaled <- prior@tauUnscaled
                  nu <- prior@nu
                  U <- prior@U
                  tau <- tau.unscaled / abs(zeta)
                  for (j in seq_along(U))
                      U[j] <- rinvchisq1(df = nu, scale = tau^2)
                  prior@tau <- tau
                  prior@U <- U
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchRobustZeroUnknown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchRobustZeroUnknown_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  tau <- prior@tau
                  nu <- prior@nu
                  U <- prior@U
                  for (j in seq_along(U))
                      U[j] <- rinvchisq1(df = nu, scale = tau^2)
                  prior@U <- U
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchRobustCovKnown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchRobustCovKnown_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  tau.unscaled <- prior@tauUnscaled
                  nu <- prior@nu
                  U <- prior@U
                  tau <- tau.unscaled / abs(zeta)
                  for (j in seq_along(U))
                      U[j] <- rinvchisq1(df = nu, scale = tau^2)
                  prior@tau <- tau
                  prior@U <- U
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchRobustCovUnknown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchRobustCovUnknown_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  tau <- prior@tau
                  nu <- prior@nu
                  U <- prior@U
                  for (j in seq_along(U))
                      U[j] <- rinvchisq1(df = nu, scale = tau^2)
                  prior@U <- U
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "PolyPredict"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_PolyPredict_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  components <- prior@components
                  priorV <- prior@priorV
                  v <- prior@v
                  forward <- prior@forward
                  for (i in seq_along(components)) 
                      components[[i]] <- predictPolyComponent(component = components[[i]],
                                                              forward = forward,
                                                              zeta = zeta)
                  for (j in seq_along(v)) 
                      v[j] <- predictVarDLM(prior = priorV, zeta = zeta)
                  prior@components <- components
                  prior@v <- v
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "AR10Predict"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_AR10Predict_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  K <- prior@K
                  L <- prior@L
                  gamma <- prior@gamma
                  phi <- prior@phi
                  v <- prior@v
                  w <- prior@w
                  priorV <- prior@priorV
                  priorW <- prior@priorW
                  iter.gamma <- prior@iteratorGamma
                  iter.v <- prior@iteratorV
                  forward <- prior@forward
                  iter.gamma <- resetA(iter.gamma)
                  iter.v <- resetA(iter.v)
                  for (l in seq_len(L)) {
                      indices.gamma <- iter.gamma@indices
                      indices.v <- iter.v@indices
                      if (forward) {
                          for (k in seq_len(K)) {
                              ig0 <- indices.gamma[k]
                              ig1 <- indices.gamma[k + 1L]
                              iv <- indices.v[k]
                              v[iv] <- predictVarDLM(priorV, zeta = zeta)
                              w[iv] <- predictVarDLM(priorW, zeta = zeta)
                              mean <- phi * gamma[ig0]
                              sd <- sqrt(w[iv])
                              gamma[ig1] <- rnorm(n = 1L, mean = mean, sd = sd)
                          }
                      }
                      else {
                          for (k in seq.int(from = K, to = 1L)) {
                              ig0 <- indices.gamma[k]
                              ig1 <- indices.gamma[k + 1L]
                              iv <- indices.v[k]
                              v[iv] <- predictVarDLM(priorV, zeta = zeta)
                              w[iv] <- predictVarDLM(priorW, zeta = zeta)
                              mean <- phi * gamma[ig1]  ## ig1
                              sd <- sqrt(w[iv])
                              gamma[ig0] <- rnorm(n = 1L, mean = mean, sd = sd) ## ig0
                          }
                      }
                      iter.gamma <- advanceA(iter.gamma)
                      iter.v <- advanceA(iter.v)                      
                  }
                  prior@gamma <- gamma
                  prior <- updateGammaNoInitialAR1(prior)
                  prior <- updateGammaHatAR10(prior)
                  prior@v <- v
                  prior@w <- w
                  prior
              }
          })

## TRANSLATED ## removed C code
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "AR11Predict"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_AR11Predict_R, prior, zeta)
                  else
                      .Call(predictPrior_R, prior, zeta)
              }
              else {
                  K <- prior@K
                  L <- prior@L
                  gamma <- prior@gamma
                  delta <- prior@delta
                  phi <- prior@phi
                  v <- prior@v
                  w <- prior@w
                  priorV <- prior@priorV
                  priorW <- prior@priorW
                  iter.gamma <- prior@iteratorGamma
                  iter.v <- prior@iteratorV
                  forward <- prior@forward
                  iter.gamma <- resetA(iter.gamma)
                  iter.v <- resetA(iter.v)
                  for (l in seq_len(L)) {
                      indices.gamma <- iter.gamma@indices
                      indices.v <- iter.v@indices
                      if (forward) {
                          for (k in seq_len(K)) {
                              ig0 <- indices.gamma[k]
                              ig1 <- indices.gamma[k + 1L]
                              iv <- indices.v[k]
                              v[iv] <- predictVarDLM(priorV, zeta = zeta)
                              w[iv] <- predictVarDLM(priorW, zeta = zeta)
                              mean <- phi * delta[ig0]
                              sd <- sqrt(w[iv])
                              delta[ig1] <- rnorm(n = 1L, mean = mean, sd = sd)
                              gamma[ig1] <- gamma[ig0] + delta[ig0]
                          }
                      }
                      else {
                          for (k in seq.int(from = K, to = 1L)) {
                              ig0 <- indices.gamma[k]
                              ig1 <- indices.gamma[k + 1L]
                              iv <- indices.v[k]
                              v[iv] <- predictVarDLM(priorV, zeta = zeta)
                              w[iv] <- predictVarDLM(priorW, zeta = zeta)
                              mean <- phi * delta[ig1]  ## ig1
                              sd <- sqrt(w[iv])
                              delta[ig0] <- rnorm(n = 1L, mean = mean, sd = sd) ## ig0
                              gamma[ig0] <- gamma[ig1] + delta[ig1]
                          }
                      }
                      iter.gamma <- advanceA(iter.gamma)
                      iter.v <- advanceA(iter.v)                      
                  }
                  prior@gamma <- gamma
                  prior@delta <- delta
                  prior <- updateGammaNoInitialAR1(prior)
                  prior <- updateDeltaNoInitialAR11(prior)
                  prior <- updateDeltaHatAR11(prior)
                  prior@v <- v
                  prior@w <- w
                  prior
              }
          })



## predictVarDLM ####################################################################

## Extract value or generate values for variance

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("predictVarDLM",
          signature(prior = "PriorVarDLMNormKnown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictVarDLM_NormKnown_R, prior, zeta)
                  else
                      .Call(predictVarDLM_R, prior, zeta)
              }
              else {
                  tau.unscaled <- prior@tauUnscaled
                  tau <- tau.unscaled / abs(zeta)
                  tau^2
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("predictVarDLM",
          signature(prior = "PriorVarDLMNormUnknown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictVarDLM_NormUnknown_R, prior, zeta)
                  else
                      .Call(predictVarDLM_R, prior, zeta)
              }
              else {
                  prior@tau^2
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("predictVarDLM",
          signature(prior = "PriorVarDLMRobustKnown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictVarDLM_RobustKnown_R, prior, zeta)
                  else
                      .Call(predictVarDLM_R, prior, zeta)
              }
              else {
                  tau.unscaled <- prior@tauUnscaled
                  nu <- prior@nu
                  tau <- tau.unscaled / abs(zeta)
                  rinvchisq1(df = nu, scale = tau^2)
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("predictVarDLM",
          signature(prior = "PriorVarDLMRobustUnknown"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictVarDLM_RobustUnknown_R, prior, zeta)
                  else
                      .Call(predictVarDLM_R, prior, zeta)
              }
              else {
                  tau <- prior@tau
                  nu <- prior@nu
                  rinvchisq1(df = nu, scale = tau^2)
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("predictVarDLM",
          signature(prior = "PriorVarDLMZero"),
          function(prior, zeta, useC = FALSE, useSpecific = FALSE) {
              validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictVarDLM_Zero_R, prior, zeta)
                  else
                      .Call(predictVarDLM_R, prior, zeta)
              }
              else {
                  0
              }
          })


## transferParamPolyComponent ########################################################

## TRANSLATED # removed C code
## HAS_TESTS
## 'forward' argument is ignored
setMethod("transferParamPolyComponent",
          signature(component = "PolyComponentTrend"),
          function(component, values, forward, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPolyComponent_Trend_R, component, values,
                            forward)
                  else
                      .Call(transferParamPolyComponent_R, component, values, forward)
              }
              else {
                  component <- transferGamma(component = component,
                                             values = values)
                  component <- transferPriorsW(component = component,
                                               values = values)
                  component
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
## 'forward' argument is ignored
setMethod("transferParamPolyComponent",
          signature(component = "PolyComponentCovariates"),
          function(component, values, forward, useC = FALSE, useSpecific = FALSE) {
              ## values
              stopifnot(is.double(values))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPolyComponent_Covariates_R, component, values, forward)
                  else
                      .Call(transferParamPolyComponent_R, component, values, forward)
              }
              else {
                  component <- transferGamma(component = component,
                                             values = values)
                  component
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
## 'forward' argument is ignored
setMethod("transferParamPolyComponent",
          signature(component = "PolyComponentSeasonal"),
          function(component, values, forward, useC = FALSE, useSpecific = FALSE) {
              ## values
              stopifnot(is.double(values))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPolyComponent_Seasonal_R, component, values,
                            forward)
                  else
                      .Call(transferParamPolyComponent_R, component, values, forward)
              }
              else {
                  component <- transferGamma(component = component,
                                             values = values)
                  component <- transferPriorsW(component = component,
                                               values = values)
                  component
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPolyComponent",
          signature(component = "PolyComponentTrendPredict"),
          function(component, values, forward, useC = FALSE, useSpecific = FALSE) {
              ## values
              stopifnot(is.double(values))
              ## forward
              stopifnot(is.logical(forward))
              stopifnot(identical(length(forward), 1L))
              stopifnot(!is.na(forward))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPolyComponent_TrendPredicted_R,
                            component, values, forward)
                  else
                      .Call(transferParamPolyComponent_R, component, values, forward)
              }
              else {
                  component <- transferGamma0(component = component,
                                              values = values,
                                              forward = forward)
                  ## THE FOLLOWING LINE IS NEW
                  component <- updateM0Poly(component = component, forward = forward)
                  component <- transferPriorsW(component = component,
                                               values = values)
                  component
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPolyComponent",
          signature(component = "PolyComponentCovariatesPredict"),
          function(component, values, forward, useC = FALSE, useSpecific = FALSE) {
              ## values
              stopifnot(is.double(values))
              ## forward
              stopifnot(is.logical(forward))
              stopifnot(identical(length(forward), 1L))
              stopifnot(!is.na(forward))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPolyComponent_CovariatesPredicted_R,
                            component, values, forward)
                  else
                      .Call(transferParamPolyComponent_R, component, values, forward)
              }
              else {
                  component <- transferGamma0(component = component,
                                              values = values,
                                              forward = forward)
                  ## THE FOLLOWING LINE IS NEW
                  component <- updateM0Poly(component = component, forward = forward)
                  component
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPolyComponent",
          signature(component = "PolyComponentSeasonalPredict"),
          function(component, values, forward, useC = FALSE, useSpecific = FALSE) {
              ## values
              stopifnot(is.double(values))
              ## forward
              stopifnot(is.logical(forward))
              stopifnot(identical(length(forward), 1L))
              stopifnot(!is.na(forward))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPolyComponent_SeasonalPredicted_R, component, values, forward)
                  else
                      .Call(transferParamPolyComponent_R, component, values, forward)
              }
              else {
                  component <- transferGamma0(component = component,
                                              values = values,
                                              forward = forward)
                  ## THE FOLLOWING LINE IS NEW
                  component <- updateM0Poly(component = component, forward = forward)
                  component <- transferPriorsW(component = component,
                                               values = values)
                  component
              }
          })


## transferParamPrior ################################################################

## Can't use this function to rescale 'tau' for '*Known' priors,
## since 'tau' not written out for these priors.

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchNormZeroKnown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(identical(length(values), 1L))
              stopifnot(!is.na(values))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchNormZeroKnown_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior@gamma <- values
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchNormZeroUnknown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(identical(length(values), 2L))
              stopifnot(!is.na(values))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchNormZeroUnknown_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior@gamma <- values[1L]
                  prior@tau <- values[2L]
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchNormCovKnown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              stopifnot(identical(length(values), length(prior@eta)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchNormCovKnown_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior@eta <- values
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchNormCovUnknown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              stopifnot(identical(length(values), length(prior@eta) + 1L))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchNormCovUnknown_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  n.eta <- length(prior@eta)
                  prior@eta <- values[1 : n.eta]
                  prior@tau <- values[n.eta + 1L]
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchRobustZeroKnown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(identical(length(values), 1L))
              stopifnot(!is.na(values))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchRobustZeroKnown_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior@gamma <- values
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchRobustZeroUnknown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(identical(length(values), 2L))
              stopifnot(!is.na(values))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchRobustZeroUnknown_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior@gamma <- values[1L]
                  prior@tau <- values[2L]
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchRobustCovKnown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              stopifnot(identical(length(values), length(prior@eta)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchRobustCovKnown_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior@eta <- values
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchRobustCovUnknown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              stopifnot(identical(length(values), length(prior@eta) + 1L))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchRobustCovUnknown_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  n.eta <- length(prior@eta)
                  prior@eta <- values[1 : n.eta]
                  prior@tau <- values[n.eta + 1L]
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "Poly"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_Poly_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  components <- prior@components
                  offsets.components <- prior@offsetsComponents
                  for (i in seq_along(components)) {
                      offsets <- offsets.components[[i]]
                      first <- offsets[1L]
                      last <- offsets[2L]
                      ## 'forward' argument is ignored.  Supply here because C equivalent
                      ## cannot take a varying number of arguments.
                      components[[i]] <- transferParamPolyComponent(component = components[[i]],
                                                                    values = values[first : last],
                                                                    forward = TRUE)
                  }
                  prior@components <- components
                  prior <- transferParamPriorV(prior = prior, values = values)
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "PolyPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_PolyPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  components <- prior@components
                  offsetsComponents <- prior@offsetsComponents
                  forward <- prior@forward
                  for (i in seq_along(components)) {
                      offsets <- offsetsComponents[[i]]
                      first <- offsets[1L]
                      last <- offsets[2L]
                      components[[i]] <- transferParamPolyComponent(component = components[[i]],
                                                                    values = values[first : last],
                                                                    forward = forward)
                  }
                  prior@components <- components
                  prior <- transferParamPriorV(prior = prior, values = values)
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "AR10"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_AR10_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior <- transferParamGammaAR1(prior = prior, values = values)
                  prior <- updateGammaNoInitialAR1(prior)
                  prior <- updateGammaHatAR10(prior)
                  prior <- transferParamPhiAR1(prior = prior, values = values)
                  prior <- transferParamPriorV(prior = prior, values = values)
                  prior <- transferParamPriorWAR1(prior = prior, values = values)
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "AR11"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_AR11_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior <- transferParamGammaAR1(prior = prior, values = values)
                  prior <- updateGammaNoInitialAR1(prior)
                  prior <- transferParamDeltaAR11(prior = prior, values = values)
                  prior <- updateDeltaNoInitialAR11(prior)
                  prior <- updateDeltaHatAR11(prior)
                  prior <- transferParamPhiAR1(prior = prior, values = values)
                  prior <- updateGAR11(prior = prior)
                  prior <- transferParamPriorV(prior = prior, values = values)
                  prior <- transferParamPriorWAR1(prior = prior, values = values)
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "AR10Predict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_AR10Predict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior <- transferParamGamma0AR1Predict(prior = prior,
                                                         values = values)
                  prior <- transferParamPhiAR1(prior = prior, values = values)
                  prior <- transferParamPriorV(prior = prior, values = values)
                  prior <- transferParamPriorWAR1(prior = prior, values = values)
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "AR11Predict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_AR11Predict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior <- transferParamGamma0AR1Predict(prior = prior,
                                                         values = values)
                  prior <- transferParamDelta0AR11Predict(prior = prior,
                                                          values = values)
                  prior <- transferParamPhiAR1(prior = prior, values = values)
                  prior <- updateGAR11(prior)
                  prior <- transferParamPriorV(prior = prior, values = values)
                  prior <- transferParamPriorWAR1(prior = prior, values = values)
                  prior
              }
          })



## transferParamPriorVarDLM ##########################################################

## This is perhaps a bit over-engineered, but I wanted to keep
## the same approach as with other methods.


## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPriorVarDLM",
          signature(prior = "PriorVarDLMNormUnknown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(identical(length(values), 1L))
              stopifnot(!is.na(values))
              stopifnot(values >= 0)
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPriorVarDLM_NormUnknown_R, prior, values)
                  else
                      .Call(transferParamPriorVarDLM_R, prior, values)
              }
              else {
                  prior@tau <- values
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("transferParamPriorVarDLM",
          signature(prior = "PriorVarDLMRobustUnknown"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              stopifnot(is.double(values))
              stopifnot(identical(length(values), 1L))
              stopifnot(!is.na(values))
              stopifnot(values >= 0)
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPriorVarDLM_RobustUnknown_R, prior, values)
                  else
                      .Call(transferParamPriorVarDLM_R, prior, values)
              }
              else {
                  prior@tau <- values
                  prior
              }
          })



## updatePrior #######################################################################

## Calling functions pass the same set of arguments to all
## 'updatePrior' functions, so 'updatePrior' functions
## must accept arguments that they do not use.

## Leave validity-checking of arguments to helper functions.


## Exchangeable Normal

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "ExchNormZeroKnown", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_ExchNormZeroKnown_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  prior <- updateTauKnown(prior = prior, zeta = zeta)
                  prior <- updateGammaNorm(prior = prior, beta = beta)
                  prior
              }
          })
## ADDED 'zeta' ARGUMENT TO updateTau FUNCTION
## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "ExchNormZeroUnknown", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_ExchNormZeroUnknown_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  ## ADDED zeta ARGUMENT
                  prior <- updateTauNormZeroUnknown(prior = prior,
                                                    beta = beta,
                                                    zeta = zeta)
                  prior <- updateGammaNorm(prior = prior, beta = beta)
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "ExchNormCovKnown", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_ExchNormCovKnown_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  prior <- updateTauKnown(prior = prior, zeta = zeta)
                  prior <- updateEtaNorm(prior = prior, beta = beta)
                  prior
              }
          })

## ADDED 'zeta' ARGUMENT TO updateTau FUNCTION
## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "ExchNormCovUnknown", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_ExchNormCovUnknown_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  for (i in seq_len(nUpdate)) {
                      ## ADDED zeta ARGUMENT
                      prior <- updateTauNormCovUnknown(prior = prior,
                                                       beta = beta,
                                                       zeta = zeta)
                      prior <- updateEtaNorm(prior = prior, beta = beta)
                  }
                  prior
              }
          })


## Exchangeable Robust

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "ExchRobustZeroKnown", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_ExchRobustZeroKnown_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  prior <- updateTauKnown(prior = prior, zeta = zeta)
                  for (i in seq_len(nUpdate)) {
                      prior <- updateUZero(prior = prior, beta = beta)
                      prior <- updateGammaRobust(prior = prior, beta = beta)
                  }
                  prior
              }
          })

## ADDED 'zeta' ARGUMENT TO updateTau FUNCTION
## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "ExchRobustZeroUnknown", beta = "numeric",
                    zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_ExchRobustZeroUnknown_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  for (i in seq_len(nUpdate)) {
                      ## ADDED zeta ARGUMENT
                      prior <- updateTauRobustUnknown(prior = prior,
                                                      beta = beta,
                                                      zeta = zeta)
                      prior <- updateUZero(prior = prior, beta = beta)
                      prior <- updateGammaRobust(prior = prior, beta = beta)
                  }
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "ExchRobustCovKnown", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_ExchRobustCovKnown_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  prior <- updateTauKnown(prior = prior, zeta = zeta)
                  for (i in seq_len(nUpdate)) {
                      prior <- updateUCov(prior = prior, beta = beta)
                      prior <- updateEtaRobust(prior = prior, beta = beta)
                  }
                  prior
              }
          })

## ADDED 'zeta' ARGUMENT TO updateTau FUNCTION
## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "ExchRobustCovUnknown", beta = "numeric",
                    zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_ExchRobustCovUnknown_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  for (i in seq_len(nUpdate)) {
                      ## ADDED zeta ARGUMENT
                      prior <- updateTauRobustUnknown(prior = prior,
                                                      beta = beta,
                                                      zeta = zeta)
                      prior <- updateUCov(prior = prior, beta = beta)
                      prior <- updateEtaRobust(prior = prior, beta = beta)
                  }
                  prior
              }
          })



## Uniform

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "Uniform", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_Uniform_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  prior
              }
          })


## Known

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "KnownCertain", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_KnownCertain_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  values.unscaled <- prior@valuesUnscaled
                  values <- values.unscaled / zeta
                  prior@values <- values
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "KnownUncertain", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_KnownUncertain_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  mean.unscaled <- prior@meanUnscaled
                  sd.unscaled <- prior@sdUnscaled
                  mean <- mean.unscaled / zeta
                  sd <- sd.unscaled / abs(zeta)
                  prior@mean <- mean
                  prior@sd <- sd
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "Poly", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior and beta
              stopifnot(identical(length(beta), length(prior@v)))
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_Poly_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  components <- prior@components
                  v <- prior@v
                  priorV <- prior@priorV
                  for (j in seq_len(nUpdate)) {
                      beta.hat <- betaHatPoly(components, forward = TRUE)
                      priorV <- updatePriorVarDLM(prior = priorV,
                                                  var = v,
                                                  observed = beta,
                                                  expected = beta.hat,
                                                  zeta = zeta)
                      v <- updateVarDLM(prior = priorV,
                                        observed = beta,
                                        expected = beta.hat)
                      for (i in seq_along(components)) {
                          components[[i]] <- updateWAndPriorsW(components[[i]],
                                                               zeta = zeta)
                          beta.tilde <- betaTilde(beta = beta,
                                                  components = components,
                                                  i = i,
                                                  forward = TRUE)
                          components[[i]] <- FFBS(components[[i]],
                                                  y = beta.tilde,
                                                  v = v,
                                                  forward = TRUE)
                      }
                  }
                  prior@components <- components
                  prior@priorV <- priorV
                  prior@v <- v
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
## Same as Poly method, except that checks 'forward' slot,
## which can be TRUE or FALSE
setMethod("updatePrior",
          signature(prior = "PolyPredict", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior and beta
              stopifnot(identical(length(beta), length(prior@v)))
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_PolyPredict_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  components <- prior@components
                  v <- prior@v
                  priorV <- prior@priorV
                  forward <- prior@forward
                  for (j in seq_len(nUpdate)) {
                      beta.hat <- betaHatPoly(components, forward = forward)
                      priorV <- updatePriorVarDLM(prior = priorV,
                                                  var = v,
                                                  observed = beta,
                                                  expected = beta.hat,
                                                  zeta = zeta)
                      v <- updateVarDLM(prior = priorV,
                                        observed = beta,
                                        expected = beta.hat)
                      for (i in seq_along(components)) {
                          components[[i]] <- updateWAndPriorsW(components[[i]],
                                                               zeta = zeta)
                          beta.tilde <- betaTilde(beta = beta,
                                                  components = components,
                                                  i = i,
                                                  forward = forward)
                          components[[i]] <- FFBS(components[[i]],
                                                  y = beta.tilde,
                                                  v = v,
                                                  forward = forward)
                      }
                  }
                  prior@components <- components
                  prior@priorV <- priorV
                  prior@v <- v
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "AR10", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_AR10_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  for (i in seq_len(nUpdate)) {
                      prior <- updateVPriorVAR1(prior = prior,
                                                beta = beta,
                                                zeta = zeta)
                      prior <- updateWPriorWAR10(prior = prior, zeta = zeta)
                      prior <- updateGammaAR10(prior = prior, beta = beta)
                      prior <- updateGammaNoInitialAR1(prior = prior)
                      prior <- updateGammaHatAR10(prior = prior)
                      prior <- updatePhiAR10(prior = prior)
                  }
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "AR11", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_AR11_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 10L
                  for (i in seq_len(nUpdate)) {
                      prior <- updateVPriorVAR1(prior = prior,
                                                beta = beta,
                                                zeta = zeta)
                      prior <- updateWPriorWAR11(prior = prior, zeta = zeta)
                      prior <- updateGammaDeltaAR11(prior = prior,
                                                    beta = beta,
                                                    forward = TRUE)
                      prior <- updateGammaNoInitialAR1(prior = prior)
                      prior <- updateDeltaNoInitialAR11(prior = prior)
                      prior <- updateDeltaHatAR11(prior = prior)
                      prior <- updatePhiAR11(prior = prior)
                      prior <- updateGAR11(prior)
                  }
                  prior
              }
          })

## TODO - TO IMPLEMENT BACKCASTING WITH CONSTRAINTS / BENCHMARKS
## NEED TO DO METHODS FOR AR10Predict and AR11Predict THAT USE
## 'forward' ARGUMENTS.  WILL REQUIRE CHANGES TO HELPER FUNCTIONS.


## updateBeta #######################################################################

## Include 'beta' argument in these functions even when it is not used,
## because calling function passes the same arguments to all 'updateBeta'
## functions, and 'beta' arguments will be needed by any future
## 'updateBeta' functions that use Metropolis-Hastings updates.

## No helper functions, so do validity checking within
## the R methods themselves.

## Exchangeable Normal

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "ExchNormZero"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_ExchNormZero_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  gamma <- prior@gamma
                  tau <- prior@tau
                  J <- length(beta)
                  prec.data <- n / sigma^2
                  prec.prior <- 1 / tau^2
                  var <- 1 / (prec.data + prec.prior)
                  sd <- sqrt(var)
                  mean <- var * (prec.data * vbar + prec.prior * gamma)
                  rnorm(n = J, mean = mean, sd = sd)
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "ExchNormCov"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and prior
              stopifnot(identical(length(beta), nrow(prior@Z)))
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_ExchNormCov_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  tau <- prior@tau
                  J <- length(beta)
                  prec.data <- n / sigma^2
                  prec.prior <- 1 / tau^2
                  var <- 1 / (prec.data + prec.prior)
                  sd <- sqrt(var)
                  beta.hat <- betaHat(prior)
                  mean <- var * (prec.data * vbar + prec.prior * beta.hat)
                  rnorm(n = J, mean = mean, sd = sd)
              }
          })


## Exchangeable Robust

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "ExchRobustZero"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and prior
              stopifnot(identical(length(beta), length(prior@U)))
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_ExchRobustZero_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  gamma <- prior@gamma
                  U <- prior@U
                  J <- length(beta)
                  prec.data <- n / sigma^2
                  prec.prior <- 1 / U
                  var <- 1 / (prec.data + prec.prior)
                  sd <- sqrt(var)
                  mean <- var * (prec.data * vbar + prec.prior * gamma)
                  rnorm(n = J, mean = mean, sd = sd)
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "ExchRobustCov"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and prior
              stopifnot(identical(length(beta), length(prior@U)))
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_ExchRobustCov_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  tau <- prior@tau
                  U <- prior@U
                  J <- length(beta)
                  prec.data <- n / sigma^2
                  prec.prior <- 1 / U
                  var <- 1 / (prec.data + prec.prior)
                  sd <- sqrt(var)
                  beta.hat <- betaHat(prior)
                  mean <- var * (prec.data * vbar + prec.prior * beta.hat)
                  rnorm(n = J, mean = mean, sd = sd)
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "Uniform"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_Uniform_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  J <- length(beta)
                  rnorm(n = J, mean = vbar, sd = sigma / sqrt(n))
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "KnownCertain"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_KnownCertain_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  prior@values
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "KnownUncertain"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_KnownUncertain_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  mean.prior <- prior@mean ## vector
                  sd.prior <- prior@sd ## vector
                  prec.data <- (n / sigma^2)
                  J <- length(beta)
                  ans <- double(length = J)
                  for (j in seq_len(J)) {
                      if (sd.prior[j] > 0) {
                          prec.prior <- (1 / sd.prior[j]^2)
                          var <- 1 / (prec.prior + prec.data)
                          mean <- var * (prec.prior * mean.prior[j] + prec.data * vbar[j])
                          sd <- sqrt(var)
                          ans[j] <- rnorm(n = 1L, mean = mean, sd = sd)
                      }
                      else
                          ans[j] <- mean.prior[j]
                  }
                  ans
              }
          })


## JAH NOTE - I think that this needs forward arg added for betaHatPoly
## IN C code I have assumed foward = TRUE
## TRANSLATED  # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "Poly"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and prior
              stopifnot(identical(length(beta), length(prior@v)))
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_Poly_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  components <- prior@components
                  v <- prior@v
                  J <- length(v)
                  beta.hat <- betaHatPoly(components, forward = TRUE)
                  prec.data <- n / sigma^2
                  prec.prior <- 1 / v
                  var <- 1 / (prec.data + prec.prior)
                  sd <- sqrt(var)
                  mean <- var * (prec.data * vbar + prec.prior * beta.hat)
                  rnorm(n = J, mean = mean, sd = sd)
              }
          })

## NEED TO DO VERSION FOR PriorPredict THAT USES 'forward' ARGUMENT - SAME FOR AR1

## name of C function changed
## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "AR10"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and prior
              stopifnot(identical(length(beta), length(prior@v)))
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      ## NAME OF C FUNCTION HAS CHANGED
                      .Call(updateBeta_AR10_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  gamma.no.initial <- prior@gammaNoInitial
                  v <- prior@v
                  J <- length(v)
                  prec.data <- n / sigma^2
                  prec.prior <- 1 / v
                  var <- 1 / (prec.data + prec.prior)
                  sd <- sqrt(var)
                  mean <- var * (prec.data * vbar + prec.prior * gamma.no.initial)
                  rnorm(n = J, mean = mean, sd = sd)
              }
          })

## identical to method for AR10 except for name of C function
## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateBeta",
          signature(prior = "AR11"),
          function(beta, prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              ## beta
              stopifnot(is.double(beta))
              stopifnot(!any(is.na(beta)))
              ## prior
              stopifnot(validObject(prior))
              ## vbar
              stopifnot(is.double(vbar))
              stopifnot(!any(is.na(vbar)))
              ## n
              stopifnot(is.integer(n))
              stopifnot(identical(length(n), 1L))
              stopifnot(!is.na(n))
              stopifnot(n > 0L)
              ## sigma
              stopifnot(is.double(sigma))
              stopifnot(identical(length(sigma), 1L))
              stopifnot(!is.na(sigma))
              stopifnot(sigma > 0)
              ## beta and prior
              stopifnot(identical(length(beta), length(prior@v)))
              ## beta and vbar
              stopifnot(identical(length(beta), length(vbar)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateBeta_AR11_R, beta, prior, vbar, n, sigma)
                  else
                      .Call(updateBeta_R, beta, prior, vbar, n, sigma)
              }
              else {
                  gamma.no.initial <- prior@gammaNoInitial
                  v <- prior@v
                  J <- length(v)
                  prec.data <- n / sigma^2
                  prec.prior <- 1 / v
                  var <- 1 / (prec.data + prec.prior)
                  sd <- sqrt(var)
                  mean <- var * (prec.data * vbar + prec.prior * gamma.no.initial)
                  rnorm(n = J, mean = mean, sd = sd)
              }
          })



## updatePriorVarDLM #################################################################

## Update hyper-parameters for Poly priors

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePriorVarDLM",
          signature(prior = "PriorVarDLMNormKnown"),
          function(prior, var, observed, expected, zeta,
                   useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## var
              stopifnot(is.double(var))
              stopifnot(!any(is.na(var)))
              stopifnot(all(var >= 0))
              if (length(var) > 1L)
                  all(var[-1L] == var[1L])
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              ## zeta
              stopifnot(is.double(zeta))
              stopifnot(identical(length(zeta), 1L))
              stopifnot(!is.na(zeta))
              ## var and observed
              stopifnot(identical(length(var), length(observed)))
              ## var and expected
              stopifnot(identical(length(var), length(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorVarDLM_NormKnown_R, prior, var, observed,
                            expected, zeta)
                  else
                      .Call(updatePriorVarDLM_R, prior, var, observed,
                            expected, zeta)
              }
              else {
                  tau.unscaled <- prior@tauUnscaled
                  tau <- tau.unscaled / abs(zeta)
                  prior@tau <- tau
                  prior
              }
          })

## ADDED TEST FOR TAU
## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePriorVarDLM",
          signature(prior = "PriorVarDLMNormUnknown"),
          function(prior, var, observed, expected, zeta,
                   useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## var
              stopifnot(is.double(var))
              stopifnot(!any(is.na(var)))
              stopifnot(all(var >= 0))
              if (length(var) > 1L)
                  all(var[-1L] == var[1L])
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              ## zeta
              stopifnot(is.double(zeta))
              stopifnot(identical(length(zeta), 1L))
              stopifnot(!is.na(zeta))
              ## var and observed
              stopifnot(identical(length(var), length(observed)))
              ## var and expected
              stopifnot(identical(length(var), length(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorVarDLM_NormUnknown_R, prior, var, observed,
                            expected, zeta)
                  else
                      .Call(updatePriorVarDLM_R, prior, var, observed,
                            expected, zeta)
              }
              else {
                  ## THE FOLLOWING 2 LINES ARE NEW
                  min.tau <- prior@minTau
                  max.tau <- prior@maxTau
                  J <- length(observed)
                  s.sq <- sum((observed - expected)^2) / (J - 1)
                  tau.sq <- rinvchisq1(J - 1, s.sq)
                  ## THE FOLLOWING 4 LINES ARE NEW
                  tau.prop <- sqrt(tau.sq)
                  tau.prop.scaled <- abs(zeta) * tau.prop
                  if ((min.tau <= tau.prop.scaled) && (tau.prop.scaled <= max.tau))
                      prior@tau <- tau.prop
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePriorVarDLM",
          signature(prior = "PriorVarDLMRobustKnown"),
          function(prior, var, observed, expected, zeta,
                   useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## var
              stopifnot(is.double(var))
              stopifnot(!any(is.na(var)))
              stopifnot(all(var >= 0))
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              ## zeta
              stopifnot(is.double(zeta))
              stopifnot(identical(length(zeta), 1L))
              stopifnot(!is.na(zeta))
              ## var and observed
              stopifnot(identical(length(var), length(observed)))
              ## var and expected
              stopifnot(identical(length(var), length(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorVarDLM_RobustKnown_R, prior, var, observed,
                            expected, zeta)
                  else
                      .Call(updatePriorVarDLM_R, prior, var, observed,
                            expected, zeta)
              }
              else {
                  tau.unscaled <- prior@tauUnscaled
                  tau <- tau.unscaled / abs(zeta)
                  prior@tau <- tau
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePriorVarDLM",
          signature(prior = "PriorVarDLMRobustUnknown"),
          function(prior, var, observed, expected, zeta,
                   useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## var
              stopifnot(is.double(var))
              stopifnot(!any(is.na(var)))
              stopifnot(all(var >= 0))
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              ## zeta
              stopifnot(is.double(zeta))
              stopifnot(identical(length(zeta), 1L))
              stopifnot(!is.na(zeta))
              ## var and observed
              stopifnot(identical(length(var), length(observed)))
              ## var and expected
              stopifnot(identical(length(var), length(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorVarDLM_RobustUnknown_R, prior, var, observed,
                            expected, zeta)
                  else
                      .Call(updatePriorVarDLM_R, prior, var, observed,
                            expected, zeta)
              }
              else {
                  ## THE FOLLOWING 2 LINES ARE NEW
                  min.tau <- prior@minTau
                  max.tau <- prior@maxTau
                  nu <- prior@nu
                  J <- length(var)
                  tau.sq <- rgamma(n = 1L,
                                   shape = (J * nu + 1) / 2,
                                   rate = (nu / 2) * sum(1 / var))
                  ## THE FOLLOWING 4 LINES ARE NEW
                  tau.prop <- sqrt(tau.sq)
                  tau.prop.scaled <- abs(zeta) * tau.prop
                  if ((min.tau <= tau.prop.scaled) && (tau.prop.scaled <= max.tau))
                      prior@tau <- tau.prop
                  prior
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updatePriorVarDLM",
          signature(prior = "PriorVarDLMZero"),
          function(prior, var, observed, expected, zeta,
                   useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## var
              stopifnot(is.double(var))
              stopifnot(!any(is.na(var)))
              stopifnot(all(var == 0))
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              ## zeta
              stopifnot(is.double(zeta))
              stopifnot(identical(length(zeta), 1L))
              stopifnot(!is.na(zeta))
              ## var and observed
              stopifnot(identical(length(var), length(observed)))
              ## var and expected
              stopifnot(identical(length(var), length(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorVarDLM_Zero_R, prior, var, observed,
                            expected, zeta)
                  else
                      .Call(updatePriorVarDLM_R, prior, var, observed,
                            expected, zeta)
              }
              else {
                  prior
              }
          })

## updateVarDLM ######################################################################

## Update variance parameters for Poly priors

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateVarDLM",
          signature(prior = "PriorVarDLMNormKnown"),
          function(prior, observed, expected, useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateVarDLM_NormKnown_R, prior, observed, expected)
                  else
                      .Call(updateVarDLM_R, prior, observed, expected)
              }
              else {
                  tau <- prior@tau
                  J <- length(observed)
                  rep(tau^2, times = J)
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateVarDLM",
          signature(prior = "PriorVarDLMNormUnknown"),
          function(prior, observed, expected, useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateVarDLM_NormUnknown_R, prior, observed, expected)
                  else
                      .Call(updateVarDLM_R, prior, observed, expected)
              }
              else {
                  ## code identical to PriorVarDLMNormKnown
                  tau <- prior@tau
                  J <- length(observed)
                  rep(tau^2, times = J)
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateVarDLM",
          signature(prior = "PriorVarDLMRobustKnown"),
          function(prior, observed, expected, useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateVarDLM_RobustKnown_R, prior, observed, expected)
                  else
                      .Call(updateVarDLM_R, prior, observed, expected)
              }
              else {
                  tau <- prior@tau
                  nu <- prior@nu
                  J <- length(observed)
                  var <- double(J)
                  for (j in seq_len(J)) {
                      s.sq <- (nu * tau^2 + (observed[j] - expected[j])^2) / (nu + 1)
                      var[j] <- rinvchisq1(df = nu + 1, scale = s.sq)
                  }
                  var
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateVarDLM",
          signature(prior = "PriorVarDLMRobustUnknown"),
          function(prior, observed, expected, useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateVarDLM_RobustUnknown_R, prior, observed, expected)
                  else
                      .Call(updateVarDLM_R, prior, observed, expected)
              }
              else {
                  ## code identical to PriorVarDLMRobustKnown
                  tau <- prior@tau
                  nu <- prior@nu
                  J <- length(observed)
                  var <- double(J)
                  for (j in seq_len(J)) {
                      s.sq <- (nu * tau^2 + (observed[j] - expected[j])^2) / (nu + 1L)
                      var[j] <- rinvchisq1(df = nu + 1, scale = s.sq)
                  }
                  var
              }
          })

## TRANSLATED # removed C code
## HAS_TESTS
setMethod("updateVarDLM",
          signature(prior = "PriorVarDLMZero"),
          function(prior, observed, expected, useC = FALSE, useSpecific = FALSE) {
              ## prior
              stopifnot(validObject(prior))
              ## observed
              stopifnot(is.double(observed))
              stopifnot(!any(is.na(observed)))
              ## expected
              stopifnot(is.double(expected))
              stopifnot(!any(is.na(expected)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateVarDLM_Zero_R, prior, observed, expected)
                  else
                      .Call(updateVarDLM_R, prior, observed, expected)
              }
              else {
                  J <- length(observed)
                  rep(0, J)
              }
          })



## TRANSLATED # removed C code
## HAS_TESTS
updateBetas <- function(object, g, useC = FALSE) {
    ## object
    stopifnot(is(object, "Varying"))
    stopifnot(validObject(object))
    ## g
    stopifnot(is.function(g))
    if (useC) {
        .Call(updateBetas_R, object) ## drop g
    }
    else {
        zetas <- object@zetas
        priors <- object@priorsBetas
        sigma <- object@sigma
        I <- length(object@theta)
        for (b in seq_along(object@betas)) {
            zeta <- zetas[b]
            vbar <- makeVBar(object, iBeta = b, g = g)  ## uses updated object
            n <- I %/% length(vbar)
            ## Scale by 'zeta'. This is a temporary hack. In future will
            ## revisit updating of beta.
            vbar.scaled <- vbar / zeta
            sigma.scaled <- sigma / abs(zeta)
            object@betas[[b]] <- updateBeta(beta = object@betas[[b]],
                                            prior = priors[[b]],
                                            vbar = vbar.scaled,
                                            n = n,
                                            sigma = sigma.scaled)
        }
        object
    }
}

## TRANSLATED # removed C code
## HAS_TESTS
updatePriorsBetas <- function(object, useC = FALSE) {
    stopifnot(is(object, "Betas"))
    validObject(object)
    if (useC) {
        .Call(updatePriorsBetas_R, object)
    }
    else {
        priors <- object@priorsBetas
        betas <- object@betas
        zetas <- object@zetas
        for (i in seq_along(priors)) {
            prior <- priors[[i]]
            beta <- betas[[i]]
            zeta <- zetas[[i]]
            priors[[i]] <- updatePrior(prior = prior, beta = beta, zeta = zeta)
        }
        object@priorsBetas <- priors
        object
    }
}

## TRANSLATED # removed C code
## HAS_TESTS
updateZetas <- function(object, g, useC = FALSE) {
    ## object
    stopifnot(is(object, "Varying"))
    stopifnot(validObject(object))
    ## g
    stopifnot(is.function(g))
    if (useC) {
        .Call(updateZetas_R, object)
    }
    else {
        kMin <- 0.5
        kMax <- 2
        betas <- object@betas
        sigma <- object@sigma
        n <- length(object@theta)
        for (b in seq_along(betas)) {
            vBar <- makeVBar(object, iBeta = b, g = g)  # uses updated object
            n.m <- n %/% length(vBar)
            beta.sum <- sum(betas[[b]])
            mean <- sum(vBar) / beta.sum
            sd <- sqrt(n) * sigma / (n.m * abs(beta.sum))
            zeta.prop <- rnorm(n = 1L, mean = mean, sd = sd)
            if ((zeta.prop > kMin) && (zeta.prop < kMax))
                object@zetas[b] <- zeta.prop
        }
        object
    }
}


## HELPER FUNCTIONS ############################################################

## ADDED 'FORWARD' ARGUMENT
## TRANSLATED # removed C code
## HAS_TESTS
FFBS <- function(object, y, v, forward, useC = FALSE) {
    ## object
    stopifnot(is(object, "PolyComponent"))
    stopifnot(validObject(object))
    ## y
    stopifnot(is.double(y))
    stopifnot(!any(is.na(y)))
    ## v
    stopifnot(is.double(v))
    stopifnot(!any(is.na(v)))
    stopifnot(all(v >= 0))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    ## object and y
    stopifnot(identical(length(y), object@J))
    ## object and v
    stopifnot(identical(length(v), object@J))
    if (useC) {
        ## ADDED 'forward' ARGUMENT
        .Call(FFBS_R, object, y, v, forward)
    }
    else { ## ADDED 'forward' ARGUMENTS
        object <- forwardFilter(object = object,
                                y = y,
                                v = v,
                                forward = forward)
        object <- updateFinalGamma(object = object, forward = forward)
        object <- backwardSample(object = object, forward = forward)
        object
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
backwardSample <- function(object, forward, useC = FALSE) {
    ## object
    stopifnot(is(object, "PolyComponent"))
    stopifnot(validObject(object))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    if (useC) {
        .Call(backwardSample_R, object, forward)
    }
    else {
        J <- object@J
        q <- object@q
        gamma <- object@gamma  ## length J+1 (starts at t = 0)
        G <- object@G
        W <- object@W          ## length J (starts at t = 1)
        m <- object@m          ## length J+1 (starts at t = 0)
        C <- object@CC         ## length J+1 (starts at t = 0)
        UC <- object@UC        ## length J+1 (starts at t = 0)
        DC.inv <- object@DCInv ## length J+1 (starts at t = 0)
        a <- object@a          ## length J (starts at t = 1)
        UR <- object@UR        ## length J (starts at t = 1)
        DR.inv <- object@DRInv ## length J (starts at t = 1)
        eps <- .Machine$double.eps
        eps.inv <- 1 / eps
        sqrt.W.inv <- double(length = q)
        DCs <- double(length = q)
        for (j in seq.int(from = J, to = 1L)) {
            if (forward) {
                j0 <- j + 1L
                j1 <- j
                j2 <- j
            }
            else {
                j0 <- J - j + 1L
                j1 <- J - j + 1L
                j2 <- J - j + 2L
            }
            R.inv <- UR[[j1]] %*% diag(DR.inv[[j1]]^2) %*% t(UR[[j1]])
            B.j <- C[[j2]] %*% t(G) %*% R.inv
            for (i in seq_len(q)) {
                if (W[[j1]][i, i] > eps)
                    sqrt.W.inv[i] <- 1 / sqrt(W[[j1]][i, i])
                else
                    sqrt.W.inv[i] <- eps.inv
            }
            M.Cs <- rbind(diag(sqrt.W.inv) %*% G,
                          diag(DC.inv[[j2]]) %*% t(UC[[j2]]))
            svd.Cs <- svd(M.Cs, nu = 0)
            UCs <- svd.Cs$v
            DCs <- 1 / svd.Cs$d
            DCs[is.infinite(DCs)] <- 0
            ms <- m[[j2]] + drop(B.j %*% (gamma[[j0]] - a[[j1]]))
            z <- rnorm(n = q)
            sqrt.Cs <- UCs %*% diag(DCs)
            gamma[[j2]] <- ms + drop(sqrt.Cs %*% z)
       }
        object@gamma <- gamma
        object
    }
}



## TRANSLATED ## function rewritten in C
## HAS_TESTS
betaHat <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "Covariates"))
    validObject(prior)
    if (useC) {
        .Call(betaHat_R, prior)
    }
    else {
        eta <- prior@eta
        Z <- prior@Z
        drop(Z %*% eta)
    }
}


## ADDED 'forward' ARGUMENT
## TRANSLATED
## HAS_TESTS
betaHatComponent <- function(component, forward, useC = FALSE) {
    ## component
    stopifnot(is(component, "PolyComponent"))
    stopifnot(validObject(component))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    if (useC) {
        ## added 'forward' argument
        .Call(betaHatComponent_R, component, forward)
    }
    else {
        J <- component@J
        F <- component@F         
        gamma <- component@gamma
        ans <- numeric(J)
        for (j in seq_len(J)) {
            ## next 2 lines have changed
            jg <- if (forward) j + 1L else j
            ans[j] <- sum(F[[j]] * gamma[[jg]])
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
betaHatPoly <- function(components, forward, useC = FALSE) {
    ## components
    stopifnot(is.list(components))
    stopifnot(length(components) >= 1L)
    stopifnot(all(sapply(components, is, "PolyComponent")))
    stopifnot(all(sapply(components, validObject)))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    if (useC) {
        .Call(betaHatPoly_R, components, forward)
    }
    else {
        J <- components[[1L]]@J
        ans <- rep(0, times = J)
        for (component in components)
            ans <- ans + betaHatComponent(component = component,
                                          forward = forward)
        ans
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
betaTilde <- function(beta, components, i, forward, useC = FALSE) {
    ## beta
    stopifnot(is.double(beta))
    stopifnot(!any(is.na(beta)))
    ## components
    stopifnot(is.list(components))
    stopifnot(length(components) >= 1L)
    stopifnot(all(sapply(components, is, "PolyComponent")))
    ## i
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    ## beta and components
    stopifnot(identical(length(beta), components[[1L]]@J))
    ## i and components
    stopifnot(i <= length(components))
    if (useC) {
        ## added 'forward' argument
        .Call(betaTilde_R, beta, components, i, forward)
    }
    else {
        ans <- beta
        for (k in seq_along(components)) {
            if (k != i) {
                ## added 'forward' argument
                beta.hat <- betaHatComponent(component = components[[k]],
                                             forward = forward)
                ans <- ans - beta.hat
            }
        }
        ans
    }
}


## TRANSLATED # removed from C code
## HAS_TESTS
## treatment of division by zero based on that
## of function 'dlmFilter' in package 'dlm'
forwardFilter <- function(object, y, v, forward, useC = FALSE) {
    ## object
    stopifnot(is(object, "PolyComponent"))
    stopifnot(validObject(object))
    ## y
    stopifnot(is.double(y))
    stopifnot(!any(is.na(y)))
    ## v
    stopifnot(is.double(v))
    stopifnot(!any(is.na(v)))
    stopifnot(all(v >= 0))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    ## object and y
    stopifnot(identical(length(y), object@J))
    ## object and v
    stopifnot(identical(length(v), object@J))
    if (useC) {
        .Call(forwardFilter_R, object, y, v, forward)
    }
    else {
        J <- object@J
        q <- object@q
        G <- object@G  ## matrix
        F <- object@F  ## length J (starts at t = 1)
        W <- object@W  ## length J (starts at t = 1)
        m <- object@m  ## length J+1 (starts at t = 0)
        C <- object@CC ## length J+1 (starts at t = 0)
        UC <- object@UC ## length J+1 (starts at t = 0)
        DC <- object@DC ## length J+1 (starts at t = 0)
        DC.inv <- object@DCInv ## length J+1 (starts at t = 0)
        a <- object@a  ## length J (starts at t = 1)
        UR <- object@UR  ## length J (starts at t = 1)
        DR.inv <- object@DRInv  ## length J (starts at t = 1)
        for (j in seq_len(J)) {
            if (forward) {
                j0 <- j
                j1 <- j
                j2 <- j + 1L
            }
            else {
                j0 <- J - j + 2L
                j1 <- J - j + 1L
                j2 <- J - j + 1L
            }
            v.inv <- 1 / v[[j1]]
            if (is.infinite(v.inv))
                v.inv <- 0
            M.R <- rbind(diag(DC[[j0]], nrow = q) %*% t(UC[[j0]]) %*% t(G),
                         sqrt(W[[j1]]))
            svd.R <- svd(M.R, nu = 0)
            UR[[j1]] <- svd.R$v
            DR.inv[[j1]] <- 1 / svd.R$d
            DR.inv[[j1]][is.infinite(DR.inv[[j1]])] <- 0
            M.C <- rbind(sqrt(v.inv) * t(F[[j1]]) %*% UR[[j1]],
                         diag(DR.inv[[j1]], nrow = q))
            svd.C <- svd(M.C, nu = 0)
            UC[[j2]] <- UR[[j1]] %*% svd.C$v
            DC[[j2]] <- 1 / svd.C$d
            DC[[j2]][is.infinite(DC[[j2]])] <- 0
            DC.inv[[j2]] <- svd.C$d
            C[[j2]] <- UC[[j2]] %*% diag(DC[[j2]]^2, nrow = q) %*% t(UC[[j2]])
            a[[j1]] <- drop(G %*% m[[j0]])
            f.j <- sum(F[[j1]] * a[[j1]])  
            e.j <- y[j1] - f.j
            A.j <- drop(C[[j2]] %*% F[[j1]]) * v.inv
            m[[j2]] <- a[[j1]] + A.j * e.j
        }
        object@m <- m
        object@CC <- C
        object@UC <- UC
        object@DC <- DC
        object@DCInv <- DC.inv
        object@a <- a
        object@UR <- UR
        object@DRInv <- DR.inv
        object
    }
}

## Based on function 'dlmFilter' from package 'dlm'.
## Used only for testing.
forwardFilterTrend <- function(object, y, v) {
    J <- object@J
    G <- object@G
    W <- object@W  
    m <- object@m  
    C <- object@CC 
    UC <- object@UC
    DC <- object@DC
    DCInv <- object@DCInv
    a <- object@a
    UR <- object@UR
    DRInv <- object@DRInv
    for (j in seq_len(J)) {
        a[[j]] <- m[[j]] + c(m[[j]][-1], 0)
        tmp <- La.svd(rbind(DC[[j]] * t(G %*% UC[[j]]),
                            sqrt(W[[j]])), nu = 0)
        UR[[j]] <- t(tmp$vt)
        DRInv[[j]] <- 1 / tmp$d
        tmp <- La.svd(rbind(sqrt(1/v[j]) * UR[[j]][1,],
                            diag(DRInv[[j]], nrow = length(DRInv[[j]]))),
                      nu = 0)
        UC[[j+1]] <- UR[[j]] %*% t(tmp$vt)
        DC[[j+1]] <- 1 / tmp$d
        DCInv[[j+1]] <- tmp$d
        m[[j+1]] <- (a[[j]] + crossprod(diag(DC[[j+1]]) %*% t(UC[[j+1]]))[1,] *
                         (y[j]-a[[j]][1])/v[j])
        C[[j+1]] <- UC[[j+1]] %*% diag(DC[[j+1]]^2) %*% t(UC[[j+1]])
    }
    object@m <- m
    object@CC <- C
    object@UC <- UC
    object@DC <- DC
    object@DCInv <- DCInv
    object@a <- a
    object@UR <- UR
    object@DRInv <- DRInv
    object
}

## Based on function 'dlmFilter' from package 'dlm'.
## Used only for testing.
forwardFilterCovariates <- function(object, y, v) {
    J <- object@J
    F <- object@F
    W <- object@W
    m <- object@m
    C <- object@CC
    UC <- object@UC
    DC <- object@DC
    DCInv <- object@DCInv
    a <- object@a
    UR <- object@UR
    DRInv <- object@DRInv
    for (j in seq_len(J)) {
        a[[j]] <- m[[j]]
        tmp <- La.svd(rbind(diag(DC[[j]]) %*% t(UC[[j]]),
                            sqrt(W[[j]])),
                      nu = 0)
        UR[[j]] <- t(tmp$vt)
        DRInv[[j]] <- 1 / tmp$d
        tmp <- La.svd(rbind(sqrt(1/v[j]) * t(F[[j]]) %*% UR[[j]],
                            diag(DRInv[[j]], nrow = length(DRInv[[j]]))),
                      nu = 0)
        UC[[j+1]] <- UR[[j]] %*% t(tmp$vt)
        DC[[j+1]] <- 1 / tmp$d
        DCInv[[j+1]] <- tmp$d
        m[[j+1]] <- (a[[j]] +
                         drop(crossprod(diag(DC[[j+1]]) %*% t(UC[[j+1]])) %*% F[[j]]) *
                             (y[j]- sum(a[[j]] * F[[j]]))/v[j])
        C[[j+1]] <- UC[[j+1]] %*% diag(DC[[j+1]]^2) %*% t(UC[[j+1]])
    }
    object@m <- m
    object@CC <- C
    object@UC <- UC
    object@DC <- DC
    object@DCInv <- DCInv
    object@a <- a
    object@UR <- UR
    object@DRInv <- DRInv
    object
}

## Based on function 'dlmFilter' from package 'dlm'.
## Used only for testing.
forwardFilterSeasonal <- function(object, y, v) {
    J <- object@J
    G <- object@G
    F <- object@F
    W <- object@W
    m <- object@m
    C <- object@CC
    UC <- object@UC
    DC <- object@DC
    DCInv <- object@DCInv
    a <- object@a
    UR <- object@UR
    DRInv <- object@DRInv
    for (j in seq_len(J)) {
        a[[j]] <- drop(G %*% m[[j]])
        tmp <- La.svd(rbind(diag(DC[[j]]) %*% t(UC[[j]]) %*% t(G),
                            sqrt(W[[j]])),
                      nu = 0)
        UR[[j]] <- t(tmp$vt)
        DRInv[[j]] <- 1 / tmp$d
        tmp <- La.svd(rbind(sqrt(1/v[j]) * UR[[j]][1,],
                            diag(DRInv[[j]], nrow = length(DRInv[[j]]))),
                      nu = 0)
        UC[[j+1]] <- UR[[j]] %*% t(tmp$vt)
        DC[[j+1]] <- 1 / tmp$d
        DCInv[[j+1]] <- tmp$d
        m[[j+1]] <- (a[[j]] +
                         crossprod(diag(DC[[j+1]]) %*% t(UC[[j+1]]))[1,] *
                             (y[j]- a[[j]][1])/v[j])
        C[[j+1]] <- UC[[j+1]] %*% diag(DC[[j+1]]^2) %*% t(UC[[j+1]])
    }
    object@m <- m
    object@CC <- C
    object@UC <- UC
    object@DC <- DC
    object@DCInv <- DCInv
    object@a <- a
    object@UR <- UR
    object@DRInv <- DRInv
    object
}


## TRANSLATED # removed C code
## HAS_TESTS
minv2 <- function(m, useC = FALSE) {
    stopifnot(identical(dim(m), c(2L, 2L)))
    stopifnot(is.double(m))
    stopifnot(!any(is.na(m)))
    if (useC) {
        .Call(minv2_R, m)
    }
    else {
        kTolerance <- 1e-10
        det <- m[1L] * m[4L] - m[2L] * m[3L]
        if (abs(det) < kTolerance)
            stop("determinant of 'm' indistinguishable from 0")
        data <- c(m[4L], -m[2L], -m[3L], m[1L]) / det
        matrix(data, nrow = 2L, ncol = 2L)
    }
}



## TRANSLATED ## removed C code
## HAS_TESTS
transferGamma <- function(component, values, useC = FALSE) {
    ## component
    stopifnot(is(component, "PolyComponent"))
    stopifnot(!is(component, "BetaIsPredicted"))
    ## values
    stopifnot(is.double(values))
    ## component and values
    if (useC) {
        .Call(transferGamma_R, component, values)
    }
    else {
        q <- component@q
        J <- component@J
        gamma <- component@gamma
        first <- 1L
        for (i in seq_len(J + 1L)) {
            last <- first + q - 1L
            gamma[[i]] <- values[first : last]
            first <- first + q
        }
        component@gamma <- gamma
        component
    }
}



## TRANSLATED ## removed C code
## HAS_TESTS
transferGamma0 <- function(component, values, forward, useC = FALSE) {
    ## component
    stopifnot(is(component, "PolyComponent"))
    stopifnot(is(component, "OffsetsGamma0"))
    ## values
    stopifnot(is.double(values))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    ## component and values
    if (useC) {
        .Call(transferGamma0_R, component, values, forward)
    }
    else {
        J <- component@J
        gamma <- component@gamma
        offsets.gamma0 <- component@offsetsGamma0
        ## put last/first element of old 'gamma'
        ## into first/last position of new 'gamma'
        first <- offsets.gamma0[1L]
        last <- offsets.gamma0[2L]
        if (forward)
            gamma[[1L]] <- values[first : last]
        else
            gamma[[J + 1L]] <- values[first : last]
        component@gamma <- gamma
        component
    }
}


## TRANSLATED ## removed C code
## HAS_TESTS
transferParamDeltaAR11 <- function(prior, values, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR11"))
    stopifnot(!is(prior, "Forward")) ## should use transferDelta0AR11Predict
    stopifnot(validObject(prior)) 
    ## values
    stopifnot(is.double(values))
    if (useC) {
        .Call(transferParamDeltaAR11_R, prior, values)
    }
    else {    
        offsets <- prior@offsetsDelta
        first <- offsets[1L]
        last <- offsets[2L]
        prior@delta <- values[first : last]
        prior
    }
}


## TRANSLATED ## removed C code
## HAS_TESTS
## Put last/first elements of old 'delta'
## into first/last positions of new 'delta'
transferParamDelta0AR11Predict <- function(prior, values, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR11Predict"))
    stopifnot(validObject(prior)) 
    ## values
    stopifnot(is.double(values))
    if (useC) {
        .Call(transferParamDelta0AR11Predict_R, prior, values)
    }
    else {
        K <- prior@K
        L <- prior@L
        delta <- prior@delta
        offsets <- prior@offsetsDelta
        iter.pred <- prior@iteratorGamma
        iter.first <- prior@iteratorGammaEst
        forward <- prior@forward
        first <- offsets[1L]
        last <- offsets[2L]
        vals <- values[first : last]
        iter.pred <- resetA(iter.pred)
        iter.first <- resetA(iter.first)
        K.plus.1.first <- length(iter.first@indices)
        for (l in seq_len(L)) {
            indices.pred <- iter.pred@indices
            indices.first <- iter.first@indices
            if (forward) {
                i.pred <- indices.pred[1L]
                i.first <- indices.first[K.plus.1.first]
            }
            else {
                i.pred <- indices.pred[K + 1L]
                i.first <- indices.first[1L]
            }
            delta[i.pred] <- vals[i.first]
            iter.pred <- advanceA(iter.pred)
            iter.first <- advanceA(iter.first)
        }
        prior@delta <- delta
        prior
    }
}

## now update gammaNoInitial and gammaHat separately
## TRANSLATED ## removed C code
## HAS_TESTS
transferParamGammaAR1 <- function(prior, values, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR1"))
    stopifnot(!is(prior, "Forward")) ## should use transferGamma0AR1Predict
    stopifnot(validObject(prior)) 
    ## values
    stopifnot(is.double(values))
    if (useC) {
        .Call(transferParamGammaAR1_R, prior, values)
    }
    else {    
        offsets <- prior@offsetsGamma
        first <- offsets[1L]
        last <- offsets[2L]
        prior@gamma <- values[first : last]
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
## Put last/first elements of old 'gamma'
## into first/last positions of new 'gamma'
transferParamGamma0AR1Predict <- function(prior, values, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR1"))
    stopifnot(is(prior, "Forward"))
    stopifnot(validObject(prior)) 
    ## values
    stopifnot(is.double(values))
    if (useC) {
        .Call(transferParamGamma0AR1Predict_R, prior, values)
    }
    else {
        K <- prior@K
        L <- prior@L
        gamma <- prior@gamma
        offsets <- prior@offsetsGamma
        iter.pred <- prior@iteratorGamma
        iter.first <- prior@iteratorGammaEst
        forward <- prior@forward
        first <- offsets[1L]
        last <- offsets[2L]
        vals <- values[first : last]
        iter.pred <- resetA(iter.pred)
        iter.first <- resetA(iter.first)
        K.plus.1.first <- length(iter.first@indices)
        for (l in seq_len(L)) {
            indices.pred <- iter.pred@indices
            indices.first <- iter.first@indices
            if (forward) {
                i.pred <- indices.pred[1L]
                i.first <- indices.first[K.plus.1.first]
            }
            else {
                i.pred <- indices.pred[K + 1L]
                i.first <- indices.first[1L]
            }
            gamma[i.pred] <- vals[i.first]
            iter.pred <- advanceA(iter.pred)
            iter.first <- advanceA(iter.first)
        }
        prior@gamma <- gamma
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
transferParamPhiAR1 <- function(prior, values, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR1"))
    stopifnot(validObject(prior)) 
    ## values
    stopifnot(is.double(values))
    if (useC) {
        .Call(transferParamPhiAR1_R, prior, values)
    }
    else {
        phi.known <- prior@phiKnown
        if (!phi.known) {
            offsets <- prior@offsetsPhi
            first <- offsets[1L]
            prior@phi <- values[first]
        }
        prior
    }
}

## TRANSLATED ## removed C code
## HAS_TESTS
transferParamPriorV <- function(prior, values, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "Poly") || is(prior, "AR1"))
    stopifnot(validObject(prior))
    ## values
    stopifnot(is.double(values))
    stopifnot(!any(is.na(values)))
    if (useC) {
        .Call(transferParamPriorV_R, prior, values)
    }
    else {
        offsets <- prior@offsetsPriorV
        if (!is.null(offsets)) {
            priorV <- prior@priorV
            first <- offsets[1L]
            last <- offsets[2L]
            priorV <- transferParamPriorVarDLM(prior = priorV,
                                               values = values[first : last])
            prior@priorV <- priorV
        }
        prior
    }
}


## TRANSLATED ## removed C code
## HAS_TESTS
transferParamZetas <- function(model, filename, lengthIter, iteration,
                               useC = FALSE) {
    ## model
    stopifnot(is(model, "Zetas"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamZetas_R, model, filename, lengthIter, iteration)
    }
    else {
        offsets <- model@offsetsZetas
        first <- offsets[1L]
        last <- offsets[2L]
        zetas <- getOneIterFromFile(filename = filename,
                                    first = first,
                                    last = last,
                                    lengthIter = lengthIter,
                                    iteration = iteration)
        model@zetas <- zetas
        model
    }
}


## TRANSLATED ## removed C code
## HAS_TESTS
transferPriorsW <- function(component, values, useC = FALSE) {
    ## component
    stopifnot(is(component, "PolyComponent"))
    stopifnot(is(component, "OffsetsPriorsW"))
    ## values
    stopifnot(is.double(values))
    if (useC) {
        .Call(transferPriorsW_R, component, values)
    }
    else {
        q <- component@q
        priorsW <- component@priorsW
        offsets.priors.w <- component@offsetsPriorsW
        for (i in seq_len(q)) {
            offsets <- offsets.priors.w[[i]]
            if (!is.null(offsets)) {
                first <- offsets[1L]
                last <- offsets[2L]
                priorsW[[i]] <- transferParamPriorVarDLM(prior = priorsW[[i]],
                                                         values = values[first : last])
            }
        }
        component@priorsW <- priorsW
        component
    }
}


## TRANSLATED ## removed C code
## HAS_TESTS
transferParamPriorWAR1 <- function(prior, values, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR1"))
    stopifnot(validObject(prior)) 
    ## values
    stopifnot(is.double(values))
    if (useC) {
        .Call(transferParamPriorWAR1_R, prior, values)
    }
    else {
        priorW <- prior@priorW
        offsets <- prior@offsetsPriorW
        if (!is.null(offsets)) {
            first <- offsets[1L]
            last <- offsets[2L]
            priorW <- transferParamPriorVarDLM(prior = priorW,
                                               values = values[first : last])
            prior@priorW <- priorW
        }
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateUBetaExchRobustZero <- function(prior, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "Exch"))
    stopifnot(is(prior, "ExchRobustZero"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateUBetaExchRobustZero_R, prior)
    }
    else {    
        J <- prior@J@.Data
        U.beta.scaled <- prior@UBetaScaled@.Data
        A <- prior@ATau@.Data
        zeta <- prior@zeta
        U.beta <- double(J)
        for (j in seq_len(J))
            U.beta[j] <- A^2 * zeta^2 * U.beta.scaled[j]
        prior@UBeta@.Data <- U.beta
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateUBetaScaled <- function(prior, betaScaled, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "ExchRobustZero"))
    stopifnot(validObject(prior))
    ## betaScaled
    stopifnot(is.double(betaScaled))
    stopifnot(!any(is.na(betaScaled)))
    ## prior and betaScaled
    stopifnot(identical(length(betaScaled), as.integer(prior@J)))
    if (useC) {
        .Call(updateUBetaScaled_R, prior, betaScaled)
    }
    else {
        J <- prior@J@.Data
        tau <- prior@tauScaled@.Data
        U <- prior@UBetaScaled@.Data
        nu <- prior@nuBeta@.Data
        df <- nu + 1
        scale <- (nu * tau^2 + betaScaled^2) / df
        for (j in seq_len(J))
            U[j] <- rinvchisq1(df = df, scale = scale[j])
        prior@UBetaScaled@.Data <- U
        prior
    }
}



## TRANSLATED
## HAS_TESTS
predictUBetaScaled <- function(prior, useC = FALSE) {
    stopifnot(is(prior, "ExchRobustZero"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(predictUBetaScaled_R, prior)
    }
    else {
        J <- prior@J@.Data
        U <- prior@UBeta@.Data
        U.scaled <- prior@UBetaScaled@.Data
        A <- prior@ATau@.Data
        zeta <- prior@zeta
        div <- A^2 * zeta^2
        for (j in seq_len(J))
            U.scaled[j] <- U[j] / div
        prior@UBetaScaled@.Data <- U.scaled
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateTauScaledRobust <- function(prior, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "ExchRobustZero"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateTauScaledRobust_R, prior)
    }
    else {
        J <- prior@J@.Data
        nu <- prior@nuTau@.Data
        U <- prior@UBetaScaled@.Data
        shape <- J * nu / 2
        rate <- (nu/2) * sum(1/U)
        tau.sq.scaled <- rgamma(n = 1L, shape = shape, rate = rate)
        prior@tauScaled@.Data <- sqrt(tau.sq.scaled)
        prior
    }
}
