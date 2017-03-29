
## ExchFixed

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "ExchFixed"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_ExchFixed_R, prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
              }
              else {
                  J <- prior@J@.Data
                  tau <- prior@tau@.Data
                  prec.data <- n / sigma^2
                  prec.prior <- 1 / tau^2
                  var <- 1 / (prec.data + prec.prior) 
                  mean <- prec.data * vbar * var # vector
                  sd <- sqrt(var) # scalar
                  beta <- stats::rnorm(n = J, mean = mean, sd = sd)
                  list(beta, prior)
              }
          })


## Exch

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
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n  = n,
                                     sigma = sigma)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  list(beta, prior)
              }
          })



## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "ExchRobustZero"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_ExchRobustZero_R, prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n  = n,
                                     sigma = sigma)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior)
                  list(beta, prior)
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "ExchNormCov"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_ExchNormCov_R, prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  prior <- updateEta(prior = prior,
                                     beta = beta)
                  prior <- updateUEtaCoef(prior)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "ExchRobustCov"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_ExchRobustCov_R, prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta)
                  prior <- updateUEtaCoef(prior)
                  list(beta, prior)
              }
          })


## DLM - Norm, Zero

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMNoTrendNormZeroNoSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMNoTrendNormZeroNoSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendNormZeroNoSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendNormZeroNoSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMNoTrendNormZeroWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMNoTrendNormZeroWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior)
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior <- updateOmegaSeason(prior = prior)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendNormZeroWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendNormZeroWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior <- updateOmegaSeason(prior = prior)
                  list(beta, prior)
              }
          })


## Norm, Cov

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMNoTrendNormCovNoSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMNoTrendNormCovNoSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatCovariates(prior)
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendNormCovNoSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendNormCovNoSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatCovariates(prior)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMNoTrendNormCovWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMNoTrendNormCovWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior) - betaHatCovariates(prior)
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatSeason(prior) 
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior <- updateOmegaSeason(prior = prior)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendNormCovWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendNormCovWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior) - betaHatCovariates(prior)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatSeason(prior)                  
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior <- updateOmegaSeason(prior = prior)
                  list(beta, prior)
              }
          })


## Robust, Zero

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMNoTrendRobustZeroNoSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMNoTrendRobustZeroNoSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendRobustZeroNoSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendRobustZeroNoSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMNoTrendRobustZeroWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMNoTrendRobustZeroWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior)
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior <- updateOmegaSeason(prior = prior)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendRobustZeroWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendRobustZeroWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior <- updateOmegaSeason(prior = prior )
                  list(beta, prior)
              }
          })


## Robust, Cov

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMNoTrendRobustCovNoSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMNoTrendRobustCovNoSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatCovariates(prior)
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendRobustCovNoSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendRobustCovNoSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatCovariates(prior)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMNoTrendRobustCovWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMNoTrendRobustCovWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior) - betaHatCovariates(prior)
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatSeason(prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior <- updateOmegaSeason(prior = prior )
                  list(beta, prior)
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "DLMWithTrendRobustCovWithSeason"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_DLMWithTrendRobustCovWithSeason_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  beta.tilde <- beta - betaHatSeason(prior) - betaHatCovariates(prior)
                  prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                  prior <- updateSeason(prior = prior,
                                        betaTilde = beta.tilde)
                  beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatSeason(prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta.tilde)
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior <- updateOmegaSeason(prior = prior )
                  list(beta, prior)
              }
          })



## ICAR #################################################################################


## Known ###############################################################################

setMethod("updateBetaAndPriorBeta",
          signature(prior = "KnownCertain"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_KnownCertain_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  alpha <- object@alphaKnown@.Data
                  beta <- alpha
                  list(beta, prior)
              }
          })

setMethod("updateBetaAndPriorBeta",
          signature(prior = "KnownUncertain"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_KnownCertain_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  alpha <- object@alphaKnown@.Data
                  beta <- alpha
                  list(beta, prior)
              }
          })



## Mix #################################################################################

## TRANSLATED
## HAS_TESTS
setMethod("updateBetaAndPriorBeta",
          signature(prior = "MixNormZero"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_MixNormZero_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  ## beta
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  ## tau
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta) # sigma-delta
                  ## vectors
                  prior <- updateVectorsMixAndProdVectorsMix(prior = prior,
                                                             betaTilde = beta) # psi
                  prior <- updateOmegaVectorsMix(prior) # sigma-e
                  ## weights
                  prior <- updateLatentComponentWeightMix(prior) # z
                  prior <- updateComponentWeightMix(prior) # W
                  prior <- updateWeightMix(prior) # v; deterministic
                  prior <- updateLatentWeightMix(prior) # u
                  prior <- updateOmegaComponentWeightMix(prior) # sigma-epsilon
                  prior <- updateOmegaLevelComponentWeightMix(prior) # sigma-eta 
                  prior <- updateIndexClassMaxPossibleMix(prior) # k-tilde
                  prior <- updateIndexClassMix(prior = prior,
                                               betaTilde = beta) # k
                  prior <- updateIndexClassMaxUsedMix(prior) # k-star; deterministic
                  prior <- updateLevelComponentWeightMix(prior) # alpha
                  prior <- updateMeanLevelComponentWeightMix(prior) # mu
                  prior <- updatePhiMix(prior) # phi
                  prior <- updateAlphaMix(prior)
                  ## return
                  list(beta, prior)
              }
          })


## Move #################################################################################

## setMethod("updateBetaAndPriorBeta",
##           signature(prior = "MoveNormZero"),
##           function(prior, vbar, sigma, useC = FALSE, useSpecific = FALSE) {
##               checkUpdateBetaAndPriorBeta(prior = prior,
##                                           vbar = vbar,
##                                           sigma = sigma)
##               if (useC) {
##                   if (useSpecific)
##                       .Call(updateBetaAndPriorBeta_MoveNormZero_R, prior, vbar, sigma)
##                   else
##                       .Call(updateBetaAndPriorBeta_R, prior, vbar, sigma)
##               }
##               else {
##                   beta <- updateBeta(prior = prior,
##                                      vbar = vbar,
##                                      sigma = sigma)
##                   prior <- updateAlphaMove(prior = prior,
##                                            betaTilde = beta)
##                   prior <- updateTauNorm(prior = prior,
##                                        beta = beta)
##                   list(beta, prior)
##               }
##           })

## Known #################################################################################


## setMethod("updateBetaAndPriorBeta",
##           signature(prior = "KnownCertain"),
##           function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
##               checkUpdateBetaAndPriorBeta(prior = prior,
##                                           vbar = vbar,
##                                           n = n,
##                                           sigma = sigma)
##               if (useC) {
##                   if (useSpecific)
##                       .Call(updateBetaAndPriorBeta_KnownCertain_R, prior, vbar, n, sigma)
##                   else
##                       .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
##               }
##               else {
##                   v <- getV(prior)
##                   prec.data <- n / sigma^2
##                   prec.prior <- 1 / v
##                   var <- 1 / (prec.data + prec.prior)
##                   beta.hat <- betaHat(prior)
##                   mean <- (prec.data * vbar + prec.prior * beta.hat) * var
##                   sd <- sqrt(var)
##                   stats::rnorm(n = J, mean = mean, sd = sd)
##                   beta <- updateBetaKnownCertain(beta = beta,
##                                                  prior = prior)
##                   list(beta, prior)
##               }
##           })


## setMethod("updateBetaAndPriorBeta",
##           signature(prior = "KnownUncertain"),
##           function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
##               checkUpdateBetaAndPriorBeta(prior = prior,
##                                           vbar = vbar,
##                                           n = n,
##                                           sigma = sigma)
##               if (useC) {
##                   if (useSpecific)
##                       .Call(updateBetaAndPriorBeta_KnownUncertain_R, prior, vbar, n, sigma)
##                   else
##                       .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
##               }
##               else {
##                   alpha <- prior@alphaKnown
##                   beta <- alpha
##                   list(beta, prior)
##               }
##           })






















## ICAR #################################################################################

## Cross #################################################################################

## Known #################################################################################


## setMethod("updateBetaAndPriorBeta",
##           signature(prior = "KnownCertain"),
##           function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
##               checkUpdateBetaAndPriorBeta(prior = prior,
##                                           vbar = vbar,
##                                           n = n,
##                                           sigma = sigma)
##               if (useC) {
##                   if (useSpecific)
##                       .Call(updateBetaAndPriorBeta_KnownCertain_R, prior, vbar, n, sigma)
##                   else
##                       .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
##               }
##               else {
##                   v <- getV(prior)
##                   prec.data <- n / sigma^2
##                   prec.prior <- 1 / v
##                   var <- 1 / (prec.data + prec.prior)
##                   beta.hat <- betaHat(prior)
##                   mean <- (prec.data * vbar + prec.prior * beta.hat) * var
##                   sd <- sqrt(var)
##                   stats::rnorm(n = J, mean = mean, sd = sd)
##                   beta <- updateBetaKnownCertain(beta = beta,
##                                                  prior = prior)
##                   list(beta, prior)
##               }
##           })


## setMethod("updateBetaAndPriorBeta",
##           signature(prior = "KnownUncertain"),
##           function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
##               checkUpdateBetaAndPriorBeta(prior = prior,
##                                           vbar = vbar,
##                                           n = n,
##                                           sigma = sigma)
##               if (useC) {
##                   if (useSpecific)
##                       .Call(updateBetaAndPriorBeta_KnownUncertain_R,
##                             prior, vbar, n, sigma)
##                   else
##                       .Call(updateBetaAndPriorBeta_R,
##                             prior, vbar, n, sigma)
##               }
##               else {
##                   alpha <- prior@alphaKnown
##                   beta <- alpha
##                   list(beta, prior)
##               }
##           })
