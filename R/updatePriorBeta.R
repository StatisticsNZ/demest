
## ExchFixed

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "ExchFixed"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_ExchFixed_R, prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R, prior, beta, thetaTransformed, sigma)
              }
              else {
                  prior
              }
          })



## Exch

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "ExchNormZero"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_ExchNormZero_R, prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R, prior, beta, thetaTransformed, sigma)
              }
              else {
                  J <- prior@J@.Data
                  is.saturated <- prior@isSaturated@.Data
                  if (is.saturated) {
                      prior@tau@.Data <- sigma
                  }
                  else {
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "ExchRobustZero"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_ExchRobustZero_R, prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R, prior, beta, thetaTransformed, sigma)
              }
              else {
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "ExchNormCov"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_ExchNormCov_R, prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R, prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data
                  if (is.saturated) {
                      prior@tau@.Data <- sigma
                      prior <- updateEta(prior = prior,
                                         beta = thetaTransformed)
                  }
                  else {
                      prior <- updateEta(prior = prior,
                                         beta = beta)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updateUEtaCoef(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "ExchRobustCov"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_ExchRobustCov_R, prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R, prior, beta, thetaTransformed, sigma)
              }
              else {
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateEta(prior = prior,
                                     beta = beta)
                  prior <- updateUEtaCoef(prior)
                  prior
              }
          })


## DLM - Norm, Zero

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMNoTrendNormZeroNoSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMNoTrendNormZeroNoSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data
                  if (is.saturated) {
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = thetaTransformed)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = beta)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMWithTrendNormZeroNoSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMWithTrendNormZeroNoSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data 
                  if (is.saturated) {
                      prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                            betaTilde = thetaTransformed)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                            betaTilde = beta)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMNoTrendNormZeroWithSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMNoTrendNormZeroWithSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data 
                  if (is.saturated) {
                      beta.tilde <- thetaTransformed - betaHatSeason(prior)
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = beta.tilde)
                      beta.tilde <- thetaTransformed - betaHatAlphaDLM(prior)
                      prior <- updateSeason(prior = prior,
                                            betaTilde = beta.tilde)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      beta.tilde <- beta - betaHatSeason(prior)
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = beta.tilde)
                      beta.tilde <- beta - betaHatAlphaDLM(prior)
                      prior <- updateSeason(prior = prior,
                                            betaTilde = beta.tilde)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior <- updateOmegaSeason(prior = prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMWithTrendNormZeroWithSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMWithTrendNormZeroWithSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data 
                  if (is.saturated) {
                      beta.tilde <- thetaTransformed - betaHatSeason(prior)
                      prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                            betaTilde = beta.tilde)
                      beta.tilde <- thetaTransformed - betaHatAlphaDLM(prior)
                      prior <- updateSeason(prior = prior,
                                            betaTilde = beta.tilde)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      beta.tilde <- beta - betaHatSeason(prior)
                      prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                            betaTilde = beta.tilde)
                      beta.tilde <- beta - betaHatAlphaDLM(prior)
                      prior <- updateSeason(prior = prior,
                                            betaTilde = beta.tilde)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior <- updateOmegaSeason(prior = prior)
                  prior
              }
          })

## Norm, Cov

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMNoTrendNormCovNoSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMNoTrendNormCovNoSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data 
                  if (is.saturated) {
                      beta.tilde <- thetaTransformed - betaHatCovariates(prior)
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = beta.tilde)
                      beta.tilde <- thetaTransformed - betaHatAlphaDLM(prior)
                      prior <- updateEta(prior = prior,
                                         beta = beta.tilde)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      beta.tilde <- beta - betaHatCovariates(prior)
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = beta.tilde)
                      beta.tilde <- beta - betaHatAlphaDLM(prior)
                      prior <- updateEta(prior = prior,
                                         beta = beta.tilde)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMWithTrendNormCovNoSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMWithTrendNormCovNoSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data 
                  if (is.saturated) {
                      beta.tilde <- thetaTransformed - betaHatCovariates(prior)
                      prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                            betaTilde = beta.tilde)
                      beta.tilde <- thetaTransformed - betaHatAlphaDLM(prior)
                      prior <- updateEta(prior = prior,
                                         beta = beta.tilde)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      beta.tilde <- beta - betaHatCovariates(prior)
                      prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                            betaTilde = beta.tilde)
                      beta.tilde <- beta - betaHatAlphaDLM(prior)
                      prior <- updateEta(prior = prior,
                                         beta = beta.tilde)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }                  
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMNoTrendNormCovWithSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMNoTrendNormCovWithSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data 
                  if (is.saturated) {
                      beta.tilde <- thetaTransformed - betaHatSeason(prior) - betaHatCovariates(prior)
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = beta.tilde)
                      beta.tilde <- thetaTransformed - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                      prior <- updateSeason(prior = prior,
                                            betaTilde = beta.tilde)
                      beta.tilde <- thetaTransformed - betaHatAlphaDLM(prior) - betaHatSeason(prior) 
                      prior <- updateEta(prior = prior,
                                         beta = beta.tilde)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      beta.tilde <- beta - betaHatSeason(prior) - betaHatCovariates(prior)
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = beta.tilde)
                      beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                      prior <- updateSeason(prior = prior,
                                            betaTilde = beta.tilde)
                      beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatSeason(prior) 
                      prior <- updateEta(prior = prior,
                                         beta = beta.tilde)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior <- updateOmegaSeason(prior = prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMWithTrendNormCovWithSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMWithTrendNormCovWithSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data 
                  if (is.saturated) {
                      beta.tilde <- thetaTransformed - betaHatSeason(prior) - betaHatCovariates(prior)
                      prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                            betaTilde = beta.tilde)
                      beta.tilde <- thetaTransformed - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                      prior <- updateSeason(prior = prior,
                                            betaTilde = beta.tilde)
                      beta.tilde <- thetaTransformed - betaHatAlphaDLM(prior) - betaHatSeason(prior) 
                      prior <- updateEta(prior = prior,
                                         beta = beta.tilde)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      beta.tilde <- beta - betaHatSeason(prior) - betaHatCovariates(prior)
                      prior <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                            betaTilde = beta.tilde)
                      beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatCovariates(prior)
                      prior <- updateSeason(prior = prior,
                                            betaTilde = beta.tilde)
                      beta.tilde <- beta - betaHatAlphaDLM(prior) - betaHatSeason(prior)                  
                      prior <- updateEta(prior = prior,
                                         beta = beta.tilde)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updateUEtaCoef(prior)
                  prior <- updatePhi(prior = prior,
                                     withTrend = TRUE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = TRUE)
                  prior <- updateOmegaDelta(prior = prior)
                  prior <- updateGWithTrend(prior = prior)
                  prior <- updateWSqrt(prior = prior)
                  prior <- updateWSqrtInvG(prior = prior)
                  prior <- updateOmegaSeason(prior = prior)
                  prior
              }
          })


## Robust, Zero

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMNoTrendRobustZeroNoSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMNoTrendRobustZeroNoSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  prior <- updateAlphaDLMNoTrend(prior = prior,
                                                 betaTilde = beta)
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateUBeta(prior = prior,
                                       beta = beta)
                  prior <- updateTauRobust(prior = prior)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMWithTrendRobustZeroNoSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMWithTrendRobustZeroNoSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
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
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMNoTrendRobustZeroWithSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMNoTrendRobustZeroWithSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
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
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMWithTrendRobustZeroWithSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMWithTrendRobustZeroWithSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
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
                  prior <- updateOmegaSeason(prior = prior)
                  prior
              }
          })


## Robust, Cov

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMNoTrendRobustCovNoSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMNoTrendRobustCovNoSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
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
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMWithTrendRobustCovNoSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMWithTrendRobustCovNoSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
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
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMNoTrendRobustCovWithSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMNoTrendRobustCovWithSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
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
                  prior <- updateOmegaSeason(prior = prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "DLMWithTrendRobustCovWithSeason"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_DLMWithTrendRobustCovWithSeason_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
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
                  prior
              }
          })



## ICAR #################################################################################


## Known ###############################################################################

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "KnownCertain"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_KnownCertain_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "KnownUncertain"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                          thetaTransformed = thetaTransformed,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_KnownUncertain_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  prior
              }
          })


## Mix #################################################################################

## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "MixNormZero"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_MixNormZero_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  is.saturated <- prior@isSaturated@.Data 
                  if (is.saturated) {
                      ## tau
                      prior@tau@.Data <- sigma
                      beta.tilde <- thetaTransformed
                  }
                  else {
                      ## tau
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta) # sigma-delta
                      beta.tilde <- beta
                  }
                  ## vectors
                  prior <- updateVectorsMixAndProdVectorsMix(prior = prior,
                                                             betaTilde = beta.tilde) # psi
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
                                               betaTilde = beta.tilde) # k
                  prior <- updateIndexClassMaxUsedMix(prior) # k-star; deterministic
                  prior <- updateLevelComponentWeightMix(prior) # alpha
                  prior <- updateMeanLevelComponentWeightMix(prior) # mu
                  prior <- updatePhiMix(prior) # phi
                  prior <- updateAlphaMix(prior)
                  prior
              }
          })


## Zero #################################################################################


## TRANSLATED
## HAS_TESTS
setMethod("updatePriorBeta",
          signature(prior = "Zero"),
          function(prior, beta, thetaTransformed, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdatePriorBeta(prior = prior,
                                   thetaTransformed = thetaTransformed,
                                   sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updatePriorBeta_Zero_R,
                            prior, beta, thetaTransformed, sigma)
                  else
                      .Call(updatePriorBeta_R,
                            prior, beta, thetaTransformed, sigma)
              }
              else {
                  prior
              }
          })
