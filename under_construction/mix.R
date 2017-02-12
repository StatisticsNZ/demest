
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
                                         beta = beta)
                  ## vectors
                  prior <- updateVectorsMixAndProdVectorsMix(prior = prior,
                                                             betaTilde = beta) # psi
                  prior <- updateOmegaVectorsMix(prior) # sigma
                  ## weights
                  prior <- updateLatentComponentWeightMix(prior) # z
                  prior <- updateComponentWeightMix(prior) # W
                  prior <- updateWeightMix(prior) # v; deterministic
                  prior <- updateLatentWeightMix(prior) # u
                  prior <- updateIndexClassMaxPossibleMix(prior) # k-tilde
                  prior <- updateIndexClassMix(prior = prior,
                                               betaTilde = beta) # k
                  prior <- updateIndexClassMaxUsedMix(prior) # k-star; deterministic
                  prior <- updateLevelComponentWeightMix(prior) # alpha
                  prior <- updateMeanLevelComponentWeightMix(prior) # mu
                  prior <- updatePhiMix(prior) # phi
                  ## return
                  list(beta, prior)
              }
          })












updateAlphaMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaMix)
    if (useC) {
        .Call(updateAlphaMix_R, prior)
    }
    else {
        alphaMix <- prior@alphaMix@.Data
        J <- prior@J@.Data
        index <- prior@indexClassMix@.Data
        prodVectors <- prior@prodVectorsMix@.Data
        for (i in seq_len(J))
            alphaMix[i] <- prodVectors[index[i]]
        prior@alphaMix@.Data <- alphaMix
        prior
    }
}

betaHatAlphaMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaMix)
    if (useC) {
        .Call(betaHatAlphaMix_R, prior)
    }
    else {
        prior@alphaMix@.Data
    }
}


                            
