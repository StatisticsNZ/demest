


updateAlphaICAR <- function(prior, betaTilde, useC = FALSE) {
    J <- prior@J
    alpha <- prior@alphaICAR@.Data
    count <- prior@neighboursCount
    index <- prior@neighboursIndex
    weight <- prior@neighboursWeight
    for (j in seq_len(J)) {
        n.j <- count[j]
        i.j <- index[[j]]
        w.j <- weight[[j]]
        mean <- 0
        for (k in seq_len(n.j)) {
            mean <- mean + w.j[k] * alpha[i.j[k]]
            sd <- XXX
        }
        alpha[j] <- rnorm(n = 1L,
                          mean = mean,
                          sd = sd)
    }
    prior@alphaICAR@.Data <- alpha
    prior
}


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
                  prior <- updateAlphaICAR(prior = prior,
                                           betaTilde = beta)
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  list(beta, prior)
              }
          })
