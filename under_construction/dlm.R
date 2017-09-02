


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
                  beta.has.errors <- prior@betaHasErrors@.Data
                  level.has.errors <- prior@levelHasErrors@.Data
                  is.saturated <- prior@isSaturated@.Data
                  if (is.saturated) {
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = vbar)
                      beta <- betaHat(prior)
                      prior@tau@.Data <- sigma
                  }
                  else {
                      beta <- updateBeta(prior = prior,
                                         vbar = vbar,
                                         n = n,
                                         sigma = sigma)
                      prior <- updateAlphaDLMNoTrend(prior = prior,
                                                     betaTilde = beta)
                      prior <- updateTauNorm(prior = prior,
                                             beta = beta)
                  }
                  prior <- updatePhi(prior = prior,
                                     withTrend = FALSE)
                  prior <- updateOmegaAlpha(prior = prior,
                                            withTrend = FALSE)
                  list(beta, prior)
              }
          })


## TRANSLATED
## HAS_TESTS
updateAlphaDLMNoTrend <- function(prior, betaTilde, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "NoTrendMixin"))
    stopifnot(methods::validObject(prior))
    ## betaTilde
    stopifnot(is.double(betaTilde))
    stopifnot(!any(is.na(betaTilde)))
    stopifnot(identical(length(betaTilde), prior@J@.Data))
    if (useC) {
        .Call(updateAlphaDLMNoTrend_R, prior, betaTilde)
    }
    else {    
        beta.has.errors <- prior@betaHasErrors@.Data
        level.has.errors <- prior@levelHasErrors@.Data
        if (beta.has.errors && level.has.errors) {
            alpha <- FFBSNoTrend()
        }
        else if (beta.has.errors && !level.has.errors) {
            NULL ## should not get this case

        }
        else if (!beta.has.errors && level.has.errors) {
            prior <- updateAlphaNoTrendLevelHasErrors(prior)
        }
        
FFBSNoTrend <- function(
        K <- prior@K@.Data
        L <- prior@L@.Data
        
        
        update.series <- prior@updateSeriesDLM # logical vector length L
        alpha <- prior@alphaDLM@.Data # numeric vector length (K+1)L
        m <- prior@mNoTrend@.Data     # list length K+1
        m0 <- prior@m0NoTrend@.Data   # list length L
        C <- prior@CNoTrend@.Data     # list length K+1
        a <- prior@aNoTrend@.Data     # list length K
        R <- prior@RNoTrend@.Data     # list length K
        phi <- prior@phi
        phi.sq <- phi^2
        omega <- prior@omegaAlpha@.Data
        omega.sq <- omega^2
        v <- getV(prior)              # numeric vector length KL
        iterator.a <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.a <- resetA(iterator.a)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (update.series[l]) { ## NEW
                indices.a <- iterator.a@indices
                indices.v <- iterator.v@indices
                m[[1L]] <- m0[[l]]
                ## forward filter
                for (i in seq_len(K)) {
                    a[[i]] <- phi * m[[i]]
                    R[[i]] <- phi.sq * C[[i]] + omega.sq
                    q <- R[[i]] + v[indices.v[i]]
                    e <- betaTilde[indices.v[i]] - a[[i]]
                    A <- R[[i]] / q
                    m[[i + 1L]] <- a[[i]] + A * e
                    C[[i + 1L]] <- R[[i]] - A^2 * q
                }
                ## draw gamma_K
                alpha[indices.a[K + 1L]] <- stats::rnorm(n = 1L,
                                                         mean = m[[K + 1L]], 
                                                         sd = sqrt(C[[K + 1L]]))
                ## backward smooth
                for (i in seq.int(from = K - 1L, to = 0L)) {
                    B <- C[[i + 1L]] * phi / R[[i + 1L]]
                    m.star <- m[[i + 1L]] + B * (alpha[indices.a[i + 2L]] - a[[i + 1L]])
                    C.star <- C[[i + 1L]] - B^2 * R[[i + 1L]]
                    alpha[indices.a[i + 1L]] <- stats::rnorm(n = 1L, mean = m.star, sd = sqrt(C.star))
                }
            } ## NEW
            iterator.a <- advanceA(iterator.a)
            iterator.v <- advanceA(iterator.v)
        }
        prior@alphaDLM@.Data <- alpha
        prior
    }
}

