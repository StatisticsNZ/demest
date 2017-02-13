
setMethod("predictPrior",
          signature(prior = "MixNormZero"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_MixNormZero_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictLevelComponentWeightMix(prior)
                  prior <- predictComponentWeightMix(prior)
                  prior <- updateWeightMix(prior)
                  XXX
              }
          })



## TRANSLATED
## HAS_TESTS
predictAlphaDLMNoTrend <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::is(prior, "NoTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(predictAlphaDLMNoTrend_R, prior)
    }
    else {
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data # numeric vector length (K+1)L
        phi <- prior@phi
        omega <- prior@omegaAlpha@.Data
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                k.curr <- indices[i + 1L]
                k.prev <- indices[i]
                mean <- phi * alpha[k.prev]
                alpha[k.curr] <- stats::rnorm(n = 1L,
                                       mean = mean,
                                       sd = omega)
            }
            iterator <- advanceA(iterator)
        }
        prior@alphaDLM@.Data <- alpha
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


                            
