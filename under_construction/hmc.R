
setMethod("updateModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_PoissonVaryingNotUseExp_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  useHMC <- object@useHMC@.Data
                  object <- updateTheta_PoissonVaryingNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object)
                  object <- updateBetas(object)
                  object <- updateMu(object)
                  object <- updatePriorsBetas(object)
                  object <- updateMeansBetas(object)
                  object <- updateVariancesBetas(object)
                  object
              }
          })


updateBetas <- function(object) {
    use.hmc <- object@useHMC@.Data
    if (use.hmc)
        object <- updateBetasHMC(object)
    else
        object <- updateBetasGibbs(object)
    object
}


updateBetasGibbs <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateBetasGibbs_R, object) 
    }
    else {
        betas <- object@betas
        means.betas <- object@meansBetas
        variances.betas <- object@variancesBetas
        priors.betas <- object@priorsBetas
        beta.equals.mean <- object@betaEqualsMean
        sigma <- object@sigma
        log.post <- 0
        for (i.beta in seq_along(betas)) {
            if (beta.equals.mean[i])
                betas[[i.beta]] <- means.betas[[i.beta]]
            else {
                J <- length(betas[[i.beta]])
                l <- makeVBarAndN(object = object,
                                  iBeta = i.beta)
                vbar <- l$vbar # vector of length J
                n <- l$n # vector of length J
                mean.beta <- means.betas[[i.beta]]
                var.beta <- variances.betas[[i.beta]]
                prior.beta <- priors.betas[[i.beta]]
                all.struc.zero <- prior.beta@allStrucZero # vector of length J
                for (j in seq_len(J)) {
                    if (!all.struct.zero[j]) {
                        prec.data <- n[j] / sigma^2
                        prec.prior <- 1 / var.beta[j]
                        var.post <- 1 / (prec.data + prec.prior)
                        mean.post <- (prec.data * vbar[j] + prec.prior * mean.beta[j]) * var.post
                        sd.post <- sqrt(var.post)
                        betas[[i.beta]][j] <- stats::rnorm(n = 1L,
                                                           mean = mean.post,
                                                           sd = sd.post)
                        log.post <- log.post + stats::dnorm(betas[[i.beta]][j],
                                                            mean = mean.post,
                                                            sd = sd.post,
                                                            log = TRUE)
                    }
                }
            }
        }
        object@betas <- betas
        object@logPostBeas <- log.post
        object
    }
}



updatePriorsBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updatePriorsBetas_R, object) ## drop g
    }
    else {
        theta <- object@theta
        betas <- object@betas
        sigma <- object@sigma
        for (b in seq_along(betas)) {
            l <- makeVBarAndN(object, iBeta = b)
            vbar <- l[[1L]]
            n <- l[[2L]]
            l <- updateBetaAndPriorBeta(prior = object@priorsBetas[[b]],
                                        vbar = vbar,
                                        n = n,
                                        sigma = sigma)
            object@betas[[b]] <- l[[1L]]
            object@priorsBetas[[b]] <- l[[2L]]
        }
        object
    }
}
       
        
