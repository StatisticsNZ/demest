
updateThetaAndBeta_PoissonUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingUseExp"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
    ## exposure
    stopifnot(is.double(exposure))
    stopifnot(all(exposure@.Data[!is.na(exposure@.Data)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    if (useC) {
        .Call(updateThetaAndBeta_PoissonUseExp_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        theta.transformed <- object@thetaTransformed
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        mu <- object@mu
        sigma <- object@sigma@.Data
        betas <- object@betas
        indices.betas.update <- object@indicesBetasUpdate
        transforms.betas.update <- object@transformsBetasUpdate
        scales.betas.update <- object@scaleBetasUpdate@.Data
        max.attempt <- object@maxAttempt
        n.failed.betas.update <- 0L
        n.accept.betas.update <- 0L
        for (k in seq_along(indices.betas.update)) {
            beta <- betas[[indices.betas.update[k]]]
            J <- length(beta)
            scale <- scales.betas.update[k]
            transform <- transforms.betas.update[[k]]
            increment <- stats::rnorm(n = 1L,
                                      mean = 0,
                                      sd = scale)
            for (j in seq_len(J)) {
                indices.theta <- dembase:::getIBefore(j,
                                                      transform = tranform,
                                                      useC = TRUE)
                n.indices.theta <- length(indices.theta)
                vec.th.curr <- theta[indices.theta]
                vec.th.tr.curr <- theta.transformed[indices.theta]
                vec.th.prop <- numeric(length = n.indices.theta)
                vec.th.tr.prop <- numeric(length = n.indices.theta)
                all.valid <- TRUE
                log.diff.lik <- 0
                log.diff.prior.th <- 0
                log.diff.beta <- 0
                for (i in seq_len(n.indices.theta)) {
                    tr.th.prop <- vec.tr.th.curr[i] + increment
                    inside.limits <- ((tr.th.prop > (lower + tolerance))
                                      && (tr.th.prop < (upper - tolerance)))
                    if (!inside.limits) {
                        all.valid <- FALSE
                        break
                    }
                    vec.tr.th.prop[i] <- tr.th.prop
                    th.prop <- exp(tr.th.prop)
                    vec.th.prop[i] <- th.prop
                    tr.diff.lik <- 

                }
                if (!all.valid) {
                    n.failed.prop.value.ag <- n.failed.prop.value.ag + 1L
                    next
                }
                
            vec.y <- y[i.ag]
            is.observed <- !is.na(vec.y)
            vec.exp <- exposure[i.ag]
            vec.mu <- mu[i.ag]
            vec.weight <- weight.ag[i.ag]
            ag.curr <- value.ag[k]
            ag.prop <- sum(vec.th.prop * vec.weight)
            mean.k <- mean.ag[k]
            sd.k <- sd.ag[k]
            log.diff.lik <- (sum(stats::dpois(x = vec.y[is.observed],
                                       lambda = vec.th.prop[is.observed] * vec.exp[is.observed],
                                       log = TRUE))
                             - sum(stats::dpois(x = vec.y[is.observed],
                                         lambda = vec.th.curr[is.observed] * vec.exp[is.observed],
                                         log = TRUE)))
            ## do not include Jacobians, since they cancel with proposal densities
            log.diff.prior <- (sum(stats::dnorm(x = vec.log.th.prop,
                                         mean = vec.mu,
                                         sd = sigma,
                                         log = TRUE))
                               - sum(stats::dnorm(x = vec.log.th.curr,
                                           mean = vec.mu,
                                           sd = sigma,
                                           log = TRUE)))
            log.diff.ag <- (stats::dnorm(x = mean.k, 
                                  mean = ag.prop,
                                  sd = sd.k,
                                  log = TRUE)
                            - stats::dnorm(x = mean.k, 
                                    mean = ag.curr,
                                    sd = sd.k,
                                    log = TRUE))
            log.diff <- log.diff.lik + log.diff.prior + log.diff.ag
            accept <- (log.diff > 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.ag <- n.accept.ag + 1L
                value.ag[k] <- ag.prop
                theta[i.ag] <- vec.th.prop
                theta.transformed[i.ag] <- vec.log.th.prop
            }
        }
        object@theta <- theta
        object@thetaTransformed <- theta.transformed
        object@valueAg@.Data <- value.ag
        object@nFailedPropValueAg@.Data <- n.failed.prop.value.ag
        object@nAcceptAg@.Data <- n.accept.ag
        object
    }
}
