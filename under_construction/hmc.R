

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


updateGradientBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateGradientBetas_R, object) 
    }
    else {
        gradient.betas <- object@gradientBetas
        mu <- object@mu
        iterator <- object@iteratorBetas
        n <- length(mu)
        for (i.mu in seq_len(n)) {
            indices <- iterator@indices
            include.cell <- cell.in.lik[i.mu]
            if (include.cell) {
                indices <- iterator@indices
                for (i.beta in seq_along(gradient.betas)) {
                    gradient.betas[[i.beta]][indices[i.beta]] <- (gradient.betas[[i.beta]][indices.i.beta]
                        + mu[i] - transformed.theta[i]
                    }
                }
            }
        }
    }
    object
}


updateBetas <- function(object) {
    betas.curr <- object@betas
    n.step.beta.hmc <- object@nStepBetaHMC
    object <- initializeMomentum(object)
    log.post.betas.curr <- logPostBetas(object)
    log.post.momentum.curr <- logPostMomentum(object)
    object <- updateMomentumOneStep(object,
                                    mult = 0.5)
    for (i in seq_len(n.update.beta.hmc)) {
        mult <- if (i < n.update.beta.hmc) 1 else 0.5
        object <- updateBetasOneStep(object)
        object <- updateMu(object)
        object <- updateGradientBetas(object)
        object <- updateMomentumOneStep(object,
                                        mult = mult)
    }
    log.post.betas.prop <- logPostBetas(object)
    log.post.momentum.prop <- logPostMomentum(object)
    log.diff <- (log.post.betas.prop + log.post.momentum.prop
        - log.post.betas.curr - log.post.momentum.curr)
    accept <- (log.diff >= 0) || (stats::runif(n = 1L) > exp(log.diff))
    if (accept) {
        object@nAcceptBeta@.Data <- object@nAcceptBeta@.Data + 1L
    }
    else {
        object@betas <- betas.curr
        object <- updateMu(object)
    }
    object
}
        
initializeMomentum <- function(momentum, sdMomentum) {
    for (i in seq_len(momentum)) {
        n <- length(momentum[[i]])
        momentum[[i]] <- rnorm(n = n, mean = 0, sd = sdMomentum)
    }
    momentum
}

    

    
