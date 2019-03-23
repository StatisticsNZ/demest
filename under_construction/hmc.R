



## NO_TESTS
setClass("MomentumMixin",
         slots = c(momentum = "list"),
         contains = "VIRTUAL", 
         validity = function(object) {
             momentum <- object@momentum
             betas <- object@momentum
             ## 'momentum' has same number of elements as 'betas'
             if (!identical(length(momentum), length(betas)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "momentum", "betas"))
             ## corresponding elements of 'momentum' and 'betas' have same length
             for (i in seq_along(momentum))
                 if (!identical(length(momentum[[i]]), length(betas[[i]])))
                     return(gettextf("element %d of '%s' and element %d of '%s' have different lengths",
                                     i, "momentum", i, "betas"))
             ## all elements of 'momentum' have type "double"
             if (!all(sapply(momentum, is.double)))
                 return(gettextf("'%s' has elements not of type \"%s\"",
                                 "momentum", "double"))
             ## 'momentum' has no missing values
             for (i in seq_along(momentum))
                 if (any(is.na(momentum[[i]])))
                     return(gettextf("element %d of '%s' has missing values",
                                     i, "momentum"))
             ## 'momentum' does not have names
             if (!is.null(names(momentum)))
                 return(gettextf("'%s' has names",
                                 "momentum"))
             TRUE
         })







## HAS_TESTS
updateThetaTransformed <- function(object, g, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    ## g
    stopifnot(is.function(g))
    if (useC) {
        .Call(updateThetaTransformed_R, object)
    }
    else {
        theta.transformed <- object@thetaTransformed
        theta <- object@theta
        cell.in.lik <- object@cellInLik
        if (identical(g, log)) { 
            box.cox.param <- object@boxCoxParam 
            uses.box.cox.transform <- box.cox.param > 0 
        } 
        else 
            uses.box.cox.transform <- FALSE 
        for (i in seq_along(theta)) {
            if (cell.in.lik[i]) {
                if (uses.box.cox.transform) 
                    transformed.theta[i] <- (theta[i] ^ box.cox.param - 1) / box.cox.param 
                else 
                    transformed.theta[i] <- g(theta[i])
            }
        }
        object@thetaTransformed <- theta.transformed
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
            l <- makeVBarAndN(object, iBeta = b, g = g)
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
        .Call(gradientBeta_R, object) 
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
        
        
    

    

    
    betas <- object@betas
    priors <- object@priors
    log.post.betas.old <- object@logPostBetas
    momentum <- object@momentum
    sd.momentum <- object@sdMomentum@.Data
    num.steps <- object@numStepsHMCBeta
    epsilon <- 1 / num.steps
    n.beta <- length(betas)
    ## get means and variances for betas
    beta.hat.vec <- vector(mode = "list", length = n.beta)
    v.vec <- vector(mode = "list", length = n.beta)
    for (i in seq_len(n.beta)) {
        beta.hat.vec[[i]] <- betaHat(priors[[i]])
        v.vec[[i]] <- getV(priors[[i]])
    }
    ## initialize 'momentum'
    for (i.beta in seq_len(n.beta)) {
        n.i <- length(momentum[[i]])
        momentum[[i]] <- rnorm(n = n.i,
                               mean = 0,
                               sd = sd.momentum)
    }
    log.post.betas.old <- logPostBetas(betas = betas,
                                       
    gradient <- makeGradientBetas(betas = betas, ...)
    momentum <- momentum + 0.5 * epsilon * gradient
    for (i in seq_len(num.steps)) {
        
    }


}


initializeMomentum <- function(momentum, sdMomentum) {
    for (i in seq_len(momentum)) {
        n <- length(momentum[[i]])
        momentum[[i]] <- rnorm(n = n, mean = 0, sd = sdMomentum)
    }
    momentum
}

    

    
