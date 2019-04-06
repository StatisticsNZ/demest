

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
        mean.betas <- object@meanBetas
        variance.betas <- object@varianceBetas
        sigma <- object@sigma
        theta.transformed <- object@thetaTransformed
        mu <- object@mu
        iterator <- object@iteratorBetas
        iterator <- resestB(iterator)
        sigma.sq <- sigma^2
        n.theta <- length(theta.transformed)
        n.beta <- length(betas)
        n.i.beta <- sapply(betas, length)
        ## reset gradient
        for (i.beta in seq_len(n.beta)) {
            for (j in seq_len(n.i.beta))
                gradient.betas[[i.beta]][j] <- 0
        }
        for (i.theta in seq_len(n.theta)) {
            include.cell <- cell.in.lik[i.theta]
            if (include.cell) {
                indices <- iterator@indices
                for (i.beta in seq_len(n.beta)) {
                    j <- indices[i.beta]
                    diff.theta <- theta.transformed[i.theta] - mu[i.theta]
                    diff.beta <- betas[[i.beta]][j] - mean.betas[[i.beta]][j]
                    var.beta <- variance.betas[[i.beta]][j] 
                    gradient.betas[[i.beta]][j] <- (gradient.betas[[i.beta]][j]
                        + diff.theta / sigma.sq
                        + diff.beta / var.beta)
                }
            }
        }
        object@gradientBetas <- gradient.betas
    }
    object
}

updateLogPostBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateGradientBetas_R, object) 
    }
    else {
        betas <- object@betas
        mean.betas <- object@meanBetas
        variance.betas <- object@varianceBetas
        log.post <- 0
        for (i in seq_along(betas))
            log.post <- log.post + stats::dnorm(x = betas[[i]],
                                                mean = mean.betas[[i]],
                                                sd = sqrt(betas[[i]]),
                                                log = TRUE)
        object@logPostBetas@.Data <- log.post
        object
    }
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
        
initializeMomentum <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(initializeMomentum_R, object)
    }
    else {
        momentum <- object@momentumBetas
        for (i in seq_len(momentum)) {
            n <- length(momentum[[i]])
            momentum[[i]] <- rnorm(n = n,
                                   mean = 0,
                                   sd = 1)
        }
        object@momentum <- momentum
        object
    }
}


## updateMomentum <- function(object, firstLast, useC = FALSE) {
##     ## object
##     stopifnot(methods::is(object, "Varying"))
##     stopifnot(methods::validObject(object))
##     ## firstLast
##     stopifnot(identical(length(firstLast), 1L))
##     stopifnot(is.logical(firstLast))
##     stopifnot(!is.na(firstLast))
##     if (useC) {
##         .Call(updateMomentum_R, object)
##     }
##     else {
##         momentum <- object@momentumBetas
##         for (i in seq_len(momentum)) {
##             n <- length(momentum[[i]])
##             momentum[[i]] <- rnorm(n = n,
##                                    mean = 0,
##                                    sd = 1)
##         }
##         object@momentum <- momentum
##         object
##     }
## }


    

    
