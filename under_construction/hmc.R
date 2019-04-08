

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
        beta.equals.mean <- object@betaEqualsMean
        sigma <- object@sigma
        theta.transformed <- object@thetaTransformed
        mu <- object@mu
        iterator <- object@iteratorBetas
        iterator <- resestB(iterator)
        var.theta <- sigma^2
        n.theta <- length(theta.transformed)
        n.beta <- length(betas)
        ## reset gradient
        for (i.beta in seq_len(n.beta))
            gradient.betas[[i.beta]] <- 0
        for (i.theta in seq_len(n.theta)) {
            include.cell <- cell.in.lik[i.theta]
            if (include.cell) {
                indices <- iterator@indices
                for (i.beta in seq_len(n.beta)) {
                    if (!beta.equals.mean[i]) {
                        j <- indices[i.beta]
                        diff.theta <- theta.transformed[i.theta] - mu[i.theta]
                        diff.beta <- betas[[i.beta]][j] - mean.betas[[i.beta]][j]
                        var.beta <- variance.betas[[i.beta]][j] 
                        gradient.betas[[i.beta]][j] <- (gradient.betas[[i.beta]][j]
                            + diff.theta / var.theta
                            - diff.beta / var.beta)
                    }
                }
            }
        }
        object@gradientBetas <- gradient.betas
    }
    object
}


updateLogPostMomentum <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateLogPostMomentum_R, object) 
    }
    else {
        momentum <- object@momentumMetas
        ans <- 0
        for (i.beta in seq_along(momentum)) {
            if (!beta.equals.mean[i.beta]) {
                ans <- ans + sum(dnorm(momentum[[i.beta]],
                                       mean = 0,
                                       sd = 1,
                                       log = TRUE))
            }
        }
        object@logPostMomentum <- ans
        object
    }
}


updateBetas <- function(object) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateBetas_R, object) 
    }
    else {
        object <- updateBetasWhereBetaEqualsMean(object)
        object <- initializeMomentum(object)
        betas.curr <- object@betas # call 'updateBetasWhereBetaEqualsMean' first
        log.post.momentum.curr <- object@logPostMomentum # call 'initializeMomentum' first
        mean.step.size <- object@meanStepSize@.Data
        log.post.betas.curr <- object@logPostBetas
        step.size <- stats::runif(n = 1L,
                                  min = 0,
                                  max = 2 * mean.step.size)
        n.step <- stats::runif(n = 1L,
                               min = 0,
                               max = 2 / step.size)
        n.step <- as.integer(n.step) + 1L
        object <- updateMomentum(object,
                                 stepSize = step.size,
                                 isFirstLast = TRUE)
        for (i in seq_len(n.step)) {
            is.first.last <- i == n.step
            object <- updateBetasOneStep(object = object,
                                         stepSize = step.size)
            object <- updateMu(object)
            object <- updateGradientBetas(object)
            object <- updateMomentum(object = object,
                                     stepSize = step.size,
                                     isFirstLast = is.first.last)
        }
        object <- updateLogPostBetas(object)
        object <- updateLogPostMomentum(object)
        log.post.betas.prop <- object@logPostBetas
        log.post.momentum.prop <- object@logPostMomentum
        log.diff <- (log.post.betas.prop + log.post.momentum.prop
            - log.post.betas.curr - log.post.momentum.curr)
        accept <- (log.diff >= 0) || (stats::runif(n = 1L) > exp(log.diff))
        if (accept) {
            object@nAcceptBeta@.Data <- object@nAcceptBeta@.Data + 1L
        }
        else {
            object@betas <- betas.curr
            object <- updateMu(object)
            object@logPostBetas <- log.post.betas.curr
        }
        object
    }
}
        

updateMomentum <- function(object, stepSize, firstLast, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    ## firstLast
    stopifnot(identical(length(firstLast), 1L))
    stopifnot(is.logical(firstLast))
    stopifnot(!is.na(firstLast))
    if (useC) {
        .Call(updateMomentum_R, object)
    }
    else {
        momentum <- object@momentumBetas
        gradient <- object@gradientBetas
        beta.equals.mean <- object@betaEqualsMean
        for (i.beta in seq_len(momentum)) {
            if (!beta.equals.mean[i]) {
                J <- length(momentum[[i.beta]])
                mult <- if (firstLast) 0.5 * stepSize else stepSize
                for (j in seq_len(J))
                    momentum[[i.beta]][j] <- momentum[[i.beta]][j] - mult * gradient[[i]]
            }
        }
        object@momentum <- momentum
        object
    }
}
