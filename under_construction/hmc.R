

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
        mean.step.size <- object@meanStepSize@.Data
        log.post.betas.curr <- object@logPostBetas
        log.post.momentum.curr <- getLogPostMomentum(object) # call 'initializeMomentum' first
        step.size <- stats::runif(n = 1L,
                                  min = 0,
                                  max = 2 * mean.step.size)
        n.step <- stats::runif(n = 1L,
                               min = 0,
                               max = 2 / step.size)
        n.step <- as.integer(n.step) + 1L
        object <- updateMomentumOneStep(object,
                                        stepSize = step.size,
                                        isFirstLast = TRUE)
        for (i in seq_len(n.step)) {
            is.last <- i == n.step
            object <- updateBetasOneStep(object = object,
                                         stepSize = step.size)
            object <- updateMu(object)
            object <- updateGradientBetas(object)
            object <- updateMomentum(object = object,
                                     stepSize = step.size,
                                     isFirstLast = is.last)
        }
        object <- updateLogPostBetas(object)
        log.post.betas.prop <- object@logPostBetas
        log.post.momentum.prop <- getLogPostMomentum(object)
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
        
