
## FROM AllClasses-11-Aggregate.R
## HAS_TESTS
setClass("MuMixin",
         slots = c(mu = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             mu <- object@mu
             theta <- object@theta
             ## 'mu' has type "double"
             if (!is.double(mu))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "mu", "double"))
             ## 'mu' has no missing values
             if (any(is.na(mu)))
                 return(gettextf("'%s' has missing values",
                                 "mu"))
             ## 'mu' has same length as 'theta'
             if (!identical(length(mu), length(theta)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mu", "theta"))
             TRUE
         })


setClass("GThetaMixin",
         slots = c(gTheta = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             gTheta <- object@gTheta
             theta <- object@theta
             cell.in.lik <- oject@cellInLik
             ## 'gTheta' has type "double"
             if (!is.double(gTheta))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "gTheta", "double"))
             ## 'gTheta' has same length as 'theta'
             if (!identical(length(gTheta), length(theta)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "gTheta", "theta"))
             ## 'gTheta' missing values iff cell not in likelihood
             if (!identical(is.na(gTheta), cell.in.lik))
                 stop(gettextf("'%s' and '%s' inconsistent",
                               "gTheta", "cellInLik"))
             TRUE
         })





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





## modified from makeMu
## HAS_TESTS
updateMu <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateMu_R, object)
    }
    else {
        mu <- object@mu
        iterator <- object@iteratorBetas
        n <- length(mu)
        for (i in seq_len(n)) {
            indices <- iterator@indices
            mu[i] <- 0
            for (b in seq_along(betas))
                mu[i] <- mu[i] + betas[[b]][indices[b]]
            iterator <- advanceB(iterator)
        }
        object@mu <- mu
        object
    }
}



## HAS_TESTS
updateGTheta <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateGTheta_R, object)
    }
    else {
        theta <- 
        iterator <- object@iteratorBetas
        n <- length(mu)
        for (i in seq_len(n)) {
            indices <- iterator@indices
            mu[i] <- 0
            for (b in seq_along(betas))
                mu[i] <- mu[i] + betas[[b]][indices[b]]
            iterator <- advanceB(iterator)
        }
        object@mu <- mu
        object
    }
}
