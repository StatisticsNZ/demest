



## TRANSLATED
## HAS_TESTS
## Includes case where 'y' has subtotals.
## Subtotals can only be used with PoissonVarying models.
updateTheta_PoissonVaryingUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingUseExp"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
    ## exposure
    stopifnot(is.double(exposure))
    stopifnot(all(exposure@.Data[!is.na(exposure@.Data)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y@.Data[!is.na(y@.Data)][exposure[!is.na(y)] == 0] == 0L))
    if (methods::is(y, "HasSubtotals")) {
        for (i in seq_along(exposure))
            if (is.na(exposure[i]) && (dembase::getIAfter(i,
                                                          transform = y@transformSubtotals,
                                                          check = FALSE,
                                                          useC = TRUE) > 0L))
                stop("exposure is missing for cell in subtotal")
    }
    if (useC) {
        .Call(updateTheta_PoissonVaryingUseExp_R, object, y, exposure)
    }
    else {
        y.has.subtotals <- methods::is(y, "HasSubtotals")
        if (y.has.subtotals) {
            subtotals <- y@subtotalsNet
            transform <- y@transformSubtotals
        }
        theta <- object@theta
        box.cox.param <- object@boxCoxParam
        cell.in.lik <- object@cellInLik ## NEW
        scale <- object@scaleTheta
        scale.multiplier <- object@scaleThetaMultiplier
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma@.Data
        betas <- object@betas
        iterator <- object@iteratorBetas
        max.attempt <- object@maxAttempt
        n.failed.prop.theta <- 0L
        n.accept.theta <- 0L
        iterator <- resetB(iterator)
        scale <- scale * scale.multiplier
        for (i in seq_along(theta)) {
            is.struc.zero <- !cell.in.lik[i] && !is.na(y[i]) && (y[i] == 0L) ## NEW
            if (!is.struc.zero) { ## NEW
                indices <- iterator@indices
                mu <- 0
                for (b in seq_along(betas))
                    mu <- mu + betas[[b]][indices[b]]
                found.prop <- FALSE
                attempt <- 0L
                y.is.missing <- is.na(y[i])
                if (y.is.missing && y.has.subtotals) {
                    i.after <- dembase::getIAfter(i = i,
                                                  transform = transform,
                                                  check = FALSE,
                                                  useC = TRUE)
                    use.subtotal <- i.after > 0L
                }
                else
                    use.subtotal <- FALSE
                draw.straight.from.prior <- y.is.missing && !use.subtotal
                if (draw.straight.from.prior) {
                    mean <- mu
                    sd <- sigma
                }
                else {
                    th.curr <- theta[i]
                    if (box.cox.param > 0)
                        tr.th.curr <- (th.curr ^ box.cox.param - 1) / box.cox.param # ('tr' short for 'transformed')
                    else
                        tr.th.curr <- log(th.curr)
                    mean <- tr.th.curr
                    if (y.is.missing)
                        sd <- scale / scale.multiplier
                    else
                        sd <- scale / sqrt(1 + y[i])
                }
                while (!found.prop && (attempt < max.attempt)) {
                    attempt <- attempt + 1L
                    tr.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                    found.prop <- ((tr.th.prop > lower + tolerance)
                        && (tr.th.prop < upper - tolerance))
                }
                if (found.prop) {
                    if (box.cox.param > 0)
                        th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                    else
                        th.prop <- exp(tr.th.prop)
                    if (draw.straight.from.prior)
                        theta[i] <- th.prop
                    else {
                        if (use.subtotal) {
                            subtotal <- subtotals[i.after]
                            i.shared <- dembase::getIShared(i = i, transform = transform)
                            lambda.curr <- 0
                            for (i.s in i.shared) {
                                if (is.na(y[i.s]))
                                    lambda.curr <- lambda.curr + theta[i.s] * exposure[i.s]
                            }
                            lambda.prop <- lambda.curr + (th.prop - th.curr) * exposure[i]
                            log.lik.prop <- stats::dpois(x = subtotal, lambda = lambda.prop, log = TRUE)
                            log.lik.curr <- stats::dpois(x = subtotal, lambda = lambda.curr, log = TRUE)
                        }
                        else {
                            log.lik.prop <- stats::dpois(y[i], lambda = th.prop * exposure[i], log = TRUE)
                            log.lik.curr <- stats::dpois(y[i], lambda = th.curr * exposure[i], log = TRUE)
                        }
                        log.dens.prop <- stats::dnorm(x = tr.th.prop, mean = mu, sd = sigma, log = TRUE)
                        log.dens.curr <- stats::dnorm(x = tr.th.curr, mean = mu, sd = sigma, log = TRUE)
                        log.diff <- log.lik.prop + log.dens.prop - log.lik.curr - log.dens.curr
                        accept <- (log.diff >= 0) || (stats::runif(n = 1L) < exp(log.diff))
                        if (accept) {
                            n.accept.theta <- n.accept.theta + 1L
                            theta[i] <- th.prop
                        }
                    }
                }
                else
                    n.failed.prop.theta <- n.failed.prop.theta + 1L
            } ## NEW
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

    

updateUBeta <- function(prior, beta, useC = FALSE) {
    ## prior
    stopifnot(methods::validObject(prior))
    stopifnot(methods::is(prior, "RobustMixin"))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(identical(length(beta), as.integer(prior@J)))
    stopifnot(!any(is.na(beta)))
    if (useC) {
        .Call(updateUBeta_R, prior, beta)
    }
    else {
        J <- prior@J@.Data
        U <- prior@UBeta@.Data
        nu <- prior@nuBeta@.Data
        tau <- prior@tau@.Data
        all.struc.zero <- prior@allStrucZero
        beta.hat <- betaHat(prior)
        df <- nu + 1
        for (i in seq_len(J)) {
            if (!is.struc.zero[i]) {
                scale <- (nu * tau^2 + (beta - beta.hat[i])^2) / df
                U[i] <- rinvchisq1(df = df, scale = scale[i])
            }
        }
        prior@UBeta@.Data <- U
        prior
    }
}

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
        K <- prior@K@.Data
        L <- prior@L@.Data
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
        is.struc.zero <- object@isStrucZero ## NEW
        v <- getV(prior)              # numeric vector length KL
        tolerance <- prior@tolerance@.Data
        iterator.a <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.a <- resetA(iterator.a)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
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
                ## backward sample
                ## for (i in seq.int(from = K - 1L, to = non.stationary)) { # if nonstationary, alpha0 = 0
                for (i in seq.int(from = K - 1L, to = 0L)) { # if nonstationary, alpha0 = 0
                    if ((i > 0L) || (C[[1L]] > tolerance)) {
                        B <- C[[i + 1L]] * phi / R[[i + 1L]]
                        m.star <- m[[i + 1L]] + B * (alpha[indices.a[i + 2L]] - a[[i + 1L]])
                        C.star <- C[[i + 1L]] - B^2 * R[[i + 1L]]
                        alpha[indices.a[i + 1L]] <- stats::rnorm(n = 1L, mean = m.star, sd = sqrt(C.star))
                    }
                }
            } ## NEW
            iterator.a <- advanceA(iterator.a)
            iterator.v <- advanceA(iterator.v)
        }
        prior@alphaDLM@.Data <- alpha
        prior
    }
}




updatePhi <- function(prior, withTrend, useC = FALSE) {
    ## 'prior'
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::validObject(prior))
    ## 'withTrend'
    stopifnot(is.logical(withTrend))
    stopifnot(identical(length(withTrend), 1L))
    stopifnot(!is.na(withTrend))
    ## 'prior' and 'withTrend'
    stopifnot((withTrend && methods::is(prior, "WithTrendMixin"))
              || (!withTrend && methods::is(prior, "NoTrendMixin")))
    if (useC) {
        .Call(updatePhi_R, prior, withTrend)
    }
    else {
        phi.known <- prior@phiKnown@.Data
        if (phi.known)
            return(prior)
        phi.curr <- prior@phi
        K <- prior@K@.Data
        L <- prior@L@.Data
        if (withTrend) {
            state <- prior@deltaDLM@.Data
            omega <- prior@omegaDelta@.Data
        }
        else {
            state <- prior@alphaDLM@.Data
            omega <- prior@omegaAlpha@.Data
        }
        phi.min <- prior@minPhi@.Data
        phi.max <- prior@maxPhi@.Data
        shape1 <- prior@shape1Phi@.Data
        shape2 <- prior@shape2Phi@.Data
        all.struc.zero <- prior@allStrucZero ## new
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        numerator <- 0
        denominator <- 0
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## New
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1]
                    k.prev <- indices[i]
                    numerator <- numerator + state[k.curr] * state[k.prev]
                    denominator <- denominator + (state[k.prev])^2
                }
            } ## New
            iterator <- advanceA(iterator)
        }
        mean <- numerator / denominator
        sd <- omega / sqrt(denominator)
        phi.prop <- rtnorm1(mean = mean,
                            sd = sd,
                            lower = phi.min,
                            upper = phi.max)
        ## proposal density and likelihood cancel
        phi.prop.tr <- (phi.prop - phi.min) / (phi.max - phi.min)
        phi.curr.tr <- (phi.curr - phi.min) / (phi.max - phi.min)
        log.dens.prop <- stats::dbeta(x = phi.prop.tr,
                                      shape1 = shape1,
                                      shape2 = shape2,
                                      log = TRUE)
        log.dens.curr <- stats::dbeta(x = phi.curr.tr,
                                      shape1 = shape1,
                                      shape2 = shape2,
                                      log = TRUE)
        log.diff <- log.dens.prop - log.dens.curr
        accept <- (log.diff >= 0) || (stats::runif(1L) < exp(log.diff))
        if (accept)
            prior@phi <- phi.prop
        prior
    }
}

updateOmegaAlpha <- function(prior, withTrend, useC = FALSE) {
    ## 'prior'
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::validObject(prior))
    ## 'withTrend'
    stopifnot(is.logical(withTrend))
    stopifnot(identical(length(withTrend), 1L))
    stopifnot(!is.na(withTrend))
    ## 'prior' and 'withTrend'
    stopifnot((withTrend && methods::is(prior, "WithTrendMixin"))
              || (!withTrend && methods::is(prior, "NoTrendMixin")))
    if (useC) {
        .Call(updateOmegaAlpha_R, prior, withTrend)
    }
    else {
        if (withTrend) {                          
            has.level <- prior@hasLevel@.Data     
            if (!has.level)                       
                return(prior)                     
        }                                         
        J <- prior@J@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data
        omega <- prior@omegaAlpha@.Data
        omegaMax <- prior@omegaAlphaMax@.Data
        A <- prior@AAlpha@.Data
        nu <- prior@nuAlpha@.Data
        if (withTrend)
            delta <- prior@deltaDLM@.Data
        else
            phi <- prior@phi
        iterator <- prior@iteratorState
        all.struc.zero <- prior@allStrucZero ## NEW
        iterator <- resetA(iterator)
        V <- 0
        n <- 0L ## NEW?
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1]
                    k.prev <- indices[i]
                    if (withTrend)
                        V <- V + (alpha[k.curr] - alpha[k.prev] - delta[k.prev])^2
                    else
                        V <- V + (alpha[k.curr] - phi * alpha[k.prev])^2
                    n <- n + 1L ## NEW?
                }
                iterator <- advanceA(iterator)
            } ## NEW
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n,
                              max = omegaMax)
        successfully.updated <- omega > 0
        if (successfully.updated)
            prior@omegaAlpha@.Data <- omega
        prior
    }
}


## TRANSLATED
## HAS_TESTS
updateOmegaDelta <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "WithTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateOmegaDelta_R, prior)
    }
    else {
        J <- prior@J@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        delta <- prior@deltaDLM@.Data
        phi <- prior@phi
        omega <- prior@omegaDelta@.Data
        omegaMax <- prior@omegaDeltaMax@.Data
        A <- prior@ADelta@.Data
        nu <- prior@nuDelta@.Data
        iterator <- prior@iteratorState
        all.struc.zero <- prior@allStrucZero ## NEW
        iterator <- resetA(iterator)
        V <- 0
        n <- 0L ## NEW
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1]
                    k.prev <- indices[i]
                    V <- V + (delta[k.curr] - phi * delta[k.prev])^2
                    n <- n + 1L ## NEW
                }
                iterator <- advanceA(iterator)
            } ## NEW
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n, ## NEW
                              max = omegaMax)
        successfully.updated <- omega > 0
        if (successfully.updated)
            prior@omegaDelta@.Data <- omega
        prior
    }
}





## TRANSLATED
## HAS_TESTS
updateSeason <- function(prior, betaTilde, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "SeasonMixin"))
    stopifnot(methods::validObject(prior))
    ## betaTilde
    stopifnot(is.double(betaTilde))
    stopifnot(!any(is.na(betaTilde)))
    if (useC) {
        .Call(updateSeason_R, prior, betaTilde)
    }
    else {    
        K <- prior@K@.Data
        L <- prior@L@.Data
        n.season <- prior@nSeason@.Data 
        s <- prior@s@.Data       # length (K+1)L list of vectors of length nSeason
        m <- prior@mSeason@.Data # length K+1 list of vectors of length nSeason
        m0 <- prior@m0Season@.Data # length L list of vectors of length nSeason
        C <- prior@CSeason@.Data # length K+1 list of vectors (not matrices) of length nSeason
        a <- prior@aSeason@.Data # length K list of vectors of length nSeason
        R <- prior@RSeason@.Data # length K list of vectors (not matrices) of length nSeason
        v <- getV(prior)         # numeric vector of length KL
        omega <- prior@omegaSeason@.Data
        omega.sq <- omega^2
        iterator.s <- prior@iteratorState
        iterator.v <- prior@iteratorV
        all.struc.zero <- prior@allStrucZero ## NEW
        iterator.s <- resetA(iterator.s)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices.s <- iterator.s@indices
                indices.v <- iterator.v@indices
                m[[1L]] <- m0[[l]]
                ## forward filter
                for (i in seq_len(K)) {
                    j <- indices.v[i]
                    for (i.n in seq_len(n.season - 1L)) {
                        a[[i]][i.n + 1L] <- m[[i]][i.n]
                        R[[i]][i.n + 1L] <- C[[i]][i.n]
                    }
                    a[[i]][1L] <- m[[i]][n.season]
                    R[[i]][1L] <- C[[i]][n.season] + omega^2
                    q <- R[[i]][1L] + v[j]
                    e <- betaTilde[j] - a[[i]][1L]
                    Ae1 <- R[[i]][1L] * e / q
                    m[[i + 1L]] <- a[[i]]
                    m[[i + 1L]][1L] <- m[[i + 1L]][1L] + Ae1
                    AAq1 <- (R[[i]][1L])^2 / q
                    C[[i + 1L]] <- R[[i]]
                    C[[i + 1L]][1L] <- C[[i + 1L]][1L] - AAq1 
                }                
                ## draw final value for 's'
                for (i.n in seq_len(n.season)) {
                    i.curr <- indices.s[K + 1L]
                    mean <- m[[K + 1L]][i.n]
                    sd <- sqrt(C[[K + 1L]][i.n])
                    s[[i.curr]][i.n] <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                }
                ## backward smooth
                for (i in seq.int(from = K, to = 1L)) {
                    i.prev <- indices.s[i + 1L]
                    i.curr <- indices.s[i]
                    s[[i.curr]][-n.season] <- s[[i.prev]][-1L]
                    lambda <- C[[i]][n.season] / (C[[i]][n.season] + omega.sq)
                    mean <- lambda * s[[i.prev]][1L] + (1 - lambda) * m[[i]][n.season]
                    sd <- sqrt(lambda) * omega
                    s[[i.curr]][n.season] <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                }
                iterator.s <- advanceA(iterator.s)
                iterator.v <- advanceA(iterator.v)
            } ## NEW
        }
        prior@s@.Data <- s
        prior
    }
}


## TRANSLATED
## HAS_TESTS
updateOmegaSeason <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "SeasonMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateOmegaSeason_R, prior)
    }
    else {
        J <- prior@J@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        s <- prior@s@.Data
        n.season <- prior@nSeason@.Data
        omega <- prior@omegaSeason@.Data
        omegaMax <- prior@omegaSeasonMax@.Data
        A <- prior@ASeason@.Data
        nu <- prior@nuSeason@.Data
        iterator <- prior@iteratorState
        all.struc.zero <- prior@allStrucZero ## NEW
        iterator <- resetA(iterator)
        V <- 0
        n <- 0L ## NEW
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    i.curr <- indices[i + 1L]
                    i.prev <- indices[i]
                    curr <- s[[i.curr]][1L]
                    prev <- s[[i.prev]][n.season]
                    V <- V + (curr - prev)^2
                    n <- n + 1L ## NEW
                }
                iterator <- advanceA(iterator)
            } ## NEW
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n, ## NEW
                              max = omegaMax)
        successfully.updated <- omega > 0
        if (successfully.updated)
            prior@omegaSeason@.Data <- omega
        prior
    }
}
