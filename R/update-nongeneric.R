


## UPDATING STANDARD DEVIATION (VIA SLICE SAMPLING) #################################

## TRANSLATED
## HAS_TESTS
## based on algorithm on p712 of Neal R. 2003. Slice sampling.
## The Annals of Statistics. 31(3): 705-767.  Our function 'f'
## is Neal's function 'g', modified to allow for right-truncation,
## specified by 'max'.
## Function returns -99.0 if it fails to generate an updated value of sigma
updateSDNorm <- function(sigma, A, nu, V, n, max, useC = FALSE) {
    ## 'sigma'
    stopifnot(identical(length(sigma), 1L))
    stopifnot(is.double(sigma))
    stopifnot(!is.na(sigma))
    stopifnot(sigma > 0)
    ## 'A'
    stopifnot(identical(length(A), 1L))
    stopifnot(is.double(A))
    stopifnot(!is.na(A))
    stopifnot(A > 0)
    ## 'nu'
    stopifnot(identical(length(nu), 1L))
    stopifnot(is.double(nu))
    stopifnot(!is.na(nu))
    stopifnot(nu > 0)
    ## 'V'
    stopifnot(identical(length(V), 1L))
    stopifnot(is.double(V))
    stopifnot(!is.na(V))
    stopifnot(V > 0)
    ## 'n'
    stopifnot(identical(length(n), 1L))
    stopifnot(is.integer(n))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## 'max'
    stopifnot(identical(length(max), 1L))
    stopifnot(is.double(max))
    stopifnot(!is.na(max))
    stopifnot(max > 0)
    if (useC) {
        .Call(updateSDNorm_R, sigma, A, nu, V, n, max)
    }
    else {
        f0 <- -n*log(sigma) - V/(2*sigma^2) - ((nu + 1)/2) * log(sigma^2 + nu*A^2)
        e <- stats::rexp(n = 1L, rate = 1)
        z <- f0 - e
        numerator <- V - n*nu*A^2 + sqrt((V - n*nu*A^2)^2 + 4*(n + nu + 1)*V*nu*A^2)
        denominator <- 2*(n + nu + 1)
        sigma.star <- sqrt(numerator / denominator)
        sigma0.left <- 0.5 * sigma.star
        root.left <- findOneRootLogPostSigmaNorm(sigma0 = sigma0.left,
                                                 z = z,
                                                 A = A,
                                                 nu = nu,
                                                 V = V,
                                                 n = n,
                                                 min = 0,
                                                 max = sigma.star,
                                                 useC = TRUE)
        found.root.left <- root.left > 0
        if (found.root.left) {
            if (is.finite(max)) {
                f.max <- -n*log(max) - V/(2*max^2) - ((nu + 1)/2) * log(max^2 + nu*A^2)
                root.less.than.max <- z > f.max
                if (root.less.than.max) {
                    sigma0.right <- 1.5 * sigma.star
                    if (sigma0.right > max)
                        sigma0.right <- 0.5 * sigma.star + 0.5 * max                    
                }
            }
            else {
                root.less.than.max <- TRUE
                sigma0.right <-  1.5 * sigma.star
            }
            if (root.less.than.max) {
                root.right <- findOneRootLogPostSigmaNorm(sigma0 = sigma0.right,
                                                          z = z,
                                                          A = A,
                                                          nu = nu,
                                                          V = V,
                                                          n = n,
                                                          min = sigma.star,
                                                          max = max)
                found.root.right <- root.right > 0
                if (found.root.right) {
                    limit.right <- root.right
                    found.limit.right <- TRUE
                }
                else
                    found.limit.right <- FALSE
            }
            else {
                limit.right <- max
                found.limit.right <- TRUE
            }
            if (found.limit.right)
                ans <- stats::runif(n = 1L, min = root.left, max = limit.right)
            else
                ans <- -99
        }
        else {
            near.sigma.star <- root.left > -2
            if (near.sigma.star)
                ans <- sigma.star
            else
                ans <- -99
        }
        ans
    }
}


## TRANSLATED
## HAS_TESTS
## based on algorithm on p712 of Neal R. 2003. Slice sampling.
## The Annals of Statistics. 31(3): 705-767.  Our function 'f'
## is Neal's function 'g', modified to allow for right-truncation,
## as specified by 'max'.
## Function returns -99.0 if it fails to generate an updated value of sigma
updateSDRobust <- function(sigma, A, nuBeta, nuTau, V, n, max, useC = FALSE) {
    ## 'sigma'
    stopifnot(identical(length(sigma), 1L))
    stopifnot(is.double(sigma))
    stopifnot(!is.na(sigma))
    stopifnot(sigma > 0)
    ## 'A'
    stopifnot(identical(length(A), 1L))
    stopifnot(is.double(A))
    stopifnot(!is.na(A))
    stopifnot(A > 0)
    ## 'nuBeta'
    stopifnot(identical(length(nuBeta), 1L))
    stopifnot(is.double(nuBeta))
    stopifnot(!is.na(nuBeta))
    stopifnot(nuBeta > 0)
    ## 'nuTau'
    stopifnot(identical(length(nuTau), 1L))
    stopifnot(is.double(nuTau))
    stopifnot(!is.na(nuTau))
    stopifnot(nuTau > 0)
    ## 'V'
    stopifnot(identical(length(V), 1L))
    stopifnot(is.double(V))
    stopifnot(!is.na(V))
    stopifnot(V > 0)
    ## 'n'
    stopifnot(identical(length(n), 1L))
    stopifnot(is.integer(n))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## 'max'
    stopifnot(identical(length(max), 1L))
    stopifnot(is.double(max))
    stopifnot(!is.na(max))
    stopifnot(max > 0)
    if (useC) {
        .Call(updateSDRobust_R, sigma, A, nuBeta, nuTau, V, n, max)
    }
    else {
        f0 <- n*nuBeta*log(sigma) - (nuBeta/2)*(sigma^2)*V - ((nuTau+1)/2)*log(sigma^2 + nuTau*A^2)
        e <- stats::rexp(n = 1L, rate = 1)
        z <- f0 - e
        H1 <- nuBeta * V
        H2 <- nuBeta * nuTau * V * A^2 + nuTau + 1 - n * nuBeta
        H3 <- -n * nuBeta * nuTau * A^2
        sigma.star <- sqrt((-H2 + sqrt(H2^2 - 4*H1*H3))/(2*H1))
        sigma0.left <- 0.5 * sigma.star
        root.left <- findOneRootLogPostSigmaRobust(sigma0 = sigma0.left,
                                                   z = z,
                                                   A = A,
                                                   nuBeta = nuBeta,
                                                   nuTau = nuTau,
                                                   V = V,
                                                   n = n,
                                                   min = 0,
                                                   max = sigma.star,
                                                   useC = TRUE)
        found.root.left <- root.left > 0
        if (found.root.left) {
            if (is.finite(max)) {
                f.max <- n*nuBeta*log(max) - (nuBeta/2)*(max^2)*V - ((nuTau+1)/2)*log(max^2 + nuTau*A^2)
                root.less.than.max <- z > f.max
                if (root.less.than.max) {
                    sigma0.right <- 1.5 * sigma.star
                    if (sigma0.right > max)
                        sigma0.right <- 0.5 * sigma.star + 0.5 * max
                }
            }
            else {
                root.less.than.max <- TRUE
                sigma0.right <-  1.5 * sigma.star
            }
            if (root.less.than.max) {
                root.right <- findOneRootLogPostSigmaRobust(sigma0 = sigma0.right,
                                                            z = z,
                                                            A = A,
                                                            nuBeta = nuBeta,
                                                            nuTau = nuTau,
                                                            V = V,
                                                            n = n,
                                                            min = sigma.star,
                                                            max = max,
                                                            useC = TRUE)
                found.root.right <- root.right > 0
                if (found.root.right) {
                    limit.right <- root.right
                    found.limit.right <- TRUE
                }
                else
                    found.limit.right <- FALSE
            }
            else {
                limit.right <- max
                found.limit.right <- TRUE
            }
            if (found.limit.right)
                ans <- stats::runif(n = 1L, min = root.left, max = limit.right)
            else
                ans <- -99
        }
        else {
            near.sigma.star <- root.left > -2
            if (near.sigma.star)
                ans <- sigma.star
            else
                ans <- -99
        }
        ans
    }
}

## UPDATING PRIORS ##################################################################

## updateAlphaICAR <- function(prior, useC) {
##     kTimesUpdate <- 2L
##     J <- prior@J
##     alpha <- prior@alphaICAR
##     neighbours <- prior@neighbors
##     n.update <- kTimesUpdate * J
##     j.prev <- J
##     for (update in seq_len(n.update)) {
##         j <- as.integer(stats::runif(n = 1L) * (J - 1L))
##         if (j == (J - 1L)) # just in case
##             j <- J - 2L
##         if (j >= j.prev)
##             j <- j + 1L
##         w <- neighbours[[j]]
##         mean <- sum(w * 

updateAlphaMove <- function(prior, betaTilde, useC = FALSE) {
    J <- prior@J@.Data
    alpha <- prior@alphaMove@.Data
    index.class <- prior@indexClassAlpha
    n.element.class <- prior@nElementClassAlpha
    A <- prior@AMove@.Data
    A.sq <- A^2
    v <- getV(prior)
    n.alpha <- length(alpha)
    sum.beta <- numeric(length = n.alpha)
    for (j in seq_along(J)) {
        k <- index.class[j]
        if (k > 0L)
            sum.beta[k] <- sum.beta[k] + betaTilde[j]
    }
    for (k in seq_len(n.alpha)) {
        n <- n.element.class[k]
        prec.data <- n / v
        prec.prior <- 1 / A^2
        var <- 1 / (prec.data + prec.prior)
        beta.bar <- sum.beta[k] / n
        mean <- (prec.data * beta.bar) * var
        sd <- sqrt(var)
        alpha[k] <- stats::rnorm(n = J, mean = mean, sd = sd)
    }
    prior@alphaMove@.Data <- alpha
}


## TRANSLATED
## HAS_TESTS
updateAlphaDeltaDLMWithTrend <- function(prior, betaTilde, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "WithTrendMixin"))
    stopifnot(methods::validObject(prior))
    ## betaTilde
    stopifnot(is.double(betaTilde))
    stopifnot(!any(is.na(betaTilde)))
    ## 'prior' and 'betaTilde'
    stopifnot(identical(length(betaTilde), prior@J@.Data))
    if (useC) {
        .Call(updateAlphaDeltaDLMWithTrend_R, prior, betaTilde)
    }
    else {    
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data  # numeric length (K+1)L
        delta <- prior@deltaDLM@.Data  # numeric length (K+1)L
        G <- prior@GWithTrend@.Data  # 2x2 matrix
        m <- prior@mWithTrend@.Data # list length K+1
        m0 <- prior@m0WithTrend@.Data # list length L
        C <- prior@CWithTrend@.Data # list length K+1
        a <- prior@aWithTrend@.Data # list length K
        W.sqrt <- prior@WSqrt@.Data # matrix 2X2
        W.sqrt.inv.G <- prior@WSqrtInvG@.Data # matrix 2X2
        UC <- prior@UC@.Data  # list length K+1
        DC <- prior@DC@.Data  # list length K+1
        DC.inv <- prior@DCInv@.Data # list length K+1
        UR <- prior@UR@.Data  # list length K
        DR.inv <- prior@DRInv@.Data
        v <- getV(prior) ## numeric length KL
        iterator.ad <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.ad <- resetA(iterator.ad)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            indices.ad <- iterator.ad@indices
            indices.v <- iterator.v@indices
            m[[1L]] <- m0[[l]]
            ## forward filter
            for (i in seq_len(K)) {
                M.R <- rbind(DC[[i]] %*% t(UC[[i]]) %*% t(G),
                             W.sqrt)
                svd.R <- svd(M.R, nu = 0)
                UR[[i]] <- svd.R$v
                DR.inv.diag <- 1 / svd.R$d
                DR.inv.diag[is.infinite(DR.inv.diag)] <- 0
                DR.inv[[i]][c(1L, 4L)] <- DR.inv.diag
                M.C <- rbind(UR[[i]][c(1L, 3L)] / sqrt(v[indices.v[i]]),
                             DR.inv[[i]])
                svd.C <- svd(M.C, nu = 0)
                UC[[i + 1L]] <- UR[[i]] %*% svd.C$v
                DC.inv.diag <- svd.C$d
                DC.diag <- 1/DC.inv.diag
                DC.diag[is.infinite(DC.diag)] <- 0
                DC.inv[[i + 1L]][c(1L, 4L)] <- DC.inv.diag
                DC[[i + 1L]][c(1L, 4L)] <- DC.diag
                a[[i]] <- drop(G %*% m[[i]])
                e <- betaTilde[indices.v[i]] - a[[i]][1L]
                C[[i + 1L]] <- UC[[i + 1L]] %*% DC[[i + 1L]] %*% DC[[i + 1L]] %*% t(UC[[i + 1L]])
                A <- C[[i + 1L]][1:2] / v[indices.v[i]]
                m[[i + 1L]] <- a[[i]] + A * e
            }
            ## draw final gamma, delta
            sqrt.C <- UC[[K + 1L]] %*% DC[[K + 1L]]
            z <- stats::rnorm(n = 2L)
            theta <- m[[K + 1L]] + drop(sqrt.C %*% z)
            alpha[indices.ad[K + 1L]] <- theta[1L]
            delta[indices.ad[K + 1L]] <- theta[2L]
            ## backward smooth
            for (i in seq.int(from = K - 1L, to = 0L)) {
                R.inv <- (UR[[i + 1L]] %*% DR.inv[[i + 1L]]
                          %*% DR.inv[[i + 1L]] %*% t(UR[[i + 1L]]))
                B <- C[[i + 1L]] %*% t(G) %*% R.inv
                M.C.star <- rbind(W.sqrt.inv.G,
                                  DC.inv[[i + 1L]] %*% t(UC[[i + 1L]]))
                svd.C.star <- svd(M.C.star, nu = 0)
                UC.star <- svd.C.star$v
                DC.star <- 1 / svd.C.star$d
                DC.star[is.infinite(DC.star)] <- 0
                DC.star <- diag(DC.star, nrow = 2L)
                theta.prev <- c(alpha[indices.ad[i + 2L]], delta[indices.ad[i + 2L]])
                m.star <- m[[i + 1L]] + drop(B %*% (theta.prev - a[[i + 1L]]))
                z <- stats::rnorm(n = 2L)
                sqrt.C.star <- UC.star %*% DC.star
                theta.curr <- m.star + drop(sqrt.C.star %*% z)
                alpha[indices.ad[i + 1L]] <- theta.curr[1L]
                delta[indices.ad[i + 1L]] <- theta.curr[2L]
            }
            iterator.ad <- advanceA(iterator.ad)
            iterator.v <- advanceA(iterator.v)
        }
        prior@alphaDLM@.Data <- alpha
        prior@deltaDLM@.Data <- delta
        prior
    }
}

## TRANSLATED
## HAS_TESTS
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
        v <- getV(prior)              # numeric vector length KL
        iterator.a <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.a <- resetA(iterator.a)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
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
            ## backward smooth
            for (i in seq.int(from = K - 1L, to = 0L)) {
                B <- C[[i + 1L]] * phi / R[[i + 1L]]
                m.star <- m[[i + 1L]] + B * (alpha[indices.a[i + 2L]] - a[[i + 1L]])
                C.star <- C[[i + 1L]] - B^2 * R[[i + 1L]]
                alpha[indices.a[i + 1L]] <- stats::rnorm(n = 1L, mean = m.star, sd = sqrt(C.star))
            }
            iterator.a <- advanceA(iterator.a)
            iterator.v <- advanceA(iterator.v)
        }
        prior@alphaDLM@.Data <- alpha
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateBeta <- function(prior, vbar, n, sigma, useC = FALSE) {
    checkUpdateBetaAndPriorBeta(prior = prior,
                                vbar = vbar,
                                n = n,
                                sigma = sigma)
    stopifnot(methods::is(prior, "ComponentFlags"))
    if (useC) {
        .Call(updateBeta_R, prior, vbar, n, sigma)
    }
    else {
        J <- prior@J@.Data
        v <- getV(prior)
        prec.data <- n / sigma^2
        prec.prior <- 1 / v
        var <- 1 / (prec.data + prec.prior)
        beta.hat <- betaHat(prior)
        mean <- (prec.data * vbar + prec.prior * beta.hat) * var
        sd <- sqrt(var)
        stats::rnorm(n = J, mean = mean, sd = sd)
    }
}

## TRANSLATED
## HAS_TESTS
updateBetaScaled <- function(prior, vbar, n, sigma, useC = FALSE) {
    checkUpdateBetaAndPriorBeta(prior = prior,
                                vbar = vbar,
                                n = n,
                                sigma = sigma)
    stopifnot(methods::is(prior, "Exch"))
    stopifnot(methods::is(prior, "ZeroMixin"))
    if (useC) {
        .Call(updateBetaScaled_R, prior, vbar, n, sigma)
    }
    else {
        J <- prior@J@.Data
        A <- prior@ATau@.Data
        zeta <- prior@zeta
        K <- vbar / (A * zeta)
        v <- getV(prior)
        prec.data <- (n * A^2 * zeta^2) / sigma^2
        prec.prior <- 1 / v
        var <- 1 / (prec.data + prec.prior)
        mean <- prec.data * K * var
        sd <- sqrt(var)
        stats::rnorm(n = J, mean = mean, sd = sd)
    }
}

## TRANSLATED
## HAS_TESTS
updateBetasAndPriorsBetas <- function(object, g, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    ## g
    stopifnot(is.function(g))
    if (useC) {
        .Call(updateBetasAndPriorsBetas_R, object) ## drop g
    }
    else {
        theta <- object@theta
        betas <- object@betas
        sigma <- object@sigma
        I <- length(theta)
        for (b in seq_along(betas)) {
            vbar <- makeVBar(object, iBeta = b, g = g)  ## uses updated object
            n <- I %/% length(vbar)
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


## TRANSLATED
## HAS_TESTS
updateEta <- function(prior, beta, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "CovariatesMixin"))
    stopifnot(methods::validObject(prior))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(!any(is.na(beta)))
    stopifnot(identical(length(beta), nrow(prior@Z)))
    if (useC) {
        .Call(updateEta_R, prior, beta)
    }
    else {
        P <- prior@P@.Data
        Z <- prior@Z
        Z <- unname(Z)
        A.eta.intercept <- prior@AEtaIntercept@.Data
        A.eta.coef <- prior@AEtaCoef@.Data
        U.eta.coef <- prior@UEtaCoef@.Data
        nu.eta.coef <- prior@nuEtaCoef@.Data
        v <- getV(prior)
        U.eta <- c(A.eta.intercept^2, U.eta.coef)
        var.inv <- crossprod(Z, diag(1 / v)) %*% Z + diag(1 / U.eta)
        qr <- qr(var.inv)
        b <- crossprod(Z, diag(1 / v)) %*% beta
        eta.hat <- qr.solve(qr, b)
        eta.hat <- drop(eta.hat)
        g <- stats::rnorm(n = P)
        R <- qr.R(qr)
        epsilon <- backsolve(R, g)
        prior@eta@.Data <- eta.hat + epsilon
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateGWithTrend <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "WithTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateGWithTrend_R, prior)
    }
    else {
        prior@GWithTrend@.Data[4L] <- prior@phi
        prior
    }
}

## TRANSLATED
## HAS_TESTS
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
        iterator <- resetA(iterator)
        V <- 0
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                k.curr <- indices[i + 1]
                k.prev <- indices[i]
                if (withTrend)
                    V <- V + (alpha[k.curr] - alpha[k.prev] - delta[k.prev])^2
                else
                    V <- V + (alpha[k.curr] - phi * alpha[k.prev])^2
            }
            iterator <- advanceA(iterator)
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = J,
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
        iterator <- resetA(iterator)
        V <- 0
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                k.curr <- indices[i + 1]
                k.prev <- indices[i]
                V <- V + (delta[k.curr] - phi * delta[k.prev])^2
            }
            iterator <- advanceA(iterator)
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = J,
                              max = omegaMax)
        successfully.updated <- omega > 0
        if (successfully.updated)
            prior@omegaDelta@.Data <- omega
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
        iterator <- resetA(iterator)
        V <- 0
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                i.curr <- indices[i + 1L]
                i.prev <- indices[i]
                curr <- s[[i.curr]][1L]
                prev <- s[[i.prev]][n.season]
                V <- V + (curr - prev)^2
            }
            iterator <- advanceA(iterator)
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = J,
                              max = omegaMax)
        successfully.updated <- omega > 0
        if (successfully.updated)
            prior@omegaSeason@.Data <- omega
        prior
    }
}

## TRANSLATED
## HAS_TESTS
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
        kMaxAttempt <- 1000L  # in C use macro to set this
        phi.known <- prior@phiKnown@.Data
        if (phi.known)
            return(prior)
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
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        numerator <- 0
        denominator <- 0
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                k.curr <- indices[i + 1]
                k.prev <- indices[i]
                numerator <- numerator + state[k.curr] * state[k.prev]
                denominator <- denominator + (state[k.prev])^2
            }
            iterator <- advanceA(iterator)
        }
        mean <- numerator / denominator
        sd <- omega / sqrt(denominator)
        found.value <- FALSE
        for (i in seq_len(kMaxAttempt)) {
            phi.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
            within.limits <- (phi.min <= phi.prop) && (phi.prop <= phi.max)
            if (within.limits) {
                found.value <- TRUE
                break
            }
        }
        if (found.value)
            prior@phi <- phi.prop
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
        iterator.s <- resetA(iterator.s)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
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
        }
        prior@s@.Data <- s
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateTauNorm <- function(prior, beta, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "NormMixin"))
    stopifnot(!methods::is(prior, "ExchNormZero"))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(!any(is.na(beta)))
    ## prior and beta
    stopifnot(identical(length(beta), as.integer(prior@J)))
    if (useC) {
        .Call(updateTauNorm_R, prior, beta)
    }
    else {
        J <- prior@J@.Data
        tau <- prior@tau@.Data
        tauMax <- prior@tauMax@.Data
        A <- prior@ATau@.Data
        nu <- prior@nuTau@.Data
        beta.hat <- betaHat(prior)
        V <- sum((beta - beta.hat)^2)
        tau <- updateSDNorm(sigma = tau,
                            A = A,
                            nu = nu,
                            V = V,
                            n = J,
                            max = tauMax)
        successfully.updated <- tau > 0
        if (successfully.updated)
            prior@tau@.Data <- tau
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateTauRobust <- function(prior, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "RobustMixin"))
    if (useC) {
        .Call(updateTauRobust_R, prior)
    }
    else {
        J <- prior@J@.Data
        UBeta <- prior@UBeta@.Data
        nuBeta <- prior@nuBeta@.Data
        tau <- prior@tau@.Data
        tauMax <- prior@tauMax@.Data
        A <- prior@ATau@.Data
        nuTau <- prior@nuTau@.Data
        V <- sum(1/UBeta)
        tau <- updateSDRobust(sigma = tau,
                              A = A,
                              nuBeta = nuBeta,
                              nuTau = nuTau,
                              V = V,
                              n = J,
                              max = tauMax)
        successfully.updated <- tau > 0
        if (successfully.updated)
            prior@tau@.Data <- tau
        prior
    }
}

## TRANSLATED
## HAS_TESTS
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
        beta.hat <- betaHat(prior)
        df <- nu + 1
        scale <- (nu * tau^2 + (beta - beta.hat)^2) / df
        for (j in seq_len(J))
            U[j] <- rinvchisq1(df = df, scale = scale[j])
        prior@UBeta@.Data <- U
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateUEtaCoef <- function(prior, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "CovariatesMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateUEtaCoef_R, prior)
    }
    else {
        P <- prior@P@.Data
        U <- prior@UEtaCoef@.Data
        nu <- prior@nuEtaCoef@.Data
        A <- prior@AEtaCoef@.Data
        eta <- prior@eta@.Data
        df <- nu + 1
        for (p in seq_len(P - 1L)) {
            scale <- (nu * A^2 + eta[p + 1]^2) / df
            U[p] <- rinvchisq1(df = df, scale = scale)
        }
        prior@UEtaCoef@.Data <- U
        prior
    }
}
 
## TRANSLATED
## HAS_TESTS
updateWSqrt <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "WithTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateWSqrt_R, prior)
    }
    else {
        WSqrt <- prior@WSqrt@.Data
        omegaAlpha <- prior@omegaAlpha@.Data
        omegaDelta <- prior@omegaDelta@.Data
        WSqrt[1L] <- omegaAlpha^2
        WSqrt[4L] <- omegaDelta^2
        prior@WSqrt@.Data <- WSqrt
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateWSqrtInvG <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "WithTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateWSqrtInvG_R, prior)
    }
    else {
        WSqrtInvG <- prior@WSqrtInvG@.Data
        omegaAlpha <- prior@omegaAlpha@.Data
        omegaDelta <- prior@omegaDelta@.Data
        phi <- prior@phi
        WSqrtInvG[1L] <- 1 / omegaAlpha
        WSqrtInvG[3L] <- 1 / omegaAlpha
        WSqrtInvG[4L] <- phi / omegaDelta
        prior@WSqrtInvG@.Data <- WSqrtInvG
        prior
    }
}

## TRANSLATED
## HAS_TESTS
updateZetaAndTau <- function(prior, betaScaled, vbar, n, sigma, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Exch"))
    stopifnot(methods::is(prior, "ZeroMixin"))
    stopifnot(methods::validObject(prior))
    ## vbar
    stopifnot(is.double(vbar))
    stopifnot(identical(length(vbar), as.integer(prior@J)))
    stopifnot(!any(is.na(vbar)))
    ## n
    stopifnot(is.integer(n))
    stopifnot(identical(length(n), 1L))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## sigma
    stopifnot(is.double(sigma))
    stopifnot(identical(length(sigma), 1L))
    stopifnot(!is.na(sigma))
    stopifnot(sigma > 0)
    if (useC) {
        .Call(updateZetaAndTau_R, prior, betaScaled, vbar, n, sigma)
    }
    else {
        J <- prior@J@.Data
        A <- prior@ATau@.Data
        nu <- prior@nuTau@.Data
        tau.max <- prior@tauMax@.Data
        L <- sum(vbar / betaScaled) / (J * A)
        prec.data <- (n * J^2 * A^2)  / (sigma^2 * sum(1/betaScaled^2))
        var <- 1 / (prec.data + 1)
        mean <- prec.data * L * var
        sd <- sqrt(var)
        zeta <- stats::rnorm(n = 1L, mean = mean, sd = sd)
        df <- nu + J
        scale <- (nu + sum(betaScaled^2)) / df
        tau.sq.scaled <- rinvchisq1(df = df, scale = scale)
        tau.scaled <- sqrt(tau.sq.scaled)
        tau <- A * abs(zeta) * tau.scaled
        if (tau < tau.max) {
            prior@zeta <- zeta
            prior@tauScaled@.Data <- tau.scaled
            prior@tau@.Data <- tau
        }
        prior
    }
}


## UPDATING MODELS ##################################################################

## TRANSLATED
## HAS_TESTS
updateSigma_Varying <- function(object, g, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    ## g
    stopifnot(is.function(g))
    if (useC) {
        .Call(updateSigma_Varying_R, object)
    }
    else {
        sigma <- object@sigma@.Data
        sigma.max <- object@sigmaMax@.Data
        A <- object@ASigma@.Data
        nu <- object@nuSigma@.Data
        theta <- object@theta
        betas <- object@betas
        iterator <- object@iteratorBetas
        iterator <- resetB(iterator)
        n <- length(theta)
        V <- 0
        for (i in seq_len(n)) {
            transformed.theta <- g(theta[i])
            indices <- iterator@indices
            mu <- 0
            for (b in seq_along(betas))
                mu <- mu + betas[[b]][indices[b]]
            V <- V + (transformed.theta - mu)^2
            iterator <- advanceB(iterator)
        }
        sigma <- updateSDNorm(sigma = sigma,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n,
                              max = sigma.max)
        successfully.updated <- sigma > 0
        if (successfully.updated)
            object@sigma@.Data <- sigma
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateTheta_BinomialVarying <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "BinomialVarying"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(is.integer(exposure))
    ## y and exposure
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(exposure[!is.na(y)] >= y[!is.na(y)]))
    if (useC) {
        .Call(updateTheta_BinomialVarying_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        scale <- object@scaleTheta
        scale.multiplier <- object@scaleThetaMultiplier
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma
        betas <- object@betas
        iterator <- object@iteratorBetas
        max.attempt <- object@maxAttempt
        n.failed.prop.theta <- 0L
        n.accept.theta <- 0L
        iterator <- resetB(iterator)
        scale <- scale * scale.multiplier
        for (i in seq_along(theta)) {
            indices <- iterator@indices
            mu <- 0
            for (b in seq_along(betas))
                mu <- mu + betas[[b]][indices[b]]
            y.is.missing <- is.na(y[i])
            if (y.is.missing) {
                mean <- mu
                sd <- sigma
            }
            else {
                th.curr <- theta[i]
                logit.th.curr <- log(th.curr / (1 - th.curr))
                mean <- logit.th.curr
                sd <- scale / sqrt(1 + log(1 + exposure[i]))
            }
            found.prop <- FALSE
            attempt <- 0L
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                logit.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                found.prop <- ((logit.th.prop > lower + tolerance)
                               && (logit.th.prop < upper - tolerance))
            }
            if (found.prop) {
                if (logit.th.prop > 0)
                    th.prop <- 1 / (1 + exp(-logit.th.prop))
                else
                    th.prop <- exp(logit.th.prop) / (1 + exp(logit.th.prop))
                if (y.is.missing) 
                    theta[i] <- th.prop
                else {                 
                    log.lik.prop <- stats::dbinom(x = y[i], size = exposure[i], prob = th.prop, log = TRUE)
                    log.lik.curr <- stats::dbinom(x = y[i], size = exposure[i], prob = th.curr, log = TRUE)
                    ## The Jacobians from the transformation of variables cancel,
                    ## as do the normal densitites in the proposal distributions.
                    log.dens.prop <- stats::dnorm(x = logit.th.prop, mean = mu, sd = sigma, log = TRUE)
                    log.dens.curr <- stats::dnorm(x = logit.th.curr, mean = mu, sd = sigma, log = TRUE)
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
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateTheta_BinomialVaryingAgCertain <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "BinomialVarying"))
    stopifnot(methods::is(object, "Aggregate"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.integer(exposure))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(exposure[!is.na(y)] >= y[!is.na(y)]))
    if (useC) {
        .Call(updateTheta_BinomialVaryingAgCertain_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        scale.theta <- object@scaleTheta@.Data
        scale.theta.multiplier <- object@scaleThetaMultiplier@.Data
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma@.Data
        betas <- object@betas
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        max.attempt <- object@maxAttempt
        iterator <- object@iteratorBetas
        n.accept.theta <- 0L
        n.failed.prop.theta <- 0L
        n.theta <- length(theta)
        mu <- makeMu(n = n.theta, betas = betas, iterator = iterator)
        iterator <- resetB(iterator)
        scale.theta <- scale.theta * scale.theta.multiplier
        for (i in seq_len(n.theta)) {
            scale.theta.i <- scale.theta / sqrt(1 + log(1 + exposure[i]))
            ## determine type of update
            i.other <- makeIOther(i = i, transform = transform.ag, useC = TRUE)
            in.delta <- i.other >= 0L
            if (in.delta) {
                has.other <- i.other > 0L
                weight <- weight.ag[i]
                weight.positive <- weight > 0
                if (has.other) {
                    weight.other <- weight.ag[i.other]
                    weight.other.positive <- weight.other > 0
                }
            }
            value.fixed <- (in.delta
                            && weight.positive
                            && (!has.other || !weight.other.positive))
            if (value.fixed)
                next
            update.pair <- (in.delta
                            && weight.positive
                            && has.other
                            && weight.other.positive)
            ## generate proposal
            found.prop <- FALSE
            attempt <- 0L
            th.curr <- theta[i]
            logit.th.curr <- log(th.curr / (1 - th.curr))
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                logit.th.prop <- stats::rnorm(n = 1L, mean = logit.th.curr, sd = scale.theta.i)
                inside.limits <- ((logit.th.prop > lower + tolerance)
                                  && (logit.th.prop < upper - tolerance))
                if (inside.limits) {
                    if (logit.th.prop > 0)
                        th.prop <- 1 / (1 + exp(-logit.th.prop))
                    else
                        th.prop <- exp(logit.th.prop) / (1 + exp(logit.th.prop))
                    if (update.pair) {
                        th.other.curr <- theta[i.other]
                        th.other.prop <- ((th.curr - th.prop) * weight / weight.other
                                          + th.other.curr)
                        ## This test a bit awkward, but is required when expressing 'lower'
                        ## 'upper' on a logit scale. Using the logit scale makes sense in
                        ## an model without aggregate values, which is the typical case.
                        valid <- (0 < th.other.prop) && (th.other.prop < 1) 
                        if (valid) {
                            logit.th.other.prop <- log(th.other.prop / (1 - th.other.prop))
                            found.prop <- ((logit.th.other.prop > lower + tolerance)
                                           && (logit.th.other.prop < upper - tolerance))
                        }
                    }
                    else
                        found.prop <- TRUE
                }
            }
            if (!found.prop) {  ## reached 'maxAttempt' without generating proposal
                n.failed.prop.theta <- n.failed.prop.theta + 1L
                next
            }
            log.diff <- 0
            ## calculate likelihoods (if used)
            if (!is.na(y[i])) {
                log.diff <- log.diff + (stats::dbinom(x = y[i],
                                               size = exposure[i],
                                               prob = th.prop,
                                               log = TRUE)
                                        - stats::dbinom(x = y[i],
                                                 size = exposure[i],
                                                 prob = th.curr,
                                                 log = TRUE))
            }
            if (update.pair) {
                if (!is.na(y[i.other])) {
                    log.diff <- log.diff + (stats::dbinom(x = y[i.other],
                                                   size = exposure[i.other],
                                                   prob = th.other.prop,
                                                   log = TRUE)
                                            - stats::dbinom(x = y[i.other],
                                                     size = exposure[i.other],
                                                     prob = th.other.curr,
                                                     log = TRUE))
                }
            }
            ## calculate prior and proposal densities
            if (update.pair) {
                logit.th.other.curr <- log(th.other.curr / (1 - th.other.curr))
                ## need to include Jacobians, since they do not cancel
                ## with the proposal density, which is additive and nasty
                log.diff.prior <- (-log(th.prop * (1 - th.prop))
                                   + stats::dnorm(x = logit.th.prop,
                                           mean = mu[i],
                                           sd = sigma,
                                           log = TRUE)
                                   - log(th.other.prop * (1 - th.other.prop))
                                   + stats::dnorm(x = logit.th.other.prop,
                                           mean = mu[i.other],
                                           sd = sigma,
                                           log = TRUE)
                                   + log(th.curr * (1 - th.curr))
                                   - stats::dnorm(x = logit.th.curr,
                                           mean = mu[i],
                                           sd = sigma,
                                           log = TRUE)
                                   + log(th.other.curr * (1 - th.other.curr))
                                   - stats::dnorm(x = logit.th.other.curr,
                                           mean = mu[i.other],
                                           sd = sigma,
                                           log = TRUE))
                log.diff.prop <-
                    (safeLogProp_Binomial(logit.th.new = logit.th.curr,
                                          logit.th.other.new = logit.th.other.curr,
                                          logit.th.old = logit.th.prop,
                                          logit.th.other.old = logit.th.other.prop,
                                          scale = scale.theta.i,
                                          weight = weight,
                                          weight.other = weight.other)
                     - safeLogProp_Binomial(logit.th.new = logit.th.prop,
                                            logit.th.other.new = logit.th.other.prop,
                                            logit.th.old = logit.th.curr,
                                            logit.th.other.old = logit.th.other.curr,
                                            scale = scale.theta.i,
                                            weight = weight,
                                            weight.other = weight.other))
                log.diff <- log.diff + log.diff.prior + log.diff.prop
            }
            else {
                ## Jacobian from prior density cancels with proposal density
                log.diff <- log.diff + (stats::dnorm(x = logit.th.prop,
                                              mean = mu[i],
                                              sd = sigma,
                                              log = TRUE)
                                        - stats::dnorm(x = logit.th.curr,
                                                mean = mu[i],
                                                sd = sigma,
                                                log = TRUE))
            }
            ## acceptance
            accept <- (log.diff >= 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.theta <- n.accept.theta + 1L
                theta[i] <- th.prop
                if (update.pair)
                    theta[i.other] <- th.other.prop
            }
        }
        object@theta <- theta
        object@mu <- mu
        object@nAcceptTheta@.Data <- n.accept.theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgNormal_Binomial <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "BinomialVarying"))
    stopifnot(methods::is(object, "AgNormal"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.integer(exposure))
    stopifnot(identical(length(exposure), length(y)))
    ## y and exposure
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(exposure[!is.na(y)] >= y[!is.na(y)]))
    if (useC) {
        .Call(updateThetaAndValueAgNormal_Binomial_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        mu <- object@mu
        sigma <- object@sigma@.Data
        value.ag <- object@valueAg@.Data
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mean.ag <- object@meanAg@.Data
        sd.ag <- object@sdAg@.Data
        scale.ag <- object@scaleAg@.Data
        max.attempt <- object@maxAttempt
        n.failed.prop.value.ag <- 0L
        n.accept.ag <- 0L
        for (k in seq_along(value.ag)) {
            ## Each cell 'k' in the aggregate values has a set of associated 'i' in 'theta'.
            ## Construct vectors holding information on these 'theta'.  Use 'vec'
            ## prefix to distinguish the (length > 1) vectors.
            i.ag <- dembase::getIBefore(k, transform = transform.ag, useC = TRUE)
            n.ag <- length(i.ag)
            vec.th.curr <- theta[i.ag]
            vec.logit.th.curr <- log(vec.th.curr / (1 - vec.th.curr))
            vec.th.prop <- numeric(length = n.ag)
            vec.logit.th.prop <- numeric(length = n.ag)
            attempt <- 0L
            found.prop <- FALSE
            while (!found.prop && (attempt < max.attempt)) {
                ## attempt to generate a new set of 'theta', and hence a new
                ## value for aggregate value k by adding random increments to
                ## the 'theta' (on the logit scale)
                attempt <- attempt + 1L
                for (i in seq_len(n.ag)) {
                    increment <- stats::rnorm(n = 1L, mean = 0, sd = scale.ag)
                    logit.th.prop <- vec.logit.th.curr[i] + increment
                    inside.limits <- ((logit.th.prop > lower + tolerance)
                                      && (logit.th.prop < upper - tolerance))
                    if (!inside.limits)
                        break
                    if (logit.th.prop > 0) {
                        th.prop <- 1 / (1 + exp(-logit.th.prop))
                        valid <- th.prop < 1.0
                    }
                    else {
                        th.prop <- exp(logit.th.prop) / (1 + exp(logit.th.prop))
                        valid <- th.prop > 0.0
                    }
                    if (!valid)
                        break
                    vec.logit.th.prop[i] <- logit.th.prop
                    vec.th.prop[i] <- th.prop
                    found.prop <- i == n.ag
                }
            }
            if (!found.prop) { ## if found.prop is FALSE, reached 'maxAttempt'
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
            log.diff.lik <- (sum(stats::dbinom(x = vec.y[is.observed],
                                        size = vec.exp[is.observed],
                                        prob = vec.th.prop[is.observed],
                                        log = TRUE))
                             - sum(stats::dbinom(x = vec.y[is.observed],
                                          size = vec.exp[is.observed],
                                          prob = vec.th.curr[is.observed],
                                          log = TRUE)))
            ## do not include Jacobians, since they cancel with proposal densities
            log.diff.prior <- (sum(stats::dnorm(x = vec.logit.th.prop,
                                         mean = vec.mu,
                                         sd = sigma,
                                         log = TRUE))
                               - sum(stats::dnorm(x = vec.logit.th.curr,
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
            accept <- (log.diff >= 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.ag <- n.accept.ag + 1L
                value.ag[k] <- ag.prop
                theta[i.ag] <- vec.th.prop
            }
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@nFailedPropValueAg@.Data <- n.failed.prop.value.ag
        object@nAcceptAg@.Data <- n.accept.ag
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgFun_Binomial <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "BinomialVarying"))
    stopifnot(methods::is(object, "AgFun"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.integer(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y[!is.na(y)] <= exposure[!is.na(y)]))
    if (useC) {
        .Call(updateThetaAndValueAgFun_Binomial_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        scale <- object@scaleTheta
        scale.multiplier <- object@scaleThetaMultiplier
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma@.Data
        betas <- object@betas
        iterator <- object@iteratorBetas
        value.ag <- object@valueAg@.Data
        mean.ag <- object@meanAg@.Data
        sd.ag <- object@sdAg@.Data
        transform.ag <- object@transformAg
        fun.ag <- object@funAg
        x.args.ag <- object@xArgsAg # list of "Values" objects
        weights.args.ag <- object@weightsArgsAg # list of "Counts" objects
        max.attempt <- object@maxAttempt
        n.failed.prop.theta <- 0L
        n.accept.theta <- 0L
        iterator <- resetB(iterator)
        scale <- scale * scale.multiplier
        n.shared <- length(x.args.ag[[1L]]@.Data)
        for (i in seq_along(theta)) {
            indices <- iterator@indices
            mu <- 0
            for (b in seq_along(betas))
                mu <- mu + betas[[b]][indices[b]]
            i.ag <- getIAfter(i = i,
                              transform = transform.ag,
                              check = FALSE,
                              useC = TRUE)
            contributes.to.ag <- i.ag > 0L
            y.is.missing <- is.na(y[i])
            th.curr <- theta[i]
            logit.th.curr <- log(th.curr / (1 - th.curr))
            if (y.is.missing) {
                mean <- mu
                sd <- sigma
            }
            else {
                mean <- logit.th.curr
                sd <- scale / sqrt(1 + log(1 + exposure[i]))
            }
            found.prop <- FALSE
            attempt <- 0L
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                logit.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                found.prop <- ((logit.th.prop > lower + tolerance)
                               && (logit.th.prop < upper - tolerance))
            }
            if (found.prop) {
                if (logit.th.prop > 0)
                    th.prop <- 1 / (1 + exp(-logit.th.prop))
                else
                    th.prop <- exp(logit.th.prop) / (1 + exp(logit.th.prop))
                draw.straight.from.prior <- y.is.missing && !contributes.to.ag
                if (draw.straight.from.prior)
                    theta[i] <- th.prop
                else {
                    if (y.is.missing)
                        log.diff <- 0
                    else {
                        log.lik.prop <- stats::dbinom(y[i], prob = th.prop, size = exposure[i], log = TRUE)
                        log.lik.curr <- stats::dbinom(y[i], prob = th.curr, size = exposure[i], log = TRUE)
                        log.diff <- log.lik.prop - log.lik.curr
                    }
                    log.dens.prop <- stats::dnorm(x = logit.th.prop, mean = mu, sd = sigma, log = TRUE)
                    log.dens.curr <- stats::dnorm(x = logit.th.curr, mean = mu, sd = sigma, log = TRUE)
                    log.diff <- log.diff + log.dens.prop - log.dens.curr
                    if (contributes.to.ag) {
                        ag.curr <- value.ag[i.ag]
                        mean <- mean.ag[i.ag]
                        sd <- sd.ag[i.ag]
                        weights <- weights.args.ag[[i.ag]]
                        x <- x.args.ag[[i.ag]]
                        i.shared <- dembase::getIShared(i = i,
                                                        transform = transform.ag,
                                                        useC = TRUE)
                        x@.Data[i.shared == i] <- th.prop
                        ag.prop <- fun.ag(x = x, weights = weights)
                        log.dens.ag.prop <- stats::dnorm(x = mean, mean = ag.prop, sd = sd, log = TRUE)
                        log.dens.ag.curr <- stats::dnorm(x = mean, mean = ag.curr, sd = sd, log = TRUE)
                        log.diff <- log.diff + log.dens.ag.prop - log.dens.ag.curr
                    }
                    accept <- (log.diff >= 0) || (stats::runif(n = 1L) < exp(log.diff))
                    if (accept) {
                        n.accept.theta <- n.accept.theta + 1L
                        theta[i] <- th.prop
                        if (contributes.to.ag) {
                            x.args.ag[[i.ag]] <- x
                            value.ag[i.ag] <- ag.prop
                        }
                    }
                }
            }
            else
                n.failed.prop.theta <- n.failed.prop.theta + 1L
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@xArgsAg <- x.args.ag
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateTheta_NormalVarying <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "NormalVarying"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "DemographicArray"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.double(y))
    if (useC) {
        .Call(updateTheta_NormalVarying_R, object, y)
    }
    else {
        theta <- object@theta
        lower <- object@lower
        upper <- object@upper
        max.attempt <- object@maxAttempt
        tolerance <- object@tolerance
        w <- object@w
        varsigma <- object@varsigma@.Data
        sigma <- object@sigma@.Data
        betas <- object@betas
        iterator <- object@iteratorBetas
        max.attempt <- object@maxAttempt
        prec.prior <- 1 / (sigma^2)
        varsigma.sq <- varsigma^2
        n.failed.prop.theta <- 0L
        iterator <- resetB(iterator)
        for (i in seq_along(theta)) {
            indices <- iterator@indices
            mu <- 0
            for (b in seq_along(betas))
                mu <- mu + betas[[b]][indices[b]]
            y.is.missing <- is.na(y[i])
            if (y.is.missing) {
                sd <- sigma
                mean <- mu
            }
            else {
                prec.data <- w[i] / varsigma.sq
                var <- 1 / (prec.data + prec.prior)
                sd <- sqrt(var)
                mean <- var * (prec.prior * mu + prec.data * y[i])
            }
            found <- FALSE
            n.attempt <- 0L
            while (!found && (n.attempt < max.attempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                found <- ((prop.value > (lower + tolerance))
                          && (prop.value < (upper - tolerance)))
            }
            if (found)
                theta[i] <- prop.value
            else
                n.failed.prop.theta <- n.failed.prop.theta + 1L
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateTheta_NormalVaryingAgCertain <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "NormalVarying"))
    stopifnot(methods::is(object, "Aggregate"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "DemographicArray"))
    stopifnot(is.double(y))
    stopifnot(identical(length(y), length(object@theta)))
    if (useC) {
        .Call(updateTheta_NormalVaryingAgCertain_R, object, y)
    }
    else {
        theta <- object@theta
        scale.theta <- object@scaleTheta@.Data  ## NEW
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        w <- object@w
        varsigma <- object@varsigma@.Data
        sigma <- object@sigma@.Data
        betas <- object@betas
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mu <- object@mu
        max.attempt <- object@maxAttempt
        iterator <- object@iteratorBetas
        n.theta <- length(theta)
        n.accept.theta <- 0L
        n.failed.prop.theta <- 0L
        mu <- makeMu(n = n.theta, betas = betas, iterator = iterator)
        for (i in seq_len(n.theta)) {
            ## determine type of update
            i.other <- makeIOther(i = i, transform = transform.ag, useC = TRUE)
            in.delta <- i.other >= 0L
            if (in.delta) {
                has.other <- i.other > 0L
                weight <- weight.ag[i]
                weight.positive <- weight > 0
                if (has.other) {
                    weight.other <- weight.ag[i.other]
                    weight.other.positive <- weight.other > 0
                }
            }
            value.fixed <- (in.delta
                            && weight.positive
                            && (!has.other || !weight.other.positive))
            if (value.fixed)
                next
            update.pair <- (in.delta
                            && weight.positive
                            && has.other
                            && weight.other.positive)
            ## generate proposal
            found.prop <- FALSE
            attempt <- 0L
            th.curr <- theta[i]
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                th.prop <- stats::rnorm(n = 1L, mean = th.curr, sd = scale.theta)
                prop.in.range <- ((th.prop > lower + tolerance)
                                  && (th.prop < upper - tolerance))
                if (!prop.in.range)
                    next
                if (update.pair) {
                    th.other.curr <- theta[i.other]
                    th.other.prop <- (th.curr - th.prop) * weight / weight.other + th.other.curr
                    found.prop <- ((th.other.prop > lower + tolerance)
                                   && (th.other.prop < upper - tolerance))
                }
                else
                    found.prop <- TRUE
            }
            if (!found.prop) {  ## reached 'maxAttempt' without generating proposal
                n.failed.prop.theta <- n.failed.prop.theta + 1L
                next
            }
            log.diff <- 0
            ## calculate likelihoods (if used)
            y.i <- y[i]
            if (!is.na(y.i)) {
                sd.i <- varsigma / sqrt(w[i])
                log.diff <- log.diff + (stats::dnorm(x = y.i,
                                              mean = th.prop,
                                              sd = sd.i,
                                              log = TRUE)
                                        - stats::dnorm(x = y.i,
                                                mean = th.curr,
                                                sd = sd.i,
                                                log = TRUE))
            }
            if (update.pair) {
                y.other <- y[i.other]
                if (!is.na(y.other)) {
                    sd.other <- varsigma / sqrt(w[i.other])
                    log.diff <- log.diff + (stats::dnorm(x = y.other,
                                                  mean = th.other.prop,
                                                  sd = sd.other,
                                                  log = TRUE)
                                            - stats::dnorm(x = y.other,
                                                    mean = th.other.curr,
                                                    sd = sd.other,
                                                    log = TRUE))
                }
            }
            ## calculate prior and proposal densities
            mu.i <- mu[i]
            if (update.pair) {
                mu.other <- mu[i.other]
                log.diff.prior <- (stats::dnorm(x = th.prop,
                                         mean = mu.i,
                                         sd = sigma,
                                         log = TRUE)
                                   + stats::dnorm(x = th.other.prop,
                                           mean = mu.other,
                                           sd = sigma,
                                           log = TRUE)
                                   - stats::dnorm(x = th.curr,
                                           mean = mu.i,
                                           sd = sigma,
                                           log = TRUE)
                                   - stats::dnorm(x = th.other.curr,
                                           mean = mu.other,
                                           sd = sigma,
                                           log = TRUE))
                weight.ratio <- abs(weight / weight.other)
                log.diff.prop <- (log(stats::dnorm(x = th.curr,
                                            mean = th.prop,
                                            sd = scale.theta)
                                      + weight.ratio
                                      * stats::dnorm(x = th.other.curr,
                                              mean = th.other.prop,
                                              sd = scale.theta))
                                  - log(stats::dnorm(x = th.prop,
                                              mean = th.curr,
                                              sd = scale.theta)
                                        + weight.ratio
                                        * stats::dnorm(x = th.other.prop,
                                                mean = th.other.curr,
                                                sd = scale.theta)))
                log.diff <- log.diff + log.diff.prior + log.diff.prop
            }
            else {
                ## proposal densities cancel
                log.diff <- log.diff + (stats::dnorm(x = th.prop,
                                              mean = mu.i,
                                              sd = sigma,
                                              log = TRUE)
                                        - stats::dnorm(x = th.curr,
                                                mean = mu.i,
                                                sd = sigma,
                                                log = TRUE))
            }
            ## acceptance
            accept <- (log.diff >= 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.theta <- n.accept.theta + 1L
                theta[i] <- th.prop
                if (update.pair)
                    theta[i.other] <- th.other.prop
            }
        }
        object@theta <- theta
        object@mu <- mu
        object@nAcceptTheta@.Data <- n.accept.theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgNormal_Normal <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "NormalVarying"))
    stopifnot(methods::is(object, "AgNormal"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "DemographicArray"))
    stopifnot(is.double(y))
    stopifnot(identical(length(y), length(object@theta)))
    if (useC) {
        .Call(updateThetaAndValueAgNormal_Normal_R, object, y)
    }
    else {
        theta <- object@theta
        w <- object@w
        varsigma <- object@varsigma@.Data
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        mu <- object@mu
        sigma <- object@sigma@.Data
        value.ag <- object@valueAg@.Data
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mean.ag <- object@meanAg@.Data
        sd.ag <- object@sdAg@.Data
        scale.ag <- object@scaleAg@.Data
        max.attempt <- object@maxAttempt
        n.failed.prop.value.ag <- 0L
        n.accept.ag <- 0L
        for (k in seq_along(value.ag)) {
            ## Each cell 'k' in the aggregate values has a set of associated 'i' in 'theta'.
            ## Construct vectors holding information on these 'theta'.  Use 'vec'
            ## prefix to distinguish the (length > 1) vectors.
            i.ag <- dembase::getIBefore(k, transform = transform.ag, useC = TRUE)
            n.ag <- length(i.ag)
            vec.th.curr <- theta[i.ag]
            vec.th.prop <- numeric(length = n.ag)
            attempt <- 0L
            found.prop <- FALSE
            while (!found.prop && (attempt < max.attempt)) {
                ## attempt to generate a new set of 'theta', and hence a new
                ## value for aggregate value k by adding increments to
                ## the 'theta'
                attempt <- attempt + 1L
                for (i in seq_len(n.ag)) {
                    increment <- stats::rnorm(n = 1L, mean = 0, sd = scale.ag)
                    th.prop <- vec.th.curr[i] + increment
                    inside.limits <- ((th.prop > (lower + tolerance))
                                      && (th.prop < (upper - tolerance)))
                    if (!inside.limits)
                        break
                    vec.th.prop[i] <- th.prop
                    found.prop <- i == n.ag
                }
            }
            if (!found.prop) { ## if found.prop is FALSE, reached 'maxAttempt'
                n.failed.prop.value.ag <- n.failed.prop.value.ag + 1L
                next
            }
            vec.y <- y[i.ag]
            is.observed <- !is.na(vec.y)
            vec.sd <- varsigma / sqrt(w[i.ag])
            vec.mu <- mu[i.ag]
            vec.weight <- weight.ag[i.ag]
            ag.curr <- value.ag[k]
            ag.prop <- sum(vec.th.prop * vec.weight)
            mean.k <- mean.ag[k]
            sd.k <- sd.ag[k]
            log.diff.lik <- (sum(stats::dnorm(x = vec.y[is.observed],
                                       mean = vec.th.prop[is.observed],
                                       sd = vec.sd[is.observed],
                                       log = TRUE))
                             - sum(stats::dnorm(x = vec.y[is.observed],
                                         mean = vec.th.curr[is.observed],
                                         sd = vec.sd[is.observed],
                                         log = TRUE)))
            log.diff.prior <- (sum(stats::dnorm(x = vec.th.prop,
                                         mean = vec.mu,
                                         sd = sigma, log = TRUE))
                               - sum(stats::dnorm(x = vec.th.curr,
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
            accept <- (log.diff >= 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.ag <- n.accept.ag + 1L
                value.ag[k] <- ag.prop
                theta[i.ag] <- vec.th.prop
            }
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@nFailedPropValueAg@.Data <- n.failed.prop.value.ag
        object@nAcceptAg@.Data <- n.accept.ag
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgFun_Normal <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "NormalVarying"))
    stopifnot(methods::is(object, "AgFun"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "DemographicArray"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.double(y))
    if (useC) {
        .Call(updateThetaAndValueAgFun_Normal_R, object, y)
    }
    else {
        theta <- object@theta
        scale <- object@scaleTheta
        w <- object@w
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        varsigma <- object@varsigma@.Data
        sigma <- object@sigma@.Data
        betas <- object@betas
        iterator <- object@iteratorBetas
        value.ag <- object@valueAg@.Data
        mean.ag <- object@meanAg@.Data
        sd.ag <- object@sdAg@.Data
        transform.ag <- object@transformAg
        fun.ag <- object@funAg
        x.args.ag <- object@xArgsAg # list of "Values" objects
        weights.args.ag <- object@weightsArgsAg # list of "Counts" objects
        max.attempt <- object@maxAttempt
        n.accept.theta <- 0L
        n.failed.prop.theta <- 0L
        iterator <- resetB(iterator)
        n.shared <- length(x.args.ag[[1L]]@.Data)
        for (i in seq_along(theta)) {
            indices <- iterator@indices
            mu <- 0
            for (b in seq_along(betas))
                mu <- mu + betas[[b]][indices[b]]
            i.ag <- getIAfter(i = i,
                              transform = transform.ag,
                              check = FALSE,
                              useC = TRUE)
            contributes.to.ag <- i.ag > 0L
            y.is.missing <- is.na(y[i])
            th.curr <- theta[i]
            if (y.is.missing) {
                mean <- mu
                sd <- sigma
            }
            else {
                mean <- th.curr
                sd <- scale / sqrt(w[i])
            }
            found.prop <- FALSE
            attempt <- 0L
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                found.prop <- ((th.prop > lower + tolerance)
                               && (th.prop < upper - tolerance))
            }
            if (found.prop) {
                draw.straight.from.prior <- y.is.missing && !contributes.to.ag
                if (draw.straight.from.prior)
                    theta[i] <- th.prop
                else {
                    if (y.is.missing)
                        log.diff <- 0
                    else {
                        log.lik.prop <- stats::dnorm(y[i], mean = th.prop, sd = varsigma / sqrt(w[i]), log = TRUE)
                        log.lik.curr <- stats::dnorm(y[i], mean = th.curr, sd = varsigma / sqrt(w[i]), log = TRUE)
                        log.diff <- log.lik.prop - log.lik.curr
                    }
                    log.dens.prop <- stats::dnorm(x = th.prop, mean = mu, sd = sigma, log = TRUE)
                    log.dens.curr <- stats::dnorm(x = th.curr, mean = mu, sd = sigma, log = TRUE)
                    log.diff <- log.diff + log.dens.prop - log.dens.curr
                    if (contributes.to.ag) {
                        ag.curr <- value.ag[i.ag]
                        mean <- mean.ag[i.ag]
                        sd <- sd.ag[i.ag]
                        weights <- weights.args.ag[[i.ag]]
                        x <- x.args.ag[[i.ag]]
                        i.shared <- dembase::getIShared(i = i,
                                                        transform = transform.ag,
                                                        useC = TRUE)
                        x@.Data[i.shared == i] <- th.prop
                        ag.prop <- fun.ag(x = x, weights = weights)
                        log.dens.ag.prop <- stats::dnorm(x = mean, mean = ag.prop, sd = sd, log = TRUE)
                        log.dens.ag.curr <- stats::dnorm(x = mean, mean = ag.curr, sd = sd, log = TRUE)
                        log.diff <- log.diff + log.dens.ag.prop - log.dens.ag.curr
                    }
                    accept <- (log.diff >= 0) || (stats::runif(n = 1L) < exp(log.diff))
                    if (accept) {
                        n.accept.theta <- n.accept.theta + 1L
                        theta[i] <- th.prop
                        if (contributes.to.ag) {
                            x.args.ag[[i.ag]] <- x
                            value.ag[i.ag] <- ag.prop
                        }
                    }
                }
            }
            else
                n.failed.prop.theta <- n.failed.prop.theta + 1L
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@xArgsAg <- x.args.ag
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

## Includes case where 'y' has subtotals.
## Subtotals can only be used with PoissonVarying models.
## TRANSLATED
## HAS_TESTS
updateTheta_PoissonVaryingNotUseExp <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingNotUseExp"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y[!is.na(y)] >= 0L))
    if (useC) {
        .Call(updateTheta_PoissonVaryingNotUseExp_R, object, y)
    }
    else {
        y.has.subtotals <- methods::is(y, "HasSubtotals")
        if (y.has.subtotals) {
            subtotals <- y@subtotalsNet
            transform <- y@transformSubtotals
        }
        theta <- object@theta
        scale <- object@scaleTheta
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
        for (i in seq_along(theta)) {
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
                log.th.curr <- log(th.curr)
                mean <- log.th.curr
                sd <- scale
            }
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                log.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                found.prop <- ((log.th.prop > lower + tolerance)
                               && (log.th.prop < upper - tolerance))
            }
            if (found.prop) {
                th.prop <- exp(log.th.prop)
                if (draw.straight.from.prior)
                    theta[i] <- th.prop
                else {
                    if (use.subtotal) {
                        subtotal <- subtotals[i.after]
                        i.shared <- dembase::getIShared(i = i, transform = transform)
                        lambda.curr <- 0 
                        for (i.s in i.shared) { 
                            if (is.na(y[i.s])) 
                                lambda.curr <- lambda.curr + theta[i.s]  
                        } 
                        lambda.prop <- lambda.curr + th.prop - th.curr
                        log.lik.prop <- stats::dpois(x = subtotal, lambda = lambda.prop, log = TRUE)
                        log.lik.curr <- stats::dpois(x = subtotal, lambda = lambda.curr, log = TRUE)
                    }
                    else {
                        log.lik.prop <- stats::dpois(y[i], lambda = th.prop, log = TRUE)
                        log.lik.curr <- stats::dpois(y[i], lambda = th.curr, log = TRUE)
                    }
                    log.dens.prop <- stats::dnorm(x = log.th.prop, mean = mu, sd = sigma, log = TRUE)
                    log.dens.curr <- stats::dnorm(x = log.th.curr, mean = mu, sd = sigma, log = TRUE)
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
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
## Includes case where 'y' has subtotals.
## Subtotals can only be used with PoissonVarying models.
updateTheta_PoissonVaryingUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingUseExp"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y[!is.na(y)] >= 0L))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.double(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y[!is.na(y)][exposure[!is.na(y)] == 0] == 0L))
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
                log.th.curr <- log(th.curr)
                mean <- log.th.curr
                sd <- scale / sqrt(1 + log(1 + exposure[i]))
            }
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                log.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                found.prop <- ((log.th.prop > lower + tolerance)
                               && (log.th.prop < upper - tolerance))
            }
            if (found.prop) {
                th.prop <- exp(log.th.prop)
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
                    log.dens.prop <- stats::dnorm(x = log.th.prop, mean = mu, sd = sigma, log = TRUE)
                    log.dens.curr <- stats::dnorm(x = log.th.curr, mean = mu, sd = sigma, log = TRUE)
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
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateTheta_PoissonVaryingNotUseExpAgCertain <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingNotUseExp"))
    stopifnot(methods::is(object, "Aggregate"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0L))
    if (useC) {
        .Call(updateTheta_PoissonVaryingNotUseExpAgCertain_R, object, y)
    }
    else {
        theta <- object@theta
        scale.theta <- object@scaleTheta@.Data
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma@.Data
        betas <- object@betas
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mu <- object@mu
        max.attempt <- object@maxAttempt
        iterator <- object@iteratorBetas
        n.theta <- length(theta)
        n.accept.theta <- 0L
        n.failed.prop.theta <- 0L
        mu <- makeMu(n = n.theta, betas = betas, iterator = iterator)
        for (i in seq_along(theta)) {
            ## determine type of update
            i.other <- makeIOther(i = i, transform = transform.ag, useC = TRUE)
            in.delta <- i.other >= 0L
            if (in.delta) {
                has.other <- i.other > 0L
                weight <- weight.ag[i]
                weight.positive <- weight > 0
                if (has.other) {
                    weight.other <- weight.ag[i.other]
                    weight.other.positive <- weight.other > 0
                }
            }
            value.fixed <- (in.delta
                            && weight.positive
                            && (!has.other || !weight.other.positive))
            if (value.fixed)
                next
            update.pair <- (in.delta
                            && weight.positive
                            && has.other
                            && weight.other.positive)
            ## generate proposal
            found.prop <- FALSE
            attempt <- 0L
            th.curr <- theta[i]
            log.th.curr <- log(th.curr)
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                log.th.prop <- stats::rnorm(n = 1L, mean = log.th.curr, sd = scale.theta)
                prop.in.range <- ((log.th.prop > (lower + tolerance))
                                 && (log.th.prop < (upper - tolerance)))
                if (!prop.in.range)
                    next
                th.prop <- exp(log.th.prop)
                if (update.pair) {
                    th.other.curr <- theta[i.other]
                    th.other.prop <- (th.curr - th.prop) * weight / weight.other + th.other.curr
                    is.positive <- th.other.prop > 0
                    if (is.positive) {
                        log.th.other.prop <- log(th.other.prop)
                        found.prop <- ((log.th.other.prop > (lower + tolerance))
                                       && (log.th.other.prop < (upper - tolerance)))
                    }
                    else
                        found.prop <- FALSE
                }
                else
                    found.prop <- TRUE
            }
            if (!found.prop) {  ## reached 'maxAttempt' without generating proposal
                n.failed.prop.theta <- n.failed.prop.theta + 1L
                next
            }
            log.diff <- 0
            ## calculate likelihoods (if used)
            y.i <- y[i]
            if (!is.na(y.i)) {
                log.diff <- log.diff + (stats::dpois(x = y.i,
                                              lambda = th.prop,
                                              log = TRUE)
                                        - stats::dpois(x = y.i,
                                                lambda = th.curr,
                                                log = TRUE))
            }
            if (update.pair) {
                y.other <- y[i.other]
                if (!is.na(y.other)) {
                    log.diff <- log.diff + (stats::dpois(x = y.other,
                                                  lambda = th.other.prop,
                                                  log = TRUE)
                                            - stats::dpois(x = y.other,
                                                    lambda = th.other.curr,
                                                    log = TRUE))
                }
            }
            ## calculate prior and proposal densities
            mu.i <- mu[i]
            if (update.pair) {
                log.th.other.curr <- log(th.other.curr)
                mu.other <- mu[i.other]
                ## Use log-normal, because it include the Jacobians
                log.diff.prior <- (stats::dlnorm(x = th.prop,
                                          meanlog = mu.i,
                                          sdlog = sigma,
                                          log = TRUE)
                                   + stats::dlnorm(x = th.other.prop,
                                            meanlog = mu.other,
                                            sdlog = sigma,
                                            log = TRUE)
                                   - stats::dlnorm(x = th.curr,
                                            meanlog = mu.i,
                                            sdlog = sigma,
                                            log = TRUE)
                                   - stats::dlnorm(x = th.other.curr,
                                            meanlog = mu.other,
                                            sdlog = sigma,
                                            log = TRUE))
                log.diff.prop <- (safeLogProp_Poisson(log.th.new = log.th.curr,
                                                      log.th.other.new = log.th.other.curr,
                                                      log.th.old = log.th.prop,
                                                      log.th.other.old = log.th.other.prop,
                                                      scale = scale.theta,
                                                      weight = weight,
                                                      weight.other = weight.other)
                                  - safeLogProp_Poisson(log.th.new = log.th.prop,
                                                        log.th.other.new = log.th.other.prop,
                                                        log.th.old = log.th.curr,
                                                        log.th.other.old = log.th.other.curr,
                                                        scale = scale.theta,
                                                        weight = weight,
                                                        weight.other = weight.other))
                log.diff <- log.diff + log.diff.prior + log.diff.prop
            }
            else {
                ## Jacobian from prior density cancels with proposal density
                log.diff <- log.diff + (stats::dnorm(x = log.th.prop,
                                              mean = mu.i,
                                              sd = sigma,
                                              log = TRUE)
                                        - stats::dnorm(x = log.th.curr,
                                                mean = mu.i,
                                                sd = sigma,
                                                log = TRUE))
            }
            ## acceptance
            accept <- (log.diff >= 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.theta <- n.accept.theta + 1L
                theta[i] <- th.prop
                if (update.pair)
                    theta[i.other] <- th.other.prop
            }
        }
        object@theta <- theta
        object@mu <- mu
        object@nAcceptTheta@.Data <- n.accept.theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateTheta_PoissonVaryingUseExpAgCertain <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingUseExp"))
    stopifnot(methods::is(object, "Aggregate"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0L))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.double(exposure))
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(exposure[!is.na(exposure)] >= 0))
    ## y and exposure
    stopifnot(all(is.na(exposure) <= is.na(y)))
    if (useC) {
        .Call(updateTheta_PoissonVaryingUseExpAgCertain_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        scale.theta <- object@scaleTheta@.Data
        scale.theta.multiplier <- object@scaleThetaMultiplier
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma@.Data
        betas <- object@betas
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mu <- object@mu
        max.attempt <- object@maxAttempt
        iterator <- object@iteratorBetas
        n.theta <- length(theta)
        n.accept.theta <- 0L
        n.failed.prop.theta <- 0L
        mu <- makeMu(n = n.theta, betas = betas, iterator = iterator)
        scale.theta <- scale.theta * scale.theta.multiplier
        for (i in seq_along(theta)) {
            scale.theta.i <- scale.theta / sqrt(1 + log(1 + exposure[i]))
            ## determine type of update
            i.other <- makeIOther(i = i, transform = transform.ag, useC = TRUE)
            in.delta <- i.other >= 0L
            if (in.delta) {
                has.other <- i.other > 0L
                weight <- weight.ag[i]
                weight.positive <- weight > 0
                if (has.other) {
                    weight.other <- weight.ag[i.other]
                    weight.other.positive <- weight.other > 0
                }
            }
            value.fixed <- (in.delta
                            && weight.positive
                            && (!has.other || !weight.other.positive))
            if (value.fixed)
                next
            update.pair <- (in.delta
                            && weight.positive
                            && has.other
                            && weight.other.positive)
            ## generate proposal
            found.prop <- FALSE
            attempt <- 0L
            th.curr <- theta[i]
            log.th.curr <- log(th.curr)
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                log.th.prop <- stats::rnorm(n = 1L, mean = log.th.curr, sd = scale.theta.i)
                prop.in.range <- ((log.th.prop > (lower + tolerance))
                                 && (log.th.prop < (upper - tolerance)))
                if (!prop.in.range)
                    next
                th.prop <- exp(log.th.prop)
                if (update.pair) {
                    th.other.curr <- theta[i.other]
                    th.other.prop <- (th.curr - th.prop) * weight / weight.other + th.other.curr
                    is.positive <- th.other.prop > 0
                    if (is.positive) {
                        log.th.other.prop <- log(th.other.prop)
                        found.prop <- ((log.th.other.prop > (lower + tolerance))
                                       && (log.th.other.prop < (upper - tolerance)))
                    }
                    else
                        found.prop <- FALSE
                }
                else
                    found.prop <- TRUE
            }
            if (!found.prop) {  ## reached 'maxAttempt' without generating proposal
                n.failed.prop.theta <- n.failed.prop.theta + 1L
                next
            }
            log.diff <- 0
            ## calculate likelihoods (if used)
            y.i <- y[i]
            if (!is.na(y.i)) {
                exp.i <- exposure[i]
                log.diff <- log.diff + (stats::dpois(x = y.i,
                                              lambda = th.prop * exp.i,
                                              log = TRUE)
                                        - stats::dpois(x = y.i,
                                                lambda = th.curr * exp.i,
                                                log = TRUE))
            }
            if (update.pair) {
                y.other <- y[i.other]
                if (!is.na(y.other)) {
                    exp.other <- exposure[i.other]
                    log.diff <- log.diff + (stats::dpois(x = y.other,
                                                  lambda = th.other.prop * exp.other,
                                                  log = TRUE)
                                            - stats::dpois(x = y.other,
                                                    lambda = th.other.curr * exp.other,
                                                    log = TRUE))
                }
            }
            ## calculate prior and proposal densities
            mu.i <- mu[i]
            if (update.pair) {
                log.th.other.curr <- log(th.other.curr)
                mu.other <- mu[i.other]
                ## Use log-normal, because it include the Jacobians
                log.diff.prior <- (stats::dlnorm(x = th.prop,
                                          meanlog = mu.i,
                                          sdlog = sigma,
                                          log = TRUE)
                                   + stats::dlnorm(x = th.other.prop,
                                            meanlog = mu.other,
                                            sdlog = sigma,
                                            log = TRUE)
                                   - stats::dlnorm(x = th.curr,
                                            meanlog = mu.i,
                                            sdlog = sigma,
                                            log = TRUE)
                                   - stats::dlnorm(x = th.other.curr,
                                            meanlog = mu.other,
                                            sdlog = sigma,
                                            log = TRUE))
                log.diff.prop <- (safeLogProp_Poisson(log.th.new = log.th.curr,
                                                      log.th.other.new = log.th.other.curr,
                                                      log.th.old = log.th.prop,
                                                      log.th.other.old = log.th.other.prop,
                                                      scale = scale.theta.i,
                                                      weight = weight,
                                                      weight.other = weight.other)
                                  - safeLogProp_Poisson(log.th.new = log.th.prop,
                                                        log.th.other.new = log.th.other.prop,
                                                        log.th.old = log.th.curr,
                                                        log.th.other.old = log.th.other.curr,
                                                        scale = scale.theta.i,
                                                        weight = weight,
                                                        weight.other = weight.other))
                log.diff <- log.diff + log.diff.prior + log.diff.prop
            }
            else {
                ## Jacobian from prior density cancels with proposal density
                log.diff <- log.diff + (stats::dnorm(x = log.th.prop,
                                              mean = mu.i,
                                              sd = sigma,
                                              log = TRUE)
                                        - stats::dnorm(x = log.th.curr,
                                                mean = mu.i,
                                                sd = sigma,
                                                log = TRUE))
            }
            ## acceptance
            accept <- (log.diff >= 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.theta <- n.accept.theta + 1L
                theta[i] <- th.prop
                if (update.pair)
                    theta[i.other] <- th.other.prop
            }
        }
        object@theta <- theta
        object@mu <- mu
        object@nAcceptTheta@.Data <- n.accept.theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgNormal_PoissonNotUseExp <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingNotUseExp"))
    stopifnot(methods::is(object, "AgNormal"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    if (useC) {
        .Call(updateThetaAndValueAgNormal_PoissonNotUseExp_R, object, y)
    }
    else {
        theta <- object@theta
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        mu <- object@mu
        sigma <- object@sigma@.Data
        value.ag <- object@valueAg@.Data
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mean.ag <- object@meanAg@.Data
        sd.ag <- object@sdAg@.Data
        scale.ag <- object@scaleAg@.Data
        max.attempt <- object@maxAttempt
        n.failed.prop.value.ag <- 0L
        n.accept.ag <- 0L
        for (k in seq_along(value.ag)) {
            ## Each cell 'k' in the benchmarks has a set of associated 'i' in 'theta'.
            ## Construct vectors holding information on these 'theta'.  Use 'vec'
            ## prefix to distinguish the (length > 1) vectors.
            i.ag <- dembase::getIBefore(k, transform = transform.ag, useC = TRUE)
            n.ag <- length(i.ag)
            vec.th.curr <- theta[i.ag]
            vec.log.th.curr <- log(vec.th.curr)
            vec.th.prop <- numeric(length = n.ag)
            vec.log.th.prop <- numeric(length = n.ag)
            attempt <- 0L
            found.prop <- FALSE
            while (!found.prop && (attempt < max.attempt)) {
                ## attempt to generate a new set of 'theta', and hence a new
                ## value for aggregate value k by adding random increments to
                ## the 'theta' (on the log scale)
                attempt <- attempt + 1L
                for (i in seq_len(n.ag)) {
                    increment <- stats::rnorm(n = 1L, mean = 0, sd = scale.ag)
                    log.th.prop <- vec.log.th.curr[i] + increment
                    inside.limits <- ((log.th.prop > (lower + tolerance))
                                      && (log.th.prop < (upper - tolerance)))
                    if (!inside.limits)
                        break
                    th.prop <- exp(log.th.prop)
                    if (log.th.prop > 0)
                        valid <- is.finite(th.prop)
                    else
                        valid <- th.prop > 0
                    if (!valid)
                        break
                    vec.log.th.prop[i] <- log.th.prop
                    vec.th.prop[i] <- th.prop
                    found.prop <- i == n.ag
                }
            }
            if (!found.prop) { ## if found.prop is FALSE, reached 'maxAttempt'
                n.failed.prop.value.ag <- n.failed.prop.value.ag + 1L
                next
            }
            vec.y <- y[i.ag]
            is.observed <- !is.na(vec.y)
            vec.mu <- mu[i.ag]
            vec.weight <- weight.ag[i.ag]
            ag.curr <- value.ag[k]
            ag.prop <- sum(vec.th.prop * vec.weight)
            mean.k <- mean.ag[k]
            sd.k <- sd.ag[k]
            log.diff.lik <- (sum(stats::dpois(x = vec.y[is.observed],
                                       lambda = vec.th.prop[is.observed],
                                       log = TRUE))
                             - sum(stats::dpois(x = vec.y[is.observed],
                                         lambda = vec.th.curr[is.observed],
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
            }
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@nFailedPropValueAg@.Data <- n.failed.prop.value.ag
        object@nAcceptAg@.Data <- n.accept.ag
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgPoisson_PoissonNotUseExp <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingNotUseExp"))
    stopifnot(methods::is(object, "AgPoisson"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    if (useC) {
        .Call(updateThetaAndValueAgPoisson_PoissonNotUseExp_R, object, y)
    }
    else {
        theta <- object@theta
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        mu <- object@mu
        sigma <- object@sigma@.Data
        value.ag <- object@valueAg@.Data
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mean.ag <- object@meanAg@.Data
        scale.ag <- object@scaleAg@.Data
        exposure.ag <- object@exposureAg@.Data
        max.attempt <- object@maxAttempt
        n.failed.prop.value.ag <- 0L
        n.accept.ag <- 0L
        for (k in seq_along(value.ag)) {
            ## Each cell 'k' in the benchmarks has a set of associated 'i' in 'theta'.
            ## Construct vectors holding information on these 'theta'.  Use 'vec'
            ## prefix to distinguish the (length > 1) vectors.
            i.ag <- dembase::getIBefore(k, transform = transform.ag, useC = TRUE)
            n.ag <- length(i.ag)
            vec.th.curr <- theta[i.ag]
            vec.log.th.curr <- log(vec.th.curr)
            vec.th.prop <- numeric(length = n.ag)
            vec.log.th.prop <- numeric(length = n.ag)
            attempt <- 0L
            found.prop <- FALSE
            while (!found.prop && (attempt < max.attempt)) {
                ## attempt to generate a new set of 'theta', and hence a new
                ## value for aggregate value k by adding random increments to
                ## the 'theta' (on the log scale)
                attempt <- attempt + 1L
                for (i in seq_len(n.ag)) {
                    increment <- stats::rnorm(n = 1L, mean = 0, sd = scale.ag)
                    log.th.prop <- vec.log.th.curr[i] + increment
                    inside.limits <- ((log.th.prop > (lower + tolerance))
                                      && (log.th.prop < (upper - tolerance)))
                    if (!inside.limits)
                        break
                    th.prop <- exp(log.th.prop)
                    if (log.th.prop > 0)
                        valid <- is.finite(th.prop)
                    else
                        valid <- th.prop > 0
                    if (!valid)
                        break
                    vec.log.th.prop[i] <- log.th.prop
                    vec.th.prop[i] <- th.prop
                    found.prop <- i == n.ag
                }
            }
            if (!found.prop) { ## if found.prop is FALSE, reached 'maxAttempt'
                n.failed.prop.value.ag <- n.failed.prop.value.ag + 1L
                next
            }
            vec.y <- y[i.ag]
            is.observed <- !is.na(vec.y)
            vec.mu <- mu[i.ag]
            vec.weight <- weight.ag[i.ag]
            ag.curr <- value.ag[k]
            ag.prop <- sum(vec.th.prop * vec.weight)
            mean.k <- mean.ag[k]
            exposure.k <- exposure.ag[k]
            log.diff.lik <- (sum(stats::dpois(x = vec.y[is.observed],
                                       lambda = vec.th.prop[is.observed],
                                       log = TRUE))
                             - sum(stats::dpois(x = vec.y[is.observed],
                                         lambda = vec.th.curr[is.observed],
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
            ## allow for mean.k * exposure.k to be non-integer
            log.diff.ag <- (exposure.k * (ag.curr - ag.prop)
                            + mean.k * exposure.k * (log(ag.prop) - log(ag.curr)))
            log.diff <- log.diff.lik + log.diff.prior + log.diff.ag
            accept <- (log.diff > 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.ag <- n.accept.ag + 1L
                value.ag[k] <- ag.prop
                theta[i.ag] <- vec.th.prop
            }
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@nFailedPropValueAg@.Data <- n.failed.prop.value.ag
        object@nAcceptAg@.Data <- n.accept.ag
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgFun_PoissonNotUseExp <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingNotUseExp"))
    stopifnot(methods::is(object, "AgFun"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    if (useC) {
        .Call(updateThetaAndValueAgFun_PoissonNotUseExp_R, object, y)
    }
    else {
        theta <- object@theta
        scale <- object@scaleTheta
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma@.Data
        betas <- object@betas
        iterator <- object@iteratorBetas
        value.ag <- object@valueAg@.Data
        mean.ag <- object@meanAg@.Data
        sd.ag <- object@sdAg@.Data
        transform.ag <- object@transformAg
        fun.ag <- object@funAg
        x.args.ag <- object@xArgsAg # list of "Values" objects
        weights.args.ag <- object@weightsArgsAg # list of "Counts" objects
        max.attempt <- object@maxAttempt
        n.failed.prop.theta <- 0L
        n.accept.theta <- 0L
        iterator <- resetB(iterator)
        n.shared <- length(x.args.ag[[1L]]@.Data)
        for (i in seq_along(theta)) {
            indices <- iterator@indices
            mu <- 0
            for (b in seq_along(betas))
                mu <- mu + betas[[b]][indices[b]]
            i.ag <- getIAfter(i = i,
                              transform = transform.ag,
                              check = FALSE,
                              useC = TRUE)
            contributes.to.ag <- i.ag > 0L
            y.is.missing <- is.na(y[i])
            th.curr <- theta[i]
            log.th.curr <- log(th.curr)
            if (y.is.missing) {
                mean <- mu
                sd <- sigma
            }
            else {
                mean <- log.th.curr
                sd <- scale
            }
            found.prop <- FALSE
            attempt <- 0L
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                log.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                found.prop <- ((log.th.prop > lower + tolerance)
                               && (log.th.prop < upper - tolerance))
            }
            if (found.prop) {
                th.prop <- exp(log.th.prop)
                draw.straight.from.prior <- y.is.missing && !contributes.to.ag
                if (draw.straight.from.prior)
                    theta[i] <- th.prop
                else {
                    if (y.is.missing)
                        log.diff <- 0
                    else {
                        log.lik.prop <- stats::dpois(y[i], lambda = th.prop, log = TRUE)
                        log.lik.curr <- stats::dpois(y[i], lambda = th.curr, log = TRUE)
                        log.diff <- log.lik.prop - log.lik.curr
                    }
                    log.dens.prop <- stats::dnorm(x = log.th.prop, mean = mu, sd = sigma, log = TRUE)
                    log.dens.curr <- stats::dnorm(x = log.th.curr, mean = mu, sd = sigma, log = TRUE)
                    log.diff <- log.diff + log.dens.prop - log.dens.curr
                    if (contributes.to.ag) {
                        ag.curr <- value.ag[i.ag]
                        mean <- mean.ag[i.ag]
                        sd <- sd.ag[i.ag]
                        weights <- weights.args.ag[[i.ag]]
                        x <- x.args.ag[[i.ag]]
                        i.shared <- dembase::getIShared(i = i,
                                                        transform = transform.ag,
                                                        useC = TRUE)
                        x@.Data[i.shared == i] <- th.prop
                        ag.prop <- fun.ag(x = x, weights = weights)
                        log.dens.ag.prop <- stats::dnorm(x = mean, mean = ag.prop, sd = sd, log = TRUE)
                        log.dens.ag.curr <- stats::dnorm(x = mean, mean = ag.curr, sd = sd, log = TRUE)
                        log.diff <- log.diff + log.dens.ag.prop - log.dens.ag.curr
                    }
                    accept <- (log.diff >= 0) || (stats::runif(n = 1L) < exp(log.diff))
                    if (accept) {
                        n.accept.theta <- n.accept.theta + 1L
                        theta[i] <- th.prop
                        if (contributes.to.ag) {
                            x.args.ag[[i.ag]] <- x
                            value.ag[i.ag] <- ag.prop
                        }
                    }
                }
            }
            else
                n.failed.prop.theta <- n.failed.prop.theta + 1L
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@xArgsAg <- x.args.ag
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgNormal_PoissonUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingUseExp"))
    stopifnot(methods::is(object, "AgNormal"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.double(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    if (useC) {
        .Call(updateThetaAndValueAgNormal_PoissonUseExp_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        mu <- object@mu
        sigma <- object@sigma@.Data
        value.ag <- object@valueAg@.Data
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mean.ag <- object@meanAg@.Data
        sd.ag <- object@sdAg@.Data
        scale.ag <- object@scaleAg@.Data
        max.attempt <- object@maxAttempt
        n.failed.prop.value.ag <- 0L
        n.accept.ag <- 0L
        for (k in seq_along(value.ag)) {
            ## Each cell 'k' in the benchmarks has a set of associated 'i' in 'theta'.
            ## Construct vectors holding information on these 'theta'.  Use 'vec'
            ## prefix to distinguish the (length > 1) vectors.
            i.ag <- dembase::getIBefore(k, transform = transform.ag, useC = TRUE)
            n.ag <- length(i.ag)
            vec.th.curr <- theta[i.ag]
            vec.log.th.curr <- log(vec.th.curr)
            vec.th.prop <- numeric(length = n.ag)
            vec.log.th.prop <- numeric(length = n.ag)
            attempt <- 0L
            found.prop <- FALSE
            while (!found.prop && (attempt < max.attempt)) {
                ## attempt to generate a new set of 'theta', and hence a new
                ## value for benchmark[k] by adding random increments to
                ## the 'theta' (on the log scale)
                attempt <- attempt + 1L
                for (i in seq_len(n.ag)) {
                    increment <- stats::rnorm(n = 1L, mean = 0, sd = scale.ag)
                    log.th.prop <- vec.log.th.curr[i] + increment
                    inside.limits <- ((log.th.prop > (lower + tolerance))
                                      && (log.th.prop < (upper - tolerance)))
                    if (!inside.limits)
                        break
                    th.prop <- exp(log.th.prop)
                    if (log.th.prop > 0)
                        valid <- is.finite(th.prop)
                    else
                        valid <- th.prop > 0
                    if (!valid)
                        break
                    vec.log.th.prop[i] <- log.th.prop
                    vec.th.prop[i] <- th.prop
                    found.prop <- i == n.ag
                }
            }
            if (!found.prop) { ## if found.prop is FALSE, reached 'maxAttempt'
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
            }
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@nFailedPropValueAg@.Data <- n.failed.prop.value.ag
        object@nAcceptAg@.Data <- n.accept.ag
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgPoisson_PoissonUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingUseExp"))
    stopifnot(methods::is(object, "AgPoisson"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.double(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    if (useC) {
        .Call(updateThetaAndValueAgPoisson_PoissonUseExp_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        mu <- object@mu
        sigma <- object@sigma@.Data
        value.ag <- object@valueAg@.Data
        weight.ag <- object@weightAg
        transform.ag <- object@transformAg
        mean.ag <- object@meanAg@.Data
        scale.ag <- object@scaleAg@.Data
        exposure.ag <- object@exposureAg@.Data
        max.attempt <- object@maxAttempt
        n.failed.prop.value.ag <- 0L
        n.accept.ag <- 0L
        for (k in seq_along(value.ag)) {
            ## Each cell 'k' in the benchmarks has a set of associated 'i' in 'theta'.
            ## Construct vectors holding information on these 'theta'.  Use 'vec'
            ## prefix to distinguish the (length > 1) vectors.
            i.ag <- dembase::getIBefore(k, transform = transform.ag, useC = TRUE)
            n.ag <- length(i.ag)
            vec.th.curr <- theta[i.ag]
            vec.log.th.curr <- log(vec.th.curr)
            vec.th.prop <- numeric(length = n.ag)
            vec.log.th.prop <- numeric(length = n.ag)
            attempt <- 0L
            found.prop <- FALSE
            while (!found.prop && (attempt < max.attempt)) {
                ## attempt to generate a new set of 'theta', and hence a new
                ## value for benchmark[k] by adding random increments to
                ## the 'theta' (on the log scale)
                attempt <- attempt + 1L
                for (i in seq_len(n.ag)) {
                    increment <- stats::rnorm(n = 1L, mean = 0, sd = scale.ag)
                    log.th.prop <- vec.log.th.curr[i] + increment
                    inside.limits <- ((log.th.prop > (lower + tolerance))
                                      && (log.th.prop < (upper - tolerance)))
                    if (!inside.limits)
                        break
                    th.prop <- exp(log.th.prop)
                    if (log.th.prop > 0)
                        valid <- is.finite(th.prop)
                    else
                        valid <- th.prop > 0
                    if (!valid)
                        break
                    vec.log.th.prop[i] <- log.th.prop
                    vec.th.prop[i] <- th.prop
                    found.prop <- i == n.ag
                }
            }
            if (!found.prop) { ## if found.prop is FALSE, reached 'maxAttempt'
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
            exposure.k <- exposure.ag[k]
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
            ## allow for mean.k * exposure.k to be non-integer
            log.diff.ag <- (exposure.k * (ag.curr - ag.prop)
                            + mean.k * exposure.k * (log(ag.prop) - log(ag.curr)))
            log.diff <- log.diff.lik + log.diff.prior + log.diff.ag
            accept <- (log.diff > 0) || (stats::runif(1) < exp(log.diff))
            if (accept) {
                n.accept.ag <- n.accept.ag + 1L
                value.ag[k] <- ag.prop
                theta[i.ag] <- vec.th.prop
            }
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@nFailedPropValueAg@.Data <- n.failed.prop.value.ag
        object@nAcceptAg@.Data <- n.accept.ag
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateThetaAndValueAgFun_PoissonUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingUseExp"))
    stopifnot(methods::is(object, "AgFun"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(all(y[!is.na(y)] >= 0))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.double(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    if (useC) {
        .Call(updateThetaAndValueAgFun_PoissonUseExp_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        scale <- object@scaleTheta
        scale.multiplier <- object@scaleThetaMultiplier
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma@.Data
        betas <- object@betas
        iterator <- object@iteratorBetas
        value.ag <- object@valueAg@.Data
        mean.ag <- object@meanAg@.Data
        sd.ag <- object@sdAg@.Data
        transform.ag <- object@transformAg
        fun.ag <- object@funAg
        x.args.ag <- object@xArgsAg # list of "Values" objects
        weights.args.ag <- object@weightsArgsAg # list of "Counts" objects
        max.attempt <- object@maxAttempt
        n.failed.prop.theta <- 0L
        n.accept.theta <- 0L
        iterator <- resetB(iterator)
        scale <- scale * scale.multiplier
        n.shared <- length(x.args.ag[[1L]]@.Data)
        for (i in seq_along(theta)) {
            indices <- iterator@indices
            mu <- 0
            for (b in seq_along(betas))
                mu <- mu + betas[[b]][indices[b]]
            i.ag <- getIAfter(i = i,
                              transform = transform.ag,
                              check = FALSE,
                              useC = TRUE)
            contributes.to.ag <- i.ag > 0L
            y.is.missing <- is.na(y[i])
            th.curr <- theta[i]
            log.th.curr <- log(th.curr)
            if (y.is.missing) {
                mean <- mu
                sd <- sigma
            }
            else {
                mean <- log.th.curr
                sd <- scale / sqrt(1 + log(1 + exposure[i]))
            }
            found.prop <- FALSE
            attempt <- 0L
            while (!found.prop && (attempt < max.attempt)) {
                attempt <- attempt + 1L
                log.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                found.prop <- ((log.th.prop > lower + tolerance)
                               && (log.th.prop < upper - tolerance))
            }
            if (found.prop) {
                th.prop <- exp(log.th.prop)
                draw.straight.from.prior <- y.is.missing && !contributes.to.ag
                if (draw.straight.from.prior)
                    theta[i] <- th.prop
                else {
                    if (y.is.missing)
                        log.diff <- 0
                    else {
                        log.lik.prop <- stats::dpois(y[i], lambda = th.prop * exposure[i], log = TRUE)
                        log.lik.curr <- stats::dpois(y[i], lambda = th.curr * exposure[i], log = TRUE)
                        log.diff <- log.lik.prop - log.lik.curr
                    }
                    log.dens.prop <- stats::dnorm(x = log.th.prop, mean = mu, sd = sigma, log = TRUE)
                    log.dens.curr <- stats::dnorm(x = log.th.curr, mean = mu, sd = sigma, log = TRUE)
                    log.diff <- log.diff + log.dens.prop - log.dens.curr
                    if (contributes.to.ag) {
                        ag.curr <- value.ag[i.ag]
                        mean <- mean.ag[i.ag]
                        sd <- sd.ag[i.ag]
                        weights <- weights.args.ag[[i.ag]]
                        x <- x.args.ag[[i.ag]]
                        i.shared <- dembase::getIShared(i = i,
                                                        transform = transform.ag,
                                                        useC = TRUE)
                        x@.Data[i.shared == i] <- th.prop
                        ag.prop <- fun.ag(x = x, weights = weights)
                        log.dens.ag.prop <- stats::dnorm(x = mean, mean = ag.prop, sd = sd, log = TRUE)
                        log.dens.ag.curr <- stats::dnorm(x = mean, mean = ag.curr, sd = sd, log = TRUE)
                        log.diff <- log.diff + log.dens.ag.prop - log.dens.ag.curr
                    }
                    accept <- (log.diff >= 0) || (stats::runif(n = 1L) < exp(log.diff))
                    if (accept) {
                        n.accept.theta <- n.accept.theta + 1L
                        theta[i] <- th.prop
                        if (contributes.to.ag) {
                            x.args.ag[[i.ag]] <- x
                            value.ag[i.ag] <- ag.prop
                        }
                    }
                }
            }
            else
                n.failed.prop.theta <- n.failed.prop.theta + 1L
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@valueAg@.Data <- value.ag
        object@xArgsAg <- x.args.ag
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

## TRANSLATED
## HAS_TESTS
updateVarsigma <- function(object, y, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "VarsigmaUnknown"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "DemographicArray"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.double(y))
    stopifnot(sum(!is.na(y)) >= 2L)
    if (useC) {
        .Call(updateVarsigma_R, object, y)
    }
    else {
        varsigma <- object@varsigma@.Data
        varsigma.max <- object@varsigmaMax@.Data
        A <- object@AVarsigma@.Data
        nu <- object@nuVarsigma@.Data
        theta <- object@theta
        w <- object@w
        n <- length(theta)
        V <- 0
        n.obs <- 0L
        for (i in seq_len(n)) {
            is.observed <- !is.na(y[i])
            if (is.observed) {
                V <- V + w[i] * (y[i] - theta[i])^2
                n.obs <- n.obs + 1L
            }
        }
        varsigma <- updateSDNorm(sigma = varsigma,
                                 A = A,
                                 nu = nu,
                                 V = V,
                                 n = n.obs,
                                 max = varsigma.max)
        successfully.updated <- varsigma > 0
        if (successfully.updated)
            object@varsigma@.Data <- varsigma
        object
    }
}



## UPDATING COUNTS ####################################################################

## TRANSLATED
## HAS_TESTS
updateCountsPoissonNotUseExp <- function(y, model, observation, datasets,
                                         transforms, useC = FALSE) {
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(!any(is.na(y)))
    stopifnot(all(y >= 0))
    stopifnot(all(round(y) == y))
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "Poisson"))
    stopifnot(methods::is(model, "NotUseExposure"))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, methods::is, "Model")))
    stopifnot(all(sapply(observation, methods::is, "UseExposure")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    stopifnot(all(sapply(datasets, is.integer)))
    stopifnot(all(sapply(datasets, function(x) all(x[!is.na(x)] >= 0))))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## y and transforms
    for (i in seq_along(transforms))
        stopifnot(identical(dim(y), transforms[[i]]@dimBefore))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    ## datasets and transforms
    for (i in seq_along(datasets))
        stopifnot(identical(transforms[[i]]@dimAfter, dim(datasets[[i]])))
    if (useC) {
        .Call(updateCountsPoissonNotUseExp_R, y, model,
              observation, datasets, transforms)
    }
    else {
        ##y, model, observation, datasets, transforms
        theta <- model@theta
        has.subtotals <- methods::is(y, "HasSubtotals")
        if (has.subtotals) {
            transform.subtotals <- y@transformSubtotals
        }
        for (i in seq_along(y)) {
            if (has.subtotals) {
                i.other <- makeIOther(i = i, transform = transform.subtotals)
                if (i.other > 0L) { ## found other cell with same subtotal
                    i <- c(i, i.other)
                    sum.y <- sum(y[i])
                    y.prop <- as.integer(stats::rmultinom(n = 1L, size = sum.y, prob = theta[i]))
                }
                else if (i.other == 0L) { ## subtotal refers to single cell
                    next
                }
                else { ## cell not included in any subtotal
                    ## as.integer needed for R < 3.0
                    y.prop <- as.integer(stats::rpois(n = 1L, lambda = theta[i]))
                }
            }
            else {
                ## as.integer needed for R < 3.0
                y.prop <- as.integer(stats::rpois(n = 1L, lambda = theta[i]))
            }
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = y,
                                       indicesY = i,
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            accept <- (diff.log.lik >= 0) || (stats::runif(n = 1L) < exp(diff.log.lik))
            if (accept)
                y[i] <- y.prop
        }
        y
    }
}

## TRANSLATED
## HAS_TESTS
updateCountsPoissonUseExp <- function(y, model, exposure, observation, datasets,
                                      transforms, useC = FALSE) {
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(!any(is.na(y)))
    stopifnot(all(y >= 0))
    stopifnot(all(round(y) == y))
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "Poisson"))
    stopifnot(methods::is(model, "UseExposure"))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(!any(is.na(exposure)))
    stopifnot(is.double(exposure))
    stopifnot(all(exposure >= 0))
    stopifnot(all(y[exposure == 0] == 0))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, methods::is, "Model")))
    stopifnot(all(sapply(observation, methods::is, "UseExposure")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    stopifnot(all(sapply(datasets, is.integer)))
    stopifnot(all(sapply(datasets, function(x) all(x[!is.na(x)] >= 0))))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## y and transforms
    for (i in seq_along(transforms))
        stopifnot(identical(dim(y), transforms[[i]]@dimBefore))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    ## datasets and transforms
    for (i in seq_along(datasets))
        stopifnot(identical(transforms[[i]]@dimAfter, dim(datasets[[i]])))
    if (useC) {
        .Call(updateCountsPoissonUseExp_R, y, model, exposure,
              observation, datasets, transforms)
    }
    else {
        theta <- model@theta
        has.subtotals <- methods::is(y, "HasSubtotals")
        if (has.subtotals)
            transform.subtotals <- y@transformSubtotals
        for (i in seq_along(y)) {
            if (has.subtotals) {
                i.other <- makeIOther(i = i, transform = transform.subtotals)
                if (i.other > 0L) { ## other cell found
                    i <- c(i, i.other)
                    sum.y <- sum(y[i])
                    ## as.integer needed for R < 3.0
                    y.prop <- as.integer(stats::rmultinom(n = 1L, size = sum.y, prob = theta[i] * exposure[i]))
                }
                else if (i.other == 0L) ## subtotal refers to single cell
                    next
                else ## not included in subtotal; as.integer needed for R < 3.0
                    y.prop <- as.integer(stats::rpois(n = 1L, lambda = theta[i] * exposure[i]))
            }
            else ## as.integer needed for R < 3.0
                y.prop <- as.integer(stats::rpois(n = 1L, lambda = theta[i] * exposure[i]))
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = y,
                                       indicesY = i,
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            accept <- (diff.log.lik >= 0) || (stats::runif(n = 1L) < exp(diff.log.lik))
            if (accept)
                y[i] <- y.prop
        }
        y
    }
}

## TRANSLATED
## HAS_TESTS
updateCountsBinomial <- function(y, model, exposure, observation, datasets,
                                 transforms, useC = FALSE) {
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(!methods::is(y, "HasSubtotals"))
    stopifnot(is.integer(y))
    stopifnot(!any(is.na(y)))
    stopifnot(all(y >= 0))
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "Binomial"))
    ## exposure
    stopifnot(methods::is(model, "UseExposure"))
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(!any(is.na(exposure)))
    stopifnot(is.integer(exposure))
    stopifnot(all(exposure >= 0))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, methods::is, "Model")))
    stopifnot(all(sapply(observation, methods::is, "UseExposure")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    stopifnot(all(sapply(datasets, is.integer)))
    stopifnot(all(sapply(datasets, function(x) all(x[!is.na(x)] >= 0))))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## y and exposure
    stopifnot(all(y <= exposure))
    ## y and transforms
    for (i in seq_along(transforms))
        stopifnot(identical(dim(y), transforms[[i]]@dimBefore))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    ## datasets and transforms
    for (i in seq_along(datasets))
        stopifnot(identical(transforms[[i]]@dimAfter, dim(datasets[[i]])))
    if (useC) {
        .Call(updateCountsBinomial_R, y, model, exposure,
              observation, datasets, transforms)
    }
    else {
        theta <- model@theta
        for (i in seq_along(y)) {
            y.prop <- stats::rbinom(n = 1L, size = exposure[i], prob = theta[i])
            y.prop <- as.integer(y.prop)  # needed for R < 3.0
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = y,
                                       indicesY = i,
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            accept <- (diff.log.lik >= 0) || (stats::runif(n = 1L) < exp(diff.log.lik))
            if (accept)
                y[i] <- y.prop
        }
        y
    }
    
}


## TRANSLATED
## HAS_TESTS
updateObservationCounts <- function(y, observation, datasets,
                                    transforms, useC = FALSE) {
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(all(y >= 0))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, methods::is, "Model")))
    stopifnot(all(sapply(observation, methods::is, "UseExposure")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    stopifnot(all(sapply(datasets, is.integer)))
    stopifnot(all(sapply(datasets, function(x) all(x[!is.na(x)] >= 0))))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## y and transforms
    for (i in seq_along(transforms))
        stopifnot(identical(dim(y), transforms[[i]]@dimBefore))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    ## datasets and transforms
    stopifnot(identical(transforms[[i]]@dimAfter, dim(datasets[[i]])))
    if (useC) {
        .Call(updateObservationCounts_R, y, observation, datasets,
              transforms)
    }
    else {
        for (i in seq_along(observation)) {
            model <- observation[[i]]
            dataset <- datasets[[i]]
            transform <- transforms[[i]]
            y.collapsed <- dembase::collapse(y, transform = transform)
            if (methods::is(model, "Poisson"))
                y.collapsed <- dembase::toDouble(y.collapsed)
            observation[[i]] <- updateModelUseExp(model,
                                                  y = dataset,
                                                  exposure = y.collapsed)
        }
        observation
    }
}

