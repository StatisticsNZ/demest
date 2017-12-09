



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

    


## HAS_TESTS
initialModelPredictHelper <- function(model, along, labels, n, offsetModel,
                                      covariates) {
    theta.old <- model@theta
    metadata.first <- model@metadataY
    betas <- model@betas
    priors.betas <- model@priorsBetas
    names.betas <- model@namesBetas
    margins <- model@margins
    dims <- model@dims
    i.method.model.first <- model@iMethodModel
    struc.zero.array.first <- model@strucZeroArray
    n.beta <- length(betas)
    metadata.pred <- makeMetadataPredict(metadata = metadata.first,
                                         along = along,
                                         labels = labels,
                                         n = n)
    struc.zero.array.pred <- makeStrucZeroArrayPredict()
    
    theta <- rep(mean(theta.old), times = prod(dim(metadata.pred)))
    cell.in.lik <- rep(FALSE, times = prod(dim(metadata.pred)))
    beta.is.predicted <- logical(length = n.beta)
    for (i in seq_len(n.beta)) {
        margin <- margins[[i]]
        prior <- priors.betas[[i]]
        beta.is.predicted[i] <- along %in% margin
        if (beta.is.predicted[i]) {
            metadata.pred.i <- metadata.pred[margin]
            dim.i <- dim(metadata.pred.i)
            J <- prod(dim.i)
            dims[[i]] <- dim.i
            betas[[i]] <- rep(0, length = J)
            covariates.i <- covariates[[names.betas[i]]]
            along.margin <- match(along, margin)
            priors.betas[[i]] <- initialPriorPredict(prior = prior,
                                                     data = covariates.i,
                                                     metadata = metadata.pred.i,
                                                     name = names.betas[i],
                                                     along = along.margin)            
        }
        else {
            J <- length(betas[[i]])
            J <- methods::new("Length", J)
            isSaturated <- methods::new("LogicalFlag", FALSE)
            priors.betas[[i]] <- methods::new("TimeInvariant",
                                              J = J,
                                              isSaturated = isSaturated)
        }
    }
    names.predicted <- names.betas[beta.is.predicted]
    names.covariates <- names(covariates)
    for (name in names.covariates) {
        if (!(name %in% names.betas))
            stop(gettextf("'%s' includes data for '%s', but '%s' is not a term in the model",
                          "covariates", name, name))
        if (!name %in% names.predicted)
            stop(gettextf("'%s' includes data for '%s', but '%s' is not predicted",
                          "covariates", name, name))
    }
    dim <- dim(metadata.pred)
    iterator.betas <- BetaIterator(dim = dim, margins = margins)
    offsets.betas <- makeOffsetsBetas(model, offsetModel = offsetModel)
    offsets.priors.betas <- makeOffsetsPriorsBetas(model, offsetModel = offsetModel)
    offsets.sigma <- makeOffsetsSigma(model, offsetModel = offsetModel)
    i.method.model <- i.method.model.first + 100L
    list(theta = theta,
         metadataY = metadata.pred,
         cellInLik = cell.in.lik,
         betas = betas,
         priorsBetas = priors.betas,
         iteratorBetas = iterator.betas,
         dims = dims,
         betaIsPredicted = beta.is.predicted,
         offsetsBetas = offsets.betas,
         offsetsPriorsBetas = offsets.priors.betas,
         offsetsSigma = offsets.sigma,
         iMethodModel = i.method.model)         
}    




## TRANSLATED
## HAS_TESTS (INCLUDING FOR MIX)
## ADD TESTS FOR ICAR AND Cross WHEN CLASSES FINISHED
betaHat <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior") || methods::is(prior, "FakePrior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    if (useC) {
        .Call(betaHat_R, prior)
    }
    else {
        J <- prior@J@.Data
        ## has.alpha.cross <- prior@hasAlphaMove@.Data
        has.alpha.dlm <- prior@hasAlphaDLM@.Data
        has.alpha.icar <- prior@hasAlphaICAR@.Data
        has.alpha.mix <- prior@hasAlphaMix@.Data
        has.covariates <- prior@hasCovariates@.Data
        has.season <- prior@hasSeason@.Data
        all.struc.zero <- prior@allStrucZero ## NEW
        ans <- rep(0, times = J)
        ## if (has.alpha.cross) {
        ##     alpha.cross <- prior@alphaCross@.Data
        ##     indices.cross <- prior@indicesCross
        ##     for (j in seq_len(J)) {
        ##         index.cross <- indices.cross[j]
        ##         if (is.infinite(index.cross))
        ##             ans[j] <- if (index.cross < 0) -Inf else Inf
        ##         else
        ##             ans[j] <- ans[j] + alpha.cross[index.cross]
        ##     }
        ## }
        if (has.alpha.dlm) {
            alpha.dlm <- prior@alphaDLM@.Data
            K <- prior@K@.Data
            L <- prior@L@.Data
            iterator.alpha <- prior@iteratorState
            iterator.v <- prior@iteratorV
            iterator.alpha <- resetA(iterator.alpha)
            iterator.v <- resetA(iterator.v)
            for (l in seq_len(L)) {
                if (!is.struc.zero[i]) { ## NEW
                    indices.alpha <- iterator.alpha@indices
                    indices.v <- iterator.v@indices
                    for (k in seq_len(K)) {
                        i.alpha <- indices.alpha[k + 1L]
                        i.ans <- indices.v[k]
                        ans[i.ans] <- ans[i.ans] + alpha.dlm[i.alpha]
                    }
                } ## NEW
                iterator.alpha <- advanceA(iterator.alpha)
                iterator.v <- advanceA(iterator.v)
            }
        }
        if (has.alpha.icar) {
            alpha.icar <- prior@alphaICAR@.Data
            ans <- ans + alpha.icar
        }
        if (has.alpha.mix) { 
            alpha.mix <- prior@alphaMix@.Data 
            ans <- ans + alpha.mix 
        } 
        if (has.covariates) {
            Z <- unname(prior@Z)
            eta <- prior@eta@.Data
            ans <- ans + drop(Z %*% eta)
        }
        if (has.season) {
            s <- prior@s@.Data
            K <- prior@K@.Data
            L <- prior@L@.Data
            iterator.s <- prior@iteratorState
            iterator.v <- prior@iteratorV
            iterator.s <- resetA(iterator.s)
            iterator.v <- resetA(iterator.v)
            for (l in seq_len(L)) {
                indices.s <- iterator.s@indices
                indices.v <- iterator.v@indices
                for (k in seq_len(K)) {
                    i.s <- indices.s[k + 1L]
                    i.ans <- indices.v[k]
                    ans[i.ans] <- ans[i.ans] + s[[i.s]][1L]
                }
                ## changed these 2 lines - JAH 18/4/2016
                iterator.s <- advanceA(iterator.s)
                iterator.v <- advanceA(iterator.v)
            }
        }
        ans
    }
}


setMethod("updateBetaAndPriorBeta",
          signature(prior = "ExchFixed"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_ExchFixed_R, prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
              }
              else {
                  J <- prior@J@.Data
                  is.saturated <- prior@isSaturated@.Data
                  if (is.saturated)
                      beta <- rep(0, times = J)
                  else {
                      tau <- prior@tau@.Data
                      all.struc.zero <- prior@allStrucZero
                      J <- prior@J
                      prec.prior <- 1 / tau^2
                      for (i in seq_len(J)) {
                          if (!is.struc.zero[i]) {
                              prec.data <- n[i] / sigma^2
                              var <- 1 / (prec.data + prec.prior) 
                              mean <- prec.data * vbar * var
                              sd <- sqrt(var)
                              beta[i] <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                          }
                      }
                  }
                  list(beta, prior)
              }
          })


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
        is.struct.zero <- prior@isStrucZero
        v <- getV(prior)
        beta.hat <- betaHat(prior)
        ans <- numeric(length = J)
        for (i in seq_len(J)) {
            if (is.struct.zero)
                ans[i] <- 0
            else {
                prec.data <- n[i] / sigma^2 ## now a vector
                prec.prior <- 1 / v[i]
                var <- 1 / (prec.data + prec.prior)
                mean <- (prec.data * vbar[i] + prec.prior * beta.hat[i]) * var
                sd <- sqrt(var)
                ans[i] <- stats::rnorm(n = 1L, mean = mean, sd = sd)
            }
        }
        ans
    }
}


updateTauNorm <- function(prior, beta, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "NormMixin"))
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
        all.struc.zero <- prior@allStrucZero
        beta.hat <- betaHat(prior)
        V <- 0
        n <- 0
        for (i in seq_len(J)) {
            if (!is.struct.zero[i]) {
                n <- n + 1L
                V <- V + (beta - beta.hat[i])^2
            }
        }
        tau <- updateSDNorm(sigma = tau,
                            A = A,
                            nu = nu,
                            V = V,
                            n = n,
                            max = tauMax)
        successfully.updated <- tau > 0
        if (successfully.updated)
            prior@tau@.Data <- tau
        prior
    }
}


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
        all.struc.zero <- prior@allStrucZero
        V <- 0
        n <- 0L
        for (i in seq_len(J)) {
            if (!is.struct.zero[i]) {
                V <- V + (1 / UBeta[i])
                n <- n + 1L
            }
        }
        tau <- updateSDRobust(sigma = tau,
                              A = A,
                              nuBeta = nuBeta,
                              nuTau = nuTau,
                              V = V,
                              n = n,
                              max = tauMax)
        successfully.updated <- tau > 0
        if (successfully.updated)
            prior@tau@.Data <- tau
        prior
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



## TRANSLATED (AGAIN 16/7/2017)
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
        has.level <- prior@hasLevel@.Data 
        phi <- prior@phi ## scalar
        omega.alpha <- prior@omegaAlpha@.Data ## scalar
        omega.delta <- prior@omegaDelta@.Data ## scalar
        is.struc.zero <- object@isStrucZero ## NEW
        v <- getV(prior) ## numeric length KL
        iterator.ad <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.ad <- resetA(iterator.ad)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
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
                    if (!has.level) {
                        if ((i == 0L) && is.infinite(DC.inv[[1L]][1L])) {
                            delta[indices.ad[1L]] <- alpha[indices.ad[2L]] - alpha[indices.ad[1L]]
                        }
                        else  {
                            C.inv <- UC[[i + 1L]] %*% DC.inv[[i + 1L]] %*% DC.inv[[i + 1L]] %*% t(UC[[i + 1L]])
                            sigma.inv.1 <- C.inv[1L]
                            sigma.inv.2 <- C.inv[2L]
                            sigma.inv.3 <- C.inv[3L]
                            sigma.inv.4 <- C.inv[4L] + phi^2 / omega.delta^2
                            determinant <- sigma.inv.1 * sigma.inv.4 - sigma.inv.2 * sigma.inv.3
                            sigma.1 <- sigma.inv.4 / determinant
                            sigma.2 <- -1 * sigma.inv.3 / determinant
                            sigma.3 <- -1 * sigma.inv.2 / determinant
                            sigma.4 <- sigma.inv.1 / determinant
                            mu.inner.1 <- C.inv[1L] * m[[i + 1L]][1L] + C.inv[3L] * m[[i + 1L]][2L]
                            mu.inner.2 <- (C.inv[2L] * m[[i + 1L]][1L] + C.inv[4L] * m[[i + 1L]][2L]
                                + phi * delta[indices.ad[i + 2L]] / omega.delta^2)
                            mu.1 <- sigma.1 * mu.inner.1 + sigma.3 * mu.inner.2
                            mu.2 <- sigma.2 * mu.inner.1 + sigma.4 * mu.inner.2
                            mu.star.1 <- mu.1
                            mu.star.2 <- mu.1 + mu.2
                            sigma.star.1 <- sigma.1
                            sigma.star.2 <- sigma.1 + sigma.2
                            sigma.star.3 <- sigma.1 + sigma.3
                            sigma.star.4 <- sigma.1 + sigma.2 + sigma.3 + sigma.4
                            rho.star.sq <- sigma.star.2 * sigma.star.3 / (sigma.star.1 * sigma.star.4)
                            mean.alpha <- (mu.star.1 + sqrt(rho.star.sq * sigma.star.1 / sigma.star.4)
                                * (alpha[indices.ad[i + 2L]] - mu.star.2))
                            var.alpha <- (1 - rho.star.sq) * sigma.star.1
                            alpha.curr <- stats::rnorm(n = 1L,
                                                       mean = mean.alpha,
                                                       sd = sqrt(var.alpha))
                            delta.curr <- alpha[indices.ad[i + 2L]] - alpha.curr
                            alpha[indices.ad[i + 1L]] <- alpha.curr
                            delta[indices.ad[i + 1L]] <- delta.curr
                        }
                    }
                    else {
                        if ((i == 0L) && is.infinite(DC.inv[[1L]][1L])) {
                            prec.delta.0 <- DC.inv[[1L]][4L] 
                            prec.alpha <- 1 / omega.alpha^2
                            prec.delta.1 <- phi^2 / omega.delta^2
                            var.delta.curr <- 1 / (prec.delta.0 + prec.alpha + prec.delta.1)
                            mean.delta.curr <- var.delta.curr * (prec.delta.0 * m[[1L]][2L] + prec.alpha * alpha[indices.ad[2L]]
                                + prec.delta.1 * delta[indices.ad[2L]] / phi)
                            delta.curr <- rnorm(n = 1L,
                                                mean = mean.delta.curr,
                                                sd = sqrt(var.delta.curr))
                            delta[indices.ad[1L]] <- delta.curr
                        }
                        else {
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
                            sqrt.C.star <- UC.star %*% DC.star
                            theta.prev <- c(alpha[indices.ad[i + 2L]], delta[indices.ad[i + 2L]])
                            m.star <- m[[i + 1L]] + drop(B %*% (theta.prev - a[[i + 1L]]))
                            z <- stats::rnorm(n = 2L)
                            theta.curr <- m.star + drop(sqrt.C.star %*% z)
                            alpha[indices.ad[i + 1L]] <- theta.curr[1L]
                            delta[indices.ad[i + 1L]] <- theta.curr[2L]
                        }
                    }
                }
            } ## NEW
            iterator.ad <- advanceA(iterator.ad)
            iterator.v <- advanceA(iterator.v)
        }
        prior@alphaDLM@.Data <- alpha
        prior@deltaDLM@.Data <- delta
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
