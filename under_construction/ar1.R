

## TRANSLATED
## HAS_TESTS
setMethod("updatePrior",
          signature(prior = "AR11", beta = "numeric", zeta = "numeric"),
          function(prior, beta, zeta, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updatePrior_AR11_R, prior, beta, zeta)
                  else
                      .Call(updatePrior_R, prior, beta, zeta)
              }
              else {
                  nUpdate <- 5L
                  for (i in seq_len(nUpdate)) {
                      prior <- updateVPriorVAR1(prior = prior,
                                                beta = beta,
                                                zeta = zeta)
                      prior <- updateWPriorWAR11(prior = prior, zeta = zeta)
                      prior <- updateGammaDeltaAR11(prior = prior,
                                                    beta = beta,
                                                    forward = TRUE)
                      prior <- updateGammaNoInitialAR1(prior = prior)
                      prior <- updateDeltaNoInitialAR11(prior = prior)
                      prior <- updateDeltaHatAR11(prior = prior)
                      prior <- updatePhiAR11(prior = prior)
                      prior <- updateGAR11(prior)
                  }
                  prior
              }
          })


## TRANSLATED
## HAS_TESTS
updateGammaDeltaAR11 <- function(prior, beta, forward, useC = FALSE) {
    ## prior
    stopifnot(is(prior, "AR11"))
    stopifnot(validObject(prior))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(!any(is.na(beta)))
    ## forward
    stopifnot(is.logical(forward))
    stopifnot(identical(length(forward), 1L))
    stopifnot(!is.na(forward))
    if (useC) {
        .Call(updateGammaDeltaAR11_R, prior, beta, forward)
    }
    else {    
        K <- prior@K
        L <- prior@L
        G <- prior@G  ## 2x2 matrix
        gamma <- prior@gamma  ## numeric length K+1
        delta <- prior@delta  ## numeric length K+1
        phi <- prior@phi  ## numeric length 1
        v <- prior@v  ## numeric length KL
        w <- prior@w  ## numeric length KL
        m <- prior@m  ## list length K+1
        UC <- prior@UC  ## list length K+1
        DC <- prior@DC  ## list length K+1
        DC.inv <- prior@DCInv
        a <- prior@a  ## list length K
        iter.gamma <- prior@iteratorGamma
        iter.v <- prior@iteratorV
        iter.gamma <- resetA(iter.gamma)
        iter.v <- resetA(iter.v)
        for (l in seq_len(L)) {
            indices.gamma <- iter.gamma@indices
            indices.v <- iter.v@indices
            ## forward filter
            for (k in seq_len(K)) {
                if (forward) {
                    k0 <- k
                    k1 <- k
                    k2 <- k + 1L
                }
                else {
                    k0 <- K - k + 2L
                    k1 <- K - k + 1L
                    k2 <- K - k + 1L
                }
                iv <- indices.v[k1]
                v.inv <- 1 / v[iv]
                if (is.infinite(v.inv))
                    v.inv <- 0
                M.R <- rbind(diag(DC[[k0]], nrow = 2L) %*% t(UC[[k0]]) %*% t(G),
                             sqrt(w[iv]))
                svd.R <- svd(M.R, nu = 0)
                UR <- svd.R$v
                DR.inv <- 1 / svd.R$d
                DR.inv[is.infinite(DR.inv)] <- 0
                M.C <- rbind(sqrt(v.inv) * UR[c(1L, 3L)],
                             diag(DR.inv, nrow = 2L))
                svd.C <- svd(M.C, nu = 0)
                UC[[k2]] <- UR %*% svd.C$v
                DC[[k2]] <- 1 / svd.C$d
                DC[[k2]][is.infinite(DC[[k2]])] <- 0
                DC.inv[[k2]] <- svd.C$d
                a[[k1]] <- drop(G %*% m[[k0]])
                e <- beta[iv] - a[[k1]][1L]
                C <- UC[[k2]] %*% diag(DC[[k2]]^2) %*% t(UC[[k2]])
                A <- C[1:2] * v.inv
                m[[k2]] <- a[[k1]] + A * e
            }
            ## draw final gamma, delta
            k <- if (forward) K + 1L else 1L
            ig <- indices.gamma[k]
            sqrt.C <- UC[[k]] %*% diag(DC[[k]], nrow = 2L)
            z <- rnorm(n = 2L)
            gamma.delta <- m[[k]] + drop(sqrt.C %*% z)
            gamma[ig] <- gamma.delta[1L]
            delta[ig] <- gamma.delta[2L]
            ## backward smooth
            for (k in seq.int(from = K, to = 1L)) {
                if (forward) {
                    k0 <- k + 1L
                    k1 <- k
                    k2 <- k
                }
                else {
                    k0 <- K - k + 1L
                    k1 <- K - k + 1L
                    k2 <- K - k + 2L
                }
                iv <- indices.v[k1]
                ig0 <- indices.gamma[k0]
                ig1 <- indices.gamma[k1]
                ig2 <- indices.gamma[k2]
                C.inv <- UC[[k2]] %*% diag(DC.inv[[k2]]^2) %*% t(UC[[k2]])
                var.inv <- phi^2 / w[iv] + C.inv[1L] - 2 * C.inv[2L] + C.inv[4L]
                var <- 1 / var.inv
                mean <- (phi * delta[ig0] / w[iv]
                         + (C.inv[1L] - C.inv[2L]) * (gamma[ig0] - m[[k2]][1L])
                         + (C.inv[4L] - C.inv[2L]) * m[[k2]][2L]) * var
                delta[ig2] <- rnorm(n = 1L, mean = mean, sd = sqrt(var))
                gamma[ig2] <- gamma[ig0] - delta[ig2]
            }
            iter.gamma <- advanceA(iter.gamma)
            iter.v <- advanceA(iter.v)
        }
        prior@gamma <- gamma
        prior@delta <- delta
        prior
    }
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
        update.series <- prior@updateSeriesDLM # logical length L ## NEW!!!
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
            if (update.series[l]) {
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
                    if (no.level) { ## NEW
                        C.inv <- UC[[k + 1L]] %*% diag(DC.inv[[k + 1L]]^2) %*% t(UC[[k + 1L]])
                        var.inv <- phi^2 / w[iv] + C.inv[1L] - 2 * C.inv[2L] + C.inv[4L]
                        var <- 1 / var.inv
                        mean <- (phi * delta[ig0] / w[iv]
                            + (C.inv[1L] - C.inv[2L]) * (gamma[ig0] - m[[k + 1L]][1L])
                            + (C.inv[4L] - C.inv[2L]) * m[[k + 1L]][2L]) * var
                        delta[ig2] <- rnorm(n = 1L, mean = mean, sd = sqrt(var))
                        gamma[ig2] <- gamma[ig0] - delta[ig2]
                    }
                    else { ## NEW
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
                        sqrt.C.star <- UC.star %*% DC.star
                    }                    ## NEW
                    z <- stats::rnorm(n = 2L) ## REARRANGED
                    theta.curr <- m.star + drop(sqrt.C.star %*% z)
                    alpha[indices.ad[i + 1L]] <- theta.curr[1L]
                    delta[indices.ad[i + 1L]] <- theta.curr[2L]
                }
            }
            iterator.ad <- advanceA(iterator.ad)
            iterator.v <- advanceA(iterator.v)
        }
        prior@alphaDLM@.Data <- alpha
        prior@deltaDLM@.Data <- delta
        prior
    }
}


setClass("SpecHasLevelMixin",
         slots = c(hasLevel = "LogicalFlag"),
         contains = "VIRTUAL")

setClass("HasLevelMixin",
         slots = c(hasLevel = "LogicalFlag"),
         contains = "VIRTUAL",
         validity = function(object) {
             hasLevel <- object@hasLevel@.Data
             omegaAlpha <- object@omegaAlpha@.Data
             if (!hasLevel && !isTRUE(all.equal(omegaAlpha, 0)))
                 return(gettextf("'%s' is %s but '%s' not equal to %d",
                                 "hasLevel" "FALSE", "omegaAlpha", 0L))
             TRUE
         })
                 


setClass("NoTrendMixin",
         contains = c("VIRTUAL",
             "aNoTrendMixin",
             "CNoTrendMixin",
             "MNoTrendMixin",
             "M0NoTrendMixin",
             "RNoTrendMixin"))
         validity = function(object) {
             hasLevel <- object@hasLevel@.Data
             ## hasLevel is FALSE
             if (hasLevel)
                 return(gettextf("does not have trend but '%s' is %s",
                                 "hasLevel", "FALSE"))
             TRUE
         })


DLM <- function(along = NULL, level = Level(), trend = Trend(),
                damp = Damp(), season = NULL, covariates = NULL,
                error = Error()) {
    ## along
    along <- checkAndTidyAlongDLM(along)
    ## level
    if (methods::is(level, "Level")) {
        has.level <- TRUE
        AAlpha <- level@AAlpha
        multAlpha <- level@multAlpha
        nuAlpha <- level@nuAlpha
        omegaAlphaMax <- level@omegaAlphaMax
    }
    else {
        if (is.null(level)) {
            has.level <- FALSE
            scale <- HalfT()
            AAlpha <- scale@A
            multAlpha <- scale@mult
            nuAlpha <- scale@nu
            omegaAlphaMax <- scale@scaleMax
        }
        else {
            stop(gettextf("'%s' has class \"%s\"",
                          "level", class(level)))
        }
    }
    ## trend
    if (methods::is(trend, "Trend")) {
        has.trend <- TRUE
        ADelta <- trend@ADelta
        ADelta0 <- trend@ADelta0
        meanDelta0 <- trend@meanDelta0
        multDelta <- trend@multDelta
        multDelta0 <- trend@multDelta0
        nuDelta <- trend@nuDelta
        omegaDeltaMax <- trend@omegaDeltaMax
    }
    else {
        if (is.null(trend))
            has.trend <- FALSE
        else
            stop(gettextf("'%s' has class \"%s\"",
                          "trend", class(trend)))
    }
    if (!has.level && !has.trend)
        stop(gettextf("'%s' and '%s' are both %s",
                      "level", "trend", "NULL"))
    ## damp
    if (!methods::is(damp, "Damp")) {
        if (is.null(damp))
            damp <- Damp(coef = 1)
        else
            stop(gettextf("'%s' has class \"%s\"",
                          "damp", class(damp)))
    }
    phiKnown <- methods::is(damp, "DampKnown")
    if (phiKnown) {
        minPhi <- 0
        maxPhi <- 1
        shape1Phi <- new("Scale", 1)
        shape2Phi <- new("Scale", 1)
        phi <- damp@phi
    }
    else {
        minPhi <- damp@minPhi
        maxPhi <- damp@maxPhi
        shape1Phi <- damp@shape1Phi
        shape2Phi <- damp@shape2Phi
        phi <- as.double(NA)
    }
    phiKnown <- methods::new("LogicalFlag", phiKnown)
    ## season
    if (methods::is(season, "Season")) {
        has.season <- TRUE
        ASeason <- season@ASeason
        multSeason <- season@multSeason
        nSeason <- season@nSeason
        nuSeason <- season@nuSeason
        omegaSeasonMax <- season@omegaSeasonMax
    }
    else {
        if (is.null(season))
            has.season <- FALSE
        else
            if (!methods::is(season, "Season"))
                stop(gettextf("'%s' has class \"%s\"",
                              "season", class(season)))
    }            
    ## covariates
    if (methods::is(covariates, "Covariates")) {
        has.covariates <- TRUE
        AEtaCoef <- covariates@AEtaCoef
        AEtaIntercept <- covariates@AEtaIntercept
        contrastsArg <- covariates@contrastsArg
        data <- covariates@data
        formula <- covariates@formula
        infant <- covariates@infant
        multEtaCoef <- covariates@multEtaCoef
        nuEtaCoef <- covariates@nuEtaCoef
    }
    else {
        if (is.null(covariates))
            has.covariates <- FALSE
        else
            stop(gettextf("'%s' has class \"%s\"",
                          "covariates", class(covariates)))
    }    
    ## error
    if (!methods::is(error, "Error"))
        stop(gettextf("'%s' has class \"%s\"",
                      "error", class(error)))
    ATau <- error@ATau
    multTau <- error@multTau
    nuTau <- error@nuTau
    tauMax <- error@tauMax
    is.robust <- methods::is(error, "ErrorRobust")
    if (is.robust)
        nuBeta <- error@nuBeta
    ## return
    if (!has.trend && !has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMNoTrendNormZeroNoSeason",
                     AAlpha = AAlpha,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormZeroNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (!has.trend && has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMNoTrendNormZeroWithSeason",
                     AAlpha = AAlpha,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     omegaSeasonMax = omegaSeasonMax,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormZeroWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (!has.trend && !has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMNoTrendNormCovNoSeason",
                     AAlpha = AAlpha,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     infant = infant,
                     multAlpha = multAlpha,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     nuAlpha = nuAlpha,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormCovNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (!has.trend && has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMNoTrendNormCovWithSeason",
                     AAlpha = AAlpha,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multEtaCoef = multEtaCoef,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuEtaCoef = nuEtaCoef,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormCovWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multEtaCoef = multEtaCoef,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuEtaCoef = nuEtaCoef,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (!has.trend && !has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMNoTrendRobustZeroNoSeason",
                     AAlpha = AAlpha,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustZeroNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (!has.trend && has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMNoTrendRobustZeroWithSeason",
                     AAlpha = AAlpha,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustZeroWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (!has.trend && !has.season && has.covariates && is.robust) {
        methods::new("SpecDLMNoTrendRobustCovNoSeason",
                     AAlpha = AAlpha,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustCovNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (!has.trend && has.season && has.covariates && is.robust) {
        methods::new("SpecDLMNoTrendRobustCovWithSeason",
                     AAlpha = AAlpha,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multEtaCoef = multEtaCoef,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuEtaCoef = nuEtaCoef,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustCovWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multEtaCoef = multEtaCoef,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuEtaCoef = nuEtaCoef,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
}
