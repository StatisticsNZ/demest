
setMethod("updateBetaAndPriorBeta",
          signature(prior = "MixNormZero"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_MixNormZero_R,
                            prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R,
                            prior, vbar, n, sigma)
              }
              else {
                  ## beta
                  beta <- updateBeta(prior = prior,
                                     vbar = vbar,
                                     n = n,
                                     sigma = sigma)
                  ## tau
                  prior <- updateTauNorm(prior = prior,
                                         beta = beta)
                  ## vectors
                  prior <- updateVectorsMixAndProdVectorsMix(prior = prior,
                                                             betaTilde = beta) # psi
                  prior <- updateOmegaVectorsMix(prior) # sigma
                  ## weights
                  prior <- updateLatentComponentWeightMix(prior) # z
                  prior <- updateComponentWeightMix(prior) # W
                  prior <- updateWeightMix(prior) # v; deterministic
                  prior <- updateLatentWeightMix(prior) # u
                  prior <- updateIndexClassMaxPossibleMix(prior) # k-tilde
                  prior <- updateIndexClassMix(prior = prior,
                                               betaTilde = beta) # k
                  prior <- updateIndexClassMaxUsedMix(prior) # k-star; deterministic
                  prior <- updateLevelComponentWeightMix(prior) # alpha
                  prior <- updateMeanLevelComponentWeightMix(prior) # mu
                  prior <- updatePhiMix(prior) # phi
                  ## return
                  list(beta, prior)
              }
          })






## READY
## 'sigma_A', 'sigma_S' in notes
updateOmegaVectorsMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateOmegaVectorsMix_R, prior)
    }
    else {    
        omega <- prior@omegaVectorsMix@.Data
        omega.max <- prior@omegaVectorsMaxMix@.Data
        A <- prior@AVectorsMix@.Data
        nu <- prior@nuVectorsMix@.Data
        vectors <- prior@vectorsMix
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        index.class.max.used <- prior@indexClassMaxUsed@.Data
        V <- 0
        n <- 0L
        for (i in seq_along(vectors)) {
            if (i != iAlong) {
                vector <- vectors[[i]]
                dim <- dim.beta[i]
                n.cells <- dim * index.class.max.used
                for (i.vector in seq_len(n.cells))
                    V <- V + vector[i.vector]^2
                n <- n + n.cells
            }
        }
        omega.vectors <- updateSDNorm(sigma = omega,
                                      A = A,
                                      nu = nu,
                                      V = V,
                                      n = n,
                                      max = omega.max)
        prior@omegaVectorsMix@.Data <- omega.vectors
        prior
    }
}



## READY??
updatePhiMix <- function(prior, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updatePhiMix_R, prior)
    }
    else {
        phi <- prior@phiMix
        level <- prior@levelComponentWeightMix@.Data # 'alpha'; n.along * index.class.max
        mean.level <- prior@meanLevelComponentWeightMix@.Data # 'mu'; 1
        index.class.max <- prior@indexClassMaxUsed@.Data # k-star; 1
        omega <- prior@omegaLevelComponentWeightMix@.Data # 'eta'; 1
        dimBeta <- prior@dimBeta
        iAlong <- prior@iAlong
        n.along <- dimBeta[iAlong]
        mode.phi <- modePhiMix(phi = phi,
                               level = level,
                               meanLevel = mean.level,
                               nAlong = n.along,
                               indexClassMax = index.class.max,
                               omega = omega)
        log.post.phi.first <- logPostPhiFirstOrderMix(phi = phi,
                                                      level = level,
                                                      meanLevel = mean.level,
                                                      nAlong = n.along,
                                                      indexClassMax = index.class.max,
                                                      omega = omega)
        log.post.phi.second <- logPostPhiSecondOrderMix(phi = phi,
                                                        level = level,
                                                        meanLevel = mean.level,
                                                        nAlong = n.along,
                                                        indexClassMax = index.class.max,
                                                        omega = omega)
        var.prop <- -1 / log.post.phi.second
        mean.prop <- phi.mode + var * log.post.phi.first
        sd.prop <- sqrt(var.prop)
        phi.prop <- rtnorm1(mean = mean.prop,
                            sd = sd.prop,
                            lower = -1,
                            upper = 1)
        log.post.prop <- logPostPhiMix(phi = phi.prop,
                                       level = level,
                                       meanLevel = mean.level,
                                       nAlong = n.along,
                                       indexClassMax = index.class.max,
                                       omega = omega)
        log.post.curr <- logPostPhiMix(phi = phi
                                       level = level,
                                       meanLevel = mean.level,
                                       nAlong = n.along,
                                       indexClassMax = index.class.max,
                                       omega = omega)
        log.dens.prop <- stats::dnorm(x = phi.prop,
                                      mean = mean,
                                      sd = sd,
                                      log = TRUE)
        log.dens.curr <- stats::dnorm(x = phi,
                                      mean = mean,
                                      sd = sd,
                                      log = TRUE)
        log.diff <- log.post.prop - log.post.curr + log.dens.prop - log.dens.curr
        accept <- (log.diff >= 0) || (stats::runif(1L) < exp(log.diff))
        if (accept)
            prior@phiMix <- phi.prop
        prior
    }
}


## 'v' in notes. Function is deterministic
updateWeightMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateWeightMix_R, prior)
    }
    else {    
        weight <- prior@weightMix@.Data # 'v'; n.along * classIndexMax
        comp.weight <- prior@componentWeightMix@.Data # 'W'; J
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        index.class.max <- prior@indexClassMax@.Data
        n.along <- dim.beta[iAlong]
        n.element <- n.along * index.class.max
        for (i in seq_len(n.element))
            weightMix[i] <- pnorm(comp.weight[i])
        for (i.along in seq_len(n.along)) {
            multiplier.next <- 1
            for (i.class in seq_len(index.class.max)) {
                index.curr <- (i.class - 1L) * n.along + i.along
                multiplier.curr <- multiplier.next
                multiplier.next <- multiplier.next * (1 - weight[index.curr])
                weight[index.curr] <- weight[index.curr] * multiplier.curr
            }
        }
        prior@weightMix@.Data <- weight
        prior
    }
}

## 'mu' in notes
updateMeanLevelComponentWeightMix <- function(prior, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateMeanLevelComponentWeightMix_R, prior)
    }
    else {
        mean.level <- prior@meanLevelComponentWeightMix@.Data # 'mu'; 1
        level <- prior@levelComponentWeightMix@.Data # 'alpha'; n.along * index.class.max
        omega <- prior@omegaLevelComponentWeightMix@.Data # 'eta'; 1
        mean.prior <- prior@priorMeanLevelComponentWeightMix@.Data # 'mu0'; 1
        sd.prior <- prior@priorSDLevelComponentWeightMix@.Data # 'sigma0'; 1
        phi <- prior@phiMix
        index.class.max.used <- prior@indexClassMaxUsedMix@.Data # 'k-star'; 1
        dim.beta <- prior@dimBeta
        iAlong <- prior@iAlong
        n.along <- dim.beta[iAlong]
        inv.omega.sq <- 1 / omega^2
        prec.prior <- 1 / sd.prior^2
        mean.data <- 0
        for (i.class in seq_len(index.class.max.used)) {
            i.w <- (i.class - 1L) * n.along + 1L
            mean.data <- mean.data + level[i.w] * (1 + phi)
            for (i.along in seq.int(from = 2L, to = n.along)) {
                i.curr <- (i.class - 1L) * n.along + i.along
                i.prev <- i.curr - 1L
                mean.data <- mean.data + (level[i.curr] - phi * level[i.prev])
            }
        }
        n.obs <- index.class.max * (n.along - 1L + (1 + phi) / (1 - phi))
        mean.data <- mean.data / n.obs
        prec.data <- n.obs * inv.omega.sq
        var <- 1 / (prec.data + prec.prior)
        mean <- var * (prec.data * mean.data + prec.prior * mean.prior)
        sd <- sqrt(var)
        prior@meanLevelComponentWeightMix@.Data <- rnorm(n = 1L,
                                                         mean = mean,
                                                         sd = sd)
        prior
    }
}




updateAlphaMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaMix)
    if (useC) {
        .Call(updateAlphaMix_R, prior)
    }
    else {
        alphaMix <- prior@alphaMix@.Data
        J <- prior@J@.Data
        index <- prior@indexClassMix@.Data
        prodVectors <- prior@prodVectorsMix@.Data
        for (i in seq_len(J))
            alphaMix[i] <- prodVectors[index[i]]
        prior@alphaMix@.Data <- alphaMix
        prior
    }
}

betaHatAlphaMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaMix)
    if (useC) {
        .Call(betaHatAlphaMix_R, prior)
    }
    else {
        prior@alphaMix@.Data
    }
}


                            
