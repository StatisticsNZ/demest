



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
                                                             betaTilde = beta)
                  prior <- updateOmegaVectorsMix(prior)
                  ## weights
                  prior <- updateLatentComponentWeightMix(prior)
                  prior <- updateComponentWeightMix(prior)
                  prior <- updateWeightMix(prior) # deterministic
                  prior <- updateLatentWeightMix(prior)
                  prior <- updateIndexClassMix(prior = prior,
                                               betaTilde = beta)
                  prior <- updateCurrentNumClassMix(prior) # deterministic
                  prior <- updateLevelComponentWeightMix(prior)
                  prior <- updateMeanLevelComponentWeightMix(prior)
                  prior <- updatePhiMix(prior) ### NOT WRITTEN YET!!!!!
                  ## return
                  list(beta, prior)
              }
          })



## 'vectors' are 'z' in notes
updateVectorsMixAndProdVectorsMix <- function(prior, betaTilde, useC = FALSE) {
    ## 'prior'
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    ## 'betaTilde'
    stopifnot(is.double(betaTilde))
    stopifnot(!any(is.na(betaTilde)))
    stopifnot(identical(length(betaTilde), prior@J@.Data))
    if (useC) {
        .Call(updateVectorsMixAndProdVectorsMix_R, prior, betaTilde)
    }
    else {    
        vectors <- prior@vectorsMix
        omega.vectors <- prior@omegaVectorsMix
        iterators.dims <- prior@iteratorsDimsMix
        prod.vectors <- prior@prodVectorsMix@.Data
        iterator.prod <- prior@iteratorProdVectorMix
        index.class <- prior@indexClassMix@.Data
        index.class.max <- object@indexClassMaxMix@.Data
        iAlong <- object@iAlong@.Data
        dim.beta <- object@dimBeta
        pos1 <- object@posProdVectors1
        pos2 <- object@posProdVectors2
        n.beta.no.along <- object@nBetaNoAlongMix
        v <- getV(prior)
        yX <- double(length = index.class.max)
        XX <- double(length = index.class.max)
        ## loop through vectors, skipping position occupied by "along" dimension
        for (i in seq_along(vectors)) {
            if (i == iAlong)
                next
            vector <- vectors[[i]]
            omega.vector <- omega.vectors[i]
            prec.prior <- 1 / omega.vector^2
            n.elements.vector <- dim.beta[i]
            iterator.beta <- iterators.dims[[i]]
            iterator.beta <- resetA(iterator.beta)
            ## update i'th vector
            for (i.element in seq_len(n.elements.vector)) {
                ## reset vectors holding statistics for updating
                for (i in seq_len(index.class.max)) {
                    yX[i] <- 0
                    XX[i] <- 0
                }
                ## Loop along selected elements of 'betaTilde', 'v', and 'index.class',
                ## plus associated elements from prod.vector, to calculate
                ## statistics needed for updating i.element'th slice.
                indices.beta <- iterator.beta@indices
                for (i.beta in indices.beta) {
                    beta.tilde.i.beta <- betaTilde[i.beta]
                    v.i.beta <- v[i.beta]
                    index.class.i.beta <- index.class[i.beta]
                    i.vector <- (index.class.i.beta - 1L) * n.elements.vector + i.element
                    val.vector <- vector[i.vector]
                    i.beta.no.along <- ((i.beta - 1L) %/% pos1) * pos2 + (i.beta - 1L) %% pos2 + 1L
                    i.prod <- (i.class - 1L) * n.beta.no.along + i.beta.no.along
                    val.prod.vector <- prod.vectors[i.prod]
                    X <- val.prod.vector / val.vector
                    yX[i.class] <- yX[i.class] + beta.tilde.i.beta * X  / v.i.beta
                    XX[i.class] <- XX[i.class] + X * X / v.i.beta
                }
                iterator.beta <- advanceA(iterator.beta)
                ## Update this slice.  Note that if no statistics were collected
                ## for a particular 'i.class', then the corresponding element
                ## of 'vector' is updating from its prior
                for (i.class in seq_len(index.class.max)) {
                    i.vector <- (i.class - 1L) * n.elements.vector + i.element
                    prec.data <- XX[i.class]
                    var <- 1 / (prec.data + prec.prior)
                    sd <- sqrt(var)
                    mean <- var * yX[i.class]
                    vector[i.vector] <- rnorm(n = 1L,
                                              mean = mean,
                                              sd = sd)
                }
            }
            vectors[[i]] <- vector
            ## update prod.vectors
            iterator.prod <- resetM(iterator.prod)
            for (i.prod in seq_along(prod.vectors)) {
                indices.vectors <- iterator.prod@indices
                prod.values <- 1
                for (i.vector in seq_along(index.class.max)) {
                    index.vector <- indices.vectors[i.vector]
                    vector <- vectors[[i.vector]]
                    value.from.vector <- vector[index.vector]
                    prod.values <- prod.values * value.from.vector
                }
                prod.vectors[i.prod] <- prod.values
                iterator.prod <- advanceA(iterator.prod)
            }
        }
        ## update slots in prior
        prior@vectors <- vectors
        prior@prodVectors@.Data <- prod.vectors
    }
}

## 'sigma_A', 'sigma_S' in notes
updateOmegaVectorsMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateOmegaVectorsMix_R, prior)
    }
    else {    
        omega.vectors <- prior@omegaVectorsMix@.Data
        omega.vectors.max <- prior@omegaVectorsMaxMix@.Data
        A.vectors <- prior@AVectorsMix@.Data
        nu.vectors <- prior@nuVectorsMix@.Data
        vectors <- prior@vectorsMix
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        index.class.max <- prior@indexClassMax@.Data
        for (i in seq_along(omega.vectors)) {
            if (i != iAlong) {
                omega <- omega.vectors[i]
                A <- A.vectors[i]
                nu <- nu.vectors[i]
                omega.max <- omega.max.vectors[i]
                vector <- vectors[[i]]
                dim <- dim.beta[i]
                n <- dim * index.class.max
                V <- 0
                for (i.vector in seq_len(n))
                    V <- V + vector[i.vector]^2
                omega.vectors[i.dim] <- updateSDNorm(sigma = omega,
                                                     A = A,
                                                     nu = nu,
                                                     V = V,
                                                     n = n,
                                                     max = omega.max)
            }
        }
        prior@omegaVectorsMix@.Data <- omega.vectors
    }
}

## 'z' in notes
updateLatentComponentWeightMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateLatentComponentWeightMix_R, prior)
    }
    else {    
        latent.comp.weight <- prior@latentComponentWeightMix@.Data # 'z'; J * indexClassMax
        comp.weight <- prior@componentWeightMix@.Data # 'W';  n.along * indexClassMax
        index.class <- prior@indexClassMix@.Data # 'k'; J
        index.class.max <- prior@indexClassMaxMix@.Data
        iAlong <- object@iAlong
        dim.beta <- object@dimBeta
        iterator <- iteratorsDims[[iAlong]]
        n.along <- dim.beta[iAlong]
        n.beta <- as.integer(prod(dim.beta))
        iterator <- resetA(iterator)
        for (i.along in seq_len(n.along)) {
            indices.beta <- iterator@indices
            for (i.beta in indices.beta) {  
                class.i.beta <- index.class[i.beta]
                for (i.class in seq_len(index.class.max)) {
                    i.z <- (i.class - 1L) * n.beta + i.beta
                    i.w <- (i.class - 1L) * n.along + i.along
                    comp.weight.i.w <- comp.weight[i.w]
                    if (i.class < class.i.beta)
                        latent.comp.weight[i.z] <- rtnorm1(mean = comp.weight.i.w,
                                                           sd = 1,
                                                           l = -Inf,
                                                           u = 0)
                    else if (i.class == class.i.beta)
                        latent.comp.weight[i.z] <- rtnorm1(mean = comp.weight.i.w,
                                                           sd = 1,
                                                           l = 0,
                                                           u = Inf)
                    else
                        latent.comp.weight[i.z] <- rnorm(n = 1L,
                                                         mean = comp.weight.i.w,
                                                         sd = 1)
                }
            }
            iterator <- advanceA(iterator)
        }
        prior@latentComponentWeightMix@.Data <- latent.comp.weight
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

## 'u' in notes
updateLatentWeightMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(updateLatentWeightMix_R, prior)
    }
    else {
        latent.weight <- prior@latentWeightMix@.Data # 'u'; J
        weight <- prior@weightMix@.Data # 'v'; n.along * index.class.max
        index.class <- prior@indexClassMix@.Data # 'k'; J
        index.class.max <- prior@indexClassMaxMix@.Data
        iteratorsDims <- object@iteratorsDimsMix
        dim.beta <- object@dimBeta
        iAlong <- object@iAlong
        iterator.beta <- iteratorsDims[[iAlong]]
        n.along <- dim.beta[iAlong]
        iterator.beta <- resetA(iterator.beta)
        for (i.along in seq_len(n.along)) {
            indices.beta <- iterator.beta@indices
            for (i.beta in indices.beta) {
                i.class <- index.class[i.beta]
                i.w <- (i.class - 1L) * n.along + i.along
                weight.i.w <- weight[i.w]
                latent.weight[i.beta] <- runif(n = 1L,
                                               min = 0,
                                               max = weight.i.w)
            }
        }
        prior@latentWeightMix@.Data <- latent.weight
    }
}

## 'k' in notes
updateIndexClassMix <- function(prior, betaTilde, useC = FALSE) {
    ## 'prior'
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    ## 'betaTilde'
    stopifnot(is.double(betaTilde))
    stopifnot(!any(is.na(betaTilde)))
    stopifnot(identical(length(betaTilde), prior@J@.Data))
    if (useC) {
        .Call(updateIndexClassMix_R, prior, betaTilde)
    }
    else {
        index.class <- prior@indexClassMix@.Data 'k'; J
        index.class.max <- prior@indexClassMaxMix@.Data
        index.class.prob <- prior@indexClassProbMix@.Data
        weight <- prior@weightMix@.Data # 'v'; n.along * classIndexMax
        latent.weight <- prior@latentWeightMix@.Data # 'u'; J
        prod.vectors <- prior@prodVectorsMix@.Data
        iAlong <- object@iAlong@.Data
        dim.beta <- object@dimBeta
        iteratorsDims <- object@iteratorsDimsMix
        iterator.beta <- iteratorsDims[[iAlong]]
        n.along <- dim.beta[iAlong]
        pos1 <- object@posProdVectors1
        pos2 <- object@posProdVectors2
        n.beta.no.along <- object@nBetaNoAlongMix
        v <- getV(prior)
        iterator.beta <- resetA(iterator.beta)
        for (i.along in seq_along(n.along)) {
            indices.beta <- iterator.beta@indices
            for (i.beta in indices.beta) {
                latent.weight.i.beta <- latent.weight[i.beta]
                beta.tilde.i.beta <- betaTilde[i.beta]
                v.i.beta <- v[i.beta]
                i.beta.no.along <- ((i.beta - 1L) %/% pos1) * pos2 + (i.beta - 1L) %% pos2 + 1L
                for (i.class in seq_len(index.class.max))
                    index.class.prob[i.class] <- 0
                for (i.class in seq_len(index.class.max)) {
                    i.w <- (i.class - 1L) * n.along + i.along
                    weight.i.w <- weight[i.w]
                    if (latent.weight.i.beta >= weight.i.beta)
                        break
                    i.prod <- (i.class - 1L) * n.beta.no.along + i.beta.no.along
                    val.prod.vector <- prod.vectors[i.prod]
                    prob <- (beta.tilde.i.beta - val.prod.vector)^2 / v.i.beta
                    index.class.prob[i.class] <- prob
                }
                sum.prob <- sum(prob)
                U <- runif(1L) * sum.prob
                cum.sum <- 0
                for (i.class in seq_len(index.class.max)) {
                    cum.sum <- cum.sum + index.class.prob[i.class]
                    if (U <= cum.sum)
                        break
                }
                index.class[i.beta] <- i.class
            }
            iterator.beta <- advanceA(iterator.beta)
        }
        prior@indexClassMix@.Data <- index.class
    }
}

updateLevelComponentWeightMix <- function(prior, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateLevelComponentWeightMix_R, prior)
    }
    else {
        dimBeta <- prior@dimBeta
        iAlong <- prior@iAlong
        n.along <- dimBeta[iAlong]
        index.class.max <- prior@indexClassMax@.Data
        comp <- prior@componentWeightMix@.Data # 'W'; n.along * index.class.max
        level <- prior@levelComponentWeightMix@.Data # 'alpha'; n.along * index.class.max
        mean.level <- prior@meanLevelComponentWeightMix@.Data # 'mu'; 1
        m <- prior@mMix@.Data # length n.along
        C <- prior@CMix@.Data # length n.along
        a <- prior@aMix@.Data # length n.along - 1
        R <- prior@RMix@.Data # length n.along - 1
        phi <- prior@phiMix
        phi.sq <- phi^2
        omega.comp <- prior@omegeComponentWeightMix@.Data # 'epsilon'; 1
        omega.comp.sq <- omega.comp^2
        omega.level <- prior@omegaLevelComponentWeightMix@.Data # 'eta'; 1
        omega.level.sq <- omega.level^2
        for (i.class in seq_len(current.num.class)) {
            m[1L] <- mean.level / (1 - phi)
            C[1L] <- omega.level.sq / (1 - phi.sq)
            ## forward filter
            for (i.along in seq_len(n.along - 1L)) {
                i.wt <- (i.class - 1L) * n.along + i.along
                a[i.along] <- mean.level + phi * m[i.along]
                R[i.along] <- phi.sq * C[i.along] + omega.level.sq
                q <- R[i.along] + omega.comp.sq
                e <- comp[i.wt] - a[i.along]
                A <- R[i.along] / q
                m[i.along + 1L] <- a[i.along] + A * e
                C[i.along + 1L] <- R[i.along] - A^2 * q
            }
            ## draw final values
            i.wt <- i.class * n.along
            level[i.wt] <- stats::rnorm(n = 1L,
                                        mean = m[n.along], 
                                        sd = sqrt(C[n.along]))
            ## backward smooth
            for (i.along in seq.int(from = n.along - 1L, to = 1L)) {
                i.wt.curr <- (i.class - 1L) * n.along + i.along
                i.wt.next <- i.wt.curr + 1L
                B <- C[i.along] * phi / R[i.along]
                m.star <- m[i] + B * (level[i.wt.next] - a[i.along])
                C.star <- C[i] - B^2 * R[i.along]
                level[i.wt.curr] <- stats::rnorm(n = 1L,
                                                 mean = m.star,
                                                 sd = sqrt(C.star))
            }
        }
        prior@levelComponentWeightMix@.Data <- level
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
        mean.level <- prior@meanLevelComponentWeightMixMixin@.Data # 'mu'; 1
        level <- prior@levelComponentWeightMix@.Data # 'alpha'; n.along * index.class.max
        omega <- prior@omegaLevelComponentWeightMix@.Data # 'eta'; 1
        mean.prior <- prior@priorMeanLevelComponentWeightMixMixin@.Data # 'mu0'; 1
        sd.prior <- prior@priorSDLevelComponentWeightMixMixin@.Data # 'sigma0'; 1
        phi <- prior@phiMix
        index.class.max <- prior@indexClassMax@.Data
        dimBeta <- prior@dimBeta
        iAlong <- prior@iAlong
        n.along <- dimBeta[iAlong]
        inv.omega.sq <- 1 / omega^2
        prec.prior <- 1 / sd.prior^2
        mean.data <- 0
        for (i.class in seq_len(index.class.max)) {
            i.level <- (i.class - 1L) * n.along + 1L
            mean.data <- mean.data + level[i.level] * (1 + phi)
            for (i.along in seq.int(from = 2L, to = n.along)) {
                i.curr <- (i.class - 1L) * n.along + i.along
                i.prev <- i.curr - 1L
                mean.data <- mean.data + (level[i.curr] - phi * level[i.prev])
            }
        }
        prec.data <- (index.class.max * (n.along - 1L + (1 + phi) / (1 - phi))
            * inv.omega.sq)
        var <- 1 / (prec.data + prec.prior)
        mean <- var * (prec.data * mean.data + prec.prior * mean.prior)
        sd <- sqrt(var)
        prior@meanLevelComponentWeightMix@.Data <- rnorm(n = 1L,
                                                         mean = mean,
                                                         sd = sd)
        prior
    }
}


## HELPER FUNCTIONS NOT FINISHED YET
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
        index.class.max <- prior@indexClassMax@.Data        
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




updateAlphaMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaMix)
    if (useC) {
        .Call(updateAlphaMix_R, prior)
    }
    else {
        alphaMix <- object@alphaMix@.Data
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






modePhiMix <- function(phi, level, meanLevel, nAlong, numClass, omega,
                       useC = FALSE) {
    ## 'phi'
    stopifnot(identical(length(phi), 1L))
    stopifnot(is.double(phi))
    stopifnot(!is.na(phi))
    stopifnot(abs(phi) <= 1)
    ## 'level'
    stopifnot(is.double(level))
    stopifnot(!any(is.na(level)))
    ## 'meanLevel'
    stopifnot(identical(length(meanLevel), 1L))
    stopifnot(is.double(meanLevel))
    stopifnot(!is.na(meanLevel))
    ## 'nAlong'
    stopifnot(identical(length(nAlong), 1L))
    stopifnot(is.integer(nAlong))
    stopifnot(!is.na(nAlong))
    stopifnot(nAlong >= 2L)
    ## 'numClass'
    stopifnot(identical(length(numClass), 1L))
    stopifnot(is.integer(numClass))
    stopifnot(!is.na(numClass))
    stopifnot(numClass > 0)
    ## 'omega'
    stopifnot(identical(length(omega), 1L))
    stopifnot(is.double(omega))
    stopifnot(!is.na(omega))
    stopifnot(omega > 0)
    ## 'level', 'nAlong', 'numClass'
    stopifnot(length(level) >= nAlong * numClass)
    if (useC) {
        .Call(maxPhiMix_R, phi, level, meanLevel, nAlong, numClass, omega)
    }
    else {
        phi.curr <- 0
        diff.outer <- 1
        while (diff.outer > 0.0001) {
            length.step <- 0.1
            log.post.curr <- logPostPhiMix(phi = phi.curr,
                                           level = level,
                                           meanLevel = meanLevel,
                                           nAlong = nAlong,
                                           numClass = numClass,
                                           omega = omega)
            diff.inner <- 0
            phi.new <- 1
            while (((diff.inner <= 0) & (length.step > 0.001)) | (abs(phi.new) >= 1)) {
                log.post.first <- logPostPhiFirstOrderMix(phi = phi,
                                                          level = level,
                                                          meanLevel = meanLevel,
                                                          nAlong = nAlong,
                                                          numClass = numClass,
                                                          omega = omega)
                log.post.second <- logPostPhiSecondOrderMix(phi = phi,
                                                            level = level,
                                                            meanLevel = meanLevel,
                                                            nAlong = nAlong,
                                                            numClass = numClass,
                                                            omega = omega)
                phi.new <- phi.curr - length.step * log.post.first / log.post.second
                log.post.new <- logPostPhiMix(phi = phi.new,
                                              level = level,
                                              meanLevel = meanLevel,
                                              nAlong = nAlong,
                                              numClass = numClass,
                                              omega = omega)
                diff.inner <- log.post.new - log.post.curr
                length.step <- length.step - 0.001
            }
            diff.outer <- abs(phi.new - phi.curr)
            phi.curr <- phi.new
        }
        phi.new
    }
}
                            

## ## record logical variable if max class not high enough
## updateCurrentNumClassMix <- function(prior) {
##     latent.weight <- prior@latentWeightMix@.Data
##     weight <- prior@weightMix@.Data
##     max.num.class <- prior@indexClassMaxMix@.Data
##     i.along <- object@iAlong
##     dim.beta <- object@dimBeta
##     n.along <- dim.beta[i.along]
##     one.minus.min.latent <- 1 - min(latent.weight)
##     sums.weights <- rep(0, times = n.along)
##     found.ans <- FALSE
##     current.num.class <- 0L
##     while (!found.ans && (current.num.class < max.num.class)) {
##         current.num.class <- current.num.class + 1L
##         offset <- (current.num.class - 1L) * n.along
##         for (i in seq_len(n.along))
##             sums.weights[i] <- sums.weights[i] + weights[i + offset]
##         for (i in seq_len(n.along)) {
##             if (sums.weights[i] < one.minus.min.latent)
##                 break
##             if (i == n.along)
##                 found.ans <- TRUE
##         }
##     }
##     prior@currentNumClass@.Data <- current.num.class
## }
