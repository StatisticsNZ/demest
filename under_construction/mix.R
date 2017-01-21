
Latent <- function(along = NULL,
                   weights = Weights(),
                   vectors = Vectors(),
                   covariates = NULL,
                   error = Error(),
                   maxClass = NULL) {
    NULL
}

Weights <- function(mean = 0, sd = 1, level = HalfT(), trend = HalfT()) {
    NULL
}    

Vectors <- function(df = 4, scale = list(HalfT())) {
    NULL
}

updateVectorsMixAndProdVectorsMix <- function(prior, betaTilde) {
    vectors <- prior@vectorsMix
    sdVectors <- prior@sdVectorsMix
    iterators.dims <- prior@iteratorsDimsMix
    prodVectors <- prior@prodVectorsMix@.Data
    mapping <- prior@mappingBetaToProdVectorMix
    indexClass <- prior@indexClass
    iterator.prod <- prior@iteratorProdVectorMix
    currentNumClass <- object@currentNumClass@.Data
    iAlong <- object@iAlong@.Data
    v <- getV(prior)
    yX <- double(length = currentNumClass)
    XX <- double(length = currentNumClass)
    for (i in seq_along(vectorsMix)) {
        if (i == iAlong)
            next
        vector <- vectors[[i]]
        iterator.beta <- iterators.dims[[i]]
        sd.vector <- sdVectors[i]
        prec.prior.vector <- 1 / sd.vector^2
        n.levels.vector <- nrow(vector)
        iterator <- resetA(iterator)
        ## update i'th vector
        for (i.level in seq_len(n.levels.vector)) {
            ## collect statistics needed for updating this slice
            indices.beta <- iterator.beta@indices
            for (i.beta in indices.beta) {
                i.class <- indexClass[i.beta]
                i.vector <- (i.class - 1L) * n.levels.vector + i.level
                i.prod <- getIProdVectorMix(iBeta = i.beta,
                                            iClass = i.class,
                                            mapping = mapping)
                betaTilde.i.beta <- betaTilde[i.beta]
                v.i.beta <- v[i.beta]
                prod.vector.i.beta <- prodVectors[i.prod]
                vector.i.beta <- vector[i.vector]
                X.i.beta <- prod.vector.i.beta / vector.i.beta
                yX.i.beta <- betaTilde.i.beta * X.i.beta / v
                XX.i.beta <- X.i.beta^2 / v
                yX[i.class] <- yX.i.beta
                XX[i.class] <- XX.i.beta
            }
            iterator.beta <- advanceA(iterator.beta)
            ## update this slice
            for (i.class in seq_len(currentNumClass)) {
                i.vector <- (i.class - 1L) * n.levels.vector + i.level
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
        ## update prodVectors
        iterator.prod <- resetPVM(iterator.prod)
        for (i.prod in seq_along(prodVectors)) {
            indices.vectors <- iterator.prod@indices ## length = maxNumClass
            prod.values <- 1
            for (i.vector in seq_along(currentNumClass)) {
                index.vector <- indices.vectors[i.vector]
                vector <- vectors[[i.vector]]
                value.from.vector <- vector[index.vector]
                prod.values <- prod.values * value.from.vector
            }
            prodVectors[i.prod] <- prod.values
            advancePVM(iterator.prod)
        }
    }
    ## update slots in prior
    prior@vectors <- vectors
    prior@prodVectors@.Data <- prodVectors
}

updateIndexClassMix <- function(prior, betaTilde) {
    alpha <- prior@alphaMixMix@.Data
    mapping <- prior@mappingBetaToAlphaMix
    indexClass <- prior@indexClass
    currentNumClass <- object@currentNumClass
    v <- getV(prior)
    yX <- double(length = currentNumClass)
    XX <- double(length = currentNumClass)
}

## assumes that weights stored in along-dimension x class order
updateWeightMix <- function(prior) {
    weightMix <- prior@weightMix@.Data
    componentWeightMix <- prior@componentWeightMix@.Data
    iAlong <- prior@iAlong
    dimBeta <- prior@dimBeta
    maxNumClass <- prior@maxNumClass@.Data
    n.along <- dimBeta[iAlong]
    n.element <- n.along * maxNumClass
    for (i in seq_len(n.element))
        weightMix[i] <- pnorm(componentWeightMix[i])
    for (i.along in seq_len(n.along)) {
        multiplier.next <- 1
        for (i.class in seq_len(maxNumClass)) {
            index.curr <- i.along + (i.class - 1L) * n.along
            multiplier.curr <- multiplier.next
            multiplier.next <- multiplier.next * (1 - weightMix[index.curr])
            weightMix[index.curr] <- weightMix[index.curr] * multiplier.curr
        }
    }
    prior@weightMix@.Data <- prior@weightMix
    prior
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

updateCurrentNumClassMix <- function(prior) {
    latent.weight <- prior@latentWeightMix@.Data
    weight <- prior@weightMix@.Data
    max.num.class <- prior@maxNumClass@.Data
    i.along <- object@iAlong
    dim.beta <- object@dimBeta
    n.along <- dim.beta[i.along]
    one.minus.min.latent <- 1 - min(latent.weight)
    sums.weights <- rep(0, times = n.along)
    found.ans <- FALSE
    current.num.class <- 0L
    while (!found.ans && (current.num.class < max.num.class)) {
        current.num.class <- current.num.class + 1L
        offset <- (current.num.class - 1L) * n.along
        for (i in seq_len(n.along))
            sums.weights[i] <- sums.weights[i] + weights[i + offset]
        for (i in seq_len(n.along)) {
            if (sums.weights[i] < one.minus.min.latent)
                break
            if (i == n.along)
                found.ans <- TRUE
        }
    }
    prior@currentNumClass@.Data <- current.num.class
}

## 'latentComponentWeightMix' is 'z' in notes
## 'componentWeightMix' is 'W' in notes
## 'index.class' is 'k' in notes
## Update to 'current.num.class' ('k-tilde' in notes) - not max.num.class
updateLatentComponentWeightMix <- function(prior) {
    latent.comp.weight <- prior@latentComponentWeightMix@.Data # length = J
    comp.weight <- prior@componentWeightMix@.Data # length = n.along x max.num.class
    index.class <- prior@indexClassMix@.Data # length = J
    current.num.class <- prior@currentNumClass@.Data
    i.along <- object@iAlong
    dim.beta <- object@dimBeta
    iterator <- iteratorsDims[[i.along]]
    n.along <- dim.beta[i.along]
    for (i.class in seq_len(current.num.class)) { # i.class is 'l' in notes
        iterator <- resetA(iterator)
        for (i.along in seq_len(n.along)) {
            indices.beta <- iterator@indices
            for (i.beta in indices.beta) {
                class <- index.class[i.beta] # 'l' in notes
                update <- class >= i.class
                if (update) {
                    if (class > i.class)
                        latent.comp.weight[i.beta] <- rtnorm1(mean = W,
                                                              sd = 1,
                                                              l = -Inf,
                                                              u = 0)
                    else
                        latent.comp.weight[i.beta] <- rtnorm1(mean = W,
                                                              sd = 1,
                                                              l = 0,
                                                              u = Inf)
                }
            }
        }
        iterator <- advanceA(iterator)
    }
    prior@latentComponentWeightMix@.Data <- latent.comp.weight
}

## 'componentWeightMix' is 'W' in notes
## 'latentComponentWeightMix' is 'z' in notes
## 'index.class' is 'k' in notes
## Update to 'current.num.class' ('k-tilde' in notes) - not max.num.class
updateComponentWeightMix <- function(prior) {
    comp.weight <- prior@componentWeightMix@.Data # length = n.along x max.num.class
    latent.comp.weight <- prior@latentComponentWeightMix@.Data # length = J
    level.comp.weight <- prior@levelComponentWeightMix@.Data # length = n.along * max.num.class
    index.class <- prior@indexClassMix@.Data # length = J
    omega <- prior@omegaComponentWeight@.Data 
    current.num.class <- prior@currentNumClass@.Data
    i.along <- object@iAlong
    dim.beta <- object@dimBeta
    iteratorsDims <- object@iteratorsDimsMix
    iterator <- iteratorsDims[[i.along]]
    n.along <- dim.beta[i.along]
    inv.omega.sq <- 1 / omega^2
    for (i.class in seq_len(current.num.class)) { # i.class is 'h' in notes
        iterator <- resetA(iterator)
        for (i.along in seq_len(n.along)) {
            i.comp.weight <- i.along + (i.class - 1L) * n.along
            indices.latent.comp.weight <- iterator@indices
            sum.is.component <- 0
            sum.latent.comp.weight <- 0
            for (i.latent.comp.weight in indices.latent.comp.weight) {
                class <- index.class[i.latent.comp.weight] # 'k' in notes
                is.component <- class >= i.class
                if (is.component) {
                    sum.is.component <- sum.is.component + 1L
                    sum.latent.comp.weight <- (sum.latent.comp.weight
                        + latent.comp.weight[i.latent.comp.weight])
                }
            }
            level <- level.comp.weight[i.comp.weight]
            var <- 1 / (inv.omega.sq + sum.is.component)
            mean <- var * (level * inv.omega.sq + sum.latent.comp.weight)
            sd <- sqrt(var)
            comp.weight[i.comp.weight] <- rnorm(n = 1L,
                                                mean = mean,
                                                sd = sd)
        }
        iterator <- advanceA(iterator)
    }
    prior@componentWeightMix@.Data <- comp.weight
}


updateOmegaVectorsMix <- function(prior) {
    omega.vectors <- prior@omegaVectorsMix@.Data
    omega.max.vectors <- prior@omegaVectorsMaxMix@.Data
    A.vectors <- prior@AVectorsMix@.Data
    nu.vectors <- prior@nuVectorsMix@.Data
    vectors <- prior@vectorsMix
    i.along <- prior@iAlong
    dimBeta <- prior@dimBeta
    current.num.class <- prior@currentNumClass@.Data ## IS THIS THE RIGHT SLOT???
    for (i.dim in seq_along(omega.vectors)) {
        if (i.dim != i.along) {
            omega <- omega.vectors[i.dim]
            A <- A.vectors[i.dim]
            nu <- nu.vectors[i.dim]
            omega.max <- omega.max.vectors[i.dim]
            vector <- vectors[[i.dim]]
            dim <- dimBeta[i.dim]
            n <- dim * current.num.class
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





