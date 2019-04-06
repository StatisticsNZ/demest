

## HAS_TESTS
checkDataPredict <- function(data) {
    if (is.null(data))
        return(NULL)
    ## has class 'list'
    if (!is.list(data))
        stop(gettextf("'%s' has class \"%s\"",
                      "data", class(data)))
    names.data <- names(data)
    ## has names
    if (is.null(names.data))
        stop(gettextf("'%s' does not have names",
                      "data"))
    ## names have no missing values
    if (any(is.na(names.data)))
        stop(gettextf("names for '%s' have missing values",
                      "data"))
    ## names have no blanks
    if (!all(nzchar(names.data)))
        stop(gettextf("names for '%s' have blanks",
                      "data"))
    ## names have no duplicates
    if (any(duplicated(names.data)))
        stop(gettextf("names for '%s' have duplicates",
                      "data"))
    ## all elements are data.frames
    for (i in seq_along(data))
        if (!methods::is(data[[i]], "data.frame"))
            stop(gettextf("item \"%s\" from '%s' has class \"%s\"",
                          names.data[i], "data", class(data[[i]])))
    NULL
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
    n.beta <- length(betas)
    metadata.pred <- makeMetadataPredict(metadata = metadata.first,
                                         along = along,
                                         labels = labels,
                                         n = n)
    DimScale.along <- DimScales(metadata.pred)[along][[1L]]
    extrapolate.struc.zero <- (methods::is(model, "StrucZeroArrayMixin")
        && (methods::is(DimScale.along, "Intervals") || methods::is(DimScale.along, "Points")))
    if (extrapolate.struc.zero) {
        struc.zero.array.first <- model@strucZeroArray
        labels <- labels(DimScale.along)
        struc.zero.array.pred <- extrapolateStrucZeroArray(struc.zero.array.first,
                                                           along = along,
                                                           labels = labels)
    }
    else {
        .Data <- array(1L,
                       dim = dim(metadata.pred),
                       dimnames = dimnames(metadata.pred))
        struc.zero.array.pred <- methods::new("Counts",
                                     .Data = .Data,
                                     metadata = metadata.pred)
    }
    theta <- ifelse(struc.zero.array.pred@.Data == 0L, 0, mean(theta.old))
    theta <- as.double(theta)
    thetaTransformed <- rep(0, length = length(theta))
    mu <- rep(0, length = length(theta))
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
                                                     along = along.margin,
                                                     margin = margin,
                                                     strucZeroArray = struc.zero.array.pred)            
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
    val.betas <- lapply(betas, function(x) rep(0, length(x)))
    meansBetas <- val.betas
    variancesBetas <- val.betas
    gradientBetas <- val.betas
    momentumBetas <- val.betas
    iterator.betas <- BetaIterator(dim = dim, margins = margins)
    offsets.betas <- makeOffsetsBetas(model, offsetModel = offsetModel)
    offsets.priors.betas <- makeOffsetsPriorsBetas(model, offsetModel = offsetModel)
    offsets.sigma <- makeOffsetsSigma(model, offsetModel = offsetModel)
    i.method.model <- i.method.model.first + 100L
    list(theta = theta,
         thetaTransformed = thetaTransformed,
         mu = mu,
         metadataY = metadata.pred,
         cellInLik = cell.in.lik,
         betas = betas,
         meansBetas = meansBetas,
         variancesBetas = variancesBetas,
         gradientBetas = gradientBetas,
         momentumBetas = momentumBetas,
         strucZeroArray = struc.zero.array.pred,
         priorsBetas = priors.betas,
         iteratorBetas = iterator.betas,
         dims = dims,
         betaIsPredicted = beta.is.predicted,
         offsetsBetas = offsets.betas,
         offsetsPriorsBetas = offsets.priors.betas,
         offsetsSigma = offsets.sigma,
         iMethodModel = i.method.model)         
}    


## HAS_TESTS
lengthValues <- function(object) {
    if (is.numeric(object))
        length(object)
    else if (is.list(object)) {
        if (length(object) > 0L)
            sum(sapply(object, lengthValues))
        else
            0L
    }
    else {
        slots.to.extract <- object@slotsToExtract
        if (length(slots.to.extract) > 0L)
            sum(sapply(slots.to.extract, function(name) lengthValues(methods::slot(object, name))))
        else
            0L
    }
}

## HAS_TESTS
makeMetadataPredict <- function(metadata, along, labels, n) {
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    dimscales <- dimscales(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    if (!is.null(labels)) {
        if (!is.null(n))
            warning(gettextf("'%s' ignored when '%s' provided",
                             "n", "labels"))
        labels <- as.character(labels)
        DimScale.pred <- dembase::inferDimScale(dimtype = dimtypes[along],
                                                dimscale = dimscales[along],
                                                labels = labels,
                                                name = names[along])
    }
    else {
        if (!is.null(n))
            DimScale.pred <- dembase::incrementDimScale(DimScales[[along]],
                                                        n = n)
        else
            stop(gettextf("must supply '%s' or '%s' argument",
                          "labels", "n"))
    }
    DimScales[[along]] <- DimScale.pred
    methods::new("MetaData",
                 nms = names,
                 dimtypes = dimtypes,
                 DimScales = DimScales)
}

## HAS_TESTS
makeOffsetsBetas <- function(model, offsetModel) {
    slots.to.extract <- model@slotsToExtract
    first <- offsetModel
    for (name in slots.to.extract) {
        if (identical(name, "betas")) {
            betas <- methods::slot(model, name)
            n.beta <- length(betas)
            ans <- vector(mode = "list", length = n.beta)
            for (i in seq_len(n.beta)) {
                last <- first + length(betas[[i]]) - 1L
                ans[[i]] <- methods::new("Offsets", c(first, last))
                first <- last + 1L
            }
            return(ans)
        }
        else {
            slot <- methods::slot(model, name)
            increment <- lengthValues(slot)
            first <- first + increment
        }
    }
    stop(gettextf("slot \"%s\" not found",
                  "betas"))
}

## HAS_TESTS
makeOffsetsPriorsBetas <- function(model, offsetModel) {
    slots.to.extract <- model@slotsToExtract
    first <- offsetModel
    for (name in slots.to.extract) {
        if (identical(name, "priorsBetas")) {
            priors <- methods::slot(model, name)
            n.beta <- length(priors)
            ans <- vector(mode = "list", length = n.beta)
            for (i in seq_len(n.beta)) {
                n.values <- lengthValues(priors[[i]])
                if (n.values > 0L) {
                    last <- first + n.values - 1L
                    ans[[i]] <- methods::new("Offsets", c(first, last))
                    first <- last + 1L
                }
                else
                    ans[i] <- list(NULL)
            }
            return(ans)
        }
        else {
            slot <- methods::slot(model, name)
            increment <- lengthValues(slot)
            first <- first + increment
        }
    }
    stop(gettextf("slot \"%s\" not found",
                  "priorsBetas"))
}

## HAS_TESTS
makeOffsetsSigma <- function(model, offsetModel) {
    slots.to.extract <- model@slotsToExtract
    first <- offsetModel
    for (name in slots.to.extract) {
        if (identical(name, "sigma")) {
            ans <- methods::new("Offsets", c(first, first))
            return(ans)
        }
        else {
            slot <- methods::slot(model, name)
            increment <- lengthValues(slot)
            first <- first + increment
        }
    }
    stop(gettextf("slot \"%s\" not found",
                  "sigma"))
}

## HAS_TESTS
makeOffsetsVarsigma <- function(model, offsetModel) {
    slots.to.extract <- model@slotsToExtract
    first <- offsetModel
    for (name in slots.to.extract) {
        if (identical(name, "varsigma")) {
            ans <- methods::new("Offsets", c(first, first))
            return(ans)
        }
        else {
            slot <- methods::slot(model, name)
            increment <- lengthValues(slot)
            first <- first + increment
        }
    }
    stop(gettextf("slot \"%s\" not found",
                  "varsigma"))
}


## HAS_TESTS
extrapolateStrucZeroArray <- function(strucZeroArray, along, labels) {
    DimScales <- DimScales(strucZeroArray)
    DS.along <- DimScales[[along]]
    if (!(methods::is(DS.along, "Points") || methods::is(DS.along, "Intervals")))
        stop(gettextf("%s for '%s' dimension has class \"%s\"",
                      "dimscales", "along", class(DS.along)))
    ans <- dembase::extrapolate(strucZeroArray,
                                along = along,
                                labels = labels)
    ans <- dembase::slab(ans,
                         dimension = along,
                         elements = labels,
                         drop = FALSE)
    ans <- toInteger(ans)
    ans
}


## TRANSLATED
## HAS_TESTS
predictAlphaDLMNoTrend <- function(prior, useC = FALSE) {
    stopifnot((methods::is(prior, "DLM") && methods::is(prior, "NoTrendMixin")))
    if (useC) {
        .Call(predictAlphaDLMNoTrend_R, prior)
    }
    else {
        K <- prior@K@.Data
        L <- prior@L@.Data
        along.all.struc.zero <- prior@alongAllStrucZero
        alpha <- prior@alphaDLM@.Data # numeric vector length (K+1)L
        phi <- prior@phi
        omega <- prior@omegaAlpha@.Data
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            if (!along.all.struc.zero[l]) {
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1L]
                    k.prev <- indices[i]
                    mean <- phi * alpha[k.prev]
                    alpha[k.curr] <- stats::rnorm(n = 1L,
                                                  mean = mean,
                                                  sd = omega)
                }
            }
            iterator <- advanceA(iterator)
        }
        prior@alphaDLM@.Data <- alpha
        prior
    }
}

## TRANSLATED
## HAS_TESTS
predictAlphaDeltaDLMWithTrend <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM") && methods::is(prior, "WithTrendMixin"))
    if (useC) {
        .Call(predictAlphaDeltaDLMWithTrend_R, prior)
    }
    else {
        K <- prior@K@.Data
        L <- prior@L@.Data
        along.all.struc.zero <- prior@alongAllStrucZero
        alpha <- prior@alphaDLM@.Data # numeric vector length (K+1)L
        delta <- prior@deltaDLM@.Data # numeric vector length (K+1)L
        phi <- prior@phi
        omega.alpha <- prior@omegaAlpha@.Data
        omega.delta <- prior@omegaDelta@.Data
        iterator <- prior@iteratorState
        has.level <- prior@hasLevel@.Data
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            if (!along.all.struc.zero[l]) {
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1L]
                    k.prev <- indices[i]
                    mean.delta <- phi * delta[k.prev]
                    delta[k.curr] <- stats::rnorm(n = 1L,
                                                  mean = mean.delta,
                                                  sd = omega.delta)
                    mean.alpha <- alpha[k.prev] + delta[k.prev]
                    if (has.level)
                        alpha[k.curr] <- stats::rnorm(n = 1L,
                                                      mean = mean.alpha,
                                                      sd = omega.alpha)
                    else
                        alpha[k.curr] <- mean.alpha
                }
            }
            iterator <- advanceA(iterator)
        }
        prior@alphaDLM@.Data <- alpha
        prior@deltaDLM@.Data <- delta
        prior
    }
}

## TRANSLATED
## HAS_TESTS
## This is ugly, but writing the function in a proper
## object-oriented way seemed wrong, since the best approach
## in C would probably be to write a single function.
predictBeta <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(predictBeta_R, prior)
    }
    else {
        J <- prior@J@.Data
        ## If we forget a special case
        ## functions 'betaHat' and 'getV' will almost
        ## certainly pick it up.
        is.exch.fixed <- methods::is(prior, "ExchFixed")
        is.known.certain <- methods::is(prior, "KnownCertain")
        is.known.uncertain <- methods::is(prior, "KnownUncertain")
        is.zero <- methods::is(prior, "Zero") 
        if (is.exch.fixed) {
            sd <- prior@tau@.Data
            stats::rnorm(n = J, mean = 0, sd = sd)
        }
        else if (is.known.certain) {
            prior@alphaKnown@.Data
        }
        else if (is.known.uncertain) {
            alpha <- prior@alphaKnown@.Data
            A <- prior@AKnownVec@.Data
            stats::rnorm(n = J, mean = alpha, sd = A)
        }
        else if (is.zero) {
            rep(0, times = J)
        }
        else {
            mean <- betaHat(prior)
            var <- getV(prior)
            sd <- sqrt(var)
            stats::rnorm(n = J, mean = mean, sd = sd)
        }
    }
}

## TRANSLATED
## HAS_TESTS
predictBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::is(object, "BetaIsPredicted"))
    methods::validObject(object)
    if (useC) {
        .Call(predictBetas_R, object)
    }
    else {
        betas <- object@betas
        priors <- object@priorsBetas
        beta.is.predicted <- object@betaIsPredicted
        for (i in seq_along(betas)) {
            if (beta.is.predicted[i])
                betas[[i]] <- predictBeta(priors[[i]])
        }
        object@betas <- betas
        object
    }
}

## TRANSLATED
## HAS_TESTS
predictComponentWeightMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    methods::validObject(prior)
    if (useC) {
        .Call(predictComponentWeightMix_R, prior)
    }
    else {    
        comp <- prior@componentWeightMix@.Data # W; n.along * index.class.max
        level <- prior@levelComponentWeightMix@.Data # alpha; n.along * index.class.max
        index.class.max <- prior@indexClassMaxMix@.Data
        omega <- prior@omegaComponentWeightMix@.Data # sigma_epsilon; 1
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        n.along <- dim.beta[iAlong]
        for (i.class in seq_len(index.class.max)) {
            for (i.along in seq_len(n.along)) {
                i.wt <- (i.class - 1L) * n.along + i.along
                comp[i.wt] <- stats::rnorm(n = 1L,
                                           mean = level[i.wt],
                                           sd = omega)
            }
        }
        prior@componentWeightMix@.Data <- comp
        prior
    }
}

## TRANSLATED
## HAS_TESTS
predictIndexClassMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    methods::validObject(prior)
    if (useC) {
        .Call(predictIndexClassMix_R, prior)
    }
    else {    
        index.class <- prior@indexClassMix
        index.class.max <- prior@indexClassMaxMix@.Data
        weight <- prior@weightMix
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        iteratorsDims <- prior@iteratorsDimsMix
        prob <- prior@indexClassProbMix@.Data # length index.class.max
        n.along <- dim.beta[iAlong]
        iterator.beta <- iteratorsDims[[iAlong]]
        iterator.beta <- resetS(iterator.beta)
        for (i.along in seq_len(n.along)) {
            ## prepare cumulative probabilities
            sum.wt <- 0
            for (i.class in seq_len(index.class.max)) {
                i.wt <- (i.class - 1L) * n.along + i.along
                prob[i.class] <- weight[i.wt]
                sum.wt <- sum.wt + weight[i.wt]
            }
            for (i.class in seq_len(index.class.max))
                prob[i.class] <- prob[i.class] / sum.wt
            for (i.class in seq.int(from = 2L, to = index.class.max))
                prob[i.class] <- prob[i.class] + prob[i.class - 1L]
            ## draw classes
            indices.beta <- iterator.beta@indices
            for (i.beta in indices.beta) {
                U <- stats::runif(n = 1L)
                for (i.class in seq_len(index.class.max)) {
                    if (U < prob[i.class])
                        break
                }
                index.class[i.beta] <- i.class
            }
            iterator.beta <- advanceS(iterator.beta)
        }
        prior@indexClassMix <- index.class
        prior
    }
}

## TRANSLATED
## HAS_TESTS
predictLevelComponentWeightMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    methods::validObject(prior)
    if (useC) {
        .Call(predictLevelComponentWeightMix_R, prior)
    }
    else {        
        level <- prior@levelComponentWeightMix@.Data # alpha; n.along * index.class.max
        level.old <- prior@levelComponentWeightOldMix # final values of alpha; index.class.max
        mean.level <- prior@meanLevelComponentWeightMix@.Data # 'mu'; 1
        dim.beta <- prior@dimBeta
        iAlong <- prior@iAlong
        n.along <- dim.beta[iAlong]
        index.class.max <- prior@indexClassMaxMix@.Data
        phi <- prior@phiMix
        omega <- prior@omegaLevelComponentWeightMix@.Data # sigma_eta; 1
        for (i.class in seq_len(index.class.max)) {
            i.wt <- (i.class - 1L) * n.along + 1L
            level.prev <- level.old[i.class]
            mean <- mean.level + phi * level.prev
            level[i.wt] <- stats::rnorm(n = 1L,
                                        mean = mean,
                                        sd = omega)
            for (i.along in seq.int(from = 2L, to = n.along)) {
                i.wt.curr <- (i.class - 1L) * n.along + i.along
                i.wt.prev <- i.wt.curr - 1L
                level.prev <- level[i.wt.prev]
                mean <- mean.level + phi * level.prev
                level[i.wt.curr] <- stats::rnorm(n = 1L,
                                                 mean = mean,
                                                 sd = omega)
            }
        }
        prior@levelComponentWeightMix@.Data <- level
        prior
    }
}


## HAS_TESTS
predictOneChain <- function(combined, tempfileOld, tempfileNew,
                            lengthIter, nIteration, nUpdate,
                            useC) {
    con <- file(tempfileNew, open = "wb")
    on.exit(close(con))
    for (iteration in seq_len(nIteration)) {
        combined <- predictCombined(object = combined,
                                    nUpdate = nUpdate,
                                    filename = tempfileOld,
                                    lengthIter = lengthIter,
                                    iteration = iteration,
                                    useC = useC)
        values <- extractValues(combined)
        writeBin(object = values, con = con)
    }
    combined
}

## TRANSLATED
## HAS_TESTS
predictPriorsBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::is(object, "BetaIsPredicted"))
    methods::validObject(object)
    if (useC) {
        .Call(predictPriorsBetas_R, object)
    }
    else {
        priors <- object@priorsBetas
        beta.is.predicted <- object@betaIsPredicted
        for (i in seq_along(priors)) {
            if (beta.is.predicted[i])
                priors[[i]] <- predictPrior(prior = priors[[i]])
        }
        object@priorsBetas <- priors
        object
    }
}

## TRANSLATED
## HAS_TESTS
predictSeason <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::is(prior, "SeasonMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(predictSeason_R, prior)
    }
    else {
        K <- prior@K@.Data
        L <- prior@L@.Data
        along.all.struc.zero <- prior@alongAllStrucZero
        s <- prior@s@.Data # list length (K+1)L
        n.season <- prior@nSeason@.Data
        omega <- prior@omegaSeason@.Data
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            if (!along.all.struc.zero) {
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1L]
                    k.prev <- indices[i]
                    mean <- s[[k.prev]][n.season]
                    s[[k.curr]][1L] <- stats::rnorm(n = 1L,
                                                    mean = mean,
                                                    sd = omega)
                    for (j in seq.int(from = 2L, to = n.season))
                        s[[k.curr]][j] <- s[[k.prev]][j - 1L]
                }
            }
            iterator <- advanceA(iterator)
        }
        prior@s@.Data <- s
        prior
    }
}


## TRANSLATED
## HAS_TESTS
predictUBeta <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::validObject(prior))
    stopifnot(prior@isRobust@.Data)
    if (useC) {
        .Call(predictUBeta_R, prior)
    }
    else {
        J <- prior@J@.Data
        nu <- prior@nuBeta@.Data
        tau <- prior@tau@.Data
        U <- prior@UBeta@.Data
        allStrucZero <- prior@allStrucZero
        scaleSq <- tau^2
        for (j in seq_len(J)) {
            if (!allStrucZero[j])
                U[j] <- rinvchisq1(df = nu, scaleSq = scaleSq)
        }
        prior@UBeta@.Data <- U
        prior
    }
}



## redistributeNotSpecified <- function(y, notSpec, notSpecRedist, transform, model) {
##     for (i in seq_along(y)) {
##         i.not.spec <- dembase::getIBefore(i = i,
##                                             transform = transform,
##                                             useC = TRUE)
##         can.have.not.spec <- i.not.spec > 0L
##         if (can.have.not.spec) {
##             i.oth <- getIOther(i = i,
##                                transform = transform,
##                                useC = TRUE)
##             not.spec.redist.curr <- notSpecRedist[i]
##             not.spec.redist.curr.oth <- notSpecRedit[i.oth]
##             sum.not.spec.redist <- not.spec.redist.curr + not.spec.redist.curr.oth
##             if (sum.not.spec.redist == 0L)
##         }
##     }
## }    
              
    

    


## HAS_TESTS
splitFile <- function(filename, nChain, nIteration, lengthIter) {
    con.read <- file(filename, "rb")
    on.exit(close(con.read))
    n.row.file <- nIteration / nChain
    if (n.row.file != round(n.row.file))
        stop(gettextf("'%s' is not a multiple of '%s'",
                      "nIteration", "nChain"))
    size.results <- readBin(con = con.read, what = "integer", n = 1L)
    size.adustments <- readBin(con = con.read, what = "integer", n = 1L)
    results <- readBin(con = con.read, what = "raw", n = size.results)
    for (i in seq_len(nChain)) {
        tempfile <- paste(filename, i, sep = "_")
        con.write <- file(tempfile, "wb")
        for (j in seq_len(n.row.file)) {
            object <- readBin(con = con.read, what = "double", n = lengthIter)
            writeBin(object, con = con.write)
        }
        close(con.write)
    }
    paste(filename, seq_len(nChain), sep = "_")
}

## TRANSLATED
## HAS_TESTS
transferAlphaDelta0 <- function(state, values, offset, iteratorNew, iteratorOld,
                                useC = FALSE) {
    ## state
    stopifnot(is.double(state))
    stopifnot(!any(is.na(state)))
    ## values
    stopifnot(is.double(values))
    stopifnot(!any(is.na(values)))
    ## offset
    stopifnot(is.integer(offset))
    stopifnot(identical(length(offset), 1L))
    stopifnot(!is.na(offset))
    ## iteratorNew
    stopifnot(methods::is(iteratorNew, "AlongIterator"))
    ## iteratorOld
    stopifnot(methods::is(iteratorOld, "AlongIterator"))
    ## offset, values
    stopifnot(offset %in% seq_along(values))
    if (useC) {
        .Call(transferAlphaDelta0_R, state, values, offset, iteratorNew, iteratorOld)
    }
    else {    
        iterator.values <- resetA(iteratorOld)
        iterator.state <- resetA(iteratorNew)
        n <- iterator.values@nWithin * iterator.values@nBetween
        Kplus1.values <- length(iterator.values@indices)
        for (i in seq_len(n)) {
            indices.values <- iterator.values@indices
            indices.values <- indices.values + offset - 1L
            indices.state <- iterator.state@indices
            i.first.state <- indices.state[1L]
            i.last.values <- indices.values[Kplus1.values]
            state[i.first.state] <- values[i.last.values]
            iterator.values <- advanceA(iterator.values)
            iterator.state <- advanceA(iterator.state)
        }
        state
    }
}

## TRANSLATED
## HAS_TESTS
transferLevelComponentWeightOldMix <- function(values, offset, nAlongOld,
                                               indexClassMax, useC = FALSE) {
    ## values
    stopifnot(is.double(values))
    stopifnot(!any(is.na(values)))
    ## offset
    stopifnot(is.integer(offset))
    stopifnot(identical(length(offset), 1L))
    stopifnot(!is.na(offset))
    ## nAlongOld
    stopifnot(is.integer(nAlongOld))
    stopifnot(identical(length(nAlongOld), 1L))
    stopifnot(!is.na(nAlongOld))
    stopifnot(nAlongOld >= 1L)
    ## indexClassMax
    stopifnot(is.integer(indexClassMax))
    stopifnot(identical(length(indexClassMax), 1L))
    stopifnot(!is.na(indexClassMax))
    stopifnot(indexClassMax >= 2L)
    ## offset, values
    stopifnot(offset %in% seq_along(values))
    if (useC) {
        .Call(transferLevelComponentWeightOldMix_R,
              values, offset, nAlongOld, indexClassMax)
    }
    else {
        ans <- double(length = indexClassMax)
        for (i.class in seq_len(indexClassMax)) {
            i.wt <- nAlongOld * i.class
            i.values <- i.wt + offset - 1L
            ans[i.class] <- values[i.values]
        }
        ans
    }
}


## TRANSLATED
## HAS_TESTS
transferParamBetas <- function(model, filename, lengthIter, iteration,
                               useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "BetaIsPredicted"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamBetas_R, model, filename, lengthIter, iteration)
    }
    else {
        betas <- model@betas
        beta.is.predicted <- model@betaIsPredicted
        offsets.betas <- model@offsetsBetas
        n.beta <- length(betas)
        for (i in seq_len(n.beta)) {
            if (!beta.is.predicted[[i]]) {
                offsets <- offsets.betas[[i]]
                betas[[i]] <- getOneIterFromFile(filename = filename,
                                                 first = offsets[1L],
                                                 last = offsets[2L],
                                                 lengthIter = lengthIter,
                                                 iteration = iteration,
                                                 useC = FALSE)
            }
        }
        model@betas <- betas
        model
    }
}

## TRANSLATED
## HAS_TESTS
transferParamPriorsBetas <- function(model, filename, lengthIter, iteration,
                                     useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Betas"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamPriorsBetas_R, model, filename, lengthIter, iteration)
    }
    else {
        priors <- model@priorsBetas
        offsets.priors <- model@offsetsPriorsBetas
        is.predicted <- model@betaIsPredicted
        n.beta <- length(priors)
        for (i in seq_len(n.beta)) {
            if (is.predicted[i]) {
                offsets <- offsets.priors[[i]]
                if (!is.null(offsets)) {
                    first <- offsets[1L]
                    last <- offsets[2L]
                    values <- getOneIterFromFile(filename = filename,
                                                 first = first,
                                                 last = last,
                                                 lengthIter = lengthIter,
                                                 iteration = iteration)
                    priors[[i]] <- transferParamPrior(prior = priors[[i]],
                                                      values = values)
                }
            } ## NEW
        }
        model@priorsBetas <- priors
        model
    }
}

## TRANSLATED
## HAS_TESTS
transferParamSigma <- function(model, filename, lengthIter, iteration, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "SigmaMixin"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamSigma_R, model, filename, lengthIter, iteration)
    }
    else {
        offsets <- model@offsetsSigma
        first <- offsets[1L]
        last <- offsets[2L]
        sigma <- getOneIterFromFile(filename = filename,
                                    first = first,
                                    last = last,
                                    lengthIter = lengthIter,
                                    iteration = iteration)
        model@sigma@.Data <- sigma
        model
    }
}

## TRANSLATED
## HAS_TESTS
transferParamVarsigma <- function(model, filename, lengthIter, iteration, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "VarsigmaMixin"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamVarsigma_R, model, filename, lengthIter, iteration)
    }
    else {
        offsets <- model@offsetsVarsigma
        first <- offsets[1L]
        last <- offsets[2L]
        varsigma <- getOneIterFromFile(filename = filename,
                                       first = first,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iteration = iteration)
        model@varsigma@.Data <- varsigma
        model
    }
}

## TRANSLATED
## HAS_TESTS
transferSeason0 <- function(s, nSeason, values, offset, iteratorNew, iteratorOld,
                            useC = FALSE) {
    ## s
    stopifnot(is.list(s))
    stopifnot(all(sapply(s, is.double)))
    stopifnot(all(sapply(s, function(x) !any(is.na(x)))))
    ## nSeason
    stopifnot(is.integer(nSeason))
    stopifnot(identical(length(nSeason), 1L))
    ## values
    stopifnot(is.double(values))
    stopifnot(!any(is.na(values)))
    ## offset
    stopifnot(is.integer(offset))
    stopifnot(identical(length(offset), 1L))
    stopifnot(!is.na(offset))
    ## iteratorNew
    stopifnot(methods::is(iteratorNew, "AlongIterator"))
    ## iteratorOld
    stopifnot(methods::is(iteratorOld, "AlongIterator"))
    ## s, nSeason
    stopifnot(all(sapply(s, length) == nSeason))
    ## s, values
    stopifnot(length(s) < length(values))
    ## offset, values
    stopifnot(offset %in% seq_along(values))
    if (useC) {
        .Call(transferSeason0_R, s, nSeason, values, offset, iteratorNew, iteratorOld)
    }
    else {    
        iterator.values <- resetA(iteratorOld)
        iterator.s <- resetA(iteratorNew)
        n <- iterator.values@nWithin * iterator.values@nBetween
        Kplus1.values <- length(iterator.values@indices)
        for (i in seq_len(n)) {
            indices.values <- iterator.values@indices
            indices.s <- iterator.s@indices
            i.first.s <- indices.s[1L]
            i.last.values <- (indices.values[Kplus1.values] - 1L) * nSeason + offset
            for (j in seq_len(nSeason))
                s[[i.first.s]][j] <- values[i.last.values + j - 1L]
            iterator.values <- advanceA(iterator.values)
            iterator.s <- advanceA(iterator.s)
        }
        s
    }
}

