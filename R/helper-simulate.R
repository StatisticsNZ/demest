
## Helper functions to check that prior specifications hold all the
## information required to generate simulated data (and do not have arguments
## that are inappropriate for simulated data).


## NO_TESTS
checkDataModelsSuitableForSimulation <- function(dataModels, datasets,
                                                 namesDatasets) {
    error.message <- "data model for '%s' not suitable for simulation : %s"
    for (i in seq_along(dataModels)) {
        model <- dataModels[[i]]
        y <- datasets[[i]]
        name <- namesDatasets[i]
        val <- tryCatch(checkAllDimensionsHavePriors(model = model,
                                                     y = y),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
        val <- tryCatch(checkPriorsAreInformative(model),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
        val <- tryCatch(checkPriorSDInformative(model),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
    }
    NULL
}

## HAS_TESTS
checkPriorInform_prohibited <- function(object, nameSlot, nameArg, nameFun) {
    val <- methods::slot(object, nameSlot)
    val <- methods::slot(val, ".Data")
    if (!isTRUE(all.equal(val, 1)))
        gettextf("value for '%s' supplied in call to '%s'",
                 nameArg, nameFun)
    else
        NULL
}

## HAS_TESTS
checkPriorInform_required <- function(object, nameSlot, nameArg, nameFun) {
    val <- methods::slot(object, nameSlot)
    val <- methods::slot(val, ".Data")
    if (any(is.na(val)))
        gettextf("value for '%s' not supplied in call to '%s'",
                 nameArg, nameFun)
    else
        NULL
}

## HAS_TESTS
checkPriorInform_ExchFixed <- function(object) {
    value.mult.tau <- checkPriorInform_prohibited(object = object,
                                                  nameSlot = "multTau",
                                                  nameArg = "mult",
                                                  nameFun = "ExchFixed")
    value.tau <- checkPriorInform_required(object = object,
                                           nameSlot = "tau",
                                           nameArg = "sd",
                                           nameFun = "ExchFixed")
    for (value in list(value.mult.tau, value.tau))
        if (!is.null(value))
            return(value)
    NULL
}

## HAS_TESTS
checkPriorInform_Error <- function(object) {
    value.mult.tau <- checkPriorInform_prohibited(object = object,
                                                  nameSlot = "multTau",
                                                  nameArg = "mult",
                                                  nameFun = "HalfT")
    value.A.tau <- checkPriorInform_required(object = object,
                                             nameSlot = "ATau",
                                             nameArg = "scale",
                                             nameFun = "HalfT")
    for (value in list(value.mult.tau, value.A.tau))
        if (!is.null(value))
            return(gettextf("%s when specifying '%s'",
                            value, "error"))
    NULL
}

## HAS_TESTS
checkPriorInform_Covariates <- function(object) {
    value.A.eta.coef <- checkPriorInform_required(object = object,
                                                  nameSlot = "AEtaCoef",
                                                  nameArg = "scale",
                                                  nameFun = "TDist")
    for (value in list(value.A.eta.coef))
        if (!is.null(value))
            return(gettextf("%s when specifying '%s'",
                            value, "covariates"))
    NULL
}

## HAS_TESTS
checkPriorInform_Level <- function(object) {
    value.mult.alpha <- checkPriorInform_prohibited(object = object,
                                                    nameSlot = "multAlpha",
                                                    nameArg = "mult",
                                                    nameFun = "HalfT")
    value.A.alpha <- checkPriorInform_required(object = object,
                                               nameSlot = "AAlpha",
                                               nameArg = "scale",
                                               nameFun = "HalfT")
    for (value in list(value.mult.alpha, value.A.alpha))
        if (!is.null(value))
            return(gettextf("%s when specifying '%s'",
                            value, "level"))
    NULL
}

## HAS_TESTS
checkPriorInform_Trend <- function(object) {
    value.mult.delta.0 <- checkPriorInform_prohibited(object = object,
                                                      nameSlot = "multDelta0",
                                                      nameArg = "mult",
                                                      nameFun = "Initial")
    value.A.delta.0 <- checkPriorInform_required(object = object,
                                                 nameSlot = "ADelta0",
                                                 nameArg = "sd",
                                                 nameFun = "Initial")
    value.mult.delta <- checkPriorInform_prohibited(object = object,
                                                    nameSlot = "multDelta",
                                                    nameArg = "mult",
                                                    nameFun = "HalfT")
    value.A.delta <- checkPriorInform_required(object = object,
                                               nameSlot = "ADelta",
                                               nameArg = "scale",
                                               nameFun = "HalfT")
    for (value in list(value.mult.delta.0, value.A.delta.0,
                       value.mult.delta, value.A.delta))
        if (!is.null(value))
            return(gettextf("%s when specifying '%s'",
                            value, "trend"))
    NULL
}

## HAS_TESTS
checkPriorInform_Season <- function(object) {
    value.mult.season <- checkPriorInform_prohibited(object = object,
                                                      nameSlot = "multSeason",
                                                      nameArg = "mult",
                                                      nameFun = "HalfT")
    value.A.season <- checkPriorInform_required(object = object,
                                                nameSlot = "ASeason",
                                                nameArg = "scale",
                                                nameFun = "HalfT")
    for (value in list(value.mult.season, value.A.season))
        if (!is.null(value))
            return(gettextf("%s when specifying '%s'",
                            value, "season"))
    NULL
}

## HAS_TESTS
checkPriorInform_Components <- function(object) {
    value.mult.vectors <- checkPriorInform_prohibited(object = object,
                                                      nameSlot = "multVectorsMix",
                                                      nameArg = "mult",
                                                      nameFun = "HalfT")
    value.A.vectors <- checkPriorInform_required(object = object,
                                                 nameSlot = "AVectorsMix",
                                                 nameArg = "scale",
                                                 nameFun = "HalfT")
    for (value in list(value.mult.vectors, value.A.vectors))
        if (!is.null(value))
            return(gettextf("%s when specifying '%s'",
                            value, "components"))
    NULL
}

## HAS_TESTS
checkPriorInform_Weights <- function(object) {
    ## ComponentWeight
    value.mult.comp.wt <- checkPriorInform_prohibited(object = object,
                                                      nameSlot = "multComponentWeightMix",
                                                      nameArg = "mult",
                                                      nameFun = "HalfT")
    value.A.comp.wt <- checkPriorInform_required(object = object,
                                                 nameSlot = "AComponentWeightMix",
                                                 nameArg = "scale",
                                                 nameFun = "HalfT")
    for (value in list(value.mult.comp.wt, value.A.comp.wt))
        if (!is.null(value))
            return(gettextf("%s when specifying '%s' for '%s'",
                            value, "scale1", "weights"))
    ## LevelComponentWeight
    value.mult.level.comp.wt <- checkPriorInform_prohibited(object = object,
                                                            nameSlot = "multLevelComponentWeightMix",
                                                            nameArg = "mult",
                                                            nameFun = "HalfT")
    value.A.level.comp.wt <- checkPriorInform_required(object = object,
                                                       nameSlot = "ALevelComponentWeightMix",
                                                       nameArg = "scale",
                                                       nameFun = "HalfT")
    for (value in list(value.mult.level.comp.wt, value.A.level.comp.wt))
        if (!is.null(value))
            return(gettextf("%s when specifying '%s' for '%s'",
                            value, "scale2", "weights"))
    NULL
}

## HAS_TESTS
checkSimulatedExposure <- function(exposure) {
    if (!methods::is(exposure, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "exposure", class(exposure)))
    if (any(is.na(exposure)))
        stop(gettextf("'%s' has missing values",
                      "exposure"))
    if (any(exposure < 0))
        stop(gettextf("'%s' has negative values",
                      "exposure"))
    if (all(exposure == 0))
        stop(gettextf("'%s' has no non-zero values",
                      "exposure"))
    NULL
}

## HAS_TESTS
checkSimulatedYNoExposure <- function(y, model) {
    if (is.null(y))
        stop(gettextf("'%s' has class \"%s\", but '%s' and '%s' are both %s",
                      "model", class(model), "y", "exposure", "NULL"))
    if (!methods::is(y, "Counts"))
        stop(gettextf("'%s' has class \"%s\", but '%s' has class \"%s\"",
                      "model", class(model), "y", class(y)))
    NULL
}

## HAS_TESTS
checkSystemModelsSuitableForSimulation <- function(systemModels, account) {
    error.message <- "system model for '%s' not suitable for simulation : %s"
    population <- population(account)
    components <- components(account, simplify = FALSE)
    series <- c(list(population), components)
    names <- c("population", componentNames(account))
    for (i in seq_along(systemModels)) {
        model <- systemModels[[i]]
        y <- series[[i]]
        name <- names[i]
        val <- tryCatch(checkAllDimensionsHavePriors(model = model,
                                                     y = y),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
        val <- tryCatch(checkPriorsAreInformative(model),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
        val <- tryCatch(checkPriorSDInformative(model),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
    }
    NULL
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(drawBetas_R, object)
    }
    else {
        betas <- object@betas
        priors <- object@priorsBetas
        for (b in seq_along(betas)) {
            beta <- betas[[b]]
            prior <- priors[[b]]
            J <- prior@J@.Data
            all.struc.zero <- prior@allStrucZero
            beta.hat <- betaHat(prior)
            v <- getV(prior)
            for (j in seq_len(J)) {
                if (!all.struc.zero[j]) {
                    mean <- beta.hat[j]
                    sd <- sqrt(v[j])
                    beta[j] <- stats::rnorm(n = 1L,
                                            mean = mean,
                                            sd = sd)
                }
            }
            betas[[b]] <- beta
        }
        object@betas <- betas
        object
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawDataModelsAccount <- function(combined, useC = FALSE) {
    stopifnot(methods::validObject(combined))
    if (useC) {
        .Call(drawDataModelsAccount_R, combined)
    }
    else {
        data.models <- combined@dataModels
        datasets <- combined@datasets
        population <- combined@account@population
        components <- combined@account@components
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        for (i in seq_along(data.models)) {
            model <- data.models[[i]]
            dataset <- datasets[[i]]
            transform <- transforms[[i]]
            series.index <- series.indices[i]
            if (series.index == 0L)
                series <- population
            else
                series <- components[[series.index]]
            series.collapsed <- collapse(series, transform = transform)
            if (methods::is(model, "Poisson") || methods::is(model, "CMP"))
                series.collapsed <- toDouble(series.collapsed)
            model <- drawModelUseExp(model,
                                     y = dataset,
                                     exposure = series.collapsed)
            data.models[[i]] <- model
        }
        combined@dataModels <- data.models
        combined
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawDelta0 <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM") && methods::is(prior, "WithTrendMixin"))
    if (useC) {
        .Call(drawDelta0_R, prior)
    }
    else {
        L <- prior@L@.Data
        along.all.struc.zero <- prior@alongAllStrucZero
        delta <- prior@deltaDLM@.Data # numeric vector length (K+1)L
        A0 <- prior@ADelta0@.Data # scalar
        mean0 <- prior@meanDelta0@.Data # scalar
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            if (!along.all.struc.zero[l]) {
                indices <- iterator@indices
                i0 <- indices[1L]
                delta[i0] <- stats::rnorm(n = 1L,
                                          mean = mean0,
                                          sd = A0)
            }
            iterator <- advanceA(iterator)
        }
        prior@deltaDLM@.Data <- delta
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawEta <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawEta_R, prior)
    }
    else {
        eta <- prior@eta@.Data
        P <- prior@P@.Data
        U.eta.coef <- prior@UEtaCoef@.Data
        mean.eta.coef <- prior@meanEtaCoef@.Data
        eta[1L] <- 0
        for (p in seq_len(P - 1L)) {
            mean <- mean.eta.coef[p]
            sd <- sqrt(U.eta.coef[p])
            eta[p + 1L] <- stats::rnorm(n = 1L,
                                        mean = mean,
                                        sd = sd)
        }
        prior@eta@.Data <- eta
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaAlpha <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaAlpha_R, prior)
    }
    else {
        A <- prior@AAlpha@.Data
        nu <- prior@nuAlpha@.Data
        max <- prior@omegaAlphaMax@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaAlpha@.Data <- omega
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaComponentWeightMix <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaComponentWeightMix_R, prior)
    }
    else {
        A <- prior@AComponentWeightMix@.Data
        nu <- prior@nuComponentWeightMix@.Data
        max <- prior@omegaComponentWeightMaxMix@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaComponentWeightMix@.Data <- omega
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaDelta <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaDelta_R, prior)
    }
    else {
        A <- prior@ADelta@.Data
        nu <- prior@nuDelta@.Data
        max <- prior@omegaDeltaMax@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaDelta@.Data <- omega
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaLevelComponentWeightMix <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaLevelComponentWeightMix_R, prior)
    }
    else {
        A <- prior@ALevelComponentWeightMix@.Data
        nu <- prior@nuLevelComponentWeightMix@.Data
        max <- prior@omegaLevelComponentWeightMaxMix@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaLevelComponentWeightMix@.Data <- omega
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaSeason <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaSeason_R, prior)
    }
    else {
        A <- prior@ASeason@.Data
        nu <- prior@nuSeason@.Data
        max <- prior@omegaSeasonMax@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaSeason@.Data <- omega
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawOmegaVectorsMix <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawOmegaVectorsMix_R, prior)
    }
    else {
        A <- prior@AVectorsMix@.Data
        nu <- prior@nuVectorsMix@.Data
        max <- prior@omegaVectorsMaxMix@.Data
        omega <- rhalftTrunc1(df = nu,
                              scale = A,
                              max = max,
                              useC = TRUE)
        prior@omegaVectorsMix@.Data <- omega
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawPhi <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawPhi_R, prior)
    }
    else {
        phi.known <- prior@phiKnown@.Data
        if (phi.known)
            prior
        else {
            phi.min <- prior@minPhi@.Data
            phi.max <- prior@maxPhi@.Data
            shape1 <- prior@shape1Phi@.Data
            shape2 <- prior@shape2Phi@.Data
            X <- stats::rbeta(n = 1L,
                              shape1 = shape1,
                              shape2 = shape2)
            phi <- phi.min + X * (phi.max - phi.min)
            prior@phi <- phi
            prior
        }
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawPhiMix <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawPhiMix_R, prior)
    }
    else {
        phi.known <- prior@phiKnown@.Data
        if (phi.known)
            prior
        else {
            phi.min <- prior@minPhi@.Data
            phi.max <- prior@maxPhi@.Data
            shape1 <- prior@shape1Phi@.Data
            shape2 <- prior@shape2Phi@.Data
            X <- stats::rbeta(n = 1L,
                              shape1 = shape1,
                              shape2 = shape2)
            phi <- phi.min + X * (phi.max - phi.min)
            prior@phiMix <- phi
            prior
        }
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawPriors <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(drawPriors_R, object)
    }
    else {
        priors <- object@priorsBetas
        for (b in seq_along(priors))
            priors[[b]] <- drawPrior(priors[[b]])
        object@priorsBetas <- priors
        object
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawSigma_Varying <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(drawSigma_Varying_R, object)
    }
    else {
        max <- object@sigmaMax@.Data
        A <- object@ASigma@.Data
        nu <- object@nuSigma@.Data
        val <- rhalftTrunc1(df = nu,
                            scale = A,
                            max = max,
                            useC = TRUE)
        object@sigma@.Data <- val
        object
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawTau <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawTau_R, prior)
    }
    else {
        A <- prior@ATau@.Data
        nu <- prior@nuTau@.Data
        max <- prior@tauMax@.Data
        tau <- rhalftTrunc1(df = nu,
                            scale = A,
                            max = max,
                            useC = TRUE)
        prior@tau@.Data <- tau
        prior
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
drawUEtaCoef <- function(prior, useC = FALSE) {
    methods::validObject(prior)
    if (useC) {
        .Call(drawUEtaCoef_R, prior)
    }
    else {
        U <- prior@UEtaCoef@.Data # vector length P-1
        P <- prior@P@.Data # scalar
        A <- prior@AEtaCoef@.Data # vector length P-1
        nu <- prior@nuEtaCoef@.Data # vector length P-1
        for (p in seq_len(P - 1L))
            U[p] <- rinvchisq1(df = nu[p], scaleSq = (A[p])^2)
        prior@UEtaCoef@.Data <- U
        prior
    }
}

## HAS_TESTS
makeCountsY <- function(exposure) {
    ans <- exposure
    size <- exposure@.Data
    n <- length(size)
    ans@.Data[] <- rbinom(n = n,
                          size = size,
                          prob = 0.5)
    ans
}

## HAS_TESTS
setYToMissing <- function(y) {
    is.integer <- is.integer(y@.Data)
    y@.Data[] <- if (is.integer) NA_integer_ else as.numeric(NA)
    y
}


## HAS_TESTS
simulateDirect <- function(combined, tempfile, nDraw, useC) {
    con <- file(tempfile, open = "wb")
    on.exit(close(con))
    for (i in seq_len(nDraw)) {
        combined <- drawCombined(combined,
                                 nUpdate = 1L,
                                 useC = useC)
        values <- extractValues(combined)
        writeBin(values, con = con)
    }
    combined
}



## HAS_TESTS
simulateOneChain <- function(combined, seed, tempfile, nBurnin, nSim, nThin,
                             nUpdateMax, continuing, useC, ...) {
    ## set seed if continuing
    if (!is.null(seed))
        assign(".Random.seed", seed, envir = .GlobalEnv)
    ## burnin
    nLoops <- nBurnin %/% nUpdateMax
    for (i in seq_len(nLoops)) {
        combined <- drawCombined(combined,
                                 nUpdate = nUpdateMax,
                                 useC = useC)
    }
    ## and any final ones
    nLeftOver <- nBurnin - nLoops * nUpdateMax
    combined <- drawCombined(combined,
                             nUpdate = nLeftOver,
                             useC = useC)
    ## production
    con <- file(tempfile, open = "wb")
    n.prod <- nSim %/% nThin
    for (i in seq_len(n.prod)) {
        nLoops <- nThin %/% nUpdateMax
        for (i in seq_len(nLoops)) {
            combined <- drawCombined(combined,
                                     nUpdate = nUpdateMax,
                                     useC = useC)
        }
        ## and any final ones
        nLeftOver <- nThin - nLoops * nUpdateMax
        combined <- drawCombined(combined,
                                 nUpdate = nLeftOver,
                                 useC = useC)
        values <- extractValues(combined)
        writeBin(values, con = con)
    }
    close(con)
    ## return final state
    combined
}


## HAS_TESTS
warnSimulateModelIgnoresArg <- function(arg, argname, model) {
    if (!is.null(arg))
        warning(gettextf("function '%s' ignores '%s' argument when '%s' argument has class \"%s\"",
                         "simulateModel", argname, "model", class(model)))
    NULL
}

## HAS_TESTS
warnSimulateModelExposureAndYSupplied <- function(y, model) {
    if (!is.null(y))
        warning(gettextf("function '%s' ignores '%s' argument when '%s' argument has class \"%s\" and '%s' argument is supplied",
                         "simulateModel", "y", "model", class(model), "exposure"))
    NULL
}




