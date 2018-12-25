
## HAS_TESTS
checkAllDimensionsHavePriors <- function(model, y) {
    if (methods::is(model, "SpecVarying")) {
        name.response <- model@nameY@.Data
        names.specs <- model@namesSpecsPriors
        names.y <- names(y)
        for (name in names.y) {
            if (!(name %in% names.specs))
                stop(gettextf("no prior specified for \"%s\" dimension in model for '%s'",
                              name, name.response))
        }
        if (!("(Intercept)" %in% names.specs))
            stop(gettextf("no prior specified for intercept in model for '%s'",
                          name.response))
    }
    NULL
}


## checkPriorInform ########################################################

## Helper functions to check that prior specifications hold all the
## information required to generate fake data (and do not have arguments
## that are inappropriate for fake data).


## HAS_TESTS
checkPriorInform_prohibited <- function(object, nameSlot, nameArg, nameFun) {
    val <- slot(object, nameSlot)
    val <- slot(val, ".Data")
    if (!isTRUE(all.equal(val, 1)))
        gettextf("value for '%s' supplied in call to '%s'",
                 nameArg, nameFun)
    else
        NULL
}

## HAS_TESTS
checkPriorInform_required <- function(object, nameSlot, nameArg, nameFun) {
    val <- slot(object, nameSlot)
    val <- slot(val, ".Data")
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
    value.A.eta.intercept <- checkPriorInform_required(object = object,
                                                       nameSlot = "AEtaIntercept",
                                                       nameArg = "sd",
                                                       nameFun = "Norm")
    value.A.eta.coef <- checkPriorInform_required(object = object,
                                                  nameSlot = "AEtaCoef",
                                                  nameArg = "scale",
                                                  nameFun = "TDist")
    for (value in list(value.A.eta.intercept, value.A.eta.coef))
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
checkPriorsAreInformative <- function(object) {
    if (methods::is(object, "SpecVarying")) {
        name.model <- object@nameY[[1L]]
        specs.priors <- object@specsPriors
        names.specs.priors <- object@namesSpecsPriors
        for (i in seq_along(specs.priors)) {
            value <- checkPriorIsInformative(specs.priors[[i]])
            if (!is.null(value))
                stop(gettextf("problem with prior for '%s' in model for '%s' : %s",
                              names.specs.priors[i], name.model, value))
        }
    }
    NULL
}

## HAS_TESTS
checkPriorSDInformative <- function(object) {
    if (methods::is(object, "SpecVarying")) {
        name.model <- object@nameY[[1L]]
        if (methods::.hasSlot(object, "multSigma"))
            value.mult.sigma <- checkPriorInform_prohibited(object = object,
                                                            nameSlot = "multSigma",
                                                            nameArg = "mult",
                                                            nameFun = "HalfT")
        else
            value.mult.sigma <- NULL
        ## no need to allow for partial matching, since 'priorSD'
        ## argument follows '...'
        i.prior.sd <- match("priorSD", names(object@call), nomatch = 0L)
        specified.prior.sd <- i.prior.sd > 0L
        if (specified.prior.sd) {
            spec.prior.sd <- object@call[[i.prior.sd]]
            ## allow for partial matching
            i.scale.prior.sd <- pmatch(names(spec.prior.sd), "scale", nomatch = 0L)
            if (any(i.scale.prior.sd > 0L))
                value.A.sigma <- NULL
            else
                value.A.sigma <- gettextf("'%s' argument not supplied in call to '%s'",
                                          "scale", "HalfT")
        }
        else
            value.A.sigma <- gettextf("'%s' argument not supplied in call to '%s'",
                                      "priorSD", "Model")
        for (value in list(value.mult.sigma, value.A.sigma))
            if (!is.null(value))
                stop(gettextf("problem with specification of '%s' in model for '%s' : %s",
                              "priorSD", name.model, value))
    }
    NULL
}    




### OLD FUNCTIONS ######################################################

## HAS_TESTS
initialFakeDLMAll <- function(spec, metadata) {
    along <- spec@along
    ATau <- spec@ATau
    nuTau <- spec@nuTau
    tauMax <- spec@tauMax
    AAlpha <- spec@AAlpha
    nuAlpha <- spec@nuAlpha
    omegaAlphaMax <- spec@omegaAlphaMax
    phi <- spec@phi
    phiKnown <- spec@phiKnown@.Data
    minPhi <- spec@minPhi
    maxPhi <- spec@maxPhi
    shape1Phi <- spec@shape1Phi
    shape2Phi <- spec@shape2Phi
    l <- makeFakeScale(A = ATau,
                       nu = nuTau,
                       scaleMax = tauMax,
                       functionName = "Error")
    tau <- l$scale
    ATau <- l$A
    tauMax <- l$scaleMax
    l <- makeFakeScale(A = AAlpha,
                       nu = nuAlpha,
                       scaleMax = omegaAlphaMax,
                       functionName = "Level")
    omegaAlpha <- l$scale
    AAlpha <- l$A
    omegaAlphaMax <- l$scaleMax
    phi <- makeFakePhi(phi = phi,
                       phiKnown = phiKnown,
                       min = minPhi,
                       max = maxPhi,
                       shape1 = shape1Phi@.Data,
                       shape2 <- shape2Phi@.Data)
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    dim <- dim(metadata)
    J <- makeJPredict(metadata)
    K <- makeK(dim = dim, iAlong = iAlong)
    L <- makeL(dim = dim, iAlong = iAlong)
    dim.alpha.delta <- dim
    dim.alpha.delta[iAlong] <- dim.alpha.delta[iAlong] + 1L
    iteratorState <- AlongIterator(dim = dim.alpha.delta,
                                   iAlong = iAlong)
    iteratorV <- AlongIterator(dim = dim,
                               iAlong = iAlong)
    alphaDLM <- makeStateDLM(K = K,
                             L = L)
    list(AAlpha = AAlpha,
         alphaDLM = alphaDLM,
         ATau = ATau,
         iAlong = iAlong,
         iteratorState = iteratorState,
         iteratorV = iteratorV,
         J = J,
         K = K,
         L = L,
         minPhi = minPhi,
         maxPhi = maxPhi,
         nuAlpha = nuAlpha,
         nuTau = nuTau,
         omegaAlpha = omegaAlpha,
         omegaAlphaMax = omegaAlphaMax,
         phi = phi,
         shape1Phi = shape1Phi,
         shape2Phi = shape2Phi,
         tau = tau,
         tauMax = tauMax)
}

## HAS_TESTS
initialFakeDLMWithTrend <- function(spec, metadata) {
    meanDelta0 <- spec@meanDelta0
    ADelta0 <- spec@ADelta0@.Data
    nuDelta <- spec@nuDelta
    omegaDeltaMax <- spec@omegaDeltaMax
    ADelta <- spec@ADelta
    hasLevel <- spec@hasLevel
    along <- spec@along
    if (is.na(ADelta0))
        stop(gettextf("need to specify '%s' in call to function '%s'",
                      "sd", "Initial"))
    ADelta0 <- methods::new("Scale", ADelta0)
    l <- makeFakeScale(A = ADelta,
                       nu = nuDelta,
                       scaleMax = omegaDeltaMax,
                       functionName = "Level")
    omegaDelta <- l$scale
    ADelta <- l$A
    omegaDeltaMax <- l$scaleMax
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    dim <- dim(metadata)
    K <- makeK(dim = dim,
               iAlong = iAlong)
    L <- makeL(dim = dim,
               iAlong = iAlong)
    deltaDLM <- makeStateDLM(K = K,
                             L = L)
    list(ADelta = ADelta,
         deltaDLM = deltaDLM,
         nuDelta = nuDelta,
         omegaDelta = omegaDelta,
         omegaDeltaMax = omegaDeltaMax,
         ADelta0 = ADelta0,
         meanDelta0 = meanDelta0,
         hasLevel = hasLevel)
}

## HAS_TESTS
makeFakeHyper <- function(priors, margins, metadata, names) {
    ans <- vector(mode = "list", length = length(priors))
    for (i in seq_along(priors)) {
        prior <- priors[[i]]
        margin <- margins[[i]]
        is.intercept <- identical(margin, 0L)
        metadata.i <- if (is.intercept) NULL else metadata[margin]
        ans[[i]] <- makeFakeOutputPrior(prior,
                                        metadata = metadata.i)
    }
    names(ans) <- names
    ans
}

## HAS_TESTS
makeFakeMargins <- function(namesSpecs, y, call) {
    names.y <- names(dimnames(y))
    if (!identical(namesSpecs[1L], "(Intercept)"))
        stop(gettextf("no prior specified for '%s' in model '%s'",
                      "(Intercept)", deparse(call[[2L]])))
    namesSpecs <- namesSpecs[-1L]
    s <- seq_along(names.y)
    possible.margins <- sapply(s, function(i) combn(s, i, simplify = FALSE))
    possible.margins <- unlist(possible.margins, recursive = FALSE)
    possible.names <- sapply(possible.margins, function(i) paste(names.y[i], collapse = ":"))
    i.found <- sapply(namesSpecs, match, possible.names, nomatch = 0L)
    not.found <- i.found == 0L
    if (any(not.found)) {
        stop(gettextf("term '%s' from formula '%s' not found in dimensions of '%s'",
                      namesSpecs[not.found][1L], deparse(call[[2L]]), "templateY"))
    }
    ans <- possible.margins[i.found]
    ans <- c(list(0L), ans)
    ans
}

## HAS_TESTS
makeFakeOutputLevelDLM <- function(prior, metadata) {
    i.along <- prior@iAlong
    alpha <- prior@alphaDLM@.Data
    dim <- dim(metadata)
    dim.alpha <- replace(dim,
                         list = i.along,
                         values = dim[i.along] + 1L)
    .Data <- array(alpha,
                   dim = dim.alpha)
    is.alpha0 <- slice.index(.Data, MARGIN = i.along) == 1L
    .Data <- .Data[!is.alpha0]
    .Data <- array(.Data,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    new("Values",
        .Data = .Data,
        metadata = metadata)
}

## HAS_TESTS
makeFakeOutputTrendDLM <- function(prior, metadata) {
    i.along <- prior@iAlong
    delta <- prior@deltaDLM@.Data
    dim <- dim(metadata)
    dim.delta <- replace(dim,
                         list = i.along,
                         values = dim[i.along] + 1L)
    .Data <- array(delta,
                   dim = dim.delta)
    is.delta0 <- slice.index(.Data, MARGIN = i.along) == 1L
    .Data <- .Data[!is.delta0]
    .Data <- array(.Data,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    new("Values",
        .Data = .Data,
        metadata = metadata)
}

## HAS_TESTS
makeFakePhi <- function(phi, phiKnown, min, max, shape1, shape2) {
    if (!phiKnown) {
        phi.transformed <- stats::rbeta(n = 1L,
                                        shape1 = shape1,
                                        shape2 = shape2)
        phi <- phi.transformed * (max - min) + min
    }
    phi
}

## HAS_TESTS
makeFakePriors <- function(specs, margins, metadata, isSaturated) {
    ans <- vector(mode = "list", length = length(specs))
    for (i in seq_along(specs)) {
        spec <- specs[[i]]
        margin <- margins[[i]]
        is.intercept <- identical(margin, 0L)
        metadata.i <- if (is.intercept) NULL else metadata[margin]
        is.saturated.i <- isSaturated[i]
        ans[[i]] <- fakePrior(spec,
                              metadata = metadata.i,
                              isSaturated = is.saturated.i)
    }
    ans
}

## HAS_TESTS
makeFakeScale <- function(A, nu, scaleMax, functionName, scaleName = "scale") {
    if (is.na(A))
        stop(gettextf("need to specify scale of half-t distribution for '%s' in call to function '%s'",
                      "scale", functionName))
    scaleMax <- makeScaleMax(scaleMax = scaleMax,
                             A = A,
                             nu = nu,
                             isSpec = FALSE)
    scale <- tryCatch(rhalftTrunc1(df = nu@.Data,
                                   scale = A@.Data,
                                   max = scaleMax@.Data,
                                   useC = TRUE),
                      error = function(e) e)
    if (methods::is(scale, "error"))
        stop(gettextf("problem generating scale parameter within function '%s' : %s",
                      functionName, scale$message))
    scale <- methods::new("Scale", scale)
    A <- methods::new("Scale", A@.Data)
    list(scale = scale,
         A = A,
         scaleMax = scaleMax)
}

