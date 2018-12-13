

## FAKE ######################################################################

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

