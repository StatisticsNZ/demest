


## HAS_TESTS            
initialCov <- function(object, beta, metadata, sY, allStrucZero) {
    AEtaCoef <- object@AEtaCoef
    contrastsArg <- object@contrastsArg
    data <- object@data
    formula <- object@formula
    infant <- object@infant
    meanEtaCoef <- object@meanEtaCoef
    multEtaCoef <- object@multEtaCoef
    nuEtaCoef <- object@nuEtaCoef
    AEtaIntercept <- makeAIntercept(sY)
    Z <- makeZ(formula = formula[-2L],
               data = data,
               metadata = metadata,
               contrastsArg = contrastsArg,
               infant = infant,
               allStrucZero = allStrucZero)
    P <- makeP(Z)
    n <- length(AEtaCoef@.Data)
    P_minus_1 <- P@.Data - 1L
    if (n == 1L) {
        nuEtaCoef@.Data <- rep_len(nuEtaCoef@.Data, length.out = P_minus_1)
        meanEtaCoef@.Data <- rep_len(meanEtaCoef@.Data, length.out = P_minus_1)
        multEtaCoef@.Data <- rep_len(multEtaCoef@.Data, length.out = P_minus_1)
        AEtaCoef@.Data <- rep_len(AEtaCoef@.Data, length.out = P_minus_1)
    }
    else {
        if (n != P_minus_1) {
            stop(gettextf("'%s', '%s', and '%s' in prior for coefficients have length %d, but data matrix (minus intercept) has %d columns",
                          "df", "mean", "scale", n, P@.Data))
        }
    }
    AEtaCoef <- makeAHalfTVec(A = AEtaCoef,
                              metadata = metadata,
                              sY = sY,
                              mult = multEtaCoef)
    UEtaCoef <- makeUEtaCoef(nu = nuEtaCoef,
                             A = AEtaCoef,
                             n = P_minus_1)
    eta <- makeEta(beta = beta,
                   UEtaCoef = UEtaCoef)
    list(AEtaCoef = AEtaCoef,
         AEtaIntercept = AEtaIntercept,
         contrastsArg = contrastsArg,
         eta = eta,
         formula = formula,
         infant = infant,
         meanEtaCoef = meanEtaCoef,
         nuEtaCoef = nuEtaCoef,
         P = P,
         UEtaCoef = UEtaCoef,
         Z = Z)
}

## HAS_TESTS            
initialCovPredict <- function(prior, data, metadata, allStrucZero) {
    formula <- prior@formula
    contrastsArg <- prior@contrastsArg
    infant <- prior@infant
    Z <- makeZ(formula = formula[-2L],
               data = data,
               metadata = metadata,
               contrastsArg = contrastsArg,
               infant = infant,
               allStrucZero = allStrucZero)
    list(Z = Z)
}

## HAS_TESTS
initialDLMAll <- function(object, beta, metadata, sY,
                          isSaturated, margin, strucZeroArray, ...) {
    AAlpha <- object@AAlpha
    ATau <- object@ATau
    along <- object@along
    multAlpha <- object@multAlpha
    multTau <- object@multTau
    nuAlpha <- object@nuAlpha
    nuTau <- object@nuTau
    omegaAlphaMax <- object@omegaAlphaMax
    phi <- object@phi
    phiKnown <- object@phiKnown
    minPhi <- object@minPhi
    maxPhi <- object@maxPhi
    shape1Phi <- object@shape1Phi
    shape2Phi <- object@shape2Phi
    tauMax <- object@tauMax
    has.trend <- methods::is(object, "SpecWithTrendMixin")
    dim <- dim(metadata)
    J <- makeJ(beta)
    ATau <- makeAHalfT(A = ATau,
                       metadata = metadata,
                       sY = sY,
                       mult = multTau)
    tauMax <- makeScaleMax(scaleMax = tauMax,
                           A = ATau,
                           nu = nuTau)
    tau <- makeScale(A = ATau,
                     nu = nuTau,
                     scaleMax = tauMax)
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    K <- makeK(dim = dim, iAlong = iAlong)
    L <- makeL(dim = dim, iAlong = iAlong)
    dim.alpha.delta <- dim
    dim.alpha.delta[iAlong] <- dim.alpha.delta[iAlong] + 1L
    iteratorState <- AlongIterator(dim = dim.alpha.delta,
                                   iAlong = iAlong)
    iteratorV <- AlongIterator(dim = dim,
                               iAlong = iAlong)
    AAlpha <- makeAHalfT(A = AAlpha,
                         metadata = metadata,
                         sY = sY,
                         mult = multAlpha)
    omegaAlphaMax <- makeScaleMax(scaleMax = omegaAlphaMax,
                                  A = AAlpha,
                                  nu = nuAlpha)
    if (has.trend && !object@hasLevel)
        omegaAlpha <- methods::new("Scale", 0)
    else
        omegaAlpha <- makeScale(A = AAlpha,
                                nu = nuAlpha,
                                scaleMax = omegaAlphaMax)
    alphaDLM <- makeStateDLM(K = K,
                             L = L)
    phi <- makePhi(phi = phi,
                   phiKnown = phiKnown,
                   minPhi = minPhi,
                   maxPhi = maxPhi)
    allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                     margin = margin,
                                     metadata = metadata)
    alongAllStrucZero <- makeAlongAllStrucZero(strucZeroArray = strucZeroArray,
                                               metadata = metadata,
                                               margin = margin,
                                               iAlong = iAlong)
    isSaturated <- methods::new("LogicalFlag", isSaturated)
    list(AAlpha = AAlpha,
         ATau = ATau,
         alphaDLM = alphaDLM,
         allStrucZero = allStrucZero,
         alongAllStrucZero = alongAllStrucZero,
         iAlong = iAlong,
         isSaturated = isSaturated,
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
         phiKnown = phiKnown,
         shape1Phi = shape1Phi,
         shape2Phi = shape2Phi,
         tau = tau,
         tauMax = tauMax)
}

## HAS_TESTS
initialDLMAllPredict <- function(prior, metadata, name, along, margin, strucZeroArray) {
    alpha.old <- prior@alphaDLM
    J.old <- prior@J
    K.old <- prior@K@.Data
    iterator.state.old <- prior@iteratorState
    i.along.old <- prior@iAlong
    J <- makeJPredict(metadata)
    i.along.new <- dembase::checkAndTidyAlong(along = along,
                                              metadata = metadata,
                                              numericDimScales = TRUE,
                                              checkNumericDimscales = FALSE)
    if (!identical(i.along.new, i.along.old))
        stop(gettextf("\"%s\" dimension of prediction does not match \"%s\" dimension of prior for '%s'",
                      "along", "along", name))
    dim <- dim(metadata)
    K.new <- makeK(dim = dim, iAlong = i.along.new)
    L <- makeL(dim = dim, iAlong = i.along.new)
    dim.alpha.delta <- dim
    dim.alpha.delta[i.along.new] <- dim.alpha.delta[i.along.new] + 1L
    iterator.state.new <- AlongIterator(dim = dim.alpha.delta,
                                        iAlong = i.along.new)
    iterator.v <- AlongIterator(dim = dim,
                                iAlong = i.along.new)
    alpha.new <- makeStateDLM(K = K.new,
                              L = L)
    allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                     margin = margin,
                                     metadata = metadata)
    alongAllStrucZero <- makeAlongAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata,
                                               iAlong = i.along.new)
    list(alphaDLM = alpha.new,
         allStrucZero = allStrucZero,
         alongAllStrucZero = alongAllStrucZero,
         iteratorState = iterator.state.new,
         iteratorStateOld = iterator.state.old,
         iteratorV = iterator.v,
         J = J,
         JOld = J.old,
         K = K.new,
         L = L)
}

## HAS_TESTS
initialDLMNoTrend <- function(object, metadata, sY) {
    along <- object@along
    phi <- object@phi
    phiKnown <- object@phiKnown@.Data
    dim <- dim(metadata)
    if (is.na(along))
        along <- NULL
    i.along <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    K <- makeK(dim = dim, iAlong = i.along)
    L <- makeL(dim = dim, iAlong = i.along)
    mNoTrend <- makeMNoTrend(K = K)
    m0NoTrend <- makeM0NoTrend(L = L)
    CNoTrend <- makeCNoTrend(K = K,
                             sY = sY,
                             phi = phi,
                             phiKnown = phiKnown)
    aNoTrend <- makeANoTrend(K = K)
    RNoTrend <- makeRNoTrend(K = K)
    list(aNoTrend = aNoTrend,
         CNoTrend = CNoTrend,
         mNoTrend = mNoTrend,
         m0NoTrend = m0NoTrend,
         RNoTrend = RNoTrend)
}

## HAS_TESTS
initialDLMNoTrendPredict <- function(prior, metadata) {
    alpha.old <- prior@alphaDLM@.Data
    iterator.old <- prior@iteratorState
    i.along <- prior@iAlong
    dim <- dim(metadata)
    K <- makeK(dim = dim, iAlong = i.along)
    L <- makeL(dim = dim, iAlong = i.along)
    mNoTrend <- makeMNoTrend(K = K)
    m0NoTrend <- makeM0NoTrend(L = L)
    CNoTrend <- makeCNoTrend(K = K, C0 = 0)
    aNoTrend <- makeANoTrend(K = K)
    RNoTrend <- makeRNoTrend(K = K)
    iterator.new <- AlongIterator(dim = dim,
                                  iAlong = i.along)
    list(aNoTrend = aNoTrend,
         CNoTrend = CNoTrend,
         mNoTrend = mNoTrend,
         m0NoTrend = m0NoTrend,
         RNoTrend = RNoTrend)
}

## HAS_TESTS
initialDLMWithTrend <- function(object, beta, metadata, sY, lAll) {
    ADelta <- object@ADelta
    ADelta0 <- object@ADelta0
    meanDelta0 <- object@meanDelta0
    along <- object@along
    hasLevel <- object@hasLevel
    multDelta <- object@multDelta
    multDelta0 <- object@multDelta0
    nuDelta <- object@nuDelta
    omegaDeltaMax <- object@omegaDeltaMax
    dim <- dim(metadata)
    J <- makeJ(beta)
    ADelta <- makeAHalfT(A = ADelta,
                         metadata = metadata,
                         sY = sY,
                         mult = multDelta)
    ADelta0 <- makeAHalfT(A = ADelta0,
                          metadata = metadata,
                          sY = sY,
                          mult = multDelta0)
    omegaDeltaMax <- makeScaleMax(scaleMax = omegaDeltaMax,
                                  A = ADelta,
                                  nu = nuDelta)        
    omegaDelta <- makeScale(A = ADelta,
                            nu = nuDelta,
                            scaleMax = omegaDeltaMax)
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    K <- makeK(dim = dim,
               iAlong = iAlong)
    L <- makeL(dim = dim,
               iAlong = iAlong)
    deltaDLM <- makeStateDLM(K = K,
                             L = L)
    mWithTrend <- makeMWithTrend(K = K)
    m0WithTrend <- makeM0WithTrend(L = L,
                                   meanDelta0 = meanDelta0)
    CWithTrend <- makeCWithTrend(K = K,
                                 sY = sY,
                                 ADelta0 = ADelta0,
                                 hasLevel = hasLevel)
    aWithTrend <- makeAWithTrend(K = K)
    RWithTrend <- makeRWithTrend(K = K)
    UC <- makeUC(K)
    DC <- makeDC(CWithTrend = CWithTrend)
    DCInv <- makeDCInv(DC)
    UR <- makeUR(K)
    DRInv <- makeDRInv(K)
    omegaAlpha <- lAll$omegaAlpha
    phi <- lAll$phi
    WSqrt <- makeWSqrt(omegaAlpha = omegaAlpha,
                       omegaDelta = omegaDelta)
    WSqrtInvG <- makeWSqrtInvG(omegaAlpha = omegaAlpha,
                               omegaDelta = omegaDelta,
                               phi = phi)
    GWithTrend <- makeGWithTrend(phi = phi)
    list(ADelta = ADelta,
         ADelta0 = ADelta0,
         aWithTrend = aWithTrend,
         CWithTrend = CWithTrend,
         DC = DC,
         DCInv = DCInv,
         DRInv = DRInv,
         deltaDLM = deltaDLM,
         GWithTrend = GWithTrend,
         hasLevel = hasLevel,
         mWithTrend = mWithTrend,
         m0WithTrend = m0WithTrend,
         meanDelta0 = meanDelta0,
         nuDelta = nuDelta,
         omegaDelta = omegaDelta,
         omegaDeltaMax = omegaDeltaMax,
         RWithTrend = RWithTrend,
         UC = UC,
         UR = UR,
         WSqrt = WSqrt,
         WSqrtInvG = WSqrtInvG)
}

## HAS_TESTS
initialDLMWithTrendPredict <- function(prior, metadata) {
    alpha.old <- prior@alphaDLM@.Data
    delta.old <- prior@deltaDLM@.Data
    iterator.old <- prior@iteratorState
    i.along <- prior@iAlong
    dim <- dim(metadata)
    K <- makeK(dim = dim, iAlong = i.along)
    L <- makeL(dim = dim, iAlong = i.along)
    deltaDLM <- makeStateDLM(K = K,
                             L = L)
    mWithTrend <- makeMWithTrend(K = K)
    m0WithTrend <- makeM0WithTrend(L = L)
    C0 <- matrix(0, nrow = 2, ncol = 2)
    CWithTrend <- makeCWithTrend(K = K, C0 = C0)
    aWithTrend <- makeAWithTrend(K = K)
    RWithTrend <- makeRWithTrend(K = K)
    UC <- makeUC(K)
    DC <- makeDC(CWithTrend = CWithTrend)
    DCInv <- makeDCInv(DC)
    UR <- makeUR(K)
    DRInv <- makeDRInv(K)
    iterator.new <- AlongIterator(dim = dim,
                                  iAlong = i.along)
    vecOld <- lapply(seq_along(alpha.old), function(i) c(alpha.old[i], delta.old[i]))
    list(aWithTrend = aWithTrend,
         CWithTrend = CWithTrend,
         DC = DC,
         DCInv = DCInv,
         DRInv = DRInv,
         deltaDLM = deltaDLM,
         mWithTrend = mWithTrend,
         m0WithTrend = m0WithTrend,
         RWithTrend = RWithTrend,
         UC = UC,
         UR = UR)
}

## HAS_TESTS
initialDLMSeason <- function(object, beta, metadata, sY) {
    along <- object@along
    ASeason <- object@ASeason
    multSeason <- object@multSeason
    nSeason <- object@nSeason
    nuSeason <- object@nuSeason
    omegaSeasonMax <- object@omegaSeasonMax
    dim <- dim(metadata)
    J <- makeJ(beta)
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    K <- makeK(dim = dim, iAlong = iAlong)
    L <- makeL(dim = dim, iAlong = iAlong)
    ASeason <- makeAHalfT(A = ASeason,
                          metadata = metadata,
                          sY = sY,
                          mult = multSeason)
    omegaSeasonMax <- makeScaleMax(scaleMax = omegaSeasonMax,
                                   A = ASeason,
                                   nu = nuSeason)        
    omegaSeason <- makeScale(A = ASeason,
                             nu = nuSeason,
                             scaleMax = omegaSeasonMax)
    mSeason <- makeMSeason(K = K, nSeason = nSeason)
    m0Season <- makeM0Season(L = L, nSeason = nSeason)
    CSeason <- makeCSeason(K = K, nSeason = nSeason, ASeason = ASeason)
    aSeason <- makeASeason(K = K, nSeason = nSeason)
    RSeason <- makeRSeason(K = K, nSeason = nSeason)
    s <- makeSeasonDLM(K = K,
                       L = L,
                       nSeason = nSeason)
    list(ASeason = ASeason,
         aSeason = aSeason,
         CSeason = CSeason,
         mSeason = mSeason,
         m0Season = m0Season,
         nSeason = nSeason,
         nuSeason = nuSeason,
         omegaSeason = omegaSeason,
         omegaSeasonMax = omegaSeasonMax,
         RSeason = RSeason,
         s = s)
}

## HAS_TESTS
initialDLMSeasonPredict <- function(prior, metadata) {
    s.old <- prior@s@.Data
    iterator.old <- prior@iteratorState
    i.along <- prior@iAlong
    n.season <- prior@nSeason
    dim <- dim(metadata)
    K <- makeK(dim = dim, iAlong = i.along)
    L <- makeL(dim = dim, iAlong = i.along)
    mSeason <- makeMSeason(K = K, nSeason = n.season)
    m0Season <- makeM0Season(L = L, nSeason = n.season)
    C0 <- rep(0, times = n.season)
    CSeason <- makeCSeason(K = K, nSeason = n.season, C0 = C0)
    aSeason <- makeASeason(K = K, nSeason = n.season)
    RSeason <- makeRSeason(K = K, nSeason = n.season)
    s.new <- makeSeasonDLM(K = K,
                           L = L,
                           nSeason = n.season)
    iterator.new <- AlongIterator(dim = dim,
                                  iAlong = i.along)
    list(aSeason = aSeason,
         CSeason = CSeason,
         mSeason = mSeason,
         m0Season = m0Season,
         RSeason = RSeason,
         s = s.new)
}

## HAS_TESTS
initialMixAll <- function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
    AComponentWeightMix <- object@AComponentWeightMix
    ALevelComponentWeightMix <- object@ALevelComponentWeightMix
    ATau <- object@ATau
    AVectorsMix <- object@AVectorsMix
    along <- object@along
    indexClassMaxMix <- object@indexClassMaxMix
    minPhi <- object@minPhi
    maxPhi <- object@maxPhi
    minLevelComponentWeight <- object@minLevelComponentWeight
    maxLevelComponentWeight <- object@maxLevelComponentWeight
    multComponentWeightMix <- object@multComponentWeightMix
    multLevelComponentWeightMix <- object@multLevelComponentWeightMix
    multTau <- object@multTau
    multVectorsMix <- object@multVectorsMix
    nuComponentWeightMix <- object@nuComponentWeightMix
    nuLevelComponentWeightMix <- object@nuLevelComponentWeightMix
    nuTau <- object@nuTau
    nuVectorsMix <- object@nuVectorsMix
    omegaComponentWeightMaxMix <- object@omegaComponentWeightMaxMix
    omegaLevelComponentWeightMaxMix <- object@omegaLevelComponentWeightMaxMix
    omegaVectorsMaxMix <- object@omegaVectorsMaxMix
    phi <- object@phi
    phiKnown <- object@phiKnown
    priorMeanLevelComponentWeightMix <- object@priorMeanLevelComponentWeightMix
    priorSDLevelComponentWeightMix <- object@priorSDLevelComponentWeightMix
    shape1Phi <- object@shape1Phi
    shape2Phi <- object@shape2Phi
    tauMax <- object@tauMax
    ## allStrucZero
    allStrucZero <- makeAllStrucZeroError(strucZeroArray = strucZeroArray,
                                          margin = margin,
                                          metadata = metadata,
                                          classPrior = "Mix")                          
    ## AComponentWeightMix, omegaComponentWeightMaxMix, omegaComponentWeight
    AComponentWeightMix <-
        makeAComponentMix(A = AComponentWeightMix,
                          metadata = metadata,
                          sY = sY,
                          mult = multComponentWeightMix)
    omegaComponentWeightMaxMix <-
        makeScaleMax(scaleMax = omegaComponentWeightMaxMix,
                     A = AComponentWeightMix,
                     nu = nuComponentWeightMix)
    omegaComponentWeightMix <-
        makeScale(A = AComponentWeightMix,
                  nu = nuComponentWeightMix,
                  scaleMax = omegaComponentWeightMaxMix)
    ## ALevelComponentWeightMix, omegaLevelComponentWeightMaxMix,
    ## omegaLevelComponentWeight
    ALevelComponentWeightMix <-
        makeAHalfT(A = ALevelComponentWeightMix,
                   metadata = metadata,
                   sY = sY,
                   mult = multLevelComponentWeightMix)
    omegaLevelComponentWeightMaxMix <-
        makeScaleMax(scaleMax = omegaLevelComponentWeightMaxMix,
                     A = ALevelComponentWeightMix,
                     nu = nuLevelComponentWeightMix)
    omegaLevelComponentWeightMix <-
        makeScale(A = ALevelComponentWeightMix,
                  nu = nuLevelComponentWeightMix,
                  scaleMax = omegaLevelComponentWeightMaxMix)
    ## ATau, tauMax, tau
    ATau <- makeAHalfT(A = ATau,
                       metadata = metadata,
                       sY = sY,
                       mult = multTau)
    tauMax <- makeScaleMax(scaleMax = tauMax,
                           A = ATau,
                           nu = nuTau)
    tau <- makeScale(A = ATau,
                     nu = nuTau,
                     scaleMax = tauMax)
    ## iAlong
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    ## J
    J <- makeJ(beta)
    ## dimBeta
    dimBeta <- dim(metadata)
    ## nBetaNoAlong
    nBetaNoAlongMix <- as.integer(prod(dimBeta[-iAlong]))
    ## posProdVectors1Mix, posProdVectors2Mix
    if (iAlong == 1L) {
        posProdVectors1Mix <- dimBeta[1L]
        posProdVectors2Mix <- 1L
    }
    else {
        s1 <- seq_len(iAlong)
        s2 <- seq_len(iAlong - 1L)
        posProdVectors1Mix <- prod(dimBeta[s1])
        posProdVectors2Mix <- prod(dimBeta[s2])
        posProdVectors1Mix <- as.integer(posProdVectors1Mix)
        posProdVectors2Mix <- as.integer(posProdVectors2Mix)
    }        
    ## AVectorsMix, omegaVectorsMaxMix, omegaVectorsMix
    AVectorsMix <-
        makeAHalfT(A = AVectorsMix,
                   metadata = metadata,
                   sY = sY,
                   mult = multVectorsMix)
    omegaVectorsMaxMix <-
        makeScaleMax(scaleMax = omegaVectorsMaxMix,
                     A = AVectorsMix,
                     nu = nuVectorsMix)
    omegaVectorsMix <-
        makeScale(A = AVectorsMix,
                  nu = nuVectorsMix,
                  scaleMax = omegaVectorsMaxMix)
    ## vectorsMix
    vectorsMix <- makeVectorsMix(dimBeta = dimBeta,
                                 iAlong = iAlong,
                                 indexClassMaxMix = indexClassMaxMix,
                                 omegaVectorsMix = omegaVectorsMix)
    ## prodVectorsMix
    prodVectorsMix <- makeProdVectorsMix(vectorsMix = vectorsMix,
                                         iAlong = iAlong,
                                         dimBeta = dimBeta,
                                         indexClassMaxMix = indexClassMaxMix)
    ## iteratorProdVectorMix
    iteratorProdVectorMix <-
        makeIteratorProdVectorMix(dimBeta = dimBeta,
                                  iAlong = iAlong)
    ## phiMix
    phiMix <- makePhi(phi = phi,
                      phiKnown = phiKnown,
                      minPhi = minPhi,
                      maxPhi = maxPhi)
    ## meanLevelComponentWeightMix
    meanLevelComponentWeightMix <-
        makeMeanLevelComponentWeightMix(priorMean = priorMeanLevelComponentWeightMix,
                                        priorSD = priorSDLevelComponentWeightMix)
    ## levelComponentWeightMix
    ## levelComponentWeightMix <-
    ##     makeLevelComponentWeightMix(dimBeta = dimBeta,
    ##                                 iAlong = iAlong,
    ##                                 indexClassMaxMix = indexClassMaxMix,
    ##                                 phiMix = phiMix,
    ##                                 meanLevel = meanLevelComponentWeightMix,
    ##                                 omegaLevel = omegaLevelComponentWeightMix)
    levelComponentWeightMix <- stats::rnorm(n = dimBeta[iAlong] * indexClassMaxMix@.Data)
    levelComponentWeightMix <- methods::new("ParameterVector", levelComponentWeightMix)
    ## componentWeightMix
    componentWeightMix <-
        makeComponentWeightMix(dimBeta = dimBeta,
                               iAlong = iAlong,
                               indexClassMaxMix = indexClassMaxMix,
                               levelComponent = levelComponentWeightMix,
                               omegaComponent = omegaComponentWeightMix)
    ## weightMix
    weightMix <- makeWeightMix(dimBeta = dimBeta,
                               iAlong = iAlong,
                               indexClassMaxMix = indexClassMaxMix,
                               componentWeightMix = componentWeightMix)
    ## iteratorsDimsMix
    makeSliceIterator <- function(i) SliceIterator(dim = dimBeta, iAlong = i)
    iteratorsDimsMix <- lapply(seq_along(dimBeta), makeSliceIterator)
    ## indexClassMix
    indexClassMix <- makeIndexClassMix(dimBeta = dimBeta,
                                       iAlong = iAlong,
                                       indexClassMaxMix = indexClassMaxMix,
                                       weightMix = weightMix)
    ## indexClassMaxPossibleMix
    indexClassMaxPossibleMix <- max(indexClassMix)
    indexClassMaxPossibleMix <- methods::new("Counter", indexClassMaxPossibleMix)
    ## indexClassMaxUsedMix
    indexClassMaxUsedMix <- max(indexClassMix)
    indexClassMaxUsedMix <- methods::new("Counter", indexClassMaxUsedMix)
    ## indexClassProbMix
    indexClassProbMix <- rep(0, times = indexClassMaxMix@.Data)
    indexClassProbMix <- methods::new("ParameterVector", indexClassProbMix)
    ## isSaturated
    isSaturated <- methods::new("LogicalFlag", isSaturated)
    ## foundIndexClassMaxPossibleMix
    foundIndexClassMaxPossibleMix <- methods::new("LogicalFlag", TRUE)
    ## sumsWeightsMix
    sumsWeightsMix <- rep(0, times = dimBeta[iAlong])
    sumsWeightsMix <- methods::new("UnitIntervalVec", sumsWeightsMix)
    ## latentComponentWeightMix
    latentComponentWeightMix <-
        makeLatentComponentWeightMix(dimBeta = dimBeta,
                                     iAlong = iAlong,
                                     indexClassMix = indexClassMix,
                                     indexClassMaxMix = indexClassMaxMix,
                                     componentWeightMix = componentWeightMix,
                                     iteratorsDimsMix = iteratorsDimsMix)
    ## latentWeightMix
    latentWeightMix <-
        makeLatentWeightMix(dimBeta = dimBeta,
                            iAlong = iAlong,
                            iteratorsDimsMix = iteratorsDimsMix,
                            indexClassMix = indexClassMix,
                            indexClassMaxMix = indexClassMaxMix,
                            weightMix = weightMix)
    ## mMix, CMix, aMix, RMix
    n.along <- dimBeta[iAlong]
    mMix <- rep(0, times = n.along)
    CMix <- rep(1, times = n.along)
    aMix <- rep(0, times = n.along - 1L)
    RMix <- rep(1, times = n.along - 1L)
    mMix <- methods::new("ParameterVector", mMix)
    CMix <- methods::new("ParameterVector", CMix)
    aMix <- methods::new("ParameterVector", aMix)
    RMix <- methods::new("ParameterVector", RMix)
    ## yXMix, XXMix
    yXMix <- rep(0, times = indexClassMaxMix@.Data)
    XXMix <- rep(0, times = indexClassMaxMix@.Data)
    yXMix <- methods::new("ParameterVector", yXMix)
    XXMix <- methods::new("ParameterVector", XXMix)
    ## alphaMix
    alphaMix <- makeAlphaMix(prodVectorsMix = prodVectorsMix,
                             indexClassMix = indexClassMix,
                             indexClassMaxMix = indexClassMaxMix,
                             nBetaNoAlongMix = nBetaNoAlongMix,
                             posProdVectors1Mix = posProdVectors1Mix,
                             posProdVectors2Mix = posProdVectors2Mix)
    list(AComponentWeightMix = AComponentWeightMix,
         ALevelComponentWeightMix = ALevelComponentWeightMix,
         ATau = ATau,
         AVectorsMix = AVectorsMix,
         aMix = aMix,
         allStrucZero = allStrucZero,
         alphaMix = alphaMix,
         CMix = CMix,
         componentWeightMix = componentWeightMix,
         dimBeta = dimBeta,
         foundIndexClassMaxPossibleMix = foundIndexClassMaxPossibleMix,
         iAlong = iAlong,
         indexClassMaxMix = indexClassMaxMix,
         indexClassMaxPossibleMix = indexClassMaxPossibleMix,
         indexClassMaxUsedMix = indexClassMaxUsedMix,
         indexClassMix = indexClassMix,
         indexClassProbMix = indexClassProbMix,
         isSaturated = isSaturated,
         iteratorProdVectorMix = iteratorProdVectorMix,
         iteratorsDimsMix = iteratorsDimsMix,
         J = J,
         latentComponentWeightMix = latentComponentWeightMix,
         latentWeightMix = latentWeightMix,
         levelComponentWeightMix = levelComponentWeightMix,
         mMix = mMix,
         maxLevelComponentWeight = maxLevelComponentWeight,
         maxPhi = maxPhi,
         minLevelComponentWeight = minLevelComponentWeight,
         minPhi = minPhi,
         nBetaNoAlongMix = nBetaNoAlongMix,
         nuComponentWeightMix = nuComponentWeightMix,
         nuLevelComponentWeightMix = nuLevelComponentWeightMix,
         nuTau = nuTau,
         nuVectorsMix = nuVectorsMix,
         omegaComponentWeightMaxMix = omegaComponentWeightMaxMix,
         omegaComponentWeightMix = omegaComponentWeightMix,
         omegaLevelComponentWeightMaxMix = omegaLevelComponentWeightMaxMix,
         omegaLevelComponentWeightMix = omegaLevelComponentWeightMix,
         omegaVectorsMaxMix = omegaVectorsMaxMix,
         omegaVectorsMix = omegaVectorsMix,
         meanLevelComponentWeightMix = meanLevelComponentWeightMix,
         phiMix = phiMix,
         phiKnown = phiKnown,
         shape1Phi = shape1Phi,
         shape2Phi = shape2Phi,
         posProdVectors1Mix = posProdVectors1Mix,
         posProdVectors2Mix = posProdVectors2Mix,
         priorMeanLevelComponentWeightMix = priorMeanLevelComponentWeightMix,
         priorSDLevelComponentWeightMix = priorSDLevelComponentWeightMix,
         prodVectorsMix = prodVectorsMix,
         RMix = RMix,
         sumsWeightsMix = sumsWeightsMix,
         tau = tau,
         tauMax = tauMax,
         vectorsMix = vectorsMix,
         weightMix = weightMix,
         XXMix = XXMix,
         yXMix = yXMix)
}

## HAS_TESTS
initialMixAllPredict <- function(prior, metadata, name, along, margin, strucZeroArray) {
    index.class.max <- prior@indexClassMaxMix@.Data
    ## allStrucZero
    allStrucZero <- makeAllStrucZeroError(strucZeroArray = strucZeroArray,
                                          metadata = metadata,
                                          margin = margin,
                                          classPrior = "Mix")                          
    ## dimBeta
    dimBeta <- dim(metadata)
    ## J
    J <- makeJPredict(metadata)
    ## iAlong
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    i.along.old <- prior@iAlong
    if (!identical(iAlong, i.along.old))
        stop(gettextf("\"%s\" dimension of prediction does not match \"%s\" dimension of prior for '%s'",
                      "along", "along", name))
    n.along <- dimBeta[iAlong]
    ## posProdVectors1Mix, posProdVectors2Mix
    if (iAlong == 1L) {
        posProdVectors1Mix <- dimBeta[1L]
        posProdVectors2Mix <- 1L
    }
    else {
        s1 <- seq_len(iAlong)
        s2 <- seq_len(iAlong - 1L)
        posProdVectors1Mix <- prod(dimBeta[s1])
        posProdVectors2Mix <- prod(dimBeta[s2])
        posProdVectors1Mix <- as.integer(posProdVectors1Mix)
        posProdVectors2Mix <- as.integer(posProdVectors2Mix)
    }        
    ## componentWeightMix
    componentWeightMix <- rep(0, times = n.along * index.class.max)
    componentWeightMix <- methods::new("ParameterVector", componentWeightMix)
    ## indexClassMix
    indexClassMix <- rep(1L, times = J@.Data)
    ## iteratorProdVectorMix
    iteratorProdVectorMix <-
        makeIteratorProdVectorMix(dimBeta = dimBeta,
                                  iAlong = iAlong)
    ## iteratorsDimsMix
    makeSliceIterator <- function(i) SliceIterator(dim = dimBeta, iAlong = i)
    iteratorsDimsMix <- lapply(seq_along(dimBeta), makeSliceIterator)
    ## latentComponentWeightMix
    latentComponentWeightMix <- rep(0, times = J * index.class.max)
    latentComponentWeightMix <- methods::new("ParameterVector",
                                             latentComponentWeightMix)
    ## latentWeightMix
    latentWeightMix <- rep(0, times = J@.Data)
    latentWeightMix <- methods::new("UnitIntervalVec", latentWeightMix)
    ## levelComponentWeightMix
    levelComponentWeightMix <- rep(0, times = n.along * index.class.max)
    levelComponentWeightMix <- methods::new("ParameterVector", levelComponentWeightMix)
    ## levelComponentWeightOldMix
    levelComponentWeightOldMix <- rep(0, times = index.class.max)
    levelComponentWeightOldMix <- methods::new("ParameterVector",
                                               levelComponentWeightOldMix)
    ## sumsWeightsMix
    sumsWeightsMix <- rep(0, times = n.along)
    sumsWeightsMix <- methods::new("UnitIntervalVec", sumsWeightsMix)
    ## weightMix
    weightMix <- rep(0, times = n.along * index.class.max)
    weightMix <- methods::new("UnitIntervalVec", weightMix)
    ## mMix, CMix, aMix, RMix
    mMix <- rep(0, times = n.along)
    CMix <- rep(1, times = n.along)
    aMix <- rep(0, times = n.along - 1L)
    RMix <- rep(1, times = n.along - 1L)
    mMix <- methods::new("ParameterVector", mMix)
    CMix <- methods::new("ParameterVector", CMix)
    aMix <- methods::new("ParameterVector", aMix)
    RMix <- methods::new("ParameterVector", RMix)
    ## alphaMix
    alphaMix <- rep(0, times = J@.Data)
    alphaMix <- methods::new("ParameterVector", alphaMix)
    list(aMix = aMix,
         allStrucZero = allStrucZero,
         alphaMix = alphaMix,
         CMix = CMix,
         componentWeightMix = componentWeightMix,
         dimBeta = dimBeta,
         iAlong = iAlong,
         indexClassMix = indexClassMix,
         iteratorProdVectorMix = iteratorProdVectorMix,
         iteratorsDimsMix = iteratorsDimsMix,
         J = J,
         latentComponentWeightMix = latentComponentWeightMix,
         latentWeightMix = latentWeightMix,
         levelComponentWeightMix = levelComponentWeightMix,
         levelComponentWeightOldMix = levelComponentWeightOldMix,
         mMix = mMix,
         posProdVectors1Mix = posProdVectors1Mix,
         posProdVectors2Mix = posProdVectors2Mix,
         RMix = RMix,
         sumsWeightsMix = sumsWeightsMix,
         weightMix = weightMix)
}

## HAS_TESTS
initialRobust <- function(object, lAll) {
    nuBeta <- object@nuBeta
    J <- lAll$J
    ATau <- lAll$ATau
    allStrucZero <- lAll$allStrucZero
    UBeta <- makeU(nu = nuBeta,
                   A = ATau,
                   n = J,
                   allStrucZero = allStrucZero)
    list(nuBeta = nuBeta,
         UBeta = UBeta)
}


## HAS_TESTS
initialRobustPredict <- function(prior, metadata, allStrucZero) {
    ATau <- prior@ATau
    nuBeta <- prior@nuBeta
    J <- makeJPredict(metadata)
    UBeta <- makeU(nu = nuBeta,
                   A = ATau,
                   n = J,
                   allStrucZero = allStrucZero)
    list(UBeta = UBeta)
}


## NO_TESTS
makeAComponentMix <- function(A, metadata, sY, mult) {
    if (is.na(A)) {
        ans <- 0.5
        if (!is.null(sY))
            ans <- sY * ans
        ans <- mult * ans
    }
    else
        ans <- A
    methods::new("Scale", ans)
}


## NO_TESTS
makeAHalfT <- function(A, metadata, sY, mult) {
    if (is.na(A)) {
        d <- length(metadata)
        ans <- (0.5)^(d - 1L)
        if (!is.null(sY))
            ans <- sY * ans
        ans <- mult * ans
    }
    else
        ans <- A
    methods::new("Scale", ans)
}

## NO_TESTS
makeAHalfTVec <- function(A, metadata, sY, mult) {
    ans <- A@.Data
    d <- length(metadata)
    for (i in seq_along(ans)) {
        if (is.na(ans[i])) {
            ans[i] <- (0.5)^(d - 1L)
            if (!is.null(sY))
                ans[i] <- sY * ans[i]
            ans[i] <- mult@.Data[i] * ans[i]
        }
    }
    methods::new("ScaleVec", ans)
}

## NO_TESTS
makeAIntercept <- function(sY) {
    ans <- 10
    if (!is.null(sY))
        ans <- sY * ans
    methods::new("Scale", ans)
}

## NO_TESTS
makeASigma <- function(A, sY, mult, isSpec = FALSE) {
    if (is.na(A)) {
        ans <- 1
        if (!is.null(sY))
            ans <- sY * ans
        ans <- ans * mult@.Data
    }
    else
        ans <- A
    if (isSpec)
        methods::new("SpecScale", ans)
    else
        methods::new("Scale", ans)
}

## HAS_TESTS
makeAlphaMix <- function(prodVectorsMix, indexClassMix, indexClassMaxMix,
                         nBetaNoAlongMix, posProdVectors1Mix,
                         posProdVectors2Mix) {
    index.class.max <- indexClassMaxMix@.Data
    prod.vectors <- prodVectorsMix@.Data
    prod.vectors <- matrix(prod.vectors,
                           nrow = nBetaNoAlongMix,
                           ncol = index.class.max)
    i.beta <- seq_along(indexClassMix)
    i.beta.no.along <- (((i.beta - 1L) %/% posProdVectors1Mix) * posProdVectors2Mix
        + (i.beta - 1L) %% posProdVectors2Mix
        + 1L)
    i <- cbind(i.beta.no.along, indexClassMix)
    ans <- prod.vectors[i]
    methods::new("ParameterVector", ans)
}
    
## NO_TESTS
makeEta <- function(beta, UEtaCoef) {
    P <- length(UEtaCoef) + 1L
    mean <- c(mean(beta), rep(0, times = P - 1L))
    sd <- c(1, sqrt(UEtaCoef))
    ans <- stats::rnorm(n = P, sd = sd)
    methods::new("ParameterVector", ans)
}

## HAS_TESTS
makeIndexClassMix <- function(dimBeta, iAlong, indexClassMaxMix,
                              weightMix) {
    kAddToProb1 <- 0.01
    n.along <- dimBeta[iAlong]
    indexClassMaxMix <- indexClassMaxMix@.Data
    weightMix <- weightMix@.Data
    weightMix <- matrix(weightMix,
                        nrow = n.along,
                        ncol = indexClassMaxMix)
    weightMix[, 1L] <- weightMix[, 1L] + kAddToProb1 # to prevent numerical problems with sampling
    ans <- array(dim = dimBeta)
    index.array <- slice.index(x = ans,
                               MARGIN = iAlong)
    length.slice.i <- prod(dimBeta[-iAlong])
    for (i in seq_len(n.along)) {
        is.in.slice.i <- index.array == i
        prob <- weightMix[i, ]
        ans[is.in.slice.i] <- replicate(n = length.slice.i,
                                        sample.int(n = indexClassMaxMix,
                                                   size = 1L,
                                                   prob = prob))
    }
    as.integer(ans)
}

## NO_TESTS
makeJ <- function(beta) {
    methods::new("Length", length(beta))
}

## NO_TESTS
makeJPredict <- function(metadata) {
    dim <- dim(metadata)
    length <- prod(dim)
    length <- as.integer(length)
    methods::new("Length", length)
}

## HAS_TESTS
makeLatentComponentWeightMix <- function(dimBeta, iAlong, indexClassMix,
                                         indexClassMaxMix, componentWeightMix,
                                         iteratorsDimsMix) {
    n.along <- dimBeta[iAlong]
    componentWeightMix <- componentWeightMix@.Data
    indexClassMaxMix <- indexClassMaxMix@.Data
    componentWeightMix <- matrix(componentWeightMix,
                                 nrow = n.along,
                                 ncol = indexClassMaxMix)
    J <- length(indexClassMix)
    ans <- matrix(nrow = J,
                  ncol = indexClassMaxMix)
    iterator.beta <- iteratorsDimsMix[[iAlong]]
    iterator.beta <- resetS(object = iterator.beta,
                            useC = TRUE)
    s <- seq_len(indexClassMaxMix)
    makeLatentComp <- function(k, W) {
        lower <- ifelse(s == k, 0, -Inf)
        upper <- ifelse(s < k, 0, Inf)
        val <- double(length = indexClassMaxMix)
        for (i in s)
            val[i] <- rtnorm1(mean = W,
                              sd = 1,
                              lower = lower[i],
                              upper = upper[i],
                              useC = TRUE)
        val
    }
    for (i.along in seq_len(n.along)) {
        indices.beta <- iterator.beta@indices
        for (i.beta in indices.beta) {
            k <- indexClassMix[i.beta]
            W <- componentWeightMix[i.along, k]
            ans[i.beta, ] <- makeLatentComp(k = k, W = W)
        }
        iterator.beta <- advanceS(object = iterator.beta,
                                  useC = TRUE)
    }
    ans <- as.double(ans)
    methods::new("ParameterVector", ans)
}
    
## NO_TESTS
makeP <- function(Z) {
    ans <- ncol(Z)
    methods::new("Length", ans)
}

## NO_TESTS
makeScale <- function(A, nu, scaleMax) {
    max <- scaleMax@.Data
    ans <- rhalft(n = 1L,
                  df = nu,
                  scale = A)
    if (ans > max)
        ans <- stats::runif(n = 1,
                            min = 0,
                            max = min(2 * A@.Data, max))
    methods::new("Scale", ans)
}

## NO_TESTS
makeScaleMax <- function(scaleMax, A, nu, isSpec = FALSE) {
    kPScaleMax <- 0.999
    if (is.na(scaleMax))
        scaleMax <- qhalft(p = kPScaleMax,
                           df = nu@.Data,
                           scale = A@.Data)
    if (isSpec)
        methods::new("SpecScale", scaleMax)
    else
        methods::new("Scale", scaleMax)
}        

## Expands factors into dummy variables, and then standardises as
## described in Gelman, A., Jakulin, A., Pittau, M. G., and Su, Y.-S.
## (2008). A weakly informative default prior distribution
## for logistic and other regression models.
## The Annals of Applied Statistics, pages 1360-1383.
makeStandardizedVariables <- function(formula, inputs, namePrior, contrastsArg, allStrucZero) { ## NEW
    if (identical(contrastsArg, list()))
        contrastsArg <- NULL
    ans <- tryCatch(stats::model.matrix(object = formula,
                                        data = inputs,
                                        contrasts.arg = contrastsArg),
                    error = function(e) e)
    if (methods::is(ans, "error"))
        stop(gettextf("problem constructing model matrix from formula '%s' in prior for '%s' : %s",
                      deparse(formula), namePrior, ans$message))
    which.term <- attr(ans, "assign")
    terms <- stats::terms(formula)
    factors <- attr(terms, "factors")
    order.term <- attr(terms, "order")
    ans[allStrucZero, ] <- NA
    for (j in seq_len(ncol(ans))[-1L]) {
        v <- ans[ , j]
        i.term <- which.term[j]
        is.main.effect <- order.term[i.term] == 1L
        if (is.main.effect) {
            is.binary <- isTRUE(all.equal(sort(unique(stats::na.omit(v))), 0:1))
            if (is.binary)
                v <- v - mean(v, na.rm = TRUE)
            else
                v <- (v - mean(v, na.rm = TRUE)) / (2 * stats::sd(v, na.rm = TRUE))
        }
        else {
            i.main.effect.contributes <- which(factors[ , i.term] == 1L) + 1L
            v <- ans[, i.main.effect.contributes, drop = FALSE]
            v <- apply(v, MARGIN = 1L, FUN = prod)
        }
        ans[ , j] <- v
    }
    ans[allStrucZero, ] <- 0
    array(ans, dim = dim(ans), dimnames = dimnames(ans))
}

## HAS_TESTS
makeAllStrucZero <- function(strucZeroArray, metadata, margin) {
    array.zero <- tryCatch(collapseDimension(strucZeroArray,
                                             margin = margin),
                           error = function(e) e)
    if (methods::is(array.zero, "error"))
        stop(gettextf("problem assigning structural zeros to prior '%s' : %s",
                      paste(names(metadata), collapse = ":"), array.zero$message))
    as.logical(array.zero@.Data == 0L)
}

## HAS_TESTS
makeAllStrucZeroError <- function(strucZeroArray, metadata, margin, classPrior) {
    all.struc.zero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                       metadata = metadata,
                                       margin = margin)
    if (any(all.struc.zero)) {
        name.prior <- paste(names(metadata), collapse = ":")
        stop(gettextf("'%s' has elements where all contributing cells are structural zeros; priors with class \"%s\" cannot be used in such cases",
                      name.prior, classPrior))
    }
    all.struc.zero
}

## HAS_TESTS
makeAlongAllStrucZero <- function(strucZeroArray, metadata, margin, iAlong) {
    if (length(metadata) == 1L)
        return(FALSE)
    name.prior <- paste(names(metadata), collapse = ":")
    metadata.along <- metadata[iAlong]
    metadata.within <- metadata[-iAlong]
    .Data.along <- array(0L,
                         dim = dim(metadata.along),
                         dimnames = dimnames(metadata.along))
    .Data.within <- array(0L,
                          dim = dim(metadata.within),
                          dimnames = dimnames(metadata.within))
    array.along <- methods::new("Counts",
                       .Data = .Data.along,
                       metadata = metadata.along)
    array.within <- methods::new("Counts",
                        .Data = .Data.within,
                        metadata = metadata.within)
    array.zero.along <- tryCatch(collapseDimension(strucZeroArray,
                                                   margin = margin[iAlong]),
                                 error = function(e) e)
    array.zero.within <- tryCatch(collapseDimension(strucZeroArray,
                                                    margin = margin[-iAlong]),
                                  error = function(e) e)
    if (methods::is(array.zero.along, "error"))
        stop(gettextf("problem assigning structural zeros to prior '%s' : %s",
                      name.prior, array.zero.along$message))
    if (methods::is(array.zero.within, "error"))
        stop(gettextf("problem assigning structural zeros to prior '%s' : %s",
                      name.prior, array.zero.within$message))
    along.is.zero <- as.logical(array.zero.along@.Data == 0L)
    within.is.zero <- as.logical(array.zero.within@.Data == 0L)
    if (any(along.is.zero)) {
        labels <- dimnames(metadata.along)[[1L]]
        i.first.zero <- which(along.is.zero)[1L]
        name.along <- names(metadata.along)
        stop(gettextf("all cells contributing to element \"%s\" of '%s\' dimension [\"%s\"] for prior '%s' are structural zeros",
                      labels[i.first.zero], "along", name.along, name.prior))
    }
    within.is.zero
}

## HAS_TESTS
makeStrucZeroArray <- function(structuralZeros, y) {
    if (is.null(structuralZeros))
        makeStrucZeroArrayNULL(y)
    else if (identical(structuralZeros, methods::new("Values")))
        makeStrucZeroArrayDiag(y)
    else
        makeStrucZeroArrayGeneral(structuralZeros = structuralZeros,
                                  y  = y)
}

## HAS_TESTS
makeStrucZeroArrayNULL <- function(y) {
    .Data <- array(1L,
                   dim = dim(y),
                   dimnames = dimnames(y))
    metadata <- y@metadata
    methods::new("Counts",
        .Data = .Data,
        metadata = metadata)
}

## HAS_TESTS
makeStrucZeroArrayDiag <- function(y) {
    metadata <- y@metadata
    names <- names(y)
    dimtypes <- dimtypes(y, use.names = FALSE)
    i.orig <- grep("origin", dimtypes)
    has.orig <- length(i.orig) > 0L
    if (!has.orig)
        stop(gettextf("'%s' has no dimensions with %s \"%s\"",
                      "y", "dimtype", "origin"))
    names.orig <- names[i.orig]
    base <- sub("_orig$", "", names.orig)
    dembase::pairAligned(y, base = base)
    .Data <- array(1L,
                   dim = dim(y),
                   dimnames = dimnames(y))
    names.dest <- sprintf("%s_dest", base)
    i.dest <- match(names.dest, names)
    for (i in seq_along(i.orig)) {
        is.diag <- slice.index(y, MARGIN = i.orig[i]) == slice.index(y, MARGIN = i.dest[i])
        .Data[is.diag] <- 0L
    }
    methods::new("Counts",
        .Data = .Data,
        metadata = metadata)
}

## HAS_TESTS
makeStrucZeroArrayGeneral <- function(structuralZeros, y) {
    ans <- tryCatch(makeCompatible(x = structuralZeros,
                                   y = y,
                                   subset = FALSE,
                                   check = TRUE),
                    error = function(e) e)
    if (methods::is(ans, "error"))
        stop(gettextf("problem expanding '%s' to make it compatible with '%s' : %s",
                      "structuralZeros", "y", ans$message))
    ans[] <- ifelse(ans == 0L, 0L, 1L)
    ans <- methods::as(ans, "Counts")
    ans <- toInteger(ans)
    ans
}

## NO_TESTS
makeTauExchFixedIntercept <- function(tau, sY) {
    if (is.na(tau)) {
        if (is.null(sY))
            ans <- 10
        else
            ans <- 10 * sY
    }
    else
        ans <- tau
    ans <- methods::new("Scale", ans)
}

## NO_TESTS
makeTauExchFixedNonIntercept <- function(tau, sY, mult) {
    if (is.na(tau)) {
        if (is.null(sY))
            ans <- 1
        else
            ans <- sY
        ans <- mult * ans
    }
    else
        ans <- tau
    ans <- methods::new("Scale", ans)
}

## NO_TESTS
makeU <- function(nu, A, n, allStrucZero) {
    ans <- double(length = n)
    for (i in seq_len(n))
        ans[i] <- rinvchisq1(df = nu, scaleSq = A^2)
    ans[allStrucZero] <- 1
    methods::new("VarTDist", ans)
}


## NO_TESTS
makeUEtaCoef <- function(nu, A, n) {
    ans <- double(length = n)
    for (i in seq_len(n))
        ans[i] <- rinvchisq1(df = nu@.Data[i],
                             scaleSq = A@.Data[i]^2)
    methods::new("VarTDist", ans)
}


## NO_TESTS
makeMNoTrend <- function(K, m0 = NULL) {
    ans <- replicate(n = K + 1L,
                     0.0,
                     simplify = FALSE)
    if (!is.null(m0))
        ans[[1L]] <- m0
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeM0NoTrend <- function(L) {
    ans <- replicate(n = L,
                     0.0,
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeMSeason <- function(K, nSeason, m0 = NULL) {
    ans <- replicate(n = K + 1L,
                     rep(0, times = nSeason),
                     simplify = FALSE)
    if (!is.null(m0))
        ans[[1L]] <- m0
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeM0Season <- function(L, nSeason) {
    ans <- replicate(n = L,
                     rep(0, times = nSeason),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeMWithTrend <- function(K, m0 = NULL) {
    ans <- replicate(n = K + 1L,
                     c(0, 0),
                     simplify = FALSE)
    if (!is.null(m0))
        ans[[1L]] <- m0
    methods::new("FFBSList", ans)
}


## NO_TESTS
makeM0WithTrend <- function(L, meanDelta0 = NULL) {
    if (is.null(meanDelta0))
        meanDelta0 <- 0
    else
        meanDelta0 <- meanDelta0@.Data
    ans <- replicate(n = L,
                     c(0, meanDelta0),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeCNoTrend <- function(K, C0 = NULL, sY, phi, phiKnown) {
    ans <- replicate(n = K + 1L,
                     1.0,
                     simplify = FALSE)
    if (is.null(C0)) {
        A0 <- makeAIntercept(sY)
        A0 <- as.double(A0)
        C0 <- A0^2
    }
    ans[[1L]] <- C0
    methods::new("FFBSList", ans)
}

## NO_TESTS
## elements of C are vectors, not matrices
makeCSeason <- function(K, nSeason, ASeason, C0 = NULL) {
    if (is.null(C0)) {
        A <- ASeason@.Data
        C0 <- rep(A^2, times = nSeason)
    }
    ans <- replicate(n = K + 1L,
                     C0,
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeCWithTrend <- function(K, C0 = NULL, sY, ADelta0, hasLevel = TRUE) {
    if (is.null(C0)) {
        AAlpha <- makeAIntercept(sY)
        ADelta <- ADelta0@.Data
        C0 <- c(AAlpha^2, ADelta^2)
        C0 <- diag(C0,
                   nrow = 2L,
                   ncol = 2L)
    }
    head <- list(C0)
    tail <- replicate(n = K,
                      diag(2L),
                      simplify = FALSE)
    ans <- c(head, tail)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeANoTrend <- function(K) {
    ans <- replicate(n = K,
                     0.0,
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeASeason <- function(K, nSeason) {
    ans <- replicate(n = K,
                     rep(0, times = nSeason),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeAWithTrend <- function(K) {
    ans <- replicate(n = K,
                     c(0, 0),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeDC <- function(CWithTrend) {
    K.plus.1 <- length(CWithTrend)
    ans <- replicate(n = K.plus.1,
                     diag(2L),
                     simplify = FALSE)
    ans[[1L]] <- sqrt(CWithTrend[[1L]])
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeDCInv <- function(DC) {
    for (i in seq_along(DC))
        diag(DC[[i]]) <- ifelse(diag(DC[[i]]) > 0, 1 / diag(DC[[i]]), Inf)
    DC
}

## NO_TESTS
makeDRInv <- function(K) {
    ans <- replicate(n = K,
                     diag(2L),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeRNoTrend <- function(K) {
    ans <- replicate(n = K,
                     1.0,
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeRSeason <- function(K, nSeason) {
    ans <- replicate(n = K,
                     rep(1.0, times = nSeason),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeRWithTrend <- function(K) {
    ans <- replicate(n = K,
                     diag(2L),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeK <- function(dim, iAlong) {
    ans <- dim[iAlong]
    methods::new("Length", ans)
}

## NO_TESTS
makeL <- function(dim, iAlong) {
    ans <- prod(dim[-iAlong])
    ans <- as.integer(ans)
    methods::new("Length", ans)
}

## NO_TESTS
makeSeasonDLM <- function(K, L, nSeason) {
    n <- (K + 1L) * L
    ans <- replicate(n = n,
                     rep(0, times = nSeason),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeStateDLM <- function(K, L) {
    n <- (K + 1L) * L
    ans <- rep(0, times = n)
    methods::new("ParameterVector", ans)
}

## NO_TESTS
makePhi <- function(phi, phiKnown, minPhi, maxPhi) {
    if (phiKnown)
        phi
    else
        stats::runif(n = 1L, min = minPhi, max = maxPhi)
}

## NO_TESTS
makeGWithTrend <- function(phi) {
    ans <- matrix(c(1, 0, 1, phi), nrow = 2, ncol = 2)
    methods::new("NumericMatrixSquare", ans)
}

## NO_TESTS
makeWSqrt <- function(omegaAlpha, omegaDelta) {
    ans <- matrix(c(omegaAlpha, 0, 0, omegaDelta), nrow = 2L, ncol = 2L)
    methods::new("NumericMatrixSquare", ans)
}

## NO_TESTS
makeWSqrtInvG <- function(omegaAlpha, omegaDelta, phi) {
    ans <- matrix(0, nrow = 2L, ncol = 2L)
    ans[1L] <- 1 / omegaAlpha
    ans[3L] <- 1 / omegaAlpha
    ans[4L] <- phi / omegaDelta
    methods::new("NumericMatrixSquare", ans)
}

## NO_TESTS
makeUC <- function(K) {
    ans <- replicate(n = K + 1L,
                     diag(2L),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeUR <- function(K) {
    ans <- replicate(n = K,
                     diag(2L),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## HAS_TESTS
makeZ <- function(formula, data, metadata, contrastsArg, infant, allStrucZero) {
    namePrior <- paste(names(metadata), collapse = ":")
    ## infant
    if (infant@.Data) {
        data <- addInfantToData(metadata = metadata,
                                data = data)
        if (length(formula) == 0L)
            formula <- ~ infant
        else {
            name.infant <- names(data)[length(data)]
            formula <- deparse(formula)
            formula <- paste(formula, name.infant, sep = " + ")
            formula <- stats::as.formula(formula)
        }
    }
    ## make required response
    dimnames <- dimnames(metadata)
    response.required <- expand.grid(dimnames)
    response.required <- do.call(paste, response.required)
    ## make response from 'data'
    i.response <- match(names(metadata), names(data), nomatch = 0L)
    unmatched <- i.response == 0L
    if (any(unmatched))
        stop(gettextf("could not find variable '%s' in covariate data for prior '%s'",
                      names(metadata)[unmatched][1L], namePrior))
    response.obtained <- data[i.response]
    response.obtained <- do.call(paste, response.obtained)
    ## get indices for rows of 'data'
    i.row <- match(response.required, response.obtained, nomatch = 0L)
    unmatched <- i.row == 0L
    if (any(unmatched)) {
        first.unmatched <- response.required[unmatched][1L]
        stop(gettextf("no covariate data for element '%s' in prior for '%s'",
                      first.unmatched, namePrior))
    }
    ## make 'inputs' - variables that covariates formed from
    input.names <- rownames(attr(stats::terms(formula), "factors"))
    inputs <- data[input.names]
    inputs <- inputs[i.row, , drop = FALSE]
    ## make Z
    makeStandardizedVariables(formula = formula,
                              inputs = inputs,
                              namePrior = namePrior,
                              contrastsArg = contrastsArg,
                              allStrucZero = allStrucZero)
}
