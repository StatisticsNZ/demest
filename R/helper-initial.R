

## PRIORS ######################################################

## HAS_TESTS            
initialCov <- function(object, beta, metadata, sY, allStrucZero) {
    AEtaCoef <- object@AEtaCoef
    AEtaIntercept <- object@AEtaIntercept
    contrastsArg <- object@contrastsArg
    data <- object@data
    formula <- object@formula
    infant <- object@infant
    meanEtaCoef <- object@meanEtaCoef
    multEtaCoef <- object@multEtaCoef
    nuEtaCoef <- object@nuEtaCoef
    AEtaIntercept <- makeAIntercept(A = AEtaIntercept,
                                    sY = sY)
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
makeAIntercept <- function(A, sY) {
    if (is.na(A)) {
        ans <- 10
        if (!is.null(sY))
            ans <- sY * ans
        methods::new("Scale", ans)
    }
    else
        methods::new("Scale", A)
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
## The Annals of Applied Statistics, pages 1360–1383.
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
            is.binary <- isTRUE(all.equal(sort(unique(na.omit(v))), 0:1))
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
    array.along <- new("Counts",
                       .Data = .Data.along,
                       metadata = metadata.along)
    array.within <- new("Counts",
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
    else if (identical(structuralZeros, new("Values")))
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
    new("Counts",
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
    new("Counts",
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
    ans <- as(ans, "Counts")
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
        ans[i] <- rinvchisq1(df = nu, scale = A^2)
    ans[allStrucZero] <- 1
    methods::new("VarTDist", ans)
}


## NO_TESTS
makeUEtaCoef <- function(nu, A, n) {
    ans <- double(length = n)
    for (i in seq_len(n))
        ans[i] <- rinvchisq1(df = nu@.Data[i],
                             scale = A@.Data[i]^2)
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
        A0 <- makeAIntercept(A = NA, sY = sY)
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
        AAlpha <- makeAIntercept(A = NA, sY = sY)
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


## INITIAL VALUES - MODELS ##################################################################

## HAS_TESTS
DimScaleIsRegular <- function(x) {
    if (!methods::is(x, "Points") && !methods::is(x, "Intervals"))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    dimvalues <- x@dimvalues
    dimvalues <- dimvalues[is.finite(dimvalues)]
    n <- length(dimvalues)
    if (n < 3L)
        TRUE
    else {
        steps <- diff(dimvalues)
        all(steps[1L] == steps[-1L])
    }
}

## HAS_TESTS
addAgCertain <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    value <- aggregate@valueAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts",
                               .Data = .Data.ag.obj,
                               metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = value)
    value <- as.double(value)
    mu <- rep(0, times = length(theta))
    for (k in seq_along(value)) {
        i.th <- dembase::getIBefore(k, transform = transform, useC = TRUE)
        sum.wt <- sum(abs(weight[i.th]))
        if (sum.wt > 0)
            theta[i.th] <- value[k] / sum.wt
    }
    value <- methods::new("ParameterVector", value)
    class <- paste0(class(object), "AgCertain")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(theta = theta,
         value = value,
         weight = weight,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgNormal <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    scale <- aggregate@scaleAg
    sd <- aggregate@sdAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts", .Data = .Data.theta.obj, metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts", .Data = .Data.ag.obj, metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    value <- array(weight * theta, dim = dim(metadata.y))
    value <- dembase::collapse(value, transform = transform)
    value <- as.double(value)
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    mu <- rep(0, times = length(theta))
    class <- paste0(class(object), "AgNormal")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         scale = scale,
         sd = sd,
         weight = weight,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgPoisson <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    scale <- aggregate@scaleAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts", .Data = .Data.theta.obj, metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts", .Data = .Data.ag.obj, metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = NULL,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    value <- array(weight * theta, dim = dim(metadata.y))
    value <- dembase::collapse(value, transform = transform)
    value <- as.double(value)
    value <- methods::new("ParameterVector", value)
    exposure <- dembase::collapse(defaultWeights@.Data, transform = transform)
    exposure <- as.double(exposure)
    if (!all(exposure > 0))
        stop(gettext("exposure term in Poisson aggregate model contains non-positive values"))
    exposure <- methods::new("ScaleVec", exposure)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    mu <- rep(0, times = length(theta))
    class <- paste0(class(object), "AgPoisson")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         scale = scale,
         weight = weight,
         exposure = exposure,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}
        
## HAS_TESTS
addAgFun <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    sd <- aggregate@sdAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    fun.ag <- aggregate@funAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts",
                               .Data = .Data.ag.obj,
                               metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    metadata.args <- dembase::makeMetaDataSubarraysBefore(metadata = metadata.y,
                                                          transform = transform)
    n.value <- length(mean)
    x.args <- vector(mode = "list", length = n.value)
    weights.args <- vector(mode = "list", length = n.value)
    value <- double(length = n.value)
    for (i in seq_len(n.value)) {
        i.y <- getIBefore(i = i,
                          transform = transform,
                          useC = TRUE)
        metadata.i <- metadata.args[[i]]
        .Data.x <- theta[i.y]
        .Data.weights <- weight[i.y]
        .Data.x <- array(.Data.x,
                         dim = dim(metadata.i),
                         dimnames = dimnames(metadata.i))
        .Data.weights <- array(.Data.weights,
                               dim = dim(metadata.i),
                               dimnames = dimnames(metadata.i))
        x.args[[i]] <- methods::new("Values",
                                    .Data = .Data.x,
                                    metadata = metadata.i)
        weights.args[[i]] <- methods::new("Counts",
                                          .Data = .Data.weights,
                                          metadata = metadata.i)
        val <- tryCatch(fun.ag(x = x.args[[i]], 
                               weights = weights.args[[i]]),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf("error applying '%s' : %s",
                          "FUN", val$message))
        if (!is.numeric(val))
            stop(gettextf("return value from '%s' has class \"%s\"",
                          "FUN", class(val)))
        if (!identical(length(val), 1L))
            stop(gettextf("return value from '%s' has length %d",
                          "FUN", length(val)))
        value[i] <- val
    }
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    class <- paste0(class(object), "AgFun")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         sd = sd,
         metadata = metadata.ag,
         transform = transform,
         funAg = fun.ag,
         xArgs = x.args,
         weightsArgs = weights.args,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgLife <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    sd <- aggregate@sdAg
    ax <- aggregate@axAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    names.y <- names(metadata.y)
    dimtypes.y <- dimtypes(metadata.y, use.names = FALSE)
    DimScales.y <- DimScales(metadata.y, use.names = FALSE)
    i.age.y <- match("age", dimtypes.y, nomatch = 0L)
    has.age.y <- i.age.y > 0L
    if (!has.age.y)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "y", "dimtype", "age"))
    name.age <- names.y[i.age.y]
    DimScale.age <- DimScales.y[[i.age.y]]
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag)) {
        names.ag <- NULL
        dimtypes.ag <- NULL
        DimScales.ag <- NULL
    }
    else {
        names.ag <- names(metadata.ag)
        dimtypes.ag <- dimtypes(metadata.ag, use.names = FALSE)
        DimScales.ag <- DimScales(metadata.ag, use.names = FALSE)
    }    
    names.mx <- c(name.age, names.ag)
    dimtypes.mx <- c("age", dimtypes.ag)
    DimScales.mx <- c(list(DimScale.age), DimScales.ag)
    metadata.mx <- methods::new("MetaData",
                                nms = names.mx,
                                dimtypes = dimtypes.mx,
                                DimScales = DimScales.mx)
    .Data.mx.obj <- array(0,
                          dim = dim(metadata.mx),
                          dimnames = dimnames(metadata.mx))
    mx.obj <- methods::new("Values",
                           .Data = .Data.mx.obj,
                           metadata = metadata.mx)
    transform <- tryCatch(dembase::makeTransform(x = theta.obj,
                                                 y = mx.obj,
                                                 subset = TRUE,
                                                 concordances = concordances.ag,
                                                 check = TRUE),
                          error = function(e) e)
    if (methods::is(transform, "error"))
        stop(gettextf("'%s' not compatible with '%s' from '%s' : %s",
                      "y", "value", "AgLife", transform$message))
    numerator.mx <- theta * defaultWeights
    numerator.mx <- numerator.mx@.Data
    denominator.mx <- defaultWeights@.Data
    numerator.mx <- collapse(numerator.mx,
                             transform = transform)
    denominator.mx <- collapse(denominator.mx,
                               transform = transform)
    mx <- numerator.mx / denominator.mx
    mx <- array(mx,
                dim = dim(metadata.mx),
                dimnames = dimnames(metadata.mx))
    mx <- methods::new("Values",
                       .Data = mx,
                       metadata = metadata.mx)
    if (is.null(ax))
        ax <- makeAxStart(mx) # mx must have metadata
    ax <- expandAx(ax = ax,
                   object = mx) # mx must have metadata
    dv.age <- DimScale.age@dimvalues
    nx <- diff(dv.age)
    mx <- as.double(mx)
    ax <- as.double(ax)
    transform <- dembase::makeCollapseTransformExtra(transform)
    value <- double(length = length(mean))
    nAge <- length(nx)
    seq.iAge0 <- seq.int(from = 1L,
                         by = nAge,
                         to = length(mx))
    for (i in seq_along(seq.iAge0)) {
        iAge0 <- seq.iAge0[i]
        value[i] <- makeLifeExpBirth(mx = mx,
                                     nx = nx,
                                     ax = ax,
                                     iAge0 = iAge0,
                                     nAge= nAge)
    }
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    nAge <- methods::new("Length", nAge)
    class <- paste0(class(object), "AgLife")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         sd = sd,
         metadataAg = metadata.ag,
         transform = transform,
         metadataMx = metadata.mx,
         mx = mx,
         ax = ax,
         nx = nx,
         nAge = nAge,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
checkAndTidyDatasets <- function(datasets) {
    if (!is.list(datasets))
        stop(gettextf("'%s' has class \"%s\"",
                      "datasets", class(datasets)))
    if (identical(length(datasets), 0L))
        stop(gettextf("'%s' has length %d",
                      "datasets", 0L))
    names.datasets <- names(datasets)
    checkListNames(names = names.datasets,
                   listName = "datasets")
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        if (!methods::is(dataset, "Counts"))
            stop(gettextf("dataset '%s' has class \"%s\"",
                          names.datasets[i], class(dataset)))
        if (any(round(dataset) != dataset, na.rm = TRUE))
            stop(gettextf("dataset '%s' has non-integer values",
                          names.datasets[i]))
        if (any(dataset < 0, na.rm = TRUE))
            stop(gettextf("dataset '%s' has negative values",
                          names.datasets[i]))
        dataset <- dembase::toInteger(dataset)
        ans[[i]] <- dataset
        names(ans) <- names.datasets
    }
    ans
}

## HAS_TESTS
checkAndTidyExposure <- function(exposure, y) {
    if (is.null(exposure))
        return(NULL)
    if (!methods::is(exposure, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "exposure", class(exposure)))
    exposure <- dembase::makeCompatible(x = exposure, y = y, subset = TRUE)
    if (any(exposure[!is.na(exposure)] < 0))
        stop(gettextf("'%s' has negative values",
                      "exposure"))
    if (all(exposure[!is.na(exposure)] == 0))
        stop(gettextf("'%s' has no non-zero values",
                      "exposure"))
    if (any(is.na(exposure) > is.na(y)))
        stop(gettextf("'%s' has missing values in places where '%s' does not",
                      "exposure", "y"))
    exposure
}

## HAS_TESTS
checkAndTidyListArgForEstimateFun <- function(arg, name = c("data", "aggregate", "lower", "upper"),
                                              isCounts = TRUE) {
    name <- match.arg(name)
    if (isCounts)
        names.expected <- c("model", "dataModels")
    else
        names.expected <- c("systemModels", "dataModels")
    if (!is.list(arg))
        stop(gettextf("'%s' has class \"%s\"",
                      name))
    if (identical(length(arg), 0L))
        return(NULL)
    names <- names(arg)
    if (is.null(names))
        stop(gettextf("'%s' does not have names",
                      name))
    if (any(is.na(names)))
        stop(gettextf("names for '%s' have missing values",
                      name))
    if (any(!nzchar(names)))
        stop(gettextf("names for '%s' have blanks",
                      name))
    if (any(duplicated(names)))
        stop(gettextf("names for '%s' have duplicates",
                      name))
    is.valid.name <- names %in% names.expected
    if (any(!is.valid.name))
        stop(gettextf("invalid name for '%s' : \"%s\"",
                      name, names[!is.valid.name][1L]))
    if (!isCounts) {
        systemModels <- arg[["systemModels"]]
        if (!is.list(systemModels))
            stop(gettextf("element \"%s\" of '%s' does not have class \"%s\"",
                          "systemModels", name, "list"))
    }
    dataModels <- arg[["dataModels"]]
    if (!is.list(dataModels))
        stop(gettextf("element \"%s\" of '%s' does not have class \"%s\"",
                      "dataModels", name, "list"))
    if (name == "aggregate")
        if (!all(sapply(unlist(arg), methods::is, "SpecAggregate")))
            stop(gettextf("'%s' contains elements not of class \"%s\"",
                          name, "SpecAggregate"))
    if (name %in% c("lower", "upper"))
        if (!all(sapply(unlist(arg), is.numeric)))
            stop(gettextf("'%s' contains elements not of type \"%s\"",
                          name, "numeric"))
    arg
}


## HAS_TESTS
checkAndTidySDAg <- function(sd, value, metadata) {
    length.sd <- length(sd)
    length.value <- length(value)
    if (methods::is(sd, "DemographicArray")) {
        ## 'sd' and 'value' have same length
        if (!identical(length.sd, length.value))
            stop(gettextf("'%s' and '%s' have different lengths",
                          "sd", "value"))
        ## 'sd' has same metadata as 'value'
        metadata.sd <- sd@metadata
        if (!isTRUE(all.equal(metadata.sd, metadata)))
            stop(gettextf("'%s' and '%s' have different metadata",
                          "sd", "value"))
        ## 'sd' has no missing values"
        if (any(is.na(sd)))
            stop(gettextf("'%s' has missing values",
                          "sd"))
        ## 'sd' has no negative values
        if (any(sd < 0))
            stop(gettextf("'%s' has negative values",
                          "sd"))
        sd <- as.double(sd@.Data)
    }
    else if (methods::is(sd, "numeric")) {
        ## 'sd' has length 1
        if (!identical(length.sd, 1L))
            stop(gettextf("'%s' is numeric but does not have length %d",
                          "sd", 1L))
        ## 'sd' is not missing
        if (is.na(sd))
            stop(gettextf("'%s' is missing",
                          "sd"))
        ## 'sd' is non-negative
        if (sd < 0)
            stop(gettextf("'%s' is negative",
                          "sd"))
        sd <- as.double(sd)
        sd <- rep(sd, times = length.value)
    }
    else {
        stop(gettextf("'%s' has class \"%s\"",
                      "sd", class(sd)))
    }
    methods::new("ScaleVec", sd)
}

## HAS_TESTS
checkAndTidyWeights <- function(weights, y) {
    if (is.null(weights))
        return(NULL)
    if (!methods::is(weights, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "weights", class(weights)))
    weights <- dembase::makeCompatible(x = weights, y = y, subset = TRUE)
    weights <- dembase::toDouble(weights)
    if (any(weights[!is.na(weights)] < 0))
        stop(gettextf("'%s' has negative values",
                      "weights"))
    if (any(is.na(weights) > is.na(y)))
        stop(gettextf("'%s' has missing values in places where '%s' does not",
                      "weights", "y"))
    weights
}

## HAS_TESTS
## Used with Normal, so does not check for Counts, negatives, or non-integer
checkAndTidyY <- function(y, impute = FALSE) {
    ## 'y' is DemographicArray
    if (!methods::is(y, "DemographicArray"))
        stop(gettextf("'%s' has class \"%s\"",
                      "y", class(y)))
    ## 'y' has no zero-length dimensions
    is.zero <- dim(y) == 0L
    if (any(is.zero))
        stop(gettextf("dimension \"%s\" of '%s' has length %d",
                      names(y)[is.zero][1L], "y", dim(y)[is.zero][1L]))
    ## 'y' does not have iteration dimension
    is.iter <- dembase::dimtypes(y) == "iteration"
    if (any(is.iter))
        stop(gettextf("dimension \"%s\" of '%s' has dimtype \"%s\"",
                      names(y)[is.iter], "y", "iteration"))
    ## 'y' does not have quantile dimension
    is.quantile <- dembase::dimtypes(y) == "quantile"
    if (any(is.quantile))
        stop(gettextf("dimension \"%s\" of '%s' has dimtype \"%s\"",
                      names(y)[is.quantile], "y", "quantile"))
    ## 'y' has at least 2 non-missing values
    if (sum(!is.na(y)) < 2L)
        stop(gettextf("'%s' has fewer than %d non-missing values",
                      "y", 2L))
    ## return 'y'
    y
}

## HAS_TESTS
checkAxAg <- function(ax, value) {
    if (is.null(ax))
        return(ax)
    if (length(ax) == 0L)
        stop(gettextf("'%s' has length %d",
                      "ax", 0L))
    if (!methods::is(ax, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "ax", class(ax)))
    names <- names(ax)
    dim <- dim(ax)
    dimtypes <- dimtypes(ax, use.names = FALSE)
    DimScales <- DimScales(ax, use.names = FALSE)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    if (!has.age)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "ax", "dimtype", "age"))
    DimScale.age <- DimScales[[i.age]]
    if (!methods::is(DimScale.age, "Intervals"))
        stop(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                      "ax", "dimtype", "age", "dimscale", "Intervals"))
    n.dim <- length(dim)
    if (n.dim > 1L) {
        if (!methods::is(value, "DemographicArray"))
            stop(gettextf("'%s' is not a demographic array, but '%s' has more than one dimension",
                          "value", "ax"))
        metadata.ax.no.age <- methods::new("MetaData",
                                  nms = names[-i.age],
                                  dimtypes = dimtypes[-i.age],
                                  DimScales = DimScales[-i.age])
        .Data.ax.no.age <- array(0,
                                 dim = dim(metadata.ax.no.age),
                                 dimnames = dimnames(metadata.ax.no.age))
        ax.no.age <- methods::new("Values",
                                  .Data = .Data.ax.no.age,
                                  metadata = metadata.ax.no.age)
        return.value <- tryCatch(dembase::canMakeCompatible(x = ax.no.age,
                                                            y = value,
                                                            subset = TRUE),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("'%s' and '%s' not compatible : %s",
                          "ax", "value", return.value$message))
    }
    NULL
}

## HAS_TESTS
checkConcordances <- function(concordances) {
    ## 'concordances' is a list
    if (!is.list(concordances))
        stop(gettextf("'%s' has class \"%s\"",
                      "concordances", class(concordances)))
    if (identical(length(concordances), 0L))
        return(NULL)
    names <- names(concordances)
    ## all elements of 'concordances' have class "ManyToOne"
    if (!all(sapply(concordances, methods::is, "ManyToOne")))
        return(gettextf("'%s' has elements not of class \"%s\"",
                        "concordances"))
    ## 'concordances' has names
    if (is.null(names))
        stop(gettextf("'%s' does not have names",
                      "concordances"))
    ## no duplicated names for 'concordances'
    if (any(duplicated(names)))
        stop(gettextf("'%s' has duplicate names",
                      "concordances"))
    NULL
}

## HAS_TESTS
checkForMarginalTerms <- function(formula) {
    terms <- stats::terms(formula)
    intercept <- attr(terms, "intercept")
    if (!identical(intercept, 1L))
        stop(gettextf("formula '%s' does not include an intercept",
                      deparse(formula)))
    factor <- attr(terms, "factor")
    if (length(factor) > 0L) {
        col.has.2 <- apply(factor, 2, function(x) any(x == 2L)) ## at most one TRUE
        if (any(col.has.2)) {
            term.without.marginal <- colnames(factor)[col.has.2]
            is.1 <- factor[ , col.has.2] == 1L
            missing.marginal <- rownames(factor)[is.1]
            missing.marginal <- paste(missing.marginal, collapse = ":")
            stop(gettextf("term '%s' is marginal to term '%s' but is not included in formula '%s'",
                          missing.marginal, term.without.marginal, deparse(formula)))
        }
    }
    else
        NULL
}

## HAS_TESTS
checkFormulaMu <- function(formula) {
    correct.length <- identical(length(formula), 3L)
    if (!correct.length)
        stop(gettextf("'%s' is not a valid formula",
                      deparse(formula)))
    if (!identical(deparse(formula[[2L]]), "mean"))
        stop(gettextf("formula '%s' does not have response '%s'",
                      deparse(formula), "mean"))
    NULL
}

## HAS_TESTS
checkFilename <- function(filename, name = "filename") {
    ## filename is character
    if (!is.character(filename))
        stop(gettextf("'%s' does not have type \"%s\"",
                      name, "character"))
    ## 'filename' has length 1
    if (!identical(length(filename), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    ## 'filename' is not missing
    if (is.na(filename))
        stop(gettextf("'%s' is missing",
                      name))
    NULL
}

## HAS_TESTS
checkFunAg <- function(FUN) {
    if (!is.function(FUN))
        stop(gettextf("'%s' has class \"%s\"",
                      "FUN", class(FUN)))
    formals.obtained <- formals(FUN)
    formals.expected <- formals(function(x, weights) NULL)
    if (!identical(formals.obtained, formals.expected))
        stop(gettextf("'%s' does not have formal arguments '%s' and '%s'",
                      "FUN", "x", "weights"))
    NULL
}

## HAS_TESTS
checkLengthDimInFormula <- function(y, formula, minLength = 2L) {
    terms.without.response <- stats::terms(formula[-2L])
    names.terms <- rownames(attr(terms.without.response, "factors"))
    lengths <- dim(y)[match(names.terms, names(y))]
    too.short <- lengths < minLength
    if (any(too.short))
        stop(gettextf("dimension \"%s\" is used in formula '%s' but has length %d",
                      names.terms[too.short][1L],
                      deparse(formula),
                      dim(y)[too.short][1L]))
    NULL
}

## HAS_TESTS
checkListNames <- function(names, listName) {
    if (is.null(names))
        stop(gettextf("'%s' does not have names",
                      listName))
    if (any(is.na(names)))
        stop(gettextf("names for '%s' has missing values",
                      listName))
    if (!all(nzchar(names)))
        stop(gettextf("names for '%s' has blanks",
                      listName))
    if (any(duplicated(names)))
        stop(gettextf("names for '%s' has duplicates",
                      listName))
    NULL
}

## HAS_TESTS
checkDataModels <- function(dataModels, needsNonDefaultSeriesArg = FALSE) {
    ## 'dataModels' is a list
    if (!is.list(dataModels))
        stop(gettextf("'%s' has class \"%s\"",
                      "dataModels", class(dataModels)))
    for (i in seq_along(dataModels)) {
        obs <- dataModels[[i]]
        ## all elements have class "SpecModel"
        if (!methods::is(obs, "SpecModel"))
            stop(gettextf("element %d of '%s' has class \"%s\"",
                          i, "dataModels", class(obs)))
        ## element uses exposure
        if (!obs@useExpose@.Data)
            stop(gettextf("model %d of '%s' does not use exposure",
                          i, "dataModels"))
        ## element has name
        if (is.na(obs@nameY@.Data) || !nzchar(obs@nameY@.Data))
            stop(gettextf("element %d of '%s' has no name for response variable",
                          i, "dataModels"))
        ## specification of model is valid
        return.value <- tryCatch(methods::validObject(obs),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("error in data model for '%s' : %s",
                          obs@nameY@.Data, return.value$message))
        if (needsNonDefaultSeriesArg) {
            ## 'series' argument supplied if needed
            if (identical(obs@series@.Data, "y"))
                stop(gettextf("'%s' argument not supplied in data model for '%s'",
                              "series", obs@nameY))
        }
        else {
            ## no 'series' argument supplied if not needed
            if (!identical(obs@series@.Data, "y"))
                warning(gettextf("non-default argument for '%s' in data model for '%s' ignored",
                                 "series", obs@nameY))
        }
    }
    NULL
}

## HAS_TESTS
checkSpecWeightAg <- function(weights, metadata) {
    if (is.null(weights))
        NULL
    else {
        if (!methods::is(weights, "Counts"))
            stop(gettextf("'%s' has class \"%s\"",
                          "weights", class(weights)))
        ## if (any(weights < 0, na.rm = TRUE))
        ##     stop(gettextf("'%s' has negative values",
        ##                   "weights"))
        if (!is.null(metadata)) {
            .Data <- array(1L,
                           dim = dim(metadata),
                           dimnames = dimnames(metadata))
            y <- methods::new("Values", .Data = .Data, metadata = metadata)
            return.value <- tryCatch(dembase::canMakeCompatible(x = weights,
                                                                y = y,
                                                                subset = TRUE),
                                     error = function(e) e)
            if (methods::is(return.value, "error"))
                stop(gettextf("'%s' and '%s' not compatible : %s",
                              "weights", "value", return.value$message))
        }
    }
    NULL
}

## HAS_TESTS
checkTermsFromFormulaFound <- function(y, formula) {
    terms.without.response <- stats::terms(formula[-2L])
    names.terms <- rownames(attr(terms.without.response, "factors"))
    not.in.y <- !(names.terms %in% names(y))
    n.not.in.y <- sum(not.in.y)
    if (n.not.in.y > 0L)
        stop(sprintf(ngettext(n.not.in.y,
                              "dimension %s from formula '%s' not found",
                              "dimensions %s from formula '%s' not found"),
                     paste(dQuote(names.terms[not.in.y]), collapse = ", "),
                     deparse(formula)))
    NULL
}

## HAS_TESTS
## Function 'loglm' arranges and names 'beta' according to order of
## dimensions used in data.  Normally functions using a formula
## arrange and name terms according to the order they appear in the formula.
## 'convertToFormulaOrder' converts from 'loglm' format to standard format.
convertToFormulaOrder <- function(betas, formulaMu) {
    n.beta <- length(betas)
    terms <- stats::terms(formulaMu)
    factors <- attr(terms, "factors")
    dims.in.formula <- rownames(factors)
    ## when a term is an array, permute the array and change the name
    ## so that dimensions occur in the same order that they do in 'formula'
    for (i in seq_along(betas)) {
        beta <- betas[[i]]
        if (is.array(beta)) {
            names.dims <- names(dimnames(beta))
            perm <- order(match(names.dims, dims.in.formula))
            beta <- aperm(beta, perm = perm)
            betas[[i]] <- beta
            names(betas)[i] <- paste(names(dimnames(beta)), collapse = ":")
        }
    }
    ## reorder terms so that they appear in right order
    term.labels <- attr(terms, "term.labels")
    term.labels <- c("(Intercept)", term.labels)
    betas <- betas[match(term.labels, names(betas))]
    betas
}




## need to have Cross default for orig-dest or parent-child
## HAS_TESTS
defaultPrior <- function(beta, metadata) {
    is.intercept <- is.null(metadata)
    if (is.intercept)
        return(ExchFixed())
    dim <- dim(metadata)
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    n.beta <- length(beta)
    name.term <- paste(names, collapse = ":")
    if (n.beta < 2L)
        stop(gettextf("'%s' for \"%s\" has length %d",
                      "beta", name.term, n.beta))
    if (n.beta != prod(dim))
        stop(gettextf("length of '%s' for \"%s\" [%d] not equal to product of dimensions [%d]",
                      "beta", name.term, n.beta, prod(dim)))
    is.main.effect <- identical(length(dim), 1L)
    i.time <- match("time", dimtypes, nomatch = 0L)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    has.age <- i.age > 0L
    n.beta <- length(beta)
    if (n.beta <= 2L) {
        ExchFixed()
    }
    else {
        if (is.main.effect) {
            if (has.time) {
                DLM()
            }
            else
                Exch()
        }
        else {
            if (has.time) {
                along <- names[i.time]
                DLM(along = along, trend = NULL)
            }
            else
                Exch()
        }
    }
}


## HAS_TESTS
formulaIsInterceptOnly <- function(formula) {
    terms <- stats::terms(formula)
    identical(attr(terms, "factors"), integer(0)) &&
        identical(attr(terms, "intercept"), 1L)
}

## HAS_TESTS
imputeCountsInternal <- function(object, max = NULL) {
    has.subtotals <- methods::is(object, "HasSubtotals")
    if (has.subtotals) {
        subtotals <- object@subtotals
        metadata.subtotals <- object@metadataSubtotals
        transform.subtotals <- object@transformSubtotals
    }
    if (any(is.na(object))) {
        object <- impute(object, max = max)
        if (has.subtotals)
            object <- methods::new("CountsWithSubtotalsInternal",
                                   .Data = object@.Data,
                                   metadata = object@metadata,
                                   subtotals = subtotals,
                                   metadataSubtotals = metadata.subtotals,
                                   transformSubtotals = transform.subtotals)
    }
    object
}

## HAS_TESTS
initialDataModels <- function(dataModels, datasets, y, transforms) {
    ans <- vector(mode = "list", length = length(dataModels))
    for (i in seq_along(ans)) {
        ans[[i]] <- initialModel(object = dataModels[[i]],
                                 y = datasets[[i]],
                                 exposure = dembase::collapse(y, transform = transforms[[i]]))
    }
    ans
}

## HAS_TESTS
jitterBetas <- function(betas, priorsBetas) {
    kIntercept <- 0.1
    kTerms <- 0.25
    betas[[1L]] <- stats::rnorm(n = 1L,
                                mean = betas[[1L]],
                                sd = kIntercept * abs(betas[[1L]]))
    if (length(betas) > 1L) {
        for (i in seq_along(betas)[-1L]) {
            val <- stats::rnorm(n = length(betas[[i]]),
                                mean = betas[[i]],
                                sd = kTerms * stats::sd(betas[[i]]))
            prior <- priorsBetas[[i]]
            all.struc.zero <- prior@allStrucZero
            val[all.struc.zero] <- NA
            betas[[i]] <- val
        }
    }
    betas
}

## HAS_TESTS
makeCellInLikHelper <- function(transform, y, strucZeroArray) {
    ans <- logical(length = length(y))
    for (i in seq_along(y)) {
        is.missing <- is.na(y[i])
        if (is.missing) {
            i.after <- dembase::getIAfter(i = i,
                                          transform = transform,
                                          check = FALSE,
                                          useC = TRUE)
            contributes <- i.after > 0L
            ans[i] <- contributes
        }
        else
            ans[i] <- TRUE
    }
    if (!is.null(strucZeroArray)) {
        is.zero <- strucZeroArray == 0L
        ans[is.zero] <- FALSE
    }
    ans
}

## HAS_TESTS
makeComponentWeightMix <- function(dimBeta, iAlong, indexClassMaxMix,
                                   levelComponent, omegaComponent) {
    n.along <- dimBeta[iAlong]
    indexClassMaxMix <- indexClassMaxMix@.Data
    levelComponent <- levelComponent@.Data
    omegaComponent <- omegaComponent@.Data
    componentWeightMix <- stats::rnorm(n = n.along * indexClassMaxMix,
                                       mean = levelComponent,
                                       sd = omegaComponent)
    methods::new("ParameterVector", componentWeightMix)
}

## HAS_TESTS
makeCountsPred <- function(modelPred) {
    metadata <- modelPred@metadataY
    .Data <- array(NA_integer_,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    has.struc.zero <- methods::is(modelPred, "StrucZeroArrayMixin")
    if (has.struc.zero) {
        struc.zero.array <- modelPred@strucZeroArray@.Data
        .Data[struc.zero.array == 0L] <- 0L
    }
    methods::new("Counts",
                 .Data = .Data,
                 metadata = metadata)
}

## HAS_TESTS
makeDims <- function(dim, margins) {
    ans <- list(0L)
    if (length(margins) > 0L) {
        extra <- lapply(margins[-1L], function(x) dim[x])
        ans <- c(ans, extra)
    }
    ans
}

## HAS_TESTS
makeIAlong <- function(along, metadata) {
    kContinuousDimtypes <- c("age", "cohort", "time")
    continuous.labels <- paste(dQuote(kContinuousDimtypes), collapse = ", ")
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    is.continuous <- dimtypes %in% kContinuousDimtypes
    if (is.null(along)) {
        n.continuous <- sum(is.continuous)
        if (n.continuous == 0L)
            stop(gettextf("cannot use random walk prior when no dimensions have dimtype %s",
                          continuous.labels))
        if (n.continuous >= 2L)
            stop(gettextf("more than one dimension with dimtype %s, but '%s' not specified",
                          continuous.labels, "along"))
        along <- which(is.continuous)
    }
    else {
        along <- match(along, names, nomatch = 0L)
        if (along == 0L)
            stop(gettextf("'%s' outside valid range", "along"))
        dimtype.along <- dimtypes[along]
        if (!(dimtype.along %in% kContinuousDimtypes))
            stop(gettextf("cannot have random walk along dimension \"%s\" because dimension has dimtype \"%s\"",
                          names[along], dimtypes[along]))
    }
    DimScale.along <- DimScales[[along]]
    steps <- dembase::stepLengths(DimScale.along)
    steps <- steps[is.finite(steps)]
    if (length(steps) > 1L) {
        all.equal <- all(mapply(identical, steps[1L], steps[-1L]))
        if (!all.equal)
            stop(gettextf("cannot have random walk along dimension \"%s\" because steps irregular",
                          names[along]))
    }
    along
}

## HAS_TESTS
makeIteratorProdVectorMix <- function(dimBeta, iAlong) {
    dim <- dimBeta
    dim[iAlong] <- 1L
    MarginIterator(dim = dim)
}

## HAS_TESTS
makeLatentWeightMix <- function(dimBeta, iAlong, iteratorsDimsMix,
                                indexClassMix, indexClassMaxMix,
                                weightMix) {
    n.along <- dimBeta[iAlong]
    iterator.beta <- iteratorsDimsMix[[iAlong]]
    indexClassMaxMix <- indexClassMaxMix@.Data
    weightMix <- weightMix@.Data
    weightMix <- matrix(weightMix,
                        nrow = n.along,
                        ncol = indexClassMaxMix)
    J <- prod(dimBeta)
    ans <- numeric(length = J)
    iterator.beta <- resetS(iterator.beta,
                            useC = TRUE)
    for (i.along in seq_len(n.along)) {
        indices.beta <- iterator.beta@indices
        for (i.beta in indices.beta) {
            i.class <- indexClassMix[i.beta]
            v <- weightMix[i.along, i.class]
            ans[i.beta] <- stats::runif(n = 1L,
                                        min = 0,
                                        max = v)
        }
        iterator.beta <- advanceS(iterator.beta,
                                  useC = TRUE)
    }
    methods::new("UnitIntervalVec", ans)
}

## HAS_TESTS
makeLevelComponentWeightMix <- function(dimBeta, iAlong, indexClassMaxMix,
                                        phiMix, meanLevel, omegaLevel) {
    n.along <- dimBeta[iAlong]
    indexClassMaxMix <- indexClassMaxMix@.Data
    meanLevel <- meanLevel@.Data
    omegaLevel <- omegaLevel@.Data
    ans <- matrix(nrow = n.along,
                  ncol = indexClassMaxMix)
    mean.initial <- meanLevel / (1 - phiMix)
    sd.initial <- omegaLevel / sqrt(1 - phiMix^2)
    sd.rest <- omegaLevel
    ans[1L, ] <- stats::rnorm(n = indexClassMaxMix,
                              mean = mean.initial,
                              sd = sd.initial)
    for (i in seq.int(from = 2L, to = n.along)) {
        mean.i <- meanLevel + phiMix * ans[i - 1L, ]
        ans[i, ] <- stats::rnorm(n = indexClassMaxMix,
                                 mean = mean.i,
                                 sd = sd.rest)
    }
    ans <- as.double(ans)
    ans <- methods::new("ParameterVector", ans)
}

## HAS_TESTS
makeLinearBetas <- function(theta, formula) {
    checkForMarginalTerms(formula)
    data <- data.frame(expand.grid(dimnames(theta)), mean = as.numeric(theta))
    names(data)[length(data)] <- extractResponse(formula)
    mod <- stats::lm(formula, data = data)
    mod <- stats::aov(mod)
    if (mod$rank > 1L) {
        ans <- stats::model.tables(mod)$tables
        one.dim <- sapply(ans, function(x) identical(length(dim(x)), 1L))
        ans[one.dim] <- lapply(ans[one.dim], as.numeric)
        ans[!one.dim] <- lapply(ans[!one.dim], as.array)
    }
    else
        ans <- list()
    ans <- c(list("(Intercept)" = mean(theta)), ans)
    ans
}

## HAS_TESTS
makeMargins <- function(betas, y) {
    names.betas <- names(betas)
    n.beta <- length(betas)
    dimnames.y <- dimnames(y)
    names.y <- names(dimnames.y)
    ans <- vector(mode = "list", length = n.beta)
    ans[[1L]] <- 0L
    if (n.beta > 1L) {
        for (i in seq.int(from = 2L, to = n.beta)) {
            names.dims <- names(dimnames(betas[[i]]))
            if (is.null(names.dims))
                ans[[i]] <- match(names.betas[i], names.y)
            else
                ans[[i]] <- match(names.dims, names.y)
        }
    }
    ans
}

## HAS_TESTS
makeMeanLevelComponentWeightMix <- function(priorMean, priorSD) {
    mean <- priorMean@.Data
    sd <- priorSD@.Data
    ans <- stats::rnorm(n = 1L,
                        mean = mean,
                        sd = sd)
    methods::new("Parameter", ans)
}

## HAS_TESTS
makeNamesEta <- function(spec) {
    formula <- spec@innerFormula
    terms <- stats::terms(formula)
    ans <- attr(terms, "term.labels")
    has.intercept <- identical(attr(terms, "intercept"), 1L)
    if (has.intercept)
        ans <- c("(Intercept)", ans)
    ans
}

## HAS_TESTS
makeNamesSpecsPriors <- function(dots) {
    if (length(dots) > 0L)
        sapply(dots, extractResponse)
    else
        character()
}

## HAS_TESTS
## priors follow order implied by formula - not order implied by metadatax
makePriors <- function(betas, specs, namesSpecs, margins, y, sY, strucZeroArray) {
    n.beta <- length(betas)
    ans <- vector(mode = "list", length = n.beta)
    names.betas <- names(betas)
    dim.y <- dim(y)
    s <- seq_along(dim.y)
    metadata.y <- y@metadata
    for (i in seq_len(n.beta)) {
        beta <- betas[[i]]
        beta <- as.double(beta)
        name <- names.betas[i]
        if (any(is.na(beta)))
            stop(gettextf("'%s' for \"%s\" has missing values",
                          "beta", name))
        margin <- margins[[i]]
        if (identical(margin, 0L))
            metadata <- NULL
        else
            metadata <- metadata.y[margin]
        is.saturated <- identical(sort(margin), s)
        i.spec <- match(name, namesSpecs, nomatch = 0L)
        has.spec <- i.spec > 0L
        if (has.spec)
            spec <- specs[[i.spec]]
        else
            spec <- defaultPrior(beta = beta,
                                 metadata = metadata)
        ans[[i]] <- initialPrior(object = spec,
                                 beta = beta,
                                 metadata = metadata,
                                 sY = sY,
                                 isSaturated = is.saturated,
                                 margin = margin,
                                 strucZeroArray = strucZeroArray)
    }
    ans
}

## HAS_TESTS
makeProdVectorsMix <- function(vectorsMix, iAlong, dimBeta, indexClassMaxMix) {
    index.class.max <- indexClassMaxMix@.Data
    vectors <- vectorsMix[-iAlong]
    dim <- dimBeta[-iAlong]
    vectors <- lapply(vectors, methods::slot, ".Data")
    for (i.vector in seq_along(vectors))
        vectors[[i.vector]] <- matrix(vectors[[i.vector]],
                                      nrow = dim[i.vector],
                                      ncol = index.class.max)
    ans <- vector(mode = "list",
                  length = index.class.max)
    for (i.class in seq_len(index.class.max)) {
        vectors.i.class <- lapply(vectors, function(x) x[ , i.class])
        ans[[i.class]] <- Reduce(`%o%`, vectors.i.class)
    }
    ans <- unlist(ans)
    methods::new("ParameterVector", ans)
}

## HAS_TESTS
makeSigmaInitialPoisson <- function(y, exposure = NULL) {
    kMinimumValue <- 0.01
    y <- as.numeric(y)
    is.missing <- is.na(y)
    if (any(is.missing)) {
        ## 'y' has at least 1 non-missing value
        if (all(is.missing))
            stop(gettextf("'%s' has no non-missing values",
                          "y"))
        y <- y[!is.missing]
        exposure <- exposure[!is.missing]
        Recall(y = y, exposure = exposure)
    }
    ## 'y' is finite
    if (any(is.infinite(y)))
        stop(gettextf("'%s' has non-finite values",
                      "y"))
    ## 'y' not all 0
    if (all(y == 0))
        stop(gettextf("'%s' has no non-zero values",
                      "y"))
    if (is.null(exposure))
        theta <- y
    else {
        exposure <- as.numeric(exposure)
        ## 'exposure' has no missing values
        if (any(is.na(exposure)))
            stop(gettextf("'%s' has missing values",
                          "exposure"))
        ## 'exposure' is finite
        if (any(is.infinite(exposure)))
            stop(gettextf("'%s' has non-finite values",
                          "exposure"))
        ## 'exposure' not all 0
        if (all(exposure == 0))
            stop(gettextf("'%s' has no non-zero values",
                          "exposure"))
        non.zero <- exposure > 0
        theta <- y[non.zero] / exposure[non.zero]
    }
    if (length(theta) > 1L) {
        theta <- 0.5 * (theta + mean(theta))
        log.theta <- log(theta)
        ans <- stats::sd(log.theta)
        ans <- max(ans, kMinimumValue)
        ans
    }
    else
        1.0
}

## NO_TESTS
makeSpecsPriors <- function(dots) {
    ans <- vector(mode = "list", length = length(dots))
    for (i in seq_along(dots)) {
        formula <- dots[[i]]
        if (!methods::is(formula, "formula"))
            stop(gettextf("'%s' is not a formula",
                          deparse(substitute(formula))))
        if (!hasResponse(formula))
            stop(gettextf("formula '%s' does not include a response",
                          deparse(formula)))
        right.hand.side <- formula[[3L]]
        ans[[i]] <- eval(right.hand.side,
                         envir = environment(formula))
    }
    ans
}

## HAS_TESTS
makeTransformsYToDatasets <- function(y, datasets, namesDatasets) {
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        transform <- tryCatch(dembase::makeTransform(x = y,
                                                     y = dataset,
                                                     subset = TRUE,
                                                     check = TRUE),
                              error = function(e) e)
        if (methods::is(transform, "error"))
            stop(gettextf("unable to collapse '%s' to make it compatible with dataset '%s' : %s",
                          "y", namesDatasets[i], transform$message))
        transform <- dembase::makeCollapseTransformExtra(transform)
        ans[[i]] <- transform
    }
    ans
}

## HAS_TESTS
makeValueAndMetaDataAg <- function(value) {
    if (methods::is(value, "DemographicArray")) {
        if (any(is.na(value)))
            stop(gettextf("'%s' has missing values",
                          "value"))
        metadata <- value@metadata
        value <- as.double(value@.Data)
    }
    else if (is.numeric(value)) {
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "value", 1L))
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          "value"))
        metadata <- NULL
        value <- as.double(value)
    }
    else
        stop(gettextf("'%s' has class \"%s\"",
                      "value", class(value)))
    value <- methods::new("ParameterVector", value)
    list(value = value, metadata = metadata)
}

## HAS_TESTS
makeVectorsMix <- function(dimBeta, iAlong, indexClassMaxMix,
                           omegaVectorsMix) {
    n.dim <- length(dimBeta)
    sd <- omegaVectorsMix@.Data
    length <- dimBeta * indexClassMaxMix
    length[iAlong] <- 0L
    ans <- vector(mode = "list",
                  length = n.dim)
    for (i in seq_len(n.dim)) {
        .Data <- stats::rnorm(n = length[i],
                              sd = sd)
        ans[[i]] <- methods::new("ParameterVector", .Data)
    }
    ans
}

## HAS_TESTS
makeWeightAg <- function(weight, default, model, thetaObj, transform, values) {
    if (is.null(weight)) {
        if (is.null(default))
            stop(gettext("no aggregate weights supplied, and no default weights"))
        else
            ans <- default
    }
    else {
        ans <- tryCatch(dembase::makeCompatible(x = weight,
                                                y = thetaObj,
                                                subset = TRUE),
                        error = function(e) e)
        if (methods::is(ans, "error"))
            stop(gettextf("%s not compatible with '%s' : %s",
                          "aggregate weight", "y", ans$message))
    }
    ans <- as.double(ans)
    for (i in seq_along(ans)) {
        k <- dembase::getIAfter(i,
                                transform = transform,
                                useC = TRUE)
        if (k == 0L)
            ans[i] <- NA
        else {
            if (is.na(ans[i]))
                stop(gettextf("element %d of '%s' is needed for aggregate values but is missing",
                              i, "weightAg"))
        }
    }
    normalise <- is.null(weight) && !methods::is(model, "PoissonVaryingNotUseExp")
    if (normalise) {
        for (k in seq_along(values)) {
            i.wt <- dembase::getIBefore(k, transform = transform, useC = TRUE)
            wt <- ans[i.wt]
            sum.wt <- sum(wt)
            if (sum.wt > 0)
                wt <- wt / sum.wt
            ans[i.wt] <- wt
        }
    }
    ans
}

## HAS_TESTS
makeWeightMix <- function(dimBeta, iAlong, indexClassMaxMix,
                          componentWeightMix) {
    n.along <- dimBeta[iAlong]
    index.class.max <- indexClassMaxMix@.Data
    component.weight <- componentWeightMix@.Data
    ans <- stats::pnorm(component.weight)
    ans <- matrix(ans,
                  nrow = n.along,
                  ncol = index.class.max)
    mult <- 1 - ans
    mult <- apply(mult,
                  MARGIN = 1L,
                  FUN = cumprod)
    mult <- t(mult)
    ans[ , -1L] <- ans[ , -1L] * mult[ , -index.class.max]
    ans <- as.double(ans)
    methods::new("UnitIntervalVec", ans)
}
    

