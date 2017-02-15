
setClass("LevelComponentWeightOldMixMixin",
                  slots = c(levelComponentWeightOldMix = "ParameterVector"),
         contains = "VIRTUAL",
         validity = function(object) {
             levelComponentWeightOldMix <- object@levelComponentWeightOldMix@.Data
             indexClassMaxMix <- object@indexClassMaxMix@.Data
             ## 'levelComponentWeightOldMix' has same length as 'indexClassMaxMix'
             if (!identical(length(levelComponentWeightOldMix),
                            length(indexClassMaxMix)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "levelComponentWeightOldMix",
                                 "indexClassMaxMix"))
             TRUE
         })             





setMethod("predictPrior",
          signature(prior = "MixNormZeroPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_MixNormZero_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictLevelComponentWeightMix(prior)
                  prior <- predictComponentWeightMix(prior)
                  prior <- updateWeightMix(prior)
                  prior <- predictIndexClassMix(prior)
                  prior <- updateAlphaMix(prior)
                  prior
              }
          })

predictLevelComponentWeightMix <- function(prior) {
    level <- prior@levelComponentWeightMix@.Data # 'alpha'; n.along * index.class.max
    level.old <- prior@levelComponentWeightOldMix # index.clas.max
    mean.level <- prior@meanLevelComponentWeightMix@.Data # 'mu'; 1
    dimBeta <- prior@dimBeta
    iAlong <- prior@iAlong
    n.along <- dimBeta[iAlong]
    index.class.max <- prior@indexClassMaxMix@.Data
    phi <- prior@phiMix
    phi.sq <- phi^2
    omega.comp <- prior@omegaComponentWeightMix@.Data # 'epsilon'; 1
    omega.comp.sq <- omega.comp^2
    omega.level <- prior@omegaLevelComponentWeightMix@.Data # 'eta'; 1
    omega.level.sq <- omega.level^2
    prior.mean.first <- mean.level / (1 - phi) 
    prior.var.first <- omega.level.sq / (1 - phi.sq) 
    prior.sd.first <- sqrt(prior.var.first)
    for (i.class in seq_len(index.class.max)) {
        i.wt <- (i.class - 1L) * n.along + 1L
        level.prev <- level.old[i.class]
        mean <- mean.level + phi * level.prev
        level[i.wt] <- rnorm(n = 1L,
                             mean = mean,
                             sd = omega.level)
        for (i.along in seq.int(from = 2L, to = n.along)) {
            i.wt.curr <- (i.class - 1L) * n.along + i.along
            i.wt.prev <- i.wt.curr - 1L
            mean <- mean.level + phi * level[i.wt.prev]
            level[i.wt.curr] <- rnorm(n = 1L,
                                      mean = mean,
                                      sd = omega.level)
        }
    }
    prior@levelComponentWeightMix@.Data <- level
    prior
}


predictComponentWeightMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(methods::is(prior, "Predict"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(predictComponentWeightMix_R, prior)
    }
    else {    
        comp <- prior@componentWeightMix@.Data # W; n.along * index.class.max
        level <- prior@levelComponentWeightMix@.Data # alpha; n.along * index.class.max
        index.class.max <- prior@indexClassMaxMix@.Data # k; J
        omega <- prior@omegaComponentWeightMix@.Data # sigma_epsilon; 1
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        n.along <- dim.beta[iAlong]
        for (i.class in seq_len(index.class.max)) {
            for (i.along in seq_len(n.along)) {
                i.w <- (i.class - 1L) * n.along + i.along
                comp[i.w] <- rnorm(n = 1L,
                                   mean = level[i.w],
                                   sd = omega)
            }
        }
        prior@componentWeightMix@.Data <- comp
        prior
    }
}


## CHANGE betaHat!!!!


predictIndexClassMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(methods::is(prior, "Predict"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(predictIndexClassMix_R, prior)
    }
    else {    
        index.class <- prior@indexClassMix
        weight <- prior@weightMix
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        iteratorsDims <- prior@iteratorsDimsMix
        prob <- prior@yXMix@.Data ## length index.class.max; using for new purpose
        n.along <- dim.beta[iAlong]
        iterator.beta <- iteratorsDims[[iAlong]]
        iterator.beta <- resetS(iterator.beta)
        for (i.along in seq_len(n.along)) {
            indices.beta <- iterator.beta@indices
            sum.wt <- 0
            for (i.class in seq_len(index.class.max)) {
                i.wt <- (i.class - 1L) * n.along + i.along
                prob[i.class] <- weight[i.wt]
                sum.wt <- sum.wt + weight[i.wt]
            }
            cum.sum.wt <- 0
            prob[1L] <- prob[1L] / sum.wt
            for (i.class in seq.int(from = 2L, to = index.class.max))
                prob[i.class] <- prob[i.class - 1L] + prob[i.class] / sum.prob
            for (i.beta in indices.beta) {
                U <- runif(n = 1L)
                for (i.class in seq_len(index.class.max)) {
                    if (U < prob[i])
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


predictAlphaMix <- function(prior, useC = FALSE) {
    ## 'prior'
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    if (useC) {
        .Call(predictAlphaMix_R, prior)
    }
    else {
        alpha <- prior@alphaMix
        index.class <- prior@indexClassMix # 'k'; length J
        index.class.max <- prior@indexClassMaxMix@.Data
        prod.vectors <- prior@prodVectorsMix@.Data # length (J/n.along) * classIndexMax
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        iteratorsDims <- prior@iteratorsDimsMix
        pos1 <- prior@posProdVectors1Mix
        pos2 <- prior@posProdVectors2Mix
        n.beta.no.along <- prior@nBetaNoAlongMix
        iterator.beta <- iteratorsDims[[iAlong]]
        n.along <- dim.beta[iAlong]
        v <- getV(prior)
        iterator.beta <- resetS(iterator.beta)
        for (i.along in seq_len(n.along)) {
            indices.beta <- iterator.beta@indices
            for (i.beta in indices.beta) {
                v.i.beta <- v[i.beta]
                i.beta.no.along <- ((i.beta - 1L) %/% pos1) * pos2 + (i.beta - 1L) %% pos2 + 1L
                i.class <- index.class[i.beta]
                i.prod <- (i.class - 1L) * n.beta.no.along + i.beta.no.along
                val.prod.vector <- prod.vectors[i.prod]
            }
            iterator.beta <- advanceS(iterator.beta)
        }
        prior@indexClassMix <- index.class
        prior
    }
}



setMethod("transferParamPrior",
          signature(prior = "MixNormZeroPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_MixNormZeroPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
              }
          })




## TRANSLATED
## HAS_TESTS
predictAlphaDLMNoTrend <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::is(prior, "NoTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(predictAlphaDLMNoTrend_R, prior)
    }
    else {
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data # numeric vector length (K+1)L
        phi <- prior@phi
        omega <- prior@omegaAlpha@.Data
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                k.curr <- indices[i + 1L]
                k.prev <- indices[i]
                mean <- phi * alpha[k.prev]
                alpha[k.curr] <- stats::rnorm(n = 1L,
                                       mean = mean,
                                       sd = omega)
            }
            iterator <- advanceA(iterator)
        }
        prior@alphaDLM@.Data <- alpha
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


initialMixAllPredict <- function(prior, metadata, name, along) {

    
    AComponentWeightMix <- prior@AComponentWeightMix
    ALevelComponentWeightMix <- prior@ALevelComponentWeightMix
    ATau <- prior@ATau
    AVectorsMix <- prior@AVectorsMix
    along <- prior@along
    indexClassMaxMix <- prior@indexClassMaxMix
    multComponentWeightMix <- prior@multComponentWeightMix
    multLevelComponentWeightMix <- prior@multLevelComponentWeightMix
    multTau <- prior@multTau
    multVectorsMix <- prior@multVectorsMix
    nuComponentWeightMix <- prior@nuComponentWeightMix
    nuLevelComponentWeightMix <- prior@nuLevelComponentWeightMix
    nuTau <- prior@nuTau
    nuVectorsMix <- prior@nuVectorsMix
    omegaComponentWeightMaxMix <- prior@omegaComponentWeightMaxMix
    omegaLevelComponentWeightMaxMix <- prior@omegaLevelComponentWeightMaxMix
    omegaVectorsMaxMix <- prior@omegaVectorsMaxMix
    priorMeanLevelComponentWeightMix <- prior@priorMeanLevelComponentWeightMix
    priorSDLevelComponentWeightMix <- prior@priorSDLevelComponentWeightMix
    tauMax <- prior@tauMax
    ## AComponentWeightMix, omegaComponentWeightMaxMix, omegaComponentWeight
    AComponentWeightMix <-
        makeAHalfT(A = AComponentWeightMix,
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
    phiMix <- runif(n = 1L,
                    min = 0.8,
                    max = 0.98)
    ## meanLevelComponentWeightMix - we want this to be reasonably high, to avoid
    ## starting with too many components
    meanLevelComponentWeightMix <-
        makeMeanLevelComponentWeightMix(priorMean = priorMeanLevelComponentWeightMix,
                                        priorSD = priorSDLevelComponentWeightMix,
                                        min = 0)
    ## levelComponentWeightMix
    levelComponentWeightMix <-
        makeLevelComponentWeightMix(dimBeta = dimBeta,
                                    iAlong = iAlong,
                                    indexClassMaxMix = indexClassMaxMix,
                                    phiMix = phiMix,
                                    meanLevel = meanLevelComponentWeightMix,
                                    omegaLevel = omegaLevelComponentWeightMix)
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
    indexClassMaxPossibleMix <- new("Counter", indexClassMaxPossibleMix)
    ## indexClassMaxUsedMix
    indexClassMaxUsedMix <- max(indexClassMix)
    indexClassMaxUsedMix <- new("Counter", indexClassMaxUsedMix)
    ## indexClassProbMix
    indexClassProbMix <- rep(0, times = indexClassMaxMix@.Data)
    indexClassProbMix <- new("ParameterVector", indexClassProbMix)
    ## foundIndexClassMaxPossibleMix
    foundIndexClassMaxPossibleMix <- new("LogicalFlag", TRUE)
    ## sumsWeightsMix
    sumsWeightsMix <- rep(0, times = dimBeta[iAlong])
    sumsWeightsMix <- new("UnitIntervalVec", sumsWeightsMix)
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
    mMix <- new("ParameterVector", mMix)
    CMix <- new("ParameterVector", CMix)
    aMix <- new("ParameterVector", aMix)
    RMix <- new("ParameterVector", RMix)
    ## yXMix, XXMix
    yXMix <- rep(0, times = indexClassMaxMix@.Data)
    XXMix <- rep(0, times = indexClassMaxMix@.Data)
    yXMix <- new("ParameterVector", yXMix)
    XXMix <- new("ParameterVector", XXMix)
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
         iteratorProdVectorMix = iteratorProdVectorMix,
         iteratorsDimsMix = iteratorsDimsMix,
         J = J,
         latentComponentWeightMix = latentComponentWeightMix,
         latentWeightMix = latentWeightMix,
         levelComponentWeightMix = levelComponentWeightMix,
         mMix = mMix,
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
                         

setMethod("initialPriorPredict",
          signature(object = "SpecMixNormZero"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialMixAllPredict(object = object,
                                            beta = beta,
                                            metadata = metadata,
                                            sY = sY)
              methods::new("MixNormZero",
                           AComponentWeightMix = l.all$AComponentWeightMix,
                           ALevelComponentWeightMix = l.all$ALevelComponentWeightMix,
                           aMix = l.all$aMix,
                           ATau = l.all$ATau,
                           AVectorsMix = l.all$AVectorsMix,
                           alphaMix = l.all$alphaMix,
                           CMix = l.all$CMix,
                           componentWeightMix = l.all$componentWeightMix,
                           dimBeta = l.all$dimBeta,
                           foundIndexClassMaxPossibleMix = l.all$foundIndexClassMaxPossibleMix,
                           iAlong = l.all$iAlong,
                           indexClassMaxMix = l.all$indexClassMaxMix,
                           indexClassMaxPossibleMix = l.all$indexClassMaxPossibleMix,
                           indexClassMaxUsedMix = l.all$indexClassMaxUsedMix,
                           indexClassMix = l.all$indexClassMix,
                           indexClassProbMix = l.all$indexClassProbMix,
                           iteratorsDimsMix = l.all$iteratorsDimsMix,
                           iteratorProdVectorMix = l.all$iteratorProdVectorMix,
                           J = l.all$J,
                           latentComponentWeightMix = l.all$latentComponentWeightMix,
                           latentWeightMix = l.all$latentWeightMix,
                           levelComponentWeightMix = l.all$levelComponentWeightMix,
                           meanLevelComponentWeightMix = l.all$meanLevelComponentWeightMix,
                           mMix = l.all$mMix,
                           nBetaNoAlongMix = l.all$nBetaNoAlongMix,
                           nuComponentWeightMix = l.all$nuComponentWeightMix,
                           nuLevelComponentWeightMix = l.all$nuLevelComponentWeightMix,
                           nuTau = l.all$nuTau,
                           nuVectorsMix = l.all$nuVectorsMix,
                           omegaComponentWeightMaxMix = l.all$omegaComponentWeightMaxMix,
                           omegaComponentWeightMix = l.all$omegaComponentWeightMix,
                           omegaLevelComponentWeightMaxMix = l.all$omegaLevelComponentWeightMaxMix,
                           omegaLevelComponentWeightMix = l.all$omegaLevelComponentWeightMix,
                           omegaVectorsMaxMix = l.all$omegaVectorsMaxMix,
                           omegaVectorsMix = l.all$omegaVectorsMaxMix,
                           phiMix = l.all$phiMix,
                           posProdVectors1Mix = l.all$posProdVectors1Mix,
                           posProdVectors2Mix = l.all$posProdVectors2Mix,
                           priorMeanLevelComponentWeightMix = l.all$priorMeanLevelComponentWeightMix,
                           priorSDLevelComponentWeightMix = l.all$priorSDLevelComponentWeightMix,
                           prodVectorsMix = l.all$prodVectorsMix,
                           RMix = l.all$RMix,
                           sumsWeightsMix = l.all$sumsWeightsMix,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           vectorsMix = l.all$vectorsMix,
                           weightMix = l.all$weightMix,
                           XXMix = l.all$XXMix,
                           yXMix = l.all$yXMix)
          })
