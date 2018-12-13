

## initialPrior #######################################################################

## In 'initialPrior' methods for priors, assume that calling
## function has checked whether 'beta' has any missing values
## and whether its length is at least 2.  Other conditions
## must be checked by 'initialPrior'.

## intercept
setMethod("initialPrior",
          signature(object = "SpecExchFixed", metadata = "NULL"),
          function(object, beta, metadata, sY, isSaturated, ...) {
              mean <- object@mean@.Data
              tau <- object@tau
              if (!isTRUE(all.equal(mean, 0)))
                  warning(gettextf("non-zero mean in '%s' prior for '%s' ignored",
                                   "ExchFixed", paste(names(metadata), collapse = ":")))
              J <- makeJ(beta)
              if (J > 1L)
                  stop(gettextf("'%s' is %s but '%s' is greater than %d",
                                "metadata", "NULL", "J", 1L))
              tau <- makeTauExchFixedIntercept(tau = tau,
                                               sY = sY)
              if (isSaturated)
                  stop(gettextf("using \"%s\" prior for highest-order term in saturated model",
                                "ExchFixed"))
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              if (all(strucZeroArray == 0L))
                  stop(gettext("data consists entirely of structural zeros"))
              methods::new("ExchFixed",
                           J = J,
                           tau = tau,
                           isSaturated = isSaturated,
                           allStrucZero = FALSE)
          })

## non-intercept
setMethod("initialPrior",
          signature(object = "SpecExchFixed"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              tau <- object@tau
              multTau <- object@multTau
              J <- makeJ(beta)
              if (J <= 1L)
                  stop(gettextf("'%s' is not %s but '%s' is less than or equal to %d",
                                "metadata", "NULL", "J", 1L))
              tau <- makeTauExchFixedNonIntercept(tau = tau,
                                                  sY = sY,
                                                  mult = multTau)
              if (isSaturated)
                  stop(gettextf("using \"%s\" prior for highest-order term in saturated model",
                                "ExchFixed"))
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              methods::new("ExchFixed",
                           allStrucZero = allStrucZero,
                           isSaturated = isSaturated,
                           J = J,
                           tau = tau)
          })

setMethod("initialPrior",
          signature(object = "SpecExchNormZero"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              ATau <- object@ATau
              multTau <- object@multTau
              nuTau <- object@nuTau
              tauMax <- object@tauMax
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
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray, margin = margin,
                                               metadata = metadata)
              methods::new("ExchNormZero",
                           ATau = ATau,
                           allStrucZero = allStrucZero,
                           isSaturated = isSaturated,
                           J = J,
                           nuTau = nuTau,
                           tau = tau,
                           tauMax = tauMax)
          })

setMethod("initialPrior",
          signature(object = "SpecExchRobustZero"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              ATau <- object@ATau
              multTau <- object@multTau
              nuBeta <- object@nuBeta
              nuTau <- object@nuTau
              tauMax <- object@tauMax
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
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              UBeta <- makeU(nu = nuBeta,
                             A = ATau,
                             n = J,
                             allStrucZero = allStrucZero)
              methods::new("ExchRobustZero",
                           allStrucZero = allStrucZero,
                           ATau = ATau,
                           isSaturated = isSaturated,
                           J = J,
                           nuBeta = nuBeta,
                           nuTau = nuTau,
                           tau = tau,
                           tauMax = tauMax,
                           UBeta = UBeta)
          })

setMethod("initialPrior",
          signature(object = "SpecExchNormCov"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              ATau <- object@ATau
              multTau <- object@multTau
              nuTau <- object@nuTau
              tauMax <- object@tauMax
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
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = allStrucZero)
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              methods::new("ExchNormCov",
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ATau = ATau,
                           allStrucZero = allStrucZero,
                           contrastsArg = l.cov$contrastsArg,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           infant = l.cov$infant,
                           isSaturated = isSaturated,
                           J = J,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuTau = nuTau,
                           P = l.cov$P,
                           tau = tau,
                           tauMax = tauMax,
                           UEtaCoef = l.cov$UEtaCoef,
                           Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecExchRobustCov"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              ATau <- object@ATau
              multTau <- object@multTau
              nuBeta <- object@nuBeta
              nuTau <- object@nuTau
              tauMax <- object@tauMax
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
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = allStrucZero)
              UBeta <- makeU(nu = nuBeta,
                             A = ATau,
                             n = J,
                             allStrucZero = allStrucZero)
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              methods::new("ExchRobustCov",
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ATau = ATau,
                           allStrucZero = allStrucZero,
                           contrastsArg = l.cov$contrastsArg,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           infant = l.cov$infant,
                           isSaturated = isSaturated,
                           J = J,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           nuBeta = nuBeta,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuTau = nuTau,
                           P = l.cov$P,
                           tau = tau,
                           tauMax = tauMax,
                           UBeta = UBeta,
                           UEtaCoef = l.cov$UEtaCoef,
                           Z = l.cov$Z)
          })


## DLM - Norm, Zero

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendNormZeroNoSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              methods::new("DLMNoTrendNormZeroNoSeason",
                           AAlpha = l.all$AAlpha,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = l.all$ATau,
                           alphaDLM = l.all$alphaDLM,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           CNoTrend = l.no.trend$CNoTrend,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nuAlpha = l.all$nuAlpha,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendNormZeroNoSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              methods::new("DLMWithTrendNormZeroNoSeason",
                           AAlpha = l.all$AAlpha,
                           ADelta = l.with.trend$ADelta,
                           ADelta0 = l.with.trend$ADelta0,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = l.with.trend$GWithTrend,
                           hasLevel = l.with.trend$hasLevel,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanDelta0 = l.with.trend$meanDelta0,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nuAlpha = l.all$nuAlpha,
                           nuDelta = l.with.trend$nuDelta,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaDelta = l.with.trend$omegaDelta,
                           omegaDeltaMax = l.with.trend$omegaDeltaMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = l.with.trend$WSqrt,
                           WSqrtInvG = l.with.trend$WSqrtInvG)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendNormZeroWithSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.season <- initialDLMSeason(object = object,
                                           beta = beta,
                                           metadata = metadata,
                                           sY = sY)
              methods::new("DLMNoTrendNormZeroWithSeason",
                           AAlpha = l.all$AAlpha,
                           ASeason = l.season$ASeason,
                           aNoTrend = l.no.trend$aNoTrend,
                           aSeason = l.season$aSeason,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           CSeason = l.season$CSeason,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nSeason = l.season$nSeason,
                           nuAlpha = l.all$nuAlpha,
                           nuSeason = l.season$nuSeason,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaSeason = l.season$omegaSeason,
                           omegaSeasonMax = l.season$omegaSeasonMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendNormZeroWithSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.season <- initialDLMSeason(object = object,
                                           beta = beta,
                                           metadata = metadata,
                                           sY = sY)
              methods::new("DLMWithTrendNormZeroWithSeason",
                           AAlpha = l.all$AAlpha,
                           ADelta = l.with.trend$ADelta,
                           ADelta0 = l.with.trend$ADelta0,
                           ASeason = l.season$ASeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           aSeason = l.season$aSeason,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           CSeason = l.season$CSeason,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = l.with.trend$GWithTrend,
                           hasLevel = l.with.trend$hasLevel,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           meanDelta0 = l.with.trend$meanDelta0,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nSeason = l.season$nSeason,
                           nuAlpha = l.all$nuAlpha,
                           nuDelta = l.with.trend$nuDelta,
                           nuSeason = l.season$nuSeason,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaDelta = l.with.trend$omegaDelta,
                           omegaDeltaMax = l.with.trend$omegaDeltaMax,
                           omegaSeason = l.season$omegaSeason,
                           omegaSeasonMax = l.season$omegaSeasonMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = l.with.trend$WSqrt,
                           WSqrtInvG = l.with.trend$WSqrtInvG)
          })


## DLM - Norm, Cov

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendNormCovNoSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = l.all$allStrucZero)
              methods::new("DLMNoTrendNormCovNoSeason",
                           AAlpha = l.all$AAlpha,
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           contrastsArg = l.cov$contrastsArg,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           iAlong = l.all$iAlong,
                           infant = l.cov$infant,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           nuAlpha = l.all$nuAlpha,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           P = l.cov$P,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UEtaCoef = l.cov$UEtaCoef,
                           Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendNormCovNoSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = l.all$allStrucZero)
              methods::new("DLMWithTrendNormCovNoSeason",
                           AAlpha = l.all$AAlpha,
                           ADelta = l.with.trend$ADelta,
                           ADelta0 = l.with.trend$ADelta0,
                           aWithTrend = l.with.trend$aWithTrend,
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           contrastsArg = l.cov$contrastsArg,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           GWithTrend = l.with.trend$GWithTrend,
                           hasLevel = l.with.trend$hasLevel,
                           iAlong = l.all$iAlong,
                           infant = l.cov$infant,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanDelta0 = l.with.trend$meanDelta0,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           nuAlpha = l.all$nuAlpha,
                           nuDelta = l.with.trend$nuDelta,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaDelta = l.with.trend$omegaDelta,
                           omegaDeltaMax = l.with.trend$omegaDeltaMax,
                           P = l.cov$P,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           UEtaCoef = l.cov$UEtaCoef,
                           WSqrt = l.with.trend$WSqrt,
                           WSqrtInvG = l.with.trend$WSqrtInvG,
                           Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendNormCovWithSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.season <- initialDLMSeason(object = object,
                                           beta = beta,
                                           metadata = metadata,
                                           sY = sY)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = l.all$allStrucZero)
              methods::new("DLMNoTrendNormCovWithSeason",
                           AAlpha = l.all$AAlpha,
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ASeason = l.season$ASeason,
                           aNoTrend = l.no.trend$aNoTrend,
                           aSeason = l.season$aSeason,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           CSeason = l.season$CSeason,
                           contrastsArg = l.cov$contrastsArg,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           iAlong = l.all$iAlong,
                           infant = l.cov$infant,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nSeason = l.season$nSeason,
                           nuAlpha = l.all$nuAlpha,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuSeason = l.season$nuSeason,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaSeason = l.season$omegaSeason,
                           omegaSeasonMax = l.season$omegaSeasonMax,
                           P = l.cov$P,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UEtaCoef = l.cov$UEtaCoef,
                           Z = l.cov$Z)
          })


setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendNormCovWithSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.season <- initialDLMSeason(object = object,
                                           beta = beta,
                                           metadata = metadata,
                                           sY = sY)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = l.all$allStrucZero)
              methods::new("DLMWithTrendNormCovWithSeason",
                           AAlpha = l.all$AAlpha,
                           ADelta = l.with.trend$ADelta,
                           ADelta0 = l.with.trend$ADelta0,
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ASeason = l.season$ASeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           aSeason = l.season$aSeason,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           CSeason = l.season$CSeason,
                           contrastsArg = l.cov$contrastsArg,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           GWithTrend = l.with.trend$GWithTrend,
                           hasLevel = l.with.trend$hasLevel,
                           iAlong = l.all$iAlong,
                           infant = l.cov$infant,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanDelta0 = l.with.trend$meanDelta0,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nSeason = l.season$nSeason,
                           nuAlpha = l.all$nuAlpha,
                           nuDelta = l.with.trend$nuDelta,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuSeason = l.season$nuSeason,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaDelta = l.with.trend$omegaDelta,
                           omegaDeltaMax = l.with.trend$omegaDeltaMax,
                           omegaSeason = l.season$omegaSeason,
                           omegaSeasonMax = l.season$omegaSeasonMax,
                           P = l.cov$P,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = l.with.trend$WSqrt,
                           WSqrtInvG = l.with.trend$WSqrtInvG,
                           UEtaCoef = l.cov$UEtaCoef,
                           Z = l.cov$Z)
          })


## DLM - Robust, Zero

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendRobustZeroNoSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMNoTrendRobustZeroNoSeason",
                           AAlpha = l.all$AAlpha,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nuAlpha = l.all$nuAlpha,
                           nuBeta = l.robust$nuBeta,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           UBeta = l.robust$UBeta,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendRobustZeroNoSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMWithTrendRobustZeroNoSeason",
                           AAlpha = l.all$AAlpha,
                           ADelta = l.with.trend$ADelta,
                           ADelta0 = l.with.trend$ADelta0,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = l.with.trend$GWithTrend, 
                           hasLevel = l.with.trend$hasLevel,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanDelta0 = l.with.trend$meanDelta0,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nuAlpha = l.all$nuAlpha,
                           nuBeta = l.robust$nuBeta,
                           nuDelta = l.with.trend$nuDelta,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaDelta = l.with.trend$omegaDelta,
                           omegaDeltaMax = l.with.trend$omegaDeltaMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UBeta = l.robust$UBeta,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = l.with.trend$WSqrt,
                           WSqrtInvG = l.with.trend$WSqrtInvG)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendRobustZeroWithSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.season <- initialDLMSeason(object = object,
                                           beta = beta,
                                           metadata = metadata,
                                           sY = sY)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMNoTrendRobustZeroWithSeason",
                           AAlpha = l.all$AAlpha,
                           ASeason = l.season$ASeason,
                           aNoTrend = l.no.trend$aNoTrend,
                           aSeason = l.season$aSeason,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           CSeason = l.season$CSeason,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nSeason = l.season$nSeason,
                           nuAlpha = l.all$nuAlpha,
                           nuBeta = l.robust$nuBeta,
                           nuSeason = l.season$nuSeason,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaSeason = l.season$omegaSeason,
                           omegaSeasonMax = l.season$omegaSeasonMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UBeta = l.robust$UBeta)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendRobustZeroWithSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.season <- initialDLMSeason(object = object,
                                           beta = beta,
                                           metadata = metadata,
                                           sY = sY)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMWithTrendRobustZeroWithSeason",
                           AAlpha = l.all$AAlpha,
                           ADelta = l.with.trend$ADelta,
                           ADelta0 = l.with.trend$ADelta0,
                           ASeason = l.season$ASeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           aSeason = l.season$aSeason,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           CSeason = l.season$CSeason,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = l.with.trend$GWithTrend,
                           hasLevel = l.with.trend$hasLevel,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanDelta0 = l.with.trend$meanDelta0,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nSeason = l.season$nSeason,
                           nuAlpha = l.all$nuAlpha,
                           nuBeta = l.robust$nuBeta,
                           nuDelta = l.with.trend$nuDelta,
                           nuSeason = l.season$nuSeason,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaDelta = l.with.trend$omegaDelta,
                           omegaDeltaMax = l.with.trend$omegaDeltaMax,
                           omegaSeason = l.season$omegaSeason,
                           omegaSeasonMax = l.season$omegaSeasonMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UBeta = l.robust$UBeta,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = l.with.trend$WSqrt,
                           WSqrtInvG = l.with.trend$WSqrtInvG)
          })


## DLM - Robust, Cov

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendRobustCovNoSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = l.all$allStrucZero)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMNoTrendRobustCovNoSeason",
                           AAlpha = l.all$AAlpha,
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           contrastsArg = l.cov$contrastsArg,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           iAlong = l.all$iAlong,
                           infant = l.cov$infant,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nuAlpha = l.all$nuAlpha,
                           nuBeta = l.robust$nuBeta,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           P = l.cov$P,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UBeta = l.robust$UBeta,
                           UEtaCoef = l.cov$UEtaCoef,
                           Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendRobustCovNoSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = l.all$allStrucZero)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMWithTrendRobustCovNoSeason",
                           AAlpha = l.all$AAlpha,
                           ADelta = l.with.trend$ADelta,
                           ADelta0 = l.with.trend$ADelta0,
                           aWithTrend = l.with.trend$aWithTrend,
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           contrastsArg = l.cov$contrastsArg,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           GWithTrend = l.with.trend$GWithTrend,
                           hasLevel = l.with.trend$hasLevel,
                           iAlong = l.all$iAlong,
                           infant = l.cov$infant,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanDelta0 = l.with.trend$meanDelta0,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nuAlpha = l.all$nuAlpha,
                           nuBeta = l.robust$nuBeta,
                           nuDelta = l.with.trend$nuDelta,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaDelta = l.with.trend$omegaDelta,
                           omegaDeltaMax = l.with.trend$omegaDeltaMax,
                           P = l.cov$P,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UBeta = l.robust$UBeta,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           UEtaCoef = l.cov$UEtaCoef,
                           WSqrt = l.with.trend$WSqrt,
                           WSqrtInvG = l.with.trend$WSqrtInvG,
                           Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendRobustCovWithSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.season <- initialDLMSeason(object = object,
                                           beta = beta,
                                           metadata = metadata,
                                           sY = sY)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = l.all$allStrucZero)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMNoTrendRobustCovWithSeason",
                           AAlpha = l.all$AAlpha,
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ASeason = l.season$ASeason,
                           aNoTrend = l.no.trend$aNoTrend,
                           aSeason = l.season$aSeason,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           CSeason = l.season$CSeason,
                           contrastsArg = l.cov$contrastsArg,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           iAlong = l.all$iAlong,
                           infant = l.cov$infant,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nSeason = l.season$nSeason,
                           nuAlpha = l.all$nuAlpha,
                           nuBeta = l.robust$nuBeta,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuSeason = l.season$nuSeason,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaSeason = l.season$omegaSeason,
                           omegaSeasonMax = l.season$omegaSeasonMax,
                           P = l.cov$P,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UBeta = l.robust$UBeta,
                           UEtaCoef = l.cov$UEtaCoef,
                           Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendRobustCovWithSeason"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              if (isSaturated)
                  stop(gettextf("using prior with '%s = %s' for highest-order term in saturated model",
                                "robust", "TRUE"))
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.season <- initialDLMSeason(object = object,
                                           beta = beta,
                                           metadata = metadata,
                                           sY = sY)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY,
                                  allStrucZero = l.all$allStrucZero)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMWithTrendRobustCovWithSeason",
                           AAlpha = l.all$AAlpha,
                           ADelta = l.with.trend$ADelta,
                           ADelta0 = l.with.trend$ADelta0,
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ASeason = l.season$ASeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           aSeason = l.season$aSeason,
                           ATau = l.all$ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           CSeason = l.season$CSeason,
                           contrastsArg = l.cov$contrastsArg,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           GWithTrend = l.with.trend$GWithTrend,
                           hasLevel = l.with.trend$hasLevel,
                           iAlong = l.all$iAlong,
                           infant = l.cov$infant,
                           isSaturated = l.all$isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           meanDelta0 = l.with.trend$meanDelta0,
                           meanEtaCoef = l.cov$meanEtaCoef,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nSeason = l.season$nSeason,
                           nuAlpha = l.all$nuAlpha,
                           nuBeta = l.robust$nuBeta,
                           nuDelta = l.with.trend$nuDelta,
                           nuEtaCoef = l.cov$nuEtaCoef,
                           nuSeason = l.season$nuSeason,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           omegaDelta = l.with.trend$omegaDelta,
                           omegaDeltaMax = l.with.trend$omegaDeltaMax,
                           omegaSeason = l.season$omegaSeason,
                           omegaSeasonMax = l.season$omegaSeasonMax,
                           P = l.cov$P,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           UBeta = l.robust$UBeta,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = l.with.trend$WSqrt,
                           WSqrtInvG = l.with.trend$WSqrtInvG,
                           UEtaCoef = l.cov$UEtaCoef,
                           Z = l.cov$Z)
          })


## Known

setMethod("initialPrior",
          signature(object = "SpecKnown"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              alpha.all <- object@alphaKnown@.Data
              metadata.all <- object@metadata
              J <- makeJ(beta)
              .Data.beta <- array(beta,
                                  dim = dim(metadata),
                                  dimnames = dimnames(metadata))
              beta <- methods::new("Values",
                                   .Data = .Data.beta,
                                   metadata = metadata)
              .Data.all <- array(alpha.all,
                                 dim = dim(metadata.all),
                                 dimnames = dimnames(metadata.all))
              alpha.all <- methods::new("Values",
                                        .Data = .Data.all,
                                        metadata = metadata.all)
              alpha <- tryCatch(dembase::makeCompatible(x = alpha.all, y = beta, subset = TRUE),
                                error = function(e) e)
              if (methods::is(alpha, "error"))
                  stop(gettextf("metadata for '%s' prior for '%s' not compatible with metadata for '%s' : %s",
                                "Known", paste(names(metadata), collapse = ":"), "y", alpha$message))
              alpha <- as.numeric(alpha)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              struc.zero.but.non.zero <- allStrucZero & (alpha != 0)
              if (any(struc.zero.but.non.zero)) {
                  i.first <- which(struc.zero.but.non.zero)[1L]
                  labels <- expand.grid(dimnames(metadata))
                  label <- labels[i.first, ]
                  label[] <- lapply(label, as.character)
                  label <- paste(label, collapse = ", ")
                  stop(gettextf("all cells contributing to element '[%s]' of \"%s\" prior for '%s' are structural zeros, but element '[%s]' does not equal %d",
                                label, "Known", paste(names(metadata), collapse = ":"), label, 0L))
              }
              alphaKnown <- methods::new("ParameterVector", alpha)
              if (isSaturated)
                  stop(gettextf("using \"%s\" prior for highest-order term in saturated model",
                                "Known"))
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              if (methods::is(object, "SpecKnownCertain")) {
                  methods::new("KnownCertain",
                               alphaKnown = alphaKnown,
                               alphaKnownAll = object@alphaKnown,
                               J = J,
                               isSaturated = isSaturated,
                               metadataAll = object@metadata,
                               allStrucZero = allStrucZero)
              }
              else {
                  A.all <- object@AKnownVec@.Data
                  .Data.A.all <- array(A.all,
                                       dim = dim(metadata.all),
                                       dimnames = dimnames(metadata.all))
                  A <- methods::new("Values",
                                    .Data = .Data.A.all,
                                    metadata = metadata.all)
                  A <- dembase::makeCompatible(x = A, y = beta, subset = TRUE)
                  A <- as.numeric(A)
                  struc.zero.but.non.zero <- allStrucZero & (A != 0)
                  if (any(struc.zero.but.non.zero)) {
                      i.first <- which(struc.zero.but.non.zero)[1L]
                      labels <- expand.grid(dimnames(metadata))
                      label <- labels[i.first, ]
                      label[] <- lapply(label, as.character)
                      label <- paste(label, collapse = ", ")
                      stop(gettextf("all cells contributing to element '[%s]' of \"%s\" prior for '%s' are structural zeros, but element '[%s]' does not have standard deviation %d",
                                    label, "Known", paste(names(metadata), collapse = ":"), label, 0L))
                  }
                  AKnownVec <- methods::new("ScaleVec", A)
                  methods::new("KnownUncertain",
                               AKnownVec = AKnownVec,
                               AKnownAllVec = object@AKnownVec,
                               alphaKnown = alphaKnown,
                               alphaKnownAll = object@alphaKnown,
                               isSaturated = isSaturated,
                               J = J,
                               metadataAll = object@metadata,
                               allStrucZero = allStrucZero)
              }
          })


## Mix

setMethod("initialPrior",
          signature(object = "SpecMixNormZero"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              l.all <- initialMixAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated,
                                     margin = margin,
                                     strucZeroArray = strucZeroArray)
              methods::new("MixNormZero",
                           AComponentWeightMix = l.all$AComponentWeightMix,
                           ALevelComponentWeightMix = l.all$ALevelComponentWeightMix,
                           aMix = l.all$aMix,
                           ATau = l.all$ATau,
                           AVectorsMix = l.all$AVectorsMix,
                           allStrucZero = l.all$allStrucZero,
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
                           isSaturated = l.all$isSaturated,
                           iteratorsDimsMix = l.all$iteratorsDimsMix,
                           iteratorProdVectorMix = l.all$iteratorProdVectorMix,
                           J = l.all$J,
                           latentComponentWeightMix = l.all$latentComponentWeightMix,
                           latentWeightMix = l.all$latentWeightMix,
                           levelComponentWeightMix = l.all$levelComponentWeightMix,
                           mMix = l.all$mMix,
                           maxLevelComponentWeight = l.all$maxLevelComponentWeight,
                           maxPhi = l.all$maxPhi,
                           meanLevelComponentWeightMix = l.all$meanLevelComponentWeightMix,
                           minLevelComponentWeight = l.all$minLevelComponentWeight,
                           minPhi = l.all$minPhi,
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
                           phiKnown = l.all$phiKnown,
                           posProdVectors1Mix = l.all$posProdVectors1Mix,
                           posProdVectors2Mix = l.all$posProdVectors2Mix,
                           priorMeanLevelComponentWeightMix = l.all$priorMeanLevelComponentWeightMix,
                           priorSDLevelComponentWeightMix = l.all$priorSDLevelComponentWeightMix,
                           prodVectorsMix = l.all$prodVectorsMix,
                           RMix = l.all$RMix,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           sumsWeightsMix = l.all$sumsWeightsMix,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax,
                           vectorsMix = l.all$vectorsMix,
                           weightMix = l.all$weightMix,
                           XXMix = l.all$XXMix,
                           yXMix = l.all$yXMix)
          })


setMethod("initialPrior",
          signature(object = "SpecZero"),
          function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...) {
              J <- makeJ(beta)
              if (isSaturated)
                  stop(gettextf("using \"%s\" prior for highest-order term in saturated model",
                                "Zero"))
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray, margin = margin,
                                               metadata = metadata)
              methods::new("Zero",
                           isSaturated = isSaturated,
                           J = J,
                           allStrucZero = allStrucZero)
          })


## initialPriorPredict ###############################################################

## ExchFixed

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "ExchFixed"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              J <- makeJPredict(metadata)
              if (is.null(metadata))
                  allStrucZero <- FALSE
              else
                  allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                                   margin = margin,
                                                   metadata = metadata)
              methods::new("ExchFixed",
                           allStrucZero = allStrucZero,
                           isSaturated = prior@isSaturated,
                           J = J,
                           tau = prior@tau)
          })


## Exch

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "ExchNormZero"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              J <- makeJPredict(metadata)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              methods::new("ExchNormZero",
                           allStrucZero = allStrucZero,
                           ATau = prior@ATau,
                           isSaturated = prior@isSaturated,
                           J = J,
                           nuTau = prior@nuTau,
                           tau = prior@tau,
                           tauMax = prior@tauMax)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "ExchRobustZero"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              A <- prior@ATau@.Data
              nu <- prior@nuBeta@.Data
              J <- makeJPredict(metadata)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              n <- J@.Data
              UBeta <- makeU(nu = nu,
                             A = A,
                             n = J,
                             allStrucZero = allStrucZero)
              methods::new("ExchRobustZero",
                           allStrucZero = allStrucZero,
                           ATau = prior@ATau,
                           isSaturated = prior@isSaturated,
                           J = J,
                           nuBeta = prior@nuBeta,
                           nuTau = prior@nuTau,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = UBeta)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "ExchNormCov"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              formula <- prior@formula
              contrastsArg <- prior@contrastsArg
              infant <- prior@infant
              J <- makeJPredict(metadata)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              Z <- makeZ(formula = formula[-2L],
                         data = data,
                         metadata = metadata,
                         contrastsArg = contrastsArg,
                         infant = infant,
                         allStrucZero = allStrucZero)
              methods::new("ExchNormCov",
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           ATau = prior@ATau,
                           allStrucZero = allStrucZero,
                           contrastsArg = contrastsArg,
                           eta = prior@eta,
                           formula = formula,
                           infant = infant,
                           isSaturated = prior@isSaturated,
                           J = J,
                           meanEtaCoef = prior@meanEtaCoef,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuTau = prior@nuTau,
                           P = prior@P,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UEtaCoef = prior@UEtaCoef,
                           Z = Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "ExchRobustCov"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              ATau <- prior@ATau@.Data
              contrastsArg <- prior@contrastsArg
              formula <- prior@formula
              infant <- prior@infant
              nuBeta <- prior@nuBeta@.Data
              J <- makeJPredict(metadata)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray, margin = margin,
                                               metadata = metadata)
              Z <- makeZ(formula = formula[-2L],
                         data = data,
                         metadata = metadata,
                         contrastsArg = contrastsArg,
                         infant = infant,
                         allStrucZero = allStrucZero)
              n <- J@.Data
              UBeta <- makeU(nu = nuBeta,
                             A = ATau,
                             n = J,
                             allStrucZero = allStrucZero)
              methods::new("ExchRobustCov",
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           ATau = prior@ATau,
                           allStrucZero = allStrucZero,
                           contrastsArg = prior@contrastsArg,
                           eta = prior@eta,
                           formula = prior@formula,
                           infant = infant,
                           isSaturated = prior@isSaturated,
                           J = J,
                           meanEtaCoef = prior@meanEtaCoef,
                           nuBeta = prior@nuBeta,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuTau = prior@nuTau,
                           P = prior@P,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = UBeta,
                           UEtaCoef = prior@UEtaCoef,
                           Z = Z)
          })


## DLM - Norm, Zero

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMNoTrendNormZeroNoSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              methods::new("DLMNoTrendNormZeroNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           iAlong = prior@iAlong,
                           isSaturated = prior@isSaturated,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nuAlpha = prior@nuAlpha,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendNormZeroNoSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              methods::new("DLMWithTrendNormZeroNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = prior@GWithTrend,
                           hasLevel = prior@hasLevel,
                           iAlong = prior@iAlong,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nuAlpha = prior@nuAlpha,
                           nuDelta = prior@nuDelta,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaDelta = prior@omegaDelta,
                           omegaDeltaMax = prior@omegaDeltaMax,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = prior@WSqrt,
                           WSqrtInvG = prior@WSqrtInvG)
          })


## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMNoTrendNormZeroWithSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              methods::new("DLMNoTrendNormZeroWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           aNoTrend = l.no.trend$aNoTrend,
                           aSeason = l.season$aSeason,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           CSeason = l.season$CSeason,
                           iAlong = prior@iAlong,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nSeason = prior@nSeason,
                           nuAlpha = prior@nuAlpha,
                           nuSeason = prior@nuSeason,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaSeason = prior@omegaSeason,
                           omegaSeasonMax = prior@omegaSeasonMax,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendNormZeroWithSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              methods::new("DLMWithTrendNormZeroWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           aSeason = l.season$aSeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                          alphaDLM = l.all$alphaDLM,
                           CSeason = l.season$CSeason,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = prior@GWithTrend,
                           hasLevel = prior@hasLevel,
                           iAlong = prior@iAlong,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nSeason = prior@nSeason,
                           nuAlpha = prior@nuAlpha,
                           nuDelta = prior@nuDelta,
                           nuSeason = prior@nuSeason,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaDelta = prior@omegaDelta,
                           omegaDeltaMax = prior@omegaDeltaMax,
                           omegaSeason = prior@omegaSeason,
                           omegaSeasonMax = prior@omegaSeasonMax,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RSeason = l.season$RSeason,
                           RWithTrend = l.with.trend$RWithTrend,
                           s = l.season$s,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = prior@WSqrt,
                           WSqrtInvG = prior@WSqrtInvG)
          })


## DLM - Norm, Cov

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMNoTrendNormCovNoSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata,
                                         allStrucZero = l.all$allStrucZero)
              methods::new("DLMNoTrendNormCovNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           contrastsArg = prior@contrastsArg,
                           eta = prior@eta,
                           formula = prior@formula,
                           iAlong = prior@iAlong,
                           infant = prior@infant,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           meanEtaCoef = prior@meanEtaCoef,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nuAlpha = prior@nuAlpha,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           P = prior@P,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UEtaCoef = prior@UEtaCoef,
                           Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendNormCovNoSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata,
                                         allStrucZero = l.all$allStrucZero)
              methods::new("DLMWithTrendNormCovNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aWithTrend = l.with.trend$aWithTrend,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           contrastsArg = prior@contrastsArg,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           eta = prior@eta,
                           formula = prior@formula,
                           GWithTrend = prior@GWithTrend,
                           hasLevel = prior@hasLevel,
                           iAlong = prior@iAlong,
                           infant = prior@infant,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanEtaCoef = prior@meanEtaCoef,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nuAlpha = prior@nuAlpha,
                           nuDelta = prior@nuDelta,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaDelta = prior@omegaDelta,
                           omegaDeltaMax = prior@omegaDeltaMax,
                           P = prior@P,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           UEtaCoef = prior@UEtaCoef,
                           WSqrt = prior@WSqrt,
                           WSqrtInvG = prior@WSqrtInvG,
                           Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMNoTrendNormCovWithSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata,
                                         allStrucZero = l.all$allStrucZero)
              methods::new("DLMNoTrendNormCovWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           aNoTrend = l.no.trend$aNoTrend,
                           aSeason = l.season$aSeason,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           CSeason = l.season$CSeason,
                           contrastsArg = prior@contrastsArg,
                           eta = prior@eta,
                           formula = prior@formula,
                           iAlong = prior@iAlong,
                           infant = prior@infant,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           meanEtaCoef = prior@meanEtaCoef,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nSeason = prior@nSeason,
                           nuAlpha = prior@nuAlpha,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuSeason = prior@nuSeason,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaSeason = prior@omegaSeason,
                           omegaSeasonMax = prior@omegaSeasonMax,
                           P = prior@P,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UEtaCoef = prior@UEtaCoef,
                           Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendNormCovWithSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata,
                                         allStrucZero = l.all$allStrucZero)
              methods::new("DLMWithTrendNormCovWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           aSeason = l.season$aSeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CSeason = l.season$CSeason,
                           CWithTrend = l.with.trend$CWithTrend,
                           contrastsArg = prior@contrastsArg,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           eta = prior@eta,
                           formula = prior@formula,
                           GWithTrend = prior@GWithTrend,
                           hasLevel = prior@hasLevel,
                           iAlong = prior@iAlong,
                           infant = prior@infant,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanEtaCoef = prior@meanEtaCoef,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nSeason = prior@nSeason,
                           nuAlpha = prior@nuAlpha,
                           nuDelta = prior@nuDelta,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuSeason = prior@nuSeason,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaDelta = prior@omegaDelta,
                           omegaDeltaMax = prior@omegaDeltaMax,
                           omegaSeason = prior@omegaSeason,
                           omegaSeasonMax = prior@omegaSeasonMax,
                           P = prior@P,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RSeason = l.season$RSeason,
                           RWithTrend = l.with.trend$RWithTrend,
                           s = l.season$s,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UC = l.with.trend$UC,
                           UEtaCoef = prior@UEtaCoef,
                           UR = l.with.trend$UR,
                           WSqrt = prior@WSqrt,
                           WSqrtInvG = prior@WSqrtInvG,
                           Z = l.cov$Z)
          })


## DLM - Robust, Zero

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMNoTrendRobustZeroNoSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata,
                                               allStrucZero = l.all$allStrucZero)
              methods::new("DLMNoTrendRobustZeroNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           iAlong = prior@iAlong,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nuAlpha = prior@nuAlpha,
                           nuBeta = prior@nuBeta,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = l.robust$UBeta)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendRobustZeroNoSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata,
                                               allStrucZero = l.all$allStrucZero)
              methods::new("DLMWithTrendRobustZeroNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = prior@GWithTrend,
                           hasLevel = prior@hasLevel,
                           iAlong = prior@iAlong,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nuAlpha = prior@nuAlpha,
                           nuBeta = prior@nuBeta,
                           nuDelta = prior@nuDelta,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaDelta = prior@omegaDelta,
                           omegaDeltaMax = prior@omegaDeltaMax,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = l.robust$UBeta,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = prior@WSqrt,
                           WSqrtInvG = prior@WSqrtInvG)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMNoTrendRobustZeroWithSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata,
                                               allStrucZero = l.all$allStrucZero)
              methods::new("DLMNoTrendRobustZeroWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           aNoTrend = l.no.trend$aNoTrend,
                           aSeason = l.season$aSeason,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           CSeason = l.season$CSeason,
                           iAlong = prior@iAlong,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nSeason = prior@nSeason,
                           nuAlpha = prior@nuAlpha,
                           nuBeta = prior@nuBeta,
                           nuSeason = prior@nuSeason,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaSeason = prior@omegaSeason,
                           omegaSeasonMax = prior@omegaSeasonMax,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = l.robust$UBeta)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendRobustZeroWithSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata,
                                               allStrucZero = l.all$allStrucZero)
              methods::new("DLMWithTrendRobustZeroWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           aSeason = l.season$aSeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CSeason = l.season$CSeason,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = prior@GWithTrend,
                           hasLevel = prior@hasLevel,
                           iAlong = prior@iAlong,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nSeason = prior@nSeason,
                           nuAlpha = prior@nuAlpha,
                           nuBeta = prior@nuBeta,
                           nuDelta = prior@nuDelta,
                           nuSeason = prior@nuSeason,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaDelta = prior@omegaDelta,
                           omegaDeltaMax = prior@omegaDeltaMax,
                           omegaSeason = prior@omegaSeason,
                           omegaSeasonMax = prior@omegaSeasonMax,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RSeason = l.season$RSeason,
                           RWithTrend = l.with.trend$RWithTrend,
                           s = l.season$s,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = l.robust$UBeta,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           WSqrt = prior@WSqrt,
                           WSqrtInvG = prior@WSqrtInvG)
          })


## DLM - Robust, Cov

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMNoTrendRobustCovNoSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata,
                                         allStrucZero = l.all$allStrucZero)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata,
                                               allStrucZero = l.all$allStrucZero)
              methods::new("DLMNoTrendRobustCovNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           contrastsArg = prior@contrastsArg,
                           eta = prior@eta,
                           formula = prior@formula,
                           iAlong = prior@iAlong,
                           infant = prior@infant,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           meanEtaCoef = prior@meanEtaCoef,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nuAlpha = prior@nuAlpha,
                           nuBeta = prior@nuBeta,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           P = prior@P,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = l.robust$UBeta,
                           UEtaCoef = prior@UEtaCoef,
                           Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendRobustCovNoSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata,
                                         allStrucZero = l.all$allStrucZero)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata,
                                               allStrucZero = l.all$allStrucZero)
              methods::new("DLMWithTrendRobustCovNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aWithTrend = l.with.trend$aWithTrend,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           contrastsArg = prior@contrastsArg,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           eta = prior@eta,
                           formula = prior@formula,
                           GWithTrend = prior@GWithTrend,
                           hasLevel = prior@hasLevel,
                           iAlong = prior@iAlong,
                           infant = prior@infant,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanEtaCoef = prior@meanEtaCoef,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nuAlpha = prior@nuAlpha,
                           nuBeta = prior@nuBeta,
                           nuDelta = prior@nuDelta,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaDelta = prior@omegaDelta,
                           omegaDeltaMax = prior@omegaDeltaMax,
                           P = prior@P,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RWithTrend = l.with.trend$RWithTrend,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = l.robust$UBeta,
                           UC = l.with.trend$UC,
                           UR = l.with.trend$UR,
                           UEtaCoef = prior@UEtaCoef,
                           WSqrt = prior@WSqrt,
                           WSqrtInvG = prior@WSqrtInvG,
                           Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMNoTrendRobustCovWithSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata,
                                         allStrucZero = l.all$allStrucZero)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata,
                                               allStrucZero = l.all$allStrucZero)
              methods::new("DLMNoTrendRobustCovWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           aNoTrend = l.no.trend$aNoTrend,
                           aSeason = l.season$aSeason,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           CSeason = l.season$CSeason,
                           contrastsArg = prior@contrastsArg,
                           eta = prior@eta,
                           formula = prior@formula,
                           iAlong = prior@iAlong,
                           infant = prior@infant,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           meanEtaCoef = prior@meanEtaCoef,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nSeason = prior@nSeason,
                           nuAlpha = prior@nuAlpha,
                           nuBeta = prior@nuBeta,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuSeason = prior@nuSeason,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaSeason = prior@omegaSeason,
                           omegaSeasonMax = prior@omegaSeasonMax,
                           P = prior@P,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           RSeason = l.season$RSeason,
                           s = l.season$s,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = l.robust$UBeta,
                           UEtaCoef = prior@UEtaCoef,
                           Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendRobustCovWithSeason"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata,
                                         allStrucZero = l.all$allStrucZero)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata, allStrucZero = l.all$allStrucZero)
              methods::new("DLMWithTrendRobustCovWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           aSeason = l.season$aSeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = prior@ATau,
                           allStrucZero = l.all$allStrucZero,
                           alongAllStrucZero = l.all$alongAllStrucZero,
                           alphaDLM = l.all$alphaDLM,
                           CSeason = l.season$CSeason,
                           CWithTrend = l.with.trend$CWithTrend,
                           contrastsArg = prior@contrastsArg,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           eta = prior@eta,
                           formula = prior@formula,
                           GWithTrend = prior@GWithTrend,
                           hasLevel = prior@hasLevel,
                           iAlong = prior@iAlong,
                           infant = prior@infant,
                           isSaturated = prior@isSaturated,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mSeason = l.season$mSeason,
                           m0Season = l.season$m0Season,
                           mWithTrend = l.with.trend$mWithTrend,
                           m0WithTrend = l.with.trend$m0WithTrend,
                           meanEtaCoef = prior@meanEtaCoef,
                           minPhi = prior@minPhi,
                           maxPhi = prior@maxPhi,
                           nSeason = prior@nSeason,
                           nuAlpha = prior@nuAlpha,
                           nuBeta = prior@nuBeta,
                           nuDelta = prior@nuDelta,
                           nuEtaCoef = prior@nuEtaCoef,
                           nuSeason = prior@nuSeason,
                           nuTau = prior@nuTau,
                           omegaAlpha = prior@omegaAlpha,
                           omegaAlphaMax = prior@omegaAlphaMax,
                           omegaDelta = prior@omegaDelta,
                           omegaDeltaMax = prior@omegaDeltaMax,
                           omegaSeason = prior@omegaSeason,
                           omegaSeasonMax = prior@omegaSeasonMax,
                           P = prior@P,
                           phi = prior@phi,
                           phiKnown = prior@phiKnown,
                           RSeason = l.season$RSeason,
                           RWithTrend = l.with.trend$RWithTrend,
                           s = l.season$s,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UBeta = l.robust$UBeta,
                           UC = l.with.trend$UC,
                           UEtaCoef = prior@UEtaCoef,
                           UR = l.with.trend$UR,
                           WSqrt = prior@WSqrt,
                           WSqrtInvG = prior@WSqrtInvG,
                           Z = l.cov$Z)
          })

## Known

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "Known"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              alpha.all <- prior@alphaKnownAll@.Data
              metadata.all <- prior@metadataAll
              J <- makeJPredict(metadata)
              .Data.beta <- array(0,
                                  dim = dim(metadata),
                                  dimnames = dimnames(metadata))
              beta <- methods::new("Values",
                                   .Data = .Data.beta,
                                   metadata = metadata)
              .Data.all <- array(alpha.all,
                                 dim = dim(metadata.all),
                                 dimnames = dimnames(metadata.all))
              alpha.all <- methods::new("Values",
                                        .Data = .Data.all,
                                        metadata = metadata.all)
              alpha <- tryCatch(dembase::makeCompatible(x = alpha.all, y = beta, subset = TRUE),
                                error = function(e) e)
              if (methods::is(alpha, "error"))
                  stop(gettextf("metadata for '%s' prior for '%s' not compatible with metadata for '%s' : %s",
                                "Known", paste(names(metadata), collapse = ":"), "y", alpha$message))
              alpha <- as.numeric(alpha)
              allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin,
                                               metadata = metadata)
              struc.zero.but.non.zero <- allStrucZero & (alpha != 0)
              if (any(struc.zero.but.non.zero)) {
                  i.first <- which(struc.zero.but.non.zero)[1L]
                  labels <- expand.grid(dimnames(metadata))
                  label <- labels[i.first, ]
                  label[] <- lapply(label, as.character)
                  label <- paste(label, collapse = ", ")
                  stop(gettextf("all cells contributing to element '[%s]' of \"%s\" prior for '%s' are structural zeros, but element '[%s]' does not equal %d",
                                label, "Known", paste(names(metadata), collapse = ":"), label, 0L))
              }
              alphaKnown <- methods::new("ParameterVector", alpha)
              if (methods::is(prior, "KnownCertain")) {
                  methods::new("KnownCertain",
                               allStrucZero = allStrucZero,
                               alphaKnown = alphaKnown,
                               alphaKnownAll = prior@alphaKnownAll,
                               isSaturated = prior@isSaturated,
                               J = J,
                               metadataAll = prior@metadataAll)
              }
              else {
                  A.all <- prior@AKnownAllVec@.Data
                  .Data.A.all <- array(A.all,
                                   dim = dim(metadata.all),
                                   dimnames = dimnames(metadata.all))
                  A <- methods::new("Values",
                                    .Data = .Data.A.all,
                                    metadata = metadata.all)
                  A <- dembase::makeCompatible(x = A, y = beta, subset = TRUE)
                  A <- as.numeric(A)
                  struc.zero.but.non.zero <- allStrucZero & (A != 0)
                  if (any(struc.zero.but.non.zero)) {
                      i.first <- which(struc.zero.but.non.zero)[1L]
                      labels <- expand.grid(dimnames(metadata))
                      label <- labels[i.first, ]
                      label[] <- lapply(label, as.character)
                      label <- paste(label, collapse = ", ")
                      stop(gettextf("all cells contributing to element '[%s]' of \"%s\" prior for '%s' are structural zeros, but element '[%s]' does not have standard deviation %d",
                                    label, "Known", paste(names(metadata), collapse = ":"), label, 0L))
                  }
                  AKnownVec <- methods::new("ScaleVec", A)
                  methods::new("KnownUncertain",
                               allStrucZero = allStrucZero,
                               AKnownVec = AKnownVec,
                               AKnownAllVec = prior@AKnownAllVec,
                               alphaKnown = alphaKnown,
                               alphaKnownAll = prior@alphaKnownAll,
                               isSaturated = prior@isSaturated,
                               J = J,
                               metadataAll = prior@metadataAll)
              }
          })


## Mix

setMethod("initialPriorPredict",
          signature(prior = "MixNormZero"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              l.all <- initialMixAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along,
                                            margin = margin,
                                            strucZeroArray = strucZeroArray)
              methods::new("MixNormZeroPredict",
                           AComponentWeightMix = prior@AComponentWeightMix,
                           ALevelComponentWeightMix = prior@ALevelComponentWeightMix,
                           ATau = prior@ATau,
                           AVectorsMix = prior@AVectorsMix,
                           aMix = l.all$aMix,
                           allStrucZero = l.all$allStrucZero,
                           alphaMix = l.all$alphaMix,
                           CMix = l.all$CMix,
                           componentWeightMix = l.all$componentWeightMix,
                           dimBeta = l.all$dimBeta,
                           dimBetaOld = prior@dimBeta,
                           foundIndexClassMaxPossibleMix = prior@foundIndexClassMaxPossibleMix,
                           iAlong = l.all$iAlong,
                           indexClassMaxMix = prior@indexClassMaxMix,
                           indexClassMaxPossibleMix = prior@indexClassMaxPossibleMix,
                           indexClassMaxUsedMix = prior@indexClassMaxUsedMix,
                           indexClassMix = l.all$indexClassMix,
                           indexClassProbMix = prior@indexClassProbMix,
                           isSaturated = prior@isSaturated,
                           iteratorsDimsMix = l.all$iteratorsDimsMix,
                           iteratorProdVectorMix = l.all$iteratorProdVectorMix,
                           J = l.all$J,
                           JOld = prior@J,
                           latentComponentWeightMix = l.all$latentComponentWeightMix,
                           latentWeightMix = l.all$latentWeightMix,
                           levelComponentWeightMix = l.all$levelComponentWeightMix,
                           levelComponentWeightOldMix = l.all$levelComponentWeightOldMix,
                           meanLevelComponentWeightMix = prior@meanLevelComponentWeightMix,
                           mMix = l.all$mMix,
                           maxLevelComponentWeight = prior@maxLevelComponentWeight,
                           minLevelComponentWeight = prior@minLevelComponentWeight,
                           maxPhi = prior@maxPhi,
                           minPhi = prior@minPhi,
                           nBetaNoAlongMix = prior@nBetaNoAlongMix,
                           nuComponentWeightMix = prior@nuComponentWeightMix,
                           nuLevelComponentWeightMix = prior@nuLevelComponentWeightMix,
                           nuTau = prior@nuTau,
                           nuVectorsMix = prior@nuVectorsMix,
                           omegaComponentWeightMaxMix = prior@omegaComponentWeightMaxMix,
                           omegaComponentWeightMix = prior@omegaComponentWeightMix,
                           omegaLevelComponentWeightMaxMix = prior@omegaLevelComponentWeightMaxMix,
                           omegaLevelComponentWeightMix = prior@omegaLevelComponentWeightMix,
                           omegaVectorsMaxMix = prior@omegaVectorsMaxMix,
                           omegaVectorsMix = prior@omegaVectorsMaxMix,
                           phiMix = prior@phiMix,
                           phiKnown = prior@phiKnown,
                           posProdVectors1Mix = l.all$posProdVectors1Mix,
                           posProdVectors2Mix = l.all$posProdVectors2Mix,
                           priorMeanLevelComponentWeightMix = prior@priorMeanLevelComponentWeightMix,
                           priorSDLevelComponentWeightMix = prior@priorSDLevelComponentWeightMix,
                           prodVectorsMix = prior@prodVectorsMix,
                           RMix = l.all$RMix,
                           shape1Phi = prior@shape1Phi,
                           shape2Phi = prior@shape2Phi,
                           sumsWeightsMix = l.all$sumsWeightsMix,
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           tolerance = prior@tolerance,
                           vectorsMix = prior@vectorsMix,
                           weightMix = l.all$weightMix,
                           XXMix = prior@XXMix,
                           yXMix = prior@yXMix)
          })


## Zero

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "Zero"),
          function(prior, data, metadata, name, along, margin, strucZeroArray) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              J <- makeJPredict(metadata)
              if (is.null(metadata))
                  allStrucZero <- FALSE
              else
                  allStrucZero <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                                   margin = margin,
                                                   metadata = metadata)
              methods::new("Zero",
                           allStrucZero = allStrucZero,
                           isSaturated = prior@isSaturated,
                           J = J)
          })
