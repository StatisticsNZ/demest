
## initialPrior #######################################################################

## In 'initialPrior' methods for priors, assume that calling
## function has checked whether 'beta' has any missing values
## and whether its length is at least 2.  Other conditions
## must be checked by 'initialPrior'.

## intercept
setMethod("initialPrior",
          signature(object = "SpecExchFixed", metadata = "NULL"),
          function(object, beta, metadata, sY, ...) {
              tau <- object@tau
              J <- makeJ(beta)
              if (J > 1L)
                  stop(gettextf("'%s' is %s but '%s' is greater than %d",
                                "metadata", "NULL", "J", 1L))
              tau <- makeTauExchFixedIntercept(tau = tau,
                                               sY = sY)
              methods::new("ExchFixed",
                           J = J,
                           tau = tau)
          })

## non-intercept
setMethod("initialPrior",
          signature(object = "SpecExchFixed"),
          function(object, beta, metadata, sY, ...) {
              tau <- object@tau
              multTau <- object@multTau
              J <- makeJ(beta)
              if (J <= 1L)
                  stop(gettextf("'%s' is not %s but '%s' is less than or equal to %d",
                                "metadata", "NULL", "J", 1L))
              tau <- makeTauExchFixedNonIntercept(tau = tau,
                                                  sY = sY,
                                                  mult = multTau)
              methods::new("ExchFixed",
                           J = J,
                           tau = tau)
          })

setMethod("initialPrior",
          signature(object = "SpecExchNormZero"),
          function(object, beta, metadata, sY, ...) {
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
              methods::new("ExchNormZero",
                           ATau = ATau,
                           J = J,
                           nuTau = nuTau,
                           tau = tau,
                           tauMax = tauMax)
          })

setMethod("initialPrior",
          signature(object = "SpecExchRobustZero"),
          function(object, beta, metadata, sY, ...) {
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
              UBeta <- makeU(nu = nuBeta, A = ATau, n = J)
              tau <- makeScale(A = ATau,
                               nu = nuTau,
                               scaleMax = tauMax)
              methods::new("ExchRobustZero",
                           ATau = ATau,
                           J = J,
                           nuBeta = nuBeta,
                           nuTau = nuTau,
                           tau = tau,
                           tauMax = tauMax,
                           UBeta = UBeta)
          })

setMethod("initialPrior",
          signature(object = "SpecExchNormCov"),
          function(object, beta, metadata, sY, ...) {
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
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY)
              methods::new("ExchNormCov",
                           AEtaCoef = l.cov$AEtaCoef,
                           AEtaIntercept = l.cov$AEtaIntercept,
                           ATau = ATau,
                           contrastsArg = l.cov$contrastsArg,
                           eta = l.cov$eta,
                           formula = l.cov$formula,
                           J = J,
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
          function(object, beta, metadata, sY, ...) {
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
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY)
              UBeta <- makeU(nu = nuBeta, A = ATau, n = J)
              methods::new("ExchRobustCov",
                  AEtaCoef = l.cov$AEtaCoef,
                  AEtaIntercept = l.cov$AEtaIntercept,
                  ATau = ATau,
                  contrastsArg = l.cov$contrastsArg,
                  eta = l.cov$eta,
                  formula = l.cov$formula,
                  J = J,
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
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              methods::new("DLMNoTrendNormZeroNoSeason",
                           AAlpha = l.all$AAlpha,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = l.all$ATau,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           iAlong = l.all$iAlong,
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
                           tau = l.all$tau,
                           tauMax = l.all$tauMax)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendNormZeroNoSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                  alphaDLM = l.all$alphaDLM,
                  CWithTrend = l.with.trend$CWithTrend,
                  DC = l.with.trend$DC,
                  DCInv = l.with.trend$DCInv,
                  DRInv = l.with.trend$DRInv,
                  deltaDLM = l.with.trend$deltaDLM,
                  GWithTrend = l.with.trend$GWithTrend,
                  iAlong = l.all$iAlong,
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
                  tau = l.all$tau,
                  tauMax = l.all$tauMax,
                  UC = l.with.trend$UC,
                  UR = l.with.trend$UR,
                  WSqrt = l.with.trend$WSqrt,
                  WSqrtInvG = l.with.trend$WSqrtInvG)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMNoTrendNormZeroWithSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  CSeason = l.season$CSeason,
                  iAlong = l.all$iAlong,
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
                  tau = l.all$tau,
                  tauMax = l.all$tauMax)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendNormZeroWithSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                  alphaDLM = l.all$alphaDLM,
                  CWithTrend = l.with.trend$CWithTrend,
                  CSeason = l.season$CSeason,
                  DC = l.with.trend$DC,
                  DCInv = l.with.trend$DCInv,
                  DRInv = l.with.trend$DRInv,
                  deltaDLM = l.with.trend$deltaDLM,
                  GWithTrend = l.with.trend$GWithTrend,
                  iAlong = l.all$iAlong,
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
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY)
              methods::new("DLMNoTrendNormCovNoSeason",
                  AAlpha = l.all$AAlpha,
                  AEtaCoef = l.cov$AEtaCoef,
                  AEtaIntercept = l.cov$AEtaIntercept,
                  aNoTrend = l.no.trend$aNoTrend,
                  ATau = l.all$ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  contrastsArg = l.cov$contrastsArg,
                  eta = l.cov$eta,
                  formula = l.cov$formula,
                  iAlong = l.all$iAlong,
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
                  nuEtaCoef = l.cov$nuEtaCoef,
                  nuTau = l.all$nuTau,
                  omegaAlpha = l.all$omegaAlpha,
                  omegaAlphaMax = l.all$omegaAlphaMax,
                  P = l.cov$P,
                  phi = l.all$phi,
                  phiKnown = l.all$phiKnown,
                  RNoTrend = l.no.trend$RNoTrend,
                  tau = l.all$tau,
                  tauMax = l.all$tauMax,
                  UEtaCoef = l.cov$UEtaCoef,
                  Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendNormCovNoSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY)
              methods::new("DLMWithTrendNormCovNoSeason",
                  AAlpha = l.all$AAlpha,
                  ADelta = l.with.trend$ADelta,
                  ADelta0 = l.with.trend$ADelta0,
                  aWithTrend = l.with.trend$aWithTrend,
                  AEtaCoef = l.cov$AEtaCoef,
                  AEtaIntercept = l.cov$AEtaIntercept,
                  ATau = l.all$ATau,
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
                  iAlong = l.all$iAlong,
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
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                                  sY = sY)
              methods::new("DLMNoTrendNormCovWithSeason",
                  AAlpha = l.all$AAlpha,
                  AEtaCoef = l.cov$AEtaCoef,
                  AEtaIntercept = l.cov$AEtaIntercept,
                  ASeason = l.season$ASeason,
                  aNoTrend = l.no.trend$aNoTrend,
                  aSeason = l.season$aSeason,
                  ATau = l.all$ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  CSeason = l.season$CSeason,
                  contrastsArg = l.cov$contrastsArg,
                  eta = l.cov$eta,
                  formula = l.cov$formula,
                  iAlong = l.all$iAlong,
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
                  tau = l.all$tau,
                  tauMax = l.all$tauMax,
                  UEtaCoef = l.cov$UEtaCoef,
                  Z = l.cov$Z)
          })


setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendNormCovWithSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                                  sY = sY)
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
                  iAlong = l.all$iAlong,
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
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.robust <- initialRobust(object = object,
                                        lAll = l.all)
              methods::new("DLMNoTrendRobustZeroNoSeason",
                  AAlpha = l.all$AAlpha,
                  aNoTrend = l.no.trend$aNoTrend,
                  ATau = l.all$ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  iAlong = l.all$iAlong,
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
                  UBeta = l.robust$UBeta,
                  tau = l.all$tau,
                  tauMax = l.all$tauMax)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendRobustZeroNoSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                  alphaDLM = l.all$alphaDLM,
                  CWithTrend = l.with.trend$CWithTrend,
                  DC = l.with.trend$DC,
                  DCInv = l.with.trend$DCInv,
                  DRInv = l.with.trend$DRInv,
                  deltaDLM = l.with.trend$deltaDLM,
                  GWithTrend = l.with.trend$GWithTrend, 
                  iAlong = l.all$iAlong,
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
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  CSeason = l.season$CSeason,
                  iAlong = l.all$iAlong,
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
                  tau = l.all$tau,
                  tauMax = l.all$tauMax,
                  UBeta = l.robust$UBeta)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendRobustZeroWithSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                  alphaDLM = l.all$alphaDLM,
                  CWithTrend = l.with.trend$CWithTrend,
                  CSeason = l.season$CSeason,
                  DC = l.with.trend$DC,
                  DCInv = l.with.trend$DCInv,
                  DRInv = l.with.trend$DRInv,
                  deltaDLM = l.with.trend$deltaDLM,
                  GWithTrend = l.with.trend$GWithTrend,
                  iAlong = l.all$iAlong,
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
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY)
              l.robust <- initialRobust(object = object,
                                           lAll = l.all)
              methods::new("DLMNoTrendRobustCovNoSeason",
                  AAlpha = l.all$AAlpha,
                  AEtaCoef = l.cov$AEtaCoef,
                  AEtaIntercept = l.cov$AEtaIntercept,
                  aNoTrend = l.no.trend$aNoTrend,
                  ATau = l.all$ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  contrastsArg = l.cov$contrastsArg,
                  eta = l.cov$eta,
                  formula = l.cov$formula,
                  iAlong = l.all$iAlong,
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
                  nuEtaCoef = l.cov$nuEtaCoef,
                  nuTau = l.all$nuTau,
                  omegaAlpha = l.all$omegaAlpha,
                  omegaAlphaMax = l.all$omegaAlphaMax,
                  P = l.cov$P,
                  phi = l.all$phi,
                  phiKnown = l.all$phiKnown,
                  RNoTrend = l.no.trend$RNoTrend,
                  tau = l.all$tau,
                  tauMax = l.all$tauMax,
                  UBeta = l.robust$UBeta,
                  UEtaCoef = l.cov$UEtaCoef,
                  Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendRobustCovNoSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
              l.with.trend <- initialDLMWithTrend(object = object,
                                                  beta = beta,
                                                  metadata = metadata,
                                                  sY = sY,
                                                  lAll = l.all)
              l.cov <- initialCov(object = object,
                                  beta = beta,
                                  metadata = metadata,
                                  sY = sY)
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
                           iAlong = l.all$iAlong,
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
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                                  sY = sY)
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
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  CSeason = l.season$CSeason,
                  contrastsArg = l.cov$contrastsArg,
                  eta = l.cov$eta,
                  formula = l.cov$formula,
                  iAlong = l.all$iAlong,
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
                  tau = l.all$tau,
                  tauMax = l.all$tauMax,
                  UBeta = l.robust$UBeta,
                  UEtaCoef = l.cov$UEtaCoef,
                  Z = l.cov$Z)
          })

setMethod("initialPrior",
          signature(object = "SpecDLMWithTrendRobustCovWithSeason"),
          function(object, beta, metadata, sY, ...) {
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY)
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
                                  sY = sY)
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
                  iAlong = l.all$iAlong,
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


## Move

## NOT FINISHED!!
setMethod("initialPrior",
          signature(object = "SpecMoveNormZero"),
          function(object, beta, metadata, sY, ...) {
              classes <- object@classes
              AMove <- object@AMove
              ATau <- object@ATau
              multMove <- object@multMove
              multTau <- object@multTau
              nuTau <- object@nuTau
              tauMax <- object@tauMax
              J <- makeJ(beta)
              indexClassAlpha <- makeIndexClassAlpha(classes = classes,
                                                         metadata = metadata)
              ## nElementClassAlpha <- makeNElementClassAlpha(indexClassAlpha)
              alphaMove <- makeAlphaMove(beta = beta,
                                         indexClassAlpha = indexClassAlpha,
                                         nElementClassAlpha = nElementClassAlpha)
              AMove <- makeAMove(A = AMove,
                                 metadata = metadata,
                                 sY = sY,
                                 mult = multMove)
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
              methods::new("MoveNormZero",
                           alphaMove = alphaMove,
                           AMove = AMove,
                           ATau = ATau,
                           indexClassAlpha = indexClassAlpha,
                           J = J,
                           nElementClassAlpha = nElementClassAlpha,
                           nuTau = nuTau,
                           tau = tau,
                           tauMax = tauMax)
          })



## ## HAS_TESTS
## Known <- function(mean, sd = NULL) {
##     ## 'mean' is "Values"
##     if (!methods::is(mean, "Values"))
##         stop(gettextf("'%s' has class \"%s\"",
##                       "mean", class(mean)))
##     ## 'mean' has no missing values
##     if (any(is.na(mean)))
##         stop(gettextf("'%s' has missing values",
##                       "mean"))
##     metadata <- mean@metadata
##     if (is.null(sd)) {
##         values <- as.double(mean)
##         methods::new("SpecKnownCertain",
##             values = values,
##             valuesUnscaled = values,
##             metadata = metadata)
##     }
##     else {
##         if (methods::is(sd, "Values")) {
##             ## 'sd' is compatible with 'mean'
##             sd <- tryCatch(dembase::makeCompatible(x = sd, y = mean, subset = FALSE),
##                            error = function(e) e)
##             if (methods::is(sd, "error"))
##                 stop(gettextf("'%s' and '%s' not compatible : %s",
##                               "sd", "mean", sd$message))
##             ## 'sd' has no missing values
##             if (any(is.na(sd)))
##                 stop(gettextf("'%s' has missing values",
##                               "sd"))
##             ## 'sd' has no negative values
##             if (any(sd < 0))
##                 stop(gettextf("'%s' has negative values",
##                               "sd"))
##         }
##         else if (methods::is(sd, "numeric")) {
##             ## 'sd' has length 1
##             if (!identical(length(sd), 1L))
##                 stop(gettextf("'%s' is numeric but does not have length %d",
##                               "sd", 1L))
##             ## 'sd' is not missing
##             if (is.na(sd))
##                 stop(gettextf("'%s' is missing",
##                               "sd"))
##             ## 'sd' is non-negative
##             if (sd < 0)
##                 stop(gettextf("'%s' is negative",
##                               "sd"))
##             sd <- as.numeric(sd)
##             sd <- rep(sd, times = length(mean))
##         }
##         else { ## sd is Values or numeric
##             stop(gettextf("'%s' has class \"%s\"",
##                           "sd", class(sd)))
##         }
##         mean <- as.double(mean)
##         sd <- as.double(sd)
##         methods::new("SpecKnownUncertain",
##             mean = mean,
##             sd = sd,
##             meanUnscaled = mean,
##             sdUnscaled = sd,
##             metadata = metadata)
##     }
## }



## initialPriorPredict ###############################################################


## Exch

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "ExchNormZero"),
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              J <- makeJPredict(metadata)
              methods::new("ExchNormZero",
                  ATau = prior@ATau,
                  J = J,
                  nuTau = prior@nuTau,
                  tau = prior@tau,
                  tauMax = prior@tauMax)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "ExchRobustZero"),
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              A <- prior@ATau@.Data
              nu <- prior@nuBeta@.Data
              J <- makeJPredict(metadata)
              UBeta <- makeU(nu = nu, A = A, n = J)
              methods::new("ExchRobustZero",
                           ATau = prior@ATau,
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
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              formula <- prior@formula
              contrastsArg <- prior@contrastsArg
              J <- makeJPredict(metadata)
              Z <- makeZ(formula = formula[-2L],
                         data = data,
                         metadata = metadata,
                         contrastsArg = contrastsArg)
              methods::new("ExchNormCov",
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           ATau = prior@ATau,
                           contrastsArg = contrastsArg,
                           eta = prior@eta,
                           formula = formula,
                           J = J,
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
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              ATau <- prior@ATau@.Data
              contrastsArg <- prior@contrastsArg
              formula <- prior@formula
              nuBeta <- prior@nuBeta@.Data
              J <- makeJPredict(metadata)
              Z <- makeZ(formula = formula[-2L],
                         data = data,
                         metadata = metadata,
                         contrastsArg = contrastsArg)
              n <- J@.Data
              UBeta <- makeU(nu = nuBeta, A = ATau, n = J)
              methods::new("ExchRobustCov",
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           ATau = prior@ATau,
                           contrastsArg = prior@contrastsArg,
                           eta = prior@eta,
                           formula = prior@formula,
                           J = J,
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
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              methods::new("DLMNoTrendNormZeroNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = prior@ATau,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           iAlong = prior@iAlong,
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
                           tau = prior@tau,
                           tauMax = prior@tauMax)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendNormZeroNoSeason"),
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              methods::new("DLMWithTrendNormZeroNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = prior@ATau,
                           alphaDLM = l.all$alphaDLM,
                           CWithTrend = l.with.trend$CWithTrend,
                           DC = l.with.trend$DC,
                           DCInv = l.with.trend$DCInv,
                           DRInv = l.with.trend$DRInv,
                           deltaDLM = l.with.trend$deltaDLM,
                           GWithTrend = prior@GWithTrend,
                           iAlong = prior@iAlong,
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
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
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
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  CSeason = l.season$CSeason,
                  iAlong = prior@iAlong,
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
                  tau = prior@tau,
                  tauMax = prior@tauMax)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendNormZeroWithSeason"),
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
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
                  alphaDLM = l.all$alphaDLM,
                  CSeason = l.season$CSeason,
                  CWithTrend = l.with.trend$CWithTrend,
                  DC = l.with.trend$DC,
                  DCInv = l.with.trend$DCInv,
                  DRInv = l.with.trend$DRInv,
                  deltaDLM = l.with.trend$deltaDLM,
                  GWithTrend = prior@GWithTrend,
                  iAlong = prior@iAlong,
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
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata)
              methods::new("DLMNoTrendNormCovNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = prior@ATau,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           contrastsArg = prior@contrastsArg,
                           eta = prior@eta,
                           formula = prior@formula,
                           iAlong = prior@iAlong,
                           iteratorState = l.all$iteratorState,
                           iteratorStateOld = l.all$iteratorStateOld,
                           iteratorV = l.all$iteratorV,
                           J = l.all$J,
                           JOld = l.all$JOld,
                           K = l.all$K,
                           L = l.all$L,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
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
                           tau = prior@tau,
                           tauMax = prior@tauMax,
                           UEtaCoef = prior@UEtaCoef,
                           Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendNormCovNoSeason"),
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata)
              methods::new("DLMWithTrendNormCovNoSeasonPredict",
                           AAlpha = prior@AAlpha,
                           aWithTrend = l.with.trend$aWithTrend,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           ATau = prior@ATau,
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
                           iAlong = prior@iAlong,
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
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata)
              methods::new("DLMNoTrendNormCovWithSeasonPredict",
                  AAlpha = prior@AAlpha,
                  ASeason = prior@ASeason,
                  AEtaCoef = prior@AEtaCoef,
                  AEtaIntercept = prior@AEtaIntercept,
                  aNoTrend = l.no.trend$aNoTrend,
                  aSeason = l.season$aSeason,
                  ATau = prior@ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  CSeason = l.season$CSeason,
                  contrastsArg = prior@contrastsArg,
                  eta = prior@eta,
                  formula = prior@formula,
                  iAlong = prior@iAlong,
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
                  tau = prior@tau,
                  tauMax = prior@tauMax,
                  UEtaCoef = prior@UEtaCoef,
                  Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendNormCovWithSeason"),
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata)
              methods::new("DLMWithTrendNormCovWithSeasonPredict",
                           AAlpha = prior@AAlpha,
                           ASeason = prior@ASeason,
                           AEtaCoef = prior@AEtaCoef,
                           AEtaIntercept = prior@AEtaIntercept,
                           aSeason = l.season$aSeason,
                           aWithTrend = l.with.trend$aWithTrend,
                           ATau = prior@ATau,
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
                           iAlong = prior@iAlong,
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
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata)
              methods::new("DLMNoTrendRobustZeroNoSeasonPredict",
                  AAlpha = prior@AAlpha,
                  aNoTrend = l.no.trend$aNoTrend,
                  ATau = prior@ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  iAlong = prior@iAlong,
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
                  nuBeta = prior@nuBeta,
                  nuTau = prior@nuTau,
                  omegaAlpha = prior@omegaAlpha,
                  omegaAlphaMax = prior@omegaAlphaMax,
                  phi = prior@phi,
                  phiKnown = prior@phiKnown,
                  RNoTrend = l.no.trend$RNoTrend,
                  tau = prior@tau,
                  tauMax = prior@tauMax,
                  UBeta = l.robust$UBeta)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendRobustZeroNoSeason"),
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata)
              methods::new("DLMWithTrendRobustZeroNoSeasonPredict",
                  AAlpha = prior@AAlpha,
                  aWithTrend = l.with.trend$aWithTrend,
                  ATau = prior@ATau,
                  alphaDLM = l.all$alphaDLM,
                  CWithTrend = l.with.trend$CWithTrend,
                  DC = l.with.trend$DC,
                  DCInv = l.with.trend$DCInv,
                  DRInv = l.with.trend$DRInv,
                  deltaDLM = l.with.trend$deltaDLM,
                  GWithTrend = prior@GWithTrend,
                  iAlong = prior@iAlong,
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
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata)
              methods::new("DLMNoTrendRobustZeroWithSeasonPredict",
                  AAlpha = prior@AAlpha,
                  ASeason = prior@ASeason,
                  aNoTrend = l.no.trend$aNoTrend,
                  aSeason = l.season$aSeason,
                  ATau = prior@ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  CSeason = l.season$CSeason,
                  iAlong = prior@iAlong,
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
                  tau = prior@tau,
                  tauMax = prior@tauMax,
                  UBeta = l.robust$UBeta)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendRobustZeroWithSeason"),
          function(prior, data, metadata, name, along) {
              if (!is.null(data))
                  stop(gettextf("covariate data supplied for prior for '%s', but prior does not use covariates",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata)
              methods::new("DLMWithTrendRobustZeroWithSeasonPredict",
                  AAlpha = prior@AAlpha,
                  ASeason = prior@ASeason,
                  aSeason = l.season$aSeason,
                  aWithTrend = l.with.trend$aWithTrend,
                  ATau = prior@ATau,
                  alphaDLM = l.all$alphaDLM,
                  CSeason = l.season$CSeason,
                  CWithTrend = l.with.trend$CWithTrend,
                  DC = l.with.trend$DC,
                  DCInv = l.with.trend$DCInv,
                  DRInv = l.with.trend$DRInv,
                  deltaDLM = l.with.trend$deltaDLM,
                  GWithTrend = prior@GWithTrend,
                  iAlong = prior@iAlong,
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
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata)
              methods::new("DLMNoTrendRobustCovNoSeasonPredict",
                  AAlpha = prior@AAlpha,
                  AEtaCoef = prior@AEtaCoef,
                  AEtaIntercept = prior@AEtaIntercept,
                  aNoTrend = l.no.trend$aNoTrend,
                  ATau = prior@ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  contrastsArg = prior@contrastsArg,
                  eta = prior@eta,
                  formula = prior@formula,
                  iAlong = prior@iAlong,
                  iteratorState = l.all$iteratorState,
                  iteratorStateOld = l.all$iteratorStateOld,
                  iteratorV = l.all$iteratorV,
                  J = l.all$J,
                  JOld = l.all$JOld,
                  K = l.all$K,
                  L = l.all$L,
                  mNoTrend = l.no.trend$mNoTrend,
                  m0NoTrend = l.no.trend$m0NoTrend,
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
                  tau = prior@tau,
                  tauMax = prior@tauMax,
                  UBeta = l.robust$UBeta,
                  UEtaCoef = prior@UEtaCoef,
                  Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendRobustCovNoSeason"),
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata)
              methods::new("DLMWithTrendRobustCovNoSeasonPredict",
                  AAlpha = prior@AAlpha,
                  aWithTrend = l.with.trend$aWithTrend,
                  AEtaCoef = prior@AEtaCoef,
                  AEtaIntercept = prior@AEtaIntercept,
                  ATau = prior@ATau,
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
                  iAlong = prior@iAlong,
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
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.no.trend <- initialDLMNoTrendPredict(prior = prior,
                                                     metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata)
              methods::new("DLMNoTrendRobustCovWithSeasonPredict",
                  AAlpha = prior@AAlpha,
                  ASeason = prior@ASeason,
                  AEtaCoef = prior@AEtaCoef,
                  AEtaIntercept = prior@AEtaIntercept,
                  aNoTrend = l.no.trend$aNoTrend,
                  aSeason = l.season$aSeason,
                  ATau = prior@ATau,
                  alphaDLM = l.all$alphaDLM,
                  CNoTrend = l.no.trend$CNoTrend,
                  CSeason = l.season$CSeason,
                  contrastsArg = prior@contrastsArg,
                  eta = prior@eta,
                  formula = prior@formula,
                  iAlong = prior@iAlong,
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
                  tau = prior@tau,
                  tauMax = prior@tauMax,
                  UBeta = l.robust$UBeta,
                  UEtaCoef = prior@UEtaCoef,
                  Z = l.cov$Z)
          })

## HAS_TESTS
setMethod("initialPriorPredict",
          signature(prior = "DLMWithTrendRobustCovWithSeason"),
          function(prior, data, metadata, name, along) {
              if (is.null(data))
                  stop(gettextf("prior for '%s' uses covariates, but no covariate data supplied",
                                name))
              l.all <- initialDLMAllPredict(prior = prior,
                                            metadata = metadata,
                                            name = name,
                                            along = along)
              l.with.trend <- initialDLMWithTrendPredict(prior = prior,
                                                         metadata = metadata)
              l.season <- initialDLMSeasonPredict(prior = prior,
                                                  metadata = metadata)
              l.cov <- initialCovPredict(prior = prior,
                                         data = data,
                                         metadata = metadata)
              l.robust <- initialRobustPredict(prior = prior,
                                               metadata = metadata)
              methods::new("DLMWithTrendRobustCovWithSeasonPredict",
                  AAlpha = prior@AAlpha,
                  ASeason = prior@ASeason,
                  AEtaCoef = prior@AEtaCoef,
                  AEtaIntercept = prior@AEtaIntercept,
                  aSeason = l.season$aSeason,
                  aWithTrend = l.with.trend$aWithTrend,
                  ATau = prior@ATau,
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
                  iAlong = prior@iAlong,
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



