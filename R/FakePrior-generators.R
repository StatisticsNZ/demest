

## HAS_TESTS
setMethod("fakePrior",
          signature(object = "SpecExchNormZero",
                    metadata = "MetaData"),
          function(object, metadata, isSaturated) {
              ATau <- object@ATau
              nuTau <- object@nuTau
              tauMax <- object@tauMax
              l <- makeFakeScale(A = ATau,
                                 nu = nuTau,
                                 scaleMax = tauMax,
                                 functionName = "Error")
              tau <- l$scale
              ATau <- l$A
              tauMax <- l$scaleMax
              J <- makeJPredict(metadata)
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              methods::new("FakeExchNormZero",
                           ATau = ATau,
                           isSaturated = isSaturated,
                           J = J,
                           nuTau = nuTau,
                           tau = tau,
                           tauMax = tauMax)
          })

## HAS_TESTS
setMethod("fakePrior",
          signature(object = "SpecDLMNoTrendNormZeroNoSeason",
                    metadata = "MetaData"),
          function(object, metadata, isSaturated) {
              l.all <- initialFakeDLMAll(spec = object,
                                         metadata = metadata)
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              ans <- methods::new("FakeDLMNoTrendNormZeroNoSeason",
                                  AAlpha = l.all$AAlpha,
                                  alphaDLM = l.all$alphaDLM,
                                  ATau = l.all$ATau,
                                  iAlong = l.all$iAlong,
                                  isSaturated = isSaturated,
                                  iteratorState = l.all$iteratorState,
                                  iteratorV = l.all$iteratorV,
                                  J = l.all$J,
                                  K = l.all$K,
                                  L = l.all$L,
                                  minPhi = l.all$minPhi,
                                  maxPhi = l.all$maxPhi,
                                  nuAlpha = l.all$nuAlpha,
                                  nuTau = l.all$nuTau,
                                  omegaAlpha = l.all$omegaAlpha,
                                  omegaAlphaMax = l.all$omegaAlphaMax,
                                  phi = l.all$phi,
                                  shape1Phi = l.all$shape1Phi,
                                  shape2Phi = l.all$shape2Phi,
                                  tau = l.all$tau,
                                  tauMax = l.all$tauMax)
              predictAlphaDLMNoTrend(ans, useC = TRUE)
          })


## HAS_TESTS
setMethod("fakePrior",
          signature(object = "SpecDLMWithTrendNormZeroNoSeason",
                    metadata = "MetaData"),
          function(object, metadata, isSaturated) {
              l.all <- initialFakeDLMAll(spec = object,
                                         metadata = metadata)
              l.with.trend <- initialFakeDLMWithTrend(spec = object,
                                                      metadata = metadata)
              isSaturated <- methods::new("LogicalFlag", isSaturated)
              ans <- methods::new("FakeDLMWithTrendNormZeroNoSeason",
                                  AAlpha = l.all$AAlpha,
                                  ADelta = l.with.trend$ADelta,
                                  ADelta0 = l.with.trend$ADelta0,
                                  ATau = l.all$ATau,
                                  alphaDLM = l.all$alphaDLM,
                                  deltaDLM = l.with.trend$deltaDLM,
                                  hasLevel = l.with.trend$hasLevel,
                                  iAlong = l.all$iAlong,
                                  isSaturated = isSaturated,
                                  iteratorState = l.all$iteratorState,
                                  iteratorV = l.all$iteratorV,
                                  J = l.all$J,
                                  K = l.all$K,
                                  L = l.all$L,
                                  meanDelta0 = l.with.trend$meanDelta0,
                                  minPhi = l.all$minPhi,
                                  maxPhi = l.all$maxPhi,
                                  nuAlpha = l.all$nuAlpha,
                                  nuTau = l.all$nuTau,
                                  omegaAlpha = l.all$omegaAlpha,
                                  omegaAlphaMax = l.all$omegaAlphaMax,
                                  omegaDelta = l.with.trend$omegaDelta,
                                  omegaDeltaMax = l.with.trend$omegaDeltaMax,
                                  phi = l.all$phi,
                                  shape1Phi = l.all$shape1Phi,
                                  shape2Phi = l.all$shape2Phi,
                                  tau = l.all$tau,
                                  tauMax = l.all$tauMax)
              predictAlphaDeltaDLMWithTrend(ans, useC = TRUE)
          })
