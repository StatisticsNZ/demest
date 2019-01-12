## betaIsEstimated ############################################################

## HAS_TESTS 
setMethod("betaIsEstimated",
    signature(prior = "Prior"),
    function(prior) {
        FALSE
    })

## HAS_TESTS
setMethod("betaIsEstimated",
          signature(prior = "Zero"),
          function(prior) {
              FALSE
          })



## drawPrior #########################################################################

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "ExchFixed"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_ExchFixed_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "ExchNormZero"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_ExchNormZero_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "ExchRobustZero"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_ExchRobustZero_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "ExchNormCov"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_ExchNormCov_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior <- drawUEtaCoef(prior)
                  prior <- drawEta(prior)
                  prior
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "ExchRobustCov"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_ExchNormCov_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior <- predictUBeta(prior)
                  prior <- drawUEtaCoef(prior)
                  prior <- drawEta(prior)
                  prior
              }
          })

## DLM - Norm, Zero

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "DLMNoTrendNormZeroNoSeason"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_DLMNoTrendNormZeroNoSeason_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior <- drawOmegaAlpha(prior)
                  prior <- drawPhi(prior)
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "DLMWithTrendNormZeroNoSeason"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_DLMWithTrendNormZeroNoSeason_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior <- drawOmegaAlpha(prior)
                  prior <- drawOmegaDelta(prior)
                  prior <- drawPhi(prior)
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "DLMNoTrendNormZeroWithSeason"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_DLMNoTrendNormZeroNoSeason_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior <- drawOmegaAlpha(prior)
                  prior <- drawOmegaSeason(prior)
                  prior <- drawPhi(prior)
                  prior <- predictSeason(prior)
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("drawPrior",
          signature(prior = "DLMWithTrendNormZeroWithSeason"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_DLMWithTrendNormZeroNoSeason_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior <- drawOmegaAlpha(prior)
                  prior <- drawOmegaDelta(prior)
                  prior <- drawOmegaSeason(prior)
                  prior <- drawPhi(prior)
                  prior <- predictSeason(prior)
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior
              }
          })

## DLM - Norm, Cov

setMethod("drawPrior",
          signature(prior = "DLMNoTrendNormCovNoSeason"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(drawPrior_DLMNoTrendNormCovNoSeason_R, prior)
                  else
                      .Call(drawPrior_R, prior)
              }
              else {
                  prior <- drawTau(prior)
                  prior <- drawOmegaAlpha(prior)
                  prior <- drawPhi(prior)
                  prior <- drawUEtaCoef(prior)
                  prior <- drawEta(prior)
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior
              }
          })




## makeOutputPrior ###################################################################


## Function "whereEstimated" controls whether a parameter
## estimated is actually printed. We can't skip parameters,
## because that messes up function 'changeInPos'.

## ExchFixed

## NO_TESTS
setMethod("makeOutputPrior",
          signature(prior = "TimeInvariant",
                    metadata = "ANY"),
          function(prior) {
              NULL
          })

## ExchFixed

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "ExchFixed",
                    metadata = "ANY"),
          function(prior) {
              scaleError <- prior@tau@.Data
              list(scaleError = scaleError)
          })


## Exch

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "ExchNormZero",
                    metadata = "ANY"),
          function(prior, pos) {
              scaleError <- makeOutputPriorScale(pos = pos)
              list(scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "ExchNormCov",
                    metadata = "MetaData"),
          function(prior, metadata, pos) {
              P <- prior@P@.Data
              Z <- prior@Z@.Data
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(coef = coef,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "ExchRobustZero",
                    metadata = "MetaData"),
          function(prior, metadata, pos) {
              scaleError <- makeOutputPriorScale(pos = pos)
              list(scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "ExchRobustCov",
                    metadata = "MetaData"),
          function(prior, metadata, pos) {
              P <- prior@P@.Data
              Z <- prior@Z@.Data
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(coef = coef,
                   scaleError = scaleError)
          })


## DLM - Norm, Zero

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMNoTrendNormZeroNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   damp = damp,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMWithTrendNormZeroNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              has.level <- prior@hasLevel@.Data
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   trend = trend,
                   scaleTrend = scaleTrend,
                   damp = damp,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMNoTrendNormZeroWithSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              nSeason <- prior@nSeason@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputStateDLM(iterator = iterator,
                                           metadata = metadata,
                                           nSeason = nSeason,
                                           iAlong = iAlong,
                                           pos = pos,
                                           strucZeroArray = strucZeroArray,
                                           margin = margin)
              pos <- pos + (K + 1L) * L * nSeason
              scaleSeason <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   damp = damp,
                   season = season,
                   scaleSeason = scaleSeason,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMWithTrendNormZeroWithSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              nSeason <- prior@nSeason@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              has.level <- prior@hasLevel@.Data
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputStateDLM(iterator = iterator,
                                           metadata = metadata,
                                           nSeason = nSeason,
                                           iAlong = iAlong,
                                           pos = pos,
                                           strucZeroArray = strucZeroArray,
                                           margin = margin)
              pos <- pos + (K + 1L) * L * nSeason
              scaleSeason <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   trend = trend,
                   scaleTrend = scaleTrend,
                   damp = damp,
                   season = season,
                   scaleSeason = scaleSeason,
                   scaleError = scaleError)
          })


## DLM - Norm, Cov

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMNoTrendNormCovNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   damp = damp,
                   coef = coef,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMWithTrendNormCovNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              has.level <- prior@hasLevel@.Data
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   trend = trend,
                   scaleTrend = scaleTrend,
                   damp = damp,
                   coef = coef,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMNoTrendNormCovWithSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputStateDLM(iterator = iterator,
                                           metadata = metadata,
                                           nSeason = nSeason,
                                           iAlong = iAlong,
                                           pos = pos,
                                           strucZeroArray = strucZeroArray,
                                           margin = margin)
              pos <- pos + (K + 1L) * L * nSeason
              scaleSeason <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   damp = damp,
                   season = season,
                   scaleSeason = scaleSeason,
                   coef = coef,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMWithTrendNormCovWithSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              has.level <- prior@hasLevel@.Data
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputStateDLM(iterator = iterator,
                                           metadata = metadata,
                                           nSeason = nSeason,
                                           iAlong = iAlong,
                                           pos = pos,
                                           strucZeroArray = strucZeroArray,
                                           margin = margin)
              pos <- pos + (K + 1L) * L * nSeason
              scaleSeason <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   trend = trend,
                   scaleTrend = scaleTrend,
                   damp = damp,
                   season = season,
                   scaleSeason = scaleSeason,
                   coef = coef,
                   scaleError = scaleError)
          })


## DLM - Robust, Zero

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMNoTrendRobustZeroNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   damp = damp,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMWithTrendRobustZeroNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              has.level <- prior@hasLevel@.Data
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   trend = trend,
                   scaleTrend = scaleTrend,
                   damp = damp,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMNoTrendRobustZeroWithSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputStateDLM(iterator = iterator,
                                           metadata = metadata,
                                           nSeason = nSeason,
                                           iAlong = iAlong,
                                           pos = pos,
                                           strucZeroArray = strucZeroArray,
                                           margin = margin)
              pos <- pos + (K + 1L) * L * nSeason
              scaleSeason <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   damp = damp,
                   season = season,
                   scaleSeason = scaleSeason,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMWithTrendRobustZeroWithSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              has.level <- prior@hasLevel@.Data
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputStateDLM(iterator = iterator,
                                           metadata = metadata,
                                           nSeason = nSeason,
                                           iAlong = iAlong,
                                           pos = pos,
                                           strucZeroArray = strucZeroArray,
                                           margin = margin)
              pos <- pos + (K + 1L) * L * nSeason
              scaleSeason <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   trend = trend,
                   scaleTrend = scaleTrend,
                   damp = damp,
                   season = season,
                   scaleSeason = scaleSeason,
                   scaleError = scaleError)
          })


## DLM - Robust, Cov

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMNoTrendRobustCovNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   damp = damp,
                   coef = coef,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMWithTrendRobustCovNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              has.level <- prior@hasLevel@.Data
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   trend = trend,
                   scaleTrend = scaleTrend,
                   damp = damp,
                   coef = coef,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMNoTrendRobustCovWithSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputStateDLM(iterator = iterator,
                                           metadata = metadata,
                                           nSeason = nSeason,
                                           iAlong = iAlong,
                                           pos = pos,
                                           strucZeroArray = strucZeroArray,
                                           margin = margin)
              pos <- pos + (K + 1L) * L * nSeason
              scaleSeason <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   damp = damp,
                   season = season,
                   scaleSeason = scaleSeason,
                   coef = coef,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "DLMWithTrendRobustCovWithSeason",
                    metadata = "MetaData"),
          function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              nSeason <- prior@nSeason@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              has.level <- prior@hasLevel@.Data
              phi <- prior@phi
              phiKnown <- prior@phiKnown@.Data
              level <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputStateDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = NULL,
                                          iAlong = iAlong,
                                          pos = pos,
                                          strucZeroArray = strucZeroArray,
                                          margin = margin)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputStateDLM(iterator = iterator,
                                           metadata = metadata,
                                           nSeason = nSeason,
                                           iAlong = iAlong,
                                           pos = pos,
                                           strucZeroArray = strucZeroArray,
                                           margin = margin)
              pos <- pos + (K + 1L) * L * nSeason
              scaleSeason <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              coef <- makeOutputPriorCoef(Z = Z,
                                          pos = pos)
              pos <- pos + P
              scaleError <- makeOutputPriorScale(pos = pos)
              list(level = level,
                   scaleLevel = scaleLevel,
                   trend = trend,
                   scaleTrend = scaleTrend,
                   damp = damp,
                   season = season,
                   scaleSeason = scaleSeason,
                   coef = coef,
                   scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "KnownCertain",
                    metadata = "MetaData"),
          function(prior, metadata, pos) {
              alpha <- prior@alphaKnown@.Data
              .Data <- array(alpha,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              mean <- new("Values",
                          .Data = .Data,
                          metadata = metadata)
              list(mean = mean)
          })

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "KnownUncertain",
                    metadata = "MetaData"),
          function(prior, metadata, pos) {
              alpha <- prior@alphaKnown@.Data
              A <- prior@AKnownVec@.Data
              .Data.mean <- array(alpha,
                                  dim = dim(metadata),
                                  dimnames = dimnames(metadata))
              .Data.sd <- array(A,
                                dim = dim(metadata),
                                dimnames = dimnames(metadata))
              mean <- new("Values",
                          .Data = .Data.mean,
                          metadata = metadata)
              sd <- new("Values",
                        .Data = .Data.sd,
                        metadata = metadata)
              list(mean = mean,
                   sd = sd)
          })


## Mix

## HAS_TESTS
setMethod("makeOutputPrior",
          signature(prior = "MixNormZero",
                    metadata = "MetaData"),
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              iAlong <- prior@iAlong
              dim.beta <- prior@dimBeta
              n.beta.no.along <- prior@nBetaNoAlongMix
              index.class.max <- prior@indexClassMaxMix@.Data
              n.along <- dim.beta[iAlong]
              metadata.vectors <- makeMetadataVectorsMix(metadata = metadata,
                                                         iAlong = iAlong,
                                                         indexClassMax = index.class.max)
              metadata.weights <- makeMetadataWeightsMix(metadata = metadata,
                                                         iAlong = iAlong,
                                                         indexClassMax = index.class.max)
              ## prodVectorsMix
              components <- Skeleton(metadata = metadata.vectors,
                                     first = pos)
              pos <- pos + n.beta.no.along * index.class.max
              ## omegaVectorsMix
              scaleComponents <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              ## weightMix
              weights <- Skeleton(metadata = metadata.weights,
                                  first = pos)
              pos <- pos + n.along * index.class.max
              ## componentWeightMix
              level1 <- Skeleton(metadata = metadata.weights,
                                   first = pos)
              pos <- pos + n.along * index.class.max
              ## omegaComponentWeightMix
              scale1 <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              ## levelComponentWeightMix
              level2 <- Skeleton(metadata = metadata.weights,
                                   first = pos)
              pos <- pos + n.along * index.class.max
              ## meanLevelComponentWeightMix
              mean <- Skeleton(first = pos)
              pos <- pos + 1L
              ## phiMix
              damp <- Skeleton(first = pos)
              pos <- pos + 1L
              ## omegaLevelComponentWeightMix
              scale2 <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              ## tau
              scaleError <- makeOutputPriorScale(pos = pos)
              ## return
              list(components = components,
                   scaleComponents = scaleComponents,
                   weights = weights,
                   level1 = level1,
                   scale1 = scale1,
                   level2 = level2,
                   mean = mean,
                   damp = damp,
                   scale2 = scale2,
                   scaleError = scaleError)
          })



## NO_TESTS
setMethod("makeOutputPrior",
          signature(prior = "Zero",
                    metadata = "ANY"),
          function(prior) {
              NULL
          })



## predictPrior ###########################################################


## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchFixed"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchFixed_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchNormZero"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchNormZero_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchRobustZero"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchRobustZero_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictUBeta(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchNormCov"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchNormCov_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "ExchRobustCov"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_ExchRobustCov_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictUBeta(prior)
                  prior
              }
          })


## DLMPredict - Norm, Zero

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMNoTrendNormZeroNoSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMNoTrendNormZeroNoSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMWithTrendNormZeroNoSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMWithTrendNormZeroNoSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMNoTrendNormZeroWithSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMNoTrendNormZeroWithSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior <- predictSeason(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMWithTrendNormZeroWithSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMWithTrendNormZeroWithSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior <- predictSeason(prior)
                  prior
              }
          })




## DLMPredict - Norm, Cov (all identical to methods without covariates)

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMNoTrendNormCovNoSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMNoTrendNormCovNoSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMWithTrendNormCovNoSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMWithTrendNormCovNoSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMNoTrendNormCovWithSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMNoTrendNormCovWithSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior <- predictSeason(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMWithTrendNormCovWithSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMWithTrendNormCovWithSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior <- predictSeason(prior)
                  prior
              }
          })


## DLMPredict - Robust, Zero

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMNoTrendRobustZeroNoSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMNoTrendRobustZeroNoSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMWithTrendRobustZeroNoSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMWithTrendRobustZeroNoSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMNoTrendRobustZeroWithSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMNoTrendRobustZeroWithSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior <- predictSeason(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMWithTrendRobustZeroWithSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMWithTrendRobustZeroWithSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior <- predictSeason(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })




## DLMPredict - Robust, Cov (all identical to methods without covariates)

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMNoTrendRobustCovNoSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMNoTrendRobustCovNoSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMWithTrendRobustCovNoSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMWithTrendRobustCovNoSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMNoTrendRobustCovWithSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMNoTrendRobustCovWithSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDLMNoTrend(prior)
                  prior <- predictSeason(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "DLMWithTrendRobustCovWithSeasonPredict"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_DLMWithTrendRobustCovWithSeasonPredict_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior <- predictAlphaDeltaDLMWithTrend(prior)
                  prior <- predictSeason(prior)
                  prior <- predictUBeta(prior)
                  prior
              }
          })


## Known

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "KnownCertain"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_KnownCertain_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "KnownUncertain"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_KnownUncertain_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior
              }
          })


## Mix

## TRANSLATED
## HAS_TESTS
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
                  prior <- updateIndexClassMaxUsedMix(prior)
                  prior <- updateAlphaMix(prior)
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictPrior",
          signature(prior = "Zero"),
          function(prior, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(prior)
              if (useC) {
                  if (useSpecific)
                      .Call(predictPrior_Zero_R, prior)
                  else
                      .Call(predictPrior_R, prior)
              }
              else {
                  prior
              }
          })


## printPriorEqns ####################################################################

setMethod("printPriorEqns",
          signature(object = "DLM"),
          function(object, name = NULL, order = 1L) {
              has.trend <- methods::is(object, "WithTrendMixin")
              has.covariates <- object@hasCovariates@.Data
              has.season <- object@hasSeason@.Data
              printDLMEqns(object = object,
                           name = name,
                           order = order,
                           hasTrend = has.trend,
                           hasCovariates = has.covariates,
                           hasSeason = has.season)
          })

setMethod("printPriorEqns",
          signature(object = "Exch"),
          function(object, name = NULL, order = 1L) {
              has.covariates <- object@hasCovariates@.Data
              printExchEqns(object = object,
                            name = name,
                            hasCovariates = has.covariates)
          })

setMethod("printPriorEqns",
          signature(object = "ExchFixed"),
          function(object, name = NULL, order = 1L) {
              printExchFixedEqns(object = object,
                                 name = name)
          })

setMethod("printPriorEqns",
          signature(object = "Mix"),
          function(object, name = NULL, order = 1L) {
              has.covariates <- object@hasCovariates@.Data
              printMixEqns(object = object,
                           name = name,
                           hasCovariates = has.covariates)
          })

setMethod("printPriorEqns",
          signature(object = "TimeInvariant"),
          function(object, name = NULL, order = 1L) {
              cat(gettextf("-- values for '%s' held constant --\n",
                           name))
          })

setMethod("printPriorEqns",
          signature(object = "Zero"),
          function(object, name = NULL, order = 1L) {
              printZeroEqns(name)
          })



## printPriorIntercept ###############################################################

setMethod("printPriorIntercept",
          signature(object = "ExchFixed"),
          function(object) {
              sd <- object@tau@.Data
              cat("     (Intercept) ~ N(0, ", format(sd, digits = 4), "^2)\n", sep = "")
          })

setMethod("printPriorIntercept",
          signature(object = "TimeInvariant"),
          function(object) {
              cat(gettextf("-- values for '%s' held constant --\n",
                           "(Intercept)"))
          })


## rescalePairPriors ##################################################################

## HAS_TESTS
setMethod("rescalePairPriors",
          signature(priorHigh = "Exchangeable",
                    priorLow = "Exchangeable"),
          function(priorHigh, priorLow, skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              if (!all(names.low %in% names.high))
                  return(NULL)
              beta.high <- fetchResults(object = skeletonBetaHigh,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              beta.low <- fetchResults(object = skeletonBetaLow,
                                       filename = filename,
                                       iterations = NULL,
                                       nIteration = nIteration,
                                       lengthIter = lengthIter)
              names.high.only <- setdiff(names.high, names.low)
              means.shared <- collapseDimension(beta.high,
                                                dimension = names.high.only,
                                                weights = 1,
                                                na.rm = TRUE)
              rescaleAndWriteBetas(high = beta.high,
                                   low = beta.low,
                                   adj = means.shared,
                                   skeletonHigh = skeletonBetaHigh,
                                   skeletonLow = skeletonBetaLow,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              recordAdjustments(priorHigh = priorHigh,
                                priorLow = priorLow,
                                namesHigh = names.high,
                                namesLow = names.low,
                                adj = means.shared,
                                adjustments = adjustments,
                                prefixAdjustments = prefixAdjustments)
              NULL
          })

## HAS_TESTS
setMethod("rescalePairPriors",
          signature(priorHigh = "Exchangeable",
                    priorLow = "DLM"),
          function(priorHigh, priorLow,
                   skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              i.along.low <- priorLow@iAlong
              phi.low <- priorLow@phi
              phi.known.low <- priorLow@phiKnown@.Data
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              skeleton.level.low <- skeletonsPriorLow$level
              has.trend.low <- methods::is(priorLow, "WithTrendMixin")
              level.non.stationary <- has.trend.low || (phi.known.low && isTRUE(all.equal(phi.low, 1)))
              if (!level.non.stationary)
                  return(NULL)
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              names.low <- names.low[-i.along.low]
              if (!all(names.low %in% names.high))
                  return(NULL)
              beta.high <- fetchResults(object = skeletonBetaHigh,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              beta.low <- fetchResults(object = skeletonBetaLow,
                                       filename = filename,
                                       iterations = NULL,
                                       nIteration = nIteration,
                                       lengthIter = lengthIter)
              level.low <- readStateDLMFromFile(skeleton = skeleton.level.low,
                                                filename = filename,
                                                iterations = NULL,
                                                nIteration = nIteration,
                                                lengthIter = lengthIter,
                                                only0 = FALSE)
              names.high.only <- setdiff(names.high, names.low)
              means.shared <- collapseDimension(beta.high,
                                                dimension = names.high.only,
                                                weights = 1,
                                                na.rm = TRUE)
              rescaleAndWriteBetas(high = beta.high,
                                   low = beta.low,
                                   adj = means.shared,
                                   skeletonHigh = skeletonBetaHigh,
                                   skeletonLow = skeletonBetaLow,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              recordAdjustments(priorHigh = priorHigh,
                                priorLow = priorLow,
                                namesHigh = names.high,
                                namesLow = names.low,
                                adj = means.shared,
                                adjustments = adjustments,
                                prefixAdjustments = prefixAdjustments)
              level.low <- level.low + means.shared
              overwriteValuesOnFile(object = level.low,
                                    skeleton = skeleton.level.low,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              NULL
          })


setMethod("rescalePairPriors",
          signature(priorHigh = "DLM",
                    priorLow = "Exchangeable"),
          function(priorHigh, priorLow,
                   skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              i.along.high <- priorHigh@iAlong
              phi.high <- priorHigh@phi
              phi.known.high <- priorHigh@phiKnown@.Data
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              skeleton.level.high <- skeletonsPriorHigh$level
              has.trend.high <- methods::is(priorHigh, "WithTrendMixin")
              level.non.stationary <- has.trend.high || (phi.known.high && isTRUE(all.equal(phi.high, 1)))
              if (!level.non.stationary)
                  return(NULL)
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              names.high <- names.high[-i.along.high]
              if (!all(names.low %in% names.high))
                  return(NULL)
              beta.high <- fetchResults(object = skeletonBetaHigh,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              beta.low <- fetchResults(object = skeletonBetaLow,
                                       filename = filename,
                                       iterations = NULL,
                                       nIteration = nIteration,
                                       lengthIter = lengthIter)
              level.0.high <- readStateDLMFromFile(skeleton = skeleton.level.high,
                                                   filename = filename,
                                                   iterations = NULL,
                                                   nIteration = nIteration,
                                                   lengthIter = lengthIter,
                                                   only0 = TRUE)
              level.high <- readStateDLMFromFile(skeleton = skeleton.level.high,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration,
                                                 lengthIter = lengthIter,
                                                 only0 = FALSE)
              names.high.only <- setdiff(names.high, names.low)
              means.shared <- collapseDimension(level.0.high,
                                                dimension = names.high.only,
                                                weights = 1,
                                                na.rm = TRUE)
              rescaleAndWriteBetas(high = beta.high,
                                   low = beta.low,
                                   adj = means.shared,
                                   skeletonHigh = skeletonBetaHigh,
                                   skeletonLow = skeletonBetaLow,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              recordAdjustments(priorHigh = priorHigh,
                                priorLow = priorLow,
                                namesHigh = names.high,
                                namesLow = names.low,
                                adj = means.shared,
                                adjustments = adjustments,
                                prefixAdjustments = prefixAdjustments)
              level.high <- level.high - means.shared
              overwriteValuesOnFile(object = level.high,
                                    skeleton = skeleton.level.high,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              NULL
          })

## HAS_TESTS
setMethod("rescalePairPriors",
          signature(priorHigh = "DLM",
                    priorLow = "DLM"),
          function(priorHigh, priorLow,
                   skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              ## extract parameters
              i.along.high <- priorHigh@iAlong
              i.along.low <- priorLow@iAlong
              phi.high <- priorHigh@phi
              phi.low <- priorLow@phi
              phi.known.high <- priorHigh@phiKnown@.Data
              phi.known.low <- priorLow@phiKnown@.Data
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              skeleton.level.high <- skeletonsPriorHigh$level
              skeleton.level.low <- skeletonsPriorLow$level
              has.trend.high <- methods::is(priorHigh, "WithTrendMixin")
              has.trend.low <- methods::is(priorLow, "WithTrendMixin")
              ## if neither series non-stationary, no rescaling needed
              if (has.trend.high)
                  level.non.stationary.high <- TRUE
              else
                  level.non.stationary.high <- (phi.known.high && isTRUE(all.equal(phi.high, 1)))
              if (has.trend.low)
                  level.non.stationary.low <- TRUE
              else
                  level.non.stationary.low <- (phi.known.low && isTRUE(all.equal(phi.low, 1)))
              at.least.one.level.is.stationary <- !level.non.stationary.high || !level.non.stationary.low
              if (at.least.one.level.is.stationary)
                  return(NULL)
              ## if lower-order term has dimension not in higher-order term, no rescaling
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              names.high <- names.high[-i.along.high]
              names.low <- names.low[-i.along.low]
              if (!all(names.low %in% names.high))
                  return(NULL)
              ## extract parameter estimates
              beta.high <- fetchResults(object = skeletonBetaHigh,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              beta.low <- fetchResults(object = skeletonBetaLow,
                                       filename = filename,
                                       iterations = NULL,
                                       nIteration = nIteration,
                                       lengthIter = lengthIter)
              level.high <- readStateDLMFromFile(skeleton = skeleton.level.high,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration,
                                                 lengthIter = lengthIter,
                                                 only0 = FALSE)
              level.low <- readStateDLMFromFile(skeleton = skeleton.level.low,
                                                filename = filename,
                                                iterations = NULL,
                                                nIteration = nIteration,
                                                lengthIter = lengthIter,
                                                only0 = FALSE)
              level.0.high <- readStateDLMFromFile(skeleton = skeleton.level.high,
                                                   filename = filename,
                                                   iterations = NULL,
                                                   nIteration = nIteration,
                                                   lengthIter = lengthIter,
                                                   only0 = TRUE)
              level.0.low <- readStateDLMFromFile(skeleton = skeleton.level.low,
                                                  filename = filename,
                                                  iterations = NULL,
                                                  nIteration = nIteration,
                                                  lengthIter = lengthIter,
                                                  only0 = TRUE)
              ## calculate adjustments for levels
              names.high.only <- setdiff(names.high, names.low)
              means.shared.level <- collapseDimension(level.0.high,
                                                      dimension = names.high.only,
                                                      weights = 1,
                                                      na.rm = TRUE)
              ## rescale betas and record them
              rescaleAndWriteBetas(high = beta.high,
                                   low = beta.low,
                                   adj = means.shared.level,
                                   skeletonHigh = skeletonBetaHigh,
                                   skeletonLow = skeletonBetaLow,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              ## adjust level and record
              level.high <- level.high - means.shared.level
              level.low <- level.low + means.shared.level
              overwriteValuesOnFile(object = level.high,
                                    skeleton = skeleton.level.high,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              overwriteValuesOnFile(object = level.low,
                                    skeleton = skeleton.level.low,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              NULL
          })


## rescalePred ########################################################################

setMethod("rescalePred",
          signature(prior = "Exchangeable"),
          function(prior, skeleton, adjustment,
                   filename, nIteration, lengthIter) {
              beta <- fetchResults(object = skeleton,
                                   filename = filename,
                                   iterations = NULL,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              beta <- beta + adjustment
              overwriteValuesOnFile(object = beta,
                                    skeleton = skeleton,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
          })



## rescalePriorIntercept ##############################################################

## HAS_TESTS
setMethod("rescalePriorIntercept",
          signature(priorTerm = "Exchangeable"),
          function(priorTerm, priorIntercept, skeletonBetaTerm,
                   skeletonBetaIntercept, skeletonsPriorTerm,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              metadata.term <- skeletonBetaTerm@metadata
              has.covariates <- (methods::.hasSlot(priorTerm, "hasCovariates")
                  && priorTerm@hasCovariates@.Data)
              names.term <- names(metadata.term)
              name.intercept <- "(Intercept)"
              beta.term <- fetchResults(object = skeletonBetaTerm,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              beta.intercept <- fetchResults(object = skeletonBetaIntercept,
                                             filename = filename,
                                             iterations = NULL,
                                             nIteration = nIteration,
                                             lengthIter = lengthIter)
              if (has.covariates) {
                  skeleton.covariates <- skeletonsPriorTerm$coef
                  adj <- readCoefInterceptFromFile(skeleton = skeleton.covariates,
                                                   filename = filename,
                                                   nIteration = nIteration,
                                                   lengthIter = lengthIter)
                  setCoefInterceptToZeroOnFile(skeleton = skeleton.covariates,
                                               filename = filename,
                                               nIteration = nIteration,
                                               lengthIter = lengthIter)
              }
              else {
                  adj <- mean(beta.term, na.rm = TRUE)
              }
              rescaleAndWriteBetas(high = beta.term,
                                   low = beta.intercept,
                                   adj = adj,
                                   skeletonHigh = skeletonBetaTerm,
                                   skeletonLow = skeletonBetaIntercept,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              recordAdjustments(priorHigh = priorTerm,
                                priorLow = priorIntercept,
                                namesHigh = names.term,
                                namesLow = name.intercept,
                                adj = adj,
                                adjustments = adjustments,
                                prefixAdjustments = prefixAdjustments)
              NULL
          })

## HAS_TESTS
setMethod("rescalePriorIntercept",
          signature(priorTerm = "DLM"),
          function(priorTerm, priorIntercept, skeletonBetaTerm,
                   skeletonBetaIntercept, skeletonsPriorTerm,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              phi.term <- priorTerm@phi
              phi.known.term <- priorTerm@phiKnown@.Data
              metadata.term <- skeletonBetaTerm@metadata
              has.covariates <- (methods::.hasSlot(priorTerm, "hasCovariates")
                  && priorTerm@hasCovariates@.Data)
              skeleton.level.term <- skeletonsPriorTerm$level
              has.trend.term <- methods::is(priorTerm, "WithTrendMixin")
              non.stationary <- has.trend.term || (phi.known.term && isTRUE(all.equal(phi.term, 1)))
              name.intercept <- "(Intercept)"
              if (non.stationary) {
                  names.term <- names(metadata.term)
                  beta.term <- fetchResults(object = skeletonBetaTerm,
                                            filename = filename,
                                            iterations = NULL,
                                            nIteration = nIteration,
                                            lengthIter = lengthIter)
                  beta.intercept <- fetchResults(object = skeletonBetaIntercept,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration,
                                                 lengthIter = lengthIter)
                  level.term <- readStateDLMFromFile(skeleton = skeleton.level.term,
                                                     filename = filename,
                                                     iterations = NULL,
                                                     nIteration = nIteration,
                                                     lengthIter = lengthIter,
                                                     only0 = FALSE)
                  level.0.term <- readStateDLMFromFile(skeleton = skeleton.level.term,
                                                       filename = filename,
                                                       iterations = NULL,
                                                       nIteration = nIteration,
                                                       lengthIter = lengthIter,
                                                       only0 = TRUE)
                  mean.level.0 <- mean(level.0.term, na.rm = TRUE)
                  rescaleAndWriteBetas(high = beta.term,
                                       low = beta.intercept,
                                       adj = mean.level.0,
                                       skeletonHigh = skeletonBetaTerm,
                                       skeletonLow = skeletonBetaIntercept,
                                       filename = filename,
                                       nIteration = nIteration,
                                       lengthIter = lengthIter)
                  level.term <- level.term - mean.level.0
                  overwriteValuesOnFile(object = level.term,
                                        skeleton = skeleton.level.term,
                                        filename = filename,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
                  recordAdjustments(priorHigh = priorTerm,
                                    priorLow = priorIntercept,
                                    namesHigh = names.term,
                                    namesLow = name.intercept,
                                    adj = mean.level.0,
                                    adjustments = adjustments,
                                    prefixAdjustments = prefixAdjustments)
              }
              if (has.covariates) {
                  beta.term <- fetchResults(object = skeletonBetaTerm,
                                            filename = filename,
                                            iterations = NULL,
                                            nIteration = nIteration,
                                            lengthIter = lengthIter)
                  beta.intercept <- fetchResults(object = skeletonBetaIntercept,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration,
                                                 lengthIter = lengthIter)
                  level.term <- readStateDLMFromFile(skeleton = skeleton.level.term,
                                                     filename = filename,
                                                     iterations = NULL,
                                                     nIteration = nIteration,
                                                     lengthIter = lengthIter,
                                                     only0 = FALSE)
                  skeleton.covariates <- skeletonsPriorTerm$coef
                  coef.intercept <- readCoefInterceptFromFile(skeleton = skeleton.covariates,
                                                              filename = filename,
                                                              nIteration = nIteration,
                                                              lengthIter = lengthIter)
                  rescaleAndWriteBetas(high = beta.term,
                                       low = beta.intercept,
                                       adj = coef.intercept,
                                       skeletonHigh = skeletonBetaTerm,
                                       skeletonLow = skeletonBetaIntercept,
                                       filename = filename,
                                       nIteration = nIteration,
                                       lengthIter = lengthIter)
                  level.term <- level.term - coef.intercept
                  overwriteValuesOnFile(object = level.term,
                                        skeleton = skeleton.level.term,
                                        filename = filename,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
                  recordAdjustments(priorHigh = priorTerm,
                                    priorLow = priorIntercept,
                                    namesHigh = names.term,
                                    namesLow = name.intercept,
                                    adj = coef.intercept,
                                    adjustments = adjustments,
                                    prefixAdjustments = prefixAdjustments)
                  setCoefInterceptToZeroOnFile(skeleton = skeleton.covariates,
                                               filename = filename,
                                               nIteration = nIteration,
                                               lengthIter = lengthIter)
              }
              NULL
          })




## rescaleSeason ######################################################################

## HAS_TESTS
setMethod("rescaleSeason",
          signature(prior = "SeasonMixin"),
          function(prior, skeleton, filename, nIteration, lengthIter) {
              i.along <- prior@iAlong
              skeleton.season <- skeleton$season
              skeleton.level <- skeleton$level
              season <- readStateDLMFromFile(skeleton = skeleton.season,
                                             filename = filename,
                                             iterations = NULL,
                                             nIteration = nIteration,
                                             lengthIter = lengthIter,
                                             only0 = FALSE)
              season.0 <- readStateDLMFromFile(skeleton = skeleton.season,
                                               filename = filename,
                                               iterations = NULL,
                                               nIteration = nIteration,
                                               lengthIter = lengthIter,
                                               only0 = TRUE)
              level <- readStateDLMFromFile(skeleton = skeleton.level,
                                            filename = filename,
                                            iterations = NULL,
                                            nIteration = nIteration,
                                            lengthIter = lengthIter,
                                            only0 = FALSE)
              means <- collapseDimension(season.0,
                                         dimension = 1L,
                                         weights = 1,
                                         na.rm = TRUE)
              season <- season - means
              level <- level + means
              overwriteValuesOnFile(object = season,
                                    skeleton = skeleton.season,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              overwriteValuesOnFile(object = level,
                                    skeleton = skeleton.level,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              NULL
          })




## transferParamPrior #################################################################

## Exch

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchNormZero"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(identical(length(values), 1L))
              stopifnot(!is.na(values))
              stopifnot(values > 0)
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchNormZero_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  prior@tau@.Data <- values
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchNormCov"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(identical(length(values), prior@P@.Data + 1L))
              stopifnot(!any(is.na(values)))
              stopifnot(values[length(values)] > 0)
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchNormCov_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  P <- prior@P@.Data
                  offset <- 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchRobustZero"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              stopifnot(all(values > 0))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchRobustZero_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  offset <- 1L
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "ExchRobustCov"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              stopifnot(all(values > 0))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_ExchRobustCov_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  P <- prior@P@.Data
                  offset <- 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })


## DLM - Norm, Zero

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMNoTrendNormZeroNoSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMNoTrendNormZeroNoSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  alpha <- prior@alphaDLM@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMWithTrendNormZeroNoSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMWithTrendNormZeroNoSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  alpha <- prior@alphaDLM@.Data
                  delta <- prior@deltaDLM@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@deltaDLM@.Data <- transferAlphaDelta0(state = delta,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaDelta@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMNoTrendNormZeroWithSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMNoTrendNormZeroWithSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  alpha <- prior@alphaDLM@.Data
                  s <- prior@s@.Data
                  n.season <- prior@nSeason@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@s@.Data <- transferSeason0(s = s,
                                                   nSeason = n.season,
                                                   values = values,
                                                   offset = offset,
                                                   iteratorNew = iterator.new,
                                                   iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L * n.season
                  prior@omegaSeason@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMWithTrendNormZeroWithSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMWithTrendNormZeroWithSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  alpha <- prior@alphaDLM@.Data
                  delta <- prior@deltaDLM@.Data
                  s <- prior@s@.Data
                  n.season <- prior@nSeason@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@deltaDLM@.Data <- transferAlphaDelta0(state = delta,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaDelta@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@s@.Data <- transferSeason0(s = s,
                                                   nSeason = n.season,
                                                   values = values,
                                                   offset = offset,
                                                   iteratorNew = iterator.new,
                                                   iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L * n.season
                  prior@omegaSeason@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })


## DLM - Norm, Cov

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMNoTrendNormCovNoSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMNoTrendNormCovNoSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  P <- prior@P@.Data
                  alpha <- prior@alphaDLM@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMWithTrendNormCovNoSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMWithTrendNormCovNoSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  P <- prior@P@.Data
                  alpha <- prior@alphaDLM@.Data
                  delta <- prior@deltaDLM@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@deltaDLM@.Data <- transferAlphaDelta0(state = delta,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaDelta@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMNoTrendNormCovWithSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMNoTrendNormCovWithSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  P <- prior@P@.Data
                  alpha <- prior@alphaDLM@.Data
                  s <- prior@s@.Data
                  n.season <- prior@nSeason@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@s@.Data <- transferSeason0(s = s,
                                                   nSeason = n.season,
                                                   values = values,
                                                   offset = offset,
                                                   iteratorNew = iterator.new,
                                                   iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L * n.season
                  prior@omegaSeason@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMWithTrendNormCovWithSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMWithTrendNormCovWithSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  P <- prior@P@.Data
                  alpha <- prior@alphaDLM@.Data
                  delta <- prior@deltaDLM@.Data
                  s <- prior@s@.Data
                  n.season <- prior@nSeason@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@deltaDLM@.Data <- transferAlphaDelta0(state = delta,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaDelta@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@s@.Data <- transferSeason0(s = s,
                                                   nSeason = n.season,
                                                   values = values,
                                                   offset = offset,
                                                   iteratorNew = iterator.new,
                                                   iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L * n.season
                  prior@omegaSeason@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })


## Robust, Zero

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMNoTrendRobustZeroNoSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMNoTrendRobustZeroNoSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  alpha <- prior@alphaDLM@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMWithTrendRobustZeroNoSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMWithTrendRobustZeroNoSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  alpha <- prior@alphaDLM@.Data
                  delta <- prior@deltaDLM@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@deltaDLM@.Data <- transferAlphaDelta0(state = delta,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaDelta@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMNoTrendRobustZeroWithSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMNoTrendRobustZeroWithSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  alpha <- prior@alphaDLM@.Data
                  s <- prior@s@.Data
                  n.season <- prior@nSeason@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@s@.Data <- transferSeason0(s = s,
                                                   nSeason = n.season,
                                                   values = values,
                                                   offset = offset,
                                                   iteratorNew = iterator.new,
                                                   iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L * n.season
                  prior@omegaSeason@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMWithTrendRobustZeroWithSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMWithTrendRobustZeroWithSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  alpha <- prior@alphaDLM@.Data
                  delta <- prior@deltaDLM@.Data
                  s <- prior@s@.Data
                  n.season <- prior@nSeason@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@deltaDLM@.Data <- transferAlphaDelta0(state = delta,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaDelta@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@s@.Data <- transferSeason0(s = s,
                                                   nSeason = n.season,
                                                   values = values,
                                                   offset = offset,
                                                   iteratorNew = iterator.new,
                                                   iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L * n.season
                  prior@omegaSeason@.Data <- values[offset]
                  offset <- offset + 1L 
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })


## DLM - Robust, Cov

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMNoTrendRobustCovNoSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMNoTrendRobustCovNoSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  P <- prior@P@.Data
                  alpha <- prior@alphaDLM@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMWithTrendRobustCovNoSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMWithTrendRobustCovNoSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  P <- prior@P@.Data
                  alpha <- prior@alphaDLM@.Data
                  delta <- prior@deltaDLM@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@deltaDLM@.Data <- transferAlphaDelta0(state = delta,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaDelta@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMNoTrendRobustCovWithSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMNoTrendRobustCovWithSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  P <- prior@P@.Data
                  alpha <- prior@alphaDLM@.Data
                  s <- prior@s@.Data
                  n.season <- prior@nSeason@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@s@.Data <- transferSeason0(s = s,
                                                   nSeason = n.season,
                                                   values = values,
                                                   offset = offset,
                                                   iteratorNew = iterator.new,
                                                   iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L * n.season
                  prior@omegaSeason@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("transferParamPrior",
          signature(prior = "DLMWithTrendRobustCovWithSeasonPredict"),
          function(prior, values, useC = FALSE, useSpecific = FALSE) {
              ## prior
              methods::validObject(prior)
              ## values
              stopifnot(is.double(values))
              stopifnot(!any(is.na(values)))
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamPrior_DLMWithTrendRobustCovWithSeasonPredict_R, prior, values)
                  else
                      .Call(transferParamPrior_R, prior, values)
              }
              else {
                  J.new <- prior@J@.Data
                  J.old <- prior@JOld@.Data
                  L <- prior@L@.Data
                  P <- prior@P@.Data
                  alpha <- prior@alphaDLM@.Data
                  delta <- prior@deltaDLM@.Data
                  s <- prior@s@.Data
                  n.season <- prior@nSeason@.Data
                  iterator.new <- prior@iteratorState
                  iterator.old <- prior@iteratorStateOld
                  K.old <- J.old %/% L
                  offset <- 1L
                  prior@alphaDLM@.Data <- transferAlphaDelta0(state = alpha,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaAlpha@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@deltaDLM@.Data <- transferAlphaDelta0(state = delta,
                                                              values = values,
                                                              offset = offset,
                                                              iteratorNew = iterator.new,
                                                              iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L
                  prior@omegaDelta@.Data <- values[offset]
                  offset <- offset + 1L
                  prior@phi <- values[offset]
                  offset <- offset + 1L
                  prior@s@.Data <- transferSeason0(s = s,
                                                   nSeason = n.season,
                                                   values = values,
                                                   offset = offset,
                                                   iteratorNew = iterator.new,
                                                   iteratorOld = iterator.old)
                  offset <- offset + (K.old + 1L) * L * n.season
                  prior@omegaSeason@.Data <- values[offset]
                  offset <- offset + 1
                  prior@eta@.Data <- values[offset : (offset + P - 1L)]
                  offset <- offset + P
                  prior@tau@.Data <- values[offset]
                  prior
              }
          })


## Mix

## TRANSLATED
## HAS_TESTS
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
                  dim.beta.old <- prior@dimBetaOld
                  iAlong <- prior@iAlong
                  index.class.max <- prior@indexClassMaxMix@.Data
                  n.beta.no.along <- prior@nBetaNoAlongMix@.Data
                  n.along.old <- dim.beta.old[iAlong]
                  offset <- 1L
                  ## prodVectorsMix
                  n.prod <- n.beta.no.along * index.class.max
                  prior@prodVectorsMix@.Data <- values[offset : (offset + n.prod - 1L)]
                  offset <- offset + n.prod
                  ## omegaVectorsMix
                  prior@omegaVectorsMix@.Data <- values[offset]
                  offset <- offset + 1L
                  ## weightMix
                  offset <- offset + n.along.old * index.class.max
                  ## componentWeightMix
                  offset <- offset + n.along.old * index.class.max
                  ## omegaComponentWeightMix
                  prior@omegaComponentWeightMix@.Data <- values[offset]
                  offset <- offset + 1L
                  ## levelComponentWeightOldMix (final values of levelComponetWeightMix)
                  prior@levelComponentWeightOldMix@.Data <-
                      transferLevelComponentWeightOldMix(values = values,
                                                         offset = offset,
                                                         nAlongOld = n.along.old,
                                                         indexClassMax = index.class.max)
                  offset <- offset + n.along.old * index.class.max
                  ## meanLevelComponentWeightMix
                  prior@meanLevelComponentWeightMix@.Data <- values[offset]
                  offset <- offset + 1L
                  ## phiMix
                  prior@phiMix <- values[offset]
                  offset <- offset + 1L
                  ## omegaLevelComponentWeightMix
                  prior@omegaLevelComponentWeightMix@.Data <- values[offset]
                  offset <- offset + 1L
                  ## tau
                  prior@tau@.Data <- values[offset]
                  ## return
                  prior
              }
          })



                  
## whereEstimated #####################################################################

## TimeInvariant

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "TimeInvariant"),
          function(object) {
              character()
          })


## ExchFixed

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "ExchFixed"),
          function(object) {
              character()
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "ExchNormZero"),
          function(object) {
              "scaleError"
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "ExchNormCov"),
          function(object) {
              c("coef", "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "ExchRobustZero"),
          function(object) {
              "scaleError"
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "ExchRobustCov"),
          function(object) {
              c("coef", "scaleError")
          })


## DLM - Norm, Zero

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendNormZeroNoSeason"),
          function(object) {
              c("scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendNormZeroNoSeason"),
          function(object) {
              c(if (object@hasLevel@.Data) "scaleLevel" else NULL,
                "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendNormZeroWithSeason"),
          function(object) {
              c("scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "scaleSeason",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendNormZeroWithSeason"),
          function(object) {
              c(if (object@hasLevel@.Data) "scaleLevel" else NULL,
                "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "scaleSeason",
                "scaleError")
          })


## DLM - Norm, Cov

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendNormCovNoSeason"),
          function(object) {
              c("scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendNormCovNoSeason"),
          function(object) {
              c(if (object@hasLevel@.Data) "scaleLevel" else NULL,
                "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendNormCovWithSeason"),
          function(object) {
              c("scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "scaleSeason",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendNormCovWithSeason"),
          function(object) {
              c(if (object@hasLevel@.Data) "scaleLevel" else NULL,
                "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "scaleSeason",
                "coef",
                "scaleError")
          })


## DLM - Robust, Zero

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendRobustZeroNoSeason"),
          function(object) {
              c("scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendRobustZeroNoSeason"),
          function(object) {
              c(if (object@hasLevel@.Data) "scaleLevel" else NULL,
                "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendRobustZeroWithSeason"),
          function(object) {
              c("scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "scaleSeason",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendRobustZeroWithSeason"),
          function(object) {
              c(if (object@hasLevel@.Data) "scaleLevel" else NULL,
                "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "scaleSeason",
                "scaleError")
          })


## DLM - Robust, Cov

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendRobustCovNoSeason"),
          function(object) {
              c("scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendRobustCovNoSeason"),
          function(object) {
              c(if (object@hasLevel@.Data) "scaleLevel" else NULL,
                "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendRobustCovWithSeason"),
          function(object) {
              c("scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "scaleSeason",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendRobustCovWithSeason"),
          function(object) {
              c(if (object@hasLevel@.Data) "scaleLevel" else NULL,
                "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "scaleSeason",
                "coef",
                "scaleError")
          })


## Known

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "KnownCertain"),
          function(object) {
              NULL
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "KnownUncertain"),
          function(object) {
              NULL
          })

## Mix

setMethod("whereEstimated",
          signature(object = "MixNormZero"),
          function(object) {
              c("scaleComponents",
                "scale1",
                "mean",
                "damp",
                "scale2",
                "scaleError")
          })

## Zero

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "Zero"),
          function(object) {
              NULL
          })
