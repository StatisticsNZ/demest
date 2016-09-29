              
## makeOutputPrior ###################################################################

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
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = 1L,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = 0L)
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
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = 1L,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = 0L)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputTrendDLM(iterator = iterator,
                                          metadata = metadata,
                                          pos = pos)
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
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              nSeason <- prior@nSeason@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              firstSeason <- pos + (K + 1L) * L + 1L + 1L
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = nSeason,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = firstSeason)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputSeasonDLM(iterator = iterator,
                                            metadata = metadata,
                                            nSeason = nSeason,
                                            iAlong = iAlong,
                                            pos = pos)
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
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              nSeason <- prior@nSeason@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              firstSeason <- pos + (K + 1L) * L + 1L + (K + 1L) * L + 1L + 1L
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = nSeason,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = firstSeason)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputTrendDLM(iterator = iterator,
                                          metadata = metadata,
                                          pos = pos)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputSeasonDLM(iterator = iterator,
                                            metadata = metadata,
                                            nSeason = nSeason,
                                            iAlong = iAlong,
                                            pos = pos)
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
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = 1L,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = 0L)
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
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = 1L,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = 0L)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputTrendDLM(iterator = iterator,
                                          metadata = metadata,
                                          pos = pos)
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
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              firstSeason <- pos + (K + 1L) * L + 1L + 1L
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = nSeason,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = firstSeason)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputSeasonDLM(iterator = iterator,
                                            metadata = metadata,
                                            nSeason = nSeason,
                                            iAlong = iAlong,
                                            pos = pos)
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
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              firstSeason <- pos + (K + 1L) * L + 1L + (K + 1L) * L + 1L + 1L
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = nSeason,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = firstSeason)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputTrendDLM(iterator = iterator,
                                          metadata = metadata, 
                                          pos = pos)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputSeasonDLM(iterator = iterator,
                                            metadata = metadata,
                                            nSeason = nSeason,
                                            iAlong = iAlong,
                                            pos = pos)
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
          function(prior, metadata, pos) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = 1L,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = 0L)
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
          function(prior, metadata, pos) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = 1L,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = 0L)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputTrendDLM(iterator = iterator,
                                          metadata = metadata,
                                          pos = pos)
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
          function(prior, metadata, pos) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              firstSeason <- pos + (K + 1L) * L + 1L + 1L
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = nSeason,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = firstSeason)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputSeasonDLM(iterator = iterator,
                                            metadata = metadata,
                                            nSeason = nSeason,
                                            iAlong = iAlong,
                                            pos = pos)
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
          function(prior, metadata, pos) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              firstSeason <- pos + (K + 1L) * L + 1L + (K + 1L) * L + 1L + 1L
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = nSeason,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = firstSeason)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputTrendDLM(iterator = iterator,
                                          metadata = metadata,
                                          pos = pos)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputSeasonDLM(iterator = iterator,
                                            metadata = metadata,
                                            nSeason = nSeason,
                                            iAlong = iAlong,
                                            pos = pos)
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
          function(prior, metadata, pos) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = 1L,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = 0L)
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
          function(prior, metadata, pos) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = 1L,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = 0L)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputTrendDLM(iterator = iterator,
                                          metadata = metadata,
                                          pos = pos)
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
          function(prior, metadata, pos) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              iAlong <- prior@iAlong
              nSeason <- prior@nSeason@.Data
              iterator <- prior@iteratorState
              firstSeason <- pos + (K + 1L) * L + 1L + 1L
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = nSeason,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = firstSeason)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputSeasonDLM(iterator = iterator,
                                         metadata = metadata,
                                            nSeason = nSeason,
                                            iAlong = iAlong,
                                            pos = pos)
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
          function(prior, metadata, pos) {
              K <- prior@K@.Data
              L <- prior@L@.Data
              P <- prior@P@.Data
              Z <- prior@Z
              nSeason <- prior@nSeason@.Data
              iAlong <- prior@iAlong
              iterator <- prior@iteratorState
              firstSeason <- pos + (K + 1L) * L + 1L + (K + 1L) * L + 1L + 1L
              level <- makeOutputLevelDLM(iterator = iterator,
                                          metadata = metadata,
                                          nSeason = nSeason,
                                          iAlong = iAlong,
                                          pos = pos,
                                          firstSeason = firstSeason)
              pos <- pos + (K + 1L) * L
              scaleLevel <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              trend <- makeOutputTrendDLM(iterator = iterator,
                                          metadata = metadata,
                                          pos = pos)
              pos <- pos + (K + 1L) * L
              scaleTrend <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              damp <- makeOutputPriorDamp(pos = pos)
              pos <- pos + 1L
              season <- makeOutputSeasonDLM(iterator = iterator,
                                            metadata = metadata,
                                            nSeason = nSeason,
                                            iAlong = iAlong,
                                            pos = pos)
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


## predictPrior ###########################################################

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
          signature(object = "TimeInvariant"),
          function(object, name = NULL, order = 1L) {
              cat(gettextf("-- values for '%s' held constant --\n",
                           name))
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
                      .Call(transferParamPrior_ExchRobustCov_R, prior, values) ##changed line from ExchRobustZero JAH 23/5 
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
              c("level", "scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendNormZeroNoSeason"),
          function(object) {
              c("level", "scaleLevel",
                "trend", "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendNormZeroWithSeason"),
          function(object) {
              c("level", "scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "season", "scaleSeason",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendNormZeroWithSeason"),
          function(object) {
              c("level", "scaleLevel",
                "trend", "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "season", "scaleSeason",
                "scaleError")
          })


## DLM - Norm, Cov

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendNormCovNoSeason"),
          function(object) {
              c("level", "scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendNormCovNoSeason"),
          function(object) {
              c("level", "scaleLevel",
                "trend", "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendNormCovWithSeason"),
          function(object) {
              c("level", "scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "season", "scaleSeason",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendNormCovWithSeason"),
          function(object) {
              c("level", "scaleLevel",
                "trend", "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "season", "scaleSeason",
                "coef",
                "scaleError")
          })


## DLM - Robust, Zero

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendRobustZeroNoSeason"),
          function(object) {
              c("level", "scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendRobustZeroNoSeason"),
          function(object) {
              c("level", "scaleLevel",
                "trend", "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendRobustZeroWithSeason"),
          function(object) {
              c("level", "scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "season", "scaleSeason",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendRobustZeroWithSeason"),
          function(object) {
              c("level", "scaleLevel",
                "trend", "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "season", "scaleSeason",
                "scaleError")
          })


## DLM - Robust, Cov

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendRobustCovNoSeason"),
          function(object) {
              c("level", "scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendRobustCovNoSeason"),
          function(object) {
              c("level", "scaleLevel",
                "trend", "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMNoTrendRobustCovWithSeason"),
          function(object) {
              c("level", "scaleLevel",
                if (object@phiKnown) NULL else "damp",
                "season", "scaleSeason",
                "coef",
                "scaleError")
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "DLMWithTrendRobustCovWithSeason"),
          function(object) {
              c("level", "scaleLevel",
                "trend", "scaleTrend",
                if (object@phiKnown) NULL else "damp",
                "season", "scaleSeason",
                "coef",
                "scaleError")
          })


