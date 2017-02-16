




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

v - weight
psi - component
sigma_delta - scaleError
W - 



mix
components

level
meanlev

scaleError


setMethod("makeOutputPrior",
          signature(prior = "MixNormZero",
                    metadata = "MetaData"),
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              iAlong <- prior@iAlong
              level <- Skeleton(metadata = metadata,
                                first = pos)
              
                                


              
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
