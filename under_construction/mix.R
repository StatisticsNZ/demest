




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

psi - component
sigma_e - scaleComponent
v - weight
W - level1AR
sigma_epsilon - scale1AR
alpha - level2AR
sigma_eta - scale2AR
mu - meanAR
phi - coefAR

sigma_delta - scaleError


setMethod("makeOutputPrior",
          signature(prior = "MixNormZero",
                    metadata = "MetaData"),
          function(prior, metadata, pos) {
              J <- prior@J@.Data
              J.old <- prior@JOld@.Data
              iAlong <- prior@iAlong
              n.beta.no.along <- prior@nBetaNoAlongMix
              index.class.max <- prior@indexClassMaxMix@.Data
              metadata.vectors <- makeMetadataVectorsMix(metadata = metadata,
                                                         iAlong = iAlong,
                                                         indexClassMax = indexClassMax)
              metadata.weights <- makeMetadataWeightsMix(metadata = metadata,
                                                         iAlong = iAlong,
                                                         indexClassMax = indexClassMax)
              ## skip 'alphaMix'
              pos <- pos + J.old
              ## prodVectorsMix
              components <- Skeleton(metadata = metadata,
                                     first = pos)
              pos <- pos + n.beta.no.along * index.class.max
              ## omegaVectorsMix
              scaleComponents <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              ## weightsMix
              weights <- Skeleton(metadata = metadata.weights,
                                  first = pos)
              pos <- pos + n.along * index.class.max
              ## componentWeightMix
              level1AR <- Skeleton(metadata = metadata.weights,
                                   first = pos)
              pos <- pos + n.along * index.class.max
              ## omegaComponentWeightMix
              scale1AR <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              ## levelComponentWeightMix
              level2AR <- Skeleton(metadata = metadata.weights,
                                   first = pos)
              pos <- pos + n.along * index.class.max
              ## meanLevelComponentWeightMix
              meanAR <- Skeleton(pos = pos)
              pos <- pos + 1L
              ## phiMix
              coefAR <- Skeleton(pos = pos)
              pos <- pos + 1L
              ## omegaLevelComponentWeightMix
              scale2AR <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              ## tau
              scaleError <- makeOutputPriorScale(pos = pos)
              pos <- pos + 1L
              
              
              
              
              
              

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
