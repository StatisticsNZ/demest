
## fakeBeta ###################################################################

## HAS_TESTS
## default method
setMethod("fakeBeta",
          signature(object = "FakePrior"),
          function(object) {
              is.saturated <- object@isSaturated@.Data
              beta.hat <- betaHat(object)
              if (is.saturated)
                  beta.hat
              else {
                  J <- object@J@.Data
                  tau <- object@tau@.Data
                  stats::rnorm(n = J,
                               mean = beta.hat,
                               sd = tau)
              }
          })

## HAS_TESTS
setMethod("fakeBeta",
          signature(object = "FakeExchFixed"),
          function(object) {
              J <- object@J@.Data
              mean <- object@mean@.Data
              tau <- object@tau@.Data
              stats::rnorm(n = J,
                           mean = mean,
                           sd = tau)
          })



## makeFakeOutputPrior ###################################################################

## HAS_TESTS
setMethod("makeFakeOutputPrior",
          signature(prior = "FakeExchFixed",
                    metadata = "ANY"),
          function(prior, metadata) {
              mean <- prior@mean@.Data
              sd <- prior@tau@.Data
              list(mean = mean,
                   sd = sd)
          })

## HAS_TESTS
setMethod("makeFakeOutputPrior",
          signature(prior = "FakeExchNormZero",
                    metadata = "MetaData"),
          function(prior, metadata) {
              scaleError <- prior@tau@.Data
              list(scaleError = scaleError)
          })

## HAS_TESTS
setMethod("makeFakeOutputPrior",
          signature(prior = "FakeDLMNoTrendNormZeroNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata) {
              omegaAlpha <- prior@omegaAlpha@.Data
              phi <- prior@phi
              tau <- prior@tau@.Data
              level <- makeFakeOutputLevelDLM(prior = prior,
                                              metadata = metadata)
              list(level = level,
                   scaleLevel = omegaAlpha,
                   damp = phi,
                   scaleError = tau)
          })

## HAS_TESTS
setMethod("makeFakeOutputPrior",
          signature(prior = "FakeDLMWithTrendNormZeroNoSeason",
                    metadata = "MetaData"),
          function(prior, metadata) {
              omegaAlpha <- prior@omegaAlpha@.Data
              omegaDelta <- prior@omegaDelta@.Data
              phi <- prior@phi
              tau <- prior@tau@.Data
              level <- makeFakeOutputLevelDLM(prior = prior,
                                              metadata = metadata)
              trend <- makeFakeOutputTrendDLM(prior = prior,
                                              metadata = metadata)
              list(level = level,
                   scaleLevel = omegaAlpha,
                   trend = trend,
                   scaleTrend = omegaDelta,
                   damp = phi,
                   scaleError = tau)
          })




