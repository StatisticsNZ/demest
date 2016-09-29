

## predictCombined #################################################################

## the 'nUpdate' argument may disappear in the long run,
## but leaving it in for the moment

## TRANSLATED
## HAS_TESTS
setMethod("predictCombined",
          signature(object = "CombinedModelNormal"),
          function(object, nUpdate = 1L, filename, lengthIter, iteration, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictCombined_CombinedModelNormal_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- predictModelNotUseExp(model, y = y)
                  object@model <- model
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictCombined",
          signature(object = "CombinedModelPoissonNotHasExp"),
          function(object, nUpdate = 1L, filename, lengthIter, iteration, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictCombined_CombinedModelPoissonNotHasExp_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- predictModelNotUseExp(model, y = y)
                  object@model <- model
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictCombined",
          signature(object = "CombinedModelBinomial"),
          function(object, nUpdate = 1L, filename, lengthIter, iteration, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictCombined_CombinedModelBinomial_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- predictModelUseExp(model, y = y, exposure = exposure)
                  object@model <- model
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictCombined",
          signature(object = "CombinedModelPoissonHasExp"),
          function(object, nUpdate = 1L, filename, lengthIter, iteration, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictCombined_CombinedModelPoissonHasExp_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- predictModelUseExp(model, y = y, exposure = exposure)
                  object@model <- model
                  object
              }
          })




## updateCombined #####################################################################

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedModelNormal"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedModelNormal_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  model <- object@model
                  y <- object@y
                  for (i in seq_len(nUpdate))
                      model <- updateModelNotUseExp(model, y = y)
                  object@model <- model
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedModelPoissonNotHasExp"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedModelPoissonNotHasExp_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  model <- object@model
                  y <- object@y
                  for (i in seq_len(nUpdate))
                      model <- updateModelNotUseExp(model, y = y)
                  object@model <- model
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedModelBinomial"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedModelBinomial_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  for (i in seq_len(nUpdate))
                      model <- updateModelUseExp(model, y = y, exposure = exposure)
                  object@model <- model
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedModelPoissonHasExp"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedModelPoissonHasExp_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  for (i in seq_len(nUpdate))
                      model <- updateModelUseExp(model, y = y, exposure = exposure)
                  object@model <- model
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedCountsPoissonNotHasExp"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedCountsPoissonNotHasExp_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  y <- object@y
                  model <- object@model
                  observation <- object@observation
                  datasets <- object@datasets
                  transforms <- object@transforms
                  for (i in seq_len(nUpdate)) {
                      y <- updateCountsPoissonNotUseExp(y = y,
                                                        model = model,
                                                        observation = observation,
                                                        datasets = datasets,
                                                        transforms = transforms)
                      model <- updateModelNotUseExp(object = model,
                                                    y = y)
                      observation <- updateObservationCounts(observation = observation,
                                                             datasets = datasets,
                                                             transforms = transforms,
                                                             y = y)
                  }
                  object@y <- y
                  object@model <- model
                  object@observation <- observation
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedCountsPoissonHasExp"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedCountsPoissonHasExp_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  y <- object@y
                  model <- object@model
                  exposure <- object@exposure
                  observation <- object@observation
                  datasets <- object@datasets
                  transforms <- object@transforms
                  for (i in seq_len(nUpdate)) {
                      y <- updateCountsPoissonUseExp(y = y,
                                                     model = model,
                                                     exposure = exposure,
                                                     observation = observation,
                                                     datasets = datasets,
                                                     transforms = transforms)
                      model <- updateModelUseExp(object = model,
                                                 y = y,
                                                 exposure = exposure)
                      observation <- updateObservationCounts(observation = observation,
                                                             datasets = datasets,
                                                             transforms = transforms,
                                                             y = y)
                  }
                  object@y <- y
                  object@model <- model
                  object@observation <- observation
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedCountsBinomial"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedCountsBinomial_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  y <- object@y
                  model <- object@model
                  exposure <- object@exposure
                  observation <- object@observation
                  datasets <- object@datasets
                  transforms <- object@transforms
                  for (i in seq_len(nUpdate)) {
                      y <- updateCountsBinomial(y = y,
                                                model = model,
                                                exposure = exposure,
                                                observation = observation,
                                                datasets = datasets,
                                                transforms = transforms)
                      model <- updateModelUseExp(object = model,
                                                 y = y,
                                                 exposure = exposure)
                      observation <- updateObservationCounts(observation = observation,
                                                             datasets = datasets,
                                                             transforms = transforms,
                                                             y = y)
                  }
                  object@y <- y
                  object@model <- model
                  object@observation <- observation
                  object
              }
          })
