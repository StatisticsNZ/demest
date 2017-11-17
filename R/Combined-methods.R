

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
                  observationModels <- object@observationModels
                  datasets <- object@datasets
                  transforms <- object@transforms
                  for (i in seq_len(nUpdate)) {
                      y <- updateCountsPoissonNotUseExp(y = y,
                                                        model = model,
                                                        observationModels = observationModels,
                                                        datasets = datasets,
                                                        transforms = transforms)
                      model <- updateModelNotUseExp(object = model,
                                                    y = y)
                      observationModels <- updateObservationCounts(observationModels = observationModels,
                                                                   datasets = datasets,
                                                                   transforms = transforms,
                                                                   y = y)
                  }
                  object@y <- y
                  object@model <- model
                  object@observationModels <- observationModels
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
                  observationModels <- object@observationModels
                  datasets <- object@datasets
                  transforms <- object@transforms
                  for (i in seq_len(nUpdate)) {
                      y <- updateCountsPoissonUseExp(y = y,
                                                     model = model,
                                                     exposure = exposure,
                                                     observationModels = observationModels,
                                                     datasets = datasets,
                                                     transforms = transforms)
                      model <- updateModelUseExp(object = model,
                                                 y = y,
                                                 exposure = exposure)
                      observationModels <- updateObservationCounts(observationModels = observationModels,
                                                             datasets = datasets,
                                                             transforms = transforms,
                                                             y = y)
                  }
                  object@y <- y
                  object@model <- model
                  object@observationModels <- observationModels
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
                  observationModels <- object@observationModels
                  datasets <- object@datasets
                  transforms <- object@transforms
                  for (i in seq_len(nUpdate)) {
                      y <- updateCountsBinomial(y = y,
                                                model = model,
                                                exposure = exposure,
                                                observationModels = observationModels,
                                                datasets = datasets,
                                                transforms = transforms)
                      model <- updateModelUseExp(object = model,
                                                 y = y,
                                                 exposure = exposure)
                      observationModels <- updateObservationCounts(observationModels = observationModels,
                                                             datasets = datasets,
                                                             transforms = transforms,
                                                             y = y)
                  }
                  object@y <- y
                  object@model <- model
                  object@observationModels <- observationModels
                  object
              }
          })


## Accounts ##############################################################################

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("diffLogDensAccount",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(diffLogDensAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(diffLogDensAccount_R, combined)
              }
              else {
                  i.comp <- combined@iComp
                  i.orig.dest <- combined@iOrigDest
                  i.pool <- combined@iPool
                  i.int.net <- combined@iIntNet
                  model.uses.exposure <- combined@modelUsesExposure
                  is.popn <- i.comp == 0L
                  is.orig.dest <- i.comp == i.orig.dest
                  is.pool <- i.comp == i.pool
                  is.int.net <- i.comp == i.int.net
                  ans <- diffLogDensPopn(combined)
                  if (is.popn)
                      ans <- ans + diffLogDensExpPopn(combined)
                  else if (is.orig.dest) {
                      if (model.uses.exposure[i.comp])
                          ans <- ans + diffLogDensJumpOrigDest(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else if (is.pool) {
                      if (model.uses.exposure[i.comp])
                          ans <- ans + diffLogDensJumpPoolWithExpose(combined)
                      else
                          ans <- ans + diffLogDensJumpPoolNoExpose(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else if (is.int.net) {
                      ans <- ans + diffLogDensJumpNet(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else {
                      if (model.uses.exposure[i.comp])
                          ans <- ans + diffLogDensJumpComp(combined)
                      ans <- ans + diffLogDensExpComp(combined)
                  }
                  ans
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("diffLogLikAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(diffLogLikAccount_CombineAccountMovements_R, object)
                  else
                      .Call(diffLogLikAccount_R, object)
              }
              else {
                  i.comp <- object@iComp
                  i.orig.dest <- object@iOrigDest
                  i.pool <- object@iPool
                  i.int.net <- object@iIntNet
                  if (i.comp == 0L)
                      diffLogLikAccountMovePopn(object)
                  else if (i.comp == i.orig.dest)
                      diffLogLikAccountMoveOrigDest(object)
                  else if (i.comp == i.pool) 
                      diffLogLikAccountMovePool(object)
                  else if (i.comp == i.int.net) 
                      diffLogLikAccountMoveNet(object)
                  else
                      diffLogLikAccountMoveComp(object)
              }
          })


## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("updateProposalAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updateProposalAccount_CombinedAccountMovements_R, object)
                  else
                      .Call(updateProposalAccount_R, object)
              }
              else {
                  account <- object@account
                  prob.popn <- object@probPopn
                  update.popn <- runif(n = 1L) < prob.popn
                  if (update.popn) {
                      object@iComp <- 0L
                      updateProposalAccountMovePopn(object)
                  }
                  else {
                      cum.prob <- object@cumProbComp
                      i.births <- object@iBirths
                      i.orig.dest <- object@iOrigDest
                      i.pool <- object@iPool
                      i.int.net <- object@iIntNet
                      i.comp <- rcateg1(cum.prob)
                      object@iComp <- i.comp
                      if (i.comp == i.births)
                          updateProposalAccountMoveBirths(object)
                      else if (i.comp == i.orig.dest)
                          updateProposalAccountMoveOrigDest(object)
                      else if (i.comp == i.pool)
                          updateProposalAccountMovePool(object)
                      else if (i.comp == i.int.net)
                          updateProposalAccountMoveNet(object)
                      else
                          updateProposalAccountMoveComp(object)
                  }
              }
          })

## READY_T0_TRANSLATE
## HAS_TESTS
setMethod("updateValuesAccount",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updateValuesAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(updateValuesAccount_R, combined)
              }
              else {
                  has.age <- combined@hasAge
                  combined <- updateCellMove(combined)
                  combined <- updateSubsequentPopnMove(combined)
                  combined <- updateSubsequentExpMove(combined)
                  if (has.age)
                      combined <- updateSubsequentAccMove(combined)
                  combined
              }
          })
