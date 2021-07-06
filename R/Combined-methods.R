
## getSeriesForDataset #############################################################

## NO_TESTS
setMethod("getSeriesForDataset",
          signature(combined = "CombinedCounts"),
          function(combined, dataset, filename) {
              fetch(filename, where = "y")
          })

## NO_TESTS
setMethod("getSeriesForDataset",
          signature(combined = "CombinedAccount"),
          function(combined, dataset, filename) {
              names.datasets <- combined@namesDatasets
              series.indices <- combined@seriesIndices
              names.components <- combined@account@namesComponents
              i.dataset <- match(dataset, names.datasets)
              i.series <- series.indices[i.dataset]
              if (i.series == 0L)
                  series <- "population"
              else
                  series <- names.components[i.series]
              where <- c("account", series)
              fetch(filename, where = where)
          })


## drawCombined ####################################################################

## TRANSLATED
## HAS_TESTS
setMethod("drawCombined",
          signature(object = "CombinedModelBinomial"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(drawCombined_CombinedModelBinomial_R, object, nUpdate)
                  else
                      .Call(drawCombined_R, object, nUpdate)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  for (i in seq_len(nUpdate))
                      model <- drawModelUseExp(model,
                                               y = y,
                                               exposure = exposure)
                  object@model <- model
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawCombined",
          signature(object = "CombinedAccountMovements"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(drawCombined_CombinedAccountMovements_R, object, nUpdate)
                  else
                      .Call(drawCombined_R, object, nUpdate)
              }
              else {
                  object <- drawSystemModels(object)
                  object <- updateExpectedExposure(object, useC = TRUE)
                  object <- drawDataModels(object)
                  object
              }
          })


## drawDataModels ##################################################################

## Elements of 'datasets' must contain only NAs, when
## 'drawDataModels' is called. Normally this is done by
## calling function 'setDatasetsToMissing'.

## TRANSLATED
## HAS_TESTS
## Function is almost identical to 'updateDataModelsAccount'
setMethod("drawDataModels",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(combined)
              if (useC) {
                  if (useSpecific)
                      .Call(drawDataModels_CombinedAccountMovements_R, combined)
                  else
                      .Call(drawDataModels_R, combined)
              }
              else {
                  data.models <- combined@dataModels
                  datasets <- combined@datasets
                  population <- combined@account@population
                  components <- combined@account@components
                  series.indices <- combined@seriesIndices
                  transforms <- combined@transforms
                  for (i in seq_along(data.models)) {
                      model <- data.models[[i]]
                      dataset <- datasets[[i]]
                      transform <- transforms[[i]]
                      series.index <- series.indices[i]
                      if (any(!is.na(dataset)))
                          stop(gettextf("'%s' have not been set to missing",
                                        "datasets"))
                      if (series.index == 0L)
                          series <- population
                      else
                          series <- components[[series.index]]
                      series.collapsed <- collapse(series, transform = transform)
                      if (methods::is(model, "Poisson") || methods::is(model, "CMP"))
                          series.collapsed <- toDouble(series.collapsed)
                      model <- drawModelUseExp(model, ## this line different from 'updateDataModelsAccount'
                                               y = dataset,
                                               exposure = series.collapsed)
                      data.models[[i]] <- model
                  }
                  combined@dataModels <- data.models
                  combined
              }
          })


## drawSystemModels ################################################################

## Unlike with 'drawDataModels', 'drawSystemModels' does
## not assume that outcome variables (ie the demographic series)
## have been set to missing, since, unlike the datasets,
## the series are generated as part of the simulation process,
## rather than imputed afterwards.

## TRANSLATED
## HAS_TESTS
setMethod("drawSystemModels",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              methods::validObject(combined)
              if (useC) {
                  if (useSpecific)
                      .Call(drawSystemModels_CombinedAccountMovements_R, combined)
                  else
                      .Call(drawSystemModels_R, combined)
              }
              else {
                  system.models <- combined@systemModels
                  population <- combined@account@population
                  components <- combined@account@components
                  model.uses.exposure <- combined@modelUsesExposure
                  transforms.exp.to.comp <- combined@transformsExpToComp
                  transform.exp.to.births <- combined@transformExpToBirths
                  i.births <- combined@iBirths
                  ## population
                  population[] <- NA
                  model <- system.models[[1L]]
                  model <- drawModelNotUseExp(model,
                                              y = population)
                  system.models[[1L]] <- model
                  ## components
                  for (i in seq_along(components)) {
                      model <- system.models[[i + 1L]]
                      component <- components[[i]]
                      component[] <- NA
                      uses.exposure <- model.uses.exposure[i + 1L]
                      if (uses.exposure) {
                          exposure <- combined@exposure@.Data
                          is.births <- i == i.births
                          if (is.births)
                              exposure <- collapse(exposure,
                                                   transform = transform.exp.to.births)
                          transform <- transforms.exp.to.comp[[i]]
                          if (!is.null(transform))
                              exposure <- extend(exposure,
                                                 transform = transforms.exp.to.comp[[i]])
                          model <- drawModelUseExp(object = model,
                                                   y = component,
                                                   exposure = exposure)
                      }
                      else {
                          if (methods::is(model, "Normal"))
                              component <- toDouble(component)
                          model <- drawModelNotUseExp(object = model,
                                                      y = component)
                      }
                      system.models[[i + 1L]] <- model
                  }
                  combined@systemModels <- system.models
                  combined
              }
          })




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



setMethod("predictCombined",
          signature(object = "CombinedCountsBinomial"),
          function(object, nUpdate = 1L, filename,
                   lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictCombined_CombinedCountsBinomial_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  datasets <- object@datasets
                  data.models <- object@dataModels
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  ## Clear previous results. Need to clear for updateTheta to work properly.
                  y[] <- NA_integer_
                  model <- predictModelUseExp(model,
                                              y = y,
                                              exposure = exposure)
                  theta <- model@theta
                  y[] <- stats::rbinom(n = length(theta),
                                       size = exposure,
                                       prob = theta)
                  for (i in seq_along(data.models)) {
                      data.model <- data.models[[i]]
                      dataset <- datasets[[i]] ## all NA
                      expose <- dataset
                      if (methods::is(data.model, "Poisson"))
                          expose <- toDouble(expose)
                      data.model <- predictModelUseExp(object = data.model,
                                                       y = dataset,
                                                       exposure = expose)
                      data.models[[i]] <- data.model
                  }
                  object@model <- model
                  object@y <- y
                  object@dataModels <- data.models
                  object
              }
          })



## TRANSLATED
## HAS_TESTS
setMethod("predictCombined",
          signature(object = "CombinedCountsPoissonHasExp"),
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
                      .Call(predictCombined_CombinedCountsPoissonHasExp_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  datasets <- object@datasets
                  struc.zero.array <- model@strucZeroArray
                  data.models <- object@dataModels
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  ## Clear previous results. Need to clear for updateTheta to work properly.
                  y[] <- ifelse(struc.zero.array == 0L, 0L, NA_integer_)
                  model <- predictModelUseExp(model,
                                              y = y,
                                              exposure = exposure)
                  theta <- model@theta
                  lambda <- theta * exposure
                  y[] <- stats::rpois(n = length(theta),
                                      lambda = lambda) ## need to revisit this if we allow for subtotals
                  for (i in seq_along(data.models)) {
                      data.model <- data.models[[i]]
                      dataset <- datasets[[i]] ## all NA
                      expose <- dataset
                      if (methods::is(data.model, "Poisson"))
                          expose <- toDouble(expose)
                      data.model <- predictModelUseExp(object = data.model,
                                                       y = dataset,
                                                       exposure = expose)
                      data.models[[i]] <- data.model
                  }
                  object@model <- model
                  object@y <- y
                  object@dataModels <- data.models
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictCombined",
          signature(object = "CombinedCountsPoissonNotHasExp"),
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
                      .Call(predictCombined_CombinedCountsPoissonNotHasExp_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  datasets <- object@datasets
                  struc.zero.array <- model@strucZeroArray
                  data.models <- object@dataModels
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  ## Clear previous results. Need to clear for updateTheta to work properly.
                  y[] <- ifelse(struc.zero.array == 0L, 0L, NA_integer_)
                  model <- predictModelNotUseExp(model,
                                                 y = y)
                  theta <- model@theta
                  y[] <- stats::rpois(n = length(theta),
                                      lambda = theta) ## need to revisit this if we allow for subtotals
                  for (i in seq_along(data.models)) {
                      data.model <- data.models[[i]]
                      dataset <- datasets[[i]] ## all NA
                      expose <- dataset
                      if (methods::is(data.model, "Poisson"))
                          expose <- toDouble(expose)
                      data.model <- predictModelUseExp(object = data.model,
                                                       y = dataset,
                                                       exposure = expose)
                      data.models[[i]] <- data.model
                  }
                  object@model <- model
                  object@y <- y
                  object@dataModels  <- data.models
                  object
              }
          })




## updateCombined #####################################################################

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedModelNormal"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
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
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
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
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
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
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
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
          signature(object = "CombinedModelCMPNotHasExp"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedModelCMPNotHasExp_R, object, nUpdate)
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
          signature(object = "CombinedModelCMPHasExp"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedModelCMPHasExp_R, object, nUpdate)
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


## Counts

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedCountsPoissonNotHasExp"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
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
                  dataModels <- object@dataModels
                  datasets <- object@datasets
                  transforms <- object@transforms
                  jointUpdate <- object@jointUpdate@.Data
                  for (i in seq_len(nUpdate)) {
                      if (jointUpdate)
                          object <- updateCountsAndThetaPoissonNotUseExp(object)
                      else
                          object@y <- updateCountsPoissonNotUseExp(y = object@y,
                                                                   model = object@model,
                                                                   dataModels = dataModels,
                                                                   datasets = datasets,
                                                                   transforms = transforms)
                      object@model <- updateModelNotUseExp(object = object@model,
                                                           y = object@y)
                      object@dataModels <- updateDataModelsCounts(dataModels = dataModels,
                                                                  datasets = datasets,
                                                                  transforms = transforms,
                                                                  y = object@y)
                  }
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedCountsPoissonHasExp"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
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
                  exposure <- object@exposure
                  dataModels <- object@dataModels
                  datasets <- object@datasets
                  transforms <- object@transforms
                  jointUpdate <- object@jointUpdate@.Data
                  for (i in seq_len(nUpdate)) {
                      if (jointUpdate)
                          object <- updateCountsAndThetaPoissonUseExp(object)
                      else
                          object@y <- updateCountsPoissonUseExp(y = object@y,
                                                                model = object@model,
                                                                exposure = exposure,
                                                                dataModels = dataModels,
                                                                datasets = datasets,
                                                                transforms = transforms)
                      object@model <- updateModelUseExp(object = object@model,
                                                        y = object@y,
                                                        exposure = exposure)
                      object@dataModels <- updateDataModelsCounts(dataModels = dataModels,
                                                                  datasets = datasets,
                                                                  transforms = transforms,
                                                                  y = object@y)
                  }
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedCountsBinomial"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
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
                  exposure <- object@exposure
                  datasets <- object@datasets
                  dataModels <- object@dataModels
                  transforms <- object@transforms
                  jointUpdate <- object@jointUpdate@.Data
                  for (i in seq_len(nUpdate)) {
                      if (jointUpdate)
                          object <- updateCountsAndThetaBinomial(object)
                      else
                          object@y <- updateCountsBinomial(y = object@y,
                                                           model = object@model,
                                                           exposure = exposure,
                                                           dataModels = dataModels,
                                                           datasets = datasets,
                                                           transforms = transforms)
                      object@model <- updateModelUseExp(object = object@model,
                                                        y = object@y,
                                                        exposure = exposure)
                      object@dataModels <- updateDataModelsCounts(dataModels = object@dataModels,
                                                                  datasets = datasets,
                                                                  transforms = transforms,
                                                                  y = object@y)
                  }
                  object
              }
          })


## Accounts
## TRANSLATED
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
                  i.births <- combined@iBirths
                  i.orig.dest <- combined@iOrigDest
                  i.pool <- combined@iPool
                  i.int.net <- combined@iIntNet
                  is.small.update <- combined@isSmallUpdate@.Data 
                  model.uses.exposure <- combined@modelUsesExposure
                  is.popn <- i.comp == 0L
                  is.births <- i.comp == i.births
                  is.orig.dest <- i.comp == i.orig.dest
                  is.pool <- i.comp == i.pool
                  is.int.net <- i.comp == i.int.net
                  use.prior.popn <- combined@usePriorPopn@.Data
                  if (use.prior.popn && !is.small.update)
                      diff.dens.popn <- diffLogDensPopn(combined)
                  else
                      diff.dens.popn <- 0
                  if (is.popn) {
                      diff.dens.self <- 0
                      diff.dens.exp <- diffLogDensExpPopn(combined)
                  }
                  else if (is.births) {
                      if (is.small.update) {
                          diff.dens.self <- diffLogDensJumpBirthsSmall(combined)
                          diff.dens.exp <- 0
                      }
                      else { 
                          if (model.uses.exposure[i.comp])
                              diff.dens.self <- diffLogDensJumpComp(combined)
                          else
                              diff.dens.self <- 0
                          diff.dens.exp <- diffLogDensExpComp(combined)
                      } 
                  }
                  else if (is.orig.dest) {
                      if (is.small.update)  {
                          diff.dens.self <- diffLogDensJumpOrigDestSmall(combined)
                          diff.dens.exp <- diffLogDensExpOrigDestSmall(combined)
                      }
                      else { 
                          if (model.uses.exposure[i.comp])
                              diff.dens.self <- diffLogDensJumpOrigDest(combined)
                          else
                              diff.dens.self <- 0
                          diff.dens.exp <- diffLogDensExpOrigDestPoolNet(combined)
                      }
                  }
                  else if (is.pool) {
                      if (model.uses.exposure[i.comp])
                          diff.dens.self <- diffLogDensJumpPoolWithExpose(combined)
                      else
                          diff.dens.self <- diffLogDensJumpPoolNoExpose(combined)
                      diff.dens.exp <- diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else if (is.int.net) {
                      diff.dens.self <- diffLogDensJumpNet(combined)
                      diff.dens.exp <- diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else { 
                      if (is.small.update) {
                          diff.dens.self <- diffLogDensJumpCompSmall(combined)
                          diff.dens.exp <- diffLogDensExpCompSmall(combined)
                      }
                      else { 
                          if (model.uses.exposure[i.comp])
                              diff.dens.self <- diffLogDensJumpComp(combined)
                          else
                              diff.dens.self <- 0
                          diff.dens.exp <- diffLogDensExpComp(combined)
                      } 
                  }
                  is.invalid <- (is.infinite(diff.dens.self)
                      && is.infinite(diff.dens.exp)
                      && ((diff.dens.self > diff.dens.exp) || (diff.dens.self < diff.dens.exp)))
                  if (is.invalid)
                      ans <- -Inf ## stay at current position
                  else
                      ans <- diff.dens.popn + diff.dens.self + diff.dens.exp
                  ans
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("diffLogLikAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(diffLogLikAccount_CombinedAccountMovements_R, object)
                  else
                      .Call(diffLogLikAccount_R, object)
              }
              else {
                  i.comp <- object@iComp
                  i.orig.dest <- object@iOrigDest
                  i.pool <- object@iPool
                  i.int.net <- object@iIntNet
                  is.small.update <- object@isSmallUpdate
                  if (i.comp == 0L)
                      diffLogLikAccountMovePopn(object)
                  else if (i.comp == i.orig.dest) {
                      if (is.small.update)
                          diffLogLikAccountMoveCompSmall(object)
                      else
                          diffLogLikAccountMoveOrigDest(object)
                  }
                  else if (i.comp == i.pool)
                      diffLogLikAccountMovePool(object)
                  else if (i.comp == i.int.net)
                      diffLogLikAccountMoveNet(object)
                  else {
                      if (is.small.update)
                          diffLogLikAccountMoveCompSmall(object)
                      else
                          diffLogLikAccountMoveComp(object)
                  }
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateProposalAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              stopifnot(methods::validObject(object))
              if (useC) {
                  if (useSpecific)
                      .Call(updateProposalAccount_CombinedAccountMovements_R, object)
                  else
                      .Call(updateProposalAccount_R, object)
              }
              else {
                  prob.popn <- object@probPopn
                  prob.small.update <- object@probSmallUpdate@.Data 
                  has.age <- object@hasAge@.Data 
                  update.popn <- stats::runif(n = 1L) < prob.popn
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
                      is.net.vec <- object@isNet 
                      prob.small.update <- object@probSmallUpdate 
                      i.comp <- rcateg1(cum.prob)
                      object@iComp <- i.comp
                      if (i.comp == i.births) {
                          is.small.update <- has.age && (stats::runif(n = 1L) < prob.small.update) 
                          if (is.small.update) 
                              updateProposalAccountMoveBirthsSmall(object) 
                          else 
                              updateProposalAccountMoveBirths(object)
                      } 
                      else if (i.comp == i.orig.dest) {
                          is.small.update <- has.age && (stats::runif(n = 1L) < prob.small.update) 
                          if (is.small.update) 
                              updateProposalAccountMoveOrigDestSmall(object) 
                          else 
                              updateProposalAccountMoveOrigDest(object) 
                      }
                      else if (i.comp == i.pool)
                          updateProposalAccountMovePool(object)
                      else if (i.comp == i.int.net)
                          updateProposalAccountMoveNet(object)
                      else { # comp
                          is.net <- is.net.vec[i.comp] 
                          is.small.update <- !is.net && has.age && (stats::runif(n = 1L) < prob.small.update) 
                          if (is.small.update) 
                              updateProposalAccountMoveCompSmall(object) 
                          else 
                              updateProposalAccountMoveComp(object)
                      }
                  }
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateValuesAccount",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              stopifnot(methods::validObject(combined))
              if (useC) {
                  if (useSpecific)
                      .Call(updateValuesAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(updateValuesAccount_R, combined)
              }
              else {
                  has.age <- combined@hasAge
                  is.small.update <- combined@isSmallUpdate
                  combined <- updateCellMove(combined)
                  if (is.small.update) {
                      combined <- updateAccSmall(combined)
                      combined <- updateExpSmall(combined)
                  }
                  else {
                      combined <- updateSubsequentPopnMove(combined)
                      combined <- updateSubsequentExpMove(combined)
                      if (has.age)
                          combined <- updateSubsequentAccMove(combined)
                  }
                  combined
              }
          })

## TRANSLATED
## HAS_TESTS
## 'expectedExposure' equals result of calling 'exposure' on
## 'population', so dimensions in same order, except for
## "triangle" dimension, which is last in 'expectedExposure'
## and absent from 'population'
setMethod("updateExpectedExposure",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updateExpectedExposure_CombinedAccountMovements_R, combined)
                  else
                      .Call(updateExpectedExposure_R, combined)
              }
              else {
                  expected.exposure <- combined@expectedExposure
                  age.time.step <- combined@ageTimeStep
                  theta <- combined@systemModels[[1L]]@theta
                  description <- combined@descriptions[[1L]]
                  n.time.popn <- description@nTime
                  step.time <- description@stepTime
                  length.popn <- description@length
                  has.age <- combined@hasAge@.Data
                  if (has.age) {
                      n.age <- description@nAge
                      step.age.popn <- description@stepAge
                  }
                  n.time.exp <- n.time.popn - 1L
                  length.exp.no.tri <- (length.popn %/% n.time.popn) * n.time.exp # excludes triangle dim, if any
                  length.slice.popn <- n.time.popn * step.time
                  length.slice.exp <- n.time.exp * step.time
                  for (i in seq_len(length.exp.no.tri)) {
                      i.popn.start <- (((i - 1L) %/% length.slice.exp) * length.slice.popn
                          + (i - 1L) %% length.slice.exp
                          + 1)
                      i.popn.end <- i.popn.start + step.time
                      exp.start <- 0.5 * age.time.step * theta[i.popn.start]
                      exp.end <- 0.5 * age.time.step * theta[i.popn.end]
                      if (has.age) {
                          i.age <- (((i.popn.start - 1L) %/% step.age.popn) %% n.age) + 1L
                          is.final  <- i.age == n.age
                          if (is.final) {
                              exp.total <- exp.start + exp.end
                              exp.start <- (2/3) * exp.total
                              exp.end <- (1/3) * exp.total
                          }
                          expected.exposure[i + length.exp.no.tri] <- exp.start # upper triangle
                          expected.exposure[i] <- exp.end # lower triangle
                      }
                      else
                          expected.exposure[i] <- exp.start + exp.end
                  }
                  combined@expectedExposure <- expected.exposure
                  combined
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateSystemModels",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updateSystemModels_CombinedAccountMovements_R, combined)
                  else
                      .Call(updateSystemModels_R, combined)
              }
              else {
                  system.models <- combined@systemModels
                  population <- combined@account@population
                  components <- combined@account@components
                  model.uses.exposure <- combined@modelUsesExposure
                  update.system.model <- combined@updateSystemModel
                  transforms.exp.to.comp <- combined@transformsExpToComp
                  transform.exp.to.births <- combined@transformExpToBirths
                  i.births <- combined@iBirths
                  ## population
                  if (update.system.model[1L]) {
                      model <- system.models[[1L]]
                      model <- updateModelNotUseExp(model,
                                                    y = population)
                      system.models[[1L]] <- model
                  }
                  ## components
                  for (i in seq_along(components)) {
                      if (update.system.model[i + 1L]) {
                          model <- system.models[[i + 1L]]
                          component <- components[[i]]
                          uses.exposure <- model.uses.exposure[i + 1L]
                          if (uses.exposure) {
                              exposure <- combined@exposure@.Data
                              is.births <- i == i.births
                              if (is.births)
                                  exposure <- collapse(exposure,
                                                       transform = transform.exp.to.births)
                              transform <- transforms.exp.to.comp[[i]]
                              if (!is.null(transform))
                                  exposure <- extend(exposure,
                                                     transform = transforms.exp.to.comp[[i]])
                              model <- updateModelUseExp(object = model,
                                                         y = component,
                                                         exposure = exposure)
                          }
                          else {
                              if (methods::is(model, "Normal"))
                                  component <- toDouble(component)
                              model <- updateModelNotUseExp(object = model,
                                                            y = component)
                          }
                          system.models[[i + 1L]] <- model
                      }
                  }
                  combined@systemModels <- system.models
                  combined
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("updateCombined",
          signature(object = "CombinedAccountMovements"),
          function(object, nUpdate = 1L,
                   useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedAccount_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  for (i in seq_len(nUpdate)) {
                      object <- updateAccount(object)
                      object <- updateSystemModels(object)
                      object <- updateExpectedExposure(object)
                      object <- updateDataModelsAccount(object)
                  }
                  object
              }
          })
