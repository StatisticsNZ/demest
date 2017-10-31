
## initialCombinedModel ###############################################################

## Assume that 'y', 'exposure', and 'weights', have had 'checkAndTidyY',
## 'checkAndTidyExposure', and 'checkAndTidyWeights' called on them.
## 'checkAndTidyY' checks that 'y' has class DemographicArray
## 'checkAndTidyExposure' checks that 'exposure' has class NULL or Counts

## Validity tests for 'y' and 'exposure' test requirements specific
## to each distribution that were not tested for by
## 'checkAndTidyY' or 'checkAndTidyExposure'

## All functions need to accept 'exposure' and 'weights' arguments
## (without necessarily doing anything with them) because the
## calling function always wants to pass these arguments.

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecBinomialVarying",
                    y = "Counts",
                    exposure = "Counts",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored when distribution is %s",
                                   "weights", "Binomial"))
              if (any(y[!is.na(y)] > exposure[!is.na(y)]))
                  stop(gettextf("'%s' greater than '%s'",
                                "y", "exposure"))
              model <- initialModel(object, y = y, exposure = exposure)
              methods::new("CombinedModelBinomial",
                           model = model,
                           y = y,
                           exposure = exposure)
          })

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecBinomialVarying",
                    y = "ANY",
                    exposure = "CountsOrNULL",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              stop(gettextf("'%s' has class \"%s\" : in a %s model '%s' must have class \"%s\"",
                            "y", class(y), "Binomial", "y", "Counts"))
          })

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecBinomialVarying",
                    y = "Counts",
                    exposure = "NULL",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              stop(gettextf("a %s model requires an '%s' argument, but no '%s' argument supplied",
                            "Binomial", "exposure", "exposure"))
          })

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecNormalVarying",
                    y = "DemographicArray",
                    exposure = "ANY",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              if (!is.null(exposure))
                  warning(gettextf("'%s' argument ignored when distribution is %s",
                                   "exposure", "Normal"))
              if (is.null(weights)) {
                  .Data <- array(1, dim = dim(y), dimnames = dimnames(y))
                  metadata <- y@metadata
                  weights <- methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              model <- initialModel(object, y = y, weights = weights)
              methods::new("CombinedModelNormal",
                           model = model,
                           y = y)
          })

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecPoissonVarying",
                    y = "Counts",
                    exposure = "NULL",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored when distribution is %s",
                                   "weights", "Poisson"))
              model <- initialModel(object, y = y, exposure = exposure)
              methods::new("CombinedModelPoissonNotHasExp",
                  model = model,
                  y = y)
          })

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecPoissonVarying",
                    y = "Counts",
                    exposure = "Counts",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored when distribution is %s",
                                   "weights", "Poisson"))
              model <- initialModel(object, y = y, exposure = exposure)
              methods::new("CombinedModelPoissonHasExp",
                  model = model,
                  y = y,
                  exposure = exposure)
          })

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecPoissonVarying",
                    y = "ANY",
                    exposure = "CountsOrNULL",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              stop(gettextf("'%s' has class \"%s\" : in a %s model '%s' must have class \"%s\"",
                            "y", class(y), "Poisson", "y", "Counts"))
          })



## initialCombinedModelPredict ##############################################################

## HAS_TESTS
setMethod("initialCombinedModelPredict",
          signature(combined = "CombinedModelNormal"),
          function(combined, along, labels, n, covariates,
                   aggregate, lower, upper, yIsCounts) {
              model <- combined@model
              model <- initialModelPredict(model = model,
                                           along = along,
                                           labels = labels,
                                           n = n,
                                           offsetModel = 1L,
                                           covariates = covariates,
                                           aggregate = aggregate,
                                           lower = lower,
                                           upper = upper)
              metadata <- model@metadataY
              .Data <- array(as.double(NA),
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              class.y <- if (yIsCounts) "Counts" else "Values"
              y <- methods::new(class.y, .Data = .Data, metadata = metadata)
              methods::new("CombinedModelNormal",
                  model = model,
                  y = y)
          })

## HAS_TESTS
setMethod("initialCombinedModelPredict",
          signature(combined = "CombinedModelPoissonNotHasExp"),
          function(combined, along, labels, n, covariates,
                   aggregate, lower, upper, yIsCounts) {
              model <- combined@model
              model <- initialModelPredict(model = model,
                                           along = along,
                                           labels = labels,
                                           n = n,
                                           offsetModel = 1L,
                                           covariates = covariates,
                                           aggregate = aggregate,
                                           lower = lower,
                                           upper = upper)
              metadata <- model@metadataY
              .Data <- array(as.integer(NA),
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              class.y <- if (yIsCounts) "Counts" else "Values"
              y <- methods::new(class.y, .Data = .Data, metadata = metadata)
              methods::new("CombinedModelPoissonNotHasExp",
                  model = model,
                  y = y)
          })

## HAS_TESTS
setMethod("initialCombinedModelPredict",
          signature(combined = "CombinedModelBinomial"),
          function(combined, along, labels, n, covariates,
                   aggregate, lower, upper, yIsCounts) {
              model <- combined@model
              model <- initialModelPredict(model = model,
                                           along = along,
                                           labels = labels,
                                           n = n,
                                           offsetModel = 1L,
                                           covariates = covariates,
                                           aggregate = aggregate,
                                           lower = lower,
                                           upper = upper)
              metadata <- model@metadataY
              .Data <- array(as.integer(NA),
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              class.y <- if (yIsCounts) "Counts" else "Values"
              y <- methods::new(class.y, .Data = .Data, metadata = metadata)
              exposure <- methods::new("Counts", .Data = .Data, metadata = metadata)
              methods::new("CombinedModelBinomial",
                  model = model,
                  y = y,
                  exposure = exposure)
          })

## HAS_TESTS
setMethod("initialCombinedModelPredict",
          signature(combined = "CombinedModelPoissonHasExp"),
          function(combined, along, labels, n, covariates,
                   aggregate, lower, upper, yIsCounts) {
              model <- combined@model
              model <- initialModelPredict(model = model,
                                           along = along,
                                           labels = labels,
                                           n = n,
                                           offsetModel = 1L,
                                           covariates = covariates,
                                           aggregate = aggregate,
                                           lower = lower,
                                           upper = upper)
              metadata <- model@metadataY
              .Data.y <- array(as.integer(NA),
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              class.y <- if (yIsCounts) "Counts" else "Values"
              y <- methods::new(class.y, .Data = .Data.y, metadata = metadata)
              .Data.exp <- array(as.double(NA),
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              exposure <- methods::new("Counts", .Data = .Data.exp, metadata = metadata)
              methods::new("CombinedModelPoissonHasExp",
                  model = model,
                  y = y,
                  exposure = exposure)
          })


## initialCombinedCounts #############################################################

## Assume that all arguments valid

## HAS_TESTS
setMethod("initialCombinedCounts",
          signature(object = "SpecPoissonVarying",
                    y = "Counts",
                    exposure = "ANY",
                    observationModels = "list",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(object, y, exposure, observationModels, datasets,
                   namesDatasets, transforms) {
              y <- imputeCountsInternal(y)
              for (i in seq_along(observationModels)) {
                  y.collapsed <- dembase::collapse(y, transform = transforms[[i]])
                  observationModels[[i]] <- initialModel(observationModels[[i]],
                                                         y = datasets[[i]],
                                                         exposure = y.collapsed)
              }
              has.exposure <- !is.null(exposure)
              if (has.exposure) {
                  model <- initialModel(object, y = y, exposure = exposure)
                  methods::new("CombinedCountsPoissonHasExp",
                               model = model,
                               y = y,
                               exposure = exposure,
                               observationModels = observationModels,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
              }
              else {
                  model <- initialModel(object, y = y, exposure = exposure)
                  methods::new("CombinedCountsPoissonNotHasExp",
                               model = model,
                               y = y,
                               observationModels = observationModels,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
              }
          })

## HAS_TESTS
setMethod("initialCombinedCounts",
          signature(object = "SpecBinomialVarying",
                    y = "Counts",
                    exposure = "ANY",
                    observationModels = "list",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(object, y, exposure, observationModels, datasets,
                   namesDatasets, transforms) {
              if (is.null(exposure))
                  stop(gettextf("binomial model, but no '%s' argument supplied",
                                "exposure"))
              y <- imputeCountsInternal(y, max = exposure)
              if (any(y[!is.na(y)] > exposure[!is.na(y)]))
                  stop(gettextf("'%s' greater than '%s'",
                                "y", "exposure"))
              for (i in seq_along(observationModels)) {
                  y.collapsed <- dembase::collapse(y, transform = transforms[[i]])
                  observationModels[[i]] <- initialModel(observationModels[[i]],
                                                   y = datasets[[i]],
                                                   exposure = y.collapsed)
              }
              model <- initialModel(object, y = y, exposure = exposure)
              methods::new("CombinedCountsBinomial",
                           model = model,
                           y = y,
                           exposure = exposure,
                           observationModels = observationModels,
                           datasets = datasets,
                           namesDatasets = namesDatasets,
                           transforms = transforms)
          })


## COMBINED ACCOUNT ###################################################################

setMethod("initialCombinedAccount",
          signature(account = "Movements",
                    systemModels = "list",
                    systemWeights = "list",
                    observationModels = "list",
                    seriesIndices = "integer",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(account, systemModels, systemWeights,
                   observationModels, seriesIndices, 
                   datasets, namesDatasets, transforms) {
              population <- account@population
              components <- account@components
              has.age <- "age" %in% dimtypes(population, use.names = FALSE)
              n.popn <- length(population)
              n.components <- sapply(components, length)
              n.cell.account <- n.popn + sum(n.components)
              prob.popn <- n.popn / (n.popn + sum(n.components))
              cum.prob.popn <- cumsum(n.components) / sum(n.components)
              is.births <- sapply(components, methods::is, "Births")
              is.orig.dest <- sapply(components, methods::is, "HasOrigDest")
              is.pool <- sapply(components, methods::is, "Pool")
              is.int.net <- sapply(components, methods::is, "InternalMovementsNet")
              is.net.move <- sapply(components, methods::is, "NetMovements")
              i.births <- if (any(is.births)) which(is.births) else 0L
              i.orig.dest <- if (any(is.orig.dest)) which(is.orig.dest) else 0L
              i.pool <- if (any(is.pool)) which(is.pool) else 0L
              i.int.net <- if (any(is.int.net)) which(is.int.net) else 0L
              is.net <- is.int.net | is.net.move
              if (has.age) {
                  accession <- dembase::accession(account,
                                                  births = FALSE)
                  accession <- new("Accession",
                                   .Data = accession@.Data,
                                   metadata = accession@metadata)
                  iterator.acc <- CohortIterator(accession)
                  mappings.to.acc <- lapply(components, function(x) Mapping(x, accession))
              }
              theta.popn <- systemModels[[1L]]@theta
              exposure <- dembase::exposure(population,
                                            triangles = has.age)
              expected.exposure <- dembase::exposure(theta.popn,
                                                     triangles = has.age)
              exposure <- new("Exposure",
                              .Data = exposure@.Data,
                              metadata = exposure@metadata)
              expected.exposure <- new("Exposure",
                                       .Data = expected.exposure@.Data,
                                       metadata = expected.exposure@metadata)
              population <- methods::new("Population",
                                         .Data = population@.Data,
                                         metadata = population@metadata)
              is.increment <- sapply(components, dembase::isPositiveIncrement)
              iterator.popn <- CohortIterator(population)
              descriptions <- lapply(c(list(population), components), Description)
              mappings.from.exp <- lapply(components, function(x) Mapping(exposure, x))
              mappings.to.exp <- lapply(components, function(x) Mapping(x, exposure))
              mappings.to.popn <- lapply(components, function(x) Mapping(x, population))
              model.uses.exposure <- sapply(systemModels, function(x) x@useExpose@.Data)
              for (i in seq_along(systemModels)) {
                  series <- if (i == 1L) population else components[[i - 1L]]
                  if (model.uses.exposure[i]) {
                      if (is(series, "Births"))
                          expose <- exposureBirths(object = population,
                                                   births = series,
                                                   triangles = has.age)
                      else {
                          expose <- exposure
                          if (is(series, "HasOrigDest")) {
                              names.series <- names(series)
                              dimtypes.series <- dimtypes(series, use.names = FALSE)
                              i.orig.vec <- grep("origin", dimtypes.series)
                              for (i.orig in i.orig.vec) {
                                  name.series <- names.series[i.orig]
                                  name.expose <- sub("_orig$", "", name.series)
                                  expose <- addPair(expose,
                                                    base = name.expose)
                              }
                          }
                      }
                      expose <- makeCompatible(x = expose,
                                               y = series,
                                               subset = TRUE)
                      systemModels[[i]] <- initialModel(systemModels[[i]],
                                                        y = series,
                                                        exposure = expose)
                  }
                  else {
                      weights <- systemWeights[[i]]
                      if (is.null(weights))
                          systemModels[[i]] <- initialModel(systemModels[[i]],
                                                            y = series,
                                                            exposure = NULL)
                      else {
                          systemModels[[i]] <- initialModel(systemModels[[i]],
                                                            y = series,
                                                            weights = weights)
                      }
                  }
              }
              for (i in seq_along(observationModels)) {
                  series.index <- seriesIndices[i]
                  series <- if (series.index == 0L) population else components[[series.index]]
                  series.collapsed <- dembase::collapse(series, transform = transforms[[i]])
                  model <- observationModels[[i]]
                  if (methods::is(model, "Poisson"))
                      series.collapsed <- dembase::toDouble(series.collapsed)
                  dataset <- datasets[[i]]
                  observationModels[[i]] <- initialModel(model,
                                                         y = dataset,
                                                         exposure = series.collapsed)
              }
              if (has.age) {
                  methods::new("CombinedAccountMovementsHasAge",
                               accession = accession,
                               account = account,
                               cumProbPopn = cum.prob.popn,
                               datasets = datasets,
                               descriptions = descriptions,
                               diffProp = NA_integer_,
                               exposure = exposure,
                               expectedExposure = expected.exposure,
                               generatedNewProposal = new("LogicalFlag", FALSE),
                               hasAge = new("LogicalFlag", TRUE),
                               iAccNext = NA_integer_,
                               iAccNextOther = NA_integer_,
                               iBirths = i.births,
                               iCell = NA_integer_,
                               iCellOther = NA_integer_,
                               iComp = 0L,
                               iExpFirst = NA_integer_,
                               iExpFirstOther = NA_integer_,
                               iExposure = NA_integer_,
                               iExposureOther = NA_integer_,
                               iIntNet = i.int.net,
                               iOrigDest = i.orig.dest,
                               iPopnNext = NA_integer_,
                               iPopnNextOther = NA_integer_,
                               iPool = i.pool,
                               isIncrement = is.increment,
                               isLowerTriangle = new("LogicalFlag", FALSE),
                               isNet = is.net,
                               iteratorAcc = iterator.acc,
                               iteratorPopn = iterator.popn,
                               mappingsFromExp = mappings.from.exp,
                               mappingsToAcc = mappings.to.acc,
                               mappingsToExp = mappings.to.exp,
                               mappingsToPopn = mappings.to.popn,
                               modelUsesExposure = model.uses.exposure,
                               namesDatasets = namesDatasets,                           
                               nCellAccount = n.cell.account,
                               observationModels = observationModels,
                               probPopn = prob.popn,
                               seriesIndices = seriesIndices,
                               systemModels = systemModels,
                               transforms = transforms)
              }
              else {
                  methods::new("CombinedAccountMovements",
                               account = account,
                               cumProbPopn = cum.prob.popn,
                               datasets = datasets,
                               descriptions = descriptions,
                               diffProp = NA_integer_,
                               exposure = exposure,
                               expectedExposure = expected.exposure,
                               generatedNewProposal = new("LogicalFlag", FALSE),
                               hasAge = new("LogicalFlag", FALSE),
                               iBirths = i.births,
                               iCell = NA_integer_,
                               iCellOther = NA_integer_,
                               iComp = 0L,
                               iExpFirst = NA_integer_,
                               iExpFirstOther = NA_integer_,
                               iExposure = NA_integer_,
                               iExposureOther = NA_integer_,
                               iIntNet = i.int.net,
                               iOrigDest = i.orig.dest,
                               iPopnNext = NA_integer_,
                               iPopnNextOther = NA_integer_,
                               iPool = i.pool,
                               isIncrement = is.increment,
                               isNet = is.net,
                               iteratorPopn = iterator.popn,
                               mappingsFromExp = mappings.from.exp,
                               mappingsToExp = mappings.to.exp,
                               mappingsToPopn = mappings.to.popn,
                               modelUsesExposure = model.uses.exposure,
                               namesDatasets = namesDatasets,                           
                               nCellAccount = n.cell.account,
                               observationModels = observationModels,
                               probPopn = prob.popn,
                               seriesIndices = seriesIndices,
                               systemModels = systemModels,
                               transforms = transforms)
              }
          })
