
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
                   datasets, namesDatasets, transforms,
                   dominant = c("Female", "Male")) {
              population <- account@population
              components <- account@components
              names.components <- account@namesComponents
              has.age <- "age" %in% dimtypes(population, use.names = FALSE)
              n.popn <- length(population)
              n.components <- sapply(components, length)
              n.cell.account <- n.popn + sum(n.components)
              prob.popn <- n.popn / (n.popn + sum(n.components))
              cum.prob.popn <- cumsum(n.components) / sum(n.components)
              is.births <- sapply(components, methods::is, "Births")
              is.orig.dest <- sapply(components, methods::is, "HasOrigDest")
              is.par.ch <- sapply(components, methods::is, "HasParentChild")
              is.pool <- sapply(components, methods::is, "InternalMovementsPool")
              is.int.net <- sapply(components, methods::is, "InternalMovementsNet")
              is.net.move <- sapply(components, methods::is, "NetMovements")
              i.births <- if (any(is.births)) which(is.births) else 0L
              i.orig.dest <- if (any(is.orig.dest)) which(is.orig.dest) else 0L
              i.par.ch <- if (any(is.par.ch)) which(is.par.ch) else 0L
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
              exposure <- dembase::exposure(population,
                                            triangles = has.age)
              exposure <- new("Exposure",
                              .Data = exposure@.Data,
                              metadata = exposure@metadata)
              population <- methods::new("Population",
                                         .Data = population@.Data,
                                         metadata = population@metadata)
              is.increment <- sapply(components, dembase::isPositiveIncrement)
              iterator.popn <- CohortIterator(population)
              iterator.exposure <- CohortIterator(exposure)
              iterators.comp <- lapply(components, CohortIterator)
              descriptions <- lapply(c(list(population), components), Description)
              mappings.from.exp <- lapply(components, function(x) Mapping(exposure, x))
              mappings.to.exp <- lapply(components, function(x) Mapping(x, exposure))
              mappings.to.popn <- lapply(components, function(x) Mapping(x, population))
              model.uses.exposure <- sapply(systemModels, function(x) x@useExpose@.Data)
              if ((i.births > 0L) && model.uses.exposure[i.births + 1L])
                  transform.exp.to.births <- makeTransformExpToBirths(exposure = exposure,
                                                                      births = components[[i.births]],
                                                                      dominant = dominant)
              else
                  transform.exp.to.births <- new("CollapseTransform")
              transforms.exp.to.comp <- vector(mode = "list", length = length(components))
              for (i in seq_along(transforms.exp.to.comp)) {
                  if (model.uses.exposure[i + 1L]) {
                      if (i == i.births) {
                          exposure.births <- collapse(exposure,
                                                      transform = transform.exp.to.births)
                          transform <- makeTransformExpToComp(exposure = exposure.births,
                                                              component = components[[i]],
                                                              nameComponent = names.components[i])
                      }
                      else
                          transform <- makeTransformExpToComp(exposure = exposure,
                                                              component = components[[i]],
                                                              nameComponent = names.components[i])
                  }
                  else
                      transform <- NULL
                  if (is.null(transform))
                      transforms.exp.to.comp[i] <- list(NULL)
                  else
                      transforms.exp.to.comp[[i]] <- transform
              }
              for (i in seq_along(systemModels)) {
                  series <- if (i == 1L) population else components[[i - 1L]]
                  if (model.uses.exposure[i]) {
                      if (i - 1L == i.births)
                          expose <- collapse(exposure,
                                             transform = transform.exp.to.births)
                      else
                          expose <- exposure
                      transform <- transforms.exp.to.comp[[i - 1L]]
                      if (!is.null(transform))
                          expose <- extend(expose,
                                           transform = transform)
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
              .Data.theta.popn <- array(systemModels[[1L]]@theta,
                                        dim = dim(population),
                                        dimnames = dimnames(population))
              metadata.theta.popn <- population@metadata
              theta.popn <- new("Counts",
                                .Data = .Data.theta.popn,
                                metadata = metadata.theta.popn)
              expected.exposure <- dembase::exposure(theta.popn,
                                                     triangles = has.age)
              expected.exposure <- new("Exposure",
                                       .Data = expected.exposure@.Data,
                                       metadata = expected.exposure@metadata)
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
                               iParCh = i.par.ch,
                               iPopnNext = NA_integer_,
                               iPopnNextOther = NA_integer_,
                               iPool = i.pool,
                               isIncrement = is.increment,
                               isLowerTriangle = new("LogicalFlag", FALSE),
                               isNet = is.net,
                               iteratorAcc = iterator.acc,
                               iteratorExposure = iterator.exposure,
                               iteratorPopn = iterator.popn,
                               iteratorsComp = iterators.comp,
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
                               transformExpToBirths = transform.exp.to.births,
                               transforms = transforms,
                               transformsExpToComp = transforms.exp.to.comp)
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
                               iParCh = i.par.ch,
                               iPopnNext = NA_integer_,
                               iPopnNextOther = NA_integer_,
                               iPool = i.pool,
                               isIncrement = is.increment,
                               isNet = is.net,
                               iteratorExposure = iterator.exposure,
                               iteratorPopn = iterator.popn,
                               iteratorsComp = iterators.comp,
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
                               transformExpToBirths = transform.exp.to.births,
                               transforms = transforms,
                               transformsExpToComp = transforms.exp.to.comp)
              }
          })
