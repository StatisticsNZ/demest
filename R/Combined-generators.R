
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

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecCMPVarying",
                    y = "Counts",
                    exposure = "NULL",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored when distribution is %s",
                                   "weights", "CMP"))
              model <- initialModel(object, y = y, exposure = exposure)
              methods::new("CombinedModelCMPNotHasExp",
                  model = model,
                  y = y)
          })

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecCMPVarying",
                    y = "Counts",
                    exposure = "Counts",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored when distribution is %s",
                                   "weights", "CMP"))
              model <- initialModel(object, y = y, exposure = exposure)
              methods::new("CombinedModelCMPHasExp",
                  model = model,
                  y = y,
                  exposure = exposure)
          })

## HAS_TESTS
setMethod("initialCombinedModel",
          signature(object = "SpecCMPVarying",
                    y = "ANY",
                    exposure = "CountsOrNULL",
                    weights = "ANY"),
          function(object, y, exposure, weights) {
              stop(gettextf("'%s' has class \"%s\" : in a %s model '%s' must have class \"%s\"",
                            "y", class(y), "CMP", "y", "Counts"))
          })

## initialCombinedModelSimulate #############################################################

## Creates orindary 'CombinedModel' object, but with initial draw
## of hyper-parameters, and with blank 'y'

## HAS_TESTS
setMethod("initialCombinedModelSimulate",
          signature(object = "SpecBinomialVarying"),
          function(object, y, exposure, weights) {
              model <- initialModel(object,
                                    y = y,
                                    exposure = exposure)
              model <- drawHyperParam(model)
              y <- setYToMissing(y)
              methods::new("CombinedModelBinomial",
                           model = model,
                           y = y,
                           exposure = exposure)
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
              struc.zero.array <- model@strucZeroArray@.Data
              .Data <- ifelse(struc.zero.array == 0L, 0L, NA_integer_)
              .Data <- array(.Data,
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
              struc.zero.array <- model@strucZeroArray@.Data
              .Data.y <- ifelse(struc.zero.array == 0L, 0L, NA_integer_)
              .Data.exp <- ifelse(struc.zero.array == 0L, 0, as.numeric(NA))
              .Data.y <- array(.Data.y,
                               dim = dim(metadata),
                               dimnames = dimnames(metadata))
              .Data.exp <- array(.Data.exp,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
              class.y <- if (yIsCounts) "Counts" else "Values"
              y <- methods::new(class.y,
                                .Data = .Data.y,
                                metadata = metadata)
              exposure <- methods::new("Counts",
                                       .Data = .Data.exp,
                                       metadata = metadata)
              methods::new("CombinedModelPoissonHasExp",
                           model = model,
                           y = y,
                           exposure = exposure)
          })


## HAS_TESTS
setMethod("initialCombinedModelPredict",
          signature(combined = "CombinedModelCMPNotHasExp"),
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
              struc.zero.array <- model@strucZeroArray@.Data
              .Data <- ifelse(struc.zero.array == 0L, 0L, NA_integer_)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              class.y <- if (yIsCounts) "Counts" else "Values"
              y <- methods::new(class.y, .Data = .Data, metadata = metadata)
              methods::new("CombinedModelCMPNotHasExp",
                           model = model,
                           y = y)
          })


## HAS_TESTS
setMethod("initialCombinedModelPredict",
          signature(combined = "CombinedModelCMPHasExp"),
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
              struc.zero.array <- model@strucZeroArray@.Data
              .Data.y <- ifelse(struc.zero.array == 0L, 0L, NA_integer_)
              .Data.exp <- ifelse(struc.zero.array == 0L, 0, as.numeric(NA))
              .Data.y <- array(.Data.y,
                               dim = dim(metadata),
                               dimnames = dimnames(metadata))
              .Data.exp <- array(.Data.exp,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
              class.y <- if (yIsCounts) "Counts" else "Values"
              y <- methods::new(class.y,
                                .Data = .Data.y,
                                metadata = metadata)
              exposure <- methods::new("Counts",
                                       .Data = .Data.exp,
                                       metadata = metadata)
              methods::new("CombinedModelCMPHasExp",
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
                    dataModels = "list",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(object, y, exposure, dataModels, datasets,
                   namesDatasets, transforms) {
              struc.zeros <- object@structuralZeros
              struc.zero.array <- makeStrucZeroArray(structuralZeros = struc.zeros,
                                                     y = y)
              y[struc.zero.array@.Data == 0L] <- 0L
              y <- imputeCountsInternal(y)
              for (i in seq_along(dataModels)) {
                  ## can't use standard collapse, which doesn't allow for concordances
                  .Data.y.collapsed <- dembase::collapse(y@.Data, transform = transforms[[i]])
                  metadata.y.collapsed <- datasets[[i]]@metadata
                  dimnames(.Data.y.collapsed) <- dimnames(metadata.y.collapsed)
                  y.collapsed <- methods::new("Counts",
                                              .Data = .Data.y.collapsed,
                                              metadata = metadata.y.collapsed)
                  dataModels[[i]] <- initialModel(dataModels[[i]],
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
                               dataModels = dataModels,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
              }
              else {
                  model <- initialModel(object, y = y, exposure = exposure)
                  methods::new("CombinedCountsPoissonNotHasExp",
                               model = model,
                               y = y,
                               dataModels = dataModels,
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
                    dataModels = "list",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(object, y, exposure, dataModels, datasets,
                   namesDatasets, transforms) {
              if (is.null(exposure))
                  stop(gettextf("binomial model, but no '%s' argument supplied",
                                "exposure"))
              y <- imputeCountsInternal(y, max = exposure)
              if (any(y[!is.na(y)] > exposure[!is.na(y)]))
                  stop(gettextf("'%s' greater than '%s'",
                                "y", "exposure"))
              for (i in seq_along(dataModels)) {
                  ## can't use standard collapse, which doesn't allow for concordances
                  .Data.y.collapsed <- dembase::collapse(y@.Data, transform = transforms[[i]])
                  metadata.y.collapsed <- datasets[[i]]@metadata
                  dimnames(.Data.y.collapsed) <- dimnames(metadata.y.collapsed)
                  y.collapsed <- methods::new("Counts",
                                              .Data = .Data.y.collapsed,
                                              metadata = metadata.y.collapsed)
                  dataModels[[i]] <- initialModel(dataModels[[i]],
                                                  y = datasets[[i]],
                                                  exposure = y.collapsed)
              }
              model <- initialModel(object, y = y, exposure = exposure)
              methods::new("CombinedCountsBinomial",
                           model = model,
                           y = y,
                           exposure = exposure,
                           dataModels = dataModels,
                           datasets = datasets,
                           namesDatasets = namesDatasets,
                           transforms = transforms)
          })


## initialCombinedCountsPredict ############################################################

## HAS_TESTS
setMethod("initialCombinedCountsPredict",
          signature(combined = "CombinedCountsPoissonHasExp"),
          function(combined, along, labels, n, exposure, covariates,
                   aggregate, lower, upper) {
              model.est <- combined@model
              metadata.y <- model.est@metadataY
              y.est <- combined@y
              datasets.est <- combined@datasets
              data.models.est <- combined@dataModels
              names.datasets <- combined@namesDatasets
              n.dataset <- length(datasets.est)
              pos <- 1L
              covariates.pred <- covariates[["model"]]
              aggregate.pred <- aggregate[["model"]]
              lower.pred <- lower[["model"]]
              upper.pred <- upper[["model"]]
              model.pred <- initialModelPredict(model = model.est,
                                                along = along,
                                                labels = labels,
                                                n = n,
                                                offsetModel = pos,
                                                covariates = covariates.pred,
                                                aggregate = aggregate.pred,
                                                lower = lower.pred,
                                                upper = upper.pred)
              pos <- pos + lengthValues(model.est)
              y.pred <- makeCountsPred(modelPred = model.pred)
              pos <- pos + lengthValues(y.est)
              data.models.pred <- vector(mode = "list", length = n.dataset)
              datasets.pred <- vector(mode = "list", length = n.dataset)
              transforms.pred <- vector(mode = "list", length = n.dataset)
              names.y <- names(metadata.y)
              name.dim.along <- names.y[along]
              for (i in seq_len(n.dataset)) {
                  data.model.est <- data.models.est[[i]]
                  name <- names.datasets[i]
                  covariates.pred <- covariates[["dataModels"]][[name]]
                  aggregate.pred <- aggregate[["dataModels"]][[name]]
                  lower.pred <- lower[["dataModels"]][[name]]
                  upper.pred <- upper[["dataModels"]][[name]]
                  along.pred <- match(name.dim.along, names(datasets.est[[i]]), nomatch = 0L)
                  if (along.pred == 0L)
                      stop(gettextf("dataset \"%s\" does not contain '%s' dimension [\"%s\"]",
                                    name, "along", name.dim.along))
                  data.model.pred <- initialModelPredict(model = data.model.est,
                                                         along = along.pred,
                                                         labels = labels,
                                                         n = n,
                                                         offsetModel = pos,
                                                         covariates = covariates.pred,
                                                         aggregate = aggregate.pred,
                                                         lower = lower.pred,
                                                         upper = upper.pred)
                  pos <- pos + lengthValues(data.model.est)
                  dataset.pred <- makeCountsPred(modelPred = data.model.pred)
                  transform.pred <- dembase::makeTransform(x = y.pred,
                                                           y = dataset.pred,
                                                           subset = TRUE)
                  transform.pred <- dembase::makeCollapseTransformExtra(transform.pred)
                  data.models.pred[[i]] <- data.model.pred
                  datasets.pred[[i]] <- dataset.pred
                  transforms.pred[[i]] <- transform.pred
              }
              methods::new("CombinedCountsPoissonHasExp",
                           model = model.pred,
                           y = y.pred,
                           exposure = exposure,
                           dataModels = data.models.pred,
                           datasets = datasets.pred,
                           namesDatasets = names.datasets,
                           transforms = transforms.pred)
          })

## HAS_TESTS
setMethod("initialCombinedCountsPredict",
          signature(combined = "CombinedCountsPoissonNotHasExp"),
          function(combined, along, labels, n, exposure, covariates,
                   aggregate, lower, upper) {
              model.est <- combined@model
              metadata.y <- model.est@metadataY
              y.est <- combined@y
              datasets.est <- combined@datasets
              data.models.est <- combined@dataModels
              names.datasets <- combined@namesDatasets
              n.dataset <- length(datasets.est)
              pos <- 1L
              covariates.pred <- covariates[["model"]]
              aggregate.pred <- aggregate[["model"]]
              lower.pred <- lower[["model"]]
              upper.pred <- upper[["model"]]
              model.pred <- initialModelPredict(model = model.est,
                                                along = along,
                                                labels = labels,
                                                n = n,
                                                offsetModel = pos,
                                                covariates = covariates.pred,
                                                aggregate = aggregate.pred,
                                                lower = lower.pred,
                                                upper = upper.pred)
              pos <- pos + lengthValues(model.est)
              y.pred <- makeCountsPred(modelPred = model.pred)
              pos <- pos + lengthValues(y.est)
              data.models.pred <- vector(mode = "list", length = n.dataset)
              datasets.pred <- vector(mode = "list", length = n.dataset)
              transforms.pred <- vector(mode = "list", length = n.dataset)
              names.y <- names(metadata.y)
              name.dim.along <- names.y[along]
              for (i in seq_len(n.dataset)) {
                  data.model.est <- data.models.est[[i]]
                  name <- names.datasets[i]
                  covariates.pred <- covariates[["dataModels"]][[name]]
                  aggregate.pred <- aggregate[["dataModels"]][[name]]
                  lower.pred <- lower[["dataModels"]][[name]]
                  upper.pred <- upper[["dataModels"]][[name]]
                  along.pred <- match(name.dim.along, names(datasets.est[[i]]), nomatch = 0L)
                  if (along.pred == 0L)
                      stop(gettextf("dataset \"%s\" does not contain '%s' dimension [\"%s\"]",
                                    name, "along", name.dim.along))
                  data.model.pred <- initialModelPredict(model = data.model.est,
                                                         along = along.pred,
                                                         labels = labels,
                                                         n = n,
                                                         offsetModel = pos,
                                                         covariates = covariates.pred,
                                                         aggregate = aggregate.pred,
                                                         lower = lower.pred,
                                                         upper = upper.pred)
                  pos <- pos + lengthValues(data.model.est)
                  dataset.pred <- makeCountsPred(modelPred = data.model.pred)
                  transform.pred <- dembase::makeTransform(x = y.pred,
                                                           y = dataset.pred,
                                                           subset = TRUE)
                  transform.pred <- dembase::makeCollapseTransformExtra(transform.pred)
                  data.models.pred[[i]] <- data.model.pred
                  datasets.pred[[i]] <- dataset.pred
                  transforms.pred[[i]] <- transform.pred
              }
              methods::new("CombinedCountsPoissonNotHasExp",
                           model = model.pred,
                           y = y.pred,
                           dataModels = data.models.pred,
                           datasets = datasets.pred,
                           namesDatasets = names.datasets,
                           transforms = transforms.pred)
          })




## COMBINED ACCOUNT ###################################################################

setMethod("initialCombinedAccount",
          signature(account = "Movements",
                    systemModels = "list",
                    systemWeights = "list",
                    dataModels = "list",
                    seriesIndices = "integer",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(account, systemModels, systemWeights,
                   dataModels, seriesIndices, 
                   datasets, namesDatasets, transforms,
                   dominant = c("Female", "Male"),
                   updateInitialPopn,
                   usePriorPopn,
                   probSmallUpdate = 0,
                   scaleNoise = 0) {
              population <- account@population
              components <- account@components
              names.components <- account@namesComponents
              has.age <- "age" %in% dimtypes(population, use.names = FALSE)
              age.time.step <- dembase::ageTimeStep(population)
              n.popn <- length(population)
              n.components <- sapply(components, length)
              n.cell.account <- n.popn + sum(n.components)
              if (updateInitialPopn)
                  prob.popn <- n.popn / (n.popn + sum(n.components))
              else
                  prob.popn <- -1
              cum.prob.comp <- cumsum(n.components) / sum(n.components)
              is.births <- sapply(components, methods::is, "Births")
              is.orig.dest <- sapply(components, methods::is, "HasOrigDest")
              is.par.ch <- sapply(components, methods::is, "HasParentChild")
              is.pool <- sapply(components, methods::is, "InternalMovementsPool")
              is.int.net <- sapply(components, methods::is, "InternalMovementsNet")
              is.net.move <- sapply(components, methods::is, "NetMovements")
              i.births <- if (any(is.births)) which(is.births) else -1L
              i.orig.dest <- if (any(is.orig.dest)) which(is.orig.dest) else -1L
              i.par.ch <- if (any(is.par.ch)) which(is.par.ch) else -1L
              i.pool <- if (any(is.pool)) which(is.pool) else -1L
              i.int.net <- if (any(is.int.net)) which(is.int.net) else -1L
              is.net <- is.int.net | is.net.move
              exposure <- dembase::exposure(population,
                                            triangles = has.age)
              exposure <- methods::new("Exposure",
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
              mappings.to.exp <- lapply(components, function(x) Mapping(x, exposure, dominant))
              mappings.to.popn <- lapply(components, function(x) Mapping(x, population))
              model.uses.exposure <- sapply(systemModels, function(x) x@useExpose@.Data)
              if ((i.births > 0L) && model.uses.exposure[i.births + 1L])
                  transform.exp.to.births <- makeTransformExpToBirths(exposure = exposure,
                                                                      births = components[[i.births]],
                                                                      dominant = dominant)
              else
                  transform.exp.to.births <- methods::new("CollapseTransform")
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
                  spec <- systemModels[[i]]
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
                      systemModels[[i]] <- initialModel(spec,
                                                        y = series,
                                                        exposure = expose)
                  }
                  else {
                      weights <- systemWeights[[i]]
                      if (is.null(weights)) {
                          uses.weights <- modelUsesWeights(spec)
                          if (uses.weights) {
                              .Data <- array(1, dim = dim(series), dimnames = dimnames(series))
                              metadata <- series@metadata
                              weights <- methods::new("Counts", .Data = .Data, metadata = metadata)
                              systemModels[[i]] <- initialModel(spec,
                                                                y = series,
                                                                weights = weights)
                          }
                          else {
                              systemModels[[i]] <- initialModel(spec,
                                                                y = series,
                                                                exposure = NULL)
                          }
                      }
                      else {
                          systemModels[[i]] <- initialModel(spec,
                                                            y = series,
                                                            weights = weights)
                      }
                  }
              }
              struc.zero.array <- systemModels[[1L]]@strucZeroArray@.Data
              if (any(struc.zero.array == 0L)) {
                  population@.Data[struc.zero.array == 0L] <- 0L
                  exposure@.Data <- dembase::exposure(population, triangles = has.age)@.Data
                  account@population <- population
              }
              for (i in seq_along(components)) {
                  sys.mod <- systemModels[[i + 1L]]
                  has.struc.zeros <- methods::is(sys.mod, "StrucZeroArrayMixin")
                  if (has.struc.zeros) {
                      struc.zero.array <- sys.mod@strucZeroArray@.Data
                      if (any(struc.zero.array == 0L))
                          account@components[[i]]@.Data[struc.zero.array == 0L] <- 0L
                  }
              }
              .Data.theta.popn <- array(systemModels[[1L]]@theta,
                                        dim = dim(population),
                                        dimnames = dimnames(population))
              metadata.theta.popn <- population@metadata
              theta.popn <- methods::new("Counts",
                                .Data = .Data.theta.popn,
                                metadata = metadata.theta.popn)
              expected.exposure <- dembase::exposure(theta.popn,
                                                     triangles = has.age)
              expected.exposure <- methods::new("Exposure",
                                       .Data = expected.exposure@.Data,
                                       metadata = expected.exposure@metadata)
              for (i in seq_along(dataModels)) {
                  series.index <- seriesIndices[i]
                  series <- if (series.index == 0L) population else components[[series.index]]
                  dataset <- datasets[[i]]
                  metadata.series.collapsed <- dataset@metadata
                  .Data.series.collapsed <- dembase::collapse(series@.Data,
                                                              transform = transforms[[i]])
                  dimnames(.Data.series.collapsed) <- dimnames(metadata.series.collapsed)
                  series.collapsed <- methods::new("Counts",
                                          .Data = .Data.series.collapsed,
                                          metadata = metadata.series.collapsed)
                  model <- dataModels[[i]]
                  if (methods::is(model, "Poisson") || methods::is(model, "CMP"))
                      series.collapsed <- dembase::toDouble(series.collapsed)
                  dataModels[[i]] <- initialModel(model,
                                                  y = dataset,
                                                  exposure = series.collapsed)
                  has.struc.zeros <- methods::is(model, "StrucZeroArrayMixin")
                  if (has.struc.zeros) {
                      struc.zero.array <- model@strucZeroArray@.Data
                      dataset@.Data[struc.zero.array == 0L] <- 0L
                      datasets[[i]] <- dataset
                  }
              }
              scaleNoise <- methods::new("Scale", as.double(scaleNoise))
              ## Create flags showing models or components are updated.
              ## At present this is used only for simulation, but in future
              ## we will probably allow for an 'identity' data model, which
              ## means that neither the data model nor the component is updated.
              update.system.model <- rep(TRUE, times = length(systemModels))
              update.data.model <- rep(TRUE, times = length(dataModels))              
              update.component <- rep(TRUE, times = length(components))
              if (has.age) {
                  accession <- dembase::accession(account,
                                                  births = FALSE,
                                                  openAge = TRUE)
                  accession <- methods::new("Accession",
                                            .Data = accession@.Data,
                                            metadata = accession@metadata)
                  iterator.acc <- CohortIterator(accession)
                  mappings.to.acc <- lapply(components, function(x) Mapping(x, accession))
                  methods::new("CombinedAccountMovementsHasAge",
                               accession = accession,
                               account = account,
                               ageTimeStep = age.time.step,
                               cumProbComp = cum.prob.comp,
                               dataModels = dataModels,
                               datasets = datasets,
                               descriptions = descriptions,
                               diffProp = NA_integer_,
                               exposure = exposure,
                               expectedExposure = expected.exposure,
                               generatedNewProposal = methods::new("LogicalFlag", FALSE),
                               hasAge = methods::new("LogicalFlag", TRUE),
                               iAccNext = NA_integer_,
                               iAccNextOther = NA_integer_,
                               iBirths = i.births,
                               iCell = NA_integer_,
                               iCellOther = NA_integer_,
                               iComp = -1L,
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
                               isLowerTriangle = methods::new("LogicalFlag", FALSE),
                               isNet = is.net,
                               isSmallUpdate = methods::new("LogicalFlag", FALSE),
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
                               probSmallUpdate = probSmallUpdate,
                               probPopn = prob.popn,
                               scaleNoise = scaleNoise,
                               seriesIndices = seriesIndices,
                               systemModels = systemModels,
                               transformExpToBirths = transform.exp.to.births,
                               transforms = transforms,
                               transformsExpToComp = transforms.exp.to.comp,
                               updateComponent = update.component,
                               updateDataModel = update.data.model,
                               updateSystemModel = update.system.model,
                               usePriorPopn = usePriorPopn)
              }
              else {
                  methods::new("CombinedAccountMovementsNoAge",
                               account = account,
                               ageTimeStep = age.time.step,
                               cumProbComp = cum.prob.comp,
                               dataModels = dataModels,
                               datasets = datasets,
                               descriptions = descriptions,
                               diffProp = NA_integer_,
                               exposure = exposure,
                               expectedExposure = expected.exposure,
                               generatedNewProposal = methods::new("LogicalFlag", FALSE),
                               hasAge = methods::new("LogicalFlag", FALSE),
                               iBirths = i.births,
                               iCell = NA_integer_,
                               iCellOther = NA_integer_,
                               iComp = -1L,
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
                               isSmallUpdate = methods::new("LogicalFlag", FALSE),
                               iteratorExposure = iterator.exposure,
                               iteratorPopn = iterator.popn,
                               iteratorsComp = iterators.comp,
                               mappingsFromExp = mappings.from.exp,
                               mappingsToExp = mappings.to.exp,
                               mappingsToPopn = mappings.to.popn,
                               modelUsesExposure = model.uses.exposure,
                               namesDatasets = namesDatasets,
                               nCellAccount = n.cell.account,
                               probPopn = prob.popn,
                               probSmallUpdate = probSmallUpdate,
                               scaleNoise = scaleNoise,
                               seriesIndices = seriesIndices,
                               systemModels = systemModels,
                               transformExpToBirths = transform.exp.to.births,
                               transforms = transforms,
                               transformsExpToComp = transforms.exp.to.comp,
                               updateComponent = update.component,
                               updateDataModel = update.data.model,
                               updateSystemModel = update.system.model,
                               usePriorPopn = usePriorPopn)
              }
          })


## HAS_TESTS
setMethod("initialCombinedAccountSimulate",
          signature(account = "Movements",
                    systemModels = "list",
                    systemWeights = "list",
                    dataModels = "list",
                    seriesIndices = "integer",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list",
                    updateDataModel = "logical",
                    updateSystemModel = "logical"),
          function(account, systemModels, systemWeights,
                   dataModels, seriesIndices, 
                   datasets, namesDatasets, transforms,
                   dominant = c("Female", "Male"),
                   updateSystemModel, updateDataModel,
                   updateInitialPopn,
                   usePriorPopn, probSmallUpdate = 0,
                   scaleNoise = 0) {
              combined <- initialCombinedAccount(account = account,
                                                 systemModels = systemModels,
                                                 systemWeights = systemWeights,
                                                 dataModels = dataModels,
                                                 seriesIndices = seriesIndices, 
                                                 datasets = datasets,
                                                 namesDatasets = namesDatasets,
                                                 transforms = transforms,
                                                 dominant = dominant,
                                                 updateInitialPopn = updateInitialPopn,
                                                 usePriorPopn = usePriorPopn,
                                                 probSmallUpdate = probSmallUpdate,
                                                 scaleNoise = scaleNoise)
              combined <- setDatasetsToMissing(combined)
              combined <- drawCombined(combined, useC = TRUE)
              combined@updateDataModel <- updateDataModel
              combined@updateSystemModel <- updateSystemModel
              combined
          })

