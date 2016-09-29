
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
                    observation = "list",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(object, y, exposure, observation, datasets,
                   namesDatasets, transforms) {
              y <- imputeCountsInternal(y)
              for (i in seq_along(observation)) {
                  y.collapsed <- dembase::collapse(y, transform = transforms[[i]])
                  observation[[i]] <- initialModel(observation[[i]],
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
                      observation = observation,
                      datasets = datasets,
                      namesDatasets = namesDatasets,
                      transforms = transforms)
              }
              else {
                  model <- initialModel(object, y = y, exposure = exposure)
                  methods::new("CombinedCountsPoissonNotHasExp",
                      model = model,
                      y = y,
                      observation = observation,
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
                    observation = "list",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(object, y, exposure, observation, datasets,
                   namesDatasets, transforms) {
              if (is.null(exposure))
                  stop(gettextf("binomial model, but no '%s' argument supplied",
                                "exposure"))
              y <- imputeCountsInternal(y, max = exposure)
              if (any(y[!is.na(y)] > exposure[!is.na(y)]))
                  stop(gettextf("'%s' greater than '%s'",
                                "y", "exposure"))
              for (i in seq_along(observation)) {
                  y.collapsed <- dembase::collapse(y, transform = transforms[[i]])
                  observation[[i]] <- initialModel(observation[[i]],
                                                   y = datasets[[i]],
                                                   exposure = y.collapsed)
              }
              model <- initialModel(object, y = y, exposure = exposure)
              methods::new("CombinedCountsBinomial",
                           model = model,
                           y = y,
                           exposure = exposure,
                           observation = observation,
                           datasets = datasets,
                           namesDatasets = namesDatasets,
                           transforms = transforms)
          })






## COMBINED ACCOUNT ###################################################################
