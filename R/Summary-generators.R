


## summary ###########################################################################

## HAS_TESTS
setMethod("summary",
          signature(object = "Model"),
          function(object) {
              call <- object@call
              metadata <- object@metadataY
              specification <- unname(sapply(call[-1L], deparse))
              specification <- lapply(specification, paste, collapse = "\n")
              specification <- Reduce(function(x, y) paste(x, y, sep = ",\n"),
                                      specification)
              dimensions <- names(metadata)
              methods::new("SummaryModel",
                  specification = specification,
                  dimensions = dimensions)
          })

## HAS_TESTS
setMethod("summary",
          signature(object = "SkeletonManyCounts"),
          function(object) {
              metadata <- object@metadata
              first <- object@first
              last <- object@last
              dimensions <- names(metadata)
              nCell <- last - first + 1L
              methods::new("SummarySeries",
                  dimensions = dimensions,
                  nCell = nCell)
          })

## HAS_TESTS
setMethod("summary",
          signature(object = "ResultsModelEst"),
          function(object, filename, iteration = NULL) {
              y <- object@y
              mcmc <- object@mcmc
              final <- object@final[[1L]]
              model <- final@model
              parameters <- makeParameters(object = object, filename = filename)
              gelman.diag <- makeGelmanDiag(object = object, filename = filename)
              metropolis <- makeMetropolis(object = object, filename = filename)
              summary.model <- summary(model)
              summary.y <- summaryDataset(y)
              methods::new("SummaryResultsModelEst",
                           mcmc = mcmc,
                           parameters = parameters,
                           gelmanDiag = gelman.diag,
                           metropolis = metropolis,
                           model = summary.model,
                           y = summary.y)
          })

## NO_TESTS
setMethod("summary",
          signature(object = "ResultsModelPred"),
          function(object, filename, iteration = NULL) {
              mcmc <- object@mcmc
              final <- object@final[[1L]]
              model <- final@model
              parameters <- makeParameters(object = object, filename = filename)
              summary.model <- summary(model)
              methods::new("SummaryResultsModelPred",
                           mcmc = mcmc,
                           parameters = parameters,
                           model = summary.model)
          })

## HAS_TESTS
setMethod("summary",
          signature(object = "ResultsCountsEst"),
          function(object, filename, iteration = NULL) {
              mcmc <- object@mcmc
              final <- object@final[[1L]]
              model <- final@model
              y <- object@y
              dataModels <- final@dataModels
              datasets <- final@datasets
              names.datasets <- final@namesDatasets
              parameters <- makeParameters(object = object, filename = filename)
              gelman.diag <- makeGelmanDiag(object = object, filename = filename)
              metropolis <- makeMetropolis(object = object, filename = filename)
              model.summary <- summary(model)
              y.summary <- summary(y)
              dataModels.summary <- lapply(dataModels, summary)
              datasets.summary <- lapply(datasets, summaryDataset)
              methods::new("SummaryResultsCounts",
                           mcmc = mcmc,
                           parameters = parameters,
                           gelmanDiag = gelman.diag,
                           metropolis = metropolis,
                           model = model.summary,
                           y = y.summary,
                           dataModels = dataModels.summary,
                           datasets = datasets.summary,
                           namesDatasets = names.datasets)
          })



## summaryDataset #########################################################

## Can't use method for summary because clashes with method from DemographicArray
## HAS_TESTS
setMethod("summaryDataset",
          signature(object = "DemographicArray"),
          function(object) {
              classStr <- as.character(class(object))
              dimensions <- names(object)
              nCell <- length(object)
              nMissing <- sum(is.na(object))
              all.missing <- nMissing == nCell
              if (all.missing) {
                  isIntegers <- NA
                  median <- as.numeric(NA)
              }
              else {
                  isIntegers <- is.integer(object) || all(round(object) == object)
                  median <- as.numeric(median(object, na.rm = TRUE))
              }
              if (isTRUE(isIntegers))
                  nZero <- sum(object == 0L, na.rm = TRUE)
              else
                  nZero <- as.integer(NA)
              methods::new("SummaryDataset",
                  classStr = classStr,
                  dimensions = dimensions,
                  nCell = nCell,
                  nMissing = nMissing,
                  isIntegers = isIntegers,
                  nZero = nZero,
                  median = median)
          })

## HAS_TESTS
setMethod("summaryDataset",
          signature(object = "SkeletonMissingData"),
          function(object) {
              object <- object@data
              methods::callGeneric()
          })
