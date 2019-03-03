
## HAS_TESTS
setMethod("finiteSDObject",
          signature(object = "ResultsModelEst"),
          function(object, filename, probs = c(0.025, 0.5, 0.975), iterations = NULL) {
              if (identical(dembase::nIteration(object), 0L))
                  return(NULL)
              model <- object@final[[1L]]@model
              finiteSDInner(filename = filename,
                            model = model,
                            where = "model",
                            probs = probs,
                            iterations = iterations)
          })

## HAS_TESTS
setMethod("finiteSDObject",
          signature(object = "ResultsCountsEst"),
          function(object, filename, probs = c(0.025, 0.5, 0.975), iterations = NULL) {
              if (identical(dembase::nIteration(object), 0L))
                  return(NULL)
              combined <- object@final[[1L]]
              model <- combined@model
              dataModels <- combined@dataModels
              names.datasets <- combined@namesDatasets
              ans.model <- finiteSDInner(filename = filename,
                                         model = model,
                                         where = "model",
                                         probs = probs,
                                         iterations = iterations)
              ans.dataModels <- vector(mode = "list", length = length(dataModels))
              names(ans.dataModels) <- names.datasets
              for (i in seq_along(ans.dataModels)) {
                  where <- c("dataModels", names.datasets[i])
                  ans.dataModels[i] <- list(finiteSDInner(filename = filename,
                                                           model = dataModels[[i]],
                                                           where = where,
                                                           probs = probs,
                                                           iterations = iterations))
              }
              list(model = ans.model,
                   dataModels = ans.dataModels)
          })


## makeResults #####################################################################

setMethod("makeResults",
          signature(object = "ResultsModelEst"),
          function(object, finalCombineds, mcmcArgs, controlArgs, seed) {
              makeResultsModelEst(finalCombineds = finalCombineds,
                                  mcmcArgs = mcmcArgs,
                                  controlArgs = controlArgs,
                                  seed = seed)
          })

setMethod("makeResults",
          signature(object = "ResultsCountsEst"),
          function(object, finalCombineds, mcmcArgs, controlArgs, seed) {
              makeResultsCounts(finalCombineds = finalCombineds,
                                mcmcArgs = mcmcArgs,
                                controlArgs = controlArgs,
                                seed = seed)
          })

setMethod("makeResults",
          signature(object = "ResultsAccount"),
          function(object, finalCombineds, mcmcArgs, controlArgs, seed) {
              makeResultsAccount(finalCombineds = finalCombineds,
                                 mcmcArgs = mcmcArgs,
                                 controlArgs = controlArgs,
                                 seed = seed)
          })
          
              


## nIteration #################################################################

## HAS_TESTS
setMethod("nIteration",
          signature(object = "Results"),
          function(object) {
              mcmc <- object@mcmc
              mcmc[["nIteration"]]
          })


## rescaleBetasPred #################################################################

## HAS_TESTS
setMethod("rescaleBetasPred",
          signature(results = "ResultsModelPred"),
          function(results, adjustments, filename, nIteration, lengthIter) {
              priorsBetas <- results@final[[1L]]@model@priorsBetas
              namesBetas <- results@final[[1L]]@model@namesBetas
              skeletonsBetas <- results@model$prior[seq_along(priorsBetas)] # omit mean, sd
              rescaleBetasPredHelper(priorsBetas = priorsBetas,
                                     namesBetas = namesBetas,
                                     skeletonsBetas = skeletonsBetas,
                                     adjustments = adjustments,
                                     prefixAdjustments = "model",
                                     filename = filename,
                                     nIteration = nIteration,
                                     lengthIter = lengthIter)
          })



## rescalePriors #################################################################

## HAS_TESTS
setMethod("rescalePriors",
          signature(results = "ResultsModelEst"),
          function(results, adjustments, filename, nIteration, lengthIter) {
              priors <- results@final[[1L]]@model@priorsBetas
              margins <- results@final[[1L]]@model@margins
              skeletons.betas <- results@model$prior[seq_along(priors)] # omit mean, sd
              skeletons.priors <- results@model$hyper
              rescalePriorsHelper(priors = priors,
                                  margins = margins,
                                  skeletonsBetas = skeletons.betas,
                                  skeletonsPriors = skeletons.priors,
                                  adjustments = adjustments,
                                  prefixAdjustments = "model",
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
          })

## HAS_TESTS
setMethod("rescalePriors",
          signature(results = "ResultsModelSimDirect"),
          function(results, adjustments, filename, nIteration, lengthIter) {
              priors <- results@final[[1L]]@model@priorsBetas
              margins <- results@final[[1L]]@model@margins
              skeletons.betas <- results@model$prior[seq_along(priors)] # omit mean, sd
              skeletons.priors <- results@model$hyper
              rescalePriorsHelper(priors = priors,
                                  margins = margins,
                                  skeletonsBetas = skeletons.betas,
                                  skeletonsPriors = skeletons.priors,
                                  adjustments = adjustments,
                                  prefixAdjustments = "model",
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
          })



## HAS_TESTS
setMethod("rescalePriors",
          signature(results = "ResultsCountsEst"),
          function(results, adjustments, filename, nIteration, lengthIter) {
              priors <- results@final[[1L]]@model@priorsBetas
              margins <- results@final[[1L]]@model@margins
              skeletons.betas <- results@model$prior[seq_along(priors)]
              skeletons.priors <- results@model$hyper
              prefix.adjustments <- "model"
              rescalePriorsHelper(priors = priors,
                                  margins = margins,
                                  skeletonsBetas = skeletons.betas,
                                  skeletonsPriors = skeletons.priors,
                                  adjustments = adjustments,
                                  prefixAdjustments = prefix.adjustments,
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
              for (i in seq_along(results@final[[1L]]@dataModels)) {
                  if (methods::is(results@final[[1L]]@dataModels[[i]], "Varying")) {
                      priors <- results@final[[1L]]@dataModels[[i]]@priorsBetas
                      margins <- results@final[[1L]]@dataModels[[i]]@margins
                      skeletons.betas <- results@dataModels[[i]]$prior[seq_along(priors)]
                      skeletons.priors <- results@dataModels[[i]]$hyper
                      name.dataset <- results@final[[1L]]@namesDatasets[i]
                      prefix.adjustments <- paste("dataModels", name.dataset, sep = ".")
                      rescalePriorsHelper(priors = priors,
                                          margins = margins,
                                          skeletonsBetas = skeletons.betas,
                                          skeletonsPriors = skeletons.priors,
                                          adjustments = adjustments,
                                          prefixAdjustments = prefix.adjustments,
                                          filename = filename,
                                          nIteration = nIteration,
                                          lengthIter = lengthIter)
                  }
              }
          })

## NO_TESTS
setMethod("rescalePriors",
          signature(results = "ResultsAccount"),
          function(results, adjustments, filename, nIteration, lengthIter) {
              combined <- results@final[[1L]]
              system.models.obj <- combined@systemModels
              system.models.out <- results@systemModels
              data.models.obj <- combined@dataModels
              data.models.out <- results@dataModels
              names.components <- combined@account@namesComponents
              names.series <- c("population", names.components)
              names.datasets <- combined@namesDatasets
              for (i in seq_along(system.models.obj)) {
                  model.obj <- system.models.obj[[i]]
                  model.out <- system.models.out[[i]]
                  if (methods::is(model.obj, "Varying")) {
                      priors <- model.obj@priorsBetas
                      margins <- model.obj@margins
                      skeletons.betas <- model.out$prior[seq_along(priors)]
                      skeletons.priors <- model.out$hyper
                      name.series <- names.series[i]
                      prefix.adjustments <- paste("systemModels", name.series, sep = ".")
                      rescalePriorsHelper(priors = priors,
                                          margins = margins,
                                          skeletonsBetas = skeletons.betas,
                                          skeletonsPriors = skeletons.priors,
                                          adjustments = adjustments,
                                          prefixAdjustments = prefix.adjustments,
                                          filename = filename,
                                          nIteration = nIteration,
                                          lengthIter = lengthIter)
                  }
              }
              for (i in seq_along(data.models.obj)) {
                  model.obj <- data.models.obj[[i]]
                  model.out <- data.models.out[[i]]
                  if (methods::is(model.obj, "Varying")) {
                      priors <- model.obj@priorsBetas
                      margins <- model.obj@margins
                      skeletons.betas <- model.out$prior[seq_along(priors)]
                      skeletons.priors <- model.out$hyper
                      name.dataset <- names.datasets[i]
                      prefix.adjustments <- paste("dataModels", name.dataset, sep = ".")
                      rescalePriorsHelper(priors = priors,
                                          margins = margins,
                                          skeletonsBetas = skeletons.betas,
                                          skeletonsPriors = skeletons.priors,
                                          adjustments = adjustments,
                                          prefixAdjustments = prefix.adjustments,
                                          filename = filename,
                                          nIteration = nIteration,
                                          lengthIter = lengthIter)
                  }
              }
          })





## NO_TESTS
setMethod("showModelHelper",
          signature(object = "ResultsModel"),
          function(object) {
              object <- object@final[[1L]]@model
              methods::callGeneric()
          })

## NO_TESTS
setMethod("showModelHelper",
          signature(object = "ResultsCountsEst"),
          function(object) {
              final <- object@final[[1L]]
              model <- final@model
              dataModels <- final@dataModels
              names.datasets <- final@namesDatasets
              kDivider <- paste(paste(rep("-", times = 50), collapse = ""), "\n", collapse = "")
              cat(kDivider)
              cat("model:\n\n")
              showModelHelper(model)
              cat(kDivider)
              n.dataset <- length(dataModels)
              cat("Data models:\n\n")
              for (i in seq_len(n.dataset)) {
                  cat(names.datasets[i], ":\n", sep = "")
                  obs <- dataModels[[i]]
                  showModelHelper(dataModels[[i]])
                  if (i < n.dataset)
                      cat("\n")
              }
              cat(kDivider)
          })

setMethod("whereFiniteSD",
          signature(object = "ResultsModelEst"),
          function(object) {
              model <- object@final[[1L]]@model
              whereFiniteSD(model)
          })

## HAS_TESTS
setMethod("whereMetropStat",
          signature(object = "ResultsModel"),
          function(object, FUN) {
              final <- object@final[[1L]]
              model <- final@model
              ans <- FUN(model)
              for (i in seq_along(ans)) {
                  if (!is.null(ans[[i]]))
                      ans[[i]] <- c("model", ans[[i]])
              }
              ans
          })
    
## HAS_TESTS
setMethod("whereMetropStat",
          signature(object = "ResultsCountsEst"),
          function(object, FUN) {
              final <- object@final[[1L]]
              model <- final@model
              y <- final@y
              dataModels <- final@dataModels
              names.datasets <- final@namesDatasets
              ans.model <- FUN(model)
              if (identical(ans.model, list(NULL)))
                  ans.model <- NULL
              else {
                  for (i in seq_along(ans.model))
                      ans.model[[i]] <- c("model", ans.model[[i]])
              }
              ans.y <- FUN(y)
              if (identical(ans.y, list(NULL)))
                  ans.y <- NULL
              ans.dataModels <- lapply(dataModels, FUN)
              is.null <- sapply(ans.dataModels,
                                function(x) identical(x, list(NULL)))
              if (all(is.null))
                  ans.dataModels <- NULL
              else {
                  ans.dataModels <- ans.dataModels[!is.null]
                  names <- names.datasets[!is.null]
                  times <- sapply(ans.dataModels, length)
                  names <- rep(names, times = times)
                  ans.dataModels <- unlist(ans.dataModels, recursive = FALSE)
                  for (i in seq_along(ans.dataModels))
                      ans.dataModels[[i]] <- c("dataModels",
                                                names[i],
                                                ans.dataModels[[i]])
              }
              c(ans.model, ans.y, ans.dataModels)
          })


setMethod("whereMetropStat",
          signature(object = "ResultsAccount"),
          function(object, FUN) {
              final <- object@final[[1L]]
              account <- final@account
              system.models <- final@systemModels
              data.models <- final@dataModels
              names.series <- c("population", account@namesComponents)
              names.datasets <- final@namesDatasets
              ans.account <- FUN(account)
              if (identical(ans.account, list(NULL)))
                  ans.account <- NULL
              else {
                  ans.account <- lapply(ans.account[[1L]],
                                        function(x) c("account", x))
              }
              ans.system.models <- lapply(system.models, FUN)
              is.null <- sapply(ans.system.models,
                                function(x) identical(x, list(NULL)))
              if (all(is.null))
                  ans.system.models <- NULL
              else {
                  ans.system.models <- ans.system.models[!is.null]
                  names <- names.series[!is.null]
                  times <- sapply(ans.system.models, length)
                  names <- rep(names, times = times)
                  ans.system.models <- unlist(ans.system.models, recursive = FALSE)
                  for (i in seq_along(ans.system.models))
                      ans.system.models[[i]] <- c("systemModels",
                                                  names[i],
                                                  ans.system.models[[i]])
              }
              ans.data.models <- lapply(data.models, FUN)
              is.null <- sapply(ans.data.models,
                                function(x) identical(x, list(NULL)))
              if (all(is.null))
                  ans.data.models <- NULL
              else {
                  ans.data.models <- ans.data.models[!is.null]
                  names <- names.datasets[!is.null]
                  times <- sapply(ans.data.models, length)
                  names <- rep(names, times = times)
                  ans.data.models <- unlist(ans.data.models, recursive = FALSE)
                  for (i in seq_along(ans.data.models))
                      ans.data.models[[i]] <- c("dataModels",
                                                names[i],
                                                ans.data.models[[i]])
              }
              c(ans.account, ans.system.models, ans.data.models)
          })

