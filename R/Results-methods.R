
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
              observationModels <- combined@observationModels
              names.datasets <- combined@namesDatasets
              ans.model <- finiteSDInner(filename = filename,
                                         model = model,
                                         where = "model",
                                         probs = probs,
                                         iterations = iterations)
              ans.observationModels <- vector(mode = "list", length = length(observationModels))
              names(ans.observationModels) <- names.datasets
              for (i in seq_along(ans.observationModels)) {
                  where <- c("observationModels", names.datasets[i])
                  ans.observationModels[i] <- list(finiteSDInner(filename = filename,
                                                           model = observationModels[[i]],
                                                           where = where,
                                                           probs = probs,
                                                           iterations = iterations))
              }
              list(model = ans.model,
                   observationModels = ans.observationModels)
          })
    
## HAS_TESTS
setMethod("nIteration",
          signature(object = "Results"),
          function(object) {
              mcmc <- object@mcmc
              mcmc[["nIteration"]]
          })          

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
              for (i in seq_along(results@final[[1L]]@observationModels)) {
                  if (methods::is(results@final[[1L]]@observationModels[[i]], "Varying")) {
                      priors <- results@final[[1L]]@observationModels[[i]]@priorsBetas
                      margins <- results@final[[1L]]@observationModels[[i]]@margins
                      skeletons.betas <- results@observationModels[[i]]$prior[seq_along(priors)]
                      skeletons.priors <- results@observationModels[[i]]$hyper
                      name.dataset <- results@final[[1L]]@namesDatasets[i]
                      prefix.adjustments <- paste("observation", name.dataset, sep = ".")
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
              observationModels <- final@observationModels
              names.datasets <- final@namesDatasets
              kDivider <- paste(paste(rep("-", times = 50), collapse = ""), "\n", collapse = "")
              cat(kDivider)
              cat("model:\n\n")
              showModelHelper(model)
              cat(kDivider)
              n.dataset <- length(observationModels)
              cat("Observation models:\n\n")
              for (i in seq_len(n.dataset)) {
                  cat(names.datasets[i], ":\n", sep = "")
                  obs <- observationModels[[i]]
                  showModelHelper(observationModels[[i]])
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
              observationModels <- final@observationModels
              names.datasets <- final@namesDatasets
              ans.model <- FUN(model)
              if (identical(ans.model, list(NULL)))
                  ans.model <- NULL
              else {
                  for (i in seq_along(ans.model))
                      ans.model[[i]] <- c("model", ans.model[[i]])
              }
              ans.observationModels <- lapply(observationModels, FUN)
              is.null <- sapply(ans.observationModels,
                                function(x) identical(x, list(NULL)))
              if (all(is.null))
                  ans.observationModels <- NULL
              else {
                  ans.observationModels <- ans.observationModels[!is.null]
                  names <- names.datasets[!is.null]
                  times <- sapply(ans.observationModels, length)
                  names <- rep(names, times = times)
                  ans.observationModels <- unlist(ans.observationModels, recursive = FALSE)
                  for (i in seq_along(ans.observationModels))
                      ans.observationModels[[i]] <- c("observationModels",
                                                names[i],
                                                ans.observationModels[[i]])
              }
              c(ans.model, ans.observationModels)
          })
