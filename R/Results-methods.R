
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
    
## HAS_TESTS
setMethod("nIteration",
          signature(object = "Results"),
          function(object) {
              mcmc <- object@mcmc
              mcmc[["nIteration"]]
          })          

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
              dataModels <- final@dataModels
              names.datasets <- final@namesDatasets
              ans.model <- FUN(model)
              if (identical(ans.model, list(NULL)))
                  ans.model <- NULL
              else {
                  for (i in seq_along(ans.model))
                      ans.model[[i]] <- c("model", ans.model[[i]])
              }
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
              c(ans.model, ans.dataModels)
          })
