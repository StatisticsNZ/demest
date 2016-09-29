
## Internal functions for extracting information via a Results object

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
              observation <- combined@observation
              names.datasets <- combined@namesDatasets
              ans.model <- finiteSDInner(filename = filename,
                                         model = model,
                                         where = "model",
                                         probs = probs,
                                         iterations = iterations)
              ans.observation <- vector(mode = "list", length = length(observation))
              names(ans.observation) <- names.datasets
              for (i in seq_along(ans.observation)) {
                  where <- c("observation", names.datasets[i])
                  ans.observation[i] <- list(finiteSDInner(filename = filename,
                                                           model = observation[[i]],
                                                           where = where,
                                                           probs = probs,
                                                           iterations = iterations))
              }
              list(model = ans.model,
                   observation = ans.observation)
          })
    
## HAS_TESTS
setMethod("nIteration",
          signature(object = "Results"),
          function(object) {
              mcmc <- object@mcmc
              mcmc[["nIteration"]]
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
              observation <- final@observation
              names.datasets <- final@namesDatasets
              kDivider <- paste(paste(rep("-", times = 50), collapse = ""), "\n", collapse = "")
              cat(kDivider)
              cat("model:\n\n")
              showModelHelper(model)
              cat(kDivider)
              n.dataset <- length(observation)
              cat("Observation models:\n\n")
              for (i in seq_len(n.dataset)) {
                  cat(names.datasets[i], ":\n", sep = "")
                  obs <- observation[[i]]
                  showModelHelper(observation[[i]])
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
              observation <- final@observation
              names.datasets <- final@namesDatasets
              ans.model <- FUN(model)
              if (identical(ans.model, list(NULL)))
                  ans.model <- NULL
              else {
                  for (i in seq_along(ans.model))
                      ans.model[[i]] <- c("model", ans.model[[i]])
              }
              ans.observation <- lapply(observation, FUN)
              is.null <- sapply(ans.observation,
                                function(x) identical(x, list(NULL)))
              if (all(is.null))
                  ans.observation <- NULL
              else {
                  ans.observation <- ans.observation[!is.null]
                  names <- names.datasets[!is.null]
                  times <- sapply(ans.observation, length)
                  names <- rep(names, times = times)
                  ans.observation <- unlist(ans.observation, recursive = FALSE)
                  for (i in seq_along(ans.observation))
                      ans.observation[[i]] <- c("observation",
                                                names[i],
                                                ans.observation[[i]])
              }
              c(ans.model, ans.observation)
          })
