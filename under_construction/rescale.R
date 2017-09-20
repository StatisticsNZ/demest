
## HAS_TESTS
makeResultsFile <- function(filename, results, tempfiles) {
    kLength <- 10000
    con.write <- file(filename, open = "wb")
    on.exit(close(con.write))
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con = con.write)
    writeBin(results, con = con.write)
    for (i in seq_along(tempfiles)) {
        con.read <- file(tempfiles[i], "rb")
        finished <- FALSE
        while (!finished) {
            object <- readBin(con = con.read, what = "double", n = kLength)
            writeBin(object, con = con.write)
            finished <- length(object) < kLength
        }
        close(con.read)
        unlink(tempfiles[i])
    }
    rescalePriors(filename = filename)
    NULL
}

rescalePriorsInFile <- function(filename) {
    results <- fetchResultsObject(filename)
    nIteration <- results@mcmc["nIteration"]
    lengthIter <- results@control$lengthIter
<<<<<<< HEAD
    adjustments <- new.env(hash = TRUE) # modified in-place
    rescalePriors(results = results,
                  adjustments = adjustments,
                  filename = filename,
                  nIteration = nIteration,
                  lengthIter = lengthIter)
    addAdjustmentsToFile(adjustments = adjustments,
                         filename = filename,
                         nIteration = nIteration,
                         lengthIter = lengthIter)
=======
    rescalePriors(results = results,
                  filename = filename,
                  nIteration = nIteration,
                  lengthIter = lengthIter)
>>>>>>> master
}

setMethod("rescalePriors",
          signature(results = "ResultsModelEst"),
<<<<<<< HEAD
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
=======
          function(results, filename, nIteration, lengthIter) {
              priors <- results@final[[1L]]@model@priorsBetas
              skeletons.betas <- results@model$prior[seq_along(priors)]
              skeletons.priors <- results@model$hyper
              margins <- results@model@margins
              rescalePriorsHelper(priors = priors,
                                  skeletonsBetas = skeletons.betas,
                                  skeletonsPriors = skeletons.priors,
                                  margins = margins,
>>>>>>> master
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
          })


setMethod("rescalePriors",
<<<<<<< HEAD
          signature(results = "ResultsCountsEst"),
          function(results, adjustments, filename, nIteration, lengthIter) {
              priors <- results@final[[1L]]@model@priorsBetas
              margins <- results@final[[1L]]@model@margins
              skeletons.betas <- results@model$prior[seq_along(priors)]
              skeletons.priors <- results@model$hyper
              prefix.adjustments <- "model"
              rescalePriorsHelper(priors = priors,
                                  margins = margins,
                                  skeletonsBetas = skeleton.betas,
                                  skeletonsPriors = skeleton.priors,
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
                      name.dataset <- namesDatasets[i]
                      prefix.adjustments <- paste("observation", name.dataset, collapse = ".")
                      rescalePriorsHelper(priors = priors,
                                          margins = margins,
                                          skeletonsBetas = skeletons.betas,
                                          skeletonsPriors = skeletons.priors,
                                          adjustments = adjustments,
                                          prefixAdjustments = prefix.adjustments,
=======
          signature(results = "ResultsCounts"),
          function(results, filename) {
              final <- object@final[[1L]]
              model <- final@model
              observation <- final@observation
              priors <- final@model@priorsBetas
              skeletons.betas <- model$prior
              skeletons.priors <- model$hyper
              margins <- final@model@margins
              rescalePriorsHelper(priors = priors,
                                  skeletonsBetas = skeletons.betas,
                                  skeletonsPriors = skeletons.priors,
                                  margins = margins,
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
              for (i in seq_along(observation)) {
                  if (methods::is(final@observation[[i]], "Varying")) {
                      priors <- final@observation[[i]]@priorsBetas
                      skeletons.betas <- observation[[i]]$prior
                      skeletons.priors <- observation[[i]]$hyper
                      margins <- finale@observation[[i]]@margins
                      rescalePriorsHelper(priors = priors,
                                          skeletonsBetas = skeletons.betas,
                                          skeletonsPriors = skeletons.priors,
                                          margins = margins,
>>>>>>> master
                                          filename = filename,
                                          nIteration = nIteration,
                                          lengthIter = lengthIter)
                  }
<<<<<<< HEAD
              }
          })



rescalePriorsHelper <- function(priors, margins, skeletonsBetas skeletonsPriors,
                                adjustments, prefixAdjustments,
                                filename, nIteration, lengthIter) {
    for (i in seq_along(priors)) {
        rescaleSeason(prior = priors[[i]],
                      skeleton = skeletonsPriors[[i]],
                      filename = filename,
                      nIteration = nIteration,
                      lengthIter = lengthIter)
    }
=======
              }                  
          })

makePairsTerms <- function(margins) {
    margins <- margins[-1L] # intercept
    n <- length(margins)
    ans <- vector(mode = "list", length = n * (n - 1L) / 2L)
    i <- 1L
    for (j in seq.int(from = n, to = 2L)) {
        for (k in seq.int(from = j - 1L, to = 1L)) {
            first <- margins[[j]]
            second <- margins[[k]]
            first.is.higher.order <- length(first) > length(second)
            first.and.second.share.term <- any(second %in% first)
            if (first.is.higher.order && first.and.second.share.term)
                ans[[i]] <- c(j, k)
            else
                ans[[i]] <- NULL
            i <- i + 1L
        }
    }
    is.null <- sapply(ans, is.null)
    ans <- ans[!is.null]
    ans
}


rescalePriorsHelper <- function(priors, skeletonsBetas skeletonsPriors, margins,
                                filename, nIteration, lengthIter) {
>>>>>>> master
    pairs.terms <- makePairsTerms(margins)
    for (pair.terms in pairs.terms) {
        i.higher <- pair.terms[1L]
        i.lower <- pair.terms[2L]
        prior.higher <- priors[[i.higher]]
        prior.lower <- priors[[i.lower]]
        skeleton.beta.higher <- skeletonsBetas[[i.higher]]
        skeleton.beta.lower <- skeletonsBetas[[i.lower]]
        skeletons.prior.higher <- skeletonsPriors[[i.higher]]
        skeletons.prior.lower <- skeletonsPriors[[i.lower]]
<<<<<<< HEAD
        rescalePairPriors(priorHigh = prior.higher,
                          priorLow = prior.lower,
                          skeletonBetaHigh = skeleton.beta.higher,
                          skeletonBetaLow = skeleton.beta.lower,
                          skeletonsPriorHigh = skeletons.prior.higher,
                          skeletonsPriorLow = skeletons.prior.lower,
                          adjustments = adjustments,
                          prefixAdjustments = prefixAdjustments,
=======
        rescalePairPriors(priorHigher = prior.higher,
                          priorLower = prior.lower,
                          skeletonBetaHigher = skeleton.beta.higher,
                          skeletonBetaLower = skeleton.beta.lower,
                          skeletonsPriorHigher = skeletons.prior.higher,
                          skeletonsPriorLower = skeletons.prior.lower,
>>>>>>> master
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    }
    skeletons.beta.intercept <- skeletonsBetas[[1L]]
<<<<<<< HEAD
    adj <- 0
=======
>>>>>>> master
    for (i in seq_along(priors[-1L])) {
        prior.term <- priors[[i]]
        skeleton.beta.term <- skeletonsBetas[[i]]
        skeletons.prior.term <- skeletonsPriors[[i]]
        rescalePriorIntercept(priorTerm = prior.term,
                              skeletonBetaTerm = skeleton.beta.term,
                              skeletonBetaIntercept = skeleton.beta.intercept,
                              skeletonsPriorTerm = skeletons.prior.term,
<<<<<<< HEAD
                              adjustments = adjustments,
                              prefixAdjustments = prefixAdjustments,
=======
>>>>>>> master
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)        
    }
}

<<<<<<< HEAD



setGeneric("rescaleSeason",
           function(prior, skeleton, filename, nIteration, lengthIter) {
               NULL
           })

setMethod("rescaleSeason",
          signature(prior = "Season"),
          function(prior, skeleton, filename, nIteration, lengthIter) {
              skeleton.level <- skeleton$level
              skeleton.season <- skeleton$season
              season <- readStateDLMFromFile(object = skeleton.season,
                                             filename = filename,
                                             iterations = NULL,
                                             nIteration = nIteration,
                                             lengthIter = lengthIter,
                                             only0 = FALSE)
              season.0 <- readStateDLMFromFile(skeleton = skeleton.season,
                                               filename = filename,
                                               iterations = NULL,
                                               nIteration = nIteration,
                                               lengthIter = lengthIter,
                                               only0 = TRUE)
              means <- collapseDimension(season.0,
                                         dimension = i.along,
                                         weights = 1)
              season <- season - means
              level <- level + means
              overwriteValuesOnFile(object = level,
                                    skeleton = skeleton.level,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              overwriteValuesOnFile(object = level,
                                    skeleton = skeleton.season,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              NULL
          })





setGeneric("rescalePairPriors",
          function(priorHigh, priorLow, skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              NULL
          })


              
setMethod("rescalePairPriors",
          signature(priorHigh = "Exchangeable",
                    priorLow = "Exchangeable"),
          function(priorHigh, priorLow, skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              if (!all(names.low %in% names.high))
                  return(NULL)
              beta.high <- fetchResults(object = skeletonBetaHigh,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration
                                        lengthIter = lengthIter)
              beta.low <- fetchResults(object = skeletonBetaLow,
                                       filename = filename,
                                       iterations = NULL,
                                       nIteration = nIteration
                                       lengthIter = lengthIter)
              names.high.only <- setdiff(names.high, names.low)
              means.shared <- collapseDimension(beta.high,
                                                dimension = names.high.only,
                                                weights = 1)
              rescaleAndWriteBetas(high = beta.high,
                                   low = beta.low,
                                   adj = means.shared,
                                   skeletonHigh = skeletonBetaHigh,
                                   skeletonLow = skeletonBetaHigh,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              recordAdjustments(priorHigh = priorHigh,
                                priorLow = priorLow,
                                namesHigh = names.high,
                                namesLow = names.low,
                                adj = means.shared,
                                adjustments = adjustments,
                                prefixAdjustments = prefixAdjustments)
              NULL
          })



## need to add argument to fetchResults - along0 = FALSE
## add similar argument to writeTermToFile
## adjust SkeletonStateDLM accordingly


setMethod("rescalePairPriors",
          signature(priorHigh = "Exchangeable",
                    priorLow = "DLM"),
          function(priorHigh, priorLow,
                   skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              i.along.low <- priorLow@iAlong
              phi.low <- priorLow@phi
              phi.known.low <- priorLow@phiKnown@.Data
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              skeleton.level.low <- skeletonsPriorLow$level
              has.trend.low <- methods::is(priorLow, "WithTrendMixin")
              level.non.stationary <- has.trend.low || (phi.known.low && isTRUE(all.equal(phi.low, 1)))
              if (!level.non.stationary)
                  return(NULL)
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              names.low <- names.low[-i.along.low]
              if (!all(names.low %in% names.high))
                  return(NULL)
              beta.high <- fetchResults(object = skeletonBetaHigh,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration
                                        lengthIter = lengthIter)
              beta.low <- fetchResults(object = skeletonBetaLow,
                                       filename = filename,
                                       iterations = NULL,
                                       nIteration = nIteration
                                       lengthIter = lengthIter)
              level.low <- readStateDLMFromFile(skeleton = skeleton.level.low,
                                                filename = filename,
                                                iterations = NULL,
                                                nIteration = nIteration
                                                lengthIter = lengthIter,
                                                only0 = FALSE)
              names.high.only <- setdiff(names.high, names.low)
              means.shared <- collapseDimension(beta.high,
                                                dimension = names.high.only,
                                                weights = 1)
              rescaleAndWriteBetas(high = beta.high,
                                   low = beta.low,
                                   adj = means.shared,
                                   skeletonHigh = skeletonBetaHigh,
                                   skeletonLow = skeletonBetaLow,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              recordAdjustments(priorHigh = priorHigh,
                                priorLow = priorLow,
                                namesHigh = names.high,
                                namesLow = names.low,
                                adj = means.shared,
                                adjustments = adjustments,
                                prefixAdjustments = prefixAdjustments)
              level.low <- level.low + means.shared
              overwriteValuesOnFile(object = level.low,
                                    skeleton = skeleton.level.low,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              NULL
          })



setMethod("rescalePairPriors",
          signature(priorHigh = "DLM",
                    priorLow = "Exchangeable"),
          function(priorHigh, priorLow,
                   skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              i.along.high <- priorHigh@iAlong
              phi.high <- priorHigh@phi
              phi.known.high <- priorHigh@phiKnown@.Data
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              skeleton.level.high <- skeletonsPriorHigh$level
              has.trend.high <- methods::is(priorHigh, "WithTrendMixin")
              level.non.stationary <- has.trend.high || (phi.known.high && isTRUE(all.equal(phi.high, 1)))
              if (!level.non.stationary)
                  return(NULL)
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              names.high <- names.high[-i.along.high]
              if (!all(names.low %in% names.high))
                  return(NULL)
              beta.high <- fetchResults(object = skeletonBetaHigh,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              beta.low <- fetchResults(object = skeletonBetaLow,
                                       filename = filename,
                                       iterations = NULL,
                                       nIteration = nIteration,
                                       lengthIter = lengthIter)
              level.0.high <- readStateDLMFromFile(skeleton = skeleton.level.high,
                                                   filename = filename,
                                                   iterations = NULL,
                                                   nIteration = nIteration,
                                                   lengthIter = lengthIter,
                                                   only0 = TRUE)
              level.high <- readStateDLMFromFile(object = skeleton.level.high,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration,
                                                 lengthIter = lengthIter,
                                                 only0 = FALSE)
              names.high.only <- setdiff(names.high, names.low)
              means.shared <- collapseDimension(level.0.high,
                                                dimension = names.high.only,
                                                weights = 1)
              rescaleAndWriteBetas(high = beta.high,
                                   low = beta.low,
                                   adj = means.shared,
                                   skeletonHigh = skeletonBetaHigh,
                                   skeletonLow = skeletonBetaLow,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              recordAdjustments(priorHigh = priorHigh,
                                priorLow = priorLow,
                                namesHigh = names.high,
                                namesLow = names.low,
                                adj = means.shared,
                                adjustments = adjustments,
                                prefixAdjustments = prefixAdjustments)
              level.high <- level.high - level.0.high
              overwriteValuesOnFile(object = level.high,
                                    skeleton = skeleton.level.high,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              NULL
          })

setMethod("rescalePairPriors",
          signature(priorHigh = "DLM",
                    priorLow = "DLM"),
          function(priorHigh, priorLow,
                   skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              ## extract parameters
              i.along.high <- priorHigh@iAlong
              i.along.low <- priorLow@iAlong
              phi.high <- priorHigh@phi
              phi.low <- priorLow@phi
              phi.known.high <- priorHigh@phiKnown@.Data
              phi.known.low <- priorLow@phiKnown@.Data
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              skeleton.level.high <- skeletonsPriorHigh$level
              skeleton.level.low <- skeletonsPriorLow$level
              has.trend.high <- methods::is(priorHigh, "WithTrendMixin")
              has.trend.low <- methods::is(priorLow, "WithTrendMixin")
              ## if neither series non-stationary, no rescaling needed
              if (has.trend.high) {
                  level.non.stationary.high <- TRUE
                  trend.non.stationary.high <- (phi.known.high && isTRUE(all.equal(phi.high, 1)))
              }
              else {
                  level.non.stationary.high <- (phi.known.high && isTRUE(all.equal(phi.high, 1)))
                  trend.non.stationary.high <- FALSE
              }
              if (has.trend.low) {
                  level.non.stationary.low <- TRUE
                  trend.non.stationary.low <- (phi.known.low && isTRUE(all.equal(phi.low, 1)))
              }
              else {
                  level.non.stationary.low <- (phi.known.low && isTRUE(all.equal(phi.low, 1)))
                  trend.non.stationary.low <- FALSE
              }
              at.least.one.level.is.stationary <- !level.non.stationary.high || !level.non.stationary.low
              at.last.one.trend.is.stationary <- !trend.non.stationary.high || !trend.non.stationary.low
              if (at.least.one.level.is.stationary && at.least.one.trend.is.stationary)
                  return(NULL)
              ## if lower-order term has dimension not in higher-order term, no rescaling
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              names.high <- names.high[-i.along.high]
              names.low <- names.low[-i.along.low]
              if (!all(names.low %in% names.high))
                  return(NULL)
              ## extract parameter estimates
              beta.high <- fetchResults(object = skeletonBetaHigh,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration
                                        lengthIter = lengthIter)
              beta.low <- fetchResults(object = skeletonBetaLow,
                                       filename = filename,
                                       iterations = NULL,
                                       nIteration = nIteration
                                       lengthIter = lengthIter)
              level.high <- readStateDLMFromFile(object = skeleton.level.high,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration
                                                 lengthIter = lengthIter,
                                                 only0 = FALSE)
              level.low <- readStateDLMFromFile(object = skeleton.level.low,
                                                filename = filename,
                                                iterations = NULL,
                                                nIteration = nIteration,
                                                lengthIter = lengthIter,
                                                only0 = FALSE)
              level.0.high <- readStateDLMFromFile(skeleton = skeleton.level.high,
                                                   filename = filename,
                                                   iterations = NULL,
                                                   nIteration = nIteration,
                                                   lengthIter = lengthIter,
                                                   only0 = TRUE)
              level.0.low <- readStateDLMFromFile(skeleton = skeleton.level.low,
                                                  filename = filename,
                                                  iterations = NULL,
                                                  nIteration = nIteration
                                                  lengthIter = lengthIter,
                                                  only0 = TRUE)
              if (trend.non.stationary.high && trend.non.stationary.low) {
                  trend.high <- readStateDLMFromFile(object = skeleton.trend.high,
                                                     filename = filename,
                                                     iterations = NULL,
                                                     nIteration = nIteration
                                                     lengthIter = lengthIter,
                                                     only0 = FALSE)
                  trend.low <- readStateDLMFromFile(object = skeleton.trend.low,
                                                    filename = filename,
                                                    iterations = NULL,
                                                    nIteration = nIteration,
                                                    lengthIter = lengthIter,
                                                    only0 = FALSE)
                  trend.0.high <- readStateDLMFromFile(skeleton = skeleton.trend.high,
                                                       filename = filename,
                                                       iterations = NULL,
                                                       nIteration = nIteration,
                                                       lengthIter = lengthIter,
                                                       only0 = TRUE)
                  trend.0.low <- readStateDLMFromFile(skeleton = skeleton.trend.low,
                                                      filename = filename,
                                                      iterations = NULL,
                                                      nIteration = nIteration
                                                      lengthIter = lengthIter,
                                                      only0 = TRUE)
              }
              ## calculate adjustments for levels
              names.high.only <- setdiff(names.high, names.low)
              means.shared.level <- collapseDimension(level.0.high,
                                                      dimension = names.high.only,
                                                      weights = 1)
              ## rescale betas and record them
              rescaleAndWriteBetas(high = beta.high,
                                   low = beta.low,
                                   adj = means.shared.level,
                                   skeletonHigh = skeletonBetaHigh,
                                   skeletonLow = skeletonBetaLow,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              ## adjust level and record
              level.high <- level.high - means.shared.level
              level.low <- level.high + means.shared.level
              overwriteValuesOnFile(object = level.high,
                                    skeleton = skeleton.level.high,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              overwriteValuesOnFile(object = level.low,
                                    skeleton = skeleton.level.low,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              ## if necessary, adjust trend and record
              if (trend.non.stationary.high && trend.non.stationary.low) {
                  means.shared.trend <- collapseDimension(trend.0.high,
                                                          dimension = names.high.only,
                                                          weights = 1)
                  trend.high <- trend.high - means.shared.trend
                  trend.low <- trend.high + means.shared.trend
                  overwriteValuesOnFile(object = trend.high,
                                        skeleton = skeleton.trend.high,
                                        filename = filename,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
                  overwriteValuesOnFile(object = trend.low,
                                        skeleton = skeleton.trend.low,
                                        filename = filename,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              }
              NULL
          })

setGeneric("rescalePriorIntercept",
           function(priorTerm, priorIntercept,
                    skeletonBetaTerm, skeletonBetaIntercept,
                    skeletonsPriorTerm, adjustments, prefixAdjustments,
                    filename, nIteration, lengthIter) {
               NULL
           })

setMethod("rescalePriorIntercept",
          signature(priorTerm = "Exchangeable"),
          function(priorTerm, priorIntercept, skeletonBetaTerm,
                   skeletonBetaIntercept, skeletonsPriorTerm,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              metadata.term <- skeletonBetaTerm@metadata
              has.covariates <- priorTerm@hasCovariates@.Data
              names.term <- names(metadata.term)
              name.intercept <- "(Intercept)"
              beta.term <- fetchResults(object = skeletonBetaTerm,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              beta.intercept <- fetchResults(object = skeletonBetaIntercept,
                                             filename = filename,
                                             iterations = NULL,
                                             nIteration = nIteration,
                                             lengthIter = lengthIter)
              mean.beta <- mean(beta.term)
              rescaleAndWriteBetas(high = beta.term,
                                   low = beta.intercept,
                                   adj = mean.beta,
                                   skeletonTerm = skeletonBetaTerm,
                                   skeletonLow = skeletonBetaIntercept,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              recordAdjustments(priorHigh = priorTerm,
                                priorLow = priorIntercept,
                                namesHigh = names.high,
                                namesLow = name.intercept,
                                adj = mean.beta,
                                adjustments = adjustments,
                                prefixAdjustments = prefixAdjustments)
              if (has.covariates) {
                  skeleton.covariates <- skeletonBetaTerm$coef
                  ## don't bother adjusting betaTerm, since do
                  ## not report intercept for covariates
                  coef.intercept <- readCoefInterceptFromFile(skeleton = skeleton.covariates,
                                                              filename = filename,
                                                              iterations = NULL,
                                                              nIteration = nIteration
                                                              lengthIter = lengthIter)
                  beta.intercept <- fetchResults(object = skeletonBetaIntercept,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration
                                                 lengthIter = lengthIter)
                  beta.intercept <- beta.intercept + coef.intercept
                  overwriteValuesOnFile(object = beta.intercept,
                                        skeleton = skeletonBetaIntercept,
                                        filename = filename,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              }
              NULL
          })


setMethod("rescalePriorIntercept",
          signature(priorTerm = "DLM"),
          function(priorTerm, priorIntercept, skeletonBetaTerm,
                   skeletonBetaIntercept, skeletonsPriorTerm,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              phi.term <- priorTerm@phi
              phi.known.term <- priorTerm@phiKnown@.Data
              metadata.term <- skeletonBetaTerm@metadata
              skeleton.level.term <- skeletonsPriorTerm$level
              has.trend.term <- methods::is(priorTerm, "WithTrendMixin")
              non.stationary <- has.trend.term || (phi.known.term && isTRUE(all.equal(phi.term, 1)))
              if (!non.stationary)
                  return(NULL)
              names.term <- names(metadata.term)
              beta.term <- fetchResults(object = skeletonBetaTerm,
                                        filename = filename,
                                        iterations = NULL,
                                        nIteration = nIteration
                                        lengthIter = lengthIter)
              beta.intercept <- fetchResults(object = skeletonBetaIntercept,
                                             filename = filename,
                                             iterations = NULL,
                                             nIteration = nIteration
                                             lengthIter = lengthIter)
              level.term <- readStateDLMFromFile(object = skeleton.level.term,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration,
                                                 lengthIter = lengthIter,
                                                 only0 = FALSE)
              level.0.term <- readStateDLMFromFile(skeleton = skeleton.level.term,
                                                   filename = filename,
                                                   iterations = NULL,
                                                   nIteration = nIteration,
                                                   lengthIter = lengthIter,
                                                   only0 = TRUE)
              mean.level.0 <- mean(level.0.term)
              rescaleAndWriteBetas(high = beta.term,
                                   low = beta.intercept,
                                   adj = mean.level.0,
                                   skeletonTerm = skeleton.term,
                                   skeletonLow = skeleton.intercept,
                                   filename = filename,
                                   nIteration = nIteration,
                                   lengthIter = lengthIter)
              level.term <- level.term - mean.level.0
              overwriteValuesOnFile(object = level.term,
                                    skeleton = skeleton.level.term,
                                    filename = filename,
                                    nIteration = nIteration,
                                    lengthIter = lengthIter)
              if (has.covariates) {
                  skeleton.covariates <- skeletonBetaTerm$coef
                  ## don't bother adjusting betaTerm, since do
                  ## not report intercept for covariates
                  coef.intercept <- readCoefInterceptFromFile(skeleton = skeleton.covariates,
                                                              filename = filename,
                                                              iterations = NULL,
                                                              nIteration = nIteration
                                                              lengthIter = lengthIter)
                  beta.intercept <- fetchResults(object = skeletonBetaIntercept,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration
                                                 lengthIter = lengthIter)
                  beta.intercept <- beta.intercept + coef.intercept
                  overwriteValuesOnFile(object = beta.intercept,
                                        skeleton = skeletonBetaIntercept,
                                        filename = filename,
                                        nIteration = nIteration,
                                        lengthIter = lengthIter)
              }
              NULL
=======
setMethod("rescalePairPriors",
          signature(priorHigher = "ExchNormZero",
                    priorLower = "ExchNormZero"),
          function(priorHigher, priorLower, skeletonBetaHigher, skeletonBetaLower,
                   skeletonsPriorHigher, skeletonsPriorLower, filename,
                   nIteration, lengthIter) {
              beta.higher <- fetchResults(object = skeletonBetaHigher,
                                          filename = filename,
                                          iterations = NULL,
                                          nIteration = nIteration
                                          lengthIter = lengthIter)
              beta.lower <- fetchResults(object = skeletonBetaLower,
                                         filename = filename,
                                         iterations = NULL,
                                         nIteration = nIteration
                                         lengthIter = lengthIter)
              means.shared.dims <- makeMeansSharedDims(betaHigher = beta.higher,
                                                       betaLower = beta.lower)
              beta.higher <- beta.higher - means.shared.dims
              beta.lower <- beta.lower + means.shared.dims
              writeToFile(object = beta.higher,
                          skeleton = skeletonBetaHigher,
                          filename = filename)
              writeToFile(object = beta.lower,
                          skeleton = skeletonBetaLower,
                          filename = filename)
          })


## can we select the right method based only on the first term???

setMethod("rescalePairPriors",
          signature(priorHigher = "DLMNoTrendNormZeroNoSeason",
                    priorLower = "ANY"), 
          function(priorHigher, priorLower, skeletonBetaHigher, skeletonBetaLower,
                   skeletonsPriorHigher, skeletonsPriorLower, filename,
                   nIteration, lengthIter) {
              phi <- priorHigher@phi
              phi.known <- priorHigher@phiKnown@.Data
              if ((phi < 1) || !phi.known)
                  return(NULL)
              beta.higher <- fetchResults(object = skeletonBetaHigher,
                                          filename = filename,
                                          iterations = NULL,
                                          nIteration = nIteration
                                          lengthIter = lengthIter)
              beta.lower <- fetchResults(object = skeletonBetaLower,
                                         filename = filename,
                                         iterations = NULL,
                                         nIteration = nIteration
                                         lengthIter = lengthIter)
              skeleton.level <- skeletonsPriorHigher$level
              level.higher <- fetchResults(object = skeleton.level,
                                           filename = filename,
                                           iterations = NULL,
                                           nIteration = nIteration
                                           lengthIter = lengthIter)
              alpha0.higher <- fetchAlpha0(object = skeleton.level,
                                           filename = filename,
                                           iterations = NULL,
                                           nIteration = nIteration
                                           lengthIter = lengthIter)
              means.alpha0 <- makeMeansAlpha0(alpha0 = alpha0,
                                              betaLower = beta.lower)
              beta.higher <- beta.higher - means.alpha0
              beta.lower <- beta.lower + means.alpha0
              level.higher <- level.higher - means.alpha0
              writeToFile(object = beta.higher,
                          skeleton = skeletonBetaHigher,
                          filename = filename)
              writeToFile(object = beta.lower,
                          skeleton = skeletonBetaLower,
                          filename = filename)
              writeToFile(object = level.higher,
                          skeleton = skeleton.level,
                          filename = filename)
          })


setMethod("rescalePairPriors",
          signature(priorHigher = "ExchNormZero",
                    priorLower = "ExchNormZero"),
          function(priorHigher, priorLower, skeletonBetaHigher, skeletonBetaLower,
                   skeletonsPriorHigher, skeletonsPriorLower, filename,
                   nIteration, lengthIter) {
              beta.higher <- fetchResults(object = skeletonBetaHigher,
                                          filename = filename,
                                          iterations = NULL,
                                          nIteration = nIteration
                                          lengthIter = lengthIter)
              beta.lower <- fetchResults(object = skeletonBetaLower,
                                         filename = filename,
                                         iterations = NULL,
                                         nIteration = nIteration
                                         lengthIter = lengthIter)
              means.shared.dims <- makeMeansSharedDims(betaHigher = beta.higher,
                                                       betaLower = beta.lower)
              beta.higher <- beta.higher - means.shared.dims
              beta.lower <- beta.lower + means.shared.dims
              writeToFile(object = beta.higher,
                          skeleton = skeletonBetaHigher,
                          filename = filename)
              writeToFile(object = beta.lower,
                          skeleton = skeletonBetaLower,
                          filename = filename)
>>>>>>> master
          })



<<<<<<< HEAD
setGeneric("rescalePred",
           function(pred, skeleton, adjustments, where) {
               pred
           })


setMethod("rescalePred",
          signature(pred = "Values", skeleton = "BetaTerm"),
          function(pred, skeleton, adjustments, where) {
              where <- tidyWhere(where)
              adj <- adjustments[[where]]
              if (is.null(adj))
                  pred
              else
                  pred + adj
          })


## also makeResultsObj
=======

>>>>>>> master



