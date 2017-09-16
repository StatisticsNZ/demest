
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
    adjustments <- new.env(hash = TRUE)
    rescalePriors(results = results,
                  adjustments = adjustments,
                  filename = filename,
                  nIteration = nIteration,
                  lengthIter = lengthIter)
    addAdjustmentsToFile(adjustments = adjustments,
                         filename = filename,
                         nIteration = nIteration,
                         lengthIter = lengthIter)
}

setMethod("rescalePriors",
          signature(results = "ResultsModelEst"),
          function(results, adjustments, filename, nIteration, lengthIter) {
              priors <- results@final[[1L]]@model@priorsBetas
              skeletons.betas <- results@model$prior[seq_along(priors)]
              skeletons.priors <- results@model$hyper
              margins <- results@model@margins
              rescalePriorsHelper(priors = priors,
                                  skeletonsBetas = skeletons.betas,
                                  skeletonsPriors = skeletons.priors,
                                  margins = margins,
                                  adjustments = adjustments,
                                  prefixAdjustments = "model",
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
          })


setMethod("rescalePriors",
          signature(results = "ResultsCounts"),
          function(results, adjustments, filename, nIteration, lengthIter) {
              final <- object@final[[1L]]
              model <- final@model
              observation <- final@observation
              namesDatasets <- final@namesDatasets
              priors <- final@model@priorsBetas
              skeletons.betas <- model$prior
              skeletons.priors <- model$hyper
              margins <- final@model@margins
              prefix.adjustments <- "model"
              rescalePriorsHelper(priors = priors,
                                  skeletonsBetas = skeletons.betas,
                                  skeletonsPriors = skeletons.priors,
                                  margins = margins,
                                  adjustments = adjustments,
                                  prefixAdjustments = prefix.adjustments,
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
              for (i in seq_along(observation)) {
                  if (methods::is(final@observation[[i]], "Varying")) {
                      priors <- final@observation[[i]]@priorsBetas
                      skeletons.betas <- observation[[i]]$prior
                      skeletons.priors <- observation[[i]]$hyper
                      margins <- finale@observation[[i]]@margins
                      name.dataset <- namesDatasets[i]
                      prefix.adjustments <- paste("observation", name.dataset, collapse = ".")
                      rescalePriorsHelper(priors = priors,
                                          skeletonsBetas = skeletons.betas,
                                          skeletonsPriors = skeletons.priors,
                                          margins = margins,
                                          adjustments = adjustments,
                                          prefixAdjustments = prefix.adjustments,
                                          filename = filename,
                                          nIteration = nIteration,
                                          lengthIter = lengthIter)
                  }
              }                  
          })



rescalePriorsHelper <- function(priors, skeletonsBetas skeletonsPriors, margins,
                                adjustments, prefixAdjustments,
                                filename, nIteration, lengthIter) {
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
        rescalePairPriors(priorHigh = prior.higher,
                          priorLow = prior.lower,
                          skeletonBetaHigh = skeleton.beta.higher,
                          skeletonBetaLow = skeleton.beta.lower,
                          skeletonsPriorHigh = skeletons.prior.higher,
                          skeletonsPriorLow = skeletons.prior.lower,
                          adjustments = adjustments,
                          prefixAdjustments = prefixAdjustments,
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    }
    skeletons.beta.intercept <- skeletonsBetas[[1L]]
    for (i in seq_along(priors[-1L])) {
        prior.term <- priors[[i]]
        skeleton.beta.term <- skeletonsBetas[[i]]
        skeletons.prior.term <- skeletonsPriors[[i]]
        rescalePriorIntercept(priorTerm = prior.term,
                              skeletonBetaTerm = skeleton.beta.term,
                              skeletonBetaIntercept = skeleton.beta.intercept,
                              skeletonsPriorTerm = skeletons.prior.term,
                              adjustments = adjustments,
                              prefixAdjustments = prefixAdjustments,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)        
    }
}

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


test_that("makePairsTerms works", {
    ## makePairsTerms <- demest:::makePairsTerms
    margins <- list(0L, 1L, 2L, 1:2)
    ans.obtained <- makePairsTerms(margins)
    ans.expected <- list(3:2, c(3L, 1L))
    expect_identical(ans.obtained, ans.expected)
    margins <- list(0L, 1L, 2L, 3L, 1:2, c(1L, 3L), 2:3, 1:3)
    ans.obtained <- makePairsTerms(margins)
    ans.expected <- list(7:6, c(7L, 5L), c(7L, 4L), c(7L, 3L), c(7L, 2L), c(7L, 1L),
                         c(6L, 3L), c(6L, 2L),
                         c(5L, 3L), c(5L, 1L),
                         c(4L, 2L), c(4L, 1L))
    expect_identical(ans.obtained, ans.expected)
})


## ignores covariates
setMethod("rescalePairPriors",
          signature(priorHigh = "Exch",
                    priorLow = "Exch"),
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
              beta.high <- beta.high - means.shared
              beta.low <- beta.low + means.shared
              nm.adj.high <- paste(names.high, collapse = ":")
              nm.adj.low <- paste(names.low, collapse = ":")
              nm.adj.high <- paste(c(prefixAdjustments, "prior", nm.adj.high),
                                   collapse = ".")
              nm.adj.low <- paste(c(prefixAdjustments, "prior", nm.adj.low),
                                  collapse = ".")
              already.has.adj.high <- !is.null(adjustments[[nm.adj.high]])
              already.has.adj.low <- !is.null(adjustments[[nm.adj.low]])
              if (already.has.adj.high)
                  adjustments[[nm.adj.high]] <- adjustments[[nm.adj.high]] - means.shared
              else
                  adjustments[[nm.adj.high]] <- -1 * means.shared
              if (already.has.adj.low)
                  adjustments[[nm.adj.low]] <- adjustments[[nm.adj.low]] + means.shared
              else
                  adjustments[[nm.adj.low]] <- means.shared
              writeTermToFile(object = beta.high,
                              skeleton = skeletonBetaHigh,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
              writeTermToFile(object = beta.low,
                              skeleton = skeletonBetaLow,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
              NULL
          })


## need to add argument to fetchResults - along0 = FALSE
## add similar argument to writeTermToFile
## adjust SkeletonStateDLM accordingly


## ignores covariates
setMethod("rescalePairPriors",
          signature(priorHigh = "Exch",
                    priorLow = "DLM"),
          function(priorHigh, priorLow, skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              i.along.low <- priorLow@iAlong
              phi.low <- priorLow@phi
              phi.known.low <- priorLow@phiKnown@.Data
              metadata.high <- skeletonBetaHigh@metadata
              metadata.low <- skeletonBetaLow@metadata
              skeleton.level.low <- skeletonsPriorLow$level
              non.stationary <- phi.known.low && isTRUE(all.equal(phi.low, 1))
              if (!non.stationary)
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
              level.low <- fetchResults(object = skeleton.level.low,
                                          filename = filename,
                                          iterations = NULL,
                                          nIteration = nIteration
                                          lengthIter = lengthIter,
                                          along0 = FALSE)
              names.high.only <- setdiff(names.high, names.low)
              means.shared <- collapseDimension(beta.high,
                                                dimension = names.high.only,
                                                weights = 1)
              beta.high <- beta.high - means.shared
              beta.low <- beta.low + means.shared
              level.low <- level.low + means.shared
              nm.adj.beta.high <- paste(names.high, collapse = ":")
              nm.adj.beta.low <- paste(names.low, collapse = ":")
              nm.adj.level.low <- paste(names.low, collapse = ":")
              nm.adj.beta.high <- paste(c(prefixAdjustments, "prior", nm.adj.beta.high),
                                            collapse = ".")
              nm.adj.beta.low <- paste(c(prefixAdjustments, "prior", nm.adj.beta.low),
                                           collapse = ".")
              nm.adj.level.low <- paste(c(prefixAdjustments, "hyper", "level", nm.adj.level.low),
                                            collapse = ".")
              already.has.adj.beta.high <- !is.null(adjustments[[nm.adj.beta.high]])
              already.has.adj.beta.low <- !is.null(adjustments[[nm.adj.beta.low]])
              already.has.adj.level.low <- !is.null(adjustments[[nm.adj.level.low]])
              if (already.has.adj.beta.high)
                  adjustments[[nm.adj.beta.high]] <- adjustments[[nm.adj.beta.high]] - means.shared
              else
                  adjustments[[nm.adj.beta.high]] <- -1 * means.shared
              if (already.has.adj.beta.low)
                  adjustments[[nm.adj.beta.low]] <- adjustments[[nm.adj.beta.low]] + means.shared
              else
                  adjustments[[nm.adj.beta.low]] <- means.shared
              if (already.has.adj.level.low)
                  adjustments[[nm.adj.level.low]] <- adjustments[[nm.adj.level.low]] + means.shared
              else
                  adjustments[[nm.adj.level.low]] <- means.shared
              writeTermToFile(object = beta.high,
                              skeleton = skeletonBetaHigh,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
              writeTermToFile(object = beta.low,
                              skeleton = skeletonBetaLow,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
              writeTermToFile(object = level.low,
                              skeleton = skeleton.level.low,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter,
                              along0 = FALSE)
              NULL
          })



## ignores covariates
setMethod("rescalePairPriors",
          signature(priorHigh = "DLM",
                    priorLow = "Exch"),
          function(priorHigh, priorLow, skeletonBetaHigh, skeletonBetaLow,
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
              non.stationary <- has.trend.high || (phi.known.high && isTRUE(all.equal(phi.high, 1)))
              if (!non.stationary)
                  return(NULL)
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              names.high <- names.high[-i.along.high]
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
              level.0.high <- fetchResults(object = skeleton.level.high,
                                           filename = filename,
                                           iterations = NULL,
                                           nIteration = nIteration
                                           lengthIter = lengthIter,
                                           along0 = TRUE)
              level.non0.high <- fetchResults(object = skeleton.level.high,
                                              filename = filename,
                                              iterations = NULL,
                                              nIteration = nIteration
                                              lengthIter = lengthIter,
                                              along0 = FALSE)
              beta.high <- beta.high - level.0.high
              beta.low <- beta.low + level.0.high
              level.non0.high <- level.non0.high - level.0.high
              nm.adj.beta.high <- paste(names.high, collapse = ":")
              nm.adj.beta.low <- paste(names.low, collapse = ":")
              nm.adj.level.high <- paste(names.high, collapse = ":")
              nm.adj.beta.high <- paste(c(prefixAdjustments, "prior", nm.adj.beta.high),
                                        collapse = ".")
              nm.adj.beta.low <- paste(c(prefixAdjustments, "prior", nm.adj.beta.low),
                                       collapse = ".")
              nm.adj.level.high <- paste(c(prefixAdjustments, "hyper", "level", nm.adj.level.high),
                                         collapse = ".")
              already.has.adj.beta.high <- !is.null(adjustments[[nm.adj.beta.high]])
              already.has.adj.beta.low <- !is.null(adjustments[[nm.adj.beta.low]])
              already.has.adj.level.high <- !is.null(adjustments[[nm.adj.level.high]])
              if (already.has.adj.beta.high)
                  adjustments[[nm.adj.beta.high]] <- adjustments[[nm.adj.beta.high]] - level.0.high
              else
                  adjustments[[nm.adj.beta.high]] <- -1 * level.0.high
              if (already.has.adj.beta.low)
                  adjustments[[nm.adj.beta.low]] <- adjustments[[nm.adj.beta.low]] + level.0.high
              else
                  adjustments[[nm.adj.beta.low]] <- level.0.high
              if (already.has.adj.level.high)
                  adjustments[[nm.adj.level.high]] <- adjustments[[nm.adj.level.high]] + level.0.high
              else
                  adjustments[[nm.adj.level.high]] <- level.0.high
              writeTermToFile(object = beta.high,
                              skeleton = skeletonBetaHigh,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
              writeTermToFile(object = beta.low,
                              skeleton = skeletonBetaLow,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
              writeTermToFile(object = level.high,
                              skeleton = skeleton.level.high,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter,
                              along0 = FALSE)
              NULL
          })


## ignores covariates
setMethod("rescalePairPriors",
          signature(priorHigh = "DLM",
                    priorLow = "DLM"),
          function(priorHigh, priorLow, skeletonBetaHigh, skeletonBetaLow,
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
              non.stationary.high <- has.trend.high || (phi.known.high && isTRUE(all.equal(phi.high, 1)))
              non.stationary.low <- has.trend.low || (phi.known.low && isTRUE(all.equal(phi.low, 1)))
              if (!non.stationary.high && !non.stationary.low)
                  return(NULL)
              ## if lower-order term has dimension not in higher-order term, no rescaling needed ## NEED TO CHECK WITH JUNNI!!!!!!
              names.high <- names(metadata.high)
              names.low <- names(metadata.low)
              names.high <- names.high[-i.along.high]
              names.low <- names.high[-i.along.low]
              if (!all(names.low %in% names.high))
                  return(NULL)
              ## extract parameter estimates (which may already have been partly rescaled)
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
              level.0.high <- fetchResults(object = skeleton.level.high,
                                           filename = filename,
                                           iterations = NULL,
                                           nIteration = nIteration
                                           lengthIter = lengthIter,
                                           along0 = TRUE)
              level.0.low <- fetchResults(object = skeleton.level.low,
                                          filename = filename,
                                          iterations = NULL,
                                          nIteration = nIteration
                                          lengthIter = lengthIter,
                                          along0 = TRUE)
              level.non0.high <- fetchResults(object = skeleton.level.high,
                                              filename = filename,
                                              iterations = NULL,
                                              nIteration = nIteration
                                              lengthIter = lengthIter,
                                              along0 = FALSE)
              level.non0.low <- fetchResults(object = skeleton.level.low,
                                             filename = filename,
                                             iterations = NULL,
                                             nIteration = nIteration
                                             lengthIter = lengthIter,
                                             along0 = FALSE)
              if (has.trend.high && has.trend.low) {
                  trend.0.high <- fetchResults(object = skeleton.trend.high,
                                               filename = filename,
                                               iterations = NULL,
                                               nIteration = nIteration
                                               lengthIter = lengthIter,
                                               along0 = TRUE)
                  trend.0.low <- fetchResults(object = skeleton.trend.low,
                                              filename = filename,
                                              iterations = NULL,
                                              nIteration = nIteration
                                              lengthIter = lengthIter,
                                              along0 = TRUE)
                  trend.non0.high <- fetchResults(object = skeleton.trend.high,
                                                  filename = filename,
                                                  iterations = NULL,
                                                  nIteration = nIteration
                                                  lengthIter = lengthIter,
                                                  along0 = FALSE)
                  trend.non0.low <- fetchResults(object = skeleton.trend.low,
                                                 filename = filename,
                                                 iterations = NULL,
                                                 nIteration = nIteration
                                                 lengthIter = lengthIter,
                                                 along0 = FALSE)
              }
              ## rescale parameters
              beta.low <- beta.high - level.0.high
              beta.low <- beta.low + level.0.high
              level.non0.high <- level.non0.high - level.0.high
              level.non0.low <- level.non0.high + level.0.high
              if (has.trend.high && has.trend.low) {
                  names.high.only <- setdiff(names.high, names.low)
                  means.shared <- collapseDimension(trend.0.high,
                                                    dimension = names.high.only,
                                                    weights = 1)
                  trend.non0.high <- trend.non0.high - means.shared
                  trend.non0.low <- trend.non0.low + means.shared
              }
              ## SAVE RESCALED DELTA0???
              ## record adjustments - ONLY BETAS WILL BE USED SO ONLY RECORD THEM??
              ## note that 'adjustments' is an environment, so
              ## the changes are done *in place*
              nm.adj.high <- paste(names.high, collapse = ":")
              nm.adj.low <- paste(names.low, collapse = ":")
              nm.adj.high <- paste(c(prefixAdjustments, "prior", nm.adj.high),
                                   collapse = ".")
              nm.adj.low <- paste(c(prefixAdjustments, "prior", nm.adj.low),
                                  collapse = ".")
              already.has.adj.high <- !is.null(adjustments[[nm.adj.high]])
              already.has.adj.low <- !is.null(adjustments[[nm.adj.low]])
              already.has.adj.level.high <- !is.null(adjustments[[nm.adj.level.high]])
              if (already.has.adj.high)
                  adjustments[[nm.adj.high]] <- adjustments[[nm.adj.high]] - level.0.high
              else
                  adjustments[[nm.adj.high]] <- -1 * level.0.high
              if (already.has.adj.low)
                  adjustments[[nm.adj.low]] <- adjustments[[nm.adj.low]] + level.0.high
              else
                  adjustments[[nm.adj.low]] <- level.0.high
              ## write terms to file
              writeTermToFile(object = beta.high,
                              skeleton = skeletonBetaHigh,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
              writeTermToFile(object = beta.low,
                              skeleton = skeletonBetaLow,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)
              writeTermToFile(object = level.high,
                              skeleton = skeleton.level.high,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter,
                              along0 = FALSE)
              writeTermToFile(object = level.low,
                              skeleton = skeleton.level.high,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter,
                              along0 = FALSE)
              if (has.trend.high && has.trend.low) {
                  writeTermToFile(object = trend.high,
                                  skeleton = skeleton.trend.high,
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter,
                                  along0 = FALSE)
                  writeTermToFile(object = trend.low,
                                  skeleton = skeleton.trend.high,
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter,
                                  along0 = FALSE)
              }
              NULL
          })






writeTermToFile <- function(object, skeleton, filename, nIteration, lengthIter,
                        useC = TRUE) {
    first <- skeleton@first
    last <- skeleton@last
    con <- file(filename, open = "rb")
    on.exit(close(con))
    ## find out size of results object - stored in first position
    size.results <- readBin(con = con, what = "integer", n = 1L)
    ## skip over results object
    for (i.res in seq_len(size.results))
        readBin(con = con, what = "raw", n = 1L)
    pos <- 1L ## positition within object
    for (i.iter in seq_len(nIteration)) {
        ## skip over positions in line of file before start of data
        for (i.col in seq_len(first - 1L))
            readBin(con = con, what = "double", n = 1L)
        ## write values
        for (i.col in seq.int(from = first, to = last)) {
            writeBin(object[pos], con = con, what = "double", n = 1L)
            pos <- pos + 1L
        }
        ## skip remaining positions in line of file, if any
        if (last < lengthIter) {
            for (i.col in seq.int(from = last + 1L, to = lengthIter))
                readBin(con = con, what = "double", n = 1L)
        }
    }
}       









