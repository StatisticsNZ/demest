
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
    rescalePriors(results = results,
                  filename = filename,
                  nIteration = nIteration,
                  lengthIter = lengthIter)
}

setMethod("rescalePriors",
          signature(results = "ResultsModelEst"),
          function(results, filename, nIteration, lengthIter) {
              priors <- results@final[[1L]]@model@priorsBetas
              skeletons.betas <- results@model$prior[seq_along(priors)]
              skeletons.priors <- results@model$hyper
              margins <- results@model@margins
              rescalePriorsHelper(priors = priors,
                                  skeletonsBetas = skeletons.betas,
                                  skeletonsPriors = skeletons.priors,
                                  margins = margins,
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
          })


setMethod("rescalePriors",
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
                                          filename = filename,
                                          nIteration = nIteration,
                                          lengthIter = lengthIter)
                  }
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
        rescalePairPriors(priorHigher = prior.higher,
                          priorLower = prior.lower,
                          skeletonBetaHigher = skeleton.beta.higher,
                          skeletonBetaLower = skeleton.beta.lower,
                          skeletonsPriorHigher = skeletons.prior.higher,
                          skeletonsPriorLower = skeletons.prior.lower,
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
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter)        
    }
}

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
          })







