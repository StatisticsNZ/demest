
rescaleInFilePred <- function(filenameEst, filenamePred) {
    ## get 'adjustments' from filenameEst
    results.est <- fetchResultsObject(filenameEst)
    nIteration <- results.est@mcmc["nIteration"]
    lengthIter <- results.est@control$lengthIter
    con.est <- file(filenameEst, open = "rb")
    size.results.est <- readBin(con = con.est, what = "integer", n = 1L)
    size.adjustments <- readBin(con = con.est, what = "integer", n = 1L)
    readBin(con = con.est, what = "raw", n = size.results.est)
    for (i in seq_len(nIteration))
        readBin(con = con.est, what = "double", n = lengthIter)
    adjustments.serialized <- readBin(con = con.est, what = "raw", n = size.adjustments)
    close(con.est)
    adjustments <- unserialize(adjustments)
    ## rescale
    rescaleBetasPred(results = results,
                     adjustments = adjustments,
                     filename = filename,
                     nIteration = nIteration,
                     lengthIter = lengthIter)
    ## add 'adjustments' to filenamePred
    con.pred <- file(filenamePred, open = "r+b")
    on.exit(close(con.pred))
    size.results.pred <- readBin(con = con.pred, what = "integer", n = 1L)
    writeBin(size.results.pred, con = con.pred)
    writeBin(size.adjustments, con = con.pred)
    NULL
}


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





