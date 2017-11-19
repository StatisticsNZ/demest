





estimateAccount <- function(y, systemModels, observationModels, datasets,
                            dominant = c("Female", "Male"),
                            filename = NULL,
                            nBurnin = 1000, nSim = 1000, nChain = 4, nThin = 1,
                            parallel = TRUE, nUpdateMax = 200,
                            verbose = FALSE, useC = TRUE) {
    call <- match.call()
    methods::validObject(y)
    checkSystem(systemModels)
    systemModels <- alignSystemToAccount(systemModels = systemModels,
                                   account = account)
    checkObservation(observationModels, needsNonDefaultSeriesArg = TRUE)
    checkNamesDatasets(datasets)
    datasets <- alignDatasetsToObservation(datasets = datasets,
                                           observationModels = observationModels)
    ## check datasets after aligning to avoid checking datasets that are not needed
    datasets <- checkAndTidyDatasets(datasets)
    dominant <- match.arg(dominant)
    transforms <- makeTransformsYToDatasets(y = y, nameY = "y", datasets = datasets)
    namesDatasets <- names(datasets)
    names(datasets) <- NULL
    mcmc.args <- makeMCMCArgs(nBurnin = nBurnin,
                              nSim = nSim,
                              nChain = nChain,
                              nThin = nThin)
    if (is.null(filename))
        filename <- tempfile()
    else
        checkFilename(filename)
    control.args <- makeControlArgs(call = call,
                                    parallel = parallel)
    combineds <- replicate(n = mcmc.args$nChain,
                           initialCombinedAccount(account = account,
                                                  systemModels = systemModels,
                                                  systemWeights = systemWeights,
                                                  observationModels = observationModels,
                                                  seriesIndices = seriesIndices,
                                                  datasets = datasets,
                                                  namesDatasets = namesDatasets,
                                                  transforms = transforms,
                                                  dominant = dominant))
    parallel <- control.args$parallel
    tempfile <- paste(filename, seq_len(mcmc.args$nChain), sep = "_")
    MoreArgs <- c(list(seed = NULL),
                  mcmc.args,
                  control.args,
                  list(continuing = FALSE))
    if (parallel) {
        cl <- parallel::makeCluster(getOption("cl.cores", default = mcmc.args$nChain))
        parallel::clusterSetRNGStream(cl)
        final.combineds <- parallel::clusterMap(cl = cl,
                                      fun = estimateOneChain,
                                      tempfile = tempfiles,
                                      combined = combineds,
                                      MoreArgs = MoreArgs,
                                      SIMPLIFY = FALSE,
                                      USE.NAMES = FALSE)
        seed <- parallel::clusterCall(cl, function() .Random.seed)
        parallel::stopCluster(cl)
    }
    else {
        final.combineds <- mapply(estimateOneChain,
                                  tempfile = tempfiles,
                                  combined = combineds,
                                  MoreArgs = MoreArgs,
                                  SIMPLIFY = FALSE,
                                  USE.NAMES = FALSE)
        seed <- list(.Random.seed)
    }
    control.args$lengthIter <- length(extractValues(final.combineds[[1L]]))
    results <- makeResultsAccount(finalCombineds = final.combineds,
                                  mcmcArgs = mcmc.args,
                                  controlArgs = control.args,
                                  seed = seed)
    makeResultsFile(filename = control.args$filename,
                    results = results,
                    tempfiles = tempfiles)
    finalMessage(filename = filename, verbose = verbose)
}


