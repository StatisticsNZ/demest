






estimateAccount <- function(account, systemModels, observationModels, datasets,
                            weights = list(), dominant = c("Female", "Male"),
                            filename = NULL, nBurnin = 1000, nSim = 1000,
                            nChain = 4, nThin = 1,
                            parallel = TRUE, nUpdateMax = 50,
                            verbose = FALSE, useC = TRUE) {
    call <- match.call()
    methods::validObject(account)
    dominant <- match.arg(dominant)
    ## align system models to account
    checkSystemModels(systemModels)
    systemModels <- alignSystemModelsToAccount(systemModels = systemModels,
                                               account = account)
    ## align weights to system models
    systemWeights <- checkAndTidySystemWeights(weights,
                                               systemModels = systemModels)
    ## align observation models to datasets
    checkObservationModels(observationModels = observationModels,
                           needsNonDefaultSeriesArg = TRUE)
    datasets <- checkAndTidyDatasets(datasets)
    namesDatasets <- names(datasets)
    names(datasets) <- NULL
    observationModels <- alignObservationModelsToDatasets(observationModels = observationModels,
                                                          datasets = datasets,
                                                          namesDatasets = namesDatasets)
    ## make 'seriesIndices', mapping observations models to account
    seriesIndices <- makeSeriesIndices(observationModels = observationModels,
                                       account = account)
    ## make transforms from account to datasets
    transforms <- makeTransformsAccountToDatasets(account = account,
                                                  datasets = datasets,
                                                  namesDatasets = namesDatasets)
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
                                                  systemWeights = weights,
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


