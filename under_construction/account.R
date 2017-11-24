






estimateAccount <- function(account, systemModels, dataModels, datasets,
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
    ## align data models to datasets
    checkDataModels(dataModels = dataModels,
                    needsNonDefaultSeriesArg = TRUE)
    datasets <- checkAndTidyDatasets(datasets)
    namesDatasets <- names(datasets)
    names(datasets) <- NULL
    dataModels <- alignDataModelsToDatasets(dataModels = dataModels,
                                            datasets = datasets,
                                            namesDatasets = namesDatasets)
    ## make 'seriesIndices', mapping data models to account
    seriesIndices <- makeSeriesIndices(dataModels = dataModels,
                                       account = account)
    ## make transforms from account to datasets
    transforms <- makeTransformsAccountToDatasets(account = account,
                                                  datasets = datasets,
                                                  namesDatasets = namesDatasets)
    ## mcmc and control arguments
    mcmc.args <- makeMCMCArgs(nBurnin = nBurnin,
                              nSim = nSim,
                              nChain = nChain,
                              nThin = nThin)
    if (is.null(filename))
        filename <- tempfile()
    else
        checkFilename(filename)
    control.args <- makeControlArgs(call = call,
                                    parallel = parallel,
                                    nUpdateMax = nUpdateMax)
    ## initial values
    combineds <- replicate(n = mcmc.args$nChain,
                           initialCombinedAccount(account = account,
                                                  systemModels = systemModels,
                                                  systemWeights = weights,
                                                  dataModels = dataModels,
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
    ## estimation
    if (parallel) {
        if (is.null(outfile)) ## passing 'outfile' as an argument always causes redirection
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args$nChain))
        else
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args$nChain),
                                        outfile = outfile)
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
    ## results object
    control.args$lengthIter <- length(extractValues(final.combineds[[1L]]))
    results <- makeResultsAccount(finalCombineds = final.combineds,
                                  mcmcArgs = mcmc.args,
                                  controlArgs = control.args,
                                  seed = seed)
    makeResultsFile(filename = filename,
                    results = results,
                    tempfiles = tempfiles)
    rescaleInFile(filename)
    finalMessage(filename = filename, verbose = verbose)
}


