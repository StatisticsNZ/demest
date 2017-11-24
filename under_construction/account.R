






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




makeResultsAccount <- function(finalCombineds, mcmcArgs, controlArgs, seed) {
    combined <- finalCombineds[[1L]]
    account <- combined@account
    systemModels <- combined@systemModels
    dataModels <- combined@dataModels
    datasets <- combined@datasets
    names.datasets <- combined@namesDatasets
    transforms <- combined@transforms
    mcmc <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                           finalCombineds = finalCombineds)
    n.sim <- mcmc[["nSim"]]
    pos <- 1L
    if (n.sim > 0L) {
        output.model <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
        pos <- pos + changeInPos(output.model)
    }
    else
        model <- list()
    output.y <- Skeleton(y, first = pos)
    pos <- pos + changeInPos(output.y)
    has.exposure <- methods::is(combined, "HasExposure")
    if (has.exposure)
        exposure <- combined@exposure
    output.data.models <- vector(mode = "list", length = length(dataModels))
    if (n.sim > 0L) {
        for (i in seq_along(dataModels)) {
            output.data.models[[i]] <- makeOutputModel(model = dataModels[[i]],
                                                       pos = pos,
                                                       mcmc = mcmc)
            pos <- pos + changeInPos(output.data.models[[i]])
        }
        for (i in seq_along(datasets)) {
            if (any(is.na(datasets[[i]])))
                datasets[[i]] <-
                    SkeletonMissingDataset(object = datasets[[i]],
                                           model = dataModels[[i]],
                                           outputModel = output.data.models[[i]],
                                           transformComponent = transforms[[i]],
                                           skeletonComponent = output.y)
        }
    }
    names(output.data.models) <- names.datasets
    names(datasets) <- names.datasets
    final <- finalCombineds
    names(final) <- paste("chain", seq_along(final), sep = "")
    if (has.exposure) {
        methods::new("ResultsCountsExposureEst",
            model = output.model,
            y = output.y,
            exposure = exposure,
            dataModels = output.data.models,
            datasets = datasets,
            mcmc = mcmc,
            control = controlArgs,
            seed = seed,
            final = final)
    }
    else
        methods::new("ResultsCountsEst",
            model = output.model,
            y = output.y,
            dataModels = output.data.models,
            datasets = datasets,
            mcmc = mcmc,
            control = controlArgs,
            seed = seed,
            final = final)
}



