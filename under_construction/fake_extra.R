
#' @inheritParams estimateAccount
#' @export
simulateAccount <- function(account, systemModels, datasets, dataModels, 
                            concordances = list(), weights = list(),
                            dominant = c("Female", "Male"),
                            filename = NULL, nBurnin = 1000, nSim = 1000,
                            nChain = 4, nThin = 1,
                            parallel = TRUE, nCore = NULL,
                            outfile = NULL, nUpdateMax = 50,
                            verbose = TRUE, useC = TRUE) {
    call <- match.call()
    methods::validObject(account)
    dominant <- match.arg(dominant)
    ## make account consistent, if necessary
    if (!all(dembase::isConsistent(account)))
        account <- dembase::makeConsistent(account)
    ## align system models to account
    checkSystemModels(systemModels)
    systemModels <- alignSystemModelsToAccount(systemModels = systemModels,
                                               account = account)
    ## align weights to system models
    systemWeights <- checkAndTidySystemWeights(weights,
                                               systemModels = systemModels)
    ## check models are suitable for simulation
    checkSystemModelsSuitableForSimulation(systemModels = systemModels,
                                           account = account)
    ## see if there are any data models, and process accordingly
    has.data.models <- length(dataModels) > 0L
    if (has.data.models) {
        ## align to datasets
        checkDataModels(dataModels = dataModels,
                        needsNonDefaultSeriesArg = TRUE)
        datasets <- checkAndTidyDatasets(datasets)
        namesDatasets <- names(datasets)
        names(datasets) <- NULL
        dataModels <- alignDataModelsToDatasets(dataModels = dataModels,
                                                datasets = datasets,
                                                namesDatasets = namesDatasets)
        ## check priors
        checkDataModelsSuitableForSimulation(dataModels = dataModels,
                                             datasets = datasets,
                                             namesDatasets = namesDatasets)
        ## make 'seriesIndices', mapping data models to account
        seriesIndices <- makeSeriesIndices(dataModels = dataModels,
                                           account = account)
        ## make transforms from account to datasets
        checkConcordancesDatasets(concordances = concordances,
                                  datasets = datasets,
                                  namesDatasets = namesDatasets)
        transforms <- makeTransformsAccountToDatasets(account = account,
                                                      datasets = datasets,
                                                      concordances = concordances,
                                                      namesDatasets = namesDatasets,
                                                      seriesIndices = seriesIndices)
    }
    else {
        if (length(datasets) > 0L)
            stop(gettext("there are datasets, but no data models"))
        seriesIndices <- list()
        transforms <- list()
    }
    ## mcmc and control arguments
    mcmc.args <- makeMCMCArgs(nBurnin = nBurnin,
                              nSim = nSim,
                              nChain = nChain,
                              nThin = nThin,
                              nCore = nCore)
    if (is.null(filename))
        filename <- tempfile()
    else
        checkFilename(filename)
    control.args <- makeControlArgs(call = call,
                                    parallel = parallel,
                                    nUpdateMax = nUpdateMax)
    ## initial values
    combined <- initialCombinedAccountSimulate(account = account,
                                               systemModels = systemModels,
                                               systemWeights = systemWeights,
                                               dataModels = dataModels,
                                               seriesIndices = seriesIndices,
                                               datasets = datasets,
                                               namesDatasets = namesDatasets,
                                               transforms = transforms,
                                               dominant = dominant)
    combineds <- rep(list(combined),
                     times = mcmc.args$nChain)
    parallel <- control.args$parallel
    tempfiles <- paste(filename, seq_len(mcmc.args$nChain), sep = "_")
    MoreArgs <- c(list(seed = NULL),
                  mcmc.args,
                  control.args,
                  list(continuing = FALSE,
                       useC = useC))
    ## estimation
    if (parallel) {
        if (is.null(outfile)) ## passing 'outfile' as an argument always causes redirection
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args$nCore))
        else
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args$nCore),
                                        outfile = outfile)
        parallel::clusterSetRNGStream(cl)
        final.combineds <- parallel::clusterMap(cl = cl,
                                                fun = simulateOneChain,
                                                tempfile = tempfiles,
                                                combined = combineds,
                                                MoreArgs = MoreArgs,
                                                SIMPLIFY = FALSE,
                                                USE.NAMES = FALSE)
        seed <- parallel::clusterCall(cl, function() .Random.seed)
        parallel::stopCluster(cl)
    }
    else {
        final.combineds <- mapply(simulateOneChain,
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



    


setMethod("drawCombined",
          signature(object = "CombinedAccountMovements"),
          function(object, nUpdate = 1L, isFirst, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(drawCombined_CombinedAccountMovements_R, object, nUpdate)
                  else
                      .Call(drawCombined_R, object, nUpdate)
              }
              else {
                  system.models.use.ag <- object@systemModelsUseAg@.Data
                  data.models.use.ag <- object@dataModelsUseAg@.Data
                  if (system.models.use.ag) {
                      object <- updateSystemModels(object)
                      object <- updateExpectedExposure(object)
                  }
                  object <- updateAccount(object)
                  if (data.models.use.ag)
                      object <- updateDataModelsAccount(object)
                  object
              }
          })




           
