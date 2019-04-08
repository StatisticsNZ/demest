

#' Draw parameters and data from a statistical model
#'
#' WARNING - THIS FUNCTION IS STILL UNDER DEVELOPMENT
#' 
#' @inheritParams estimateModel
#' @param nDraw The number of random draws to make from the model.
#' @export
simulateModel <- function(model, y = NULL, exposure = NULL, weights = NULL,
                          filename = NULL, nDraw = 10, 
                          verbose = TRUE, useC = TRUE) {
    call <- match.call()
    methods::validObject(model)
    model.uses.aggregate <- !methods::is(model@aggregate, "SpecAgPlaceholder")
    if (model.uses.aggregate) {
        stop(gettextf("function '%s' cannot be used if model includes aggregate values : consider using function '%s' instead",
                      "simulateModel", "simulateModelAg"))
    }
    checkPositiveInteger(x = nDraw,
                         name = "nDraw")
    nDraw <- as.integer(nDraw)
    if (is.null(filename))
        filename <- tempfile()
    else
        checkFilename(filename)
    tempfile <- paste(filename, "sim", sep = "_")
    l <- checkAndTidySimulatedYExposureWeights(model = model,
                                               y = y,
                                               exposure = exposure,
                                               weights = weights)
    y <- l$y
    exposure <- l$exposure
    y <- castY(y = y,
               spec = model)
    checkForSubtotals(object = y,
                      model = model,
                      name = "y")
    exposure <- castExposure(exposure = exposure,
                             model = model)
    weights <- checkAndTidyWeights(weights = weights,
                                   y = y)
    checkAllDimensionsHavePriors(model = model,
                                 y = y)
    checkPriorsAreInformative(model)
    checkPriorSDInformative(model)
    combined <- initialCombinedModelSimulate(model,
                                             y = y,
                                             exposure = exposure,
                                             weights = weights)
    combined <- simulateDirect(combined = combined,
                               tempfile = tempfile,
                               nDraw = nDraw,
                               useC = useC)
    seed <- list(.Random.seed)
    control.args <- list(call = call,
                         parallel = FALSE,
                         lengthIter = length(extractValues(combined)),
                         nUpdateMax = 1L)
    results <- makeResultsModelSimDirect(combined = combined,
                                         nDraw = nDraw,
                                         controlArgs = control.args,
                                         seed = seed)
    makeResultsFile(filename = filename,
                    results = results,
                    tempfiles = tempfile)
    rescaleInFile(filename)
    finalMessage(filename = filename,
                 verbose = verbose)
}



#' Create a synthetic demographic account
#'
#' WARNING - THIS FUNCTION IS STILL UNDER DEVELOPMENT
#' 
#' @inheritParams estimateAccount
#' @export
simulateAccount <- function(account, systemModels,
                            datasets = list(), dataModels = list(), 
                            concordances = list(), weights = list(),
                            dominant = c("Female", "Male"), scaleNoise = 0,
                            filename = NULL, nBurnin = 1000, nSim = 1000,
                            nChain = 4, nThin = 1,
                            parallel = TRUE, nCore = NULL,
                            outfile = NULL, nUpdateMax = 50,
                            verbose = TRUE, useC = TRUE) {
    call <- match.call()
    methods::validObject(account)
    dominant <- match.arg(dominant)
    checkNonNegativeNumeric(x = scaleNoise,
                            name = "scaleNoise")
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
        seriesIndices <- integer()
        namesDatasets <- character()
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
    ## initial values - unlike with estimateAccount,
    ## all chains start from same initial values
    combined <- initialCombinedAccountSimulate(account = account,
                                               systemModels = systemModels,
                                               systemWeights = systemWeights,
                                               dataModels = dataModels,
                                               seriesIndices = seriesIndices,
                                               datasets = datasets,
                                               namesDatasets = namesDatasets,
                                               transforms = transforms,
                                               dominant = dominant,
                                               scaleNoise = scaleNoise)
    combineds <- rep(list(combined),
                     times = mcmc.args$nChain)
    parallel <- control.args$parallel
    tempfiles <- paste(filename, seq_len(mcmc.args$nChain), sep = "_")
    MoreArgs <- c(list(seed = NULL),
                  mcmc.args,
                  control.args,
                  list(useC = useC))
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



#' Add extra iterations to simulation
#'
#' WARNING - THIS FUNCTION IS STILL UNDER DEVELOPMENT
#' 
#' @inheritParams simulateAccount
#' @param scaleNoise Governs noise added to Metropolis-Hastings
#' ratio when updating accounts. Should only be used non-zero
#' when generating initial values. Currently experimental,
#' and may change.
#' @export
continueSimulation <- function(filename, nBurnin = NULL, nSim = 1000, nThin = NULL,
                               parallel = NULL, scaleNoise = 0,
                               outfile = NULL, verbose = FALSE,
                               useC = TRUE) {
    object <- fetchResultsObject(filename)
    checkNonNegativeNumeric(scaleNoise)
    if (methods::is(object, "ResultsAccount")) {
        n.chain <- object@mcmc[["nChain"]]
        for (i in seq_along(n.chain))
            object@final[[i]]@scaleNoise@.Data <- as.double(scaleNoise)
    }
    else {
        if (scaleNoise > 0)
            stop(gettextf("not simulating a demographic account, but '%s' is non-zero",
                          "scaleNoise"))
    }
    mcmc.args.old <- object@mcmc
    control.args <- object@control
    seed.old <- object@seed
    append <- is.null(nBurnin)
    if (append)
        nBurnin <- 0L
    if (is.null(nThin))
        nThin <- mcmc.args.old[["nThin"]]
    mcmc.args.new <- makeMCMCArgs(nBurnin = nBurnin,
                                  nSim = nSim,
                                  nChain = mcmc.args.old[["nChain"]],
                                  nThin = nThin)
    if (!is.null(parallel)) {
        checkLogical(x = parallel,
                     name = "parallel")
        control.args$parallel <- parallel
    }
    combineds <- object@final
    tempfiles.new <- paste(filename, "cont", seq_len(mcmc.args.new$nChain), sep = "_")
    MoreArgs <- c(mcmc.args.new, control.args, list(useC = useC))
    if (control.args$parallel) {
        if (is.null(outfile)) ## passing 'outfile' as an argument always causes redirection
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args.new$nChain))
        else
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args.new$nChain),
                                        outfile = outfile)
        parallel::clusterSetRNGStream(cl)
        final.combineds <- parallel::clusterMap(cl = cl,
                                                fun = simulateOneChain,
                                                seed = seed.old,
                                                tempfile = tempfiles.new,
                                                combined = combineds,
                                                MoreArgs = MoreArgs,
                                                SIMPLIFY = FALSE,
                                                USE.NAMES = FALSE)
        seed <- parallel::clusterCall(cl, function() .Random.seed)
        parallel::stopCluster(cl)
    }
    else {
        set.seed(seed.old[[1L]])
        final.combineds <- mapply(simulateOneChain,
                                  seed = seed.old,
                                  tempfile = tempfiles.new,
                                  combined = combineds,
                                  MoreArgs = MoreArgs,
                                  SIMPLIFY = FALSE,
                                  USE.NAMES = FALSE)
        seed <- list(.Random.seed)
    }
    if (append) {
        mcmc.args.new[["nBurnin"]] <- mcmc.args.old[["nBurnin"]]
        mcmc.args.new[["nSim"]] <- mcmc.args.old[["nSim"]] + mcmc.args.new[["nSim"]]
    }
    else {
        mcmc.args.new[["nBurnin"]] <- (mcmc.args.old[["nBurnin"]]
            + mcmc.args.old[["nSim"]]
            + mcmc.args.new[["nBurnin"]])
    }
    results <- makeResults(object = object,
                           finalCombineds = final.combineds,
                           mcmcArgs = mcmc.args.new,
                           controlArgs = control.args,
                           seed = seed)
    if (append) {
        tempfiles.old <- splitFile(filename = filename,
                                   nChain = mcmc.args.old[["nChain"]],
                                   nIteration = mcmc.args.old[["nIteration"]],
                                   lengthIter = control.args[["lengthIter"]])
        joinFiles(filenamesFirst = tempfiles.old,
                  filenamesLast = tempfiles.new)
        makeResultsFile(filename = filename,
                        results = results,
                        tempfiles = tempfiles.old)
    }
    else {
        makeResultsFile(filename = filename,
                        results = results,
                        tempfiles = tempfiles.new)
    }
    rescaleInFile(filename)
    finalMessage(filename = filename, verbose = verbose)    
}
