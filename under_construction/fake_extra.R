
#' @inheritParams estimateAccount
#' @export
simulateAccount <- function(account, systemModels, datasets, dataModels, 
                            concordances = list(), weights = list(),
                            dominant = c("Female", "Male"),
                            filename = NULL, nDraw = 1,
                            nBurnin = 1000, nSim = 1000,
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
    combineds <- replicate(n = mcmc.args$nChain,
                           initialCombinedAccountSimulate(account = account,
                                                          systemModels = systemModels,
                                                          systemWeights = systemWeights,
                                                          dataModels = dataModels,
                                                          seriesIndices = seriesIndices,
                                                          datasets = datasets,
                                                          namesDatasets = namesDatasets,
                                                          transforms = transforms,
                                                          dominant = dominant))
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



setMethod("drawSystemModels",
          signature(combined = "CombinedAccountMovements"),
          function(combined, isFirst, useC = FALSE) {
              ## combined
              methods::validObject(combined)
              ## isFirst
              stopifnot(identical(length(isFirst), 1L))
              stopifnot(is.logical(isFirst))
              stopifnot(!is.na(isFirst))
              if (useC) {
                  .Call(drawSystemModels_R, combined, isFirst)
              }
              else {
                  system.models <- combined@systemModels
                  population <- combined@account@population
                  components <- combined@account@components
                  model.uses.exposure <- combined@modelUsesExposure
                  transforms.exp.to.comp <- combined@transformsExpToComp
                  transform.exp.to.births <- combined@transformExpToBirths
                  i.births <- combined@iBirths
                  ## population
                  model <- system.models[[1L]]
                  model <- drawModelNotUseExp(model,
                                              y = population,
                                              isFirst = isFirst)
                  system.models[[1L]] <- model
                  ## components
                  for (i in seq_along(components)) {
                      model <- system.models[[i + 1L]]
                      component <- components[[i]]
                      uses.exposure <- model.uses.exposure[i + 1L]
                      if (uses.exposure) {
                          exposure <- combined@exposure@.Data
                          is.births <- i == i.births
                          if (is.births)
                              exposure <- collapse(exposure,
                                                   transform = transform.exp.to.births)
                          transform <- transforms.exp.to.comp[[i]]
                          if (!is.null(transform))
                              exposure <- extend(exposure,
                                                 transform = transforms.exp.to.comp[[i]])
                          model <- drawModelUseExp(object = model,
                                                   y = component,
                                                   exposure = exposure)
                      }
                      else {
                          if (methods::is(model, "Normal"))
                              component <- toDouble(component)
                          model <- drawModelNotUseExp(object = model,
                                                      y = component)
                      }
                      system.models[[i + 1L]] <- model
                  }
                  combined@systemModels <- system.models
                  combined
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
                  object <- drawSystemModels(object)
                  object <- updateExpectedExposure(object)
                  object <- updateAccount(object)

                  
                  object <- drawDataModelsAccount(object)
                  object
              }
          })




## NEED TO TURN 'drawHyperParam' INTO A METHOD

setMethod("initialCombinedAccountSimulate",
          signature(account = "Movements",
                    systemModels = "list",
                    systemWeights = "list",
                    dataModels = "list",
                    seriesIndices = "integer",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(account, systemModels, systemWeights,
                   dataModels, seriesIndices, 
                   datasets, namesDatasets, transforms,
                   dominant = c("Female", "Male")) {
              combined <- initialCombinedAccount(account = account,
                                                 systemModels = systemModels,
                                                 systemWeights = systemWeights,
                                                 dataModels = dataModels,
                                                 seriesIndices = seriesIndices, 
                                                 datasets = datasets,
                                                 namesDatasets = namesDatasets,
                                                 transforms = transforms,
                                                 dominant = dominant)
              systemModels <- combined@systemModels
              dataModels <- combined@dataModels
              datasets <- combined@datasets
              systemModels <- lapply(systemModels, drawHyperParam)
              dataModels <- lapply(dataModels, drawHyperParam)
              datasets <- lapply(datasets, setYToMissing)
              combined@systemModels <- systemModels
              combined@dataModels <- dataModels
              combined@datasets <- datasets
              combined              
          })












