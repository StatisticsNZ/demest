              

## updateCombined
##   updateAccount
##   updateSystemModels
##   updateExpectedExposure
##   updateObservationModelsAccount

## identical for transition
setMethod("updateCombined",
          signature(object = "CombinedAccount"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedAccount_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  for (i in seq_len(nUpdate)) {
                      object <- updateAccount(object)
                      object <- updateSystemModels(object)
                      object <- updateExpectedExposure(object)
                      object <- updateObservationAccount(object)
                  }
                  object
              }
          })


              
updateSystemModels <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccount"))
    stopifnot(methods::validObject(combined))
    if (useC) {
        .Call(updateSystemModels_R, combined)
    }
    else {
        system.models <- combined@systemModels
        population <- combined@account@population
        components <- combined@account@components
        has.age <- combined@hasAge
        model.uses.exposure <- combined@modelUsesExposure
        transforms.exp.to.comp <- combined@transformsExpToComp
        transform.exp.to.births <- combined@transformExpToBirths
        i.births <- combined@iBirths
        model <- system.models[[1L]]
        model <- updateModelNotUseExp(model,
                                      y = population)
        system.models[[1L]] <- model.popn
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
                                       transform = transforms.exp.to.comp[i])
                model <- updateModelUseExp(model = model.comp,
                                           y = component,
                                           exposure = exposure)
            }
            else
                model <- updateModelNotUseExp(model = model,
                                              y = component)
            system.models[[i + 1L]] <- model
        }
        combined@systemModels <- system.models
        combined
    }
}

updateObservationModelsAccount <- function(combined) {
    observationModels <- combined@observationModels
    account <- combined@account
    population <- account@population
    components <- account@components
    series.indices <- combined@seriesIndices
    transforms <- combined@transformsDatasets
    for (i in seq_along(observationModels)) {
        model <- observationModels[[i]]
        dataset <- datasets[[i]]
        transform <- transforms[[i]]
        series.index <- series.indices[i]
        if (series.index == 0L)
            series <- population
        else
            series <- components[[series.index]]
        series.collapsed <- collapse(series, transform = transform)
        if (is(model, "Poisson"))
            series.collapsed <- toDouble(series.collapsed)
        model <- updateModelUseExp(model, y = dataset, exposure = series.collapsed)
        observationModels[[i]] <- model
    }
    combined@observationModels <- observationModels
    combined
}

## UPDATE VALUES ################################################################












updateExpectedExposure <- function(combined) {
    NULL
}



## HELPER FUNCTIONS ################################################










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


