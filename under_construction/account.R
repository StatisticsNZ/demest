

checkSystemModels <- function(models, needsNonDefaultSeriesArg = FALSE) {
    ## 'systemModels' is a list
    if (!is.list(systemModels))
        stop(gettextf("'%s' has class \"%s\"",
                      "systemModels", class(systemModels)))
    for (i in seq_along(systemModels)) {
        spec <- systemModels[[i]]
        ## all elements have class "SpecModel"
        if (!methods::is(spec, "SpecModel"))
            stop(gettextf("element %d of '%s' has class \"%s\"",
                          i, "systemModels", class(spec)))
        ## element has name
        if (is.na(spec@nameY@.Data) || !nzchar(spec@nameY@.Data))
            stop(gettextf("element %d of '%s' has no name for response variable",
                          i, "systemModels"))
        ## specification of spec is valid
        return.value <- tryCatch(methods::validObject(spec),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("error in system model for '%s' : %s",
                          spec@nameY@.Data, return.value$message))
        ## no 'series' argument supplied if not needed
        if (!identical(spec@series@.Data, "y"))
            warning(gettextf("non-default argument for '%s' in system model for '%s' ignored",
                             "series", spec@nameY))
    }
    NULL
}


alignSystemModelsToAccount <- function(systemModels, account) {
    names.components <- account@namesDatasets
    names.series <- c("population", names.components)
    ans <- vector(mode = "list", length = length(names.series))
    for (i in seq_along(systemModels)) {
        spec <- systemModels[[i]]
        name.y <- spec@nameY
        i.series <- match(name.y, names.series, nomatch = 0L)
        has.series <- i.series > 0L
        if (has.series)
            ans[[i.series]] <- spec
        else
            stop(gettextf("'%s' has a model for '%s' but '%s' does not have a series called '%s'",
                          "systemModels", name.y, "account", name.y))
    }
    is.series.without.model <- sapply(ans, is.null)
    if (any(is.series.without.model)) {
        i.missing <- which(is.series.without.model)[1L]
        stop(gettextf("'%s' does not have a model for series '%s' in '%s'",
                      "systemModels", names.series[i.missing], "account"))
    }
    ans
}


makeTransformAccountToDatasets <- function(account, observationModels, datasets) {
    population <- account@population
    components <- account@components
    series <- c(list(population), components)
    names.components <- account@namesDatasets
    names.series <- c("population", names.components)
    names.datasets <- names(datasets)
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        return.value <- tryCatch(dembase::canMakeCompatible(x = y, y = dataset, subset = TRUE),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("unable to collapse '%s' to make it compatible with dataset '%s' : %s",
                          nameY, names.datasets[i], return.value$message))
        transform <- dembase::makeTransform(x = y, y = dataset, subset = TRUE, check = FALSE)
        transform <- dembase::makeCollapseTransformExtra(transform)
        ans[[i]] <- transform
    }
    ans
}




estimateAccount <- function(y, systemModels, observationModels, datasets,
                            dominant = c("Female", "Male"),
                            filename = NULL,
                            nBurnin = 1000, nSim = 1000, nChain = 4, nThin = 1,
                            parallel = TRUE, nUpdateMax = 200,
                            verbose = FALSE, useC = TRUE) {
    call <- match.call()
    methods::validObject(y)
    checkSystemModels(systemModels)
    systemModels <- alignSystemModelsToAccount(systemModels = systemModels,
                                               account = account)
    checkObservation(observationModels, needsNonDefaultSeriesArg = TRUE)
    checkNamesDatasets(datasets)
    datasets <- alignDatasetsToObservation(datasets = datasets,
                                           observationModels = observationModels)
    ## check datasets after aligning to avoid checking datasets that are not needed
    datasets <- checkAndTidyDatasets(datasets)
    dominant <- match.arg(dominant)
    transforms <- makeTransformsAccountToDatasets(y = y, nameY = "y", datasets = datasets)
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


