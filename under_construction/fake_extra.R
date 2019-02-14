

## NEED TO CHANGE rhalft TO rhalftTrunc IN MANY OF THE drawPrior FUNCTIONS

#' @inheritParams estimateAccount
#' @export
simulateAccount <- function(account, systemModels, datasets, dataModels, 
                            concordances = list(), weights = list(),
                            dominant = c("Female", "Male"),
                            filename = NULL, nBurnin = 1000, nSim = 1000,
                            nChain = 4, nThin = 1, parallel = TRUE,
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
    if (!has.data.models) {
        if (length(datasets) > 0L)
            stop(gettext("there are datasets, but no data models"))
        seriesIndices <- list()
        transforms <- list()
    }
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
                                                  default = mcmc.args$nChain))
        else
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args$nChain),
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




## CHANGE THE LOWER-LEVEL CHECKING FUNCTIONS TO PROPER METHODS

checkSystemModelsSuitableForSimulation <- function(systemModels, account) {
    error.message <- "system model for '%s' not suitable for simulation : %s"
    population <- population(account)
    components <- components(account)
    series <- c(list(population = population),
                components)
    names <- names(series)
    for (i in seq_along(systemModels)) {
        model <- systemModels[[i]]
        y <- series[[i]]
        name <- names[i]
        val <- tryCatch(checkAllDimensionsHavePriors(model = model,
                                                     y = y),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
        val <- tryCatch(checkPriorsAreInformative(model),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
        val <- tryCatch(checkPriorSDInformative(model),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
    }
    NULL
}



checkDataModelsSuitableForSimulation <- function(dataModels, datasets,
                                                 namesDatasets) {
    error.message <- "data model for '%s' not suitable for simulation : %s"
    for (i in seq_along(dataModels)) {
        model <- dataModels[[i]]
        y <- datasets[[i]]
        name <- namesDatasets[i]
        val <- tryCatch(checkAllDimensionsHavePriors(model = model,
                                                     y = y),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
        val <- tryCatch(checkPriorsAreInformative(model),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
        val <- tryCatch(checkPriorSDInformative(model),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf(error.message, name, val$message))
    }
    NULL
}



drawDataModelsAccount <- function(object, useC = FALSE) {
    stopifnot(methods::validObject(combined))
    if (useC) {
        .Call(drawDataModelsAccount_R, combined)
    }
    else {
        data.models <- combined@dataModels
        datasets <- combined@datasets
        population <- combined@account@population
        components <- combined@account@components
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        for (i in seq_along(data.models)) {
            model <- data.models[[i]]
            dataset <- datasets[[i]]
            transform <- transforms[[i]]
            series.index <- series.indices[i]
            if (series.index == 0L)
                series <- population
            else
                series <- components[[series.index]]
            series.collapsed <- collapse(series, transform = transform)
            if (methods::is(model, "Poisson") || methods::is(model, "CMP"))
                series.collapsed <- toDouble(series.collapsed)
            model <- drawModelUseExp(model,
                                     y = dataset,
                                     exposure = series.collapsed)
            data.models[[i]] <- model
        }
        combined@dataModels <- data.models
        combined
    }
}
    


setMethod("drawCombined",
          signature(object = "CombinedAccountMovements"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
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
                  object <- updateAccount(object)
                  object <- updateSystemModels(object)
                  object <- updateExpectedExposure(object)
                  object <- drawDataModelsAccount(object)
                  object
              }
          })



## HAS_TESTS
setMethod("initialCombinedModelSimulate",
          signature(object = "SpecBinomialVarying"),
          function(object, y, exposure, weights) {
              model <- initialModel(object,
                                    y = y,
                                    exposure = exposure)
              model <- drawHyperParam(model)
              y <- setYToMissing(y)
              methods::new("CombinedModelBinomial",
                           model = model,
                           y = y,
                           exposure = exposure)
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




setMethod("drawModelUseExp",
          signature(object = "PoissonVaryingUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data) & (exposure@.Data == 0L)] == 0))
              stopifnot(all(y@.Data[!is.na(y@.Data) & (exposure@.Data == 0L)] == 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_PoissonVarying_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- drawPriors(object)
                  object <- drawBetas(object)
                  object <- drawSigma_Varying(object)
                  object <- updateTheta_PoissonVaryingUseExp(object,
                                                             y = y,
                                                             exposure = exposure)
                  object
              }
          })



test_that("drawModelUseExp works with PoissonVaryingUseExp", {
    initialModel <- demest:::initialModel
    drawModelUseExp <- demest:::drawModelUseExp
    drawPriors <- demest:::drawPriors
    drawBetas <- demest:::drawBetas
    drawSigma_Varying <- demest:::drawSigma_Varying
    updateTheta_PoissonVaryingUseExp <- demest:::updateTheta_PoissonVaryingUseExp
    spec <- Model(y ~ Poisson(mean ~ age + sex),
                  `(Intercept)` ~ ExchFixed(sd = 10), 
                  age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                  sex ~ ExchFixed(sd = 0.1),
                  priorSD = HalfT(scale = 0.2))
    y <- Counts(array(1L,
                      dim = 2:3,
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"))))
    exposure <- Counts(array(1:6,
                             dim = 2:3,
                             dimnames = list(sex = c("F", "M"),
                                             age = c("0-4", "5-9", "10+"))))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- initialModel(spec, y = y, exposure = exposure)
        y.tmp <- y
        y.tmp[] <- NA
        set.seed(seed)
        ans.obtained <- drawModelUseExp(object = model,
                                        y = y.tmp,
                                        exposure = exposure)
        set.seed(seed)
        ans.expected <- model
        ans.expected <- drawPriors(ans.expected)
        ans.expected <- drawBetas(ans.expected)
        ans.expected <- drawSigma_Varying(ans.expected)
        ans.expected <- updateTheta_PoissonVaryingUseExp(ans.expected,
                                                         y = y.tmp,
                                                         exposure = exposure)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})



test_that("R and C versions of drawModelUseExp give same answer with PoissonVaryingUseExp", {
    initialModel <- demest:::initialModel
    drawModelUseExp <- demest:::drawModelUseExp
    drawPriors <- demest:::drawPriors
    drawBetas <- demest:::drawBetas
    drawSigma_Varying <- demest:::drawSigma_Varying
    updateTheta_PoissonVaryingUseExp <- demest:::updateTheta_PoissonVaryingUseExp
    spec <- Model(y ~ Poisson(mean ~ age + sex),
                  `(Intercept)` ~ ExchFixed(sd = 10), 
                  age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                  sex ~ ExchFixed(sd = 0.1),
                  priorSD = HalfT(scale = 0.2))
    y <- Counts(array(1L,
                      dim = 2:3,
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"))))
    exposure <- Counts(array(1:6,
                             dim = 2:3,
                             dimnames = list(sex = c("F", "M"),
                                             age = c("0-4", "5-9", "10+"))))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- initialModel(spec, y = y, exposure = exposure)
        y.tmp <- y
        y.tmp[] <- NA
        set.seed(seed)
        ans.R <- drawModelUseExp(object = model,
                                 y = y.tmp,
                                 exposure = exposure,
                                 useC = FALSE)
        ans.C.generic <- drawModelUseExp(object = model,
                                         y = y.tmp,
                                         exposure = exposure,
                                         useC = TRUE,
                                         useSpecific = FALSE)
        ans.C.specific <- drawModelUseExp(object = model,
                                          y = y.tmp,
                                          exposure = exposure,
                                          useC = TRUE,
                                          useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})



setMethod("drawModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelNotUseExp_PoissonVarying_R, object, y, exposure)
                  else
                      .Call(drawModelNotUseExp_R, object, y, exposure)
              }
              else {
                  object <- drawPriors(object)
                  object <- drawBetas(object)
                  object <- drawSigma_Varying(object)
                  object <- updateTheta_PoissonVaryingNotUseExp(object,
                                                                y = y)
                  object
              }
          })


test_that("drawModelNotUseExp works with PoissonVaryingNotUseExp", {
    initialModel <- demest:::initialModel
    drawModelNotUseExp <- demest:::drawModelNotUseExp
    drawPriors <- demest:::drawPriors
    drawBetas <- demest:::drawBetas
    drawSigma_Varying <- demest:::drawSigma_Varying
    updateTheta_PoissonVaryingNotUseExp <- demest:::updateTheta_PoissonVaryingNotUseExp
    spec <- Model(y ~ Poisson(mean ~ age + sex, useExpose = FALSE),
                  `(Intercept)` ~ ExchFixed(sd = 10), 
                  age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                  sex ~ ExchFixed(sd = 0.1),
                  priorSD = HalfT(scale = 0.2))
    y <- Counts(array(1L,
                      dim = 2:3,
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"))))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- initialModel(spec, y = y, exposure = NULL)
        y.tmp <- y
        y.tmp[] <- NA
        set.seed(seed)
        ans.obtained <- drawModelNotUseExp(object = model,
                                           y = y.tmp)
        set.seed(seed)
        ans.expected <- model
        ans.expected <- drawPriors(ans.expected)
        ans.expected <- drawBetas(ans.expected)
        ans.expected <- drawSigma_Varying(ans.expected)
        ans.expected <- updateTheta_PoissonVaryingNotUseExp(ans.expected,
                                                            y = y.tmp)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawModelNotUseExp give same answer with PoissonVaryingNotUseExp", {
    initialModel <- demest:::initialModel
    drawModelNotUseExp <- demest:::drawModelNotUseExp
    drawPriors <- demest:::drawPriors
    drawBetas <- demest:::drawBetas
    drawSigma_Varying <- demest:::drawSigma_Varying
    updateTheta_PoissonVaryingNotUseExp <- demest:::updateTheta_PoissonVaryingNotUseExp
    spec <- Model(y ~ Poisson(mean ~ age + sex),
                  `(Intercept)` ~ ExchFixed(sd = 10), 
                  age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                  sex ~ ExchFixed(sd = 0.1),
                  priorSD = HalfT(scale = 0.2))
    y <- Counts(array(1L,
                      dim = 2:3,
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"))))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- initialModel(spec, y = y, exposure = NULL)
        y.tmp <- y
        y.tmp[] <- NA
        set.seed(seed)
        ans.R <- drawModelNotUseExp(object = model,
                                    y = y.tmp,
                                    useC = FALSE)
        ans.C.generic <- drawModelNotUseExp(object = model,
                                            y = y.tmp,
                                            useC = TRUE,
                                            useSpecific = FALSE)
        ans.C.specific <- drawModelNotUseExp(object = model,
                                             y = y.tmp,
                                             useC = TRUE,
                                             useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.generic, ans.C.specific)
    }
})

