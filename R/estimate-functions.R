
#' Estimate model from single reliable dataset.
#'
#' Estimate rates, counts, probabilities, or means for a single
#' \code{\link[dembase:DemographicArray-class]{demographic array}}.  The
#' demographic array is treated as observed without error.
#'
#' @section Model, y, and exposure:
#' 
#' The model for the contents of the array is specified using function
#' \code{\link{Model}}.
#'
#' If \code{model} specifies a Poisson, binomial, or multinomial model,
#' then \code{y} must have class
#' \code{\link[dembase:DemographicArray-class]{Counts}}.  If \code{model}
#' specifies a normal distribution, then \code{y} can have class
#' \code{\link[dembase:DemographicArray-class]{Counts}} or
#' \code{\link[dembase:DemographicArray-class]{Counts}}.
#'
#' \code{y} may include \code{NA}s.  Missing values can be imputed via function
#' function \code{\link{fetch}}.  If \code{model} specifies a Poisson
#' distribution, then \code{y} can also have known
#' \code{\link[dembase:attachSubtotals]{subtotals}}, which
#' can help with the imputation of the missing values.
#' 
#' An \code{exposure} term is optional in Poisson models, and required
#' in binomial models.  (For convenience, \code{demest} treats the sample size
#' parameter in binomial models as kind of exposure.)  A \code{weights} term
#' is optional in normal models.
#'
#' @section Output:
#'
#' The output from \code{estimateModel} would often be too large to fit into
#' memory.  \code{estimateModel} therefore departs from the standard R
#' behavior in the way it handles output.  Rather than returning an object
#' containing the output, \code{estimateModel} creates a file on disk,
#' somewhat like a database.
#'
#' The name and location of the output file is specified using the
#' \code{filename} argument. The file is just a text file, and can be copied
#' and moved.
#'
#' Users extract items from the file using function such as \code{\link{fetch}},
#' \code{\link{fetchSummary}}, \code{\link{fetchMCMC}}, and \code{\link{fetchFiniteSD}}.
#' 
#' Functions \code{\link{estimateCounts}}, \code{\link{estimateAccount}},
#' and \code{\link{predictModel}} follow the same strategy for returning
#' output.
#'
#' @section nBurnin, nSim, nChain, nThin:
#'
#' \code{estimateModel}, \code{\link{estimateCounts}}, and
#' \code{\link{estimateAccount}} use Markov chain Monte Carlo (MCMC)
#' methods for inference.  MCMC methods have two stages: burnin and
#' production.  During the burnin phase, the model moves from an
#' initial guess at the location of the posterior distribution
#' towards the true location.  During the production phase, if all goes
#' well, the model samples from the true posterior distribution.
#'
#' Parameter \code{nBurnin} specifies the number of iterations that the model
#' spendss moving away from its initial location. Parameter \code{nSim}
#' specifies the number of iterations that the model spends sampling from
#' the posterior distribution.
#'
#' Collecting every iteration during the production phase would lead to
#' huge output files.  Instead, the model collects only one in every
#' \code{nThin} iterations.  The resulting loss in information is relatively
#' small, since successive iterations are typically highly correlated.
#'
#' The calculations are run \code{nChain} times, with each chain yielding
#' a different sample.  As described in the documentation for
#' \code{\link{fetchMCMC}}, comparing the samples is a way of checking whether the
#' model has found the posterior distribution. When each chain seems to be
#' sampling from the same distribution, the model is said to have
#' converged.
#'
#' At the end of the estimation process, the \code{estimateModel} and similar
#' functions pool the results from all the chains to form a single sample.
#' This sample has \code{floor(nChain * nSim / nThin)} iterations.
#'
#' @section Parallel:
#'
#' Because each chain is run independently, the calculations can be carried out
#' in parallel.  By default, \code{estimateModel}, \code{\link{estimateCounts}},
#' and \code{\link{estimateAccount}} run the calculations on separate cores,
#' using functions from the \pkg{parallel} package. For portability, however,
#' the \code{parallel} argument is set to \code{FALSE} in the examples in the
#' online help.
#' 
#' @param model An object of class \code{\linkS4class{SpecModel}},
#' specifying the model to be fit.
#' @param y A \code{\link[dembase:DemographicArray-class]{demographic array}}
#' holding the outcome data.
#' @param exposure A \code{\link[dembase:DemographicArray-class]{Counts}}
#' object specifying exposure or sample size.
#' @param weights A \code{\link[dembase:DemographicArray-class]{Counts}}
#' object containing weights.
#' @param filename The name of a file where output is collected.  
#' @param nBurnin Number of iteration discarded before recording begins.
#' @param nSim Number of iterations carried out during recording.
#' @param nChain Number of parallel chains used in simulations.
#' @param nThin Thinning interval.
#' @param parallel Logical.  If \code{TRUE} (the default), parallel processing
#' is used.
#' @param verbose Logical.  If \code{TRUE} (the default) a message is
#' printed at the end of the calculations.
#'
#' @seealso \code{\link{estimateCounts}} is similar to \code{estimateModel},
#' except that \code{y} is not observed directly, but must be inferred from
#' multiple noisy datasets. \code{\link{estimateAccount}} infers a demographic
#' account from multiple noisy datasets.  Calculations can be extended using
#' \code{\link{continueEstimation}}.  Forecasts based on the results
#' from \code{estimateModel} can be constructed using function
#' \code{\link{predictModel}}.
#'
#' @references Gelman, A., Carlin, J. B., Stern, H. S.,
#' Dunson, D. B., Vehtari, A., Rubin, D. B. (2013)
#' \emph{Bayesian Data Analysis. Third Edition}. Boca Raton: Chapman &
#' Hall/CRC.
#'
#' @examples
#' library(datasets)
#' admissions <- Counts(UCBAdmissions)
#' admitted <- subarray(admissions, Admit == "Admitted")
#' filename <- tempfile()
#' estimateModel(Model(y ~ Binomial(mean ~ Gender + Dept)),
#'               y = admitted,
#'               exposure = admissions,
#'               file = filename,
#'               nBurnin = 50,
#'               nSim = 50,
#'               nChain = 2,
#'               nThin = 2)
#' fetchSummary(filename)
#' @export
estimateModel <- function(model, y, exposure = NULL, weights = NULL,
                          filename = NULL, nBurnin = 1000, nSim = 1000,
                          nChain = 4, nThin = 1, parallel = TRUE,
                          verbose = FALSE) {
    call <- match.call()
    methods::validObject(model)
    mcmc.args <- makeMCMCArgs(nBurnin = nBurnin,
                              nSim = nSim,
                              nChain = nChain,
                              nThin = nThin)
    if (is.null(filename))
        filename <- tempfile()
    control.args <- makeControlArgs(call = call,
                                    parallel = parallel)
    y <- checkAndTidyY(y)
    y <- castY(y = y,
               spec = model)
    checkForSubtotals(object = y,
                      model = model,
                      name = "y")
    exposure <- checkAndTidyExposure(exposure = exposure,
                                     y = y)
    exposure <- castExposure(exposure = exposure,
                             model = model)
    weights <- checkAndTidyWeights(weights = weights,
                                   y = y)
    combineds <- replicate(n = mcmc.args$nChain,
                           initialCombinedModel(model,
                                                y = y,
                                                exposure = exposure,
                                                weights = weights))
    parallel <- control.args$parallel
    tempfiles <- sapply(seq_len(mcmc.args$nChain),
                        function(x) paste(control.args$filename, x, sep = "_"))
    MoreArgs <- c(list(seed = NULL),
                  mcmc.args,
                  control.args,
                  list(continuing = FALSE))
    if (parallel) {
        cl <- parallel::makeCluster(getOption("cl.cores",
                                              default = mcmc.args$nChain))
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
    results <- makeResultsModelEst(finalCombineds = final.combineds,
                                   mcmcArgs = mcmc.args,
                                   controlArgs = control.args,
                                   seed = seed)
    makeResultsFile(filename = filename,
                    results = results,
                    tempfiles = tempfiles)
    finalMessage(filename = filename, verbose = verbose)
}


#' Use results from function estimateModel to make predictions.
#'
#' Typically predict along time dimension, but can predict along other
#' dimenions.  When predicting along time dimension, typically predict
#' forwards, but can predict backwards.
#'
#' If an \code{along} argument is not supplied, \code{predictModel} looks
#' for a dimension with \code{\link[dembase]{dimtype}} \code{"time"}, and,
#' failing that, a dimension with \code{\link[dembase]{dimtype}} \code{"age"},
#' or \code{"cohort"}.
#'
#' When predicting along \code{"time"}, \code{"age"} or \code{"cohort"}
#' dimensions, specifying an \code{n} argument is usually more convenient
#' than specifying a \code{labels} argument.
#' 
#' @inheritParams estimateModel
#' @param filenameEst Filename used to \code{\link{estimateModel}}.
#' @param filenamePred Filename to be used by \code{\link{predictModel}}.
#' @param along Name or index along which prediction should occur. 
#' @param labels Labels of new categories.
#' @param n Number of new categories.  Can only be used when predicting
#' along \code{"time"}, \code{"age"}, or \code{"cohort"} dimensions,
#' and when the units have equal length.
#' @param data A named list containing future values of covariates.
#' @param aggregate An object of class \code{\linkS4class{SpecAggregate}}.
#' @param lower A lower bound for estimates of data-level rates,
#' probabilities, or means.
#' @param upper An upper bound for estimates of data-level
#' rate, probabilities, or means.
#' @export
predictModel <- function(filenameEst, filenamePred, along = NULL, labels = NULL, n = NULL,
                         data = NULL, aggregate = NULL, lower = NULL,
                         upper = NULL, nBurnin = 0L,  parallel = TRUE,
                         verbose = FALSE) {
    if (!identical(nBurnin, 0L))
        stop("'nBurnin' must currently be 0L")
    call <- match.call()
    results.first <- fetchResultsObject(filenameEst)
    ## extract information about old results
    combined.first <- results.first@final[[1L]]
    mcmc.args.first <- results.first@mcmc
    control.args.first <- results.first@control
    model.first <- combined.first@model
    metadata.first <- model.first@metadataY
    y.first <- combined.first@y
    y.is.counts <- methods::is(y.first, "Counts")
    ## set up new objects
    along <- dembase::checkAndTidyAlong(along = along,
                                        metadata = metadata.first,
                                        numericDimScales = FALSE)
    if(is.null(filenamePred))
        filenamePred <- tempfile()
    checkDataPredict(data)
    if (!(is.null(aggregate) || methods::is(aggregate, "SpecAggregate")))
        stop(gettextf("'%s' has class \"%s\"",
                      "aggregate", class(aggregate)))
    combined.pred <- initialCombinedModelPredict(combined = combined.first,
                                                 along = along,
                                                 labels = labels,
                                                 n = n,
                                                 covariates = data,
                                                 aggregate = aggregate,
                                                 lower = lower,
                                                 upper = upper,
                                                 yIsCounts = y.is.counts)
    control.args.pred <- list(call = call,
                              parallel = parallel,
                              lengthIter = lengthValues(combined.pred))
    mcmc.args.pred <- list(nBurnin = nBurnin,
                           nSim = mcmc.args.first[["nSim"]],
                           nChain = mcmc.args.first[["nChain"]],
                           nThin = mcmc.args.first[["nThin"]],
                           nIteration = mcmc.args.first[["nIteration"]])
    tempfiles.first <- splitFile(filename = filenameEst,
                                 nChain = mcmc.args.first[["nChain"]],
                                 nIteration = mcmc.args.first[["nIteration"]],
                                 lengthIter = control.args.first[["lengthIter"]])
    tempfiles.pred <- sapply(seq_len(mcmc.args.pred[["nChain"]]),
                             function(x) paste(filenamePred, x, sep = "_"))
    n.iter.chain <- mcmc.args.first[["nIteration"]] / mcmc.args.first[["nChain"]]
    if (parallel) {
        cl <- parallel::makeCluster(getOption("cl.cores", default = mcmc.args.pred$nChain))
        parallel::clusterSetRNGStream(cl)
        final.combineds <- parallel::clusterMap(cl = cl,
                                                fun = predictOneChain,
                                                combined = list(combined.pred),
                                                tempfileOld = tempfiles.first,
                                                tempfileNew = tempfiles.pred,
                                                lengthIter = control.args.first[["lengthIter"]],
                                                nIteration = n.iter.chain,
                                                nUpdate = nBurnin,
                                                SIMPLIFY = FALSE,
                                                USE.NAMES = FALSE)
        seed <- parallel::clusterCall(cl, function() .Random.seed)
        parallel::stopCluster(cl)
    }
    else {
        final.combineds <- mapply(predictOneChain,
                                  combined = list(combined.pred),
                                  tempfileOld = tempfiles.first,
                                  tempfileNew = tempfiles.pred,
                                  lengthIter = control.args.first[["lengthIter"]],
                                  nIteration = n.iter.chain,
                                  nUpdate = nBurnin,
                                  SIMPLIFY = FALSE,
                                  USE.NAMES = FALSE)
        seed <- list(.Random.seed)
    }
    sapply(tempfiles.first, unlink)
    results <- makeResultsModelPred(finalCombineds = final.combineds,
                                    mcmcArgs = mcmc.args.pred,
                                    controlArgs = control.args.pred,
                                    seed = seed)
    makeResultsFile(filename = filenamePred,
                    results = results,
                    tempfiles = tempfiles.pred)
    finalMessage(filename = filenamePred, verbose = verbose)
}


#' Estimate counts and model from multiple noisy datasets.
#'
#' Infer the contents of a demographic array, and fit a model describing
#' the array, using multiple noisy datasets.
#'
#' See the documentation for \code{\link{estimateModel}} for details on
#' model output and on MCMC settings.
#'
#' \code{observation} is a list of specificiations for data models,
#' and \code{datasets} is a named list of datasets.  The response for each
#' data model must be the name of a dataset.  See below for examples.
#' 
#' 
#' @inheritParams estimateModel
#' @param y An object of class
#' \code{\link[dembase:DemographicArray-class]{Counts}} with the same
#' structure as the counts to be estimated.
#' @param observation A list of objects of class
#' \code{\linkS4class{SpecModel}} describing the relationship between 
#' the datasets and counts.  
#' @param datasets A named list of objects of class
#' \code{\link[dembase:DemographicArray-class]{Counts}}.
#'
#' @seealso \code{\link{estimateModel}}, \code{\link{estimateAccount}}
#' 
#' @examples
#' nat <- demdata::sim.admin.nat
#' health <- demdata::sim.admin.health
#' survey <- demdata::sim.admin.survey
#' nat <- Counts(nat, dimscales = c(year = "Points"))
#' health <- Counts(health, dimscales = c(year = "Points"))
#' survey <- Counts(survey)
#' y <- health + 10
#' model <- Model(y ~ Poisson(mean ~ age + sex + region))
#' observation <- list(Model(nat ~ PoissonBinomial(prob = 0.98)),
#'                     Model(health ~ Poisson(mean ~ age)),
#'                     Model(survey ~ Binomial(mean ~ 1)))
#' datasets <- list(nat = nat, health = health, survey = survey)
#' filename <- tempfile()
#' ## in a real example, nBurnin and nSim would be much larger
#' \dontrun{
#' estimateCounts(model = model,
#'                y = y,
#'                observation = observation,
#'                datasets = datasets,
#'                filename = filename,
#'                nBurnin = 50,
#'                nSim = 50,
#'                nThin = 2,
#'                nChain = 2,
#'                parallel = FALSE)
#' fetchSummary(filename)
#' }
#' @export
estimateCounts <- function(model, y, exposure = NULL, observation,
                           datasets, filename = NULL, nBurnin = 1000,
                           nSim = 1000, nChain = 5, nThin = 1,
                           parallel = TRUE, verbose = FALSE) {
    call <- match.call()
    methods::validObject(model)
    mcmc.args <- makeMCMCArgs(nBurnin = nBurnin,
                              nSim = nSim,
                              nChain = nChain,
                              nThin = nThin)
    if (is.null(filename))
        filename <- tempfile()
    control.args <- makeControlArgs(call = call, parallel = parallel)
    y <- checkAndTidyY(y)
    y <- dembase::toInteger(y)
    checkForSubtotals(object = y, model = model, name = "y")
    exposure <- checkAndTidyExposure(exposure = exposure, y = y)
    exposure <- castExposure(exposure = exposure, model = model)
    checkObservation(observation, needsNonDefaultSeriesArg = FALSE)
    checkNamesDatasets(datasets)
    datasets <- alignDatasetsToObservation(datasets = datasets,
                                           observation = observation)
    ## check of datasets comes after aligning,
    ## to avoid checking datasets that are not needed
    datasets <- checkAndTidyDatasets(datasets)
    transforms <- makeTransformsYToDatasets(y = y, nameY = "y", datasets = datasets)
    namesDatasets <- names(datasets)
    names(datasets) <- NULL
    combineds <- replicate(n = mcmc.args$nChain,
                           initialCombinedCounts(model,
                                                 y = y,
                                                 exposure = exposure,
                                                 observation = observation,
                                                 datasets = datasets,
                                                 namesDatasets = namesDatasets,
                                                 transforms = transforms))
    parallel <- control.args$parallel
    tempfiles <- sapply(seq_len(mcmc.args$nChain),
                        function(x) paste(control.args$filename, x, sep = "_"))
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
    results <- makeResultsCounts(finalCombineds = final.combineds,
                                 mcmcArgs = mcmc.args,
                                 controlArgs = control.args,
                                 seed = seed)
    makeResultsFile(filename = filename,
                    results = results,
                    tempfiles = tempfiles)
    finalMessage(filename = filename, verbose = verbose)
}

#' Use results from function estimateCounts to make predictions.
#'
#' Not written yet.
#' 
#' @inheritParams predictModel
#'
#' @export
predictCounts <- function(filenameEst, filenamePred, along = NULL, labels = NULL, n = NULL,
                          data = NULL, aggregate = NULL, lower = NULL,
                          upper = NULL, nBurnin = 0L,  parallel = TRUE,
                          verbose = FALSE) {
    stop("not written yet")
}

#' Estimate demographic account and models from multiple noisy datasets.
#'
#' Infer the contents of a demographic account, and fit models
#' describing series within the account, using multiple noisy datasets.
#' \emph{The function is still under construction.}
#'
#' @inheritParams estimateCounts
#' @param y An object of class
#' \code{\link[dembase:DemographicAccount-class]{DemographicAccount}}.
#' @param system A list of objects of class \code{\linkS4class{SpecModel}}
#' specifying models for the demographic series.
#' @param observation A list of objects of class
#' \code{\linkS4class{SpecModel}} specifying models for the datasets.
#'
#' @seealso \code{\link{estimateModel}}, \code{\link{estimateCounts}}
#'
#' @references Bryant, J., Graham, P. Bayesian demographic accounts:
#' Subnational population estimation using multiple datasources. 2013.
#' \emph{Bayesian Analysis}
#' @export
estimateAccount <- function(y, system, observation, datasets, filename = NULL,
                            nBurnin = 1000, nSim = 1000, nChain = 4, nThin = 1,
                            parallel = TRUE, verbose = FALSE) {
    stop("not written yet")
}


#' Use results from function estimateAccount to make predictions.
#'
#' Not written yet.
#' 
#' @inheritParams predictModel
#'
#' @export
predictAccount <- function(filenameEst, filenamePred, along = NULL, labels = NULL, n = NULL,
                           data = NULL, aggregate = NULL, lower = NULL,
                           upper = NULL, nBurnin = 0L,  parallel = TRUE,
                           verbose = FALSE) {
    stop("not written yet")
}


##     call <- match.call()
##     methods::validObject(account)
##     checkSystem(system)
##     system <- alignSystemToAccount(system = system,
##                                    account = account)
##     checkObservation(observation, needsNonDefaultSeriesArg = TRUE)
##     checkNamesDatasets(datasets)
##     datasets <- alignDatasetsToObservation(datasets = datasets,
##                                            observation = observation)
##     ## check datasets after aligning to avoid checking datasets that are not needed
##     datasets <- checkAndTidyDatasets(datasets)
##     transforms <- makeTransformsYToDatasets(y = y, nameY = "y", datasets = datasets)
##     namesDatasets <- names(datasets)
##     names(datasets) <- NULL
##     mcmc.args <- makeMCMCArgs(nBurnin = nBurnin,
##                               nSim = nSim,
##                               nChain = nChain,
##                               nThin = nThin)
##     if (is.null(filename))
##         filename <- tempfile()
##     control.args <- makeControlArgs(call = call,
##                                     filename = filename,
##                                     parallel = parallel)
##     combineds <- replicate(n = mcmc.args$nChain,
##                            initialCombinedAccount(account = account,
##                                                   system = system,
##                                                   observation = observation,
##                                                   datasets = datasets,
##                                                   namesDatasets = namesDatasets,
##                                                   transforms = transforms))
##     parallel <- control.args$parallel
##     tempfiles <- sapply(seq_len(mcmc.args$nChain),
##                         function(x) paste(control.args$filename, x, sep = "_"))
##     MoreArgs <- c(list(seed = NULL),
##                   mcmc.args,
##                   control.args,
##                   list(continuing = FALSE))
##     if (parallel) {
##         cl <- parallel::makeCluster(getOption("cl.cores", default = mcmc.args$nChain))
##         parallel::clusterSetRNGStream(cl)
##         final.combineds <- parallel::clusterMap(cl = cl,
##                                       fun = estimateOneChain,
##                                       tempfile = tempfiles,
##                                       combined = combineds,
##                                       MoreArgs = MoreArgs,
##                                       SIMPLIFY = FALSE,
##                                       USE.NAMES = FALSE)
##         seed <- parallel::clusterCall(cl, function() .Random.seed)
##         parallel::stopCluster(cl)
##     }
##     else {
##         final.combineds <- mapply(estimateOneChain,
##                                   tempfile = tempfiles,
##                                   combined = combineds,
##                                   MoreArgs = MoreArgs,
##                                   SIMPLIFY = FALSE,
##                                   USE.NAMES = FALSE)
##         seed <- list(.Random.seed)
##     }
##     control.args$lengthIter <- length(extractValues(final.combineds[[1L]]))
##     results <- makeResultsAccount(finalCombineds = final.combineds,
##                                   mcmcArgs = mcmc.args,
##                                   controlArgs = control.args,
##                                   seed = seed)
##     makeResultsFile(filename = control.args$filename,
##                     results = results,
##                     tempfiles = tempfiles)
##     finalMessage(filename = filename, verbose = verbose)
## }




#' Add extra iterations to burnin or output.
#' 
#' Continue estimation process, retaining current settings, but extending
#' the burnin or sampling from the posterior distribution.
#' \code{continueEstimation} is called after \code{\link{estimateModel}},
#' \code{\link{estimateCounts}}, or \code{\link{estimateAccount}},
#' and can be called multiple times.
#'
#' The treatment of output from previous calls to the estimation functions or
#' to \code{continueEstimation} depends on whether \code{nBurnin} in the
#' current call to \code{continueEstimation} is equal to 0 or greater than 0.
#' If \code{nBurnin} is equal to 0, then any further iterations are added to
#' the current posterior sample.  If \code{nBurnin} is greater than
#' 0, then any previous output is treated as part of the burnin, and the
#' construction of the posterior sample begins again from scratch.
#' See below for an example.
#'
#' Because model output includes the state of the random number generator,
#' it should be possible to obtain identical results by (i) calling an estimation
#' function followed by one or more calls to \code{continueEstimation},
#' and (ii) doing all the calculations in one call to an estimaton function.
#' For instance,
#' \itemize{
#'   \item \code{estimateModel} with \code{nBurnin = 2000} and \code{nSim = 0}
#'   \item \code{continueEstimation} with \code{nSim = 1000}
#'   \item \code{continueEstimation} with \code{nSim = 1000}
#' }
#' should give the same results as
#' \itemize{
#'   \item \code{estimateModel} with \code{nBurnin = 2000} and \code{nSim = 2000}.
#' }
#'
#' Note that the total size of the posterior sample depends not just on
#' \code{nSim}, but also on \code{nChain} and \code{nThin}.  In the simplest
#' case (ie a single call to an estimation function, and \code{nSim * nChain}
#' divisible by \code{nThin}) the number of iterations in the sample equals
#' \code{nSim * nChain / nThin}.
#' 
#' @inheritParams estimateModel
#' @param filename The filename used by the original call.
#' @param nSim Number of additional iterations.
#' 
#' @seealso \code{continueEstimation} is used together with
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' and \code{\link{estimateAccount}}.  
#' 
#' @references \code{continueEstimation} was inspired by the discussion of
#' checkpointing in Geyer, C. 2011. Introduction to Markov chain Monte Carlo.
#' Brooks, S., Gelman, A., Jones, G. L., and Meng, X-L. (eds.)
#' \emph{Handbook of Markov Chain Monte Carlo}. Chapman & Hall/CRC.
#'
#' @examples
#' ## prepare data
#' deaths <- demdata::VADeaths2
#' popn <- demdata::VAPopn
#' deaths <- round(deaths)
#' deaths <- Counts(deaths)
#' popn <- Counts(popn)
#'
#' ## Show that estimation using a single call to
#' ## 'estimateModel' gives the same results as
#' ## a call to 'estimateModel' followed by a call
#' ## to 'continueEstimation'
#' 
#' ## estimate all at once
#' set.seed(1)
#' filename.all.at.once <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ age)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename.all.at.once,
#'               nBurnin = 20,
#'               nSim = 20,
#'               nThin = 2,
#'               nChain = 2,
#'               parallel = FALSE)
#' rates.all.at.once <- fetch(filename.all.at.once,
#'                            where = c("model", "likelihood", "rate"))
#'
#' ## estimate using 'continueEstimation'
#' set.seed(1)
#' filename.with.continue <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ age)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename.with.continue,
#'               nBurnin = 20,
#'               nSim = 0,
#'               nThin = 2,
#'               nChain = 2,
#'               parallel = FALSE)
#' continueEstimation(filename = filename.with.continue,
#'                    nBurnin = 0,
#'                    nSim = 20)
#' rates.with.continue <- fetch(filename.all.at.once,
#'                            where = c("model", "likelihood", "rate"))
#'
#' ## the two approaches give the same answer
#' all.equal(rates.all.at.once, rates.with.continue)
#' 
#'
#' ## Demonstrate the differences between nBurnin = 0
#' ## and nBurnin > 0.
#'
#' ## nBurnin = 0 in call to continueEstimation, so keep 
#' ## iterations from original call to estimateModel
#' filename.keep.original <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ age)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename.keep.original,
#'               nBurnin = 20,
#'               nSim = 10,
#'               nThin = 2,
#'               nChain = 2,
#'               parallel = FALSE)
#' continueEstimation(filename = filename.keep.original,
#'                    nBurnin = 0,
#'                    nSim = 20)
#' fetchSummary(filename.keep.original) # see 'nBurnin' and 'nSim'
#'
#' ## nBurnin > 0 in call to continueEstimation, so treat
#' ## iterations from original call to estimateModel
#' ## as part of burnin
#' filename.discard.original <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ age)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename.discard.original,
#'               nBurnin = 20,
#'               nSim = 10,
#'               nThin = 2,
#'               nChain = 2,
#'               parallel = FALSE)
#' continueEstimation(filename = filename.discard.original,
#'                    nBurnin = 10,
#'                    nSim = 10)
#' fetchSummary(filename.discard.original) # see 'nBurnin' and 'nSim'
#' 
#' @export
continueEstimation <- function(filename, nBurnin = 0, nSim = 1000, verbose = FALSE) {
    object <- fetchResultsObject(filename)
    if (methods::is(object, "CombinedCounts"))
        stop("sorry - method for estimateCounts not written yet!")
    mcmc.args.old <- object@mcmc
    control.args <- object@control
    seed.old <- object@seed
    mcmc.args.new <- makeMCMCArgs(nBurnin = nBurnin,
                                  nSim = nSim,
                                  nChain = mcmc.args.old[["nChain"]],
                                  nThin = mcmc.args.old[["nThin"]])
    append <- identical(mcmc.args.new$nBurnin, 0L)
    combineds <- object@final
    tempfiles.new <- sapply(seq_len(mcmc.args.new$nChain),
                            function(i) paste(filename, "cont", i, sep = "_"))
    MoreArgs <- c(mcmc.args.new, control.args, list(continuing = TRUE))
    if (control.args$parallel) {
        cl <- parallel::makeCluster(getOption("cl.cores", default = mcmc.args.new$nChain))
        parallel::clusterSetRNGStream(cl)
        final.combineds <- parallel::clusterMap(cl = cl,
                                      fun = estimateOneChain,
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
        final.combineds <- mapply(estimateOneChain,
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
    results <- makeResultsModelEst(finalCombineds = final.combineds,
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
    finalMessage(filename = filename, verbose = verbose)    
}




## projectAccount <- function(formula, initial, values,
##                            filename = NULL,
##                            nBurnin = 1000,
##                            nSim = 1000, nChain = 4, nThin = 10,
##                            parallel = TRUE, maxAttempt = 0L) {
##     call <- match.call()
##     values <- alignValuesAndFormula(values)
##     account <- makeInitialAccount(formula = formula,
##                                   initial = initial,
##                                   values = values)
##     thetas <- makeThetas(account = account, values = values)
##     mcmc.args <- makeMCMCArgs(nBurnin = nBurnin,
##                               nSim = nSim,
##                               nChain = nChain,
##                               nThin = nThin)
##     filename <- checkAndTidyFilename(filename = filename, prefix = "project")
##     control.args <- makeControlArgs(call = call,
##                                     filename = filename,
##                                     parallel = parallel)
##     combineds <- replicate(n = mcmc.args$nChain,
##                            initialCombinedProject(account = account,
##                                                   thetas = thetas))
##     parallel <- control.args$parallel
##     tempfiles <- sapply(seq_len(mcmc.args$nChain),
##                         function(x) paste(control.args$filename, x, sep = "_"))
##     MoreArgs <- c(list(seed = NULL), mcmc.args, control.args)
##     if (parallel) {
##         cl <- parallel::makeCluster(getOption("cl.cores", default = mcmc.args$nChain))
##         parallel::clusterSetRNGStream(cl)
##         final.combineds <- parallel::clusterMap(cl = cl,
##                                       fun = estimateOneChain,
##                                       tempfile = tempfiles,
##                                       combined = combineds,
##                                       MoreArgs = MoreArgs,
##                                       SIMPLIFY = FALSE,
##                                       USE.NAMES = FALSE)
##         seed <- parallel::clusterCall(cl, function() .Random.seed)
##         parallel::stopCluster(cl)
##     }
##     else {
##         final.combineds <- mapply(estimateOneChain,
##                                   tempfile = tempfiles,
##                                   combined = combineds,
##                                   MoreArgs = MoreArgs,
##                                   SIMPLIFY = FALSE,
##                                   USE.NAMES = FALSE)
##         seed <- list(.Random.seed)
##     }
##     control.args$lengthIter <- length(extractValues(final.combineds[[1L]]))
##     makeResultsFile(filename = control.args$filename,
##                     tempfiles = tempfiles)
##     makeResultsProject(finalCombineds = final.combineds,
##                        mcmcArgs = mcmc.args,
##                        controlArgs = control.args,
##                        seed = seed)
## }


                                       
