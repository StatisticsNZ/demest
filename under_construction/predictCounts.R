
predictCounts <- function(filenameEst, filenamePred, along = NULL, labels = NULL, n = NULL,
                          data = list(), aggregate = list(), lower = list(),
                          upper = list(), nBurnin = 0L,  parallel = TRUE, outfile = NULL,
                          verbose = FALSE, useC = TRUE) {
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
    ## set up new objects
    along <- dembase::checkAndTidyAlong(along = along,
                                        metadata = metadata.first,
                                        numericDimScales = FALSE)
    checkFilename(filename = filenameEst,
                  name = "filenameEst")
    if(is.null(filenamePred))
        filenamePred <- tempfile()
    else
        checkFilename(filename = filenamePred,
                      name = "filenamePred")
    if (!(is.null(aggregate) || methods::is(aggregate, "SpecAggregate")))
        stop(gettextf("'%s' has class \"%s\"",
                      "aggregate", class(aggregate)))
    data <- checkandTidyListArgForEstimate(arg = data,
                                           name = "data",
                                           isCounts = TRUE)
    aggregate <- checkandTidyListArgForEstimate(arg = aggregate,
                                                name = "aggregate",
                                                isCounts = TRUE)
    lower <- checkandTidyListArgForEstimate(arg = lower,
                                            name = "lower",
                                            isCounts = TRUE)
    upper <- checkandTidyListArgForEstimate(arg = upper,
                                            name = "upper",
                                            isCounts = TRUE)
    combined.pred <- initialCombinedCountsPredict(combined = combined.first,
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
    tempfiles.pred <- paste(filenamePred, seq_len(mcmc.args.pred[["nChain"]]), sep = "_")
    n.iter.chain <- mcmc.args.first[["nIteration"]] / mcmc.args.first[["nChain"]]
    if (parallel) {
        if (is.null(outfile)) ## passing 'outfile' as an argument always causes redirection
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args.pred$nChain))
        else
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args.pred$nChain),
                                        outfile = outfile)
        parallel::clusterSetRNGStream(cl)
        final.combineds <- parallel::clusterMap(cl = cl,
                                                fun = predictOneChain,
                                                combined = list(combined.pred),
                                                tempfileOld = tempfiles.first,
                                                tempfileNew = tempfiles.pred,
                                                lengthIter = control.args.first[["lengthIter"]],
                                                nIteration = n.iter.chain,
                                                nUpdate = nBurnin,
                                                useC = useC,
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
                                  useC = useC,
                                  SIMPLIFY = FALSE,
                                  USE.NAMES = FALSE)
        seed <- list(.Random.seed)
    }
    sapply(tempfiles.first, unlink)
    results <- makeResultsCountsPred(finalCombineds = final.combineds,
                                     mcmcArgs = mcmc.args.pred,
                                     controlArgs = control.args.pred,
                                     seed = seed)
    makeResultsFile(filename = filenamePred,
                    results = results,
                    tempfiles = tempfiles.pred)
    finalMessage(filename = filenamePred,
                 verbose = verbose)
}




## ALLOW FOR CONCORDANCES TO USE IN TRANSFORMS FOR DATA, IN estimateCounts AND estimateAccount

setMethod("initialCombinedCountsPredict",
          signature(combined = "CombinedCountsPoissonHasExp"),
          function(combined, along, labels, n, covariates,
                   aggregate, lower, upper, yIsCounts) {
              model.est <- combined@model
              y.est <- combined@y
              datasets.est <- combined@datasets
              data.models.est <- combined@dataModels
              names.datasets <- combined@namesDatasets
              n.dataset <- length(datasets.est)
              pos <- 1L
              covariates.pred <- covariates[["model"]]
              aggregate.pred <- aggregate[["model"]]
              lower.pred <- lower[["model"]]
              upper.pred <- upper[["model"]]
              model.pred <- initialModelPredict(model = model.est,
                                                along = along,
                                                labels = labels,
                                                n = n,
                                                offsetModel = pos,
                                                covariates = covariates.pred,
                                                aggregate = aggregate.pred,
                                                lower = lower.pred,
                                                upper = upper.pred)
              pos <- pos + lengthValues(model.est)
              y.pred <- makeCountsPred(modelPred = model.pred)
              pos <- pos + lengthValues(y.est)
              exposure.pred <- toDouble(y.pred)
              data.models.pred <- vector(mode = "list", length = n.dataset)
              datasets.pred <- vector(mode = "list", length = n.dataset)
              transforms.pred <- vector(mode = "list", length = n.dataset)
              for (i in seq_len(n.dataset)) {
                  data.model.est <- data.models.est[[i]]
                  name <- names.datasets[i]
                  covariates.pred <- covariates[["dataModels"]][[name]]
                  aggregate.pred <- aggregate[["dataModels"]][[name]]
                  lower.pred <- lower[["dataModels"]][[name]]
                  upper.pred <- upper[["dataModels"]][[name]]
                  data.model.pred <- initialModelPredict(model = data.model.est,
                                                         along = along,
                                                         labels = labels,
                                                         n = n,
                                                         offsetModel = pos,
                                                         covariates = covariates.pred,
                                                         aggregate = aggregate.pred,
                                                         lower = lower.pred,
                                                         upper = upper.pred)
                  pos <- pos + lengthValues(data.model.est)
                  dataset.pred <- makeCountsPred(modelPred = data.model.pred)
                  transform.pred <- dembase::makeTransform(x = y.pred,
                                                           y = dataset.pred,
                                                           subset = TRUE)
                  data.models.pred[[i]] <- data.model.pred
                  datasets.pred[[i]] <- dataset.pred
                  transforms.pred[[i]] <- transform.pred
              }
              methods::new("CombinedCountsPoissonHasExp",
                           model = model.pred,
                           y = y.pred,
                           exposure = exposure.pred,
                           dataModels = data.models.pred,
                           dataset = datasets,
                           namesDatasets = names.dataset,
                           transforms = transforms)
                           
          })


              


setMethod("predictCombined",
          signature(object = "CombinedCountsPoissonHasExp"),
          function(object, nUpdate = 1L, filename, lengthIter, iteration, useC = FALSE, useSpecific = FALSE) {
              ## object
              methods::validObject(object)
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(predictCombined_CombinedCountsPoissonHasExp_R,
                            object, filename, lengthIter, iteration)
                  else
                      .Call(predictCombined_R,
                            object, filename, lengthIter, iteration)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  datasets <- object@datasets
                  data.models <- object@dataModels
                  transforms <- object@transforms
                  model <- transferParamModel(model = model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- predictModelUseExp(model, y = y, exposure = exposure)
                  object@model <- model
                  
                  object
              }
          })


