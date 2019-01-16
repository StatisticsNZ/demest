model <- Model(y ~ Poisson(mean ~ age * sex),
               `(Intercept)` ~ ExchFixed(sd = 10), 
               age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
               sex ~ ExchFixed(sd = 0.1),
               age:sex ~ ExchFixed(sd = 0.05),
               priorSD = HalfT(scale = 0.2))

model <- Model(y ~ Poisson(mean ~ age * sex),
               `(Intercept)` ~ ExchFixed(sd = 10), 
               age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
               sex ~ ExchFixed(sd = 0.1),
               age:sex ~ ExchFixed(sd = 0.05),
               priorSD = HalfT(sc = 0.2))





              
checkAllDimensionsHavePriors <- function(model, y) {
    names.specs <- model@namesSpecsPriors
    names.y <- names(y)
    for (name in names.y) {
        if (!(name %in% names.specs))
            stop(gettextf("no prior specified for \"%s\" dimension",
                          name))
    }
    NULL
}


checkSimulate <- function(val, nameVal, useVal, nameY) {
    if (is.null(val) && useVal)
        stop(gettextf("problem with model for '%s' : value for '%s' not supplied",
                      nameY, nameVal))
    if (!is.null(val) && !useVal)
        stop(gettextf("problem with model for '%s' : value for '%s' supplied but should not be",
                      nameY, nameVal))
    NULL
}
        

setGeneric("checkAndTidyYExposureWeightsSimulate",
           function(model, y, exposure, weights)
               standardGeneric("checkAndTidyYExposureWeightsSimulate"))

setMethod("checkAndTidyYExposureWeightsSimulate",
          signature(model = "SpecBinomialVarying"),
          function(model, y, exposure, weights) {
              nameY <- model@nameY[[1L]]
              checkSimulate(val = y,
                            nameVal = "y",
                            useVal = FALSE,
                            nameY = nameY)
              checkSimulate(val = exposure,
                            nameVal = "exposure",
                            useVal = TRUE,
                            nameY = nameY)
              checkSimulate(val = y,
                            nameVal = "y",
                            useVal = FALSE,
                            nameY = nameY)
              
          })              
              
              


simulateModel <- function(model, y = NULL, exposure = NULL, weights = NULL,
                          filename = NULL, nBurnin = 0, nSim = 25,
                          nChain = 4, nThin = 1, parallel = TRUE,
                          outfile = NULL, nUpdateMax = 50,
                          verbose = TRUE, useC = TRUE) {
    call <- match.call()
    methods::validObject(model)
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
    l <- checkFakeYExposureWeights(model = model,
                                   y = y,
                                   exposure = exposure,
                                   weights = weights)
    y <- l$y
    exposure <- l$exposure
    weights <- l$weights
    checkForSubtotals(object = y,
                      model = model,
                      name = "y")
    checkAllDimensionsHavePriors(model = model,
                                 y = y)
    checkPriorsAreInformative(model)
    checkPriorSDInformative(model)
    combineds <- replicate(n = mcmc.args$nChain,
                           initialCombinedModel(model,
                                                y = y,
                                                exposure = exposure,
                                                weights = weights))
    parallel <- control.args$parallel
    tempfiles <- paste(filename, seq_len(mcmc.args$nChain), sep = "_")
    MoreArgs <- c(list(seed = NULL),
                  mcmc.args,
                  control.args,
                  list(continuing = FALSE,
                       useC = useC))
    if (parallel) {
        pseed <- sample.int(n = 100000, # so that RNG behaves the same whether or not
                            size = 1)   # seed has previously been set
                                        # this must be done BEFORE call to makeCluster!
        if (is.null(outfile)) ## passing 'outfile' as an argument always causes redirection
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args$nChain))
        else
            cl <- parallel::makeCluster(getOption("cl.cores",
                                                  default = mcmc.args$nChain),
                                        outfile = outfile)
        parallel::clusterSetRNGStream(cl,
                                      iseed = pseed)
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
    control.args$lengthIter <- length(extractValues(final.combineds[[1L]]))
    results <- makeResultsModelEst(finalCombineds = final.combineds,
                                   mcmcArgs = mcmc.args,
                                   controlArgs = control.args,
                                   seed = seed)
    makeResultsFile(filename = filename,
                    results = results,
                    tempfiles = tempfiles)
    rescaleInFile(filename)
    finalMessage(filename = filename,
                 verbose = verbose)
}


simulateOneChain <- function(combined, nIteration, ...) {
    ## set seed if continuing
    if (!is.null(seed))
        assign(".Random.seed", seed, envir = .GlobalEnv)
    ## burnin
    nLoops <- nBurnin %/% nUpdateMax
    for (i in seq_len(nLoops)) {
        combined <- updateCombined(combined, nUpdate = nUpdateMax, useC = useC)
    }
    ## and any final ones
    nLeftOver <- nBurnin - nLoops * nUpdateMax
    combined <- updateCombined(combined, nUpdate = nLeftOver, useC = useC)
    ## production
    con <- file(tempfile, open = "wb")
    n.prod <- nSim %/% nThin
    for (i in seq_len(n.prod)) {
        nLoops <- nThin %/% nUpdateMax
        for (i in seq_len(nLoops)) {
           combined <- updateCombined(combined, nUpdate = nUpdateMax, useC = useC)
        }
        ## and any final ones
        nLeftOver <- nThin - nLoops * nUpdateMax
        combined <- updateCombined(combined, nUpdate = nLeftOver, useC = useC)
        values <- extractValues(combined)
        writeBin(values, con = con)
    }
    close(con)
    ## return final state
    combined
}


setMethod("simulateCombined",
          signature(object = "CombinedModelPoissonHasExp"),
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
                      .Call(updateCombined_CombinedModelPoissonHasExp_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  model <- object@model
                  y <- object@y
                  exposure <- object@exposure
                  for (i in seq_len(nUpdate))
                      model <- predictModelUseExp(model, exposure = exposure)
                  object@model <- model
                  object
              }
          })




#' Generate Fake Data
#' 
#' Generate a simulated dataset, based on a hierarchical model.
#' 
#' NOTE - AT PRESENT ONLY IMPLEMENTED FOR POISSON-VARYING AND BINOMIAL-VARYING
#' MODELS
#' 
#' The values of \code{templateY} are ignored by \code{fakeData}: only the
#' metadata is used.
#' 
#' The model specified by \code{model} must not have improper priors.  This
#' implies, for instance, that a \code{priorSD} argument must be supplied in a
#' varying-means model.
#' 
#' All prior distributions for cell means in varying-means models are centered
#' at zero before values are drawn.  See below for an example.
#' 
#' Benchmarked models are not allowed.
#' 
#' Bounds on probabilities or means can be specified via the \code{lower} and
#' \code{upper} arguments to functions \code{\link{Poisson}},
#' \code{\link{Binomial}} or \code{\link{Normal}}.  In some situations,
#' \code{fakeData} make not be able to generate a value from the target
#' distribution within the specified bounds.  In such cases, \code{fakeData}
#' will instead draw from a uniform distribution if \code{uniform} is
#' \code{TRUE}, and raise an error otherwise.
#' 
#' @aliases fakeData fakeData,SpecBinomialVarying,ANY,Counts-method
#' fakeData,SpecBinomialVaryingBench,ANY,Counts-method
#' fakeData,SpecNormalVarying,ANY,Counts-method
#' fakeData,SpecPoissonVarying,ANY,Counts-method
#' fakeData,SpecPoissonVaryingBench,ANY,Counts-method
#' @param model An object of class \code{"\linkS4class{SpecModel}"}.
#' @param intercept A real number, possibly negative.  Determines the expected
#' value of the fake data.
#' @param templateY An object of class
#' \code{\link[Demographic:Counts-class]{Counts}} with the dimensions and
#' metadata required for the fake data.
#' @param exposure An object of class
#' \code{\link[Demographic:Counts-class]{Counts}}.
#' @param weights An object of class
#' \code{\link[Demographic:Counts-class]{Counts}}.
#' @param uniform Logical.  Only used if the model includes upper or lower
#' bounds.  See below for details.
#' @return An object of class \code{"\linkS4class{FakeData}"}
#' @author John Bryant \email{demographic.packages@@gmail.com}
#' @seealso \code{\link{Model}}, \code{"\linkS4class{FakeData}"},
#' \code{\link{fetch}} \code{\link{listContents}}
#' @keywords models
#' @examples
#' 
#' sex.mean <- ValuesOne(values = c(-1, 1), labels = c("Female", "Male"), name = "sex")
#' sex.prior <- Known(mean = sex.mean, sd = 0.2)
#' model <- Model(y ~ Poisson(mean ~ sex + time,
#'                            priorSD = list(df = 10, scale = 0.2)),
#'                time ~ Exch(sd = 0.2),
#'                sex ~ sex.prior)
#' exposure <- Counts(array(100 * rbeta(n = 60, shape1 = 0.5, shape2 = 2),
#'                          dim = c(3, 2, 10),
#'                          dimnames = list(region = c("A", "B", "C"),
#'                          sex = c("Female", "Male"),
#'                          time = 2000:2009)))
#' fake <- fakeData(model = model,
#'                  intercept = 1,
#'                  templateY = exposure,
#'                  exposure = exposure)
#' 
#' ## the results would have been identical if the mean for the sex prior had been
#' sex.mean <- ValuesOne(values = c(100, 101), labels = c("Female", "Male"), name = "sex")
#' ## since priors are centered before values are drawn
#' 
#' ## to change the mean, change the intercept
#' fake <- fakeData(model = model,
#'                  intercept = -1,
#'                  templateY = exposure,
#'                  exposure = exposure)
#' 
NULL

#' Display the Contents of a Results or Fake Data Object
#' 
#' Display the contents of an object of class \code{"\linkS4class{Results}"} or
#' \code{"\linkS4class{FakeData}"}, in hierarchical form.
#' 
#' Arguments \code{where} and \code{max} restrict number of elements shown in
#' complementary ways: providing values for \code{where} yeilds narrower
#' hierarchies, and providing values for \code{depth} yields shallower ones.
#' 
#' Partial matching is used with elements of \code{where}, so that only the
#' first few letters need to be used, provided this is enough to identify the
#' place in the hierarchy.  See below for an example.
#' 
#' @aliases listContents listContents,FakeData-method
#' listContents,Results-method
#' @param object An object of class \code{"\linkS4class{Results}"} or
#' \code{"\linkS4class{FakeData}"}.
#' @param where A character vector used to select a subset of the hierarchy.
#' @param max The maximum depth into the hierarchy to display.
#' @return The function is typically called for its side effect, the display of
#' a list, but the list is returned invisibly.
#' @author John Bryant \email{demographic.packages@@gmail.com}
#' @seealso To select elements with an object of class
#' \code{"\linkS4class{Results}"}, use \code{\link{fetch}}.
#' @keywords list
#' @examples
#' 
#' ## create Results object
#' deaths <- Counts(round(VADeaths2))
#' popn <- Counts(VAPopn)
#' res <- estimateModel(Model(y ~ Poisson(mean ~ age)),
#'                      y = deaths,
#'                      exposure = popn,
#'                      filename = tempfile(),
#'                      nBurnin = 2,
#'                      nSim = 5,
#'                      nChain = 2)
#' 
#' ## look at whole object
#' listContents(res)
#' 
#' ## look only at the model
#' listContents(res, where = "model")
#' 
#' ## restrict further, to the hyper-parameters part of the model
#' listContents(res, where = c("model", "hyper"))
#' 
#' ## use only enough letters to uniquely identify names
#' listContents(res, where = c("mo", "h"))
#' 
#' ## look only at first level of hierarchy
#' listContents(res, max = 1)
#' 
#' ## look at second and third levels of model
#' listContents(res, where = "model", max = 3)
#' 
NULL


setGeneric("fetchInnerFake",
           function(object, nameObject, where)
           standardGeneric("fetchInnerFake"))


## HAS_TESTS
makeFakeBetas <- function(y, formula, specPriors, namesSpecPriors, intercept) {
    dim.y <- dim(y)
    dimnames.y <- dimnames(y)
    names.y <- names(y)
    metadata.y <- y@metadata
    checkTermsFromFormulaFound(y = y, formula = formula)
    checkLengthDimInFormula(y = y, formula = formula)
    checkAllTermsInFormulaSpecified(formula = formula, namesSpecPriors = namesSpecPriors)
    betas <- vector(mode = "list", length = length(namesSpecPriors))
    for (i in seq_along(betas)) {
        name.split <- strsplit(namesSpecPriors[i], split = ":", fixed = TRUE)[[1L]]
        margin <- match(name.split, names.y)
        metadata <- metadata.y[margin]
        betas[[i]] <- fakeBeta(object = specPriors[[i]], metadata = metadata)
    }
    betas <- c(list(intercept), betas)
    betas
}


kainga_data <- data.frame(kainga = letters[1:10],
                          pc_pakeha = runif(n = 10, max = 0.2))

model <- Model(y ~ Binomial(mean ~ kainga),
               `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.1),
               kainga ~ Exch(covariates = Covariates(mean ~ pc_pakeha, data = kainga_data),
                             error = Error(scale = HalfT(scale = 0.1))),
               priorSD = HalfT(scale = 0.1))

fakeData(model = model,
         exposure = exposure,
         filename = filename,
         nIteration = 100)





setMethod("drawFromModelUseExp",
          signature(object = "BinomialVarying"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_BinomialVarying_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- drawBetas(object)
                  object <- drawSigma_Varying(object)
                  object <- updateTheta_BinomialVarying(object,
                                                        y = y,
                                                        exposure = exposure)
                  object
              })




setGeneric("drawFromModelNotUseExp",
           function(object)
               standardGeneric("drawFromModelNotUseExp"))


setMethod("drawFromModelNotUseExp",
          signature(object = "CMPVaryingNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_CMPVaryingNotUseExp_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  object <- drawBetas(object)
                  object <- drawSigma_Varying(object)
                  object <- updateThetaAndNu_CMPVaryingNotUseExp(object, y = y)
                  object
              }
          })


setMethod("drawFromModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnown"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalVaryingVarsigmaKnown_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  varsigmaSetToZero <- object@varsigmaSetToZero@.Data
                  object <- drawBetas(object)
                  object <- drawSigma_Varying(object)
                  if (varsigmaSetToZero)
                      object <- updateThetaVarsigmaSetToZero(object)
                  else
                      object <- updateTheta_NormalVarying(object, y = y)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknown"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalVaryingVarsigmaUnknown_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVarying(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_PoissonVaryingNotUseExp_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgCertain"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgCertain"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgCertain"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_PoissonVaryingNotUseExpAgCertain_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgNormal"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_Normal(object = object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgNormal"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_Normal(object = object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgFun"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalVaryingVarsigmaKnownAgFun_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateThetaAndValueAgFun_Normal(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgFun"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateThetaAndValueAgFun_Normal(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgNormal"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_PoissonVaryingNotUseExpAgNormal_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgFun"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_PoissonVaryingNotUseExpAgFun_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateThetaAndValueAgFun_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgPoisson"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_PoissonVaryingNotUseExpAgPoisson_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateThetaAndValueAgPoisson_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "NormalFixedNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_NormalFixedNotUseExp_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  ## object is not updated
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelNotUseExp",
          signature(object = "TFixedNotUseExp"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelNotUseExp_TFixedNotUseExp_R, object, y)
                  else
                      .Call(drawFromModelNotUseExp_R, object, y)
              }
              else {
                  ## object is not updated
                  object
              }
          })





## drawFromModelUseExp #################################################################


## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "CMPVaryingUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data) & (exposure@.Data == 0L)] == 0))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_CMPVaryingUseExp_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateThetaAndNu_CMPVaryingUseExp(object, y = y, exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
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
                      .Call(drawFromModelUseExp_PoissonVarying_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "PoissonBinomialMixture"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_PoissonBinomialMixture_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "Round3"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(dataset[!is.na(dataset)] %% 3L == 0L))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_Round3_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "BinomialVaryingAgCertain"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_BinomialVaryingAgCertain_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateTheta_BinomialVaryingAgCertain(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "PoissonVaryingUseExpAgCertain"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_PoissonVaryingUseExpAgCertain_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "BinomialVaryingAgNormal"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_BinomialVaryingAgNormal_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateTheta_BinomialVaryingAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgNormal_Binomial(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "BinomialVaryingAgFun"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_BinomialVaryingAgFun_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateThetaAndValueAgFun_Binomial(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "PoissonVaryingUseExpAgNormal"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_PoissonVaryingUseExpAgNormal_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgNormal_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "PoissonVaryingUseExpAgFun"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_PoissonVaryingUseExpAgFun_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateThetaAndValueAgFun_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "PoissonVaryingUseExpAgLife"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_PoissonVaryingUseExpAgLife_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateThetaAndValueAgLife_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "PoissonVaryingUseExpAgPoisson"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_PoissonVaryingUseExpAgPoisson_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgPoisson_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "NormalFixedUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_NormalFixedUseExp_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "Round3"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_Round3_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("drawFromModelUseExp",
          signature(object = "TFixedUseExp"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawFromModelUseExp_TFixedUseExp_R, object, y, exposure)
                  else
                      .Call(drawFromModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })


