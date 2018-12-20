
getPriorSD <- function(spec) {
    if (!methods::is(spec, "SpecVarying"))
    i.prior.sd <- match("priorSD", names(spec@call), nomatch = 0L)
    has.prior.sd <- i.prior.sd > 0L                                  
    if (!has.prior.sd)
        stop(gettextf("'%s' argument required in call to '%s' when specifying model for fake data",
                      "priorSD", "Model"))
    call.prior.sd <- spec@call[[i.prior.sd]]
    i.scale.prior.sd <- match("scale", names(call.prior.sd), nomatch = 0L)
    has.scale.prior.sd <- i.scale.prior.sd > 0L
    if (!has.scale.prior.sd)
        stop(gettextf("'%s' argument required in call to '%s' when specifying '%s' in model for fake data",
                      "scale", "priorSD", "Model"))
    nu <- object@nuSigma
    A <- object@ASigma@.Data
    max <- object@sigmaMax@.Data
    HalfT(df = nu,
          scale = A,
          max = max)
}



fakeModel <- function(model, y, exposure = NULL, weights = NULL,
                      filename = NULL, nBurnin = 0, nSim = 25,
                      nChain = 4, nThin = 1, parallel = TRUE, outfile = NULL,
                      nUpdateMax = 50, verbose = TRUE, useC = TRUE) {
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
    l <- checkFakeYExposureWeights(spec = model,
                                   y = y,
                                   exposure = exposure,
                                   weights = weights)
    y <- l$y
    exposure <- l$exposure
    weights <- l$weights
    checkForSubtotals(object = y,
                      model = model,
                      name = "y")
    checkPriorsAreInformative(model)
    checkAllDimensionsHavePriors(model)
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
    rescaleInFile(filename)
    finalMessage(filename = filename,
                 verbose = verbose)
}


fakeOneChain <- function(combined, nIteration, ...) {
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

