
setMethod("fakePrior",
          signature(object = "SpecDLMNoTrendNormZeroNoSeason"),
          function(object, metadata) {
              ATau <- object@ATau
              nuTau <- object@nuTau
              tauMax <- object@tauMax
              AAlpha <- object@AAlpha
              nuAlpha <- object@nuAlpha
              omegaAlphaMax <- object@omegaAlphaMax
              phi <- object@phi
              minPhi <- object@minPhi
              maxPhi <- object@maxPhi
              shape1Phi <- object@shape1Phi
              shape2Phi <- object@shape2Phi
              l <- fakeScaleAndScaleMax(A = ATau,
                                        nu = nuTau,
                                        scaleMax = tauMax,
                                        functionName = "Error")
              tau <- l$scale
              tauMax <- l$scaleMax
              l <- fakeScaleAndScaleMax(A = ATau,
                                        nu = nuTau,
                                        scaleMax = tauMax,
                                        functionName = "Level")
              omega <- l$scale
              omegaMax <- l$scaleMax
              phi <- fakePhi(phi = phi,
                             min = minPhi,
                             max = maxPhi,
                             shape1 = shape1Phi@.Data,
                             shape2 <- shape2Phi@.Data)
              iAlong <- dembase::checkAndTidyAlong(along = along,
                                                   metadata = metadata,
                                                   numericDimScales = TRUE)
              K <- makeK(dim = dim, iAlong = iAlong)
              L <- makeL(dim = dim, iAlong = iAlong)
              dim.alpha.delta <- dim
              dim.alpha.delta[iAlong] <- dim.alpha.delta[iAlong] + 1L
              iteratorState <- AlongIterator(dim = dim.alpha.delta,
                                             iAlong = iAlong)
              iteratorV <- AlongIterator(dim = dim,
                                         iAlong = iAlong)
              l.all <- initialDLMAll(object = object,
                                     beta = beta,
                                     metadata = metadata,
                                     sY = sY,
                                     isSaturated = isSaturated)
              l.no.trend <- initialDLMNoTrend(object = object,
                                              metadata = metadata,
                                              sY = sY)
              ans <-methods::new("DLMNoTrendNormZeroNoSeason",
                           AAlpha = l.all$AAlpha,
                           aNoTrend = l.no.trend$aNoTrend,
                           ATau = l.all$ATau,
                           alphaDLM = l.all$alphaDLM,
                           CNoTrend = l.no.trend$CNoTrend,
                           iAlong = l.all$iAlong,
                           isSaturated = l.all$isSaturated,
                           J = l.all$J,
                           K = l.all$K,
                           L = l.all$L,
                           iteratorState = l.all$iteratorState,
                           iteratorV = l.all$iteratorV,
                           mNoTrend = l.no.trend$mNoTrend,
                           m0NoTrend = l.no.trend$m0NoTrend,
                           minPhi = l.all$minPhi,
                           maxPhi = l.all$maxPhi,
                           nuAlpha = l.all$nuAlpha,
                           nuTau = l.all$nuTau,
                           omegaAlpha = l.all$omegaAlpha,
                           omegaAlphaMax = l.all$omegaAlphaMax,
                           phi = l.all$phi,
                           phiKnown = l.all$phiKnown,
                           RNoTrend = l.no.trend$RNoTrend,
                           shape1Phi = l.all$shape1Phi,
                           shape2Phi = l.all$shape2Phi,
                           tau = l.all$tau,
                           tauMax = l.all$tauMax)
              predictAlphaDLMNoTrend(ans, useC = TRUE)
          })

    
    


fakePhi <- function(phi, min, max, shape1, shape2) {
    if (is.na(phi)) {
        phi.transformed <- rbeta(n = 1L,
                                 shape1 = shape1,
                                 shape2 = shape2)
        phi <- phi.transformed * (max - min) + min
    }
    phi
}

setGeneric("fakeDLMErrors",
           function(spec, J)
           standardGeneric("fakeDLMErrors"))

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



setGeneric("fakeData",
           function(model, intercept, templateY, exposure = NULL,
                    weights = NULL, uniform = TRUE)
           standardGeneric("fakeData"))


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



makeFakePriors <- function(specs, margins, metadata) {
    ans <- vector(mode = "list", length = length(specs))
    for (i in seq_along(specs)) {
        is.intercept <- identical(margin, 0L)
        if (is.intercept)
            
            ans[[i]] <- fakePrior(spec, metadata = NULL)
        else
            ans[[i]] <- fakePrior(spec, metadata = metadata[margin])
    }
    ans
}


makeFakeMargins <- function(names, y, call) {
    names.y <- names(dimnames(y))
    i.intercept <- match("(Intercept)", names, nomatch = 0L)
    has.intercept <- i.intercept > 0L
    if (!has.intercept)
        stop(gettextf("no prior specified for '%s' in model '%s'",
                      "(Intercept)", deparse(call[[2L]])))
    names <- names[-i.intercept]
    s <- seq_along(names.y)
    possible.margins <- sapply(s, function(i) combn(s, i, simplify = FALSE))
    possible.margins <- unlist(possible.margins, recursive = FALSE)
    possible.names <- sapply(possible.margins, function(i) paste(names.y[i], collapse = ":"))
    i.found <- sapply(names, match, possible.names, nomatch = 0L)
    not.found <- i.found == 0L
    if (any(not.found)) {
        stop(gettextf("term '%s' not found in dimensions of '%s'",
                      names[not.found][1L], "templateY"))
    }
    ans <- possible.margins[i.found]
    ans <- c(list(0L), ans)
    ans
}


setMethod("fakeData",
          signature(model = "SpecPoissonVarying",
                    templateY = "Counts",
                    exposure = "ANY",
                    weights = "missing"),
          function(model, templateY, exposure = NULL,
                   weights = NULL) {
              call <- model@call
              formula <- model@formulaMu
              use.expose <- model@useExpose
              spec.priors <- model@specsPriors
              names.spec.priors <- model@namesSpecsPriors
              A.sigma <- model@ASigma@.Data
              nu.sigma <- model@nuSigma@.Data
              sigma.max <- model@sigmaMax@.Data
              lower <- model@lower
              upper <- model@upper
              box.cox.param <- model@boxCoxParam
              max.attempt <- model@maxAttempt
              n <- length(templateY)
              dim.y <- dim(templateY)
              dimnames.y <- dimnames(templateY)
              metadata.y <- templateY@metadata
              exposure <- checkAndTidyExposure(exposure = exposure,
                                               y = templateY)
              has.exposure <- !is.null(exposure)
              if (has.exposure && !use.expose)
                  stop(gettextf("'%s' argument supplied, but model '%s' does not use exposure",
                                "exposure", deparse(call[[2L]])))
              if (!has.exposure && use.expose)
                  stop(gettextf("model '%s' uses exposure, but no '%s' argument supplied",
                                deparse(call[[2L]]), "exposure"))
              margins <- makeFakeMargins(names = names.spec.priors,
                                         y = templateY,
                                         call = call)
              priors.betas <- makeFakePriors(specs = specs.priors,
                                             namesSpecs = names.specs.priors,
                                             margins = margins,
                                             y = templateY)
              betas <- lapply(priors.betas, makeFakeBeta)
              sigma <- rhalftTrunc1(df = nuSigma,
                                    scale = ASigma,
                                    max = sigmaMax)
              iterator <- makeIteratorBetas(betas = betas,
                                            namesBetas = names.betas,
                                            y = templateY)
              mu <- makeMu(n = n,
                           betas = betas,
                           iterator = iterator,
                           useC = TRUE)
              has.limits <- (lower > 0) || (upper < Inf)
              if (has.limits) {
                  lower <- log(lower)
                  upper <- log(upper)
                  for (i in seq_len(n)) {
                      tr.theta <- rtnorm1(mean = mu[i],
                                          sd = sigma,
                                          lower = lower,
                                          upper = upper,
                                          useC = TRUE)
                  }
                  else
                      tr.theta <- stats::rnorm(n = n, mean = mu, sd = sigma)
              }
              if (box.cox.param > 0)
                  theta <- (box.cox.param * tr.theta + 1) ^ (1 / box.cox.param)
              else
                  theta <- exp(tr.theta)
              lambda <- if (is.null(exposure)) theta else exposure * theta
              .Data.y <- stats::rpois(n = n, lambda = lambda)
              .Data.y <- array(.Data.y, dim = dim.y, dimnames = dimnames.y)
              y <- methods::new("Counts", .Data = .Data.y, metadata = metadata.y)
              .Data.theta <- array(theta, dim = dim.y, dimnames = dimnames.y)
              if (is.null(exposure))
                  theta <- methods::new("Counts", .Data = .Data.theta, metadata = metadata.y)
              else
                  theta <- methods::new("Values", .Data = .Data.theta, metadata = metadata.y)
              likelihood <- list(mean = theta)
              betas <- makeFakeBetasOutput(betas = betas,
                                           namesBetas = names.betas,
                                           y = y)
              prior <- list(param = betas, sd = sigma)
              model <- list(likelihood = likelihood, prior = prior)
              if (is.null(exposure))
                  methods::new("FakeData", call = call, y = y, model = model)
              else
                  methods::new("FakeDataExposure", call = call, y = y, exposure = exposure, model = model)
          })
