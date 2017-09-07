
## HAS_TESTS
#' Extract estimates from model output.
#'
#' Functions \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' and \code{\link{estimateAccount}}, and their \code{predict} counterparts,
#' send their output to a simple database specified by \code{filename}.
#' Function \code{fetch} extracts estimates from this database.
#'
#' Estimates are stored in a hierarchical structure.  The structure be viewed
#' with function \code{\link{listContents}}.  Function \code{fetch} uses
#' a description of the path through the hierarchical structure to locate
#' estimates.  The \code{where} argument to \code{fetch} is a character vector,
#' giving names of nodes in the hierarchy.  Partial matching is used with
#' the names of the nodes, so names can be shortened, provided they are still
#' long enough to uniquely identify a node.  (To make code self-documenting,
#' it is still best to use the full names in production code.)
#' 
#' Using the \code{iterations} to select a subset of iterations can be useful
#' if the object being extracted is large relative to memory or if calculations
#' are running slowly.
#'
#' In the hierarchical models specified by functions such as
#' \code{\link{Poisson}}, \code{\link{Binomial}}, and \code{\link{Normal}},
#' main effects and interactions are only
#' weakly identified, in that adding a given quantity to the intercept term
#' and subtracting it from the remaining terms would yield the same likelihood.
#' The weak identification helps speed convergence.  However, to obtain
#' interpretable parameter estimates, it is necessary to impose some
#' constraints.  If \code{normalize} is \code{TRUE} (the default), all main
#' effects are constrained to sum to 0, as are all margins in interactions.
#' For example, in a two-way interaction, the row sums and column sums will
#' all be 0.
#'
#' Where possible, \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' and \code{\link{estimateAccount}} avoid imputing missing values during
#' model fitting, since the imputed values are typically highly correlated
#' with other unknown quantities, which can slow convergence.  If a batch
#' of estimates has missing values and if \code{impute} is \code{TRUE} the
#' missing values will be imputed at the time they are fetched.
#' 
#' @param filename The filename used by the estimate function.
#' @param where A character vector describing the path to the item to be
#' extracted.
#' @param iterations A vector of positive integers giving the iterations to be
#' extracted if an item has multiple iterations.
#' @param normalize Logical. Whether to centre estimates of main effects and
#' interactions from hierarchical models.  Defaults to \code{TRUE}.
#' @param impute Logical. Whether to impute missing values.
#'
#' @return Parameters that were estimated from the data typically have class
#' \code{DemographicArray} and have a dimension with
#' \code{\link[dembase]{dimtype}} \code{"iteration"}.  Other elements
#' stored in \code{object} have a variety of classes.
#'
#' @seealso \code{fetch} is used to extract output from functions 
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' and \code{\link{estimateAccount}}.  Function \code{\link{listContents}}
#' shows the internal structure of the output, which is useful for
#' constructing the \code{where} argument to \code{fetch}.
#'
#' @references
#' The weak identification of main effects and interactions is discussed in
#'
#' Gelman, A. (2005). Analysis of variance: Why it is more important than ever.
#' \emph{Annals of Statistics}, 33(1):1-53.
#'
#' and
#'
#' Nelder, J. A. (1994). The statistics of linear models: Back to basics.
#' \emph{Statistics and Computing}. 4: 221-234.
#' 
#' @examples
#' deaths <- demdata::VADeaths2
#' popn <- demdata::VAPopn
#' deaths <- round(deaths)
#' deaths <- Counts(deaths)
#' popn <- Counts(popn)
#' filename <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ age * sex)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 20,
#'               nSim = 20,
#'               nChain = 2,
#'               parallel = FALSE)
#' ## the model hasn't been run for long enough,
#' ## but we'll extract some results anyway
#' 
#' ## examine structure
#' listContents(filename)
#' 
#' ## extract means from likelihood model
#' rate <- fetch(filename,
#'               where = c("model", "likelihood", "rate"))
#' plot(rate)
#' 
#' ## only supply enough of component of 'where' to identify
#' rate <- fetch(filename,
#'               where = c("mo", "l", "r"))
#' 
#' ## extract all iterations of intercept
#' fetch(filename,
#'       where = c("model", "prior", "(Intercept)"))
#' 
#' ## extract every fifth iteration
#' fetch(filename,
#'       where = c("model", "prior", "(Intercept)"),
#'       iterations = seq(from = 5, by = 5, to = 40))
#'
#' ## normalised vs normalised age effects
#' age.sex.norm <- fetch(filename,
#'                       where = c("model", "prior", "age:sex"),
#'                       iterations = 1)
#' age.sex.unnorm <- fetch(filename,
#'                         where = c("model", "prior", "age:sex"),
#'                         normalize = FALSE,
#'                         iterations = 1)
#' age.sex.norm <- drop(age.sex.norm) # drop 'iterations' dimension
#' age.sex.unnorm <- drop(age.sex.unnorm) 
#' rowSums(age.sex.norm)
#' rowSums(age.sex.unnorm)
#' colSums(age.sex.norm)
#' colSums(age.sex.unnorm)
#' @export
fetch <- function(filename, where = character(), iterations = NULL,
                  normalize = TRUE, impute = FALSE) {
    object <- fetchResultsObject(filename)
    nIteration <- object@mcmc["nIteration"]
    lengthIter <- object@control$lengthIter
    listsAsSingleItems <- listsAsSingleItems()
    if (identical(length(where), 0L))
        stop(gettextf("'%s' has length %d",
                      "where", 0L))
    if (any(is.na(where)))
        stop(gettextf("'%s' has missing values",
                      "where"))
    where <- as.character(where)
    if (!is.null(iterations)) {
        if (identical(length(iterations), 0L))
            stop(gettextf("'%s' has length %d",
                          "iterations", 0L))
        if (!is.numeric(iterations))
            stop(gettextf("'%s' does not have type \"%s\"",
                          "iterations", "numeric"))
        if (any(is.na(iterations)))
            stop(gettextf("'%s' has missing values",
                          "iterations"))
        if (!all(round(iterations) == iterations))
            stop(gettextf("'%s' has non-integer values",
                          "iterations"))
        if (min(iterations) < 1L)
            stop(gettextf("'%s' has values less than %d",
                          "iterations", 1L))
        if (max(iterations) > nIteration)
            stop(gettextf("maximum value for '%s' argument [%s] exceeds number of iterations [%d]",
                          "iterations", max(iterations), nIteration))
        if (any(duplicated(iterations)))
            stop(gettextf("'%s' has duplicates",
                          "iterations"))
        iterations <- as.integer(iterations)
        iterations <- sort(iterations)
    }
    if (identical(dembase::nIteration(object), 0L))
        return(NULL)
    choices <- methods::slotNames(object)
    name <- where[1L]
    i <- charmatch(name, choices, nomatch = -1L)
    if (i > 0L) {
        name <- choices[i] ## in case of partial match
        ans <- fetchInner(object = methods::slot(object, name),
                          nameObject = name,
                          where = where[-1L],
                          iterations = iterations,
                          filename = filename,
                          lengthIter = lengthIter,
                          nIteration = nIteration,
                          listsAsSingleItems = listsAsSingleItems,
                          shift = normalize,
                          impute = impute)
    }
    else if (i == 0L)
        raiseMultipleMatchesError(target = name, choices = choices)
    else
        raiseNotFoundError(target = name, choices = choices)
    if (normalize) {
        skeleton <- fetchSkeleton(object, where = where)
        need.to.center <- needToCenter(skeleton)
        if (need.to.center)
            ans <- sweepAllMargins(ans)
    }
    ans
}


## HAS_TESTS
#' Extract combined results from estimation and prediction.
#'
#' Combined results from calling \code{\link{estimateModel}} followed
#' by \code{\link{predictModel}}, \code{\link{estimateCounts}} followed
#' by \code{\link{predictCounts}}, or \code{\link{estimateAccount}}
#' followed by \code{\link{predictAccount}}.  (Note - functions
#' predictCounts, estimateAccount, and predictAccount have not been
#' written yet.)
#'
#' Identical results can be obtained by calling \code{\link{fetch}}
#' separately on the files created by the estimation and prediction functions,
#' and then combining.  However, \code{fetchBoth} is more convenient.
#'
#' \code{fetchBoth} is particularly convenient in the case of main effects,
#' since it normalises the combines the estimates and predictions before
#' normalising, rather than normalising each separately.  (See the
#' documentation for \code{\link{fetch}} for a discussion of normalisation.)
#'
#' @inheritParams fetch
#' @param filenameEst The filename used for estimation.
#' @param filenamePred The filename used for prediction.
#'
#' @seealso Function \code{\link{fetch}} extracts results from a single file.
#'
#' @examples
#' \dontrun{
#' deaths <- demdata::us.deaths
#' exposure <- demdata::us.exposure
#' deaths <- Counts(deaths,
#'                  dimscales = c(year = "Intervals"))
#' exposure <- Counts(exposure,
#'                    dimscales = c(year = "Intervals"))
#' filename.est <- tempfile()
#' filename.pred <- tempfile()
#' model <- Model(y ~ Poisson(mean ~ age + sex + year))
#' estimateModel(model,
#'               filename = filename.est,
#'               y = deaths,
#'               exposure = exposure,
#'               nBurnin = 5,
#'               nSim = 5,
#'               nChain = 2,
#'               parallel = FALSE)
#' predictModel(filenameEst = filename.est,
#'              filenamePred = filename.pred,
#'              n = 5)
#' ## the model is too simple, and hasn't been run for long
#' ## enough, but we'll extract some results anyway...
#' rate <- fetchBoth(filenameEst = filename.est,
#'                   filenamePred = filename.pred,
#'                   where = c("model", "likelihood", "rate"))
#' year <- fetchBoth(filenameEst = filename.est,
#'                   filenamePred = filename.pred,
#'                   where = c("model", "prior", "year"))
#' }
#' @export
fetchBoth <- function(filenameEst, filenamePred, where, iterations = NULL,
                      normalize = TRUE, impute = FALSE) {
    ## preparation and checking
    results.est <- fetchResultsObject(filenameEst)
    nIteration <- results.est@mcmc["nIteration"]
    lengthIterEst <- results.est@control$lengthIter
    listsAsSingleItems <- listsAsSingleItems()
    if (identical(length(where), 0L))
        stop(gettextf("'%s' has length %d",
                      "where", 0L))
    if (any(is.na(where)))
        stop(gettextf("'%s' has missing values",
                      "where"))
    where <- as.character(where)
    if (!is.null(iterations)) {
        if (identical(length(iterations), 0L))
            stop(gettextf("'%s' has length %d",
                          "iterations", 0L))
        if (!is.numeric(iterations))
            stop(gettextf("'%s' does not have type \"%s\"",
                          "iterations", "numeric"))
        if (any(is.na(iterations)))
            stop(gettextf("'%s' has missing values",
                          "iterations"))
        if (!all(round(iterations) == iterations))
            stop(gettextf("'%s' has non-integer values",
                          "iterations"))
        if (min(iterations) < 1L)
            stop(gettextf("'%s' has values less than %d",
                          "iterations", 1L))
        if (max(iterations) > nIteration)
            stop(gettextf("maximum value for '%s' argument [%s] exceeds number of iterations [%d]",
                          "iterations", max(iterations), nIteration))
        if (any(duplicated(iterations)))
            stop(gettextf("'%s' has duplicates",
                          "iterations"))
        iterations <- as.integer(iterations)
        iterations <- sort(iterations)
    }
    if (identical(dembase::nIteration(results.est), 0L))
        return(NULL)
    choices <- methods::slotNames(results.est)
    name <- where[1L]
    i <- charmatch(name, choices, nomatch = -1L)
    if (i == -1L)
        raiseNotFoundError(target = name, choices = choices)
    if (i == 0L)
        raiseMultipleMatchesError(target = name, choices = choices)
    name <- choices[i] ## in case of partial match
    ## extract reults
    is.time.varying <- isTimeVarying(filenameEst = filenameEst,
                                     filenamePred = filenamePred,
                                     where = where)
    if (is.time.varying) {
        results.pred <- fetchResultsObject(filenamePred)
        lengthIterPred <- results.pred@control$lengthIter
        est <- fetchInner(object = methods::slot(results.est, name),
                          nameObject = name,
                          where = where[-1L],
                          iterations = iterations,
                          filename = filenameEst,
                          lengthIter = lengthIterEst,
                          nIteration = nIteration,
                          listsAsSingleItems = listsAsSingleItems,
                          shift = normalize,
                          impute = impute)
        pred <- fetchInner(object = methods::slot(results.pred, name),
                           nameObject = name,
                           where = where[-1L],
                           iterations = iterations,
                           filename = filenamePred,
                           lengthIter = lengthIterPred,
                           nIteration = nIteration,
                           listsAsSingleItems = listsAsSingleItems,
                           shift = normalize,
                           impute = impute)
        ans <- combineEstPred(est = est, pred = pred)
    }
    else {
        ans <- fetchInner(object = methods::slot(results.est, name),
                          nameObject = name,
                          where = where[-1L],
                          iterations = iterations,
                          filename = filenameEst,
                          lengthIter = lengthIterEst,
                          nIteration = nIteration,
                          listsAsSingleItems = listsAsSingleItems,
                          shift = normalize,
                          impute = impute)
    }
    if (normalize) {
        skeleton <- fetchSkeleton(results.est, where = where)
        need.to.center <- needToCenter(skeleton)
        if (need.to.center)
            ans <- sweepAllMargins(ans)
        }
    }
    ans
}

## HAS_TESTS
#' Create a list of objects for analysis with package "coda".
#' 
#' Create an object of class \code{"\link{mcmc.list}"}, or a list of objects of
#' class \code{"mcmc.list"}, which can be analysed using the diagnostic
#' functions in package \pkg{coda}.
#' 
#' If no \code{where} argument is supplied, \code{\link[coda]{mcmc.list}}
#' objects are returned for every parameter or batch of parameters that were
#' estimated. If a \code{where} argument is supplied, it must describe the
#' path to parameter or parameters.  See \code{\link{fetch}} for more on
#' specifying paths.
#' 
#' If a batch of parameters has more than 25 elements, then, by default,
#' \code{fetchMCMC} extracts 25 randomly-chosen elements.  However, users can
#' control the selection of parameters throught the \code{sample} argument.
#' See below for an example.
#' 
#' If \code{thinned} is \code{TRUE}, then the \code{thin} argument in
#' \pkg{coda} function \code{\link[coda]{mcmc}} is set to 1; otherwise
#' \code{thin} is set to \code{nThin}, extracted from \code{object}.
#' 
#' @param filename The name of the file where the output from the
#' \code{estimate} function is kept.
#' @param where A character vector used to select a single parameter or batch
#' of parameters.  See below for details.
#' @param sample Indices of parameters to be sampled.  Optional, and only
#' needed when there are many parameters.
#' @param thinned Logical.  If \code{TRUE}, the default, the
#' \code{"\link{mcmc.list}"} object or objects describe the thinned process.
#'
#' @return A single object of class \code{"\link{mcmc.list}"}, or a named list
#' of such objects.
#' 
#' @seealso Other functions for examining output from
#' calls to \code{\link{estimateModel}}, \code{\link{estimateCounts}}, and
#' \code{\link{estimateAccount}} include \code{\link{fetch}},
#' \code{\link{fetchFiniteSD}}, and \code{\link{listContents}}.
#'
#' @examples
#' library(demdata)
#' ## fit model
#' deaths <- Counts(round(VADeaths2))
#' popn <- Counts(VAPopn)
#' filename <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ age)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 20,
#'               nSim = 20,
#'               nChain = 2,
#'               parallel = FALSE)
#' 
#' ## create a list containing an "mcmc.list" object for
#' ## every element of the results that was estimated
#' l <- fetchMCMC(filename)
#' names(l)
#' sapply(l, class)
#' 
#' ## analyse using functions from 'coda'
#' \dontrun{
#' library(coda)
#' plot(l$model.likelihood.rate)
#' plot(l$model.likelihood.rate, ask = FALSE)
#' plot(l$model.likelihood.rate, ask = FALSE, smooth = FALSE)
#' gelman.diag(l$model.likelihood.rate)
#' }
#' 
#' ## create a single "mcmc.list" object
#' mean.mcmc <- fetchMCMC(filename,
#'                   where = c("model", "likelihood", "rate"))
#' 
#' ## only write part of each name
#' mean.mcmc <- fetchMCMC(filename, where = c("mod", "like", "r"))
#' 
#' ## sample the first 5 values
#' mean.mcmc.5 <- fetchMCMC(filename,
#'                     where = c("model", "likelihood", "rate"),
#'                     sample = 1:5)
#' @export
fetchMCMC <- function(filename, where = NULL, sample = NULL, thinned = TRUE) {
    object <- fetchResultsObject(filename)
    if (!methods::is(object, "Results"))
        stop(gettextf("results object has class \"%s\"",
                      class(object)))
    if (!identical(length(thinned), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "thinned", 1L))
    if (!is.logical(thinned))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "thinned", "logical"))
    if (is.na(thinned))
        stop(gettextf("'%s' is missing",
                      "thinned"))
    mcmc.args <- fetch(filename, where = "mcmc")
    n.chain <- mcmc.args[["nChain"]]
    if (thinned)
        n.thin <- 1L
    else
        n.thin <- mcmc.args[["nThin"]]
    if (is.null(where)) {
        where.mcmc <- whereMetropStat(object, whereEstimated)
        n.where <- length(where.mcmc)
        if (n.where == 0L)
            NULL
        else {
            ans <- vector(mode = "list", length = n.where)
            for (i in seq_len(n.where)) {
                where <- where.mcmc[[i]]
                obj <- fetch(filename, where = where)
                ans[[i]] <- MCMCDemographic(object = obj,
                                            sample = sample,
                                            nChain = n.chain,
                                            nThin = n.thin)
            }
            names(ans) <- sapply(where.mcmc, paste, collapse = ".")
            ans
        }
    }
    else {
        obj <- fetch(filename, where = where)
        if (!methods::is(obj, "DemographicArray"))
            stop(gettextf("'%s' has class \"%s\"",
                          where[length(where)], class(obj)))
        if (!("iteration" %in% dembase::dimtypes(obj)))
            stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                          where[length(where)], "iteration"))
        MCMCDemographic(object = obj,
                        sample = sample,
                        nChain = n.chain,
                        nThin = n.thin)
    }
}


## NO_TESTS
#' Summarise estimation output.
#'
#' Summarise the results from \code{\link{estimateModel}},
#' \code{\link{estimateCounts}} or \code{\link{estimateAccount}}.
#' \code{fetchSummary} can also be called on results from prediction
#' functions, though doing so is generally less informative.
#'
#' @param filename The filename used by the estimation or prediction
#' function.
#'
#' @section Metropolis-Hastings updates:
#'
#' Depending on the model, one or more batches of parameters is updated
#' using Metropolis-Hasting updates.  \code{jump} is the standard
#' deviation of the propoposal density.  \code{acceptance} is the
#' proportion of proposals that are accepted.  \code{autocorrelation}
#' is the correlation between successive iterations, after thinning.
#' Autocorrelations are calculated separately, parameter by parameter,
#' and then averaged.  If a batch contains more than 25 parameters,
#' then 25 parameters are chosen at random, and autocorrelations calculated
#' only for that sample.
#'
#' The Metropolis-Hastings statistics can be extracted from the summary
#' object using function \code{\link{metropolis}}.
#'
#' @section Parameters:
#' 
#' Some parameters, such as  the rates in the likelihood for a Poisson
#' model (\code{model.likelihood.rate}), come in batches.
#' Others, such as the standard deviation in the prior for a Poisson
#' model (\code{model.prior.sd}), come on their own.  \code{length} shows
#' the length of each batch.
#'
#' The quantiles summarise the distribution of the parameters across iterations,
#' and, if the batch has more than one element, among parameters within a batch.
#' For example, if a batch has 5 elements, and there are 1000 iterations,
#' then \code{parameters} shows (approximately) the quantiles that would
#' be obtained by calling function \code{quantile} on the combined
#' 5 x 1000 = 5000 elements.  The results are only approximate because,
#' to save time, \code{parameters} samples at most 25 iterations.
#'
#' The parameter summaries can be extracted using
#' function \code{\link{parameters}}.
#' 
#' @section Rhat:
#'
#' Obtain potential scale reduction factors - i.e. the 'Rhats' for batches
#' of parameters.   The Rhats are calculated using function
#' \code{\link[coda]{gelman.diag}} from package \pkg{coda}.  However,
#' in line with  Gelman et al (2014: 284-286), each chain is split into two,
#' and the first and second halves compared, to capture variation within each chain,
#' in addition to variation across chains.
#'
#' Potential scale reduction factors are used to diagnose convergence in MCMC
#' simulations.  Values close to 1 suggest that convergence has occurred.
#' See the references below for discussion.  A dot is shown next to a Rhat
#' if the value if greater than 1.1.
#'
#' There is a multivariate of Rhat for use with batches of parameters,
#' but it is prone to numeric problems. \code{fetchSummary} calculates
#' Rhats parameter by parameter, and then reports the maximum value,
#' which is a conservative approach. To save time, if a batch of parameters
#' contains more than 25 elements, a sample of 25 parameters is extracted,
#' and Rhats calculated for those. The sampling introduces an element
#' of randomness: if \code{fetchSample} is called twice on the same
#' \code{filename} the results for \code{Rhat} will differ.
#'
#' When the number of chains (\code{nChain}), the number of iterations
#' (\code{nSim}), or the proportion of proposals accepted is small,
#' estimated Rhats can be unstable.
#'
#' If greater control over the calculation of Rhat is desired, parameter
#' estimates can be extracted using \code{\link{fetchMCMC}}, and
#' results calculated using \code{\link[coda]{gelman.diag}}.  
#'
#' @return An object of class \code{\linkS4class{SummaryResults}}.
#'
#' @seealso Individual components of the summary can be extracted
#' using functions \code{\link{metropolis}}, \code{\link{parameters}},
#' and \code{\link{gelmanDiag}}.  Parameter estimates from estimation
#' or prediction are obtained using function \code{fetch}.
#'
#' @references
#' Geyer, C. J. (2011) Introduction to Markov Chain Monte Carlo.
#' Brooks, S., Gelman, A., Jones, G., Meng, X-L (eds.) \emph{Handbook of Markov
#' Chain Monte Carlo}. CRC Press.
#'
#' Gelman, A., Shirley, K. (2011) Inference from simulations and monitoring
#' convergence.  Brooks, S., Gelman, A., Jones, G., Meng, X-L (eds.)
#' \emph{Handbook of Markov Chain Monte Carlo}. CRC Press.
#'
#' Gelman, A., Carlin, J.B., Stern, H.S. and Rubin, D.B., 2014.
#' \emph{Bayesian Ddata Analysis. Third Edition.} Boca Raton, FL, USA:
#' Chapman & Hall/CRC.
#'
#' @examples
#' deaths <- demdata::VADeaths2
#' popn <- demdata::VAPopn
#' deaths <- round(deaths)
#' deaths <- Counts(deaths)
#' popn <- Counts(popn)
#' filename <- tempfile()
#' model <- Model(y ~ Poisson(mean ~ age + sex),
#'                jump = 0.5)
#' estimateModel(model = model,
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 50,
#'               nSim = 50,
#'               nChain = 2,
#'               parallel = FALSE)
#' fetchSummary(filename)
#' 
#' ## keep summary object and extract parts from it
#' summary.est <- fetchSummary(filename)
#' gelmanDiag(summary.est)
#' metropolis(summary.est)
#' parameters(summary.est)
#' @export
fetchSummary <- function(filename) {
    object <- fetchResultsObject(filename)
    summary(object = object, filename = filename)
}

## HAS_TESTS
#' Finite-population standard deviations.
#' 
#' Extract the finite-population standard deviation of main effects
#' and interactions in hierarchical models.
#' 
#' In the hierarchical models discussed in \code{\link{Model}}, cell means
#' are moderned using terms formed from cross-classifying dimensions such as
#' age or sex, or from interactions between these dimensions.  Adapting ideas
#' from ANOVA, Gelman (2004: 408-409) propose that the finite-population
#' standard deviations of these terms be used as a measure of the terms'
#' relative importance.  For instance, is an 'age' term has a greater
#' finite-population standard deviation than a 'region' term in a model, this
#' suggests that age is a more important predictor of outcomes than region.
#' 
#' @inheritParams fetchMCMC
#' @param probs A vectors of quantiles, for summarizing the standard deviations.
#' @param iterations A vector of positive integers giving the iterations to be
#'     extracted.
#' 
#' @return A list of objects of class \code{"FiniteSD"} if
#' \code{object} contains several hierarchical models; a single
#' \code{"FiniteSD"} object if' \code{object} contains
#' a single hierchical model; and \code{NULL} if \code{object}
#' contains no hierarchical models.
#'
#' @seealso \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' \code{\link{estimateAccount}}
#'
#' @references Gelman, A. (2004) Analysis of variance:
#' why it is more important than ever
#' (with discussion). \emph{Annals of Statistics}. 33(1): 1-53.
#' @export
fetchFiniteSD <- function(filename, probs = c(0.025, 0.5, 0.975), iterations = NULL) {
    object <- fetchResultsObject(filename)
    finiteSDObject(object = object,
                   filename = filename,
                   probs = probs,
                   iterations = iterations)
}


## HAS_TESTS
#' Estimate or predict finite-population quantity 'y'.
#'
#' Estimate or predict finite-population quantities for
#' the toal population, from estimated rates, probabilities,
#' or means for the sampled population, plus information
#' on the sizes of the sampled and total population.
#'
#' Consider a model in which
#'
#' \deqn{y_i \sim G(\gamma_i, n_i)}
#'
#' for some distribution \eqn{G}, such as a Poisson, binomial, or
#' normal distribution.  \eqn{y_i} is a count or other value
#' for cell \eqn{i} within a classification defined by
#' dimensions such as age and sex.  \eqn{\gamma_i} is an
#' unobserved cell-level parameter such as a rate, probability,
#' or mean.  \eqn{n_i} is a cell-level exposure or sample size, which
#' is included in some models, such as Poisson or binomial models,
#' but not all.
#'
#' We assume that \eqn{y_i} is observed for only part of
#' the population (the sampled population), and would like to
#' know \eqn{Y_i}, the corresponding value for the total population.
#' This requires estimating values for the unobserved' part
#' of the population (the nonsampled population).  We assume that
#' the unobserved part of the population is subject to the same
#' \eqn{\gamma_i} as the observed part.
#'
#' Quantities \eqn{y_i} and \eqn{Y_i} are finite-population
#' quantities, that is, they are values that are, or
#' could theoretically be, observed in real populations.
#' Value \eqn{\gamma_i}, in contrast, is a super-population
#' quantity. It describes the hypothetical super-population
#' from which values such as \eqn{y_i} and \eqn{Y_i} are
#' drawn.
#'
#' @inheritParams fetch
#' 
#' @param total An object of class \code{\linkS4class{Counts}}
#' giving the size of the population for which the finite-population
#' estimates are to be generated.
#' @param sampled An object of class \code{\linkS4class{Counts}}
#' giving the same of the sample from which the rates, means, or
#' probabilities were estimated.  If no \code{sampled} argument
#' is supplied, and if the model includes an \code{exposure}
#' term, then \code{sampled} is assumed to equal \code{exposure}.
#'
#' @seealso \code{finiteY} can be used with the results from
#' a call to \code{estimate} or \code{predict} functions such
#' as \code{\link{estimateModel}} and \code{\link{predictModel}}.
#' 
#' @references
#' Gelman, A., Carlin, J.B., Stern, H.S. and Rubin, D.B., 2014.
#' \emph{Bayesian Ddata Analysis. Third Edition.} Boca Raton, FL, USA:
#' Chapman & Hall/CRC. Section 8.3.
#' 
#' @examples 
#' ## generate sampled, non-sampled, and total population
#' popn.sampled <- Counts(array(rpois(n = 20, lambda = 100),
#'                              dim = c(10, 2),
#'                              dimnames = list(region = LETTERS[1:10],
#'                                  sex = c("Female", "Male"))))
#' popn.nonsampled <- Counts(array(rpois(n = 20, lambda = 900),
#'                              dim = c(10, 2),
#'                              dimnames = list(region = LETTERS[1:10],
#'                                  sex = c("Female", "Male"))))
#' popn.total <- popn.sampled + popn.nonsampled
#'
#' ## binomial model
#' y.sampled.binom <- Counts(array(rbinom(n = 20, size = popn.sampled, prob = 0.5),
#'                           dim = c(10, 2),
#'                           dimnames = list(region = LETTERS[1:10],
#'                               sex = c("Female", "Male"))))
#' filename.binom <- tempfile()
#' estimateModel(Model(y ~ Binomial(mean ~ region + sex)),
#'               y = y.sampled.binom,
#'               exposure = popn.sampled,
#'               filename = filename.binom,
#'               nBurnin = 10,
#'               nSim = 10,
#'               nChain = 2)
#' fy.binom <- finiteY(filename = filename.binom, # sample population assumed
#'                     total = popn.total)        # to equal exposure
#'
#' ## normal model
#' y.sampled.norm <- Counts(array(rnorm(n = 20),
#'                                dim = c(10, 2),
#'                                dimnames = list(region = LETTERS[1:10],
#'                                    sex = c("Female", "Male"))))
#' filename.norm <- tempfile()
#' estimateModel(Model(y ~ Normal(mean ~ region + sex)),
#'               y = y.sampled.norm,
#'               filename = filename.norm,
#'               nBurnin = 10,
#'               nSim = 10,
#'               nChain = 2)
#' fy.norm <- finiteY(filename = filename.norm,
#'                    sampled = popn.sampled, # specify sample population
#'                    total = popn.total)
#'@export
finiteY <- function(filename, total, sampled = NULL, iterations = NULL) {
    object <- fetchResultsObject(filename)
    if (!methods::is(object, "ResultsModel"))
        stop(gettextf("results object has class \"%\"", class(object)))
    if (identical(dembase::nIteration(object), 0L))
        return(NULL)
    model <- object@final[[1L]]@model
    has.exposure <- methods::is(model, "UseExposure")
    y.sampled <- fetch(filename, where = "y", iterations = iterations, impute = TRUE)
    total <- checkAndTidyTotalOrSampled(x = total,
                                        model = model,
                                        ySampled = y.sampled,
                                        name = "total")
    if (is.null(sampled)) {
        if (has.exposure)
            sampled <- fetch(filename, where = "exposure")
        else
            stop(gettextf("argument '%s' is missing with no default",
                          "sampled"))
    }            
    else
        sampled <- checkAndTidyTotalOrSampled(x = sampled,
                                              model = model,
                                              ySampled = y.sampled,
                                              name = "sampled")
    nonsampled <- total - sampled
    if (any(nonsampled < 0L))
        stop(gettextf("'%s' has cells with fewer units than corresponding cells in '%s'",
                      "total", "sampled"))
    y.nonsampled <- drawYNonSampled(filename,
                                    model = model,
                                    nonsampled = nonsampled,
                                    iterations = iterations)
    y.sampled + y.nonsampled
}

## NO_TESTS
#' List of output from estimate function.
#' 
#' Calling \code{\link{listContents}} on a the filenmame used by
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' or \code{\link{estimateAccount}} shows the items available to be
#' fetched, in a hierarchical structure.
#' @inheritParams fetchMCMC
#' @param max Maximum depth of hierarchical structure to show.
#' @export
listContents <- function(filename, where = character(), max = NULL) {
    checkMax(max)
    object <- fetchResultsObject(filename)
    l <- makeContentsList(object = object, where = where, max = max)
    showContentsList(l)
    invisible(l)
}


#' Show final model specification.
#'
#' Show the complete specification model used by functions
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' and \code{\link{estimateAccount}}, or by their prediction
#' counterparts such as \code{\link{predictModel}}.
#'
#' When setting up models to be fitted by the estimation functions,
#' users typically only specify some of the details of the priors for
#' the main effects or interactions.  The remaining details are filled in
#' by the estimation functions, based on the input data.  Function
#' \code{showModel} can be used find out what choices the estimation
#' functions made.
#'
#' To see the partial specification constructed by function
#' \code{\link{Model}}, print the model object (typically by typing
#' the name of the object at the console.)  See below for an example.
#' show \code{\link{fetch}}.
#'
#' The \code{trunc-half-t(df, s^2, max)} in the printed results refers to a
#' truncated \code{\link[=halft-distn]{half-t}} distribution with \code{df}
#' degrees of freedom, scale \code{s^2}, and maximum value \code{max}.
#'
#' @inheritParams fetchMCMC
#'
#' @return \code{showModel} is called for its side effect, which is to print a
#' description of the model.
#'
#' @seealso Parameter estimate can be extracted using function
#' \code{\link{fetch}}.
#' 
#' @examples
#' births <- demdata::nz.births.reg
#' popn <- demdata::nz.popn.reg
#' births <- Counts(births, dimscales = c(year = "Intervals"))
#' popn <- Counts(popn, dimscales = c(year = "Intervals"))
#' births <- subarray(births, year > 2005 & year < 2014)
#'
#' filename.est <- tempfile()
#' filename.pred <- tempfile()
#' 
#' model <- Model(y ~ Poisson(mean ~ age * region + year),
#'                year ~ DLM(trend = NULL))
#'
#' ## model specification before estimateModel called
#' model
#'
#' estimateModel(model = model,
#'               y = births,
#'               exposure = popn,
#'               filename = filename.est,
#'               nBurnin = 5,
#'               nSim = 5,
#'               nChain = 2,
#'               parallel = FALSE)
#'
#' ## model specification after estimateModel called
#' showModel(filename.est)
#'
#' predictModel(filenameEst = filename.est,
#'              filenamePred = filename.pred,
#'              n = 5)
#'
#' ## specification used by predictModel
#' showModel(filename.pred)
#' @export
showModel <- function(filename) {
    object <- fetchResultsObject(filename)
    showModelHelper(object = object)
}
