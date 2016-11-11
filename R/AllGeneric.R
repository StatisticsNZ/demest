
#' Print description of model or prior.
#'
#' Printing an object of class \code{\linkS4class{SpecModel}},
#' \code{\linkS4class{SpecLikelihood}}, or \code{\linkS4class{SpecPrior}} gives
#' an informal mathematical description of a model or prior.  The description
#' is typically incomplete, since defaults for priors or hyperparameters
#' cannot be determined until the data are known, which happens when
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}}, or
#' \code{\link{estimateAccount}} is called.  The final specification can be
#' seen by calling function \code{\link{showModel}} after the \code{estimate}
#' function has run.
#'
#' The \code{trunc-half-t(df, s^2, max)} in the printed results refers to a
#' truncated \code{\link[=halft-distn]{half-t}} distribution with \code{df}
#' degrees of freedom, scale \code{s^2}, and maximum value \code{max}.
#'
#' @param object Object of class \code{\linkS4class{SpecModel}},
#' \code{\linkS4class{SpecLikelihood}}, or \code{\linkS4class{SpecPrior}}.
#'
#' @seealso \code{\link{showModel}} for a more complete description of the
#' model.
#'
#' @name show-methods
NULL


setGeneric("CohortIterator",
           function(object)
               standardGeneric("CohortIterator"))

setGeneric("Description",
           function(object)
               standardGeneric("Description"))

setGeneric("Mapping",
           function(current, target)
               standardGeneric("Mapping"))

setGeneric("Skeleton",
           function(object, metadata, first)
           standardGeneric("Skeleton"))

setGeneric("SkeletonAccept",
           function(nAttempt, first, nChain, nIteration)
               standardGeneric("SkeletonAccept"))

setGeneric("SkeletonMissingData",
           function(object, model, outputModel, exposure)
               standardGeneric("SkeletonMissingData"))

setGeneric("SkeletonMissingDataset",
           function(object, model, outputModel,
                    transformComponent, skeletonComponent)
               standardGeneric("SkeletonMissingDataset"))

setGeneric("SpecModel",
           function(specInner, call, nameY, dots, 
                    lower, upper, priorSD, jump,
                    series, aggregate)
               standardGeneric("SpecModel"))

setGeneric("SummaryModel",
           function(object, ModelCall, dimensions)
           standardGeneric("SummaryModel"))

setGeneric("addAg",
           function(model, aggregate, defaultWeights)
           standardGeneric("addAg"))

setGeneric("castExposure",
           function(exposure, model)
           standardGeneric("castExposure"))

setGeneric("castPopnOrSampled",
           function(x, model, name)
           standardGeneric("castPopnOrSampled"))

setGeneric("castY",
           function(y, spec)
           standardGeneric("castY"))

setGeneric("checkForSubtotals",
           function(object, model, name = "y") {
               NULL
           })

setGeneric("classY",
           function(y)
           stop(gettextf("cannot handle '%s' with class \"%s\"",
                         "y", class(y))))

setGeneric("combineEstPred",
           function(est, pred)
               standardGeneric("combineEstPred"))

setGeneric("concatDimScaleFirstSecond",
           function(first, second, name) {
               if (identical(class(first), class(second)))
                   stop(gettextf("\"%s\" dimension [\"%s\"] has %s \"%s\"",
                                 "along", name, "dimscale", class(first)))
               else
                   stop(gettextf("new \"%s\" dimension [\"%s\"] has dimscales \"%s\" and \"%s\"",
                                 "along", name, class(first), class(second)))
           })

setGeneric("drawYNonSampled",
           function(filename, model, nonsampled, iterations)
           standardGeneric("drawYNonSampled"))

#' Convert estimates from a complex survey into a form suitable for
#' analysis with an area-level model.
#'
#' Take estimates of probabilities or rates from a complex survey,
#' along with the associated standard errors, and turn them into counts and
#' effective sample sizes that can be analysed using area-level models,
#' such as those implemented by \code{\link{estimateModel}}.
#'
#' Most surveys of households or individuals firms are 'complex', in that
#' they use techniques such as stratification and clustering which do
#' not yield simple random samples from the population of
#' interest.  When analysing data from complex surveys, the survey design must
#' be taken into account.
#'
#' The cleanest way to take the survey design into account is to include in
#' the model all the features of the survey design that are likely to be
#' correlated with the outcome variables.  For instance, if the survey
#' uses stratification, then the model should include strata-level effects.
#' (Gelman et al 2014, Chapter 8.)
#'
#' Incorporating all the relevant features of a complex survey is much
#' easier with individual-level models than with the cell-level or
#' area-level models implemented by \code{\link{estimateModel}}.  Some
#' applications are, however, more suited to area-level models than
#' to individual-level models.  An example is the forecasting of area-level
#' quantities such as disease prevalence. Statisticians therefore looked for
#' ways of fitting area-level models to data from complex surveys.
#'
#' Function \code{equivalentSample} implements the methods developed by Chen,
#' Wakefield, and Lumley (2014).  The input to the function are estimates of
#' rates or probabilities for each area, together with the standard
#' errors for these estimates.  These estimates arecalculated from complex
#' survey data using methods that account for the complex survey design
#' (Lumley 2011). Function \code{equivalentSample} converts the estimates into
#' counts and sample sizes (which, to be consististent with the terminology
#' used by \code{estimateModel}, we refer to as exposures.)
#' The counts and exposures contain the same information as the original
#' rates or probabilities and standard errors, in that Poisson or binomial
#' distributions formed from them have the same (or nearly the same) means
#' and variances as the original design-based estimates.  The analysis can then
#' proceed as if the counts and exposures had been obtained through simple random
#' sampling from the population of interest.
#'
#' Both \code{mean} and \code{se} can contain \code{NA}s.  The corresponding
#' counts and exposures will also be \code{NA}.  Function
#' \code{estimateModel} accepts missing values for \code{y} and
#' \code{exposure}, though a cell in \code{exposure} can only be missing
#' if the correspondent cell in \code{y} is missing.
#' 
#' Design-based methods for calculating standard errors can break down
#' when cell sizes are small, giving standard errors of zero.
#' \code{equivalentSamples} treats as missing elements of \code{se} that
#' are equal to 0.
#'
#' @param mean An object of class \code{\linkS4class{Values}} holding
#' the estimated probabilities or rates from the complex survey.
#' @param se An object of class \code{\linkS4class{Values}} holding
#' standard errors for the elements of \code{mean}.
#' @param to The distribution that the effective counts are generated
#' from: \code{"binomial"} (the default) or \code{"Poisson"}.
#' Can be abbreviated.
#' @param epsilon A small number.  Values for \code{se} less than
#' \code{epsilon} are treated as equal to 0.
#'
#' @return A named list with elements \code{y} (the counts) and
#' \code{exposure}, both of which have class \code{\linkS4class{Counts}}.
#'
#' @seealso Package \code{survey} contains tools for estimating rates,
#' probabilities, and standard errors from complex surveys.  The output from
#' \code{equivalentSample} are typically analysed using function
#' \code{\link{estimateModel}}.
#' 
#' @references
#' Lumley, T. (2011). \emph{Complex Surveys: A Guide to Analysis using R}
#' John Wiley & Sons.
#'
#' Chen C, Wakefield J, and Lumley T (2014). "The use of sampling weights
#' in Bayesian hierarchical models for small area estimation".
#' \emph{Spatial and Spatio-temporal Epidemiology}. 11: 33-43.
#'
#' Gelman, A., Carlin, J.B., Stern, H.S. and Rubin, D.B., 2014.
#' \emph{Bayesian Ddata Analysis. Third Edition.} Chapman & Hall/CRC.
#' 
#' @examples
#' mean <- demdata::nz.obesity.mean
#' se <- demdata::nz.obesity.se
#' mean <- Values(mean, dimscales = c(time = "Points"))
#' se <- Values(se, dimscales = c(time = "Points"))
#' l <- equivalentSample(mean = mean, se = se, to = "Poisson")
#' names(l)
#' y <- l$y
#' exposure <- l$exposure
#' ## 'y' and 'exposure' can now be analysed using
#' ## function 'estimateModel'
#' @export
setGeneric("equivalentSample",
           function(mean, se, to = c("binomial", "Poisson"), epsilon = 1e-6)
               standardGeneric("equivalentSample"))

setGeneric("fetchResults",
           function(object, nameObject, filename, iterations,
                    nIteration, lengthIter, shift = TRUE,
                    impute = FALSE) {
               object
           })

setGeneric("finiteSDObject",
           function(object, filename, probs = c(0.025, 0.5, 0.975), iterations = NULL)
           standardGeneric("finiteSDObject"))

setGeneric("formula")

setGeneric("getTransform",
           function(object)
           standardGeneric("getTransform"))

setGeneric("hasEstimated",
           function(object)
           standardGeneric("hasEstimated"))

setGeneric("initialCombinedCounts",
           function(object, y, exposure, observation, datasets,
                    namesDatasets, transforms)
           standardGeneric("initialCombinedCounts"))

setGeneric("initialCombinedModel",
           function(object, y, exposure, weights)
           standardGeneric("initialCombinedModel"))

setGeneric("initialCombinedModelPredict",
           function(combined, along, labels, n, covariates,
                    aggregate, lower, upper, yIsCounts = TRUE)
          standardGeneric("initialCombinedModelPredict"))

setGeneric("initialModel",
           function(object, y, exposure, weights)
           standardGeneric("initialModel"))

setGeneric("initialModelPredict",
           function(model, along, labels, n, offsetModel,
                    covariates, aggregate, lower, upper)
           standardGeneric("initialModelPredict"))

setGeneric("initialPrior",
           function(object, beta, metadata, sY, ...)
               standardGeneric("initialPrior"))

setGeneric("initialPriorPredict",
           function(prior, data, metadata, name, along)
           standardGeneric("initialPriorPredict"))

setGeneric("initialVarDLM",
           function(object, J)
           standardGeneric("initialVarDLM"))

setGeneric("logLikelihood",
           function(model, count, dataset, i, useC = FALSE, useSpecific = FALSE)
           standardGeneric("logLikelihood"))

setGeneric("makeOutputAggregate",
           function(model, pos, nChain, nIteration)
           standardGeneric("makeOutputAggregate"))

setGeneric("makeOutputModel",
           function(model, pos, mcmc)
           standardGeneric("makeOutputModel"))

setGeneric("makeOutputPrior",
           function(prior, metadata, pos)
           standardGeneric("makeOutputPrior"))

setGeneric("needToCenter",
           function(object) FALSE)

setGeneric("predictBeta",
           function(prior, J, useC = FALSE, useSpecific = FALSE)
               standardGeneric("predictBeta"))

setGeneric("predictCombined",
           function(object, nUpdate = 1L, filename, lengthIter, iteration,
                    useC = FALSE, useSpecific = FALSE)
           standardGeneric("predictCombined"))

setGeneric("predictPrior",
           function(prior, zeta, useC = FALSE, useSpecific = FALSE)
               standardGeneric("predictPrior"))

setGeneric("predictModelNotUseExp",
           function(object, y, useC = FALSE, useSpecific = FALSE)
               standardGeneric("predictModelNotUseExp"))

setGeneric("predictModelUseExp",
           function(object, y, exposure, useC = FALSE, useSpecific = FALSE)
               standardGeneric("predictModelUseExp"))

setGeneric("predictVarDLM",
           function(prior, zeta, useC = FALSE, useSpecific = FALSE)
           standardGeneric("predictVarDLM"))

setGeneric("printAgValEqns",
           function(object) invisible())

setGeneric("printAgAccuracyEqns",
           function(object) invisible())

setGeneric("printPriorEqns",
           function(object, name = NULL, order = 1L)
               standardGeneric("printPriorEqns"))

setGeneric("printPriorIntercept",
           function(object)
               standardGeneric("printPriorIntercept"))

setGeneric("printSpecAgAccuracyEqns",
           function(object)
               standardGeneric("printSpecAgAccuracyEqns"))

setGeneric("printSpecAgValEqns",
           function(object, aggregate)
               standardGeneric("printSpecAgValEqns"))

setGeneric("showModelHelper",
           function(object)
               standardGeneric("showModelHelper"))

setGeneric("stringScaleAg",
           function(object) "")

setGeneric("stringScaleTheta",
           function(object) "")

setGeneric("sweepAllMargins",
           function(object)
           standardGeneric("sweepAllMargins"))

setGeneric("summaryDataset",
           function(object)
               standardGeneric("summaryDataset"))

setGeneric("transferParamModel",
          function(model, filename, lengthIter, iteration, useC = FALSE, useSpecific = FALSE)
              standardGeneric("transferParamModel"))

setGeneric("transferParamPrior",
           function(prior, values, useC = FALSE, useSpecific = FALSE)
           standardGeneric("transferParamPrior"))

setGeneric("updateBetaAndPriorBeta",
           function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE)
               standardGeneric("updateBetaAndPriorBeta"))

setGeneric("updateCombined",
           function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE)
           standardGeneric("updateCombined"))

setGeneric("updateModelNotUseExp",
           function(object, y, useC = FALSE, useSpecific = FALSE)
           standardGeneric("updateModelNotUseExp"))

setGeneric("updateModelUseExp",
           function(object, y, exposure, useC = FALSE, useSpecific = FALSE)
           standardGeneric("updateModelUseExp"))

setGeneric("updatePredictedBetaAndPriorBeta",
           function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE)
               standardGeneric("updatePredictedBetaAndPriorBeta"))

setGeneric("whereAcceptance",
           function(object)
           standardGeneric("whereAcceptance"))

setGeneric("whereAutocorr",
           function(object)
           standardGeneric("whereAutocorr"))

setGeneric("whereEstimated",
           function(object)
           standardGeneric("whereEstimated"))

setGeneric("whereFiniteSD",
           function(object)
           standardGeneric("whereFiniteSD"))

setGeneric("whereJump",
           function(object)
           standardGeneric("whereJump"))

setGeneric("whereMetropStat",
           function(object, FUN)
           standardGeneric("whereMetropStat"))

setGeneric("whereNoProposal",
           function(object)
           standardGeneric("whereNoProposal"))

setGeneric("whereTheta",
           function(object)
           standardGeneric("whereTheta"))


