
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
           function(current, target, dominant = c("Female", "Male"))
               standardGeneric("Mapping"))

setGeneric("Skeleton",
           function(object, metadata, first, strucZeroArray = NULL, margin = NULL)
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

setGeneric("betaIsEstimated",
           function(prior)
               standardGeneric("betaIsEstimated"))

setGeneric("castExposure",
           function(exposure, model)
           standardGeneric("castExposure"))

setGeneric("castPopnOrSampled",
           function(x, model, name)
           standardGeneric("castPopnOrSampled"))

setGeneric("castY",
           function(y, spec)
               standardGeneric("castY"))

setGeneric("checkAllDimensionsHavePriors",
           function(model, y) {
               NULL
           })

setGeneric("checkAndTidySimulatedYExposureWeights",
           function(model, y = NULL, exposure = NULL, weights = NULL)
               standardGeneric("checkAndTidySimulatedYExposureWeights"))

setGeneric("checkForSubtotals",
           function(object, model, name = "y") {
               NULL
           })

setGeneric("checkPriorIsInformative",
           function(object)
               standardGeneric("checkPriorIsInformative"))

setGeneric("checkPriorsAreInformative",
           function(object) {
               NULL
           })

setGeneric("checkPriorSDInformative",
           function(object) {
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

#' Decompose a demographic array
#'
#' Decompose a \code{\link[dembase]{DemographicArray}} array into terms
#' made up of component dimensions, plus an error.
#' \code{decomposition} is typically used to obtain
#' initial estimates of main effects and interactions,
#' as part of model building.
#'
#' When building a Poisson model, the decomposition is usually
#' carried out on log-rates, and when building a binomial model,
#' it is usually carried out on logit-proportions, though in both
#' cases other transformations (or no transformation) may be
#' appropriate if there are lots of zeros.
#'
#' The final element in the return value is an 'error'
#' array. This equals the observed value for \code{object} minus the
#' sum of the terms in the decomposition.
#'
#' The \code{max} argument controls the maximum order of
#' the interactions included in decomposition. For instance,
#' if \code{max} is \code{2}, then only main effects and second-order
#' interactions are included in the decomposition.  By default,
#' all interactions are included.
#'
#' Internally, \code{decomposition} calls function
#' \code{\link[dembase]{pairToState}} on \code{object},
#' to cope with origin-destination or parent-child dimensions.
#'
#' @param object An object of class \code{\link[dembase]{Values}}.
#' @param max An integer. Optional.
#'
#' @return A named list, the elements of which have class
#' \code{\link[dembase]{Values}}.
#'
#' @references Chapter 12 of Bryant and Zhang,
#' \emph{Bayesian Demographic Estimation and Forecasting}.
#'
#' @examples
#' deaths <- Counts(demdata::VADeaths2)
#' popn <- Counts(demdata::VAPopn)
#' rates <- deaths/popn
#' log.rates <- log(rates)
#' ans <- decomposition(log.rates)
#' names(ans)
#' ans[1:3]
#' ans[["age:residence"]]
#' mean(log.rates)
#' round(sapply(ans, sum), 5)
#' all.equal(Reduce("+", ans), log.rates)
#' ## main effects only
#' decomposition(log.rates, max = 1)
#' @export
setGeneric("decomposition",
           function(object, max = NULL)
               standardGeneric("decomposition"))

setGeneric("describePrior",
           function(object)
               standardGeneric("describePrior"))

## HAS_TESTS
setGeneric("describePriorsModel",
           function(object) {
               NULL
           })

setGeneric("describePriorsResults",
           function(object)
               standardGeneric("describePriorsResults"))

setGeneric("diffLogDensAccount",
           function(combined, useC = FALSE, useSpecific = FALSE)
               standardGeneric("diffLogDensAccount"))

setGeneric("diffLogLikAccount",
           function(object, useC = FALSE, useSpecific = FALSE)
               standardGeneric("diffLogLikAccount"))

setGeneric("drawCombined",
           function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE)
               standardGeneric("drawCombined"))

setGeneric("drawDataModels",
           function(combined, useC = FALSE)
               standardGeneric("drawDataModels"))

setGeneric("drawHyperParam",
           function(model) {
               model
           })

setGeneric("drawModelNotUseExp",
           function(object, y, useC = FALSE, useSpecific = FALSE)
               standardGeneric("drawModelNotUseExp"))

setGeneric("drawModelUseExp",
           function(object, y, exposure, useC = FALSE, useSpecific = FALSE)
               standardGeneric("drawModelUseExp"))

setGeneric("drawPrior",
           function(prior, useC = FALSE, useSpecific = FALSE)
               standardGeneric("drawPrior"))

setGeneric("drawSystemModels",
           function(combined, useC = FALSE)
               standardGeneric("drawSystemModels"))

setGeneric("drawYNonSampled",
           function(filename, model, nonsampled, iterations)
           standardGeneric("drawYNonSampled"))

#' Convert estimates from a complex survey into a form suitable for
#' analysis with an area-level model
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
                    nIteration, lengthIter, impute = FALSE) {
               object
           })

setGeneric("finiteSDObject",
           function(object, filename, probs = c(0.025, 0.5, 0.975), iterations = NULL)
           standardGeneric("finiteSDObject"))

setGeneric("formula")


setGeneric("getIndicesStrucZero",
           function(object) {
               integer()
           })

setGeneric("getSeriesForDataset",
           function(combined, dataset, filename)
               standardGeneric("getSeriesForDataset"))

setGeneric("getTransform",
           function(object)
           standardGeneric("getTransform"))

setGeneric("hasEstimated",
           function(object)
           standardGeneric("hasEstimated"))

setGeneric("initialCombinedAccount",
           function(account, systemModels, systemWeights,
                    dataModels, seriesIndices, 
                    datasets, namesDatasets, transforms,
                    dominant = c("Female", "Male"),
                    scaleNoise = 0)
               standardGeneric("initialCombinedAccount"))

setGeneric("initialCombinedAccountSimulate",
          function(account, systemModels, systemWeights,
                   dataModels, seriesIndices, 
                   datasets, namesDatasets, transforms,
                   dominant = c("Female", "Male"),
                   updateSystemModel, updateDataModel,
                   scaleNoise = 0)
              standardGeneric("initialCombinedAccountSimulate"))

setGeneric("initialCombinedCounts",
           function(object, y, exposure, dataModels, datasets,
                    namesDatasets, transforms)
               standardGeneric("initialCombinedCounts"))

setGeneric("initialCombinedCountsPredict",
           function(combined, along, labels, n, exposure,
                    covariates, aggregate, lower, upper)
              standardGeneric("initialCombinedCountsPredict"))

setGeneric("initialCombinedModel",
           function(object, y, exposure, weights)
               standardGeneric("initialCombinedModel"))

setGeneric("initialCombinedModelPredict",
           function(combined, along, labels, n, covariates,
                    aggregate, lower, upper, yIsCounts = TRUE)
          standardGeneric("initialCombinedModelPredict"))

setGeneric("initialCombinedModelSimulate",
           function(object, y, exposure, weights)
           standardGeneric("initialCombinedModelSimulate"))

setGeneric("initialModel",
           function(object, y, exposure, weights)
           standardGeneric("initialModel"))

setGeneric("initialModelPredict",
           function(model, along, labels, n, offsetModel,
                    covariates, aggregate, lower, upper)
           standardGeneric("initialModelPredict"))

setGeneric("initialPrior",
           function(object, beta, metadata, sY, isSaturated, margin, strucZeroArray, ...)
               standardGeneric("initialPrior"))

setGeneric("initialPriorPredict",
           function(prior, data, metadata, name, along, margin, strucZeroArray)
           standardGeneric("initialPriorPredict"))

setGeneric("initialVarDLM",
           function(object, J)
           standardGeneric("initialVarDLM"))

setGeneric("logLikelihood",
           function(model, count, dataset, i, useC = FALSE, useSpecific = FALSE)
           standardGeneric("logLikelihood"))

## HAS_TESTS
setGeneric("makeCellInLik",
           function(model, y, strucZeroArray = NULL) {
               y <- as.numeric(y)
               model@cellInLik <- !is.na(y)
               if (!is.null(strucZeroArray)) {
                   is.zero <- strucZeroArray == 0L
                   model@cellInLik[is.zero] <- FALSE
               }
               model
           })

setGeneric("makeTransformExpToComp",
           function(exposure, component, nameComponent)
               standardGeneric("makeTransformExpToComp"))

setGeneric("makeOutputAggregate",
           function(model, pos, nChain, nIteration)
           standardGeneric("makeOutputAggregate"))

setGeneric("makeOutputModel",
           function(model, pos, mcmc)
           standardGeneric("makeOutputModel"))

setGeneric("makeOutputPrior",
           function(prior, metadata, pos, strucZeroArray = NULL, margin = NULL)
               standardGeneric("makeOutputPrior"))

setGeneric("makeResults",
           function(object, finalCombineds, mcmcArgs, controlArgs, seed)
               standardGeneric("makeResults"))

setGeneric("modelUsesWeights",
           function(object) FALSE)

setGeneric("predictBeta",
           function(prior, J, useC = FALSE, useSpecific = FALSE)
               standardGeneric("predictBeta"))

setGeneric("predictCombined",
           function(object, nUpdate = 1L, filename, lengthIter, iteration,
                    useC = FALSE, useSpecific = FALSE)
           standardGeneric("predictCombined"))

setGeneric("predictModelNotUseExp",
           function(object, y, useC = FALSE, useSpecific = FALSE)
               standardGeneric("predictModelNotUseExp"))

setGeneric("predictModelUseExp",
           function(object, y, exposure, useC = FALSE, useSpecific = FALSE)
               standardGeneric("predictModelUseExp"))

setGeneric("predictPrior",
           function(prior, useC = FALSE, useSpecific = FALSE)
               standardGeneric("predictPrior"))

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

setGeneric("rescaleBetasPred",
           function(results, adjustments, filename, nIteration, lengthIter)
               standardGeneric("rescaleBetasPred"))

setGeneric("rescalePairPriors",
          function(priorHigh, priorLow, skeletonBetaHigh, skeletonBetaLow,
                   skeletonsPriorHigh, skeletonsPriorLow,
                   adjustments, prefixAdjustments,
                   filename, nIteration, lengthIter) {
              NULL
          })

setGeneric("rescalePred",
          function(prior, skeleton, adjustment,
                   filename, nIteration, lengthIter) {
               NULL
           })

setGeneric("rescalePriorIntercept",
           function(priorTerm, priorIntercept,
                    skeletonBetaTerm, skeletonBetaIntercept,
                    skeletonsPriorTerm, adjustments, prefixAdjustments,
                    filename, nIteration, lengthIter) {
               NULL
           })

setGeneric("rescalePriors",
           function(results, adjustments, filename, nIteration, lengthIter)
               standardGeneric("rescalePriors"))

setGeneric("rescaleSeason",
           function(prior, skeleton, filename, nIteration, lengthIter) {
               NULL
           })

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

setGeneric("subtractAlpha0",
           function(object, iAlong)
           standardGeneric("subtractAlpha0"))

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
           function(object, nUpdate = 1L,
                    useC = FALSE, useSpecific = FALSE)
           standardGeneric("updateCombined"))

setGeneric("updateExpectedExposure",
           function(combined, useC = FALSE, useSpecific = FALSE)
               standardGeneric("updateExpectedExposure"))

setGeneric("updateModelNotUseExp",
           function(object, y, useC = FALSE, useSpecific = FALSE)
           standardGeneric("updateModelNotUseExp"))

setGeneric("updateModelUseExp",
           function(object, y, exposure, useC = FALSE, useSpecific = FALSE)
           standardGeneric("updateModelUseExp"))

setGeneric("updatePredictedBetaAndPriorBeta",
           function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE)
               standardGeneric("updatePredictedBetaAndPriorBeta"))

setGeneric("updateProposalAccount",
           function(object, useC = FALSE, useSpecific = FALSE)
               standardGeneric("updateProposalAccount"))

setGeneric("updateSystemModels",
           function(combined, useC = FALSE, useSpecific = FALSE)
               standardGeneric("updateSystemModels"))

setGeneric("updateValuesAccount",
           function(combined, useC = FALSE, useSpecific = FALSE)
               standardGeneric("updateValuesAccount"))

setGeneric("usesExposure",
           function(object)
               standardGeneric("usesExposure"))

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


