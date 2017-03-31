
#' A S4 superclass for prior specifications.
#'
#' All classes that describe priors inherit from
#' class \code{SpecPrior}.  Examples include
#' \code{\linkS4class{SpecExchFixed}}, \code{\linkS4class{SpecExch}}
#' \code{\linkS4class{SpecDLM}}, and \code{\linkS4class{SpecMix}}.
#'
#' @export
setClass("SpecPrior",
         contains = "VIRTUAL")


#' A S4 class to specify a simple exchangeable prior.
#'
#' An object of class \code{SpecExchFixed} specifies
#' a prior in which
#'   parameter[j] ~ N(0, sd^2).
#' For details, see the documentation for function
#' \code{\link{ExchFixed}}.
#'
#' @section Warning:
#' In  normal usage, it should not be necessary to
#' access, or even know about, the slots of a
#' \code{SpecExchFixed} object.  The slots are not part of
#' the API of the package, and may change in future.
#' 
#' @slot tau Standard deviation.
#' @slot multTau Multiplier applied to default value for \code{tau}.
#'
#' @seealso
#' Object of class \code{SpecExchFixed} should be generated
#' using function \code{\link{ExchFixed}}.  More complicated
#' exchangeable priors are specified by object of class
#' \code{\linkS4class{SpecExch}}.
#'
#' @export
setClass("SpecExchFixed",
         contains = c("MultTauMixin",
             "SpecPrior",
             "SpecTauMixin"))


#' A S4 class to specify an exchangeable prior.
#'
#' An object of class \code{SpecExch} specifies
#' a prior in which the order of the elements is
#' irrelevant.  The prior consists of an optional
#' covariates term, and an error term. The error term
#' can have a normal or t distribution. For details,
#' see the documentation for function \code{Exch}.
#'
#' @section Warning:
#' In  normal usage, it should not be necessary to
#' access, or even know about, the slots of a
#' \code{SpecExch} object.  The slots are not part of
#' the API of the package, and may change in future.
#' 
#' @slot formula A \code{\link[stats]{formula}} with response
#' \code{mean}.
#' @slot data A data.frame containing covariate data.
#' @slot contrastsArg A named list, the elements which are matrices
#' or names of contrasts functions.
#' @slot AIntercept The standard deviation for the prior
#' for the intercept in the covariates term.
#' @slot nuEtaCoef The degrees of freedom for the prior
#' for the coefficients in the covariates term.
#' @slot AEtaCoef The scale for the prior
#' for the coefficients in the covariates term.
#' @slot nuBeta The degrees of freedom for the error
#' term, if the error term has a t distribution.
#' @slot nuTau The degrees of freedom for the truncated
#' half-t prior for the standard deviation or scale
#' parameter for the error term.
#' @slot ATau The scale for the truncated
#' half-t prior for the standard deviation or scale
#' parameter for the error term.
#' @slot maxTau The maximum value for the standard
#' deviation or scale parameter for the error term.
#'
#' @seealso Object of class \code{SpecExch} are generated
#' using function \code{\link{Exch}}.
#'
#' @name SpecExch-class
#' @export
setClass("SpecExch",
         contains = c("VIRTUAL",
             "SpecPrior"))

#' @rdname SpecExch-class
#' @export
setClass("SpecExchNormZero",
         contains = c("SpecExch",
             "SpecNormMixin"))

#' @rdname SpecExch-class
#' @export
setClass("SpecExchNormCov",
         contains = c("SpecExch",
             "SpecCovariatesMixin",
             "SpecNormMixin"))

#' @rdname SpecExch-class
#' @export
setClass("SpecExchRobustZero",
         contains = c("SpecExch",
             "SpecRobustMixin"))

#' @rdname SpecExch-class
#' @export
setClass("SpecExchRobustCov",
         contains = c("SpecExch",
             "SpecCovariatesMixin",
             "SpecRobustMixin"))


#' An S4 class to specify a dynamic linear model (DLM) prior.
#'
#' An object of class \code{SpecDLM} specifies a prior
#' in which elements that are next to each other are
#' are expected to be more similar than elements that
#' are distant for each other.  DLMs are typically used to
#' model variation over time, but are often also appropriate
#' for variation over age.
#'
#' The prior consists of a level term, optional trend,
#' seasonal, and covariates terms, and an error term.
#' The error term can follow a normal or t distribution.
#' For details, see the documentation for function
#' \code{\link{DLM}}.
#'
#' @section Warning:
#' In  normal usage, it should not be necessary to
#' access, or even know about, the slots of a
#' \code{SpecDLM} object.  The slots are not part of
#' the API of the package, and may change in future.
#' 
#' @slot along The name of the dimension that the
#' time series (or age equivalents) extend along.
#' @slot phi Damping parameter.
#' @slot phiKnown Whether the damping parameter is known
#' or is estimated from the data.
#' @slot minPhi Minimum value for the damping parameter.
#' @slot maxPhi Maximum value for the damping parameter.
#' @slot nuAlpha Degrees of freedom for the prior for
#' the standard deviation for innovations
#' in the level term.
#' @slot AAlpha Scale for the prior for the standard
#' deviation for innovations in the level term.
#' @slot omegaAlphaAMax Maximum for the standard deviation
#' for innovations in the level term.
#' @slot nuDelta Degrees of freedom for the prior for
#' the standard deviation for innovations
#' in the trend term.
#' @slot ADelta Scale for the prior for the standard
#' deviation for innovations in the trend term.
#' @slot omegaDeltaAMax Maximum for the standard deviation
#' for innovations in the trend term.
#' @slot nSeason Number of seasons.
#' @slot nuSeason Degrees of freedom for the prior for
#' the standard deviation for innovations
#' in the season term.
#' @slot ASeason Scale for the prior for the standard
#' deviation for innovations in the season term.
#' @slot omegaSeasonAMax Maximum for the standard deviation
#' for innovations in the season term.
#' @slot formula A \code{\link[stats]{formula}} with response
#' \code{mean}.
#' @slot data A data.frame containing covariate data.
#' @slot contrastsArg A named list, the elements which are matrices
#' or names of contrasts functions.
#' @slot AIntercept The standard deviation for the prior
#' for the intercept in the covariates term.
#' @slot nuEtaCoef The degrees of freedom for the prior
#' for the coefficients in the covariates term.
#' @slot AEtaCoef The scale for the prior
#' for the coefficients in the covariates term.
#' @slot nuBeta The degrees of freedom for the error
#' term, if the error term has a t distribution.
#' @slot nuTau The degrees of freedom for the truncated
#' half-t prior for the standard deviation or scale
#' parameter for the error term.
#' @slot ATau The scale for the truncated
#' half-t prior for the standard deviation or scale
#' parameter for the error term.
#' @slot maxTau The maximum value for the standard
#' deviation or scale parameter for the error term.
#'
#' @seealso Object of class \code{SpecDLM} are generated
#' using function \code{\link{DLM}}.
#'
#' @name SpecDLM-class
#' @export
setClass("SpecDLM",
         contains = c("VIRTUAL",
             "SpecPrior",
             "AlongMixin",
             "MultAlphaMixin",
             "NuAlphaMixin",
             "PhiKnownMixin",
             "PhiMinMaxMixin",
             "SpecAAlphaMixin",
             "SpecOmegaAlphaMaxMixin",
             "SpecPhiMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMNoTrendNormZeroNoSeason",
         contains = c("SpecDLM",
             "SpecNormMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMNoTrendNormZeroWithSeason",
         contains = c("SpecDLM",
             "SpecNormMixin",
             "SpecSeasonMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMNoTrendNormCovNoSeason",
         contains = c("SpecDLM",
             "SpecCovariatesMixin",
             "SpecNormMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMNoTrendNormCovWithSeason",
         contains = c("SpecDLM",
             "SpecCovariatesMixin",
             "SpecNormMixin",
             "SpecSeasonMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMNoTrendRobustZeroNoSeason",
         contains = c("SpecDLM",
             "SpecRobustMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMNoTrendRobustZeroWithSeason",
         contains = c("SpecDLM",
             "SpecRobustMixin",
             "SpecSeasonMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMNoTrendRobustCovNoSeason",
         contains = c("SpecDLM",
             "SpecCovariatesMixin",
             "SpecRobustMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMNoTrendRobustCovWithSeason",
         contains = c("SpecDLM",
             "SpecCovariatesMixin",
             "SpecRobustMixin",
             "SpecSeasonMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMWithTrendNormZeroNoSeason",
         contains = c("SpecDLM",
             "SpecNormMixin",
             "SpecWithTrendMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMWithTrendNormZeroWithSeason",
         contains = c("SpecDLM",
             "SpecNormMixin",
             "SpecSeasonMixin",
             "SpecWithTrendMixin"))
         
#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMWithTrendNormCovNoSeason",
         contains = c("SpecDLM",
             "SpecCovariatesMixin",
             "SpecNormMixin",
             "SpecWithTrendMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMWithTrendNormCovWithSeason",
         contains = c("SpecDLM",
             "SpecCovariatesMixin",
             "SpecNormMixin",
             "SpecSeasonMixin",
             "SpecWithTrendMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMWithTrendRobustZeroNoSeason",
         contains = c("SpecDLM",
             "SpecRobustMixin",
             "SpecWithTrendMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMWithTrendRobustZeroWithSeason",
         contains = c("SpecDLM",
             "SpecRobustMixin",
             "SpecSeasonMixin",
             "SpecWithTrendMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMWithTrendRobustCovNoSeason",
         contains = c("SpecDLM",
             "SpecCovariatesMixin",
             "SpecRobustMixin",
             "SpecWithTrendMixin"))

#' @rdname SpecDLM-class
#' @export
setClass("SpecDLMWithTrendRobustCovWithSeason",
         contains = c("SpecDLM",
             "SpecCovariatesMixin",
             "SpecRobustMixin",
             "SpecSeasonMixin",
             "SpecWithTrendMixin"))

## FINISH THIS CLASS LATER
setClass("SpecICAR",  
         contains = c("VIRTUAL",
             "SpecPrior"))



## SpecKnown

#' A S4 class to specify a Known prior.
#'
#' An object of class \code{SpecExchFixed} specifies
#' a prior in which
#'   parameter[j] ~ N(mean[j], sd[j]^2),
#' where sd[j] can be 0. For details, see the documentation
#' for function \code{\link{Known}}.
#'
#' @section Warning:
#' In  normal usage, it should not be necessary to
#' access, or even know about, the slots of a
#' \code{SpecKnown} object.  The slots are not part of
#' the API of the package, and may change in future.
#' 
#' @slot tau alphaKnown Mean values
#' @slot multTau Multiplier applied to default value for \code{tau}.
#'
#' @seealso
#' Object of class \code{SpecExchFixed} should be generated
#' using function \code{\link{ExchFixed}}.  More complicated
#' exchangeable priors are specified by object of class
#' \code{\linkS4class{SpecExch}}.
#'
#' @export
setClass("SpecKnown",
         contains = c("VIRTUAL",
                      "SpecPrior",
                      "AlphaKnownMixin",
                      "MetadataMixin"))

setClass("SpecKnownCertain",
         contains = "SpecKnown")

setClass("SpecKnownUncertain",
         contains = c("SpecKnown",
                      "AKnownVecMixin"))

## SpecMove

setClass("SpecMove",
         contains = c("VIRTUAL",
             "SpecPrior",
             "ClassesMixin",
             "MultMoveMixin",
             "SpecAMoveMixin"))

setClass("SpecMoveNormZero",
         contains = c("SpecMove",
             "SpecNormMixin"))

setClass("SpecMoveNormCov",
         contains = c("SpecMove",
             "SpecCovariatesMixin",
             "SpecNormMixin"))

setClass("SpecMoveRobustZero",
         contains = c("SpecMove",
             "SpecRobustMixin"))

setClass("SpecMoveRobustCov",
         contains = c("SpecMove",
             "SpecCovariatesMixin",
             "SpecRobustMixin"))


#' An S4 class to specify a Mix prior.
#'
#' An object of class \code{SpecMix} specifies a non-parametric
#' prior, based on a mix of normal distributions, that can
#' be used for modelling interactions, particularly interactions
#' involving time.
#'
#' @section Warning:
#' In  normal usage, it should not be necessary to
#' access, or even know about, the slots of a
#' \code{SpecMix} object.  The slots are not part of
#' the API of the package, and may change in future.
#'
#' @slot along The name of the "along" dimension.
#' @slot indexClassMaxMix The maximum number of components that
#' can be accommodated.
#' @slot minLevelComponentWeight The lower bound for values of
#' \code{levelComponentWeightMix}.
#' @slot maxLevelComponentWeight The upper bound for values of
#' \code{levelComponentWeightMix}.
#' @slot multComponentWeightMix Multiplier applied to the default
#' value for \code{omegaComponentWeightMix}.
#' @slot multLevelComponentWeightMix Multiplier applied to the default
#' value for \code{omegaLevelComponentWeightMix}.
#' @slot multVectorsMix Multiplier applied to the default
#' value for \code{omegaVectorsMix}.
#' @slot nuComponentWeightMix The degrees of freedom for the truncated
#' half-t prior for \code{omegaComponentWeightMix}.
#' @slot nuLevelComponentWeightMix The degrees of freedom for the truncated
#' half-t prior for \code{omegaLevelComponentWeightMix}.
#' @slot nuVectorsMix The degrees of freedom for the truncated
#' half-t prior for \code{omegaVectorsMix}.
#' @slot priorMeanLevelComponentWeightMix Mean for normal prior
#' for \code{meanLevelComponentWeightMix}.
#' @slot priorSDLevelComponentWeightMix Standard deviation for normal prior
#' for \code{meanLevelComponentWeightMix}
#' @slot AComponentWeightMix Scale for truncated half-t prior for
#' \code{omegaComponentWeightMix}.
#' @slot ALevelComponentWeightMix Scale for truncated half-t prior for
#' \code{omegaLevelComponentWeightMix}.
#' @slot AVectorsMix Scale for truncated half-t prior for
#' \code{omegaVectorsMix}.
#' @slot omegaComponentWeightMaxMix Upper limit for truncated half-t prior
#' for \code{omegaComponentWeightMaxMix}.
#' @slot omegaLevelComponentWeightMaxMix Upper limit for truncated half-t prior
#' for \code{omegalevelComponentWeightMaxMix}.
#' @slot omegaVectorsMix Upper limit for truncated half-t prior
#' for \code{omegaVectorsMix}.
#'
#' @seealso Objects of class \code{SpecMix} are generated using function
#' \code{\link{Mix}}.
#' 
#' @name SpecMix-class
#' @export
setClass("SpecMix",
         contains = c("VIRTUAL",
                      "SpecPrior",
                      "AlongMixin",
                      "IndexClassMaxMixMixin",
                      "LevelComponentWeightMinMaxMixin",
                      "MultComponentWeightMixMixin",
                      "MultLevelComponentWeightMixMixin",
                      "MultVectorsMixMixin",
                      "NuComponentWeightMixMixin",
                      "NuLevelComponentWeightMixMixin",
                      "NuVectorsMixMixin",
                      "PriorMeanLevelComponentWeightMixMixin",
                      "PriorSDLevelComponentWeightMixMixin",
                      "SpecAComponentWeightMixMixin",
                      "SpecALevelComponentWeightMixMixin",
                      "SpecAVectorsMixMixin",
                      "SpecOmegaComponentWeightMaxMixMixin",
                      "SpecOmegaLevelComponentWeightMaxMixMixin",
                      "SpecOmegaVectorsMaxMixMixin"))

setClass("SpecMixNormZero",
         contains = c("SpecMix",
             "SpecNormMixin"))

setClass("SpecMixNormCov",
         contains = c("SpecMix",
             "SpecCovariatesMixin",
             "SpecNormMixin"))

setClass("SpecMixRobustZero",
         contains = c("SpecMix",
             "SpecRobustMixin"))

setClass("SpecMixRobustCov",
         contains = c("SpecMix",
             "SpecCovariatesMixin",
             "SpecRobustMixin"))

#' A S4 class to specify a Zero prior.
#'
#' Specify a prior that fixes all values of the associated
#' main effect or interaction to zero.
#'
#' The class has no slots.
#'
#' @seealso
#' An object of class \code{SpecZero} should be generated
#' using function \code{\link{Zero}}.
#'
#' @export
setClass("SpecZero",
         contains = "SpecPrior")
