
setClass("Classes",
         contains = c("SpecAMixin",
             "ClassesMixin",
             "MultMixin"))

#' S4 class to specify the covariates term of a prior.
#'
#' An object of class \code{Covariates} holds the information required to
#' construct a covariates term in a prior for a main effect or interaction.
#'
#' @slot formula An R \code{\link[stats]{formula}}.
#' @slot data A data.frame holding the covariate data.
#' @slot contrastsArg A named list of contrasts, as described in the
#' documentation for \code{\link[stats]{model.matrix}}.
#' @slot AEtaIntercept Scale parameter for the normal
#' prior for the intercept.
#' @slot nuEtaCoef Degrees of freedom for the t priors
#' for the coefficients.
#' @slot AEtaCoef Scale parameter for the t priors
#' for the coefficients.
#'
#' @seealso Objects of class \code{Covariates} are created
#' by a call to function \code{\link{Covariates}}.
#' 
#' @export
setClass("Covariates",
         contains = "SpecCovariatesMixin")


#' S4 classes to specify damping terms in DLM priors.
#'
#' Priors specified using \code{\link{DLM}} can include damping terms.  The
#' coefficient can be supplied and treated as known, in which case an object
#' of class \code{DampKnown} is used. Alternatively, the coefficient can be
#' estimated from the data, using a uniform prior, in which case an object
#' of class \code{DampUnknown} is used. \code{Damp} is a virtual superclass
#' for \code{DampKnown} and \code{DampUnknown}.
#'
#' @slot phi Coefficient, which must be between 0 and 1.
#' @slot minPhi Minimum value for coefficient.
#' @slot maxPhi Maximum value for coefficient.
#' 
#' @seealso Objects of class \code{Damp} are created  by a call to function
#' \code{\link{Damp}}.
#'
#' @name Damp-class
NULL

#' @rdname Damp-class
#' @export
setClass("Damp",
         contains = "VIRTUAL")

#' @rdname Damp-class
#' @export
setClass("DampKnown",
         contains = c("Damp", "PhiMixin"))

#' @rdname Damp-class
#' @export
setClass("DampUnknown",
         contains = c("Damp", "PhiMinMaxMixin"))

         
#' S4 classes to specify error terms for priors.
#'
#' An object of class \code{Error} holds the information required to construct
#' an error term in a prior for a main effect or interaction.  Errors can have
#' a normal or \emph{t} distributions.  Error terms with normal distributions are 
#' described by objects of class \code{ErrorNorm}, and error terms with \emph{t}
#' distributions are described by objects of class \code{ErrorRobust}.
#' 
#' \code{ErrorNorm} and \code{ErrorRobust} are both subclasses of virtual
#' class \code{Error}.
#' 
#' The \code{scale} parameters for the normal and t-distribution priors
#' have truncated half-t priors.
#' 
#' @slot nuBeta Degrees of freedom for the \emph{t} distribution. (\code{ErrorRobust} only).
#' @slot nuTau Degrees of freedom for the prior for the \code{scale} parameter.
#' @slot ATau Scale for the prior for the \code{scale} parameter.
#' @slot tauMax Maximum value for the \code{scale} parameter. 
#' 
#' @seealso Objects of class \code{Error} are created by a call to function
#' \code{\link{Error}}.
#' @name Error-class
NULL

#' @rdname Error-class
#' @export
setClass("Error",
         contains = "VIRTUAL")

#' @rdname Error-class
#' @export
setClass("ErrorNorm",
         contains = c("Error", "SpecNormMixin"))

#' @rdname Error-class
#' @export
setClass("ErrorRobust",
         contains = c("Error", "SpecRobustMixin"))


#' An S4 class to specify a truncated half-\emph{t} distribution.
#'
#' Truncated half-\emph{t} distributions are used as priors
#' for standard deviations or scale parameters.
#'
#' @slot nu Degrees of freedom.
#' @slot A Scale.
#' @slot scaleMax Maximum value.
#' @slot mult Multiplier applied to default value for \code{A}.
#'
#' @seealso Objects of class \code{HalfT} are created
#' by calls to function \code{\link{HalfT}}.  
#' @export
setClass("HalfT",
         contains = c("MultMixin",
             "NuMixin",
             "SpecAMixin",
             "SpecScaleMaxMixin"))

#' An S4 class to specify a normal prior for a scalar parameter.
#'
#' An object of class \code{Initial} is used to specify the
#' prior for the initial trend term, typically as part of a
#' call to function \code{\link{DLM}}.
#'
#' @slot A Scale.
#' @slot mean The mean of the normal distribution.  Defaults to 0.
#' @slot mult Multiplier applied to default value for \code{A}.
#'
#' @seealso Objects of class \code{Initial} are created
#' by calls to function \code{\link{Initial}}.  
#' @export
setClass("Initial",
         contains = c("MeanMixin",
                      "MultMixin",
                      "SpecAMixin"))


#' An S4 class to specify the level term in a DLM prior.
#'
#' An object of class \code{Level} is used to specify the
#' level term in a \code{\link{DLM}} prior.
#'
#' @slot nuAlpha Degrees of freedom.
#' @slot AAlpha Scale.
#' @slot omegaAlphaMax Maximum value for standard deviation of
#' innovations.
#' @slot multAlpha Multiplier applied to default value for \code{AAlpha}.
#'
#' @seealso Objects of class \code{Level} are created
#' by calls to function \code{\link{Level}}.  
#' @export
setClass("Level",
         contains = c("MultAlphaMixin",
                      "NuAlphaMixin",
                      "SpecAAlphaMixin",
                      "SpecOmegaAlphaMaxMixin"))


#' An S4 class to specify a normal distribution.
#'
#' An object of class \code{Norm} specifies a normal
#' distribution with mean 0.  It is used as a
#' prior distribution.
#'
#' @slot A Standard deviation.
#'
#' @seealso Objects of class \code{Norm} are created
#'     by calls to function \code{\link{Norm}}.
#'
#' @export
setClass("Norm",
         contains = "SpecAMixin")


#' An S4 class to specify a seasonal effect in a DLM prior.
#'
#' A prior specified using \code{\link{DLM}}
#' can include a seasonal effect.  The innovation
#' term for the seasonal effect has a normal distribution,
#' the standard deviation for which has a truncated
#' half-\emph{t} distribution.
#'
#' @slot nSeason Number of seasons.
#' @slot nuSeason Degrees of freedom of truncated half-t
#'     prior for standard deviation.
#' @slot ASeason Scale of truncated half-t
#'     prior for standard deviation.
#' @slot omegaSeasonMax Maximum value of truncated half-t
#'     prior for standard deviation.
#'
#' @seealso Objects of class \code{Season} are created
#'     using function \code{\link{Season}}.
#'
#' @export
setClass("Season",
         contains = "SpecSeasonMixin")


#' An S4 class to specify the trend term in a DLM prior.
#'
#' An object of class \code{Trend} is used to specify the
#' trend term in a \code{\link{DLM}} prior.
#'
#' @slot ADelta Scale for half-t prior for standard deviation
#' of innovations.
#' @slot ADelta0 Standard deviation of the normal prior
#' for the first trend term.
#' @slot meanDelta0 Mean of the normal prior for the first
#' trend term. Defaults to 0.
#' @slot multDelta Multiplier applied to code{ADelta}.
#' @slot multDelta0 Multiplier applied to code{ADelta0}.
#' @slot nuDelta Degrees of freedom for half-t prior for
#' standard deviation of innovations.
#' @slot omegaDeltaMax Maximum value for standard deviation
#' of innovations.
#'
#' @seealso Objects of class \code{Trend} are created
#' by calls to function \code{\link{Trend}}.  
#' @export
setClass("Trend",
         contains = c("MeanDelta0Mixin",
                      "MultDeltaMixin",
                      "MultDelta0Mixin",
                      "NuDeltaMixin",
                      "SpecADeltaMixin",
                      "SpecADelta0Mixin",
                      "SpecOmegaDeltaMaxMixin"))



setClass("Vectors",
         contains = c("SpecDFVectorsMixin",
                      "SpecScaleVectorsMixin"))
         
