% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGeneric.R, R/SpecModel-methods.R,
%   R/SpecPrior-methods.R
\name{show-methods}
\alias{show-methods}
\alias{show,DampKnown-method}
\alias{show,DampUnknown-method}
\alias{show,Dispersion-method}
\alias{show,SpecAgPlaceholder-method}
\alias{show,SpecAgCertain-method}
\alias{show,SpecAgNormal-method}
\alias{show,SpecAgFun-method}
\alias{show,SpecAgPoisson-method}
\alias{show,SpecLikelihoodBinomial-method}
\alias{show,SpecLikelihoodCMP-method}
\alias{show,SpecLikelihoodNormalVarsigmaKnown-method}
\alias{show,SpecLikelihoodNormalVarsigmaUnknown-method}
\alias{show,SpecLikelihoodPoisson-method}
\alias{show,SpecLikelihoodPoissonBinomialMixture-method}
\alias{show,SpecLikelihoodRound3-method}
\alias{show,SpecLikelihoodExact-method}
\alias{show,SpecLikelihoodNormalFixed-method}
\alias{show,SpecLikelihoodTFixed-method}
\alias{show,SpecLikelihoodLN2-method}
\alias{show,SpecBinomialVarying-method}
\alias{show,SpecCMPVarying-method}
\alias{show,SpecNormalVaryingVarsigmaKnown-method}
\alias{show,SpecNormalVaryingVarsigmaUnknown-method}
\alias{show,SpecPoissonVarying-method}
\alias{show,SpecPoissonBinomialMixture-method}
\alias{show,SpecRound3-method}
\alias{show,SpecExact-method}
\alias{show,SpecNormalFixed-method}
\alias{show,SpecTFixed-method}
\alias{show,SpecLN2-method}
\alias{show,Components-method}
\alias{show,Covariates-method}
\alias{show,InvChiSq-method}
\alias{show,Error-method}
\alias{show,HalfT-method}
\alias{show,Initial-method}
\alias{show,Level-method}
\alias{show,Norm-method}
\alias{show,TDist-method}
\alias{show,Trend-method}
\alias{show,Season-method}
\alias{show,SpecDLM-method}
\alias{show,SpecExch-method}
\alias{show,SpecExchFixed-method}
\alias{show,SpecKnown-method}
\alias{show,SpecMix-method}
\alias{show,Weights-method}
\alias{show,SpecZero-method}
\title{Print description of model or prior.}
\usage{
\S4method{show}{DampKnown}(object)

\S4method{show}{DampUnknown}(object)

\S4method{show}{Dispersion}(object)

\S4method{show}{SpecAgPlaceholder}(object)

\S4method{show}{SpecAgCertain}(object)

\S4method{show}{SpecAgNormal}(object)

\S4method{show}{SpecAgFun}(object)

\S4method{show}{SpecAgPoisson}(object)

\S4method{show}{SpecLikelihoodBinomial}(object)

\S4method{show}{SpecLikelihoodCMP}(object)

\S4method{show}{SpecLikelihoodNormalVarsigmaKnown}(object)

\S4method{show}{SpecLikelihoodNormalVarsigmaUnknown}(object)

\S4method{show}{SpecLikelihoodPoisson}(object)

\S4method{show}{SpecLikelihoodPoissonBinomialMixture}(object)

\S4method{show}{SpecLikelihoodRound3}(object)

\S4method{show}{SpecLikelihoodExact}(object)

\S4method{show}{SpecLikelihoodNormalFixed}(object)

\S4method{show}{SpecLikelihoodTFixed}(object)

\S4method{show}{SpecLikelihoodLN2}(object)

\S4method{show}{SpecBinomialVarying}(object)

\S4method{show}{SpecCMPVarying}(object)

\S4method{show}{SpecNormalVaryingVarsigmaKnown}(object)

\S4method{show}{SpecNormalVaryingVarsigmaUnknown}(object)

\S4method{show}{SpecPoissonVarying}(object)

\S4method{show}{SpecPoissonBinomialMixture}(object)

\S4method{show}{SpecRound3}(object)

\S4method{show}{SpecExact}(object)

\S4method{show}{SpecNormalFixed}(object)

\S4method{show}{SpecTFixed}(object)

\S4method{show}{SpecLN2}(object)

\S4method{show}{Components}(object)

\S4method{show}{Covariates}(object)

\S4method{show}{InvChiSq}(object)

\S4method{show}{Error}(object)

\S4method{show}{HalfT}(object)

\S4method{show}{Initial}(object)

\S4method{show}{Level}(object)

\S4method{show}{Norm}(object)

\S4method{show}{TDist}(object)

\S4method{show}{Trend}(object)

\S4method{show}{Season}(object)

\S4method{show}{SpecDLM}(object)

\S4method{show}{SpecExch}(object)

\S4method{show}{SpecExchFixed}(object)

\S4method{show}{SpecKnown}(object)

\S4method{show}{SpecMix}(object)

\S4method{show}{Weights}(object)

\S4method{show}{SpecZero}(object)
}
\arguments{
\item{object}{Object of class \code{\linkS4class{SpecModel}},
\code{\linkS4class{SpecLikelihood}}, or \code{\linkS4class{SpecPrior}}.}
}
\description{
Printing an object of class \code{\linkS4class{SpecModel}},
\code{\linkS4class{SpecLikelihood}}, or \code{\linkS4class{SpecPrior}} gives
an informal mathematical description of a model or prior.  The description
is typically incomplete, since defaults for priors or hyperparameters
cannot be determined until the data are known, which happens when
\code{\link{estimateModel}}, \code{\link{estimateCounts}}, or
\code{\link{estimateAccount}} is called.  The final specification can be
seen by calling function \code{\link{showModel}} after the \code{estimate}
function has run.
}
\details{
The \code{trunc-half-t(df, s^2, max)} in the printed results refers to a
truncated \code{\link[=halft-distn]{half-t}} distribution with \code{df}
degrees of freedom, scale \code{s^2}, and maximum value \code{max}.
}
\seealso{
\code{\link{showModel}} for a more complete description of the
model.
}
