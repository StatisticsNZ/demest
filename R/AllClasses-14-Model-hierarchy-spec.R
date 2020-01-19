

#' S4 class to specify prior for dispersion parameter in CMP model.
#'
#' Specify the mean and standard deviation of (the log of) the
#' dispersion parameter ('nu') in a CMP model.  Also specify
#' the prior mean and standard deviation of the mean.
#'
#' @section Warning:
#' In  normal usage, it should not be necessary to
#' access, or even know about, the slots of a
#' \code{Dispersion} object.  The slots are not part of
#' the API of the package, and may change in future.
#'
#' @slot meanLogNuCMP The prior mean of the dispersion parameter.
#' @slot sdLogNuCMP The prior standard deviation of the
#' dispersion parameter.
#' 
#' @seealso Objects of class \code{Dispersion} are created
#' by calls to function \code{\link{Dispersion}}.
#'
#' @export
setClass("Dispersion",
         contains = c("MeanLogNuCMPMixin",
                      "SDLogNuCMPMixin"))


#' S4 classes to specify one or two levels of a model.
#'
#' Classes to specify the likelihood and, if the model
#' has higher levels, part or all of the second level.
#'
#' @section Warning:
#' In  normal usage, it should not be necessary to
#' access, or even know about, the slots of a
#' \code{SpecLikelihood} object.  The slots are not part of
#' the API of the package, and may change in future.
#'
#' @slot formulaMu Object of class \code{\link[stats]{formula}}
#' specifying main effects and interactions in second level
#' of model.
#' @slot varsigma Data-level standard deviation,
#' when this is supplied by the user (and treated
#' as known.)
#' @slot nuVarsigma Degrees of freedom for truncated
#' half-t prior for data-level standard deviation.
#' @slot AVarsigma Scale for truncated
#' half-t prior for data-level standard deviation.
#' @slot varsigmaMax Maximum value for data-level
#' standard deviation.
#' @slot mean Vector of means.
#' @slot sd Vector of standard deviations.
#' @slot metadata Metadata for \code{mean} and \code{sd}.
#'
#' @seealso Objects of class \code{SpecLikelihood}
#' are created by calls to function such as
#' \code{\link{Poisson}}, \code{\link{Binomial}},
#' \code{\link{Normal}}, and \code{\link{PoissonBinomial}}.
#'
#' @export
setClass("SpecLikelihood",
         contains = "VIRTUAL")

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodBinomial",
         contains = c("SpecLikelihood",
                      "FormulaMuMixin"))

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodCMP",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecLikelihood",
                      "BoxCoxParamMixin",
                      "StructuralZerosMixin",
                      "MeanLogNuCMPMixin",
                      "SDLogNuCMPMixin",
                      "FormulaMuMixin",
                      "UseExposeMixin"))

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodNormalVarsigmaKnown",
         contains = c("SpecLikelihood",
             "FormulaMuMixin",
             "VarsigmaMixin",
             "VarsigmaSetToZeroMixin"))

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodNormalVarsigmaUnknown",
         contains = c("SpecLikelihood",
             "FormulaMuMixin",
             "NuVarsigmaMixin",
             "SpecAVarsigmaMixin",
             "SpecVarsigmaMaxMixin"))

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodPoisson",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecLikelihood",
                      "BoxCoxParamMixin",
                      "FormulaMuMixin",
                      "StructuralZerosMixin",
                      "UseExposeMixin"))

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodPoissonBinomialMixture",
         contains = c("SpecLikelihood",
                      "Prob"))

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodNormalFixed",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecLikelihood",
                      "MeanSDMetadataMixin",
                      "UseExposeMixin"))

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodRound3",
         contains = "SpecLikelihood")

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodTFixed",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecLikelihood",
                      "MeanSDMetadataMixin",
                      "NuMixin",
                      "UseExposeMixin"))



validity_LN2 <- function(object) {
    constraintLN2 <- object@constraintLN2
    concordances <- object@concordances
    ## 'constraintLN2' has type "integer"
    if (!is.integer(constraintLN2))
        return(gettextf("'%s' does not have type \"%s\"",
                        "constraintLN2", "integer"))
    ## all values of 'constraintLN2' valid
    is.invalid <- !(constraintLN2@.Data %in% c(NA, -1L, 0L, 1L))
    i.invalid <- match(TRUE, is.invalid, nomatch = 0L)
    if (i.invalid > 0L)
        return(gettextf("'%s' has invalid value [%s]",
                        "constraintLN2", constraintLN2@.Data[[i.invalid]]))
    ## concordances
    if (!identical(concordances, list())) {
        if (!is.list(concordances))
            stop(gettextf("'%s' has class \"%s\"",
                          "concordances", class(concordances)))
        if (!all(sapply(concordances, methods::is,"ManyToOne")))
            stop(gettextf("'%s' has elements not of class \"%s\"",
                          "concordances", "ManyToOne"))
        names.conc <- names(concordances)
        if (is.null(names.conc))
            stop(gettextf("'%s' does not have names",
                          "concordances"))
        if (any(duplicated(names.conc)))
            stop(gettextf("'%s' has duplicate names",
                          "concordances"))
    }
    TRUE
}

#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodLN2",
         contains = c("SpecLikelihood",
                      "MultVarsigmaMixin",
                      "NuVarsigmaMixin",
                      "SpecAVarsigmaMixin",
                      "SpecVarsigmaMaxMixin",
                      "StructuralZerosMixin"),
         slots = c(constraintLN2 = "Values",
                   concordances = "list"),
         validity = validity_LN2)




#' S4 classes to specify a model.
#'
#' Classes describing all the parts of a model that can
#' be specified wihout knowing the exact structure of,
#' and, possibly variation in, the data.
#'
#' The specification might, for instance, contain a list
#' of main effects and interactions to be included
#' in a hierarchical model, but not the priors for
#' themain effects and interactions,  which depend
#' on the length and \code{\link[dembase]{dimtype}}
#' of the dimensions.  These are not known until the
#' relevant data, such as outcome variable \code{y},
#' are supplied, which occurs in the call to function
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' or \code{\link{estimateAccount}}.
#'
#' @section Warning:
#' In  normal usage, it should not be necessary to
#' access, or even know about, the slots of a
#' \code{SpecModel} object.  The slots are not part of
#' the API of the package, and may change in future.
#' 
#' @slot call The original \code{\link[base]{call}}
#' to function \code{\link{Model}}.
#' @slot nameY The name of the outcome variable,
#' which, in models for the data, may be the name
#' of a dataset.
#' @slot series The name of the demographic series
#' being modelled.  Used only when dealing with demographic
#' accounts.
#' @slot varsigma Data-level standard deviation,
#' when this is supplied by the user (and treated
#' as known.)
#' @slot nuVarsigma Degrees of freedom for truncated
#' half-t prior for data-level standard deviation.
#' @slot AVarsigma Scale for truncated
#' half-t prior for data-level standard deviation.
#' @slot varsigmaMax Maximum value for data-level
#' standard deviation.
#' @slot prob In a Poisson-binomial model, the probability
#' that a person or event is enumerated and is placed in
#' the correct cell.
#' @slot lower Lower limit for the data-level rate,
#' probability, or mean parameter.
#' @slot upper Upper limit for the data-level rate,
#' probability, or mean parameter.
#' @slot tolerance Small quantity added to \code{lower}
#' or subtracted from \code{upper} when testing
#' whether a proposed value for a data-level rate,
#' probability, or mean is within the required bounds.
#' @slot maxAttempt Maximum number of attempts at
#' generating a proposal for a data-level rate,
#' probability or mean before giving up and retaining
#' the current value, within one iteration of the
#' Gibbs sampler.
#' @slot scaleTheta The standard deviation of the
#' proposal density for Metropolis-Hastings updates
#' of the data-level rate, probability, or mean
#' parameter.
#' @slot formulaMu A \code{\link[stats]{formula}}
#' describing the main effects and interactions in
#' a hierarchical model.
#' @slot specsPriors A list of object of class
#' \code{\linkS4class{SpecPrior}}, describing
#' any non-default priors for main effects and
#' interactions.
#' @slot nameSpecPriors The names of the main effects
#' or interactions that have non-default priors.
#' @slot nu Degrees of freedom for TFixed model.
#' @slot nuSigma Degrees of freedom for truncated
#' half-t prior for standard deviation in prior
#' (level 2) model.
#' @slot ASigma Scale for truncated
#' half-t prior for standard deviation in prior
#' (level 2) model.
#' @slot sigmaMax Maximum value for standard deviation
#' in prior (level 2) model.
#' @slot aggregate An object of class
#' \code{\linkS4class{SpecAggregate}}.
#' @slot mean Vector of means in NormalFixed or TFixed model - subsetted to
#' include only cells that are observed in \code{y}.
#' @slot meanAll Vector of means in NormalFixed or TFixed model, before
#' subsetting.
#' @slot sd Vector of standard deviations or scales in NormalFixed or TFixed
#' model - subsetted to include only cells that are observed in \code{y}
#' @slot sdAll Vector of standard deviations or scales in NormalFixed model,
#' or TFixed model before subsetting.
#' @slot metadata Metadata for \code{mean} and \code{sd}.
#' @slot metadataAll Metadata for \code{meanAll} and \code{sdAll}.
#' @slot useExpose Whether the model includes and exposure term.
#' 
#' @seealso Object of class \code{SpecModel} are created
#' by a call to function \code{\link{Model}}.
#' 
#' @export
setClass("SpecModel",
         slots = c(call = "call"),
         contains = c("VIRTUAL",
                      "NameYMixin",
                      "UseExposeMixin"))


setClass("SpecVarying",
         contains = c("VIRTUAL",
                      "SpecModel",
                      "ScaleThetaMixin",
                      "SpecASigmaMixin",
                      "SpecAggregateMixin",
                      "FormulaMuMixin",
                      "LowerUpperMixin",
                      "MaxAttemptMixin",
                      "SpecSigmaMaxMixin",
                      "SpecsPriorsMixin",
                      "NuSigmaMixin",
                      "SpecSeriesMixin",
                      "SpecAggregate"))

## HAS_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecBinomialVarying",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecVarying",
                      "SpecAgNotPoissonMixin"),
         validity = function(object) {
             lower <- object@lower
             upper <- object@upper
             ## 'lower' non-negative
             if (lower < 0)
                 return(gettextf("'%s' is less than %d",
                                 "lower", 0L))
             ## 'upper' less than or equal to 1
             if (upper > 1)
                 return(gettextf("'%s' is greater than %d",
                                 "upper", 1L))
             TRUE
         })


#' @rdname SpecModel-class
#' @export
setClass("SpecCMPVarying",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecVarying",
                      "BoxCoxParamMixin",
                      "MeanLogNuCMPMixin",
                      "MultSigmaMixin",
                      "SDLogNuCMPMixin",
                      "StructuralZerosMixin"),
         validity = function(object) {
             lower <- object@lower
             ## 'lower' non-negative
             if (lower < 0)
                 return(gettextf("'%s' is less than %d",
                                 "lower", 0L))
             TRUE
         })

## HAS_TESTS
setClass("SpecNormalVarying",
         contains = c("VIRTUAL",
                      "MultSigmaMixin",
                      "SpecVarying",
                      "SpecAgNotPoissonMixin"))

## HAS_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecNormalVaryingVarsigmaUnknown",
         prototype = prototype(useExpose = methods::new("LogicalFlag", FALSE)),
         contains = c("SpecNormalVarying",
                      "NuVarsigmaMixin",
                      "SpecAVarsigmaMixin",
                      "SpecVarsigmaMaxMixin"))

## HAS_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecNormalVaryingVarsigmaKnown",
         prototype = prototype(useExpose = methods::new("LogicalFlag", FALSE),
                               varsigmaSetToZero = methods::new("LogicalFlag", FALSE)),
         contains = c("SpecNormalVarying",
                      "VarsigmaMixin",
                      "VarsigmaSetToZeroMixin"),
         validity = function(object) {
             aggregate <- object@aggregate
             varsigmaSetToZero <- object@varsigmaSetToZero@.Data
             if (varsigmaSetToZero) {
                 ## if varsigma is 0, lower, upper not specified
                 for (name in c("lower", "upper")) {
                     value <- methods::slot(object, name)
                     if (is.finite(value))
                         return(gettextf("'%s' is %d but '%s' is finite",
                                         "varsigma", 0L, name))
                 }
                 ## if varsigma is 0, 'aggregate' not specified
                 if (varsigmaSetToZero) {
                     if (!methods::is(aggregate, "SpecAgPlaceholder"))
                         return(gettextf("'%s' is %d but '%s' has class \"%s\"",
                                         "varsigma", 0L, "aggregate", class(aggregate)))
                 }
             }
             TRUE
         })

## HAS_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecPoissonVarying",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecVarying",
                      "StructuralZerosMixin",
                      "MultSigmaMixin",
                      "BoxCoxParamMixin"),
         validity = function(object) {
             lower <- object@lower
             ## 'lower' non-negative
             if (lower < 0)
                 return(gettextf("'%s' is less than %d",
                                 "lower", 0L))
             TRUE
         })


## HAS_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecNormalFixed",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecModel", "MeanSDMetadataMixin",
                      "SpecSeriesMixin"))

## HAS_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecPoissonBinomialMixture",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecModel", "Prob", "SpecSeriesMixin"))

## HAS_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecRound3",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         contains = c("SpecModel", "SpecSeriesMixin"))




## NO_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecTFixed",
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE),
                               nu = methods::new("DegreesFreedom", 7)),
         contains = c("SpecModel",
                      "MeanSDMetadataMixin",
                      "NuMixin",
                      "SpecSeriesMixin"))

## NO_TESTS
#' @rdname SpecModel-class
#' @export
setClass("SpecLN2",
         contains = c("SpecModel",
                      "MultSigmaMixin",
                      "MultVarsigmaMixin",
                      "NuSigmaMixin",
                      "NuVarsigmaMixin",
                      "SpecASigmaMixin",
                      "SpecAVarsigmaMixin",
                      "SpecSigmaMaxMixin",
                      "SpecVarsigmaMaxMixin",
                      "StructuralZerosMixin",
                      "SpecSeriesMixin",
                      "StructuralZerosMixin"),
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         slots = c(constraintLN2 = "Values",
                   concordances = "list"),
         validity = validity_LN2)
