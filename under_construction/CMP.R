



#' @rdname SpecModel-class
#' @export
setClass("SpecCMPVarying",
         prototype = prototype(useExpose = new("LogicalFlag", TRUE)),
         contains = "SpecVarying",
         validity = function(object) {
             lower <- object@lower
             ## 'lower' non-negative
             if (lower < 0)
                 return(gettextf("'%s' is less than %d",
                                 "lower", 0L))
             TRUE
         })




setClass("CMP",
         slots = c(ASDLogNuCMP = "Scale",
                   nuSDLogNuCMP = "DegreesFreedom",
                   sdLogNuCMPMax = "Scale",
                   meanMeanLogNuCMP = "Parameter",
                   sdMeanLogNuCMP = "Scale"),                   
         contains = c("VIRTUAL",
                      "MeanLogCMPMixin",
                      "NuCMPMixin",
                      "SpecSDLogNuCMPMixin",
                      
                      "ScaleThetaMultiplierMixin"),
         validity = function(object) {
             theta <- object@theta
             ## 'theta' is non-negative
             if (any(theta < 0))
                 return(gettextf("'%s' has negative values", "theta"))
             TRUE
         })

setClass("CMPVarying",
         contains = c("VIRTUAL",
                      "Model",
                      "CMP",
                      "Varying"),
         validity = function(object) {
             theta <- object@theta
             lower <- object@lower
             upper <- object@upper
             tolerance <- object@tolerance
             lower.back.tr <- exp(lower)
             upper.back.tr <- exp(upper)
             ## 'theta' greater than or equal to back-transformed 'lower'
             if (any(theta < lower.back.tr - tolerance))
                 return(gettextf("'%s' has values that are less than '%s'",
                                 "theta", "lower"))
             ## 'theta' less than or equal to back-transformed 'upper'
             if (any(theta > upper.back.tr + tolerance))
                 return(gettextf("'%s' has values that are greater than '%s'",
                                 "theta", "upper"))
             TRUE
         })

setClass("CMPVaryingNotUseExp",
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta",
                                   "nAcceptTheta",
                                   "betas",
                                   "sigma",
                                   "priorsBetas"),
             iMethodModel = -999L,
             nuSigma = methods::new("DegreesFreedom", 7)),
        contains = c("CMPVarying",
             "NotUseExposure"))

setClass("CMPVaryingUseExp",
         prototype = prototype(slotsToExtract = c("theta",
                                   "nFailedPropTheta", "nAcceptTheta",
                                   "betas", "sigma", "priorsBetas"),
             iMethodModel = -999L,
             nuSigma = methods::new("DegreesFreedom", 7)),
         contains = c("CMPVarying",
                      "UseExposure"))


setClass("CMPVaryingNotUseExpPredict",
         prototype = prototype(iMethodModel = -999L),
         contains = c("CMPVaryingNotUseExp", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))


setClass("CMPVaryingUseExpPredict",
         prototype = prototype(iMethodModel = -999L),
         contains = c("CMPVaryingUseExp", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))

setClass("Dispersion",
         contains = c("MeanMeanLogNuCMPMixin",
                      "SDMeanLogNuCMPMixin",
                      "SpecAMixin",
                      "MultMixin",
                      "NuMixin",
                      "SpecScaleMaxMixin"))

setMethod("show",
          signature(object = "Dispersion"),
          function(object) {
              mean <- object@meanMeanLogNuCMP@.Data
              sd <- object@sdMeanLogNuCMP@.Data
              nu <- object@nu@.Data
              A <- object@A@.Data
              max <- object@scaleMax@.Data
              cat("An object of class \"", class(object), "\"\n", sep = "")
              cat("log(dispersion[i]) ~ N(mean[i], scale^2)\n")
              cat("           mean[i] ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
              cat("             scale ~ trunc-half-t(", nu, ", ", sep = "")
              cat(squaredOrNA(A), ", ", max, ")\n", sep = "")
          })

Dispersion <- function(mean = Norm(), scale = HalfT()) {
    ## mean
    if (!methods::is(mean, "Norm"))
        stop(gettextf("'%s' has class \"%s\"",
                      mean, class(mean)))
    meanMeanLogNuCMP <- mean@mean
    sdMeanLogNuCMP <- mean@A@.Data
    if (is.na(sdMeanLogNuCMP))
        sdMeanLogNuCMP <- new("Scale", 1)
    else
        sdMeanLogNuCMP <- new("Scale", sdMeanLogNuCMP)
    ## scale
    if (!methods::is(scale, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      scale, class(scale)))
    A <- scale@A
    mult <- scale@mult
    nu <- scale@nu
    scaleMax <- scale@scaleMax
    methods::new("Dispersion",
                 meanMeanLogNuCMP = meanMeanLogNuCMP,
                 sdMeanLogNuCMP = sdMeanLogNuCMP,
                 A = A,
                 mult = mult,
                 nu = nu,
                 scaleMax = scaleMax)
}
Dispersion()


#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodCMP",
         prototype = prototype(useExpose = new("LogicalFlag", TRUE)),
         contains = c("MeanMeanLogNuCMPMixin",
                      "SDMeanLogNuCMPMixin",
                      "SpecAMixin",
                      "MultMixin",
                      "NuMixin",
                      "SpecScaleMaxMixin",
                      "FormulaMuMixin",
                      "UseExposeMixin"))


CMP <- function(formula, dispersion = Dispersion(), useExpose = TRUE) {
    ## formula
    checkFormulaMu(formula)
    checkForMarginalTerms(formula)
    ## dispersion
    if (!methods::is(dispersion, "Dispersion"))
        stop(gettextf("'%s' has class \"%s\"",
                      dispersion, class(dispersion)))
    meanMeanLogNuCMP <- dispersion@meanMeanLogNuCMP
    sdMeanLogNuCMP <- dispersion@sdMeanLogNuCMP
    A <- dispersion@A
    mult <- dispersion@mult
    nu <- dispersion@nu
    scaleMax <- dispersion@scaleMax
    ## useExpose
    useExpose <- checkAndTidyLogicalFlag(x = useExpose,
                                         name = "useExpose")
    methods::new("SpecLikelihoodCMP",
                 formulaMu = formula,
                 meanMeanLogNuCMP = meanMeanLogNuCMP,
                 sdMeanLogNuCMP = sdMeanLogNuCMP,
                 A = A,
                 mult = mult,
                 nu = nu,
                 scaleMax = scaleMax,
                 useExpose = useExpose)
}
CMP(mean ~ age + sex)



printCMPLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    useExpose <- object@useExpose@.Data
    mean <- object@meanMeanLogNuCMP@.Data
    sd <- object@sdMeanLogNuCMP@.Data
    nu <- object@nu@.Data
    A <- object@A@.Data
    max <- object@scaleMax@.Data
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        cat("              y[i] ~ CMP(rate[i] * exposure[i], dispersion[i])\n")
        cat("      log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("              y[i] ~ CMP(count[i], dispersion[i])\n")
        cat("     log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    cat("log(dispersion[i]) ~ N(mean[i], scale^2)\n")
    cat("           mean[i] ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
    cat("             scale ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", max, ")\n", sep = "")
}


#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodCMP"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printCMPLikEqns(object)
          })

