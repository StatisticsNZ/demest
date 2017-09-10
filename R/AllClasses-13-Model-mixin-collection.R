
setClass("VarsigmaKnown",
         contains = c("VIRTUAL",
             "VarsigmaMixin"))

setClass("VarsigmaUnknown",
         contains = c("VIRTUAL",
             "AVarsigmaMixin",
             "NuVarsigmaMixin",
             "VarsigmaMixin",
             "VarsigmaMaxMixin"))

## HAS_TESTS
setClass("Varying",
         contains = c("VIRTUAL",
             "ASigmaMixin",
             "Betas",
             "CellInLikMixin",
             "LowerUpperMixin",
             "MaxAttemptMixin",
             "NAcceptThetaMixin",
             "NFailedPropThetaMixin",
             "NuSigmaMixin",
             "ScaleThetaMixin",
             "SigmaMaxMixin",
             "SigmaMixin",
             "Theta"),
         validity = function(object) {
             theta <- object@theta
             nFailedPropTheta <- object@nFailedPropTheta
             ## 'nFailedPropTheta' less or equal to length(theta)
             if (nFailedPropTheta > length(theta))
                 return(gettextf("'%s' is greater than the length of '%s'",
                                 "nFailedPropTheta", "theta"))
             TRUE
         })


