
setClass("ComponentFlags",
         contains = c("VIRTUAL",
             "HasAlphaMoveMixin",
             "HasAlphaDLMMixin",
             "HasAlphaICARMixin",
             "HasAlphaMixMixin",
             "HasCovariatesMixin",
             "HasSeasonMixin"))

setClass("CovariatesMixin",
         contains = c("VIRTUAL",
             "AEtaCoefMixin",
             "AEtaInterceptMixin",
             "ContrastsArgMixin",
             "EtaMixin",
             "FormulaMixin",
             "InfantMixin",
             "NuEtaCoefMixin",
             "PMixin",
             "UEtaCoefMixin",
             "ZMixin"))

setClass("DLMPredictMixin",
         contains = c("VIRTUAL",
             "JOldMixin",
             "IteratorStateOldMixin"))

setClass("ObsError",
         contains = c("VIRTUAL",
             "ATauMixin",
             "IsRobustMixin",
             "NuTauMixin",
             "TauMixin",
             "TauMaxMixin"))

setClass("NormMixin", 
         contains = c("VIRTUAL",
             "ObsError"))

setClass("MixPredictMixin",
         contains = c("VIRTUAL",
                      "DimBetaOldMixin",
                      "JOldMixin",
                      "LevelComponentWeightOldMixMixin"))

setClass("MoveMixin",
         contains = c("VIRTUAL",
             "AMoveMixin",
             "AlphaMoveMixin",
             "IndexClassAlphaMoveMixin",
             "NElementClassAlphaMixin"))

setClass("NoTrendMixin",
         contains = c("VIRTUAL",
             "aNoTrendMixin",
             "CNoTrendMixin",
             "MNoTrendMixin",
             "M0NoTrendMixin",
             "RNoTrendMixin",
             "ToleranceMixin"))

setClass("RobustMixin",
         contains = c("VIRTUAL",
             "ObsError",
             "NuBetaMixin",
             "UBetaMixin"))

setClass("SeasonMixin",
         contains = c("VIRTUAL",
             "ASeasonMixin",
             "aSeasonMixin",
             "CSeasonMixin",
             "MSeasonMixin",
             "M0SeasonMixin",
             "NSeasonMixin",
             "NuSeasonMixin",
             "OmegaSeasonMixin",
             "OmegaSeasonMaxMixin",
             "RSeasonMixin",
             "SMixin"))

setClass("SpecObsMixin", 
         contains = c("VIRTUAL",
             "MultTauMixin",
             "NuTauMixin",
             "SpecATauMixin",
             "SpecTauMaxMixin"))

setClass("SpecNormMixin", 
         contains = c("VIRTUAL",
             "SpecObsMixin"))

setClass("SpecPhiKnown",
         contains = "PhiMixin")

setClass("SpecPhiUnknown",
         contains = "PhiMinMaxMixin")

setClass("SpecRobustMixin",
         contains = c("VIRTUAL",
                      "NuBetaMixin",
                      "SpecObsMixin"))

setClass("SpecCovariatesMixin",
         contains = c("VIRTUAL",
                      "ContrastsArgMixin",
                      "DataMixin",
                      "FormulaMixin",
                      "InfantMixin",
                      "MultEtaCoefMixin",
                      "NuEtaCoefMixin",
                      "SpecAEtaCoefMixin",
                      "SpecAEtaInterceptMixin"),
         validity = function(object) {
             data <- object@data
             formula <- object@formula
             ## 'data' has length 0 iff 'formula' has length 0
             data.length.0 <- length(data) == 0L
             formula.length.0 <- length(formula) == 0L
             if (data.length.0 && !formula.length.0)
                 return(gettextf("'%s' has length %d but '%s' does not",
                                 "data", "formula"))
             if (formula.length.0 && !data.length.0)
                 return(gettextf("'%s' has length %d but '%s' does not",
                                 "formula", "data"))
             TRUE
         })
                 

## NOT FINISHED!!!!!!!!!!!!!!
setClass("SpecMoveMixin",
         contains = c("VIRTUAL",
             "MultMoveMixin",
             "SpecAMoveMixin"))             

setClass("SpecSeasonMixin",
         contains = c("VIRTUAL",
             "MultSeasonMixin",
             "NSeasonMixin",
             "NuSeasonMixin",
             "SpecASeasonMixin",
             "SpecOmegaSeasonMaxMixin"))

setClass("SpecWithTrendMixin",
         contains = c("VIRTUAL",
                      "MeanDelta0Mixin",
                      "MultDeltaMixin",
                      "MultDelta0Mixin",
                      "NuDeltaMixin",
                      "SpecADeltaMixin",
                      "SpecADelta0Mixin",
                      "SpecHasLevelMixin",
                      "SpecOmegaDeltaMaxMixin"))

setClass("WithTrendMixin",
         contains = c("VIRTUAL",
             "aWithTrendMixin",
             "ADeltaMixin",
             "ADelta0Mixin",
             "CWithTrendMixin",
             "DeltaDLMMixin",
             "GWithTrendMixin",
             "HasLevelMixin",
             "MWithTrendMixin",
             "M0WithTrendMixin",
             "MeanDelta0Mixin",
             "NuDeltaMixin",
             "OmegaDeltaMixin",
             "OmegaDeltaMaxMixin",
             "RWithTrendMixin",
             "UCDCMixin",
             "URDRMixin",
             "WSqrtMixin"))

setClass("ZeroMixin",
         contains = "VIRTUAL")
