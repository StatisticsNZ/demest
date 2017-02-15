
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
             "RNoTrendMixin"))

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
             "MultEtaCoefMixin",
             "NuEtaCoefMixin",
             "SpecAEtaCoefMixin",
             "SpecAEtaInterceptMixin"))

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
                      "SpecOmegaDeltaMaxMixin"))

setClass("WithTrendMixin",
         contains = c("VIRTUAL",
             "aWithTrendMixin",
             "ADeltaMixin",
             "ADelta0Mixin",
             "CWithTrendMixin",
             "DeltaDLMMixin",
             "GWithTrendMixin",
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
