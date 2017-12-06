

setClass("FakePrior",
         contains = c("VIRTUAL",
                      "IsSaturatedMixin",
                      "JMixin"))

setClass("FakeExch",
         contains = c("VIRTUAL",
                      "FakePrior",
                      "ComponentFlags"))

setClass("FakeDLM",
         contains = c("VIRTUAL",
                      "FakePrior",
                      "AAlphaMixin",
                      "AlphaDLMMixin",
                      "ComponentFlags",
                      "IAlongMixin",
                      "IteratorStateMixin",
                      "IteratorVMixin",
                      "KLMixin",
                      "NuAlphaMixin",
                      "OmegaAlphaMixin",
                      "OmegaAlphaMaxMixin",
                      "PhiMixin",
                      "PhiMinMaxMixin",
                      "Shape1Shape2PhiMixin"))

setClass("FakeWithTrendMixin",
         contains = c("VIRTUAL",
                      "ADeltaMixin",
                      "ADelta0Mixin",
                      "DeltaDLMMixin",
                      "HasLevelMixin",
                      "MeanDelta0Mixin",
                      "NuDeltaMixin",
                      "OmegaDeltaMixin",
                      "OmegaDeltaMaxMixin"))         

setClass("FakeExchFixed",
         contains = c("FakePrior",
                      "MeanMixin",
                      "TauMixin"))

setClass("FakeExchNormZero",
         prototype = prototype(hasAlphaDLM = methods::new("LogicalFlag", FALSE),
                               hasAlphaICAR = methods::new("LogicalFlag", FALSE),
                               hasAlphaMix = methods:::new("LogicalFlag", FALSE),
                               hasCovariates = methods::new("LogicalFlag", FALSE),
                               hasSeason = methods::new("LogicalFlag", FALSE),
                               isRobust = methods::new("LogicalFlag", FALSE)),
         contains = c("FakeExch",
                      "NormMixin",
                      "ZeroMixin"))

setClass("FakeDLMNoTrendNormZeroNoSeason",
         prototype = prototype(hasAlphaDLM = methods::new("LogicalFlag", TRUE),
                               hasAlphaICAR = methods::new("LogicalFlag", FALSE),
                               hasAlphaMix = methods:::new("LogicalFlag", FALSE),
                               hasCovariates = methods::new("LogicalFlag", FALSE),
                               hasSeason = methods::new("LogicalFlag", FALSE),
                               isRobust = methods::new("LogicalFlag", FALSE)),
         contains = c("FakeDLM",
                      "NormMixin",
                      "ZeroMixin"))

setClass("FakeDLMWithTrendNormZeroNoSeason",
         prototype = prototype(hasAlphaDLM = methods::new("LogicalFlag", TRUE),
                               hasAlphaICAR = methods::new("LogicalFlag", FALSE),
                               hasAlphaMix = methods:::new("LogicalFlag", FALSE),
                               hasCovariates = methods::new("LogicalFlag", FALSE),
                               hasSeason = methods::new("LogicalFlag", FALSE),
                               isRobust = methods::new("LogicalFlag", FALSE)),
         contains = c("FakeDLM",
                      "NormMixin",
                      "FakeWithTrendMixin",
                      "ZeroMixin"))

setClass("FakeData",
         slots = c(call = "call"),
         contains = "VIRTUAL")
                      

## HAS_TESTS
setClass("FakeModel",
         slots = c(model = "list",
                   y = "DemographicArray"),
         contains = "FakeData",
         validity = function(object) {
             y <- object@y
             model <- object@model
             model.unlisted <- unlist(model)
             ## 'y' does not have iteration or quantile dimensions
             dimtypes <- dembase::dimtypes(y, use.names = FALSE)
             for (dimtype in c("iteration", "quantile"))
                 if (dimtype %in% dimtypes)
                     return(gettextf("'%s' has dimension with dimtype \"%s\"",
                                     "y", dimtype))
             ## all elements of 'model' have type "numeric"
             if (!all(sapply(model.unlisted, is.numeric)))
                 return(gettextf("'%s' has elements not of type \"%s\"",
                                 "model", "numeric"))
             TRUE
         })

## HAS_TESTS
setClass("FakeModelExposure",
         slots = c(model = "list", ## list all slots so that they show up correctly in listContents
             y = "DemographicArray",
             exposure = "Counts"),
         contains = "FakeData",
         validity = function(object) {
             y <- object@y
             exposure <- object@exposure
             for (name in c("y", "exposure")) {
                 value <- methods::slot(object, name)
                 ## 'y', 'exposure' are "Counts
                 if (!methods::is(value, "Counts"))
                     return(gettextf("'%s' has class \"%s\"",
                                     name, class(value)))
             }
             ## 'exposure' has same metadata as 'y'
             if (!identical(exposure@metadata, y@metadata))
                 return(gettextf("'%s' and '%s' have different metadata",
                                 "exposure", "y"))
             TRUE
         })
