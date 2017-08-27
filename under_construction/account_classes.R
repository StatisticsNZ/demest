
## SingleIter ##################################################################

setClass("SingleIter",
         contains = c("VIRTUAL",
                      "Model",
                      "Theta"))

setClass("SingleIterNotUseExp",
         contains = c("SingleIter",
                      "NotUseExposure"),
         prototype = prototype(slotsToExtract = character(),
                               iMethodModel = 999L))


setClass("SingleIterUseExp",
         contains = c("SingleIter",
                      "UseExposure"),
         prototype = prototype(slotsToExtract = character(),
                               iMethodModel = 999L))



## CombinedAccount #############################################################
