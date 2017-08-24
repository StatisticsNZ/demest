
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



## AccountMixin ##################################################################

## NO_TESTS
setClass("DiffPropMixin",
         slots = c(diffProp = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             diffProp <- object@diffProp
             ## 'diffProp' has length 1
             if (!identical(length(diffProp), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "diffProp", 1L))
             ## if 'diffProp' not missing, is not equal to 0
             if (!is.na(diffProp) && (value == 0L))
                 return(gettextf("'%s' equals %d",
                                 name, 0L))
             TRUE
         })

## NO_TESTS
setClass("ExposureMixin",
         slots = c(exposure = "Exposure"),
         contains = "VIRTUAL",
         validity = function(object) {
             exposure <- object@exposure
             population <- object@exposure
             ## 'exposure' object equals result of calling 'exposure' function
             ## on 'population' with triangles = TRUE
             exposure.calc <- dembase::exposure(population,
                                                triangles = TRUE)
             if (!isTRUE(all.equal(exposure, exposure.calc)))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "exposure", "population"))
             TRUE
         })

setClass("GeneratedNewProposalMixin",
         slots = c(generatedNewProposal = "LogicalFlag"))

setClass("HasAgeMixin",
         slots = c(hasAge = "LogicalFlag"))

## NO_TESTS
setClass("ICellMixin",
         slots = c(iCell = "integer",
                   iCellOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             for (name in c("iCell", "iCellOther")) {
                 value <- slot(object, name)
                 ## 'iCell', 'iCellOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iCell', 'iCellOther' not missing, they are greater than or equal to 1
                 if (!is.na(value) && (value < 1L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
             }
             TRUE
         })

## NO_TESTS
setClass("ICompMixin",
         slots = c(iComp = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iComp <- object@iComp
             components <- object@account@components
             ## 'iComp' has  length 1
             if (!identical(length(iComp), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "iComp", 1L))
             ## 'iComp' is not missing
             if (is.na(iComp))
                 return(gettextf("'%s' is missing",
                                 iComp))
             ## if 'iComp' is not 0, it is the index of a component
             if (iComp != 0L) {
                 if (!(iComp %in% seq_along(components)))
                     return(gettextf("'%s' does not index a component",
                                     "iComp"))
             }
             TRUE
         })

## NO_TESTS
## the index of the first cell in 'exposure' that
## will change if the cell being updated is changed
setClass("IExpFirstMixin",
         slots = c(iExpFirst = "integer",
                   iExpFirstOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iExpFirst <- object@iExpFirst
             iExpFirstOther <- object@iExpFirstOther
             for (name in c("iExpFirst", "iExpFirstOther")) {
                 value <- slot(object, name)
                 ## 'iExpFirst', 'iExpFirstOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iExpFirst', 'iExpFirstOther' not missing, they are greater than or equal to 1L
                 if (!is.na(value) && (value < 1L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
                 if (!is.na(value)) {
                     ## if 'iExpFirst', 'iExpFirstOther' not missing, they are less than or
                     ## equal to length of 'exposure'
                     exposure <- object@exposure
                     n.exposure <- length(exposure)
                     if (value > n.exposure)
                         return(gettextf("'%s' is greater than the length of '%s'",
                                         name, "exposure"))
                     ## if 'iExpFirst', 'iExpFirstOther' not missing,
                     ## iExposure and iExposureOther not missing
                     name.i.exp <- if (name == "iExpFirstFirst") "iExposure" else "iExposureOther"
                     val.i.exp <- slot(object, name.i.exp)
                     if (value <= val.i.exp)
                         return(gettextf("'%s' less than or equal to '%s'",
                                         name, name.i.exp))
                 }
             }
             ## if 'iExpFirst' and 'iExpFirstOther' not missing, they have different values
             if (!is.na(iExpFirst) && !is.na(iExpFirstOther) && (iExpFirst == iExpFirstOther))
                 return(gettextf("'%s' equals '%s'",
                                 "iExpFirst", "iExpFirstOther"))
             TRUE
         })

## NO_TESTS
## The index of the cell in 'exposure' that appears in the likelihood for
## the cell being updated.  iExposure is 0L if the model for the cell being
## updated does not include exposure.
setClass("IExposureMixin",
         slots = c(iExposure = "integer",
                   iExposureOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iExposure <- object@iExposure
             iExposureOther <- object@iExposureOther
             for (name in c("iExposure", "iExposureOther")) {
                 value <- slot(object, name)
                 ## 'iExposure', 'iExposureOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iExposure', 'iExposureOther' not missing, they are greater than or equal to 1L
                 if (!is.na(value) && (value < 1L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
                 ## if 'iExposure', 'iExposureOther' not missing, they are less than or
                 ## equal to length of 'exposure'
                 if (!is.na(value)) {
                     exposure <- object@exposure
                     n.exposure <- length(exposure)
                     if (value > n.exposure)
                         return(gettextf("'%s' is greater than the length of '%s'",
                                         name, "exposure"))
                 }
             }
             ## if 'iExposure' and 'iExposureOther' not missing, they have different values
             if (!is.na(iExposure) && !is.na(iExposureOther) && (iExposure == iExposureOther))
                 return(gettextf("'%s' equals '%s'",
                                 "iExposure", "iExposureOther"))
             TRUE
         })

## NO_TESTS
setClass("IPopnNextMixin",
         slots = c(iPopnNext = "integer",
               iPopnNextOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iPopnNext <- object@iPopnNext
             iPopnNextOther <- object@iPopnNextOther
             population <- object@population
             n.population <- length(population)
             for (name in c("iPopnNext", "iPopnNextOther")) {
                 value <- slot(object, name)
                 ## 'iPopnNext', 'iPopnNextOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iPopnNext', 'iPopnNextOther' not missing, they are greater than or equal to 0L
                 if (!is.na(value) && (value < 0L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
                 ## if 'iPopnNext', 'iPopnNextOther' not missing, they are less than or
                 ## equal to length of 'population'
                 if (!is.na(value) && (value > n.population))
                     return(gettextf("'%s' is greater than the length of '%s'",
                                     name, "population"))
             }
             ## if 'iPopnNext' and 'iPopnNextOther' not missing, they have different values
             if (!is.na(iPopnNext) && !is.na(iPopnNextOther) && (iPopnNext == iPopnNextOther))
                 return(gettextf("'%s' equals '%s'",
                                 "iPopnNext", "iPopnNextOther"))
             TRUE
         })

## NO_TESTS
setClass("IsIncrementMixin",
         slots = c(isIncrement = "logical"),
         contains = "VIRTUAL",
         validity = function(object) {
             isIncrement <- object@isIncrement
             components <- object@account@components
             componentNames <- object@account@componentNames
             ## 'isIncrement' has no missing values
             if (any(is.na(isIncrement)))
                 return(gettextf("'%s' has missing values",
                                 "isIncrement"))
             ## 'isIncrement' has same length as 'components'
             if (!identical(length(isIncrement), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "isIncrement", "components"))
             TRUE
         })

## NO_TESTS
setClass("MappingsToExpMixin",
         slots = c(mappingsToExp = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             mappingsToExp <- object@mappingsToExp
             components <- object@components
             ## all elements have class "MappingToExp"
             if (!all(sapply(mappingsToExp, methods::is, "MappingToExp")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "mappingsToExp", "MappingToExp"))
             ## has same length as 'components'
             if (!identical(length(mappingsToExp), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mappingsToExp", "components"))
             ## element has class "MappingBirthsToExp" iff corresponding
             ## element of 'components' has class "Births",
             is.mapping.births <- sapply(components, methods::is, "MappingBirthsToExp")
             is.births <- sapply(components, methods::is, "Births")
             if (!identical(is.mapping.births, is.births))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToExp", "MappingBirthsToExp", "components", "Births"))             
             ## element has class "MappingOrigDestToExp" iff corresponding
             ## element of 'components' has class "HasOrigDest",
             is.mapping.orig.dest <- sapply(components, methods::is, "MappingOrigDestToExp")
             is.has.orig.dest <- sapply(components, methods::is, "HasOrigDest")
             if (!identical(is.mapping.orig.dest, is.has.orig.dest))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToExp", "MappingOrigDestToExp", "components", "HasOrigDest"))             
             TRUE
         })

## NO_TESTS
setClass("MappingsFromExpMixin",
         slots = c(mappingsFromExp = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             mappingsFromExp <- object@mappingsFromExp
             components <- object@components
             ## all elements have class "MappingFromExp"
             if (!all(sapply(mappingsFromExp, methods::is, "MappingFromExp")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "mappingsFromExp", "MappingFromExp"))
             ## has same length as 'components'
             if (!identical(length(mappingsFromExp), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mappingsFromExp", "components"))
             ## element has class "MappingExpToBirths" iff corresponding
             ## element of 'components' has class "Births",
             is.mapping.births <- sapply(components, methods::is, "MappingExpToBirths")
             is.births <- sapply(components, methods::is, "Births")
             if (!identical(is.mapping.births, is.births))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsFromExp", "MappingExpToBirths", "components", "Births"))             
             TRUE
         })

## NO_TESTS
setClass("MappingsToPopnMixin",
         slots = c(mappingsToPopn = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             mappingsToPopn <- object@mappingsToPopn
             components <- object@components
             ## all elements have class "MappingToPopn"
             if (!all(sapply(mappingsToPopn, methods::is, "MappingToPopn")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "mappingsToPopn", "MappingToPopn"))
             ## has same length as 'components'
             if (!identical(length(mappingsToPopn), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mappingsToPopn", "components"))
             ## element has class "MappingOrigDestToPopn" iff corresponding
             ## element of 'components' has class "HasOrigDest",
             is.mapping.orig.dest <- sapply(components, methods::is, "MappingOrigDestToPopn")
             is.has.orig.dest <- sapply(components, methods::is, "HasOrigDest")
             if (!identical(is.mapping.orig.dest, is.has.orig.dest))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToPopn", "MappingOrigDestToPopn", "components", "HasOrigDest"))             
             TRUE
         })

## NO_TESTS
setClass("NCellAccountMixin",
         slots = c(nCellAccount = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             nCellAccount <- object@nCellAccount
             population <- object@account@population
             components <- object@account@components
             ## 'nCellAccount' has length 1
             if (!identical(length(probPopn), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "nCellAccount", 1L))
             ## 'nCellAccount' is not missing
             if (is.na(nCellAccount))
                 return(gettextf("'%s' is missing",
                                 "nCellAccount"))
             ## 'nCellAccount' equals sum of length of population plus components
             length.popn <- length(population)
             length.components <- sum(sapply(components, length))
             if (!identical(nCellAccount, length.popn + length.components))
                 return(gettextf("'%s' does not equal length of population plus lengths of components",
                                 "nCellAccount"))
             TRUE
         })

## NO_TESTS
setClass("ProbPopnMixin",
         slots = c(probPopn = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             probPopn <- object@probPopn
             ## 'probPopn' has length 1
             if (!identical(length(probPopn), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "probPopn", 1L))
             ## 'probPopn' is not missing
             if (is.na(probPopn))
                 return(gettextf("'%s' is missing",
                                 "probPopn"))
             ## 'probPopn' is double
             if (!is.double(probPopn))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "probPopn", "double"))
             ## 'probPopn' is between 0 and 1
             if ((probPopn < 0) || (probPopn > 1))
                 return(gettextf("'%s' is not between %d and %d",
                                 "probPopn", 0L, 1L))
             TRUE
         })

setClass("AccountMixin",
         contains = c("DiffPropMixin",
                      "ExposureMixin",
                      "GeneratedNewProposalMixin",
                      "HasAgeMixin",
                      "ICellMixin",
                      "ICompMixin",
                      "IExposureMixin",
                      "IPopnNextMixin",
                      "IsIncrementMixin",
                      "MappingsFromExpMixin",
                      "MappingsToExpMixin",
                      "MappingsToPopnMixin",
                      "NCellAccountMixin",
                      "ProbPopnMixin"))


## MovementsMixin ######################################################################

## NO_TESTS
setClass("AccessionMixin",
         slot = c(accession = "Accession"),
         contains = "VIRTUAL",
         validity = function(object) {
             accession <- object@accession
             population <- object@population
             .Data.acc <- accession@.Data
             names.acc <- names(accession)
             names.popn <- names(population)
             dimtypes.acc <- dimtypes(accession, use.names = FALSE)
             dimtypes.popn <- dimtypes(population, use.names = FALSE)
             DimScales.acc <- DimScales(accession, use.names = FALSE)
             DimScales.popn <- DimScales(population, use.names = FALSE)
             ## 'accession' has no missing values
             if (any(is.na(.Data.acc)))
                 return(gettextf("'%s' has missing values",
                                 "accession"))
             ## 'accession' has same names as 'population'
             if (!identical(names.acc, names.popn))
                 return(gettextf("'%s' and '%s' have different names",
                                 "accession", "population"))
             ## 'accession' has same dimtypes as 'population'
             if (!identical(dimtypes.acc, dimtypes.popn))
                 return(gettextf("'%s' and '%s' have different dimtypes",
                                 "accession", "population"))
             ## 'accession' has same DimScales as 'population',
             ## except for "time" dimension, where have same
             ## dimvalues (except for last),             
             ## and "age" dimension, where accession does not start at age 0
             ## (otherwise have different mappings to accession,
             ## depending on whether account includes births)
             for (i in seq_along(DimScales.acc)) {
                 DS.acc <- DimScales.acc[[i]]
                 DS.popn <- DimScales.popn[[i]]
                 if (dimtypes.acc[i] == "time") {
                     dv.acc <- DS.acc@dimvalues
                     dv.popn <- DS.popn@dimvalues
                     valid <- isTRUE(all.equal(dv.acc, dv.popn))
                 }
                 else if (dimtypes.acc[i] == "age") {
                     dv.acc <- DS.acc@dimvalues
                     dv.popn <- DS.popn@dimvalues
                     n.dv.popn <- length(dv.popn)
                     valid <- isTRUE(all.equal(dv.acc, dv.popn[c(-1L -n)]))
                 }
                 else
                     valid <- isTRUE(all.equal(DS.acc, DS.popn))
                 if (!valid)
                     return(gettextf("'%s' and '%s' have inconsistent %s for dimension \"%s\" with %s \"%s\"",
                                     "accession", "population", "dimscales", names.acc[i], "dimtype", dimtype[i]))
             }
             TRUE
         })

## NO_TESTS
setClass("IAccNextMixin",
         slots = c(iAccNext = "integer",
                   iAccNextOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iAccNext <- object@iAccNext
             iAccNextOther <- object@iAccNextOther
             for (name in c("iAccNext", "iAccNextOther")) {
                 value <- slot(object, name)
                 ## 'iAccNext', 'iAccNextOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iAccNext', 'iAccNextOther' not missing, they are greater than or equal to 0L
                 if (!is.na(value) && (value < 0L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
                 ## if 'iAccNext', 'iAccNextOther' not missing, they are less than or
                 ## equal to length of 'accession'
                 if (!is.na(value)) {
                     accession <- object@accession
                     n.accession <- length(accession)
                     return(gettextf("'%s' is greater than the length of '%s'",
                                     name, "accession"))
                 }
             }
             ## if 'iAccNext' and 'iAccNextOther' not missing, they have different values
             if (!is.na(iAccNext) && !is.na(iAccNextOther) && (iAccNext == iAccNextOther))
                 return(gettextf("'%s' equals '%s'",
                                 "iAccNext", "iAccNextOther"))
             TRUE
         })

## NO_TESTS
setClass("IsLowerTriangleMixin",
         slots = c(isLowerTriangle = "logical"),
         contains = "VIRTUAL",
         validity = function(object) {
             isLowerTriangle <- object@isLowerTriangle
             ## 'isLowerTriangle' has length 1
             if (!identical(length(isLowerTriangle), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "lowerTriangle", 1L))
             TRUE
         })

## NO_TESTS
setClass("MappingsToAccMixin",
         slots = c(mappingsToAcc = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             mappingsToAcc <- object@mappingsToAcc
             components <- object@components
             ## all elements have class "MappingToAcc"
             if (!all(sapply(mappingsToAcc, methods::is, "MappingToAcc")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "mappingsToAcc", "MappingToAcc"))
             ## has same length as 'components'
             if (!identical(length(mappingsToAcc), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mappingsToAcc", "components"))
             ## element has class "MappingOrigDestToAcc" iff corresponding
             ## element of 'components' has class "HasOrigDest",
             is.mapping.orig.dest <- sapply(components, methods::is, "MappingOrigDestToAcc")
             is.has.orig.dest <- sapply(components, methods::is, "HasOrigDest")
             if (!identical(is.mapping.orig.dest, is.has.orig.dest))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToAcc", "MappingOrigDestToAcc", "components", "HasOrigDest"))             
             TRUE
         })

setClass("AccountMovementsMixin",
         contains = c("AccessionMixin",
                      "IAccNextMixin",
                      "IsLowerTriangleMixin",
                      "MappingsToAccMixin"))


## SystemMixin #################################################################

setClass("SystemModelsMixin",
         slots = c(systemModels = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             systemModels <- object@systemModels
             account <- object@account
             population <- account@population
             components <- account@components
             namesComponents <- account@namesComponents
             ## all elements of "system" have class "Model"
             if (!all(sapply(systemModels, methods::is, "Model")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "systemModels", "model"))
             ## 'systemModels' has one more element than 'components'
             ## (extra is model for 'population')
             if (!identical(length(systemModels), length(components) + 1L))
                 return(gettextf("'%s' and '%s' have inconsistent lengths",
                                 "systemModels", "components"))
             ## first system model (for 'population') is a Poisson model
             if (!methods::is(systemModels[[1L]], "Poisson"))
                 return(gettextf("system model for '%s' has class \"%s\"",
                                 "population", class(systemModels[[1L]])))
             ## 'theta' for first system model has same length as 'population'
             if (!identical(length(systemModels[[1L]]@theta), length(population)))
                 return(gettextf("'%s' for system model for '%s' has wrong length",
                                 "theta", "population"))
             ## 'theta' for system model has same length as corresponding element of 'components'
             for (i in seq_along(components)) {
                 model <- systemModels[[i + 1L]]
                 component <- components[[i]]
                 name <- namesComponents[i]
                 if (!identical(length(model@theta), length(component)))
                     return(gettextf("'%s' for system model for '%s' has wrong length",
                                     "theta", name))                 
             }
             TRUE
         })

setClass("ModelUsesExposureMixin",
         slots = c(modelUsesExposure = "logical")
         contains = "VIRTUAL",
         validity = function(object) {
             modelUsesExposure <- object@modelUsesExposure
             systemModels <- object@systemModels
             componentNames <- object@account@componentNames
             ## 'modelUsesExposure' has no missing values
             if (any(is.na(modelUsesExposure)))
                 return(gettextf("'%s' has missing values",
                                 "modelUsesExposure"))
             ## 'modelUsesExposure' has same length as 'systemModels'
             if (!identical(length(modelUsesExposure), length(systemModels)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "modelUsesExposure", "systemModels"))
             ## first model (for population) does not use exposure
             if (modelUsesExposure[[1L]])
                 return(gettextf("system model for '%s' uses exposure",
                                 "population"))
             ## if a model has class "Normal", the corresponding
             ## element of modelUsesExposure is FALSE
             for (i in seq_along(componentNames)) {
                 model <- systemModels[[i + 1L]]
                 if (methods::is(model, "Normal") && modelUsesExposure[i + 1L])
                     return(gettextf("system model for '%s' uses exposure but has class \"%s\"",
                                     name, "Normal"))
             }
             TRUE
         })

setClass("SystemMixin",
         contains = c("VIRTUAL",
                      "SystemModelsMixin",
                      "ModelUsesExposureMixin"))

setClass("SystemMovementsMixin",
         contains = "VIRTUAL",
         validity = function(object) {
             systemModels <- object@systemModels
             account <- object@account
             components <- account@components
             namesComponents <- account@namesComponents
             ## if a component does not have class "Net",
             ## then the system model has class "Poisson"
             for (i in seq_along(components)) {
                 model <- systemModels[[i + 1L]]
                 component <- components[[i]]
                 name <- namesComponents[i]
                 if (!methods::is(component, "Net") && !methods::is(model, "Poisson"))
                     return(gettextf("'%s' has class \"%s\" but its system model does not have class \"%s\"",
                                     name, class(component), "Poisson"))
             }
             ## a system model has class "Normal" iff the corresponding component has class "Net"
             is.net <- sapply(components, methods::is, "Net")
             is.normal <- sapply(systemModels[-1L], methods::is, "Normal")
             if (any(is.net & !is.normal))                 
                 return(gettextf("component has class \"%s\" but corresponding system model does not have class \"%s\"",
                                 "Net", "Normal"))
             if (any(is.normal & !is.net))                 
                 return(gettextf("system model has class \"%s\" but correspondening component does not have class \"%s\"",
                                 "Normal", "Net"))
             TRUE
         })


## Observation ###################################################################

## HAS_TESTS
setClass("ObservationModelsMixin",
         slots = c(observationModels = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             observationModels <- object@observationModels
             ## all elements of 'observationModels' have class "Model"
             if (!all(sapply(observationModels, is, "Model")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "observationModels", "Model"))
             ## all elements of 'observationModels' have class "UseExposure"
             if (!all(sapply(observationModels, is, "UseExposure")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "observationModels", "UseExposure"))
             ## 'observationModels' does not have names
             if (!is.null(names(observationModels)))
                 return(gettextf("'%s' has names",
                                 "observationModels"))
             TRUE
         })

## HAS_TESTS
setClass("DatasetsMixin",
         slots = c(datasets = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             datasets <- object@datasets
             observationModels <- object@observationModels
             hasNegative <- function(x) any(x[!is.na(x)] < 0)
             ## all elements of 'datasets' have class "Counts"
             if (!all(sapply(datasets, is, "Counts")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "datasets", "Counts"))
             ## all elements of 'datasets' have type "integer"
             if (!all(sapply(datasets, is.integer)))
                 return(gettextf("'%s' has elements not of type \"%s\"",
                                 "datasets", "integer"))
             ## all elements of "datasets' are non-negative
             if (any(sapply(datasets, hasNegative)))
                 return(gettextf("'%s' has elements with negative values",
                                 "datasets"))
             ## 'datasets' does not have names
             if (!is.null(names(datasets)))
                 return(gettextf("'%s' has names",
                                 "datasets"))
             ## 'observationModels' and 'datasets' have same length
             if (!identical(length(observationModels), length(datasets)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "observationModels", "datasets"))
             TRUE
         })

## HAS_TESTS
setClass("NamesDatasetsMixin",
         slots = c(namesDatasets = "character"),
         contains = "VIRTUAL",
         validity = function(object) {
             namesDatasets <- object@namesDatasets
             observationModels <- object@observationModels
             hasMissing <- function(x) any(is.na(x))
             ## 'namesDatasets' has no missing values
             if (hasMissing(namesDatasets))
                 return(gettextf("'%s' has missing values",
                                 "namesDatasets"))
             ## 'namesDatasets' has no blanks
             if (!all(nzchar(namesDatasets)))
                 return(gettextf("'%s' has blanks",
                                 "namesDatasets"))
             ## 'namesDatasets' has no duplicates
             if (any(duplicated(namesDatasets)))
                 return(gettextf("'%s' has duplicates",
                                 "namesDatasets"))
             ## 'observationModels' and 'namesDatasets' have same length
             if (!identical(length(observationModels), length(namesDatasets)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "observationModels", "namesDatasets"))
             TRUE
         })

## HAS_TESTS
setClass("TransformsMixin",
         slots = c(transforms = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             transforms <- object@transforms
             observationModels <- object@observationModels
             datasets <- object@datasets
             namesDatasets <- object@namesDatasets
             dimBefore <- function(x) x@dimBefore
             ## all elements of 'transforms' have class "CollapseTransformExtra"
             if (!all(sapply(transforms, is, "CollapseTransformExtra")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "transforms", "CollapseTransformExtra"))
             ## all elements of 'transforms' have same 'dimBefore'
             if (length(transforms) > 1L) {
                 dim.before.first <- transforms[[1L]]@dimBefore
                 dim.before.other <- lapply(transforms[-1L], dimBefore)
                 if (!all(mapply(identical, x = list(dim.before.first), y = dim.before.other)))
                     return(gettextf("elements of '%s' do not all have same '%s'",
                                     "transforms", "dimBefore"))
             }
             ## 'transforms' does not have names
             if (!is.null(names(transforms)))
                 return(gettextf("'%s' has names",
                                 "transforms"))
             ## 'observationModels' and 'transforms' have same length
             if (!identical(length(observationModels), length(transforms)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "observationModels", "transforms"))
             ## 'transforms' have 'dimAfter' consistent with datasets
             for (i in seq_along(datasets)) {
                 if (!identical(dim(datasets[[i]]), transforms[[i]]@dimAfter))
                     return(gettextf("'%s' and '%s' for \"%s\" inconsistent",
                                     "dataset", "transform", namesDatasets[i]))
             }
             TRUE
         })


## HAS_TESTS
## 'seriesIndices' is a mapping from observationModels models to
## demographic series.  'seriesIndices' equals 0L in places
## where the observationModels model maps back to population
setClass("SeriesIndicesMixin",
         slots = c(seriesIndices = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             seriesIndices <- object@seriesIndices
             observationModels <- object@observationModels
             components <- object@account@components
             ## 'seriesIndices' does not have missing values
             if (any(is.na(seriesIndices)))
                 return(gettextf("'%s' has missing values",
                                 "seriesIndices"))
             ## 'seriesIndices' has same length as 'observationModels'
             if (!identical(length(observationModels), length(seriesIndices)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "observationModels", "seriesIndices"))
             ## elements of 'seriesIndices' inside valid range
             valid.range <- c(0L, seq_along(components))
             if (!all(indicesSeries %in% valid.range))
                 return(gettextf("'%s' outside valid range",
                                 "indicesSeries"))
             TRUE
         })

setClass("ObservationMixin",
         contains = c("VIRTUAL",
                      "ObservationModelsMixin",
                      "DatasetsMixin",
                      "NamesDatasetsMixin",
                      "TransformsMixin",
                      "SeriesIndicesMixin"))


## CombinedAccount #############################################################

setClass("CombinedAccount",
         contains = c("VIRTUAL",
                      "AccountMixin",
                      "SystemMixin",
                      "ObservationMixin"))
         validity = function(object) {
             system <- object@system
             observationModels <- object@observationModels
             ## 'observationModels' has length 0 iff 'system' consists
             ## entirely of models with class "SingleIter"
             obs.length.0 <- identical(length(observationModels), 0L)
             sys.all.single <- all(sapply(system, methods::is, "SingleIter"))
             if (!identical(obs.length.0, sys.all.single))
                 return(gettextf("'%s' should have length %d iff '%s' consists entirely of models with class \"%s\"",
                                 "observationModels", 0L, "system", "SingleIter"))
             TRUE
         })
                               
setClass("CombinedAccountMovements",
         contains = c("CombinedAccount",
                      "SystemMovementsMixin",
                      "AccountMovementsMixin"))
