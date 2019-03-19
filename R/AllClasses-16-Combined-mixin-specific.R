
## NO_TESTS
setClass("AccessionMixin",
         slot = c(accession = "Accession"),
         contains = "VIRTUAL",
         validity = function(object) {
             hasAge <- object@hasAge@.Data
             accession <- object@accession
             population <- object@account@population
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
             ## except for "time" dimension, where have same dimvalues,
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
                     valid <- isTRUE(all.equal(dv.acc, dv.popn[-c(1L, n.dv.popn)]))
                 }
                 else
                     valid <- isTRUE(all.equal(DS.acc, DS.popn))
                 if (!valid)
                     return(gettextf("'%s' and '%s' have inconsistent %s for dimension \"%s\" with %s \"%s\"",
                                     "accession", "population", "dimscales", names.acc[i], "dimtype", dimtypes.acc[i]))
             }
             TRUE
         })


## NO_TESTS
setClass("AgeTimeStepMixin",
         slot = c(ageTimeStep = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             ageTimeStep <- object@ageTimeStep
             population <- object@account@population
             ## identical to calling function 'ageTimeStep' on 'population'
             if (!identical(dembase::ageTimeStep(population), ageTimeStep))
                 return(gettextf("'%s' not equal to result of calling function '%s' on '%s'",
                                 "ageTimeStep", "ageTimeStep", "population"))
             TRUE
         })

## NO_TESTS
setClass("ProbAccountMixin",
         slots = c(cumProbComp = "numeric",
                   probPopn = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             cumProbComp <- object@cumProbComp
             components <- object@account@components
             probPopn <- object@probPopn
             ## 'cumProbComp' and 'components' have same length
             if (!identical(length(cumProbComp), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "cumProbComp", "components"))
             ## 'cumProbComp' has no missing values
             if (any(is.na(cumProbComp)))
                 return(gettextf("'%s' has missing values",
                                 "cumProbComp"))
             ## 'cumProbComp' is double
             if (!is.double(cumProbComp))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "cumProbComp", "double"))
             ## 'cumProbComp' is between 0 and 1
             if (any((cumProbComp < 0) || (cumProbComp > 1)))
                 return(gettextf("'%s' has values between %d and %d",
                                 "cumProbComp", 0L, 1L))
             ## 'cumProbComp' strictly increasing
             if (any(diff(cumProbComp) <= 0))
                 return(gettextf("'%s' not strictly increasing",
                                 "cumProbComp"))
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

## NO_TESTS
setClass("DatasetsMixin",
         slots = c(datasets = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             datasets <- object@datasets
             dataModels <- object@dataModels
             ## all elements of 'datasets' have class "Counts"
             if (!all(sapply(datasets, is, "Counts")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "datasets", "Counts"))
             ## all elements of 'datasets' have type "integer"
             if (!all(sapply(datasets, is.integer)))
                 return(gettextf("'%s' has elements not of type \"%s\"",
                                 "datasets", "integer"))
             ## 'datasets' does not have names
             if (!is.null(names(datasets)))
                 return(gettextf("'%s' has names",
                                 "datasets"))
             ## 'dataModels' and 'datasets' have same length
             if (!identical(length(dataModels), length(datasets)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "dataModels", "datasets"))
             TRUE
         })

## HAS_TESTS
setClass("DataModelsUseAgMixin",
         slots = c(dataModelsUseAg = "LogicalFlag"),
         contains = "VIRTUAL",
         validity = function(object) {
             dataModelsUseAg <- object@dataModelsUseAg@.Data
             dataModels <- object@dataModels
             mod.uses.ag <- sapply(dataModels, methods::is, "Aggregate")
             if (dataModelsUseAg && !any(mod.uses.ag))
                 return(gettextf("'%s' is %s but no data models use aggregates",
                                 "dataModelsUseAg", TRUE))
             if (!dataModelsUseAg && any(mod.uses.ag))
                 return(gettextf("'%s' is %s but data models use aggregates",
                                 "dataModelsUseAg", FALSE))
             TRUE
         })

## NO_TESTS
setClass("DescriptionsMixin",
         slots = c(descriptions = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             descriptions <- object@descriptions
             components <- object@account@components
             ## 'descriptions' has length equal to length of 'components' plus 1
             if (!identical(length(descriptions), length(components) + 1L))
                 return(gettextf("lengths of '%s' and '%s' inconsistent",
                                 "descriptions", "components"))
             ## first element has class "DescriptionPopn"
             if (!methods::is(descriptions[[1L]], "DescriptionPopn"))
                 return(gettextf("first element of '%s' does not have class \"%s\"",
                                 "descriptions", "DescriptionPopn"))
             ## remaining elements has class "DescriptionComp"
             if (!all(sapply(descriptions[-1L], methods::is, "DescriptionComp")))
                 return(gettextf("first element of '%s' does not have class \"%s\"",
                                 "descriptions", "DescriptionComp"))
             ## element has class "DescriptionPool" iff corresponding
             ## element of 'components' has class InternalMovementsPool",
             is.desc.pool <- sapply(descriptions[-1], methods::is, "DescriptionPool")
             is.pool <- sapply(components, methods::is, "InternalMovementsPool")
             if (!identical(is.desc.pool, is.pool))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "descriptions", "DescriptionPool", "components", "InternalMovementsPool"))             
             TRUE
         })

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
             if (!is.na(diffProp) && (diffProp == 0L))
                 return(gettextf("'%s' equals %d",
                                 name, 0L))
             TRUE
         })

## NO_TESTS
setClass("ExpectedExposureMixin",
         slots = c(expectedExposure = "Exposure"),
         contains = "VIRTUAL",
         validity = function(object) {
             expectedExposure <- object@expectedExposure
             theta.popn <- object@systemModels[[1L]]@theta
             population <- object@account@population
             hasAge <- object@hasAge@.Data
             ## 'expectedExposure' object equals result of
             ## calling 'exposure' function on 'theta.popn'
             metadata.theta.popn <- population@metadata
             .Data.theta.popn <- array(theta.popn,
                                       dim = dim(population),
                                       dimnames = dimnames(population))
             theta.popn <- methods::new("Counts",
                               .Data = .Data.theta.popn,
                               metadata = metadata.theta.popn)
             exposure.calc <- dembase::exposure(theta.popn,
                                                triangles = hasAge)
             exposure.calc <- methods::new("Exposure",
                                  .Data = exposure.calc@.Data,
                                  metadata = exposure.calc@metadata)
             if (!isTRUE(all.equal(expectedExposure, exposure.calc)))
                 return(gettextf("'%s' and '%s' for '%s' inconsistent",
                                 "expectedExposure", "theta", "population"))
             TRUE
         })

## NO_TESTS
setClass("ExposureMixin",
         slots = c(exposure = "Exposure"),
         contains = "VIRTUAL",
         validity = function(object) {
             exposure <- object@exposure
             population <- object@account@population
             hasAge <- object@hasAge@.Data
             ## 'exposure' object equals result of calling 'exposure'
             ## function on 'population'
             exposure.calc <- dembase::exposure(population,
                                                triangles = hasAge)
             exposure.calc <- methods::new("Exposure",
                                  .Data = exposure.calc@.Data,
                                  metadata = exposure.calc@metadata)
             if (!isTRUE(all.equal(exposure, exposure.calc))) {
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "exposure", "population"))
             }
             TRUE
         })

setClass("GeneratedNewProposalMixin",
         slots = c(generatedNewProposal = "LogicalFlag"),
         contains = "VIRTUAL")

## NO_TESTS
setClass("HasAgeMixin",
         slots = c(hasAge = "LogicalFlag"),
         contains = "VIRTUAL",
         validity = function(object) {
             hasAge <- object@hasAge@.Data
             population <- object@account@population
             popn.has.age <- "age" %in% dimtypes(population, use.names = FALSE)
             if (!identical(hasAge, popn.has.age))
                 return(gettextf("'%s' and '%s' not consistent",
                                 "hasAge", "population"))
             TRUE
         })                 

## HAS_TESTS
setClass("HasExposure",
         slots = c(exposure = "Counts"),
         contains = "VIRTUAL",
         validity = function(object) {
             model <- object@model
             y <- object@y
             exposure <- object@exposure
             ## 'exposure' is missing only if 'y' is
             if (any(is.na(exposure) > is.na(y)))
                 return(gettextf("'%s' has missing values where '%s' does not",
                                 "exposure", "y"))
             ## 'exposure' non-negative
             if (any(exposure < 0, na.rm = TRUE))
                 return(gettextf("'%s' has negative values",
                                 "exposure"))
             ## 'exposure' and 'y' have identical metadata
             if (!identical(exposure@metadata, y@metadata))
                 return(gettextf("'%s' and '%s' have different metadata",
                                 "exposure", "y"))
             ## y is 0 if exposure is 0
             if (any((y[!is.na(y)] > 0) & (exposure[!is.na(y)] == 0)))
                 return(gettextf("%s but %s for some cells",
                                 "y > 0", "exposure == 0"))
             ## 'model' has class "UseExposure"
             if (!methods::is(model, "UseExposure"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "model", class(model)))
             TRUE
         })

## NO_TESTS
setClass("IAccNextMixin",
         slots = c(iAccNext = "integer",
                   iAccNextOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             accession <- object@accession
             n.accession <- length(accession)
             for (name in c("iAccNext", "iAccNextOther")) {
                 value <- methods::slot(object, name)
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
                     n.accession <- length(accession)
                     if (value > n.accession)
                         return(gettextf("'%s' is greater than the length of '%s'",
                                         name, "accession"))
                 }
             }
             TRUE
         })

## NO_TESTS
setClass("ICellMixin",
         slots = c(iCell = "integer",
                   iCellOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             for (name in c("iCell", "iCellOther")) {
                 value <- methods::slot(object, name)
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
         slots = c(iComp = "integer",
                   iBirths = "integer",
                   iIntNet = "integer",
                   iOrigDest = "integer",
                   iParCh = "integer",
                   iPool = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iBirths <- object@iBirths
             iOrigDest <- object@iOrigDest
             iPool <- object@iPool
             iIntNet <- object@iIntNet
             iParCh <- object@iParCh
             components <- object@account@components
             s <- c(-1L, 0L, seq_along(components))
             for (name in c("iComp", "iBirths", "iIntNet", "iOrigDest", "iParCh", "iPool")) {
                 value <- methods::slot(object, name)
                 ## 'iComp', 'iBirths', iOrigDest', 'iParCh', 'iPool' 'iIntNet' has  length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## 'iComp', 'iBirths', 'iOrigDest', 'iParCh', 'iPool', 'iIntNet' is not missing
                 if (is.na(value))
                     return(gettextf("'%s' is missing",
                                     name))
                 ## if 'iComp', 'iBirths', iOrigDest', 'iParCh', 'iPool', 'iIntNet' is -1, 0, or index of a component
                 if (!(value %in% s))
                     return(gettextf("'%s' does not index a component",
                                     name))
             }
             ## 'iBirths', 'iIntNet', iOrigDest', 'iPool' do not overlap, except at -1L
             indices <- c(iBirths, iIntNet, iOrigDest, iPool)
             indices.valid <- indices[indices != -1L]
             if (any(duplicated(indices.valid)))
                 return(gettextf("'%s', '%s', '%s', '%s' overlap",
                                 "iBirths", "iIntNet", "iOrigDest", "iPool"))
             ## if 'iParCh' is not -1L, then it equals 'iBirths'
             if ((iParCh != -1L) && (iParCh != iBirths))
                 return(gettextf("'%s' does not equal %d, and does not equal '%s'",
                                 -1L, "iParCh", "iBirths"))                 
             TRUE
         })


## NO_TESTS
## the index of the first cell in 'exposure' that
## will change if the cell being updated is changed
setClass("IExpFirstMixin",
         slots = c(iExpFirst = "integer",
                   iExpFirstOther = "integer",
                   iExposure = "integer",
                   iExposureOther = "integer",
                   iPopnNext = "integer",
                   iPopnNextOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             ## iExpFirst, iExpFirstOther
             for (name in c("iExpFirst", "iExpFirstOther")) {
                 value <- methods::slot(object, name)
                 ## 'iExpFirst', 'iExpFirstOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iExpFirst', 'iExpFirstOther' not missing, they are greater than or equal to 0L
                 if (!is.na(value) && (value < 0L))
                     return(gettextf("'%s' is less than %d",
                                     name, 0L))
                 ## if 'iExpFirst', 'iExpFirstOther' not missing, they are less than or
                 ## equal to length of 'exposure'
                 if (!is.na(value)) {
                     exposure <- object@exposure
                     n.exposure <- length(exposure)
                     if (value > n.exposure)
                         return(gettextf("'%s' is greater than the length of '%s'",
                                         name, "exposure"))
                 }
             }
             ## iExposure, iExposureOther
             for (name in c("iExposure", "iExposureOther")) {
                 value <- methods::slot(object, name)
                 ## 'iExposure', 'iExposureOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iExposure', 'iExposureOther' not missing, they are greater than or equal to 0L
                 if (!is.na(value) && (value < 0L))
                     return(gettextf("'%s' is less than %d",
                                     name, 0L))
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
             ## iPopnNext, iPopnNextOther
             population <- object@account@population
             n.population <- length(population)
             for (name in c("iPopnNext", "iPopnNextOther")) {
                 value <- methods::slot(object, name)
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

             TRUE
         })

## ## NO_TESTS
## ## the index of the first cell in 'exposure' that
## ## will change if the cell being updated is changed
## setClass("IExpFirstMixin",
##          slots = c(iExpFirst = "integer",
##                    iExpFirstOther = "integer"),
##          contains = "VIRTUAL",
##          validity = function(object) {
##              for (name in c("iExpFirst", "iExpFirstOther")) {
##                  value <- methods::slot(object, name)
##                  ## 'iExpFirst', 'iExpFirstOther' have length 1
##                  if (!identical(length(value), 1L))
##                      return(gettextf("'%s' does not have length %d",
##                                      name, 1L))
##                  ## if 'iExpFirst', 'iExpFirstOther' not missing, they are greater than or equal to 0L
##                  if (!is.na(value) && (value < 0L))
##                      return(gettextf("'%s' is less than %d",
##                                      name, 0L))
##                  ## if 'iExpFirst', 'iExpFirstOther' not missing, they are less than or
##                  ## equal to length of 'exposure'
##                  if (!is.na(value)) {
##                      exposure <- object@exposure
##                      n.exposure <- length(exposure)
##                      if (value > n.exposure)
##                          return(gettextf("'%s' is greater than the length of '%s'",
##                                          name, "exposure"))
##                  }
##              }
##              TRUE
##          })

## ## NO_TESTS
## ## The index of the cell in 'exposure' that appears in the likelihood for
## ## the cell being updated.  iExposure is 0L if the model for the cell being
## ## updated does not include exposure.
## setClass("IExposureMixin",
##          slots = c(iExposure = "integer",
##                    iExposureOther = "integer"),
##          contains = "VIRTUAL",
##          validity = function(object) {
##              for (name in c("iExposure", "iExposureOther")) {
##                  value <- methods::slot(object, name)
##                  ## 'iExposure', 'iExposureOther' have length 1
##                  if (!identical(length(value), 1L))
##                      return(gettextf("'%s' does not have length %d",
##                                      name, 1L))
##                  ## if 'iExposure', 'iExposureOther' not missing, they are greater than or equal to 0L
##                  if (!is.na(value) && (value < 0L))
##                      return(gettextf("'%s' is less than %d",
##                                      name, 0L))
##                  ## if 'iExposure', 'iExposureOther' not missing, they are less than or
##                  ## equal to length of 'exposure'
##                  if (!is.na(value)) {
##                      exposure <- object@exposure
##                      n.exposure <- length(exposure)
##                      if (value > n.exposure)
##                          return(gettextf("'%s' is greater than the length of '%s'",
##                                          name, "exposure"))
##                  }
##              }
##              TRUE
##          })

setClass("IMethodCombined",
         slots = c(iMethodCombined = "integer"),
         contains = "VIRTUAL")

## ## NO_TESTS
## setClass("IPopnNextMixin",
##          slots = c(iPopnNext = "integer",
##                    iPopnNextOther = "integer"),
##          contains = "VIRTUAL",
##          validity = function(object) {
##              population <- object@account@population
##              n.population <- length(population)
##              for (name in c("iPopnNext", "iPopnNextOther")) {
##                  value <- methods::slot(object, name)
##                  ## 'iPopnNext', 'iPopnNextOther' have length 1
##                  if (!identical(length(value), 1L))
##                      return(gettextf("'%s' does not have length %d",
##                                      name, 1L))
##                  ## if 'iPopnNext', 'iPopnNextOther' not missing, they are greater than or equal to 0L
##                  if (!is.na(value) && (value < 0L))
##                      return(gettextf("'%s' is less than %d",
##                                      name, 1L))
##                  ## if 'iPopnNext', 'iPopnNextOther' not missing, they are less than or
##                  ## equal to length of 'population'
##                  if (!is.na(value) && (value > n.population))
##                      return(gettextf("'%s' is greater than the length of '%s'",
##                                      name, "population"))
##              }
##              TRUE
##          })

## NO_TESTS
setClass("IsIncrementMixin",
         slots = c(isIncrement = "logical"),
         contains = "VIRTUAL",
         validity = function(object) {
             isIncrement <- object@isIncrement
             components <- object@account@components
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

setClass("IsLowerTriangleMixin",
         slots = c(isLowerTriangle = "logical"),
         contains = "VIRTUAL",
         validity = function(object) {
             isLowerTriangle <- object@isLowerTriangle
             hasAge <- object@hasAge@.Data
             ## 'isLowerTriangle' has length 1
             if (!identical(length(isLowerTriangle), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "lowerTriangle", 1L))
             ## 'isLowerTriangle' is missing if 'hasAge' is FALSE
             if (!hasAge && !is.na(isLowerTriangle))
                 return(gettextf("'%s' is %s but '%s' is not missing",
                                 "hasAge", "FALSE", "lowerTriangle"))
             TRUE
         })

setClass("IsNetMixin",
         contains = "VIRTUAL",
         slots = c(isNet = "logical"),
         validity = function(object) {
             isNet <- object@isNet
             components <- object@account@components
             if (!identical(length(isNet), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "isNet", "components"))
             if (any(is.na(isNet)))
                 return(gettextf("'%s' has missing values",
                                 "isNet"))
             is.net.move <- sapply(components, methods::is, "NetMovements")
             is.int.net <- sapply(components, methods::is, "InternalMovementsNet")
             if (!identical(isNet, is.net.move | is.int.net))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "isNet", "components"))
             TRUE
         })

setClass("IteratorsAccountMixin",
         contains = "VIRTUAL",
         slots = c(iteratorExposure = "CohortIteratorComponent",
                   iteratorPopn = "CohortIteratorPopulation",
                   iteratorsComp = "list"),
         validity = function(object) {
             iteratorsComp <- object@iteratorsComp
             components <- object@account@components
             iOrigDest <- object@iOrigDest
             iPool <- object@iPool
             ## all elements have class "CohortIteratorComponent"
             if (!all(sapply(iteratorsComp, methods::is, "CohortIteratorComponent")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "iteratorsComp", "CohortIteratorComponent"))
             ## 'iteratorsComp' has same length as 'components'
             if (!identical(length(iteratorsComp), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "iteratorsComp", "components"))
             ## elements have class "CohortIteratorOrigDestParChPool" iff
             ## they have class "InternalMovements" or "HasParentChild",
             for (i in seq_along(iteratorsComp)) {
                 is.iter.odpcp <- methods::is(iteratorsComp[[i]], "CohortIteratorOrigDestParChPool")
                 is.odpcp <- ((i == iOrigDest)
                     || methods::is(components[[i]], "HasParentChild")
                     || (i == iPool))
                 if (is.iter.odpcp && !is.odpcp)
                     return(gettextf("element %d of '%s' has class \"%s\" but element %d of '%s' has class \"%s\"",
                                     i, "iteratorsComp", class(iteratorsComp[[i]]),
                                     i, "components", class(components[[i]])))
                 if (is.odpcp && !is.iter.odpcp)
                     return(gettextf("element %d of '%s' has class \"%s\" but element %d of '%s' has class \"%s\"",
                                     i, "components", class(components[[i]]),
                                     i, "iteratorsComp", class(iteratorsComp[[i]])))
             }
             TRUE
         })    
         
setClass("IteratorAccMixin",
         contains = "VIRTUAL",
         slots = c(iteratorAcc = "CohortIteratorAccession"))

setClass("MappingsAccountMixin",
         slots = c(mappingsFromExp = "list",
                   mappingsToExp = "list",
                   mappingsToPopn = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             ## mappingsFromExp
             mappingsFromExp <- object@mappingsFromExp
             components <- object@account@components
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
             is.mapping.births <- sapply(mappingsFromExp, methods::is, "MappingExpToBirths")
             is.births <- sapply(components, methods::is, "Births")
             if (!identical(is.mapping.births, is.births))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsFromExp", "MappingExpToBirths", "components", "Births"))
             ## mappingsToExp
             mappingsToExp <- object@mappingsToExp
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
             is.mapping.births <- sapply(mappingsToExp, methods::is, "MappingBirthsToExp")
             is.births <- sapply(components, methods::is, "Births")
             if (!identical(is.mapping.births, is.births))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToExp", "MappingBirthsToExp", "components", "Births"))             
             ## element has class "MappingOrigDestToExp" iff corresponding
             ## element of 'components' has class "HasOrigDest",
             is.mapping.orig.dest <- sapply(mappingsToExp, methods::is, "MappingOrigDestToExp")
             is.has.orig.dest <- sapply(components, methods::is, "HasOrigDest")
             if (!identical(is.mapping.orig.dest, is.has.orig.dest))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToExp", "MappingOrigDestToExp", "components", "HasOrigDest"))             
             ## mappingsToPopn
             mappingsToPopn <- object@mappingsToPopn
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
             is.mapping.orig.dest <- sapply(mappingsToPopn, methods::is, "MappingOrigDestToPopn")
             is.has.orig.dest <- sapply(components, methods::is, "HasOrigDest")
             if (!identical(is.mapping.orig.dest, is.has.orig.dest))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToPopn", "MappingOrigDestToPopn", "components", "HasOrigDest"))             
             TRUE
         })

## NO_TESTS
setClass("MappingsToAccMixin",
         slots = c(mappingsToAcc = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             mappingsToAcc <- object@mappingsToAcc
             components <- object@account@components
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
             is.mapping.orig.dest <- sapply(mappingsToAcc, methods::is, "MappingOrigDestToAcc")
             is.has.orig.dest <- sapply(components, methods::is, "HasOrigDest")
             if (!identical(is.mapping.orig.dest, is.has.orig.dest))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToAcc", "MappingOrigDestToAcc", "components", "HasOrigDest"))             
             TRUE
         })


## NO_TESTS
setClass("ModelUsesExposureMixin",
         slots = c(modelUsesExposure = "logical"),
         contains = "VIRTUAL",
         validity = function(object) {
             modelUsesExposure <- object@modelUsesExposure
             systemModels <- object@systemModels
             namesComponents <- object@account@namesComponents
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
             for (i in seq_along(namesComponents)) {
                 model <- systemModels[[i + 1L]]
                 if (methods::is(model, "Normal") && modelUsesExposure[i + 1L])
                     return(gettextf("system model for '%s' uses exposure but has class \"%s\"",
                                     name, "Normal"))
             }
             TRUE
         })

## HAS_TESTS
setClass("NamesDatasetsMixin",
         slots = c(namesDatasets = "character"),
         contains = "VIRTUAL",
         validity = function(object) {
             namesDatasets <- object@namesDatasets
             dataModels <- object@dataModels
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
             ## 'dataModels' and 'namesDatasets' have same length
             if (!identical(length(dataModels), length(namesDatasets)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "dataModels", "namesDatasets"))
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
             if (!identical(length(nCellAccount), 1L))
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

## HAS_TESTS
setClass("NotHasExposure",
         contains = "VIRTUAL",
         validity = function(object) {
             model <- object@model
             ## 'model' has class "NotUseExposure"
             if (!methods::is(model, "NotUseExposure"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "model", class(model)))
             TRUE
         })

## HAS_TESTS
setClass("NotHasSubtotals",
         contains = "VIRTUAL",
         validity = function(object) {
             model <- object@model
             y <- object@y
             if (methods::is(y, "HasSubtotals"))
                 return(gettextf("'%s' has class \"%s\" but '%s' has subtotals",
                                 "model", class(model), "y"))
             TRUE
         })

## HAS_TESTS
setClass("DataModelsMixin",
         slots = c(dataModels = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             dataModels <- object@dataModels
             ## all elements of 'dataModels' have class "Model"
             if (!all(sapply(dataModels, methods::is, "Model")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "dataModels", "Model"))
             ## all elements of 'dataModels' have class "UseExposure"
             if (!all(sapply(dataModels, methods::is, "UseExposure")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "dataModels", "UseExposure"))
             ## 'dataModels' does not have names
             if (!is.null(names(dataModels)))
                 return(gettextf("'%s' has names",
                                 "dataModels"))
             TRUE
         })

## NO_TESTS
## 'seriesIndices' is a mapping from dataModels models
## to demographic series.  'seriesIndices' equals 0L when
## the dataModels model maps back to population
setClass("SeriesIndicesMixin",
         slots = c(seriesIndices = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             seriesIndices <- object@seriesIndices
             dataModels <- object@dataModels
             components <- object@account@components
             ## 'seriesIndices' does not have missing values
             if (any(is.na(seriesIndices)))
                 return(gettextf("'%s' has missing values",
                                 "seriesIndices"))
             ## 'seriesIndices' has same length as 'dataModels'
             if (!identical(length(dataModels), length(seriesIndices)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "dataModels", "seriesIndices"))
             ## elements of 'seriesIndices' inside valid range
             valid.range <- c(0L, seq_along(components))
             if (!all(seriesIndices %in% valid.range))
                 return(gettextf("'%s' outside valid range",
                                 "seriesIndices"))
             TRUE
         })

## NO_TESTS
setClass("SystemModelsMixin",
         slots = c(systemModels = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             systemModels <- object@systemModels
             population <- object@account@population
             components <- object@account@components
             namesComponents <- object@account@namesComponents
             ## all elements of 'systemModels' have class "Model"
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

## NO_TESTS
setClass("SystemModelsUseAgMixin",
         slots = c(systemModelsUseAg = "LogicalFlag"),
         contains = "VIRTUAL",
         validity = function(object) {
             systemModelsUseAg <- object@systemModelsUseAg@.Data
             systemModels <- object@systemModels
             mod.uses.ag <- sapply(systemModels, methods::is, "Aggregate")
             if (systemModelsUseAg && !any(mod.uses.ag))
                 return(gettextf("'%s' is %s but no system models use aggregates",
                                 "systemModelsUseAg", TRUE))
             if (!systemModelsUseAg && any(mod.uses.ag))
                 return(gettextf("'%s' is %s but system models use aggregates",
                                 "systemModelsUseAg", FALSE))
             TRUE
         })


setClass("SystemMovementsMixin",
         contains = "VIRTUAL",
         validity = function(object) {
             systemModels <- object@systemModels
             account <- object@account
             components <- account@components
             namesComponents <- account@namesComponents
             for (i in seq_along(components)) {
                 model <- systemModels[[i + 1L]]
                 component <- components[[i]]
                 name <- namesComponents[i]
                 is.net <- (methods::is(component, "NetMovements")
                     || methods::is(component, "InternalMovementsNet"))
                 is.poisson <- methods::is(model, "Poisson")
                 is.normal <- methods::is(model, "Normal")
                 ## if a component has class "NetMovements" or "InternalNetMigration",
                 ## then its system model has class "Normal"
                 if (is.net && !is.normal)
                     return(gettextf("component has class \"%s\" but corresponding system model does not have class \"%s\"",
                                     class(component), "Normal"))
                 ## otherwise its system model most have class "Poisson"
                 if (!is.net && !is.poisson)
                     return(gettextf("component has class \"%s\" but corresponding system model does not have class \"%s\"",
                                     class(component), "Poisson"))
             }
             TRUE
         })


setClass("TransformExpToBirthsMixin",
         slots = c(transformExpToBirths = "CollapseTransform"),
         contains = "VIRTUAL",
         validity = function(object) {
             transformExpToBirths <- object@transformExpToBirths
             iBirths <- object@iBirths
             exposure <- object@exposure
             modelUsesExposure <- object@modelUsesExposure
             ## if 'iBirths' is -1L, then 'transformExpToBirths' is empty
             if (iBirths == -1L) {
                 if (!identical(transformExpToBirths, methods::new("CollapseTransform")))
                     return(gettextf("account does not have births component, but '%s' is not empty",
                                     "transformExpToBirths"))
             }
             else {
                 if (modelUsesExposure[iBirths + 1L]) {
                     ## if model for births uses exposure, then 'dimBefore' matches dimensions of 'exposure'
                     if (!identical(transformExpToBirths@dimBefore, dim(exposure)))
                         return(gettextf("'%s' from '%s' not consistent with dimensions of '%s'",
                                         "dimBefore", "transformExpToBirths", "exposure"))
                 }
                 else {
                     ## if model for births does not use exposure, then 'transformExpToBirths' is empty
                     if (!identical(transformExpToBirths, methods::new("CollapseTransform")))
                         return(gettextf("model for births does not use exposure, but '%s' is not empty",
                                         "transformExpToBirths"))
                 }
             }
             TRUE
         })


## HAS_TESTS
setClass("TransformsMixin",
         slots = c(transforms = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             transforms <- object@transforms
             dataModels <- object@dataModels
             datasets <- object@datasets
             namesDatasets <- object@namesDatasets
             dimBefore <- function(x) x@dimBefore
             ## all elements of 'transforms' have class "CollapseTransformExtra"
             if (!all(sapply(transforms, is, "CollapseTransformExtra")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "transforms", "CollapseTransformExtra"))
             ## 'transforms' does not have names
             if (!is.null(names(transforms)))
                 return(gettextf("'%s' has names",
                                 "transforms"))
             ## 'dataModels' and 'transforms' have same length
             if (!identical(length(dataModels), length(transforms)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "dataModels", "transforms"))
             ## 'transforms' have 'dimAfter' consistent with datasets
             for (i in seq_along(datasets)) {
                 if (!identical(dim(datasets[[i]]), transforms[[i]]@dimAfter))
                     return(gettextf("'%s' and '%s' for \"%s\" inconsistent",
                                     "dataset", "transform", namesDatasets[i]))
             }
             TRUE
         })

setClass("TransformsExpToCompMixin",
         slots = c(transformsExpToComp = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             transformsExpToComp <- object@transformsExpToComp
             transformExpToBirths <- object@transformExpToBirths
             components <- object@account@components
             namesComponents <- object@account@namesComponents
             exposure <- object@exposure
             iBirths <- object@iBirths
             modelUsesExposure <- object@modelUsesExposure
             ## 'transformsExpToComp' has same length as 'components'
             if (!identical(length(transformsExpToComp), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "transformsExpToComp", "components"))
             for (i in seq_along(components)) {
                 transform <- transformsExpToComp[[i]]
                 if (modelUsesExposure[i + 1L]) {
                     component <- components[[i]]
                     expose <- exposure
                     ## [collapse 'expose' if 'component' has class "Births"]
                     if (i == iBirths)
                         expose <- dembase::collapse(expose,
                                                     transform = transformExpToBirths)
                     if (is.null(transform)) {
                         ## if element of 'transformsExpToComp' is NULL, then 'exposure' and
                         ## 'component' must have the same metadata
                         same.dim <- isTRUE(all.equal(dim(expose), dim(component)))
                         if (!same.dim)
                             return(gettextf("element %d of '%s' is %s but correspondending element of '%s' does not have same dimensions as '%s'",
                                             i, "transformsExpToComp", "NULL", "components", "exposure"))
                     }
                     else {
                         ## if element of 'transformsExpToComp' is non-NULL, it must have
                         ## class "ExtendTransform"
                         if (!methods::is(transform, "ExtendTransform"))
                             return(gettextf("element %d of '%s' has class \"%s\"",
                                             i, "transformsExpToComp", class(transform)))
                         ## after extending 'exposure', 'exposure' and 'component' must have the same metadata
                         expose <- dembase::extend(expose, transform = transform)
                         same.dim <- isTRUE(all.equal(dim(expose), dim(component)))
                         if (!same.dim)
                             return(gettextf("even after \"extend\" transformation, '%s' has different dimensions from '%s'",
                                             "exposure", namesComponents[i]))
                     }
                 }
                 else ## if component does not use exposure, transform is NULL
                     if (!is.null(transform))
                         return(gettextf("system model for '%s' does not use exposure, but corresponding element of '%s' is not %s",
                                         namesComponents[i + 1L], "transformsExpToComp", "NULL"))
             }
             TRUE
         })

## HAS_TESTS
setClass("Y",
         slots = c(y = "DemographicArray"),
         contains = "VIRTUAL",
         validity = function(object) {
             y <- object@y
             if (!is.null(y)) {
                 ## 'y' does not have iteration dimension
                 if (any(dembase::dimtypes(y) == "iteration"))
                     return(gettextf("'%s' has dimension with dimtype \"%s\"",
                                     "y", "iteration"))
                 ## 'y' does not have quantile dimension
                 if (any(dembase::dimtypes(y) == "quantile"))
                     return(gettextf("'%s' has dimension with dimtype \"%s\"",
                                     "y", "quantile"))
             }
             TRUE
         })

## HAS_TESTS
setClass("YCounts",
         contains = c("Y", "VIRTUAL"),
         validity = function(object) {
             y <- object@y
             ## 'y' has class "Counts"
             if (!methods::is(y, "Counts"))
                 return(gettextf("'%s' has class \"%s\"",
                                 "y", class(y)))
             ## 'y' has type "integer"
             if (!is.integer(y))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "y", "integer"))
             TRUE
         })

## HAS_TESTS
setClass("YNonNegativeCounts",
         contains = c("YCounts", "VIRTUAL"),
         validity = function(object) {
             y <- object@y
             ## 'y' non-negative
             if (any(y < 0L, na.rm = TRUE))
                 return(gettextf("'%s' has negative values",
                                 "y"))
             TRUE
         })

