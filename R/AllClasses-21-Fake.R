

## HAS_TESTS
setClass("FakeData",
         slots = c(call = "call",
             model = "list",
             y = "DemographicArray"),
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
setClass("FakeDataExposure",
         slots = c(call = "call",
             model = "list", ## list all slots so that they show up correctly in listContents
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
