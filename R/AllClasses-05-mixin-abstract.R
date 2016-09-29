
## Mixin classes that extend a general class such as "numeric" or "list",
## and that do not contain a specific slot

setClass("Counter",
         contains = "integer",
         validity = function(object) {
             ## 'object' has length 1
             if (!identical(length(object), 1L))
                 return(gettextf("does not have length %d",
                                 1L))
             ## 'object' is not missing
             if (is.na(object))
                 return(gettext("missing"))
             ## 'object' is non-negative
             if (object < 0)
                 return(gettext("negative"))
             TRUE
         })

setClass("DegreesFreedom",
         contains = "numeric",
         validity = function(object) {
             ## 'object' has length 1
             if (!identical(length(object), 1L))
                 return(gettextf("does not have length %d",
                                 1L))
             ## 'object' has type "double"
             if (!is.double(object))
                 return(gettextf("does not have type \"%s\"",
                                 "double"))
             ## 'object' is not missing
             if (is.na(object))
                 return(gettext("missing"))
             ## 'object' is positive
             if (object <= 0)
                 return(gettext("non-positive"))
             TRUE
         })

setClass("DLMMatrix",
         contains = "matrix",
         validity = function(object) {
             ## 'object' has type "double"
             if (!is.double(object))
                 return(gettextf("does not have type \"%s\"",
                                 "double"))
             ## 'object' has no missing values
             if (any(is.na(object)))
                 return(gettext("has missing values"))
             ## 'object' is square
             if (!identical(nrow(object), ncol(object)))
                 return(gettext("not square"))
             TRUE
         })

setClass("FFBSList",
         contains = "list",
         validity = function(object) {
             ## all elements have type "double"
             if (!all(sapply(object, is.double)))
                 return(gettextf("elements not of type \"%s\"",
                                 "double"))
             ## at least one element
             if (length(object) == 0L)
                 return(gettextf("length %d",
                                 0L))
             ## all elements have same length
             if (length(object) > 1L) {
                 lengths <- sapply(object, length)
                 if (!all(mapply(identical, lengths[1L], lengths[-1L])))
                     return(gettext("elements have different lengths"))
             }
             ## no missing values
             if (any(sapply(object, function(x) any(is.na(x)))))
                 return(gettext("missing values"))
             TRUE
         })

setClass("Length",
         contains = "integer",
         validity = function(object) {
             ## 'object' has length 1
             if (!identical(length(object), 1L))
                 return(gettextf("does not have length %d",
                                 1L))
             ## 'object' is not missing
             if (is.na(object))
                 return(gettext("missing"))
             ## 'object' is positive
             if (object <= 0)
                 return(gettext("non-positive"))
             TRUE
         })

setClass("LogicalFlag",
         contains = "logical",
         validity = function(object) {
             ## 'object' has length 1
             if (!identical(length(object), 1L))
                 return(gettextf("does not have length %d",
                                 1L))
             ## 'object' is not missing
             if (is.na(object))
                 return(gettext("missing"))
             TRUE
         })

setClass("Name",
         contains = "character",
         validity = function(object) {
             ## 'object' has length 1
             if (!identical(length(object), 1L))
                 return(gettextf("does not have length %d",
                                 1L))
             ## 'object' is not missing
             if (is.na(object))
                 return(gettext("missing"))
             ## 'object' is not blank
             if (!nzchar(object))
                 return(gettext("blank"))
             TRUE
         })

setClass("ParameterVector",
         contains = "numeric",
         validity = function(object) {
             ## 'object' has type "double"
             if (!is.double(object))
                 return(gettextf("does not have type \"%s\"",
                                 "double"))
             ## 'object' has no missing values
             if (any(is.na(object)))
                 return(gettext("has missing values"))
             TRUE
         })

setClass("Scale",
         contains = "numeric",
         validity = function(object) {
             ## 'object' has length 1
             if (!identical(length(object), 1L))
                 return(gettextf("does not have length %d",
                                 1L))
             ## 'object' has type "double"
             if (!is.double(object))
                 return(gettextf("does not have type \"%s\"",
                                 "double"))
             ## 'object' is not missing
             if (is.na(object))
                 return(gettext("missing"))
             ## 'object is positive
             if (object <= 0)
                 return(gettextf("non-positive"))
             TRUE
         })

setClass("ScaleVec",
         contains = "numeric",
         validity = function(object) {
             ## 'object' is double
             if (!is.double(object))
                 return(gettextf("does not have type \"%s\"",
                                 "double"))
             ## 'object' has no missing values
             if (any(is.na(object)))
                 return(gettext("missing values"))
             ## 'object' all positive
             if (any(object <= 0))
                 return(gettextf("non-positive values"))
             TRUE
         })

setClass("SpecName",
         contains = "character",
         validity = function(object) {
             ## 'object' has length 1
             if (!identical(length(object), 1L))
                 return(gettextf("does not have length %d",
                                 1L))
             ## 'object' is not blank
             if (!nzchar(object))
                 return(gettext("blank"))
             TRUE
         })

setClass("SpecScale",
         contains = "numeric",
         validity = function(object) {
             ## 'object' has length 1
             if (!identical(length(object), 1L))
                 return(gettextf("does not have length %d",
                                 1L))
             ## 'object' has type "double"
             if (!is.double(object))
                 return(gettextf("does not have type \"%s\"",
                                 "double"))
             ## if 'object' is not missing, 'object is positive
             if (!is.na(object) && (object <= 0))
                 return(gettextf("non-positive"))
             TRUE
         })

setClass("VarTDist",
         contains = "numeric",
         validity = function(object) {
             ## 'object' has at least one element
             if (identical(length(object), 0L))
                 return(gettextf("'%s' has length %d",
                                 0L))
             ## 'object' has no missing values
             if (any(is.na(object)))
                 return(gettext("has missing values"))
             ## 'object' is double
             if (!is.double(object))
                 return(gettextf("does not have type \"%s\"",
                                 "double"))
             ## 'object' all positive
             if (any(object <= 0))
                 return(gettext("non-positive values"))
             TRUE
         })


