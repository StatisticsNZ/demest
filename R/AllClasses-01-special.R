
## S3 CLASSES ##################################################################

setOldClass("data.frame")

setOldClass("mcmc")

setOldClass("mcmc.list")


## CLASSES FOR SPECIAL ELEMENTS ######################################################

## HAS_TESTS
setClass("Offsets",
         contains = "integer",
         validity = function(object) {
             ## 'object' has length 2
             if (!identical(length(object), 2L))
                 return(gettextf("'%s' does not have length %d",
                                 "offsets", 2L))
             ## 'object' has no missing values
             if (any(is.na(object)))
                 return(gettextf("'%s' has missing values",
                                 "offsets"))
             ## all elements of 'object' are positive
             if (any(object <= 0L))
                 return(gettextf("'%s' has non-positive elements",
                                 "offsets"))
             ## second element >= first element
             if (object[2L] < object[1L])
                 return(gettextf("second element of '%s' less than first",
                                 "offsets"))
             TRUE
         })

setClassUnion(name = "OffsetsOrNULL",
              members = c("Offsets", "NULL"))


## CLASS UNIONS ##################################################################

setClassUnion("CountsOrNULL",
              members = c("Counts", "NULL"))

setClassUnion("characterOrNULL",
              members = c("character", "NULL"))

setClassUnion("dataframeOrNULL",
              members = c("data.frame", "NULL"))

setClassUnion("listOrNULL",
              members = c("list", "NULL"))

setClassUnion("numericOrNULL",
              members = c("numeric", "NULL"))

setClassUnion("numericOrCharacterOrNULL",
              members = c("numeric", "character", "NULL"))

setClassUnion("matrixOrNULL",
              members = c("matrix", "NULL"))

setClassUnion("ValuesOrNULL",
              members = c("Values", "NULL"))

setClassUnion("ValuesOrNumeric",
              members = c("Values", "numeric"))

