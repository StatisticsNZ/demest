

## whereAcceptance ####################################################################

## NO_TESTS
setMethod("whereAcceptance",
          signature(object = "DemographicAccount"),
          function(object) list(NULL))


## whereAutocorr ####################################################################

## NO_TESTS
setMethod("whereAutocorr",
          signature(object = "DemographicAccount"),
          function(object) list(NULL))


## whereJump ####################################################################

## NO_TESTS
setMethod("whereJump",
          signature(object = "DemographicAccount"),
          function(object) list(NULL))


## whereEstimated ####################################################################

## NO_TESTS
setMethod("whereEstimated",
          signature(object = "DemographicAccount"),
          function(object) {
              names.components <- object@namesComponents
              list(c("population", names.components))
          })
