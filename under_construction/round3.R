



## Calling function should test that dataset[i] is not missing
setMethod("logLikelihood",
          signature(model = "Round3",
                    count = "integer",
                    dataset = "Counts",
                    i = "integer"),
          function(model, count, dataset, i, useC = FALSE, useSpecific = FALSE) {
              ## count
              stopifnot(identical(length(count), 1L))
              stopifnot(!is.na(count))
              stopifnot(count >= 0)
              ## dataset
              stopifnot(is.integer(dataset))
              stopifnot(all(dataset[!is.na(dataset)] >= 0))
              ## i
              stopifnot(identical(length(i), 1L))
              stopifnot(!is.na(i))
              stopifnot(i >= 1L)
              ## dataset and i
              stopifnot(i <= length(dataset))
              stopifnot(!is.na(dataset@.Data[i]))
              if (useC) {
                  if (useSpecific)
                      .Call(logLikelihood_Round3_R, model, count, dataset, i)
                  else
                      .Call(logLikelihood_R, model, count, dataset, i)
              }
              else {
                  logLikelihood_Round3(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i)
              }
          })


## Calling function should test that dataset[i] is not missing.
## Assume that 'dataset' is composed entirely of values divisible by 3
logLikelihood_Round3 <- function(model, count, dataset, i) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "Round3"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    stopifnot(count >= 0L)
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    if (useC) {
        .Call(logLikelihood_Round3_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        diff <- abs(x - count)
        if (diff > 2L)
            -Inf
        else if (diff == 2L)
            -log(3)
        else if (diff = 1L)
            log(2) - log(3)
        else
            0
    }
}



 
setMethod("predictModelUseExp",
          signature(object = "Round3Predict"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(is.na(y)))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelUseExp_Round3Predict_R,
                            object, y, exposure)
                  else
                      .Call(predictModelUseExp_R,
                            object, y, exposure)
              }
              else {
                  object
              }
          })



setMethod("transferParamModel",
          signature(model = "Round3Predict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_Round3_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  model
              }
          })


setMethod("updateModelUseExp",
          signature(object = "Round3"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_Round3_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })













