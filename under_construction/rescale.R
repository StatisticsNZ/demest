
getNameAlongFromOutput <- function(output) {
    skeleton <- output$trend
    iAlong <- skeleton@iAlong
    metadata <- skeleton@metadata
    names <- names(metadata)
    names[iAlong]
}

makeSumDeltas <- function(priors, hyper) {
    is.trend <- sapply(priors, methods::is, "WithTrendMixin")
    if (!any(is.trend))
        return(NULL)
    output.trend <- hyper[is.trend]
    output.trend <- lapply(output.trend, `[[`, "trend")
    i.along <- sapply(output.trend, methods::slot, "iAlong")
    metadata <- sapply(output.trend, methods::slot, "metadata")
    
    name.along <- sapply(output.trend, getNameAlongFromOutput)
    skeletons <- tapply(output.trend,
                        INDEX = name.along,
                        FUN = c,
                        simplify = FALSE)
    names(skeletons) <- paste("trend", names(skeletons), sep = ".")
    lapply(skeletons, SkeletonSumDeltas)
}


setClass("SkeletonSumDeltas",
         slots = c(skeletons = "list"),
         validity = function(object) {
             skeletons <- object@skeletons
             ## all elements of 'skeleton' have class "SkeletonStateDLM"
             if (!all(sapply(skeletons, methods::is, "SkeletonStateDLM")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "skeletons", "SkeletonStateDLM"))
             ## 'skeletons' has names
             if (is.null(names(skeletons)))
                 return(gettextf("'%s' does not have names",
                                 "skeletons"))
             TRUE
         })



setMethod("fetchResults",
          signature(object = "SkeletonSumDeltas"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
                   impute = FALSE) {
              skeletons <- object@skeletons
              ans <- lapply(skeletons,
                            fetchResults,
                            nameObject = nameObject,
                            filename = filename,
                            iterations = iterations,
                            nIteration = nIteration,
                            lengthIter = lengthIter,
                            impute = impute)
              Reduce(f = `+`, x = ans)
          })





         
    





