



p(age:time[k,l]) = sum_h weight[h,k] N(age:time[k,l] | component[h,l], scaleError)
component[h,l] = prod_d vector_d
vector_d[j] ~ N(0, scaleComponent^2)




setMethod("printPriorEqns",
          signature(object = "Mix"),
          function(object, name = NULL, order = 1L) {
              printMixEqns()
          })



printMixEqns <- function(object, name, order, hasCovariates) {
    is.main <- order == 1L
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%11s", name, sep = "")
    cat(name, "[k,l] ~ level[k,l] + ", sep = "")
    if (hasCovariates)
            cat("covariates[k,l] + ")
    cat("error[k,l]\n")
    if (hasCovariates)
        printCovariatesDLMEqns(object = object,
                               isMain = is.main)
    printErrorDLMEqns(object = object,
                      isMain = is.main)
}


betaHatAlphaMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaMix)
    if (useC) {
        .Call(betaHatAlphaMix_R, prior)
    }
    else {
        prior@alphaMix@.Data
    }
}


                                   
