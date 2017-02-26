

    
p(age:time[l,k]) = sum_h weights[h,k] N(age:time[l,k] | components[l,h], scaleError)
components[l,h] = prod_d v_d[l,h]
v_d[l,h] ~ N(0, scaleComponent)
weights[h,k] = f(level1AR[1,k], ..., level1AR[h,k])
level1AR[h,k] = level2AR[h,k] + error1AR[h,k]
level2AR[h,k] = meanAR + coefAR * level2AR[h,k-1] + error2AR[h,k]
scaleC



    level1AR[h,k] + error1AR
level1AR = level
v[h
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


                                   
