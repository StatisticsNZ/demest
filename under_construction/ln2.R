
setMethod("drawModelUseExp",
          signature(object = "LN2"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_LN2_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- drawSigma_Varying(object) # not Varying, but works anyway
                  object <- drawVarsigma(object)
                  object <- predictAlphaLN2(object)
                  object
              }
          })


setMethod("logLikelihood",
          signature(model = "LN2",
                    count = "integer",
                    dataset = "Counts",
                    i = "integer"),
          function(model, count, dataset, i, useC = FALSE, useSpecific = FALSE) {
              ## count
              stopifnot(identical(length(count), 1L))
              stopifnot(!is.na(count))
              ## dataset
              stopifnot(is.integer(dataset))
              ## i
              stopifnot(identical(length(i), 1L))
              stopifnot(!is.na(i))
              stopifnot(i >= 1L)
              ## dataset and i
              stopifnot(i <= length(dataset))
              stopifnot(!is.na(dataset@.Data[i]))
              if (useC) {
                  if (useSpecific)
                      .Call(logLikelihood_NormalFixedUseExp_R, model, count, dataset, i)
                  else
                      .Call(logLikelihood_R, model, count, dataset, i)
              }
              else {
                  logLikelihood_LN2(model = model,
                                    count = count,
                                    dataset = dataset,
                                    i = i)
              }
          })



## Calling function should test that dataset[i] is not missing
logLikelihood_LN2 <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "LN2"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
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
        .Call(logLikelihood_LN2_R, model, count, dataset, i)
    }
    else {
        alpha <- model@alphaLN2@.Data
        transform <- model@transformLN2
        sd <- model@varsigma@.Data
        x <- log1p(dataset[[i]])
        j <- dembase::getIAfter(i = i,
                                transform = transform)
        mean <- log1p(count) + alpha[j]
        stats::dnorm(x = x,
                     mean = mean,
                     sd = sd,
                     log = TRUE)
    }
}

setMethod("makeOutputModel",
          signature(model = "NormalFixed"),
          function(model, pos, mcmc) {
              metadata <- model@metadataLN2
              alpha <- model@alphaLN2@.Data
              ## make alpha
              first <- pos
              pos <- first + length(alpha)
              .Data <- array(alpha,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              alpha <- methods::new("Values",
                                    metadata = metadata,
                                    .Data = .Data)
              s <- seq_along(dim(metadata))
              alpha <- Skeleton(object = alpha,
                                first = first,
                                strucZeroArray = NULL,
                                margin = s)
              ## make varsigma
              first <- pos
              pos <- first + 1L
              varsigma <- Skeleton(first = first)
              ## make sigma
              first <- pos
              pos <- first + 1L
              sigma <- Skeleton(first = first)
              ## return value
              likelihood <- list(mean = alpha,
                                 sd = varsigma)
              prior <- list(sd = sigma)
              ans <- list(likelihood = likelihood,
                          prior = prior)
              ans
          })


setMethod("updateModelUseExp",
          signature(object = "LN2"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@cellInLik)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_LN2_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateAlphaLN2(object = object,
                                           y = y,
                                           exposure = exposure)
                  object <- updateSigmaLN2(object = object,
                                           y = y,
                                           exposure = exposure)
                  object <- updateVarsigmaLN2(object)
                  object
              }
          })


updateAlphaLN2 <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(identical(length(y), length(object@cellInLik)))
    stopifnot(is.integer(y))
    stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
    ## exposure
    stopifnot(is.integer(exposure))
    stopifnot(!any(is.na(exposure)))
    stopifnot(all(exposure > 0L))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    if (useC) {
        .Call(updateAlphaLN2_R, object, y, exposure)
    }
    else {            
        alpha <- object@alphaLN2@.Data
        cell.in.lik <- object@cellInLik
        n.cell.vec <- object@nCellBeforeLN2
        constraint <- object@constraintLN2@.Data
        transform <- object@transformLN2
        varsigma <- object@varsigma@.Data # variance of log(y + 1)
        sigma <- object@sigma@.Data # variance of alpha
        varsigma.sq <- varsigma^2
        sigma.sq <- sigma^2
        resid.vec <- rep(0, times = length(alpha)) # or could use alpha
        for (i in seq_along(y)) {
            if (cell.in.lik[i]) {
                j <- dembase::getIAfter(i = i,
                                        transform = transform)
                resid.vec[j] <- resid.vec[j] + log1p(y[i]) - log1p(exposure[i])
            }
        }
        for (j in seq_along(alpha)) {
            constraint.j <- constraint[j]
            update <- is.na(constraint.j) || (constraint.j != 0L)
            if (update) {
                n.cell <- n.cell.vec[j]
                prec.data  <- n.cell / varsigma.sq
                prec.prior <- 1 / sigma.sq
                V <- 1 / (prec.data + prec.prior)
                resid <- resid.vec[j]
                mean.post <- prec.data * V * resid / n.cell
                sd.post <- sqrt(V)
                if (is.na(constraint.j)) {
                    alpha[j] <- stats::rnorm(n = 1L,
                                             mean = mean.post,
                                             sd = sd.post)
                }
                else if (constraint.j == -1L) {
                    alpha[j] <- rtnorm1(mean = mean.post,
                                        sd = sd.post,
                                        lower = -Inf,
                                        upper = 0)
                }
                else if (constraint.j == 1L) {
                    alpha[j] <- rtnorm1(mean = mean.post,
                                        sd = sd.post,
                                        lower = 0,
                                        upper = Inf)
                }
                else
                    stop("invalid value for 'constraint'")
            }
        }
        object@alphaLN2@.Data <- alpha
        object
    }
}
        

    
setMethod("predictModelUseExp",
          signature(object = "LN2"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@cellInLik)))
              stopifnot(all(is.na(y) | (y == 0L)))
              ## exposure
              stopifnot(is.integer(exposure))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelUseExp_LN2Predict_R,
                            object, y, exposure)
                  else
                      .Call(predictModelUseExp_R,
                            object, y, exposure)
              }
              else {
                  object <- predictAlphaLN2(object)
                  object
              }
          })


predictAlphaLN2 <- function(object, useC = FALSE) {
    ## object
    stopifnot(methods::validObject(object))
    stopifnot(methods::is(object, "LN2Predict"))
    if (useC) {
        .Call(predictAlphaLN2_R, object)
    }
    else {
        alpha <- object@alphaLN2@.Data
        constraint <- object@constraintLN2@.Data
        sigma <- object@sigma@.Data # variance of alpha
        for (j in seq_along(alpha)) {
            constraint.j <- constraint[j]
            if (is.na(constraint.j) || (constraint.j != 0L)) {
                x <- stats::rnorm(n = 1L,
                                  mean = 0,
                                  sd = sigma)
                if (is.na(constraint.j))
                    alpha[j] <- x
                else if (constraint.j == -1L)
                    alpha[j] <- -1 * abs(x)
                else if (constraint.j == 1L)
                    alpha[j] <- abs(x)
                else
                    stop("invalid value for 'constraint'")
            }
        }
        object@alphaLN2@.Data <- alpha
        object
    }
}

setMethod("showModelHelper",
          signature(object = "LN2"),
          function(object) {
              printLN2ModEqns(object)
          })





printLN2LikEqns <- function(object) {
    nu <- object@nuVarsigma@.Data
    A <- object@AVarsigma@.Data
    max <- object@varsigmaMax@.Data
    cat("   log(y[i] + 1) ~ N(log(exposure[i] + 1) + alpha[j[i]], sdData^2)\n")
    cat("        alpha[i] ~ N*(0, sd^2)\n", sep = "")
    cat("          sdData ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printLN2ModEqns <- function(object) {
    call <- object@call
    series <- call$series
    ASigma <- object@ASigma@.Data
    sigmaMax <- object@sigmaMax
    nuSigma <- object@nuSigma
    AVarsigma <- object@AVarsigma@.Data
    varsigmaMax <- object@varsigmaMax
    nuVarsigma <- object@nuVarsigma
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    n.spaces <- max(5L - nchar(name.y), 0L)
    spaces <- rep(" ", n.spaces)
    response <- sprintf("%slog(%s + 1)", spaces, name.y)
    cat(name.y, response, " ~ N(log(", exposure, "[i] + 1) + alpha[j[i]], sd^2)\n",
        sep = "")
    cat("      alpha[j] ~ N*(0, sdAlpha^2)")
    cat("      sdAlpha ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(AVarsigma),
        ", ",
        format(varsigmaMax, digits = 4),
        ")\n",
        sep = "")
    cat("      sd ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(ASigma),
        ", ",
        format(sigmaMax, digits = 4),
        ")\n",
        sep = "")
}

printLN2SpecEqns <- function(object) {
    series <- object@series@.Data
    call <- object@call
    ASigma <- object@ASigma@.Data
    sigmaMax <- object@sigmaMax
    nuSigma <- object@nuSigma
    AVarsigma <- object@AVarsigma@.Data
    varsigmaMax <- object@varsigmaMax
    nuVarsigma <- object@nuVarsigma
    nameY <- object@nameY
    has.series <- !is.na(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    cat(name.y, "[i] ~ N(", exposure, "[i] * mean[i], sd[i]^2)\n", sep = "")
    n.spaces <- max(5L - nchar(name.y), 0L)
    spaces <- rep(" ", n.spaces)
    response <- sprintf("%slog(%s + 1)", spaces, name.y)
    cat(name.y, response, " ~ N(log(", exposure, "[i] + 1) + alpha[j[i]], sd^2)\n",
        sep = "")
    cat("      alpha[j] ~ N*(0, sdAlpha^2)")
    cat("      sdAlpha ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(AVarsigma),
        ", ",
        format(varsigmaMax, digits = 4),
        ")\n",
        sep = "")
    cat("      sd ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(ASigma),
        ", ",
        format(sigmaMax, digits = 4),
        ")\n",
        sep = "")
}


setMethod("transferParamModel",
          signature(model = "LN2Predict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_LN2_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  model <- transferParamVarsigma(model,
                                                 filename = filename,
                                                 lengthIter = lengthIter,
                                                 iteration = iteration)
                  model <- transferParamSigma(model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model
              }
          })



setMethod("whereAcceptance",
          signature(object = "LN2"),
          function(object) list(NULL))

setMethod("whereAutocorr",
          signature(object = "LN2"),
          function(object) list(NULL))

setMethod("whereJump",
          signature(object = "LN2"),
          function(object) list(NULL))

setMethod("whereEstimated",
          signature(object = "LN2"),
          function(object) {
              list("alpha", "sd", "priorSD")
          })

setMethod("whereNoProposal",
          signature(object = "LN2"),
          function(object) list(NULL))

setMethod("whereTheta",
          signature(object = "LN2"),
          function(object) {
              stop(gettextf("'%s' has class \"%s\"",
                            "object", class(object)))
          })
