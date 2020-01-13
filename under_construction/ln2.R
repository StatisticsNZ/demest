

setClass("LN2",
         slots = c(alphaLN2 = "ParameterVector",
                   alphaIsZeroLN2 = "logical",
                   alphaIsZeroAllLN2 = "logical",
                   metadataAlphaLN2 = "MetaData",
                   metadataAllAlphaLN2 = "MetaData",
                   nCellBeforeLN2 = "integer",
                   transformAlphaLN2 = "CollapseTransform",
                   truncationLN2 = "integer",
                   truncationAllLN2 = "integer"),
         contains = c("Model",
                      "ASigmaMixin",
                      "CellInLikMixin",
                      "MaxAttemptMixin",
                      "NuSigmaMixin",
                      "SigmaMaxMixin",
                      "SigmaMixin",
                      "VarsigmaUnknown"))



setMethod("drawModelUseExp",
          signature(object = "LN2"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
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
              varsigma <- model@varsigma@.Data
              sigma <- model@sigma@.Data
              ## make alpha
              first <- pos
              pos <- first + length(alpha)
              .Data <- array(alpha,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              alpha <- methods::new("Values",
                                    metadata = metadata,
                                    .Data = .Data)
              s <- set_along(dim(metadata))
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
              list(mean = alpha,
                   sd = varsigma,
                   priorSD = sigma)
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
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
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
                  object <- updateSDAlphaLN2(object)
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
    stopifnot(all(exposure[!is.na(exposure)] >= 0L))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
    if (useC) {
        .Call(updateAlphaLN2_R, object, y, exposure)
    }
    else {            
        alpha <- object@alphaLN2@.Data
        alpha.is.zero <- object@alphaIsZeroLN2
        cell.in.lik <- object@cellInLik
        n.cell.vec <- object@nCellBeforeLN2
        transform <- object@transformAlphaLN2
        truncation <- object@truncationLN2
        varsigma <- object@varsigma@.Data # variance of log(y + 1)
        sigma <- object@sigma@.Data # variance of alpha
        varsigma.sq <- varsigma^2
        sigma.sq <- sigma^2
        for (i.alpha in seq_along(alpha)) {
            if (!alpha.is.zero[i.alpha]) {
                i.y.vec <- dembase::getIBefore(i = i.alpha,
                                               transform = transform)
                n.cell <- n.cell.vec[i.alpha]
                trunc <- truncation[i.alpha]
                prec.data  <- n.cell / varsigma.sq
                prec.prior <- 1 / sigma.sq
                V <- 1 / (prec.data + prec.prior)
                y.vec <- y[i.y.vec]
                expose.vec <- exposure[i.y.vec]
                sum.y <- sum(log1p(y.vec))
                sum.expose  <- sum(log1p(expose.vec))
                mean.post <- prec.data * V * (sum.y - sum.expose) / n.cell
                sd.post <- sqrt(V)
                if (trunc == -1L) {
                    alpha[i.alpha] <- rtnorm1(mean = mean.post,
                                              sd = sd.post,
                                              lower = -Inf,
                                              upper = 0)
                }
                else if (trunc == 0L) {
                    alpha[i.alpha] <- stats::rnorm(n = 1L,
                                                   mean = mean.post,
                                                   sd = sd.post)
                }
                else {
                    alpha[i.alpha] <- rtnorm1(mean = mean.post,
                                              sd = sd.post,
                                              lower = 0,
                                              upper = Inf)
                }
            }
        }
        object@alphaLN2@.Data <- alpha
        object
    }
}
        
updateVarsigmaLN2 <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(identical(length(y), length(object@cellInLik)))
    stopifnot(is.integer(y))
    stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
    ## exposure
    stopifnot(is.integer(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0L))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
    if (useC) {
        .Call(updateSigmaLN2_R, object)
    }
    else {
        varsigma <- object@varsigma@.Data
        varsigma.max <- object@varsigmaMax@.Data
        A <- object@AVarsigma@.Data
        nu <- object@nuVarsigma@.Data
        alpha <- object@alphaLN2@.Data
        cell.in.lik <- object@cellInLik
        transform <- object@transformAlphaLN2
        V <- 0
        n <- 0L
        for (i in seq_along(y)) {
            if (cell.in.lik[i]) {
                j <- dembase::getIAfter(i = i,
                                        transform = transform)
                V <- V + (log1p(y[i]) - log1p(exposure[i]) - alpha[j])^2
                n <- n + 1L
            }
        }
        varsigma <- updateSDNorm(sigma = varsigma,
                                 A = A,
                                 nu = nu,
                                 V = V,
                                 n = n,
                                 max = varsigma.max)
        successfully.updated <- varsigma > 0
        if (successfully.updated)
            object@varsigma@.Data <- varsigma
        object
    }
}

updateSigmaLN2 <- function(object, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateSigmaLN2_R, object)
    }
    else {
        alpha <- object@alphaLN2@.Data
        alpha.is.zero <- object@alphaIsZeroLN2
        sigma <- object@sigma@.Data
        sigma.max <- object@sigmaMax@.Data
        A <- object@ASigma@.Data
        nu <- object@nuSigma@.Data
        V <- 0
        n <- 0L
        for (j in seq_along(alpha)) {
            if (!alpha.is.zero[j]) {
                V <- V + alpha[j]^2
                n <- n + 1L
            }
        }
        sigma <- updateSDNorm(sigma = sigma,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n,
                              max = sigma.max)
        successfully.updated <- sigma > 0
        if (successfully.updated)
            object@sigma@.Data <- sigma
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
              stopifnot(is.double(exposure))
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
        alpha.is.zero <- object@alphaIsZeroLN2
        truncation <- object@truncationLN2
        sigma <- object@sigma@.Data # variance of alpha
        for (i.alpha in seq_along(alpha)) {
            if (!alpha.is.zero[i.alpha]) {
                x <- stats::rnorm(n = 1L,
                                  mean = 0,
                                  sd = sigma)
                if (trunc == -1L) {
                    alpha[i.alpha] <- -1 * abs(x)
                }
                else if (trunc == 0L) {
                    alpha[i.alpha] <- x
                }
                else {
                    alpha[i.alpha] <- abs(x)
                }
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



makeNCellLN2 <- function(object) {
    alpha <- object@alphaLN2@.Data
    cell.in.lik <- object@cellInLik
    transform <- object@transform
    ans <- integer(length = length(alpha))
    for (i in seq_along(cell.in.lik)) {
        if (cell.in.lik.[i]) {
            j <- dembase::getIAfter(i = i,
                                    transform = transform,
                                    check = FALSE,
                                    useC = TRUE)
            ans[i] <- alpha[i] + 1L
        }
    }
    ans
}



printLN2LikEqns <- function(object) {
    cat("   log(y[i] + 1) ~ Normal(log(exposure[i] + 1) * alpha[j[i]], sd^2)\n")
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
    cat(name.y, response, " ~ Normal(log(", exposure, "[i] + 1) + alpha[j[i]], sd^2)\n",
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
    cat(name.y, "[i] ~ Normal(", exposure, "[i] * mean[i], sd[i]^2)\n", sep = "")
    n.spaces <- max(5L - nchar(name.y), 0L)
    spaces <- rep(" ", n.spaces)
    response <- sprintf("%slog(%s + 1)", spaces, name.y)
    cat(name.y, response, " ~ Normal(log(", exposure, "[i] + 1) + alpha[j[i]], sd^2)\n",
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

