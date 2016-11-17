
## drawYNonSampled #####################################################################

## HAS_TESTS
setMethod("drawYNonSampled",
          signature(filename = "character",
                    model = "Binomial",
                    nonsampled = "Counts"),
          function(filename, model, nonsampled, iterations) {
              theta <- fetch(filename,
                             where = c("model", "likelihood", "prob"),
                             iterations = iterations)
              n <- length(theta)
              size <- as.integer(nonsampled)
              prob <- as.numeric(theta)
              .Data <- stats::rbinom(n = n, size = size, prob = prob)
              .Data <- array(.Data, dim = dim(theta), dimnames = dimnames(theta))
              metadata <- theta@metadata
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })


## HAS_TESTS
setMethod("drawYNonSampled",
          signature(filename = "character",
                    model = "Poisson",
                    nonsampled = "Counts"),
          function(filename, model, nonsampled, iterations) {
              if (methods::is(model, "NotUseExposure"))
                  stop(gettext("finite-population estimates not defined for Poisson model without exposure"))
              theta <- fetch(filename,
                             where = c("model", "likelihood", "rate"),
                             iterations = iterations)
              n <- length(theta)
              lambda <- as.numeric(theta) * as.numeric(nonsampled)
              .Data <- stats::rpois(n = n, lambda = lambda)
              .Data <- array(.Data, dim = dim(theta), dimnames = dimnames(theta))
              metadata <- theta@metadata
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })


## HAS_TESTS
setMethod("drawYNonSampled",
          signature(filename = "character",
                    model = "Normal",
                    nonsampled = "Counts"),
          function(filename, model, nonsampled, iterations) {
              w <- model@w
              if (methods::is(model, "VarsigmaKnown"))
                  varsigma <- model@varsigma
              else {
                  varsigma <- fetch(filename,
                                    where = c("model", "likelihood", "sd"),
                                    iterations = iterations)
                  varsigma <- as.numeric(varsigma)
                  varsigma <- rep(varsigma, each = length(w))
              }
              theta <- fetch(filename,
                             where = c("model", "likelihood", "mean"),
                             iterations = iterations)
              n <- length(theta)
              nonsampled <- as.numeric(nonsampled)
              mean <- nonsampled * as.numeric(theta)
              sd <- varsigma * sqrt(nonsampled / w)
              .Data <- stats::rnorm(n = n, mean = mean, sd = sd)
              .Data <- array(.Data, dim = dim(theta), dimnames = dimnames(theta))
              metadata <- theta@metadata
              methods::new("Values", .Data = .Data, metadata = metadata)
          })


## getTransform #################################################################

setMethod("getTransform",
          signature = "BinomialVarying",
          function(object) {
              function(x) log(x / (1 - x))
          })

setMethod("getTransform",
          signature = "NormalVarying",
          function(object) {
              function(x) x
          })

setMethod("getTransform",
          signature = "PoissonVarying",
          function(object) {
              log
          })
          

## logLikelihood ################################################################


## 'logLikelihood' is only used with data models (as part of updating counts or
## account. PoissonNotUseExp models are not used for data models,
## so there are no 'logLikelihood' methods for them.

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
setMethod("logLikelihood",
          signature(model = "BinomialVarying",
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
              ## model and dataset
              stopifnot(identical(length(model@theta), length(dataset)))
              ## model and i
              stopifnot(i <= length(model@theta))
              if (useC) {
                  if (useSpecific)
                      .Call(logLikelihood_Binomial_R, model, count, dataset, i)
                  else
                      .Call(logLikelihood_R, model, count, dataset, i)
              }
              else {
                  logLikelihood_Binomial(model = model,
                                         count = count,
                                         dataset = dataset,
                                         i = i)
              }
          })

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
setMethod("logLikelihood",
          signature(model = "PoissonVaryingUseExp",
                    count = "integer",
                    dataset = "Counts",
                    i = "integer"),
          function(model, count, dataset, i, useC = FALSE, useSpecific = FALSE) {
              ## count
              stopifnot(identical(length(count), 1L))
              stopifnot(!is.na(count))
              stopifnot(count >= 0)
              ## dataset
              stopifnot(all(dataset[!is.na(dataset)] >= 0))
              ## i
              stopifnot(identical(length(i), 1L))
              stopifnot(!is.na(i))
              stopifnot(i >= 1L)
              ## model and dataset
              stopifnot(identical(length(model@theta), length(dataset)))
              ## model and i
              stopifnot(i <= length(model@theta))
              ## dataset and i
              stopifnot(i <= length(dataset))
              stopifnot(!is.na(dataset@.Data[i]))
              if (useC) {
                  if (useSpecific)
                      .Call(logLikelihood_Poisson_R, model, count, dataset, i)
                  else
                      .Call(logLikelihood_R, model, count, dataset, i)
              }
              else {
                  logLikelihood_Poisson(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i)
              }
          })

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
setMethod("logLikelihood",
          signature(model = "PoissonBinomialMixture",
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
                      .Call(logLikelihood_PoissonBinomialMixture_R, model, count, dataset, i)
                  else
                      .Call(logLikelihood_R, model, count, dataset, i)
              }
              else {
                  logLikelihood_PoissonBinomialMixture(model = model,
                                                       count = count,
                                                       dataset = dataset,
                                                       i = i)
              }
          })



## makeOutputAggregate ######################################################

## HAS_TESTS
setMethod("makeOutputAggregate",
          signature(model = "AgCertain"),
          function(model) {
              metadata.y <- model@metadataY
              value.ag <- model@valueAg@.Data
              metadata.ag <- model@metadataAg
              weight.ag <- model@weightAg
              if (is.null(metadata.ag))
                  value <- value.ag
              else {
                  dim.ag <- dim(metadata.ag)
                  dimnames.ag <- dimnames(metadata.ag)
                  .Data.value <- array(value.ag, dim = dim.ag, dimnames = dimnames.ag)
                  value <- methods::new("Values", .Data = .Data.value, metadata = metadata.ag)
              }
              .Data.weights <- array(weight.ag,
                                     dim = dim(metadata.y),
                                     dimnames = dimnames(metadata.y))
              weights <- methods::new("Counts", .Data = .Data.weights, metadata = metadata.y)
              list(value = value,
                   weights = weights)
          })

## HAS_TESTS
setMethod("makeOutputAggregate",
          signature(model = "AgNormal"),
          function(model, pos, nChain, nIteration) {
              metadata.y <- model@metadataY
              metadata.ag <- model@metadataAg
              scale.ag <- model@scaleAg@.Data
              mean.ag <- model@meanAg@.Data
              sd.ag <- model@sdAg@.Data
              weight.ag <- model@weightAg
              max.attempt <- model@maxAttempt@.Data
              if (is.null(metadata.ag)) {
                  first <- pos
                  pos <- first + 1L
                  value <- Skeleton(first = first)
                  mean <- mean.ag
                  sd <- sd.ag
              }
              else {
                  dim.ag <- dim(metadata.ag)
                  dimnames.ag <- dimnames(metadata.ag)
                  first <- pos
                  pos <- first + as.integer(prod(dim.ag))
                  value <- Skeleton(metadata = metadata.ag, first = first)
                  .Data.mean <- array(mean.ag, dim = dim.ag, dimnames = dimnames.ag)
                  mean <- methods::new("Values", .Data = .Data.mean, metadata = metadata.ag)
                  .Data.sd <- array(sd.ag, dim = dim.ag, dimnames = dimnames.ag)
                  sd <- methods::new("Values", .Data = .Data.sd, metadata = metadata.ag)
              }
              first <- pos
              pos <- first + 1L
              no.proposal <- SkeletonAccept(nAttempt = max.attempt,
                                            first = first,
                                            nChain = nChain,
                                            nIteration = nIteration)
              first <- pos
              accept.ag <- SkeletonAccept(nAttempt = length(mean.ag),
                                          first = first,
                                          nChain = nChain,
                                          nIteration = nIteration)
              .Data.weights <- array(weight.ag,
                                     dim = dim(metadata.y),
                                     dimnames = dimnames(metadata.y))
              weights <- methods::new("Counts", .Data = .Data.weights, metadata = metadata.y)
              list(value = value,
                   jump = scale.ag,
                   noProposal = no.proposal,
                   accept = accept.ag,
                   mean = mean,
                   sd = sd,
                   weights = weights)
          })

## HAS_TESTS
setMethod("makeOutputAggregate",
          signature(model = "AgLife"),
          function(model, pos) {
              metadata.y <- model@metadataY
              metadata.ag <- model@metadataAg
              metadata.mx <- model@metadataMxAg
              mean.ag <- model@meanAg@.Data
              sd.ag <- model@sdAg@.Data
              max.attempt <- model@maxAttempt@.Data
              if (is.null(metadata.ag)) {
                  first <- pos
                  pos <- first + 1L
                  value <- Skeleton(first = first)
                  mean <- mean.ag
                  sd <- sd.ag
              }
              else {
                  dim.ag <- dim(metadata.ag)
                  dimnames.ag <- dimnames(metadata.ag)
                  first <- pos
                  pos <- first + as.integer(prod(dim.ag))
                  value <- Skeleton(metadata = metadata.ag, first = first)
                  .Data.mean <- array(mean.ag, dim = dim.ag, dimnames = dimnames.ag)
                  mean <- methods::new("Values", .Data = .Data.mean, metadata = metadata.ag)
                  .Data.sd <- array(sd.ag, dim = dim.ag, dimnames = dimnames.ag)
                  sd <- methods::new("Values", .Data = .Data.sd, metadata = metadata.ag)
              }
              dim.mx <- dim(metadata.mx)
              dimnames.mx <- dimnames(metadata.mx)
              first <- pos
              pos <- first + as.integer(prod(dim.mx))
              mx <- Skeleton(metadata = metadata.mx, first = first)
              list(value = value,
                   mean = mean,
                   sd = sd,
                   mx = mx)
          })

## HAS_TESTS
setMethod("makeOutputAggregate",
          signature(model = "AgFun"),
          function(model, pos, nChain, nIteration) {
              metadata.y <- model@metadataY
              metadata.ag <- model@metadataAg
              mean.ag <- model@meanAg@.Data
              sd.ag <- model@sdAg@.Data
              max.attempt <- model@maxAttempt@.Data
              if (is.null(metadata.ag)) {
                  first <- pos
                  pos <- first + 1L
                  value <- Skeleton(first = first)
                  mean <- mean.ag
                  sd <- sd.ag
              }
              else {
                  dim.ag <- dim(metadata.ag)
                  dimnames.ag <- dimnames(metadata.ag)
                  first <- pos
                  pos <- first + as.integer(prod(dim.ag))
                  value <- Skeleton(metadata = metadata.ag, first = first)
                  .Data.mean <- array(mean.ag, dim = dim.ag, dimnames = dimnames.ag)
                  mean <- methods::new("Values", .Data = .Data.mean, metadata = metadata.ag)
                  .Data.sd <- array(sd.ag, dim = dim.ag, dimnames = dimnames.ag)
                  sd <- methods::new("Values", .Data = .Data.sd, metadata = metadata.ag)
              }
              first <- pos
              pos <- first + 1L
              no.proposal <- SkeletonAccept(nAttempt = max.attempt,
                                            first = first,
                                            nChain = nChain,
                                            nIteration = nIteration)
              first <- pos
              accept.ag <- SkeletonAccept(nAttempt = length(mean.ag),
                                          first = first,
                                          nChain = nChain,
                                          nIteration = nIteration)
              list(value = value,
                   noProposal = no.proposal,
                   accept = accept.ag,
                   mean = mean,
                   sd = sd)
          })

## HAS_TESTS
setMethod("makeOutputAggregate",
          signature(model = "AgPoisson"),
          function(model, pos, nChain, nIteration) {
              metadata.y <- model@metadataY
              metadata.ag <- model@metadataAg
              scale.ag <- model@scaleAg@.Data
              mean.ag <- model@meanAg@.Data
              exposure.ag <- model@exposureAg
              weight.ag <- model@weightAg
              max.attempt <- model@maxAttempt@.Data
              if (is.null(metadata.ag)) {
                  first <- pos
                  pos <- first + 1L
                  value <- Skeleton(first = first)
                  mean <- mean.ag@.Data
                  exposure <- exposure.ag@.Data
              }
              else {
                  dim.ag <- dim(metadata.ag)
                  dimnames.ag <- dimnames(metadata.ag)
                  first <- pos
                  pos <- first + as.integer(prod(dim.ag))
                  value <- Skeleton(metadata = metadata.ag, first = first)
                  .Data.mean <- array(mean.ag, dim = dim.ag, dimnames = dimnames.ag)
                  mean <- methods::new("Values", .Data = .Data.mean, metadata = metadata.ag)
                  .Data.exposure <- array(exposure.ag, dim = dim.ag, dimnames = dimnames.ag)
                  exposure <- methods::new("Counts", .Data = .Data.exposure, metadata = metadata.ag)
              }
              first <- pos
              pos <- first + 1L
              no.proposal <- SkeletonAccept(nAttempt = max.attempt,
                                            first = first,
                                            nChain = nChain,
                                            nIteration = nIteration)
              first <- pos
              accept.ag <- SkeletonAccept(nAttempt = length(mean.ag),
                                          first = first,
                                          nChain = nChain,
                                          nIteration = nIteration)
              .Data.weights <- array(weight.ag,
                                     dim = dim(metadata.y),
                                     dimnames = dimnames(metadata.y))
              weights <- methods::new("Counts", .Data = .Data.weights, metadata = metadata.y)
              list(value = value,
                   jump = scale.ag,
                   noProposal = no.proposal,
                   accept = accept.ag,
                   mean = mean,
                   exposure = exposure,
                   weights = weights)
          })



## makeOutputModel #######################################################


## Varying

## HAS_TESTS
setMethod("makeOutputModel",
          signature(model = "NormalVarying"),
          function(model, pos, mcmc) {
              theta <- model@theta
              scale.theta <- model@scaleTheta@.Data
              metadata <- model@metadataY
              w <- model@w
              betas.obj <- model@betas
              priors.betas <- model@priorsBetas
              names.betas <- model@namesBetas
              margins <- model@margins
              dims <- model@dims
              n.beta <- length(betas.obj)
              n.attempt <- as.integer(prod(dim(metadata)))
              nChain <- mcmc["nChain"]
              nIteration <- mcmc["nIteration"]
              ## make theta
              first <- pos
              pos <- first + length(theta)
              theta <- Skeleton(metadata = metadata, first = first)
              ## make nFailedPropTheta
              first <- pos
              pos <- first + 1L
              fail.prop.theta <- SkeletonAccept(nAttempt = n.attempt,
                                                first = first,
                                                nChain = nChain,
                                                nIteration = nIteration)
              ## if Aggregate, make nAcceptTheta
              if (methods::is(model, "Aggregate")) {
                  first <- pos
                  pos <- first + 1L
                  accept.theta <- SkeletonAccept(nAttempt = n.attempt,
                                                 first = first,
                                                 nChain = nChain,
                                                 nIteration = nIteration)
              }
              ## make varsigma
              if (methods::is(model, "VarsigmaKnown"))
                  varsigma <- model@varsigma@.Data
              else {
                  first <- pos
                  pos <- first + 1L
                  varsigma <- Skeleton(first = first)
              }
              ## make betas
              betas <- vector(mode = "list", length = n.beta)
              first <- pos
              pos <- pos + 1L
              mu <- SkeletonMu(betas = betas.obj,
                               margins = margins,
                               first = first,
                               metadata = metadata)
              betas[[1L]] <- SkeletonBetaIntercept(betas = betas.obj,
                                                   first = first)
              if (n.beta > 1L) {
                  for (i in seq_len(n.beta)[-1L]) {
                      first <- pos
                      pos <- first + length(betas.obj[[i]])
                      margin <- margins[[i]]
                      i.tail <- seq(from = i, to = n.beta)
                      betas[[i]] <- SkeletonBetaTerm(betas = betas.obj[i.tail],
                                                     margins = margins[i.tail],
                                                     dims = dims[i.tail],
                                                     first = first,
                                                     metadata = metadata[margin])
                  }
              }
              names(betas) <- names.betas
              ## make sigma
              first <- pos
              pos <- first + 1L
              sigma <- Skeleton(first = first)
              ## make hyper
              hyper <- vector(mode = "list", length = n.beta)
              for (i in seq_len(n.beta)) {
                  if (i == 1L)
                      metadata.i <- NULL
                  else {
                      margin <- margins[[i]]
                      metadata.i <- metadata[margin]
                  }
                  hyper[i] <- list(makeOutputPrior(priors.betas[[i]],
                                                   metadata = metadata.i,
                                                   pos = pos))
                  pos <- pos + changeInPos(hyper[[i]])
              }
              names(hyper) <- names.betas
              ## assemble return value
              prior <- c(betas, list(mean = mu), list(sd = sigma))
              if (methods::is(model, "Aggregate")) {
                  likelihood <- list(mean = theta,
                                     jumpMean = scale.theta,
                                     noProposal = fail.prop.theta,
                                     acceptMean = accept.theta,
                                     sd = varsigma,
                                     weights = w)
                  aggregate <- makeOutputAggregate(model = model,
                                                   pos = pos,
                                                   nChain = nChain,
                                                   nIteration = nIteration)
                  ans <- list(likelihood = likelihood,
                              prior = prior,
                              hyper = hyper,
                              aggregate = aggregate)
              }
              else {
                  likelihood <- list(mean = theta,
                                     noProposal = fail.prop.theta,
                                     sd = varsigma,
                                     weights = w)
                  ans <-  list(likelihood = likelihood,
                               prior = prior,
                               hyper = hyper)
              }
              ans
          })

## HAS_TESTS
setMethod("makeOutputModel",
          signature(model = "BinomialVarying"),
          function(model, pos, mcmc) {
              theta <- model@theta
              metadata <- model@metadataY
              scale.theta <- model@scaleTheta@.Data
              betas.obj <- model@betas
              priors.betas <- model@priorsBetas
              names.betas <- model@namesBetas
              margins <- model@margins
              dims <- model@dims
              n.beta <- length(betas.obj)
              n.attempt <- as.integer(prod(dim(metadata)))
              nChain <- mcmc["nChain"]
              nIteration <- mcmc["nIteration"]
              ## make theta
              first <- pos
              pos <- first + length(theta)
              theta <- Skeleton(metadata = metadata, first = first)
              ## make nFailedPropTheta
              first <- pos
              pos <- first + 1L
              fail.prop.theta <- SkeletonAccept(nAttempt = n.attempt,
                                                first = first,
                                                nChain = nChain,
                                                nIteration = nIteration)
              ## make nAcceptTheta
              first <- pos
              pos <- first + 1L
              accept.theta <- SkeletonAccept(nAttempt = n.attempt,
                                             first = first,
                                             nChain = nChain,
                                             nIteration = nIteration)
              ## make betas
              betas <- vector(mode = "list", length = n.beta)
              first <- pos
              pos <- pos + 1L
              mu <- SkeletonMu(betas = betas.obj,
                               margins = margins,
                               first = first,
                               metadata = metadata)
              betas[[1L]] <- SkeletonBetaIntercept(betas = betas.obj,
                                                   first = first)
              if (n.beta > 1L) {
                  for (i in seq_len(n.beta)[-1L]) {
                      first <- pos
                      pos <- first + length(betas.obj[[i]])
                      margin <- margins[[i]]
                      i.tail <- seq(from = i, to = n.beta)
                      betas[[i]] <- SkeletonBetaTerm(betas = betas.obj[i.tail],
                                                     margins = margins[i.tail],
                                                     dims = dims[i.tail],
                                                     first = first,
                                                     metadata = metadata[margin])
                  }
              }
              names(betas) <- names.betas
              ## make sigma
              first <- pos
              pos <- first + 1L
              sigma <- Skeleton(first = first)
              ## make hyper
              hyper <- vector(mode = "list", length = n.beta)
              for (i in seq_len(n.beta)) {
                  if (i == 1L)
                      metadata.i <- NULL
                  else {
                      margin <- margins[[i]]
                      metadata.i <- metadata[margin]
                  }
                  hyper[i] <- list(makeOutputPrior(priors.betas[[i]],
                                                   metadata = metadata.i,
                                                   pos = pos))
                  pos <- pos + changeInPos(hyper[[i]])
              }
              names(hyper) <- names.betas
              ## return value
              likelihood <- list(prob = theta,
                                 jumpProb = scale.theta,
                                 noProposal = fail.prop.theta,
                                 acceptProb = accept.theta)
              prior <- c(betas, list(mean = mu), list(sd = sigma))
              ans <- list(likelihood = likelihood, prior = prior, hyper = hyper)
              if (methods::is(model, "Aggregate")) {
                  aggregate <- makeOutputAggregate(model = model,
                                                   pos = pos,
                                                   nChain = nChain,
                                                   nIteration = nIteration)
                  ans <- c(ans, list(aggregate = aggregate))
              }
              ans
          })

## HAS_TESTS
setMethod("makeOutputModel",
          signature(model = "PoissonVarying"),
          function(model, pos, mcmc) {
              theta <- model@theta
              metadata <- model@metadataY
              scale.theta <- model@scaleTheta@.Data
              betas.obj <- model@betas
              priors.betas <- model@priorsBetas
              names.betas <- model@namesBetas
              margins <- model@margins
              dims <- model@dims
              n.beta <- length(betas.obj)
              n.attempt <- as.integer(prod(dim(metadata)))
              nChain <- mcmc["nChain"]
              nIteration <- mcmc["nIteration"]
              uses.exposure <- methods::is(model, "UseExposure")
              ## make theta
              first <- pos
              pos <- first + length(theta)
              class <- if (uses.exposure) "Values" else "Counts"
              .Data <- array(theta, dim = dim(metadata), dimnames = dimnames(metadata))
              theta <- methods::new(class, .Data = .Data, metadata = metadata)
              theta <- Skeleton(object = theta, first = first)
              ## make nFailedPropTheta
              first <- pos
              pos <- first + 1L
              fail.prop.theta <- SkeletonAccept(nAttempt = n.attempt,
                                                first = first,
                                                nChain = nChain,
                                                nIteration = nIteration)
              ## make nAcceptTheta
              first <- pos
              pos <- first + 1L
              accept.theta <- SkeletonAccept(nAttempt = n.attempt,
                                             first = first,
                                             nChain = nChain,
                                             nIteration = nIteration)
              ## make betas
              betas <- vector(mode = "list", length = n.beta)
              first <- pos
              pos <- pos + 1L
              mu <- SkeletonMu(betas = betas.obj,
                               margins = margins,
                               first = first,
                               metadata = metadata)
              betas[[1L]] <- SkeletonBetaIntercept(betas = betas.obj,
                                                   first = first)
              if (n.beta > 1L) {
                  for (i in seq_len(n.beta)[-1L]) {
                      first <- pos
                      pos <- first + length(betas.obj[[i]])
                      margin <- margins[[i]]
                      i.tail <- seq(from = i, to = n.beta)
                      betas[[i]] <- SkeletonBetaTerm(betas = betas.obj[i.tail],
                                                     margins = margins[i.tail],
                                                     dims = dims[i.tail],
                                                     first = first,
                                                     metadata = metadata[margin])
                  }
              }
              names(betas) <- names.betas
              ## make sigma
              first <- pos
              pos <- first + 1L
              sigma <- Skeleton(first = first)
              ## make hyper
              hyper <- vector(mode = "list", length = n.beta)
              for (i in seq_len(n.beta)) {
                  if (i == 1L)
                      metadata.i <- NULL
                  else {
                      margin <- margins[[i]]
                      metadata.i <- metadata[margin]
                  }
                  hyper[i] <- list(makeOutputPrior(priors.betas[[i]],
                                                   metadata = metadata.i,
                                                   pos = pos))
                  pos <- pos + changeInPos(hyper[[i]])
              }
              names(hyper) <- names.betas
              ## return value
              likelihood <- list(theta,
                                 jumpMean = scale.theta,
                                 noProposal = fail.prop.theta,
                                 acceptMean = accept.theta)
              if (uses.exposure)
                  names <- c("rate", "jumpRate", "noProposal", "acceptRate")
              else
                  names <- c("count", "jumpCount", "noProposal", "acceptCount")
              names(likelihood) <- names
              prior <- c(betas, list(mean = mu), list(sd = sigma))
              ans <- list(likelihood = likelihood, prior = prior, hyper = hyper)
              if (methods::is(model, "Aggregate")) {
                  aggregate <- makeOutputAggregate(model = model,
                                                   pos = pos,
                                                   nChain = nChain,
                                                   nIteration = nIteration)
                  ans <- c(ans, list(aggregate = aggregate))
              }
              ans
          })


## Poisson-binomial mixture

## HAS_TESTS
setMethod("makeOutputModel",
          signature(model = "PoissonBinomialMixture"),
          function(model) {
              prob <- model@prob
              list(prob = prob)
          })


## predictModelNotUseExp ##############################################################

## TRANSLATED
## HAS_TESTS
setMethod("predictModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownPredict",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelNotUseExp_NormalVaryingVarsigmaKnownPredict_R,
                            object, y)
                  else
                      .Call(predictModelNotUseExp_R,
                            object, y)
              }
              else {
                  object <- predictPriorsBetas(object)
                  object <- predictBetas(object)
                  object <- updateTheta_NormalVarying(object, y = y)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownPredict",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelNotUseExp_NormalVaryingVarsigmaUnknownPredict_R,
                            object, y)
                  else
                      .Call(predictModelNotUseExp_R,
                            object, y)
              }
              else {
                  object <- predictPriorsBetas(object)
                  object <- predictBetas(object)
                  object <- updateTheta_NormalVarying(object, y = y)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpPredict",
                    y = "Counts"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelNotUseExp_PoissonVaryingNotUseExpPredict_R,
                            object, y)
                  else
                      .Call(predictModelNotUseExp_R,
                            object, y)
              }
              else {
                  object <- predictPriorsBetas(object)
                  object <- predictBetas(object)
                  object <- updateTheta_PoissonVaryingNotUseExp(object, y = y)
                  object
              }
          })


## predictModelUseExp #################################################################

## TRANSLATED
## HAS_TESTS
setMethod("predictModelUseExp",
          signature(object = "BinomialVaryingPredict",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelUseExp_BinomialVaryingPredict_R,
                            object, y, exposure)
                  else
                      .Call(predictModelUseExp_R,
                            object, y, exposure)
              }
              else {
                  object <- predictPriorsBetas(object)
                  object <- predictBetas(object)
                  object <- updateTheta_BinomialVarying(object, y = y, exposure = exposure)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("predictModelUseExp",
          signature(object = "PoissonVaryingUseExpPredict",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(is.na(y)))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(all(is.na(exposure)))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelUseExp_PoissonVaryingUseExpPredict_R,
                            object, y, exposure)
                  else
                      .Call(predictModelUseExp_R,
                            object, y, exposure)
              }
              else {
                  object <- predictPriorsBetas(object)
                  object <- predictBetas(object)
                  object <- updateTheta_PoissonVaryingUseExp(object, y = y, exposure = exposure)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("predictModelUseExp",
          signature(object = "PoissonBinomialMixturePredict",
                    y = "Counts",
                    exposure = "Counts"),
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
                      .Call(predictModelUseExp_PoissonBinomialMixturePredict_R,
                            object, y, exposure)
                  else
                      .Call(predictModelUseExp_R,
                            object, y, exposure)
              }
              else {
                  object
              }
          })


## printAgAccuracyEqns #####################################################

setMethod("printAgAccuracyEqns",
          signature(object = "AgCertain"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("        value[a] = aggregate[a]\n")
              else
                  cat("           value = aggregate\n")
          })
          
setMethod("printAgAccuracyEqns",
          signature(object = "AgNormal"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("        value[a] ~ N(aggregate[a], sd[a]^2)\n")
              else
                  cat("           value ~ N(aggregate, sd^2)\n")
          })

setMethod("printAgAccuracyEqns",
          signature(object = "AgFun"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("        value[a] ~ N(aggregate[a], sd[a]^2)\n")
              else
                  cat("           value ~ N(aggregate, sd^2)\n")
          })

setMethod("printAgAccuracyEqns",
          signature(object = "AgLife"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("        value[a] ~ N(aggregate[a], sd[a]^2)\n")
              else
                  cat("           value ~ N(aggregate, sd^2)\n")
          })

setMethod("printAgAccuracyEqns",
          signature(object = "AgPoisson"),
          function(object) {
              value <- object@valueAg
              n.value <- length(value)
              cat("\n")
              if (n.value > 1L)
                  cat("exposure[a] * value[a] ~ Poisson(exposure[a] * aggregate[a])\n")
              else
                  cat("exposure * value ~ Poisson(aggregate * value)\n")
          })


## printAgValEqns ##############################################################

setMethod("printAgValEqns",
          signature(object = "NormalVaryingVarsigmaKnown"),
          function(object) {
              cat("\n")
              cat("       aggregate = sum(mean * weight)")
          })

setMethod("printAgValEqns",
          signature(object = "NormalVaryingVarsigmaKnown"),
          function(object) {
              cat("\n")
              cat("       aggregate = sum(mean * weight)")
          })

setMethod("printAgValEqns",
          signature(object = "NormalVaryingVarsigmaKnownAgFun"),
          function(object) {
              cat("\n")
              cat("       aggregate = f(mean, weight)")
          })


setMethod("printAgValEqns",
          signature(object = "NormalVaryingVarsigmaUnknown"),
          function(object) {
              cat("\n")
              cat("       aggregate = sum(mean * weight)")
          })

setMethod("printAgValEqns",
          signature(object = "NormalVaryingVarsigmaUnknownAgFun"),
          function(object) {
              cat("\n")
              cat("       aggregate = f(mean, weight)")
          })

setMethod("printAgValEqns",
          signature(object = "BinomialVarying"),
          function(object) {
              cat("\n")
              cat("       aggregate = sum(prob * weight)")
          })

setMethod("printAgValEqns",
          signature(object = "BinomialVaryingAgFun"),
          function(object) {
              cat("\n")
              cat("       aggregate = f(prob, weight)")
          })

setMethod("printAgValEqns",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object) {
              cat("\n")
              cat("      aggregate = sum(count * weight)")
          })

setMethod("printAgValEqns",
          signature(object = "PoissonVaryingNotUseExpAgFun"),
          function(object) {
              cat("\n")
              cat("      aggregate = f(count, weight)")
          })

setMethod("printAgValEqns",
          signature(object = "PoissonVaryingUseExp"),
          function(object) {
              cat("\n")
              cat("       aggregate = sum(rate * weight)")
          })

setMethod("printAgValEqns",
          signature(object = "PoissonVaryingUseExpAgFun"),
          function(object) {
              cat("\n")
              cat("       aggregate = f(rate, weight)")
          })

setMethod("printAgValEqns",
          signature(object = "PoissonVaryingUseExpAgLife"),
          function(object) {
              cat("\n")
              cat("         rate.ag = sum(rate * weight)\n")
              cat("       aggregate = LifeExp(rate.ag)\n")
          })



## showModelHelper #############################################################################
    

setMethod("showModelHelper",
          signature(object = "BinomialVarying"),
          function(object) {
              printBinomialModEqns(object)
              cat("\n")
              printPriorsEqns(object)
              cat("\n")
              printSDEqns(object)
              printAggregateEqns(object)
          })

setMethod("showModelHelper",
          signature(object = "NormalVaryingVarsigmaKnown"),
          function(object) {
              printNormalVarsigmaKnownModEqns(object)
              cat("\n")
              printPriorsEqns(object)
              cat("\n")
              printSDEqns(object)
              printAggregateEqns(object)
          })

setMethod("showModelHelper",
          signature(object = "NormalVaryingVarsigmaKnown"),
          function(object) {
              printNormalVarsigmaUnknownModEqns(object)
              cat("\n")
              printPriorsEqns(object)
              cat("\n")
              printSDEqns(object)
              printAggregateEqns(object)
          })

setMethod("showModelHelper",
          signature(object = "PoissonVarying"),
          function(object) {
              printPoissonModEqns(object)
              cat("\n")
              printPriorsEqns(object)
              cat("\n")
              printSDEqns(object)
              printAggregateEqns(object)
          })

setMethod("show",
          signature(object = "SpecPoissonBinomialMixture"),
          function(object) {
              printPoissonBinomialSpecEqns(object)
          })


## transferParamModel ################################################################

## TRANSLATED
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "NormalVaryingVarsigmaKnownPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_NormalVaryingVarsigmaKnownPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  model <- transferParamBetas(model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- transferParamPriorsBetas(model,
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

## TRANSLATED
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "NormalVaryingVarsigmaUnknownPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_NormalVaryingVarsigmaUnknownPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  model <- transferParamBetas(model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- transferParamPriorsBetas(model,
                                                    filename = filename,
                                                    lengthIter = lengthIter,
                                                    iteration = iteration)
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

## TRANSLATED
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "PoissonVaryingNotUseExpPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_PoissonVaryingNotUseExpPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  model <- transferParamBetas(model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- transferParamPriorsBetas(model,
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

## TRANSLATED
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "BinomialVaryingPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_BinomialVaryingPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  model <- transferParamBetas(model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  ## model <- transferParamZetas(model,
                  ##                             filename = filename,
                  ##                             lengthIter = lengthIter,
                  ##                             iteration = iteration)
                  model <- transferParamPriorsBetas(model,
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

## TRANSLATED
## HAS_TESTS
setMethod("transferParamModel",
          signature(model = "PoissonVaryingUseExpPredict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_PoissonVaryingUseExpPredict_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  model <- transferParamBetas(model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model <- transferParamPriorsBetas(model,
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



## updateModelNotUseExp ##############################################################

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnown",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalVaryingVarsigmaKnown_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVarying(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknown",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalVaryingVarsigmaUnknown_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVarying(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExp",
                    y = "Counts"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y[!is.na(y)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_PoissonVaryingNotUseExp_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgCertain",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgCertain_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgCertain",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgCertain_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgCertain",
                    y = "Counts"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y[!is.na(y)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_PoissonVaryingNotUseExpAgCertain_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgNormal",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgNormal_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_Normal(object = object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgNormal",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgNormal_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateTheta_NormalVaryingAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_Normal(object = object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaKnownAgFun",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalVaryingVarsigmaKnownAgFun_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateThetaAndValueAgFun_Normal(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "NormalVaryingVarsigmaUnknownAgFun",
                    y = "DemographicArray"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.double(y))
              stopifnot(identical(length(y), length(object@theta)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_NormalVaryingVarsigmaUnknownAgFun_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  identity <- function(x) x
                  object <- updateThetaAndValueAgFun_Normal(object, y = y)
                  object <- updateVarsigma(object, y = y)
                  object <- updateSigma_Varying(object, g = identity)
                  object <- updateBetasAndPriorsBetas(object, g = identity)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgNormal",
                    y = "Counts"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y[!is.na(y)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_PoissonVaryingNotUseExpAgNormal_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateThetaAndValueAgNormal_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgFun",
                    y = "Counts"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y[!is.na(y)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_PoissonVaryingNotUseExpAgFun_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateThetaAndValueAgFun_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelNotUseExp",
          signature(object = "PoissonVaryingNotUseExpAgPoisson",
                    y = "Counts"),
          function(object, y, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(all(y[!is.na(y)] >= 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelNotUseExp_PoissonVaryingNotUseExpAgPoisson_R, object, y)
                  else
                      .Call(updateModelNotUseExp_R, object, y)
              }
              else {
                  object <- updateTheta_PoissonVaryingNotUseExpAgCertain(object, y = y)
                  object <- updateThetaAndValueAgPoisson_PoissonNotUseExp(object, y = y)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })



## updateModelUseExp #################################################################

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "BinomialVarying",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y[!is.na(y)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_BinomialVarying_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateTheta_BinomialVarying(object,
                                                        y = y,
                                                        exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "PoissonVaryingUseExp",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y[!is.na(y)][exposure[!is.na(y)] == 0] == 0))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_PoissonVarying_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "PoissonBinomialMixture",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_PoissonBinomialMixture_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  ## object is not updated
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "BinomialVaryingAgCertain",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y[!is.na(y)] <= exposure[is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_BinomialVaryingAgCertain_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateTheta_BinomialVaryingAgCertain(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "PoissonVaryingUseExpAgCertain",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_PoissonVaryingUseExpAgCertain_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "BinomialVaryingAgNormal",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0L))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y[!is.na(y)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_BinomialVaryingAgNormal_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateTheta_BinomialVaryingAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgNormal_Binomial(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "BinomialVaryingAgFun",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0L))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y[!is.na(y)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_BinomialVaryingAgFun_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  logit <- function(x) log(x / (1 - x))
                  object <- updateThetaAndValueAgFun_Binomial(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = logit)
                  object <- updateBetasAndPriorsBetas(object, g = logit)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "PoissonVaryingUseExpAgNormal",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_PoissonVaryingUseExpAgNormal_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgNormal_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "PoissonVaryingUseExpAgFun",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_PoissonVaryingUseExpAgFun_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateThetaAndValueAgFun_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })

## READY_TO_TRANSLATE
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "PoissonVaryingUseExpAgLife",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_PoissonVaryingUseExpAgLife_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateThetaAndValueAgLife_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })


## TRANSLATED
## HAS_TESTS
setMethod("updateModelUseExp",
          signature(object = "PoissonVaryingUseExpAgPoisson",
                    y = "Counts",
                    exposure = "Counts"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
              stopifnot(is.integer(y))
              stopifnot(all(y[!is.na(y)] >= 0L))
              ## exposure
              stopifnot(is.double(exposure))
              stopifnot(!any(is.na(exposure)))
              stopifnot(all(exposure[!is.na(exposure)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_PoissonVaryingUseExpAgPoisson_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateTheta_PoissonVaryingUseExpAgCertain(object, y = y, exposure = exposure)
                  object <- updateThetaAndValueAgPoisson_PoissonUseExp(object, y = y, exposure = exposure)
                  object <- updateSigma_Varying(object, g = log)
                  object <- updateBetasAndPriorsBetas(object, g = log)
                  object
              }
          })



## whereAcceptance ###################################################################

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "NormalVarying"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "NormalVaryingVarsigmaKnownAgCertain"),
          function(object) list(c("likelihood", "acceptMean")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "NormalVaryingVarsigmaUnknownAgCertain"),
          function(object) list(c("likelihood", "acceptMean")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "NormalVaryingVarsigmaKnownAgNormal"),
          function(object) list(c("likelihood", "acceptMean"),
                                c("aggregate", "accept")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "NormalVaryingVarsigmaUnknownAgNormal"),
          function(object) list(c("likelihood", "acceptMean"),
                                c("aggregate", "accept")))
          
## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "BinomialVarying"),
          function(object) list(c("likelihood", "acceptProb")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "BinomialVaryingAgNormal"),
          function(object) list(c("likelihood", "acceptProb"),
                                c("aggregate", "accept")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object) list(c("likelihood", "acceptCount")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "PoissonVaryingUseExp"),
          function(object) list(c("likelihood", "acceptRate")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "PoissonVaryingNotUseExpAgNormal"),
          function(object) list(c("likelihood", "acceptCount"),
                                c("aggregate", "accept")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "PoissonVaryingUseExpAgNormal"),
          function(object) list(c("likelihood", "acceptRate"),
                                c("aggregate", "accept")))


## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "PoissonVaryingNotUseExpAgPoisson"),
          function(object) list(c("likelihood", "acceptCount"),
                                c("aggregate", "accept")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "PoissonVaryingUseExpAgPoisson"),
          function(object) list(c("likelihood", "acceptRate"),
                                c("aggregate", "accept")))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "PoissonBinomialMixture"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "BinomialVaryingPredict"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereAcceptance",
          signature(object = "PoissonVaryingUseExpPredict"),
          function(object) list(NULL))



## whereAutocorr #####################################################################

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "NormalVarying"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "NormalVaryingVarsigmaKnownAgCertain"),
          function(object) list(c("likelihood", "mean")))
          
## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "NormalVaryingVarsigmaUnknownAgCertain"),
          function(object) list(c("likelihood", "mean")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "NormalVaryingVarsigmaKnownAgNormal"),
          function(object) list(c("likelihood", "mean"),
                                c("aggregate", "value")))
          
## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "NormalVaryingVarsigmaUnknownAgNormal"),
          function(object) list(c("likelihood", "mean"),
                                c("aggregate", "value")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "BinomialVarying"),
          function(object) list(c("likelihood", "prob")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "BinomialVaryingAgNormal"),
          function(object) list(c("likelihood", "prob"),
                                c("aggregate", "value")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object) list(c("likelihood", "count")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "PoissonVaryingUseExp"),
          function(object) list(c("likelihood", "rate")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "PoissonVaryingNotUseExpAgNormal"),
          function(object) list(c("likelihood", "count"),
                                c("aggregate", "value")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "PoissonVaryingUseExpAgNormal"),
          function(object) list(c("likelihood", "rate"),
                                c("aggregate", "value")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "PoissonVaryingNotUseExpAgPoisson"),
          function(object) list(c("likelihood", "count"),
                                c("aggregate", "value")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "PoissonVaryingUseExpAgPoisson"),
          function(object) list(c("likelihood", "rate"),
                                c("aggregate", "value")))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "PoissonBinomialMixture"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "BinomialVaryingPredict"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereAutocorr",
          signature(object = "PoissonVaryingUseExpPredict"),
          function(object) list(NULL))


## whereJump #########################################################################

## HAS_TESTS
setMethod("whereJump",
          signature(object = "NormalVarying"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereJump",
          signature(object = "NormalVaryingVarsigmaKnownAgCertain"),
          function(object) list(c("likelihood", "jumpMean")))
          
## HAS_TESTS
setMethod("whereJump",
          signature(object = "NormalVaryingVarsigmaUnknownAgCertain"),
          function(object) list(c("likelihood", "jumpMean")))

## HAS_TESTS
setMethod("whereJump",
          signature(object = "NormalVaryingVarsigmaKnownAgNormal"),
          function(object) list(c("likelihood", "jumpMean"),
                                c("aggregate", "jump")))
          
## HAS_TESTS
setMethod("whereJump",
          signature(object = "NormalVaryingVarsigmaUnknownAgNormal"),
          function(object) list(c("likelihood", "jumpMean"),
                                c("aggregate", "jump")))

## HAS_TESTS
setMethod("whereJump",
          signature(object = "BinomialVarying"),
          function(object) list(c("likelihood", "jumpProb")))

## HAS_TESTS
setMethod("whereJump",
          signature(object = "BinomialVaryingAgNormal"),
          function(object) {
              list(c("likelihood", "jumpProb"),
                   c("aggregate", "jump"))
          })

## HAS_TESTS
setMethod("whereJump",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object) list(c("likelihood", "jumpCount")))

## HAS_TESTS
setMethod("whereJump",
          signature(object = "PoissonVaryingUseExp"),
          function(object) list(c("likelihood", "jumpRate")))


## HAS_TESTS
setMethod("whereJump",
          signature(object = "PoissonVaryingNotUseExpAgNormal"),
          function(object) {
              list(c("likelihood", "jumpCount"),
                   c("aggregate", "jump"))
          })

## HAS_TESTS
setMethod("whereJump",
          signature(object = "PoissonVaryingUseExpAgNormal"),
          function(object) {
              list(c("likelihood", "jumpRate"),
                   c("aggregate", "jump"))
          })

## HAS_TESTS
setMethod("whereJump",
          signature(object = "PoissonVaryingNotUseExpAgPoisson"),
          function(object) {
              list(c("likelihood", "jumpCount"),
                   c("aggregate", "jump"))
          })

## HAS_TESTS
setMethod("whereJump",
          signature(object = "PoissonVaryingUseExpAgPoisson"),
          function(object) {
              list(c("likelihood", "jumpRate"),
                   c("aggregate", "jump"))
          })

## HAS_TESTS
setMethod("whereJump",
          signature(object = "PoissonBinomialMixture"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereJump",
          signature(object = "BinomialVaryingPredict"),
          function(object) list(NULL))

## HAS_TESTS
setMethod("whereJump",
          signature(object = "PoissonVaryingUseExpPredict"),
          function(object) list(NULL))


## whereEstimated ####################################################################

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "NormalVaryingVarsigmaKnown"),
          function(object) {
              names.betas <- object@namesBetas
              priors.betas <- object@priorsBetas
              likelihood <- list(c("likelihood", "mean"))
              prior <- c(names.betas, "sd")
              prior <- lapply(prior, function(name) c("prior", name))
              hyper <- makeMCMCPriorsBetas(priors = priors.betas, names = names.betas)
              c(likelihood, prior, hyper)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "NormalVaryingVarsigmaUnknown"),
          function(object) {
              names.betas <- object@namesBetas
              priors.betas <- object@priorsBetas
              likelihood <- list(c("likelihood", "mean"),
                                 c("likelihood", "sd"))
              prior <- c(names.betas, "sd")
              prior <- lapply(prior, function(name) c("prior", name))
              hyper <- makeMCMCPriorsBetas(priors = priors.betas, names = names.betas)
              c(likelihood, prior, hyper)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "NormalVaryingVarsigmaKnownAgNormal"),
          function(object) {
              ans <- methods::callNextMethod()
              aggregate <- list(c("aggregate", "value"))
              c(ans, aggregate)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "NormalVaryingVarsigmaUnknownAgNormal"),
          function(object) {
              ans <- methods::callNextMethod()
              aggregate <- list(c("aggregate", "value"))
              c(ans, aggregate)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "BinomialVarying"),
          function(object) {
              names.betas <- object@namesBetas
              priors.betas <- object@priorsBetas
              likelihood <- list(c("likelihood", "prob"))
              prior <- c(names.betas, "sd")
              prior <- lapply(prior, function(name) c("prior", name))
              hyper <- makeMCMCPriorsBetas(priors = priors.betas, names = names.betas)
              c(likelihood, prior, hyper)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "BinomialVaryingAgNormal"),
          function(object) {
              ans <- methods::callNextMethod()
              aggregate <- list(c("aggregate", "value"))
              c(ans, aggregate)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object) {
              names.betas <- object@namesBetas
              priors.betas <- object@priorsBetas
              likelihood <- list(c("likelihood", "count"))
              prior <- c(names.betas, "sd")
              prior <- lapply(prior, function(name) c("prior", name))
              hyper <- makeMCMCPriorsBetas(priors = priors.betas, names = names.betas)
              c(likelihood, prior, hyper)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "PoissonVaryingUseExp"),
          function(object) {
              names.betas <- object@namesBetas
              priors.betas <- object@priorsBetas
              likelihood <- list(c("likelihood", "rate"))
              prior <- c(names.betas, "sd")
              prior <- lapply(prior, function(name) c("prior", name))
              hyper <- makeMCMCPriorsBetas(priors = priors.betas, names = names.betas)
              c(likelihood, prior, hyper)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "PoissonVaryingNotUseExpAgNormal"),
          function(object) {
              ans <- methods::callNextMethod()
              aggregate <- list(c("aggregate", "value"))
              c(ans, aggregate)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "PoissonVaryingNotUseExpAgPoisson"),
          function(object) {
              ans <- methods::callNextMethod()
              aggregate <- list(c("aggregate", "value"))
              c(ans, aggregate)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "PoissonVaryingUseExpAgNormal"),
          function(object) {
              ans <- methods::callNextMethod()
              aggregate <- list(c("aggregate", "value"))
              c(ans, aggregate)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "PoissonVaryingUseExpAgPoisson"),
          function(object) {
              ans <- methods::callNextMethod()
              aggregate <- list(c("aggregate", "value"))
              c(ans, aggregate)
          })

## HAS_TESTS
setMethod("whereEstimated",
          signature(object = "PoissonBinomialMixture"),
          function(object) {
              list(NULL)
          })

## whereNoProposal ###################################################################

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "BinomialVarying"),
          function(object) {
              lower <- object@lower
              upper <- object@upper
              if (is.finite(lower) || is.finite(upper))
                  list(c("likelihood", "noProposal"))
              else
                  list(NULL)
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "BinomialVaryingAgCertain"),
          function(object) {
              list(c("likelihood", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "BinomialVaryingAgNormal"),
          function(object) {
              list(c("likelihood", "noProposal"),
                   c("aggregate", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "PoissonVarying"),
          function(object) {
              lower <- object@lower
              upper <- object@upper
              if (is.finite(lower) || is.finite(upper))
                  list(c("likelihood", "noProposal"))
              else
                  list(NULL)
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "PoissonVaryingUseExpAgCertain"),
          function(object) {
              list(c("likelihood", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "PoissonVaryingUseExpAgNormal"),
          function(object) {
              list(c("likelihood", "noProposal"),
                   c("aggregate", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "PoissonVaryingUseExpAgPoisson"),
          function(object) {
              list(c("likelihood", "noProposal"),
                   c("aggregate", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "PoissonVaryingNotUseExpAgCertain"),
          function(object) {
              list(c("likelihood", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "PoissonVaryingNotUseExpAgNormal"),
          function(object) {
              list(c("likelihood", "noProposal"),
                   c("aggregate", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "PoissonVaryingNotUseExpAgPoisson"),
          function(object) {
              list(c("likelihood", "noProposal"),
                   c("aggregate", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "NormalVarying"),
          function(object) {
              lower <- object@lower
              upper <- object@upper
              if (is.finite(lower) || is.finite(upper))
                  list(c("likelihood", "noProposal"))
              else
                  list(NULL)
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "NormalVaryingVarsigmaKnownAgCertain"),
          function(object) {
              list(c("likelihood", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "NormalVaryingVarsigmaUnknownAgCertain"),
          function(object) {
              list(c("likelihood", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "NormalVaryingVarsigmaKnownAgNormal"),
          function(object) {
              list(c("likelihood", "noProposal"),
                   c("aggregate", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "NormalVaryingVarsigmaUnknownAgNormal"),
          function(object) {
              list(c("likelihood", "noProposal"),
                   c("aggregate", "noProposal"))
          })

## HAS_TESTS
setMethod("whereNoProposal",
          signature(object = "PoissonBinomialMixture"),
          function(object) list(NULL))


## whereTheta #########################################################################

## HAS_TESTS
setMethod("whereTheta",
          signature(object = "Normal"),
          function(object) c("likelihood", "mean"))

## HAS_TESTS
setMethod("whereTheta",
          signature(object = "PoissonVaryingNotUseExp"),
          function(object) c("likelihood", "count"))

## HAS_TESTS
setMethod("whereTheta",
          signature(object = "PoissonVaryingUseExp"),
          function(object) c("likelihood", "rate"))

## HAS_TESTS
setMethod("whereTheta",
          signature(object = "Binomial"),
          function(object) c("likelihood", "prob"))

## HAS_TESTS
setMethod("whereTheta",
          signature(object = "PoissonBinomialMixture"),
          function(object) {
              stop(gettextf("'%s' has class \"%s\"",
                            "object", class(object)))
          })
