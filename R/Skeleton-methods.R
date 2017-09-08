

## classY ###############################################################################

## HAS_TESTS
setMethod("classY",
          signature(y = "SkeletonManyCounts"),
          function(y) "Counts")

## HAS_TESTS
setMethod("classY",
          signature(y = "SkeletonManyValues"),
          function(y) "Values")


## fetchResults ######################################################################


## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonOneCounts"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              last <- first
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              metadata <- methods::new("MetaData",
                              nms = "iteration",
                              dimtypes = "iteration",
                              DimScales = list(methods::new("Iterations", dimvalues = iterations)))
              .Data <- getDataFromFile(filename = filename,
                                       first = first,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonOneValues"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              last <- first
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              metadata <- methods::new("MetaData",
                              nms = "iteration",
                              dimtypes = "iteration",
                              DimScales = list(methods::new("Iterations", dimvalues = iterations)))
              .Data <- getDataFromFile(filename = filename,
                                       first = first,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Values", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonManyCounts"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              last <- object@last
              metadata <- object@metadata
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
              .Data <- getDataFromFile(filename = filename,
                                       first = first,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonManyValues"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              last <- object@last
              metadata <- object@metadata
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
              .Data <- getDataFromFile(filename = filename,
                                       first = first,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Values", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonBetaIntercept"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              offsets.higher <- object@offsetsHigher
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              n.iter <- length(iterations)
              metadata <- methods::new("MetaData",
                              nms = "iteration",
                              dimtypes = "iteration",
                              DimScales = list(methods::new("Iterations",
                                  dimvalues = iterations)))
              .Data <- getDataFromFile(filename = filename,
                                       first = first,
                                       last = first,
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              if (shift) {
                  n <- length(offsets.higher)
                  for (i in seq_len(n)) {
                      offsets.high <- offsets.higher[[i]]
                      high <- getDataFromFile(filename = filename,
                                              first = offsets.high[1L],
                                              last = offsets.high[2L],
                                              lengthIter = lengthIter,
                                              iterations = iterations)
                      high <- matrix(high, ncol = n.iter)
                      high <- colMeans(high)
                      .Data <- .Data + high
                  }
              }
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              methods::new("Values",
                           .Data = .Data,
                           metadata = metadata)
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonBetaTerm"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              last <- object@last
              metadata <- object@metadata
              offsets.higher <- object@offsetsHigher
              transforms.higher <- object@transformsHigher
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              n.iter <- length(iterations)
              length.slice <- prod(dim(metadata))
              metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
              .Data <- getDataFromFile(filename = filename,
                                       first = first,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              if (shift) {
                  n <- length(offsets.higher)
                  for (i in seq_len(n)) {
                      offsets.high <- offsets.higher[[i]]
                      transform.high <- transforms.higher[[i]]
                      transform.high <- addIterationsToTransform(transform.high,
                                                                 nIter = n.iter)
                      dim.high <- transform.high@dimBefore
                      high <- getDataFromFile(filename = filename,
                                              first = offsets.high[1L],
                                              last = offsets.high[2L],
                                              lengthIter = lengthIter,
                                              iterations = iterations)
                      high <- array(high, dim = dim.high)
                      high <- dembase::collapse(high, transform = transform.high)
                      n <- prod(transform.high@dimBefore) / prod(transform.high@dimAfter)
                      high <- high / n 
                      .Data <- .Data + high
                  }
              }
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              methods::new("Values",
                           .Data = .Data,
                           metadata = metadata)
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMu"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              metadata <- object@metadata
              margins <- object@margins
              offsets <- object@offsets
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              n.iter <- length(iterations)
              metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
              n.dim <- length(dim(metadata))
              betas <- vector(mode = "list", length = length(margins))
              .Data.ans <- array(0, dim = dim(metadata), dimnames = dimnames(metadata))
              ans <- methods::new("Values", .Data = .Data.ans, metadata = metadata)
              for (i in seq_along(margins)) {
                  ## get unadjusted beta estimates
                  first <- offsets[[i]][1L]
                  last <- offsets[[i]][2L]
                  .Data.beta <- getDataFromFile(filename = filename,
                                                first = first,
                                                last = last,
                                                lengthIter = lengthIter,
                                                iterations = iterations)
                  ## make Values object
                  if (identical(margins[[i]], 0L))
                      margin <- n.dim
                  else
                      margin <- c(margins[[i]], n.dim)
                  metadata.beta <- metadata[margin]
                  .Data.beta <- array(.Data.beta,
                                      dim = dim(metadata.beta),
                                      dimnames = dimnames(metadata.beta))
                  beta <- methods::new("Values",
                                       .Data = .Data.beta,
                                       metadata = metadata.beta)
                  ans <- ans + beta
              }
              ans
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonCovariates"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              last <- object@last
              metadata <- object@metadata
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              length.slice <- prod(dim(metadata))
              metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
              first.skip.intercept <- first + 1L
              .Data <- getDataFromFile(filename = filename,
                                       first = first.skip.intercept,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Values", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonStateDLM"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              last <- object@last
              metadata <- object@metadata
              indices <- object@indicesShow
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              n.iter <- length(iterations)
              metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
              .Data <- getDataFromFile(filename = filename,
                                       first = first,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              .Data <- matrix(.Data, ncol = n.iter)
              .Data <- .Data[indices, ]
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Values", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonAccept"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              i.first.in.chain <- object@iFirstInChain
              last <- first
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              iterations <- setdiff(iterations, i.first.in.chain)
              ans <- getDataFromFile(filename = filename,
                                     first = first,
                                     last = last,
                                     lengthIter = lengthIter,
                                     iterations = iterations)
              ans <- as.logical(ans)
              ans
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonNAccept"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              first <- object@first
              i.first.in.chain <- object@iFirstInChain
              n.attempt <- object@nAttempt
              last <- first
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              iterations <- setdiff(iterations, i.first.in.chain)
              n.accept <- getDataFromFile(filename = filename,
                                          first = first,
                                          last = last,
                                          lengthIter = lengthIter,
                                          iterations = iterations)
              n.accept / n.attempt
          })



## DemographicArray objects with missing data

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataNormalVarsigmaKnown"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  w <- object@w
                  offsets.theta <- object@offsetsTheta
                  varsigma <- object@varsigma
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  mean <- getDataFromFile(filename = filename,
                                          first = offsets.theta[1L],
                                          last = offsets.theta[2L],
                                          lengthIter = lengthIter,
                                          iterations = iterations)
                  is.missing <- is.na(.Data)
                  n.missing <- sum(is.missing)
                  .Data[is.missing] <- stats::rnorm(n = n.missing,
                                             mean = mean[is.missing],
                                             sd = varsigma)
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataNormalVarsigmaUnknown"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  w <- object@w
                  offsets.theta <- object@offsetsTheta
                  offsets.varsigma <- object@offsetsVarsigma
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  mean <- getDataFromFile(filename = filename,
                                          first = offsets.theta[1L],
                                          last = offsets.theta[2L],
                                          lengthIter = lengthIter,
                                          iterations = iterations)
                  sd <- getDataFromFile(filename = filename,
                                        first = offsets.varsigma[1L],
                                        last = offsets.varsigma[2L],
                                        lengthIter = lengthIter,
                                        iterations = iterations)
                  sd <- rep(sd, each = length(data))
                  is.missing <- is.na(.Data)
                  n.missing <- sum(is.missing)
                  .Data[is.missing] <- stats::rnorm(n = n.missing,
                                             mean = mean[is.missing],
                                             sd = sd[is.missing])
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataPoissonNotUseExp"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  offsets <- object@offsetsTheta
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets[1L],
                                           last = offsets[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  is.missing <- is.na(.Data)
                  n.missing <- sum(is.missing)
                  .Data[is.missing] <- stats::rpois(n = n.missing, lambda = theta[is.missing])
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataPoissonNotUseExpSubtotals"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  offsets <- object@offsetsTheta
                  subtotals <- data@subtotals
                  transform <- data@transformSubtotals
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  n.data <- length(data)
                  n.iter <- length(iterations)
                  .Data <- data@.Data
                  .Data <- matrix(.Data, nrow = n.data, ncol = n.iter)
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets[1L],
                                           last = offsets[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  theta <- matrix(theta, ncol = n.iter)
                  s <- seq_len(n.iter)
                  for (i.sub in seq_along(subtotals)) {
                      i.obj <- dembase::getIBefore(i.sub, transform = transform, useC = TRUE)
                      size <- subtotals[i.sub]
                      theta.sub <- theta[i.obj, , drop = FALSE]
                      FUN <- function(i) stats::rmultinom(n = 1L, size = size, prob = theta.sub[, i])
                      FUN.VALUE <- integer(length = nrow(theta.sub))
                      .Data[i.obj, ] <- vapply(s, FUN = FUN, FUN.VALUE = FUN.VALUE)
                  }
                  is.missing.out.sub <- is.na(.Data)
                  n.missing.out.sub <- sum(is.missing.out.sub)
                  if (n.missing.out.sub > 0L)
                      .Data[is.missing.out.sub] <- stats::rpois(n = n.missing.out.sub,
                                                         lambda = theta[is.missing.out.sub])
                  .Data <- array(.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataPoissonUseExp"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  exposure <- object@exposure
                  offsets <- object@offsetsTheta
                  exposure <- as.numeric(exposure)
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets[1L],
                                           last = offsets[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  lambda <- exposure * theta
                  is.missing <- is.na(.Data)
                  n.missing <- sum(is.missing)
                  .Data[is.missing] <- stats::rpois(n = n.missing, lambda = lambda[is.missing])
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataPoissonUseExpSubtotals"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  exposure <- object@exposure
                  offsets <- object@offsetsTheta
                  subtotals <- data@subtotals
                  transform <- data@transformSubtotals
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  exposure <- as.numeric(exposure)
                  n.data <- length(data)
                  n.iter <- length(iterations)
                  .Data <- data@.Data
                  .Data <- matrix(.Data, nrow = n.data, ncol = n.iter)
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets[1L],
                                           last = offsets[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  theta <- matrix(theta, ncol = n.iter)
                  lambda <- theta * exposure
                  s <- seq_len(n.iter)
                  for (i.sub in seq_along(subtotals)) {
                      i.obj <- dembase::getIBefore(i.sub, transform = transform, useC = TRUE)
                      size <- subtotals[i.sub]
                      lambda.sub <- lambda[i.obj, , drop = FALSE]
                      FUN <- function(i) stats::rmultinom(n = 1L, size = size, prob = lambda.sub[, i])
                      FUN.VALUE <- integer(length = nrow(lambda.sub))
                      .Data[i.obj, ] <- vapply(s, FUN = FUN, FUN.VALUE = FUN.VALUE)
                  }
                  is.missing.out.sub <- is.na(.Data)
                  n.missing.out.sub <- sum(is.missing.out.sub)
                  if (n.missing.out.sub > 0L)
                      .Data[is.missing.out.sub] <- stats::rpois(n = n.missing.out.sub,
                                                         lambda = lambda[is.missing.out.sub])
                  .Data <- array(.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataBinomial"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  exposure <- object@exposure
                  offsets <- object@offsetsTheta
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  exposure <- as.integer(exposure)
                  exposure <- rep(exposure, times = length(iterations))
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets[1L],
                                           last = offsets[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  is.missing <- is.na(.Data)
                  n.missing <- sum(is.missing)
                  .Data[is.missing] <- stats::rbinom(n = n.missing,
                                              size = exposure[is.missing],
                                              prob = theta[is.missing])
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDatasetPoisson"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  offsets.theta <- object@offsetsTheta
                  offsets.component <- object@offsetsComponent
                  transform <- object@transformComponent
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  n.iter <- length(iterations)
                  transform <- addIterationsToTransform(transform, nIter = n.iter)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets.theta[1L],
                                           last = offsets.theta[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  exposure <- getDataFromFile(filename = filename,
                                              first = offsets.component[1L],
                                              last = offsets.component[2L],
                                              lengthIter = lengthIter,
                                              iterations = iterations)
                  exposure <- array(exposure, dim = transform@dimBefore)
                  exposure <- dembase::collapse(exposure, transform = transform)
                  lambda <- theta * exposure
                  is.missing <- is.na(.Data)
                  n.missing <- sum(is.missing)
                  .Data[is.missing] <- stats::rpois(n = n.missing, lambda = lambda[is.missing])
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDatasetPoissonSubtotals"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  subtotals <- data@subtotals
                  transform.subtotals <- data@transformSubtotals
                  offsets.theta <- object@offsetsTheta
                  offsets.component <- object@offsetsComponent
                  transform.component <- object@transformComponent
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  n.data <- length(data)
                  n.iter <- length(iterations)
                  transform.component <- addIterationsToTransform(transform.component, nIter = n.iter)
                  .Data <- data@.Data
                  .Data <- matrix(.Data, nrow = n.data, ncol = n.iter)
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets.theta[1L],
                                           last = offsets.theta[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  exposure <- getDataFromFile(filename = filename,
                                              first = offsets.component[1L],
                                              last = offsets.component[2L],
                                              lengthIter = lengthIter,
                                              iterations = iterations)
                  exposure <- array(exposure, dim = transform.component@dimBefore)
                  exposure <- dembase::collapse(exposure, transform = transform.component)
                  exposure <- matrix(exposure, nrow = n.data, ncol = n.iter)
                  lambda <- theta * exposure
                  s <- seq_len(n.iter)
                  for (i.sub in seq_along(subtotals)) {
                      i.obj <- dembase::getIBefore(i.sub, transform = transform.subtotals, useC = TRUE)
                      size <- subtotals[i.sub]
                      lambda.sub <- lambda[i.obj, , drop = FALSE]
                      FUN <- function(i) stats::rmultinom(n = 1L, size = size, prob = lambda.sub[, i])
                      FUN.VALUE <- integer(length = nrow(lambda.sub))
                      .Data[i.obj, ] <- vapply(s, FUN = FUN, FUN.VALUE = FUN.VALUE)
                  }
                  is.missing.out.sub <- is.na(.Data)
                  n.missing.out.sub <- sum(is.missing.out.sub)
                  if (n.missing.out.sub > 0L)
                      .Data[is.missing.out.sub] <- stats::rpois(n = n.missing.out.sub,
                                                         lambda = lambda[is.missing.out.sub])
                  .Data <- array(.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDatasetBinomial"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  offsets.theta <- object@offsetsTheta
                  offsets.component <- object@offsetsComponent
                  transform <- object@transformComponent
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  n.iter <- length(iterations)
                  transform <- addIterationsToTransform(transform, nIter = n.iter)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets.theta[1L],
                                           last = offsets.theta[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  exposure <- getDataFromFile(filename = filename,
                                              first = offsets.component[1L],
                                              last = offsets.component[2L],
                                              lengthIter = lengthIter,
                                              iterations = iterations)
                  exposure <- array(exposure, dim = transform@dimBefore)
                  exposure <- dembase::collapse(exposure, transform = transform)
                  is.missing <- is.na(.Data)
                  n.missing <- sum(is.missing)
                  .Data[is.missing] <- stats::rbinom(n = n.missing,
                                              size = exposure[is.missing],
                                              prob = theta[is.missing])
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDatasetPoissonBinomial"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter, shift = TRUE,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  prob <- object@prob
                  offsets <- object@offsetsComponent
                  transform <- object@transformComponent
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
                  n.iter <- length(iterations)
                  transform <- addIterationsToTransform(transform, nIter = n.iter)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  exposure <- getDataFromFile(filename = filename,
                                              first = offsets[1L],
                                              last = offsets[2L],
                                              lengthIter = lengthIter,
                                              iterations = iterations)
                  exposure <- array(exposure, dim = transform@dimBefore)
                  exposure <- dembase::collapse(exposure, transform = transform)
                  is.missing <- is.na(.Data)
                  n.missing <- sum(is.missing)
                  exposure <- exposure[is.missing]
                  correctly.counted <- stats::rbinom(n = n.missing, size = exposure, prob = prob)
                  over.counted <- stats::rpois(n = n.missing, lambda = (1 - prob) * exposure)
                  .Data[is.missing] <- correctly.counted + over.counted
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })


## needToCenter #########################################################

## HAS_TESTS
setMethod("needToCenter",
          signature(object = "SkeletonBetaTerm"),
          function(object) TRUE)
