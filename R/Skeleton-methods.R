

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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
                   impute = FALSE) {
              first <- object@first
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
                   nIteration, lengthIter,
                   impute = FALSE) {
              first <- object@first
              last <- object@last
              metadata <- object@metadata
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
                   nIteration, lengthIter,
                   impute = FALSE) {
              metadata <- object@metadata
              margins <- object@margins
              offsets <- object@offsets
              if (is.null(iterations))
                  iterations <- seq_len(nIteration)
              n.iter <- length(iterations)
              metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)
              dim <- dim(metadata)
              n.dim <- length(dim)
              betas <- vector(mode = "list", length = length(margins))
              .Data.ans <- array(0, dim = dim(metadata), dimnames = dimnames(metadata))
              ans <- methods::new("Values", .Data = .Data.ans, metadata = metadata)
              dim.after <- dim(ans)
              for (i in seq_along(margins)) {
                  ## get unadjusted beta estimates
                  first <- offsets[[i]][1L]
                  last <- offsets[[i]][2L]
                  .Data.beta <- getDataFromFile(filename = filename,
                                                first = first,
                                                last = last,
                                                lengthIter = lengthIter,
                                                iterations = iterations)
                  ## extend to match dims of answer, then add to answer
                  is.intercept <- i == 1L
                  if (is.intercept)
                      beta <- rep(.Data.beta, each = length(ans) / length(.Data.beta))
                  else {
                      margin <- c(margins[[i]], n.dim)
                      dim.before <- dim[margin]
                      dims <- integer(length = n.dim)
                      indices <- vector(mode = "list", length = n.dim)
                      for (i in seq_len(n.dim)) {
                          i.before <- match(i, margin, nomatch = 0L)
                          dims[i] <- i.before
                          dim.included <- i.before > 0L
                          indices[[i]] <- if (dim.included) seq_len(dim.after[i]) else rep(1L, times = dim.after[i])
                      }
                      transform <- methods::new("ExtendTransform",
                                                dims = dims,
                                                indices = indices,
                                                dimBefore = dim.before,
                                                dimAfter = dim.after)
                      .Data.beta <- array(.Data.beta, dim = dim.before)
                      beta <- dembase::extend(.Data.beta, transform = transform)
                  }                  
                  ans <- ans + beta
              }
              ans
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonCovariates"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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

## NO_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataCMPNotUseExp"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  offsets.theta <- object@offsetsTheta
                  offsets.nu <- object@offsetsNu
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata,
                                                               iterations = iterations)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  mu <- getDataFromFile(filename = filename,
                                        first = offsets.theta[1L],
                                        last = offsets.theta[2L],
                                        lengthIter = lengthIter,
                                        iterations = iterations)
                  nu <- getDataFromFile(filename = filename,
                                        first = offsets.nu[1L],
                                        last = offsets.nu[2L],
                                        lengthIter = lengthIter,
                                        iterations = iterations)
                  n <- length(.Data)
                  for (i in seq_len(n)) {
                      if (is.na(.Data[i]))
                          .Data[i] <- rcmp1(mu = mu[i],
                                            nu = nu[i],
                                            maxAttempt = 1000L,
                                            useC = TRUE)
                  }
                  methods::new("Counts",
                               .Data = .Data,
                               metadata = metadata)
              }
              else
                  data
          })

## NO_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataCMPUseExp"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  exposure <- object@exposure
                  offsets.theta <- object@offsetsTheta
                  offsets.nu <- object@offsetsNu
                  exposure <- as.numeric(exposure)
                  if (is.null(iterations))
                      iterations <- seq_len(nIteration)
                  metadata <- data@metadata
                  metadata <- dembase::addIterationsToMetadata(metadata,
                                                               iterations = iterations)
                  .Data <- array(data@.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  theta <- getDataFromFile(filename = filename,
                                           first = offsets.theta[1L],
                                           last = offsets.theta[2L],
                                           lengthIter = lengthIter,
                                           iterations = iterations)
                  nu <- getDataFromFile(filename = filename,
                                        first = offsets.nu[1L],
                                        last = offsets.nu[2L],
                                        lengthIter = lengthIter,
                                        iterations = iterations)
                  mu <- exposure * theta
                  n <- length(.Data)
                  for (i in seq_len(n)) {
                      if (is.na(.Data[i]))
                          .Data[i] <- rcmp1(mu = mu[i],
                                            nu = nu[i],
                                            maxAttempt = 1000L,
                                            useC = TRUE)
                  }
                  methods::new("Counts",
                               .Data = .Data,
                               metadata = metadata)
              }
              else
                  data
          })

## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDataBinomial"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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
                   nIteration, lengthIter,
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


## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDatasetRound3"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
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
                  exposure <- exposure[is.missing]
                  rounded <- dembase::round3(exposure)
                  .Data[is.missing] <- rounded
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })


## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDatasetNormalFixedUseExp"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  offsets <- object@offsetsComponent
                  transform <- object@transformComponent
                  mean <- object@mean@.Data
                  sd <- object@sd@.Data
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
                  mean <- rep(mean, times = n.iter)
                  sd <- rep(sd, times = n.iter)
                  is.missing <- is.na(.Data)
                  mean <- mean[is.missing] * exposure[is.missing]
                  sd <- sd[is.missing]
                  n <- sum(is.missing)
                  imputed <- stats::rnorm(n = n, mean = mean, sd = sd)
                  .Data[is.missing] <- imputed
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })



## HAS_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDatasetLN2"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
                   impute = FALSE) {
            data <- object@data
            if (impute) {
              offsets.alpha <- object@offsetsAlphaLN2
              offsets.varsigma <- object@offsetsVarsigmaLN2
              offsets.comp <- object@offsetsComponent
              transform.alpha <- object@transformLN2
              transform.comp <- object@transformComponent
              struc.zero.array <- object@strucZeroArray
              if (is.null(iterations))
                iterations <- seq_len(nIteration)
              metadata <- data@metadata
              n.data <- prod(dim(metadata))
              metadata <- dembase::addIterationsToMetadata(metadata,
                                                           iterations = iterations)
              n.iter <- length(iterations)
              transform.alpha <- addIterationsToTransform(transform.alpha,
                                                          nIter = n.iter)
              transform.comp <- addIterationsToTransform(transform.comp,
                                                         nIter = n.iter)
              transform.alpha <- methods::new("ExtendTransform",
                                              indices = transform.alpha@indices,
                                              dims = transform.alpha@dims,
                                              dimBefore = transform.alpha@dimAfter,
                                              dimAfter = transform.alpha@dimBefore)
              .Data <- array(data@.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              alpha <- getDataFromFile(filename = filename,
                                       first = offsets.alpha[1L],
                                       last = offsets.alpha[2L],
                                       lengthIter = lengthIter,
                                       iterations = iterations)
              varsigma <- getDataFromFile(filename = filename,
                                          first = offsets.varsigma[1L],
                                          last = offsets.varsigma[2L],
                                          lengthIter = lengthIter,
                                          iterations = iterations)
              exposure <- getDataFromFile(filename = filename,
                                          first = offsets.comp[1L],
                                          last = offsets.comp[2L],
                                          lengthIter = lengthIter,
                                          iterations = iterations)
              alpha <- array(alpha,
                             dim = transform.alpha@dimBefore)
              varsigma <- rep(varsigma, each = n.data)
              exposure <- array(exposure,
                                dim = transform.comp@dimBefore)
              alpha <- dembase::extend(alpha,
                                       transform = transform.alpha)
              exposure <- dembase::collapse(exposure,
                                            transform = transform.comp)
              is.struc.zero <- struc.zero.array@.Data == 0L
              is.struc.zero <- array(is.struc.zero,
                                     dim = dim(.Data))
              is.missing <- is.na(.Data)
              n.impute <- sum(is.missing)
              mean <- log(exposure[is.missing] + 1) + alpha[is.missing]
              sd <- varsigma[is.missing]
              imputed <- exp(rnorm(n = n.impute, mean = mean, sd = sd)) - 1
              .Data[is.missing] <- imputed
              .Data[is.struc.zero] <- 0L
              methods::new("Counts",
                           .Data = .Data,
                           metadata = metadata)
            }
            else
              data
          })


## getIndicesStrucZero #########################################################

setMethod("getIndicesStrucZero",
          signature(object = "SkeletonIndicesStrucZero"),
          function(object) {
              object@indicesStrucZero
          })

