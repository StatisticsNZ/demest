

setClass("SkeletonNu",
         contains = "NuMixin")

## NO_TESTS
## NO_FETCH
setClass("SkeletonMissingDatasetTFixedUseExp",
         contains = c("SkeletonMissingDataset",
                      "SkeletonMetadata",
                      "SkeletonMeanSD",
                      "SkeletonNu"))


## NO_TESTS
setMethod("makeOutputModel",
          signature(model = "TFixed"),
          function(model) {
              metadata <- model@metadataY
              mean <- model@mean@.Data
              sd <- model@sd@.Data
              nu <- model@nu@.Data
              .Data.mean <- array(mean,
                                  dim = dim(metadata),
                                  dimnames = dimnames(metadata))
              .Data.sd <- array(sd,
                                dim = dim(metadata),
                                dimnames = dimnames(metadata))
              mean <- new("Values",
                          .Data = .Data.mean,
                          metadata = metadata)
              sd <- new("Values",
                        .Data = .Data.sd,
                        metadata = metadata)
              list(mean = mean,
                   scale = sd,
                   nu = nu)
          })










## Need to do notuseexp version of this - also for NormalFixed
## NO_TESTS
setMethod("SkeletonMissingDataset",
          signature(object = "Counts",
                    model = "TFixedUseExp",
                    outputModel = "list",
                    transformComponent = "CollapseTransform",
                    skeletonComponent = "SkeletonMany"),
          function(object, model, outputModel, transformComponent, skeletonComponent) {
              offsets.component <- methods::new("Offsets", c(skeletonComponent@first, skeletonComponent@last))
              methods::new("SkeletonMissingDatasetTFixedUseExp",
                           mean = model@mean,
                           sd = model@sd,
                           nu = model@nu,
                           metadata = model@metadataY,
                           data = object,
                           transformComponent = transformComponent,
                           offsetsComponent = offsets.component)
          })


## Do NotUseExp version - including for Normal

## NO_TESTS
setMethod("fetchResults",
          signature(object = "SkeletonMissingDatasetTFixedUseExp"),
          function(object, nameObject, filename, iterations,
                   nIteration, lengthIter,
                   impute = FALSE) {
              data <- object@data
              if (impute) {
                  offsets <- object@offsetsComponent
                  transform <- object@transformComponent
                  mean <- object@mean@.Data
                  sd <- object@sd@.Data
                  nu <- object@nu@.Data
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
                  z <- stats::rnorm(n = n)
                  x <- stats::rchisq(n = n, df = nu)
                  imputed <- mean + sd * z * sqrt(df / x)
                  .Data[is.missing] <- imputed
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  data
          })




