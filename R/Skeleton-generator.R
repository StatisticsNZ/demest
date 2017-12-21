
## HAS_TESTS
setMethod("Skeleton",
          signature(object = "missing",
                    metadata = "missing",
                    first = "integer"),
          function(first) {
              methods::new("SkeletonOneValues",
                           first = first)
          })

## HAS_TESTS
setMethod("Skeleton",
          signature(object = "missing",
                    metadata = "MetaData",
                    first = "integer"),
          function(metadata, first, strucZeroArray = NULL) {
              last <- first + as.integer(prod(dim(metadata))) - 1L
              indices.struc.zero <- makeIndicesStrucZero(metadata = metadata,
                                                         strucZeroArray = strucZeroArray)
              methods::new("SkeletonManyValues",
                           first = first,
                           last = last,
                           metadata = metadata,
                           indicesStrucZero = indices.struc.zero)
          })

## HAS_TESTS
setMethod("Skeleton",
          signature(object = "Counts",
                    metadata = "missing",
                    first = "integer"),
          function(object, first) {
              metadata <- object@metadata
              last <- first + as.integer(prod(dim(metadata))) - 1L
              methods::new("SkeletonManyCounts",
                           first = first,
                           last = last,
                           metadata = metadata)
          })

## HAS_TESTS
setMethod("Skeleton",
          signature(object = "Values",
                    metadata = "missing",
                    first = "integer"),
          function(object, first, strucZeroArray = NULL) {
              metadata <- object@metadata
              last <- first + as.integer(prod(dim(metadata))) - 1L
              indices.struc.zero <- makeIndicesStrucZero(metadata = metadata,
                                                         strucZeroArray = strucZeroArray)
              methods::new("SkeletonManyValues",
                           first = first,
                           last = last,
                           metadata = metadata,
                           indicesStrucZero = indices.struc.zero)
          })


## HAS_TESTS
SkeletonMu <- function(betas, margins, first, metadata, strucZeroArray = NULL) {
    n <- length(betas)
    offsets <- vector(mode = "list", length = n)
    pos <- first
    for (i in seq_len(n)) {
        first <- pos
        pos <- pos + length(betas[[i]])
        last <- pos - 1L
        offsets[[i]] <- methods::new("Offsets", c(first, last))
    }
    indices.struc.zero <- makeIndicesStrucZero(metadata = metadata,
                                               strucZeroArray = strucZeroArray)
    methods::new("SkeletonMu",
                 margins = margins,
                 metadata = metadata,
                 offsets = offsets,
                 indicesStrucZero = indices.struc.zero)
}

## HAS_TESTS
SkeletonBetaIntercept <- function(first) {
    methods::new("SkeletonBetaIntercept",
                 first = first,
                 last = first)
}

## HAS_TESTS
SkeletonBetaTerm <- function(first, metadata, strucZeroArray = NULL) {
    last <- first + as.integer(prod(dim(metadata))) - 1L
    indices.struc.zero <- makeIndicesStrucZero(metadata = metadata,
                                               strucZeroArray = strucZeroArray)
    methods::new("SkeletonBetaTerm",
                 first = first,
                 last = last,
                 metadata = metadata,
                 indicesStrucZero = indices.struc.zero)
}

## HAS_TESTS
setMethod("SkeletonAccept",
          signature(nAttempt = "missing",
                    first = "integer",
                    nChain = "integer",
                    nIteration = "integer"),
          function(first, nChain, nIteration) {
              i.first.in.chain <- seq(from = 1L,
                                      by = nIteration / nChain,
                                      to = nIteration)
              i.first.in.chain <- as.integer(i.first.in.chain)
              methods::new("SkeletonAccept",
                           first = first,
                           iFirstInChain = i.first.in.chain)
          })

## HAS_TESTS
setMethod("SkeletonAccept",
          signature(nAttempt = "integer",
                    first = "integer",
                    nChain = "integer",
                    nIteration = "integer"),
          function(nAttempt, first, nChain, nIteration) {
              i.first.in.chain <- seq(from = 1L,
                                      by = nIteration / nChain,
                                      to = nIteration)
              i.first.in.chain <- as.integer(i.first.in.chain)
              methods::new("SkeletonNAccept",
                  nAttempt = nAttempt,
                  first = first,
                  iFirstInChain = i.first.in.chain)
          })

## Missing data

## HAS_TESTS
setMethod("SkeletonMissingData",
          signature(object = "DemographicArray",
                    model = "Normal",
                    outputModel = "list",
                    exposure = "NULL"),
          function(object, model, outputModel, exposure) {
              offsets.theta <- outputModel$likelihood$mean
              offsets.theta <- methods::new("Offsets", c(offsets.theta@first, offsets.theta@last))
              w <- model@w
              if (methods::is(model, "VarsigmaKnown")) {
                  varsigma <- model@varsigma
                  methods::new("SkeletonMissingDataNormalVarsigmaKnown",
                      data = object,
                      offsetsTheta = offsets.theta,
                      w = w,
                      varsigma = varsigma)
              }
              else {
                  offsets.varsigma <- outputModel$likelihood$sd
                  offsets.varsigma <- methods::new("Offsets", c(offsets.varsigma@first, offsets.varsigma@first))
                  methods::new("SkeletonMissingDataNormalVarsigmaUnknown",
                      data = object,
                      offsetsTheta = offsets.theta,
                      w = w,
                      offsetsVarsigma = offsets.varsigma)
              }
          })

## HAS_TESTS
setMethod("SkeletonMissingData",
          signature(object = "Counts",
                    model = "Poisson",
                    outputModel = "list",
                    exposure = "NULL"),
          function(object, model, outputModel, exposure) {
              offsets <- outputModel$likelihood$count
              offsets <- methods::new("Offsets", c(offsets@first, offsets@last))
              class <- "SkeletonMissingDataPoissonNotUseExp"
              if (methods::is(object, "HasSubtotals"))
                  class <- paste0(class, "Subtotals")
              methods::new(class,
                  data = object,
                  offsetsTheta = offsets)
          })

## HAS_TESTS
setMethod("SkeletonMissingData",
          signature(object = "Counts",
                    model = "Poisson",
                    outputModel = "list",
                    exposure = "Counts"),
          function(object, model, outputModel, exposure) {
              offsets <- outputModel$likelihood$rate
              offsets <- methods::new("Offsets", c(offsets@first, offsets@last))
              class <- "SkeletonMissingDataPoissonUseExp"
              if (methods::is(object, "HasSubtotals"))
                  class <- paste0(class, "Subtotals")
              methods::new(class,
                  data = object,
                  exposure = exposure,
                  offsetsTheta = offsets)
          })

## HAS_TESTS
setMethod("SkeletonMissingData",
          signature(object = "Counts",
                    model = "Binomial",
                    outputModel = "list",
                    exposure = "Counts"),
          function(object, model, outputModel, exposure) {
              offsets <- outputModel$likelihood$prob
              offsets <- methods::new("Offsets", c(offsets@first, offsets@last))
              class <- "SkeletonMissingDataBinomial"
              if (methods::is(object, "HasSubtotals"))
                  class <- paste0(class, "Subtotals")
              methods::new(class,
                  data = object,
                  exposure = exposure,
                  offsetsTheta = offsets)
          })

## HAS_TESTS
setMethod("SkeletonMissingDataset",
          signature(object = "Counts",
                    model = "Poisson",
                    outputModel = "list",
                    transformComponent = "CollapseTransform",
                    skeletonComponent = "SkeletonMany"),
          function(object, model, outputModel, transformComponent, skeletonComponent) {
              uses.exposure <- methods::is(model, "UseExposure")
              if (uses.exposure)
                  offsets.theta <- outputModel$likelihood$rate
              else
                  offsets.theta <- outputModel$likelihood$count
              offsets.theta <- methods::new("Offsets", c(offsets.theta@first, offsets.theta@last))
              offsets.component <- methods::new("Offsets", c(skeletonComponent@first, skeletonComponent@last))
              class <- "SkeletonMissingDatasetPoisson"
              if (methods::is(object, "HasSubtotals"))
                  class <- paste0(class, "Subtotals")
              methods::new(class,
                  data = object,
                  offsetsTheta = offsets.theta,
                  transformComponent = transformComponent,
                  offsetsComponent = offsets.component)
          })

## HAS_TESTS
setMethod("SkeletonMissingDataset",
          signature(object = "Counts",
                    model = "Binomial",
                    outputModel = "list",
                    transformComponent = "CollapseTransform",
                    skeletonComponent = "SkeletonMany"),
          function(object, model, outputModel, transformComponent, skeletonComponent) {
              offsets.theta <- outputModel$likelihood$prob
              offsets.theta <- methods::new("Offsets", c(offsets.theta@first, offsets.theta@last))
              offsets.component <- methods::new("Offsets", c(skeletonComponent@first, skeletonComponent@last))
              methods::new("SkeletonMissingDatasetBinomial",
                  data = object,
                  offsetsTheta = offsets.theta,
                  transformComponent = transformComponent,
                  offsetsComponent = offsets.component)
          })

## HAS_TESTS
setMethod("SkeletonMissingDataset",
          signature(object = "Counts",
                    model = "PoissonBinomialMixture",
                    outputModel = "list",
                    transformComponent = "CollapseTransform",
                    skeletonComponent = "SkeletonMany"),
          function(object, model, outputModel, transformComponent, skeletonComponent) {
              prob <- outputModel$prob
              offsets.component <- methods::new("Offsets", c(skeletonComponent@first, skeletonComponent@last))
              methods::new("SkeletonMissingDatasetPoissonBinomial",
                  data = object,
                  prob = prob,
                  transformComponent = transformComponent,
                  offsetsComponent = offsets.component)
          })




