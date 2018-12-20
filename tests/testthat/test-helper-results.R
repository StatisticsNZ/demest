
context("helper-results")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("changeInPos works", {
    changeInPos <- demest:::changeInPos
    object <- new("SkeletonOneValues",
                  first = 3L)
    expect_identical(changeInPos(object), 1L)
    object <- list(a = new("SkeletonManyValues",
                  first = 3L,
                  last = 4L,
                  metadata = new("MetaData",
                  nms = "sex",
                  dimtypes = "sex",
                  DimScales = list(new("Sexes", dimvalues = c("f", "m"))))),
                   b = 4)
    expect_identical(changeInPos(object), 2L)
    expect_identical(changeInPos(list()), 0L)
    expect_identical(changeInPos(1:5), 0L)
})

test_that("fetchAdjustments works", {
    fetchAdjustments <- demest:::fetchAdjustments
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    adjustments <- new.env(hash = TRUE)
    adjustments[["model.prior.intercept"]] <- 1
    adjustments.serialized <- serialize(adjustments, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(length(adjustments.serialized), con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:200)
    writeBin(data, con = con)
    writeBin(adjustments.serialized, con = con)
    close(con)
    nIteration <- 20L
    lengthIter <- 10L
    ans.obtained <- fetchAdjustments(filename = filename,
                                     nIteration = nIteration,
                                     lengthIter = lengthIter)
    ans.expected <- adjustments
    expect_equal(ans.obtained, ans.expected)
})

test_that("indices0 works - nSeason is NULL", {
    indices0 <- demest:::indices0
    AlongIterator <- demest:::AlongIterator
    ## dim = 5, along = 1
    iterator <- AlongIterator(dim = 5L, iAlong = 1L)
    ans.obtained <- indices0(iterator, dim = 5L, iAlong = 1L)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, along = 1
    iterator <- AlongIterator(dim = 3:4, iAlong = 1L)
    ans.obtained <- indices0(iterator, dim = 3:4, iAlong = 1L)
    ans.expected <- sort(array(1:12, 3:4)[1,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, along = 2
    iterator <- AlongIterator(dim = 3:4, iAlong = 2L)
    ans.obtained <- indices0(iterator, dim = 3:4, iAlong = 2L)
    ans.expected <- sort(array(1:12, 3:4)[,1])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 10, along = 1
    iterator <- AlongIterator(dim = 10L, iAlong = 1L)
    ans.obtained <- indices0(iterator, dim = 10L, iAlong = 1L)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 1
    iterator <- AlongIterator(dim = 2:4, iAlong = 1L)
    ans.obtained <- indices0(iterator, dim = 2:4, iAlong = 1L)
    ans.expected <- sort(array(1:24, 2:4)[1,,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 2
    iterator <- AlongIterator(dim = 2:4, iAlong = 2L)
    ans.obtained <- indices0(iterator, dim = 2:4, iAlong = 2L)
    ans.expected <- sort(array(1:24, 2:4)[,1,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 3
    iterator <- AlongIterator(dim = 2:4, iAlong = 3L)
    ans.obtained <- indices0(iterator, dim = 2:4, iAlong = 3L)
    ans.expected <- sort(array(1:24, 2:4)[,,1])
    expect_identical(ans.obtained, ans.expected)
})

test_that("indices0 works - nSeason is non-NULL", {
    indices0 <- demest:::indices0
    AlongIterator <- demest:::AlongIterator
    ## dim = 3:4, along = 1, nSeason = 2
    iterator <- AlongIterator(dim = 3:4, iAlong = 1L)
    ans.obtained <- indices0(iterator, nSeason = 2L, dim = 3:4, iAlong = 1L)
    ans.expected <- sort(array(1:24, 2:4)[,1,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, along = 2, nSeason = 2
    iterator <- AlongIterator(dim = 3:4, iAlong = 2L)
    ans.obtained <- indices0(iterator, nSeason = 2L, dim = 3:4, iAlong = 2L)
    ans.expected <- sort(array(1:24, 2:4)[,,1])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 10, along = 1, nSeason = 12
    iterator <- AlongIterator(dim = 10L, iAlong = 1L)
    ans.obtained <- indices0(iterator, nSeason = 12L, dim = 10L, iAlong = 1L)
    ans.expected <- 1:12
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 1, nSeason = 4
    iterator <- AlongIterator(dim = 2:4, iAlong = 1L)
    ans.obtained <- indices0(iterator, nSeason = 4L, dim = 2:4, iAlong = 1L)
    ans.expected <- sort(array(1:96, c(4, 2, 3, 4))[,1,,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 2, nSeason = 4
    iterator <- AlongIterator(dim = 2:4, iAlong = 2L)
    ans.obtained <- indices0(iterator, nSeason = 4L, dim = 2:4, iAlong = 2L)
    ans.expected <- sort(array(1:96, c(4, 2, 3, 4))[,,1,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 3
    iterator <- AlongIterator(dim = 2:4, iAlong = 3L)
    ans.obtained <- indices0(iterator, nSeason = 4L, dim = 2:4, iAlong = 3L)
    ans.expected <- sort(array(1:96, c(4, 2, 3, 4))[,,,1])
    expect_identical(ans.obtained, ans.expected)
})

test_that("indicesShow works - nSeason is NULL", {
    indicesShow <- demest:::indicesShow
    AlongIterator <- demest:::AlongIterator
    ## dim = 5, along = 1
    iterator <- AlongIterator(dim = 5L, iAlong = 1L)
    ans.obtained <- indicesShow(iterator, dim = 5L, iAlong = 1L)
    ans.expected <- 2:5
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, along = 1
    iterator <- AlongIterator(dim = 3:4, iAlong = 1L)
    ans.obtained <- indicesShow(iterator, dim = 3:4, iAlong = 1L)
    ans.expected <- sort(array(1:12, 3:4)[-1,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, along = 2
    iterator <- AlongIterator(dim = 3:4, iAlong = 2L)
    ans.obtained <- indicesShow(iterator, dim = 3:4, iAlong = 2L)
    ans.expected <- sort(array(1:12, 3:4)[,-1])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 10, along = 1
    iterator <- AlongIterator(dim = 10L, iAlong = 1L)
    ans.obtained <- indicesShow(iterator, dim = 10L, iAlong = 1L)
    ans.expected <- 2:10
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 1
    iterator <- AlongIterator(dim = 2:4, iAlong = 1L)
    ans.obtained <- indicesShow(iterator, dim = 2:4, iAlong = 1L)
    ans.expected <- sort(array(1:24, 2:4)[-1,,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 2
    iterator <- AlongIterator(dim = 2:4, iAlong = 2L)
    ans.obtained <- indicesShow(iterator, dim = 2:4, iAlong = 2L)
    ans.expected <- sort(array(1:24, 2:4)[,-1,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 3
    iterator <- AlongIterator(dim = 2:4, iAlong = 3L)
    ans.obtained <- indicesShow(iterator, dim = 2:4, iAlong = 3L)
    ans.expected <- sort(array(1:24, 2:4)[,,-1])
    expect_identical(ans.obtained, ans.expected)
})

test_that("indicesShow works - nSeason is non-NULL", {
    indicesShow <- demest:::indicesShow
    AlongIterator <- demest:::AlongIterator
    ## dim = 3:4, along = 1, nSeason = 2
    iterator <- AlongIterator(dim = 3:4, iAlong = 1L)
    ans.obtained <- indicesShow(iterator, nSeason = 2L, dim = 3:4, iAlong = 1L)
    ans.expected <- c(3L, 5L, 9L, 11L, 15L, 17L, 21L, 23L)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, along = 2, nSeason = 2
    iterator <- AlongIterator(dim = 3:4, iAlong = 2L)
    ans.obtained <- indicesShow(iterator, nSeason = 2L, dim = 3:4, iAlong = 2L)
    ans.expected <- c(7L, 9L, 11L, 13L, 15L, 17L, 19L, 21L, 23L)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 10, along = 1, nSeason = 12
    iterator <- AlongIterator(dim = 10L, iAlong = 1L)
    ans.obtained <- indicesShow(iterator, nSeason = 12L, dim = 10L, iAlong = 1L)
    ans.expected <- seq.int(13L, 109L, 12L)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 1, nSeason = 4
    iterator <- AlongIterator(dim = 2:4, iAlong = 1L)
    ans.obtained <- indicesShow(iterator, nSeason = 4L, dim = 2:4, iAlong = 1L)
    ans.expected <- sort(array(1:96, c(4, 2, 3, 4))[1,-1,,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 2, nSeason = 4
    iterator <- AlongIterator(dim = 2:4, iAlong = 2L)
    ans.obtained <- indicesShow(iterator, nSeason = 4L, dim = 2:4, iAlong = 2L)
    ans.expected <- sort(array(1:96, c(4, 2, 3, 4))[1,,-1,])
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:4, along = 3
    iterator <- AlongIterator(dim = 2:4, iAlong = 3L)
    ans.obtained <- indicesShow(iterator, nSeason = 4L, dim = 2:4, iAlong = 3L)
    ans.expected <- sort(array(1:96, c(4, 2, 3, 4))[1,,,-1])
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeIndicesStrucZero works", {
    makeIndicesStrucZero <- demest:::makeIndicesStrucZero
    margin <- 2L
    strucZeroArray <- NULL
    ans.obtained <- makeIndicesStrucZero(strucZeroArray = strucZeroArray,
                                         margin = margin)
    ans.expected <- integer()
    expect_identical(ans.obtained, ans.expected)
    strucZeroArray <- Counts(array(c(0L, 1L),
                                   dim = c(2, 3),
                                   dimnames = list(sex = c("f", "m"), region = c("a", "b", "c"))))
    margin <- 1L
    ans.obtained <- makeIndicesStrucZero(strucZeroArray = strucZeroArray,
                                         margin = margin)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    strucZeroArray <- Counts(array(c(0L, 1L),
                                   dim = c(2, 3),
                                   dimnames = list(sex = c("f", "m"), region = c("a", "b", "c"))))
    margin <- 1:2
    ans.obtained <- makeIndicesStrucZero(strucZeroArray = strucZeroArray,
                                         margin = margin)
    ans.expected <- c(1L, 3L, 5L)
    expect_identical(ans.obtained, ans.expected)
})


test_that("makeMetadata0 works", {
    makeMetadata0 <- demest:::makeMetadata0
    ## length(metadata) > 1, nSeason is NULL
    metadata <- new("MetaData",
                    nms = c("time", "sex", "age"),
                    dimtypes = c("time", "sex", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Sexes", dimvalues = c("Female", "Male")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    iAlong <- 1L
    ans.obtained <- makeMetadata0(metadata = metadata,
                                  iAlong = iAlong,
                                  nSeason = NULL)
    ans.expected <- new("MetaData",
                        nms = c("sex", "age"),
                        dimtypes = c("sex", "age"),
                        DimScales = list(new("Sexes", dimvalues = c("Female", "Male")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    expect_identical(ans.obtained, ans.expected)
    ## length(metadata) > 1, nSeason is 4
    metadata <- new("MetaData",
                    nms = c("time", "sex", "age"),
                    dimtypes = c("time", "sex", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Sexes", dimvalues = c("Female", "Male")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    iAlong <- 1L
    ans.obtained <- makeMetadata0(metadata = metadata,
                                  iAlong = iAlong,
                                  nSeason = 4L)
    ans.expected <- new("MetaData",
                        nms = c("season", "sex", "age"),
                        dimtypes = c("state", "sex", "age"),
                        DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                         new("Sexes", dimvalues = c("Female", "Male")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    expect_identical(ans.obtained, ans.expected)
    ## length(metadata) == 1, nSeason is NULL
    metadata <- new("MetaData",
                    nms = "time", 
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    iAlong <- 1L
    ans.obtained <- makeMetadata0(metadata = metadata,
                                  iAlong = iAlong,
                                  nSeason = NULL)
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
    ## length(metadata) == 1, nSeason is 4
    metadata <- new("MetaData",
                    nms = "time", 
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    iAlong <- 1L
    ans.obtained <- makeMetadata0(metadata = metadata,
                                  iAlong = iAlong,
                                  nSeason = 4L)
    ans.expected <- new("MetaData",
                        nms = "season",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetadataIncl0 works", {
    makeMetadataIncl0 <- demest:::makeMetadataIncl0
    metadata <- new("MetaData",
                    nms = c("time", "sex", "age"),
                    dimtypes = c("time", "sex", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Sexes", dimvalues = c("Female", "Male")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    ## nSeason is NULL
    iAlong <- 1L
    ans.obtained <- makeMetadataIncl0(metadata = metadata,
                                      iAlong = iAlong,
                                      nSeason = NULL)
    ans.expected <- new("MetaData",
                        nms = c("time", "sex", "age"),
                        dimtypes = c("state", "sex", "age"),
                        DimScales = list(new("Categories", dimvalues = as.character(1:11)),
                                     new("Sexes", dimvalues = c("Female", "Male")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    expect_identical(ans.obtained, ans.expected)
    ## nSeason is 4
    iAlong <- 1L
    ans.obtained <- makeMetadataIncl0(metadata = metadata,
                                      iAlong = iAlong,
                                      nSeason = 4L)
    ans.expected <- new("MetaData",
                        nms = c("season", "time", "sex", "age"),
                        dimtypes = c("state", "state", "sex", "age"),
                        DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                         new("Categories", dimvalues = as.character(1:11)),
                                     new("Sexes", dimvalues = c("Female", "Male")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetadataVectorsMix works", {
    makeMetadataVectorsMix <- demest:::makeMetadataVectorsMix
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    iAlong <- 1L
    indexClassMax <- 10L
    ans.obtained <- makeMetadataVectorsMix(metadata = metadata,
                                           iAlong = iAlong,
                                           indexClassMax = indexClassMax)
    ans.expected <- new("MetaData",
                    nms = c("component", "reg", "age"),
                    dimtypes = c("state", "state", "age"),
                    DimScales = list(new("Categories", dimvalues = as.character(1:10)),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetadataWeightsMix works", {
    makeMetadataWeightsMix <- demest:::makeMetadataWeightsMix
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    iAlong <- 1L
    indexClassMax <- 10L
    ans.obtained <- makeMetadataWeightsMix(metadata = metadata,
                                           iAlong = iAlong,
                                           indexClassMax = indexClassMax)
    ans.expected <- new("MetaData",
                    nms = c("time", "component"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = as.character(1:10))))
    expect_identical(ans.obtained, ans.expected)
})
    
test_that("makeOutputMCMC works with valid input", {
    makeOutputMCMC <- demest:::makeOutputMCMC
    mcmcArgs <- list(nBurnin = 1000L,
                     nSim = 1000L,
                     nChain = 5L,
                     nThin = 20L)
    finalCombineds <- list(NULL, "combined", "combined", NULL, "combined")
    ans.obtained <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                                   finalCombineds = finalCombineds)
    ans.expected <- list(nBurnin = 1000L,
                         nSim = 1000L,
                         nChain = 3L,
                         nThin = 20L,
                         nIteration = 3000L)
    mcmcArgs <- list(nBurnin = 0L,
                     nSim = 1000L,
                     nChain = 1L,
                     nThin = 30L)
    finalCombineds <- list("combined")
    ans.obtained <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                                   finalCombineds = finalCombineds)
    ans.expected <- list(nBurnin = 0L,
                         nSim = 1000L,
                         nChain = 1L,
                         nThin = 30L,
                         nIteration = 33L)
})

test_that("makeOutputMCMC raises appropriate errors", {
    makeOutputMCMC <- demest:::makeOutputMCMC
    mcmcArgs <- list(nBurnin = 1000L,
                     nSim = 1000L,
                     nChain = 5L,
                     nThin = 20L)
    finalCombineds <- list(NULL, "combined", "combined", NULL)
    expect_error(makeOutputMCMC(mcmcArgs = mcmcArgs, finalCombineds = finalCombineds),
                 "length of 'finalCombineds' \\[4\\] not equal to 'nChain' argument \\[5\\]")
})

test_that("makeOutputPriorCoef works", {
    makeOutputPriorCoef <- demest:::makeOutputPriorCoef
    Skeleton <- demest:::Skeleton
    Z <- cbind("(Intercept)" = rep(1, 10), a = rnorm(10))
    ans.obtained <- makeOutputPriorCoef(Z = Z, pos = 3L)
    metadata <- new("MetaData",
                    nms = "coef",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = "a")))
    ans.expected <- new("SkeletonCovariates",
                        first = 3L,
                        last = 4L,
                        metadata = metadata)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputPriorScale works", {
    makeOutputPriorScale <- demest:::makeOutputPriorScale
    Skeleton <- demest:::Skeleton
    ans.obtained <- makeOutputPriorScale(pos = 3L)
    ans.expected <- Skeleton(first = 3L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputStateDLM works with Level", {
    makeOutputStateDLM <- demest:::makeOutputStateDLM
    AlongIterator <- demest:::AlongIterator
    ## phi = 1
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    iterator <- AlongIterator(dim = 11L, iAlong = 1L)
    ans.obtained <- makeOutputStateDLM(iterator = iterator,
                                       metadata = metadata,
                                       nSeason = NULL,
                                       iAlong = 1L,
                                       pos = 3L)
    metadata0 <- NULL
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- new("SkeletonStateDLM",
                        metadata = metadata,
                        metadata0 = metadata0,
                        metadataIncl0 = metadataIncl0,
                        iAlong = 1L,
                        first = 3L,
                        last = 13L,
                        indicesShow = 2:11,
                        indices0 = 1L)
    expect_identical(ans.obtained, ans.expected)
    ## phi = 1
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    metadata0 <- NULL
    metadataIncl0 <- new("MetaData",
                         nms = "time",
                         dimtypes = "state",
                         DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    iterator <- AlongIterator(dim = 11L, iAlong = 1L)
    ans.obtained <- makeOutputStateDLM(iterator = iterator,
                                       metadata = metadata,
                                       nSeason = NULL,
                                       iAlong = 1L,
                                       pos = 3L)
    ans.expected <- new("SkeletonStateDLM",
                        metadata = metadata,
                        metadata0 = metadata0,
                        metadataIncl0 = metadataIncl0,
                        iAlong = 1L,
                        first = 3L,
                        last = 13L,
                        indices0 = 1L,
                        indicesShow = 2:11)
    expect_identical(ans.obtained, ans.expected)
    ## two dimensions
    metadata <- new("MetaData",
                    nms = c("sex", "time"),
                    dimtypes = c("sex", "time"),
                    DimScales = list(new("Sexes", dimvalues = c("Female", "Male")),
                                     new("Points", dimvalues = 1:10)))
    iterator <- AlongIterator(dim = c(2L, 11L), iAlong = 2L)
    ans.obtained <- makeOutputStateDLM(iterator = iterator,
                                       metadata = metadata,
                                       nSeason = NULL,
                                       iAlong = 2L,
                                       pos = 3L)
    metadata0 <- new("MetaData",
                         nms = "sex",
                         dimtypes = "sex", 
                         DimScales = list(new("Sexes", dimvalues = c("Female", "Male"))))
    metadataIncl0 <- new("MetaData",
                         nms = c("sex", "time"),
                         dimtypes = c("sex", "state"),
                         DimScales = list(new("Sexes", dimvalues = c("Female", "Male")),
                                          new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- new("SkeletonStateDLM",
                        metadata = metadata,
                        metadata0 = metadata0,
                        metadataIncl0 = metadataIncl0,
                        first = 3L,
                        last = 24L,
                        iAlong = 2L,
                        indices0 = 1:2,
                        indicesShow = 3:22)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputStateDLM works with Trend", {
    makeOutputStateDLM <- demest:::makeOutputStateDLM
    AlongIterator <- demest:::AlongIterator
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    iterator <- AlongIterator(dim = 11L, iAlong = 1L)
    ans.obtained <- makeOutputStateDLM(iterator = iterator,
                                       metadata = metadata,
                                       nSeason = NULL,
                                       iAlong = 1L,
                                       pos = 3L)
    metadataIncl0 <- new("MetaData",
                    nms = "time",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- new("SkeletonStateDLM",
                        metadata = metadata,
                        metadata0 = NULL,
                        metadataIncl0 = metadataIncl0,
                        first = 3L,
                        last = 13L,
                        iAlong = 1L,
                        indices0 = 1L,
                        indicesShow = 2:11)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOutputStateDLM works with Season", {
    makeOutputStateDLM <- demest:::makeOutputStateDLM
    AlongIterator <- demest:::AlongIterator
    ## no structural zeros
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 1:10)))
    iterator <- AlongIterator(dim = 11L, iAlong = 1L)
    ans.obtained <- makeOutputStateDLM(iterator = iterator,
                                       metadata = metadata,
                                       nSeason = 4L,
                                       iAlong = 1L,
                                       pos = 3L)
    metadata0 <- new("MetaData",
                     nms = "season",
                     dimtypes = "state",
                     DimScales = list(new("Categories", dimvalues = as.character(1:4))))
    metadataIncl0 <- new("MetaData",
                         nms = c("season", "time"),
                         dimtypes = c("state", "state"),
                         DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                          new("Categories", dimvalues = as.character(1:11))))
    ans.expected <- new("SkeletonStateDLM",
                        metadata = metadata,
                        metadata0 = metadata0,
                        metadataIncl0 = metadataIncl0,
                        first = 3L,
                        last = 46L,
                        iAlong = 1L,
                        indicesShow = seq.int(5L, 41L, 4L),
                        indices0 = 1:4,
                        indicesStrucZero = integer())
    expect_identical(ans.obtained, ans.expected)
    ## with structural zeros
    metadata <- new("MetaData",
                    nms = c("time", "sex"),
                    dimtypes = c("time", "sex"),
                    DimScales = list(new("Points", dimvalues = 1:10),
                                     new("Sexes", dimvalues = c("Female", "Male"))))
    iterator <- AlongIterator(dim = c(11L, 2L), iAlong = 1L)
    strucZeroArray <- Counts(array(rep(c(1L, 0L), each = 10L),
                                   dim = c(10, 2),
                                   dimnames = list(time = 1:10, sex = c("Female", "Male"))),
                             dimscales = c(time = "Points"))
    ans.obtained <- makeOutputStateDLM(iterator = iterator,
                                       metadata = metadata,
                                       nSeason = 4L,
                                       iAlong = 1L,
                                       pos = 3L,
                                       strucZeroArray = strucZeroArray,
                                       margin = 1:2)
    metadata0 <- new("MetaData",
                     nms = c("season", "sex"),
                     dimtypes = c("state", "sex"),
                     DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                      new("Sexes", dimvalues = c("Female", "Male"))))
    metadataIncl0 <- new("MetaData",
                         nms = c("season", "time", "sex"),
                         dimtypes = c("state", "state", "sex"),
                         DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                                          new("Categories", dimvalues = as.character(1:11)),
                                          new("Sexes", dimvalues = c("Female", "Male"))))
    ans.expected <- new("SkeletonStateDLM",
                        metadata = metadata,
                        metadata0 = metadata0,
                        metadataIncl0 = metadataIncl0,
                        first = 3L,
                        last = 90L,
                        iAlong = 1L,
                        indicesShow = c(seq.int(5L, 41L, 4L), seq.int(49L, 85L, 4L)),
                        indices0 = c(1:4, 45:48),
                        indicesStrucZero = 11:20)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makePairsTerms works", {
    makePairsTerms <- demest:::makePairsTerms
    margins <- list(0L, 1L, 2L, 1:2)
    ans.obtained <- makePairsTerms(margins)
    ans.expected <- list(4:3, c(4L, 2L))
    expect_identical(ans.obtained, ans.expected)
    margins <- list(0L, 1L, 2L, 3L, 1:2, c(1L, 3L), 2:3, 1:3)
    ans.obtained <- makePairsTerms(margins)
    ans.expected <- list(8:7, c(8L, 6L), c(8L, 5L), c(8L, 4L), c(8L, 3L), c(8L, 2L),
                         c(7L, 4L), c(7L, 3L),
                         c(6L, 4L), c(6L, 2L),
                         c(5L, 3L), c(5L, 2L))
    expect_identical(ans.obtained, ans.expected)
    margins <- list(0L)
    ans.obtained <- makePairsTerms(margins)
    ans.expected <- list()
    expect_identical(ans.obtained, ans.expected)
    margins <- list(0L, 1L)
    ans.obtained <- makePairsTerms(margins)
    ans.expected <- list()
    expect_identical(ans.obtained, ans.expected)
    margins <- list(0L, 1L, 2L)
    ans.obtained <- makePairsTerms(margins)
    ans.expected <- list()
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeResultsFile works", {
    makeResultsFile <- demest:::makeResultsFile
    ## files are 500 char long
    filename <- tempfile()
    results <- new("ResultsModelEst")
    tempfiles <- c(tempfile(), tempfile(), tempfile())
    con <- file(filename, "wb")
    res.vec <- serialize(results, connection = NULL)
    size.res <- length(res.vec)
    writeBin(size.res, con = con)
    writeBin(0L, con = con)
    writeBin(res.vec, con = con)
    close(con)
    for (i in 1:3) {
        con <- file(tempfiles[i], "wb")
        writeBin((1:500) + (i - 1) * 500, con)
        close(con)
    }
    makeResultsFile(filename = filename, results = results, tempfiles = tempfiles)
    con <- file(filename, "rb")
    ans.length <- readBin(con, what = "integer", n = 1L)
    expect_identical(ans.length, size.res)
    ans.adj <- readBin(con, what = "integer", n = 1L)
    expect_identical(ans.adj, 0L)
    ans.res <- unserialize(connection = readBin(con, what = "raw", n = size.res))
    expect_identical(ans.res, results)
    ans.data <- readBin(con, what = "double", n = 1500)
    close(con)
    expect_identical(ans.data, as.double(1:1500))
})

test_that("makeResultsModelEst works with valid input", {
    makeResultsModelEst <- demest:::makeResultsModelEst
    initialCombinedModel <- demest:::initialCombinedModel
    extractValues <- demest:::extractValues
    ## without exposure
    y <- Counts(array(rnorm(n = 24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Normal(mean ~ sex * age + time))
    finalCombineds <- replicate(n = 3,
                                initialCombinedModel(spec,
                                                     y = y,
                                                     exposure = NULL,
                                                     weights = NULL))
    filename <- "filename"
    call <- call("estimateModel", list("model"))
    mcmcArgs <- list(nBurnin = 1000L, nSim = 1000L, nChain = 3L, nThin = 20L)
    lengthIter <- length(extractValues(finalCombineds[[1L]]))
    controlArgs <- list(call = call,
                        parallel = TRUE,
                        lengthIter = lengthIter,
                        nUpdateMax = 200L)
    seed <- list(c(407L, 1:6), c(407L, 6:1), c(407L, 3:8))
    ans <- makeResultsModelEst(finalCombineds = finalCombineds,
                            mcmcArgs = mcmcArgs,
                            controlArgs = controlArgs,
                            seed = seed)
    expect_true(validObject(ans))
    expect_is(ans, "ResultsModelEst")
    ## with exposure
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ sex * age + time))
    finalCombineds <- replicate(n = 3,
                                initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL))
    names(finalCombineds) <- c("chain1", "chain2", "chain3")
    filename <- "filename"
    call <- call("estimateModel", list("model"))
    mcmcArgs <- list(nBurnin = 1000L, nSim = 1000L, nChain = 3L, nThin = 20L)
    lengthIter <- length(extractValues(finalCombineds[[1L]]))
    controlArgs <- list(call = call,
                        parallel = TRUE,
                        lengthIter = lengthIter,
                        nUpdateMax = 200L)
    seed <- list(c(407L, 1:6), c(407L, 6:1), c(407L, 3:8))
    ans <- makeResultsModelEst(finalCombineds = finalCombineds,
                            mcmcArgs = mcmcArgs,
                            controlArgs = controlArgs,
                            seed = seed)
    expect_true(validObject(ans))
    expect_is(ans, "ResultsModelExposureEst")
    ## parallel is FALSE
    y <- Counts(array(rnorm(n = 24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Normal(mean ~ sex * age + time))
    finalCombineds <- replicate(n = 3,
                                initialCombinedModel(spec,
                                                     y = y,
                                                     exposure = NULL,
                                                     weights = NULL))
    filename <- "filename"
    call <- call("estimateModel", list("model"))
    mcmcArgs <- list(nBurnin = 1000L, nSim = 1000L, nChain = 3L, nThin = 20L)
    lengthIter <- length(extractValues(finalCombineds[[1L]]))
    controlArgs <- list(call = call,
                        parallel = FALSE,
                        lengthIter = lengthIter,
                        nUpdateMax = 200L)
    seed <- list(c(407L, 1:6))
    ans <- makeResultsModelEst(finalCombineds = finalCombineds,
                               mcmcArgs = mcmcArgs,
                               controlArgs = controlArgs,
                               seed = seed)
    expect_true(validObject(ans))
    expect_is(ans, "ResultsModelEst")
    ## 'y' has missing values exposure
    y <- Counts(array(c(rnorm(n = 23), NA),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Normal(mean ~ sex * age + time))
    finalCombineds <- replicate(n = 3,
                                initialCombinedModel(spec,
                                                     y = y,
                                                     exposure = NULL,
                                                     weights = NULL))
    filename <- "filename"
    call <- call("estimateModel", list("model"))
    mcmcArgs <- list(nBurnin = 1000L, nSim = 1000L, nChain = 3L, nThin = 20L)
    lengthIter <- length(extractValues(finalCombineds[[1L]]))
    controlArgs <- list(call = call,
                        parallel = TRUE,
                        lengthIter = lengthIter,
                        nUpdateMax = 200L)
    seed <- list(c(407L, 1:6), c(407L, 6:1), c(407L, 3:8))
    ans <- makeResultsModelEst(finalCombineds = finalCombineds,
                            mcmcArgs = mcmcArgs,
                            controlArgs = controlArgs,
                            seed = seed)
    expect_true(validObject(ans))
    expect_is(ans, "ResultsModelEst")
    expect_is(ans@y, "SkeletonMissingDataNormalVarsigmaUnknown")
})

test_that("makeResultsModelPred works with valid input", {
    makeResultsModelPred <- demest:::makeResultsModelPred
    initialCombinedModel <- demest:::initialCombinedModel
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    extractValues <- demest:::extractValues
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ sex * age + time))
    finalCombineds <- replicate(n = 3,
                                initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL))
    for (i in 1:3) {
        finalCombineds[[i]] <- initialCombinedModelPredict(combined = finalCombineds[[i]],
                                                           along = 3L,
                                                           labels = c("2004", "2005", "2006"),
                                                           n = NULL,
                                                           covariates = NULL,
                                                           aggregate = NULL,
                                                           lower = NULL,
                                                           upper = NULL,
                                                           yIsCounts = TRUE)
    }
    names(finalCombineds) <- c("chain1", "chain2", "chain3")
    filename <- "filename"
    call <- call("estimateModel", list("model"))
    mcmcArgs <- list(nBurnin = 1000L, nSim = 1000L, nChain = 3L, nThin = 20L)
    lengthIter <- length(extractValues(finalCombineds[[1L]]))
    controlArgs <- list(call = call,
                        parallel = TRUE,
                        lengthIter = lengthIter,
                        nUpdateMax = 200L)
    seed <- list(c(407L, 1:6), c(407L, 6:1), c(407L, 3:8))
    ans <- makeResultsModelPred(finalCombineds = finalCombineds,
                                mcmcArgs = mcmcArgs,
                                controlArgs = controlArgs,
                                seed = seed)
    expect_true(validObject(ans))
    expect_is(ans, "ResultsModelPred")
})

test_that("makeResultsCounts works with no exposure", {
    makeResultsCounts <- demest:::makeResultsCounts
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    extractValues <- demest:::extractValues
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 10)) + 1L,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ sex + age + time, useExpose = FALSE))
    datasets <- list(Counts(array(c(2:12, NA),
                                  dim = c(3, 4),
                                  dimnames = list(age = 0:2, time = 2000:2003)),
                            dimscales = c(time = "Intervals")),
                     Counts(array(1:6,
                                  dim = c(2, 3, 1),
                                  dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000)),
                            dimscales = c(time = "Intervals")))
    data.models <- list(Model(register ~ Poisson(mean ~ age)),
                        Model(census ~ PoissonBinomial(prob = 0.98)))
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    finalCombineds <- replicate(n = 3,
                                initialCombinedCounts(spec,
                                                      y = y,
                                                      exposure = NULL,
                                                      dataModels = data.models,
                                                      datasets = datasets,
                                                      namesDatasets = c("register", "census"),
                                                      transforms = transforms))
    filename <- "filename"
    call <- call("estimateCounts", list("model"))
    mcmcArgs <- list(nBurnin = 1000L, nSim = 1000L, nChain = 3L, nThin = 20L)
    lengthIter <- length(extractValues(finalCombineds[[1L]]))
    controlArgs <- list(call = call,
                        parallel = TRUE,
                        lengthIter = lengthIter,
                        nUpdateMax = 200L)
    seed <- list(c(407L, 1:6), c(407L, 6:1), c(407L, 3:8))
    ans <- makeResultsCounts(finalCombineds = finalCombineds,
                             mcmcArgs = mcmcArgs,
                             controlArgs = controlArgs,
                             seed = seed)
    expect_true(validObject(ans))
    expect_is(ans, "ResultsCountsEst")
})

test_that("makeResultsCounts works with exposure", {
    makeResultsCounts <- demest:::makeResultsCounts
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    extractValues <- demest:::extractValues
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 10)) + 1L,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure <- y + 1.0
    spec <- Model(y ~ Poisson(mean ~ sex + age + time))
    datasets <- list(Counts(array(c(2:12, NA),
                                  dim = c(3, 4),
                                  dimnames = list(age = 0:2, time = 2000:2003)),
                            dimscales = c(time = "Intervals")),
                     Counts(array(1:6,
                                  dim = c(2, 3, 1),
                                  dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000)),
                            dimscales = c(time = "Intervals")))
    data.models <- list(Model(register ~ Poisson(mean ~ age)),
                        Model(census ~ PoissonBinomial(prob = 0.98)))
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    finalCombineds <- replicate(n = 3,
                                initialCombinedCounts(spec,
                                                      y = y,
                                                      exposure = exposure,
                                                      dataModels = data.models,
                                                      datasets = datasets,
                                                      namesDatasets = c("register", "census"),
                                                      transforms = transforms))
    filename <- "filename"
    call <- call("estimateCounts", list("model"))
    mcmcArgs <- list(nBurnin = 1000L, nSim = 1000L, nChain = 3L, nThin = 20L)
    lengthIter <- length(extractValues(finalCombineds[[1L]]))
    controlArgs <- list(call = call,
                        parallel = TRUE,
                        lengthIter = lengthIter,
                        nUpdateMax = 200L)
    seed <- list(c(407L, 1:6), c(407L, 6:1), c(407L, 3:8))
    ans <- makeResultsCounts(finalCombineds = finalCombineds,
                             mcmcArgs = mcmcArgs,
                             controlArgs = controlArgs,
                             seed = seed)
    expect_true(validObject(ans))
    expect_is(ans, "ResultsCountsExposureEst")
})

test_that("makeResultsAccount works", {
    makeResultsAccount <- demest:::makeResultsAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    extractValues <- demest:::extractValues
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 15),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                        Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(Counts(array(7L,
                                  dim = 10,
                                  dimnames = list(time = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-")))),
                     Counts(array(seq.int(110L, 210L, 10L),
                                  dim = 11,
                                  dimnames = list(time = seq(2000, 2100, 10)))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    finalCombineds <- replicate(n = 3,
                                initialCombinedAccount(account = account,
                                                       systemModels = systemModels,
                                                       systemWeights = systemWeights,
                                                       dataModels = data.models,
                                                       seriesIndices = seriesIndices,
                                                       datasets = datasets,
                                                       namesDatasets = namesDatasets,
                                                       transforms = transforms))
    filename <- "filename"
    call <- call("estimateAccount", list("model"))
    mcmcArgs <- list(nBurnin = 1000L, nSim = 1000L, nChain = 3L, nThin = 20L)
    lengthIter <- length(extractValues(finalCombineds[[1L]]))
    controlArgs <- list(call = call,
                        parallel = TRUE,
                        lengthIter = lengthIter,
                        nUpdateMax = 200L)
    seed <- list(c(407L, 1:6), c(407L, 6:1), c(407L, 3:8))
    ans <- makeResultsAccount(finalCombineds = finalCombineds,
                              mcmcArgs = mcmcArgs,
                              controlArgs = controlArgs,
                              seed = seed)
    expect_true(validObject(ans))
    expect_is(ans, "ResultsAccount")
})


test_that("rescaleAndWriteBetas works", {
    rescaleAndWriteBetas <- demest:::rescaleAndWriteBetas
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:200)
    writeBin(data, con = con)
    close(con)
    high <- Values(array(as.double(1:60),
                         dim = c(3, 20),
                         dimnames = list(reg = 1:3, iteration = 1:20)))
    low <- Values(array(as.double(1:20),
                        dim = 20,
                        dimnames = list(iteration = 1:20)))
    adj <- Values(array(as.double(20:1),
                        dim = 20,
                        dimnames = list(iteration = 1:20)))
    skeleton.high <- new("SkeletonBetaTerm",
                         first = 2L,
                         last = 4L,
                         metadata = new("MetaData",
                                        nms = "reg",
                                        dimtypes = "state",
                                        DimScales = list(new("Categories", dimvalues = c("a", "b", "c")))))
    skeleton.low <- new("SkeletonBetaIntercept",
                        first = 5L,
                        last = 5L)
    nIteration <- 20L
    lengthIter <- 10L
    rescaleAndWriteBetas(high = high,
                         low = low,
                         adj = adj,
                         skeletonHigh = skeleton.high,
                         skeletonLow = skeleton.low,
                         filename = filename,
                         nIteration = nIteration,
                         lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    length.results <- readBin(con, what = "integer", n = 1)
    readBin(con, what = "integer", n = 1)
    readBin(con, what = "raw", n = length.results)
    output <- readBin(con, what = "double", n = 1000)
    close(con)
    output <- matrix(output, nr = lengthIter)
    high.adj <- high - adj
    low.adj <- low + adj
    expect_equal(as.numeric(output[2:4, ]), as.numeric(high.adj))
    expect_equal(as.numeric(output[5, ]), as.numeric(low.adj))
})

test_that("rescaleBetasPredHelper works", {
    rescaleBetasPredHelper <- demest:::rescaleBetasPredHelper
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex),
                        age ~ Exch()),
                  y = y,
                  exposure = exposure,
                  nBurnin = 5,
                  nSim = 10,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    results <- fetchResultsObject(filename)
    priorsBetas <- results@final[[1]]@model@priorsBetas
    namesBetas <- results@final[[1]]@model@namesBetas
    skeletonsBetas <- results@model$prior[seq_along(namesBetas)]
    nIteration <- results@mcmc[["nIteration"]]
    lengthIter <- results@control$lengthIter
    con <- file(filename, open = "rb")
    size.res <- readBin(con, what = "integer", n = 1L)
    size.adj <- readBin(con, what = "integer", n = 1L)
    res <- readBin(con, what = "raw", n = size.res)
    data <- readBin(con, what = "double", n = nIteration * lengthIter)
    adj.ser <- readBin(con, what = "raw", n = size.adj)
    close(con)
    adjustments <- unserialize(adj.ser)
    betas0 <- lapply(namesBetas,
                     function(x) fetch(filename, c("model", "prior", x)))
    rescaleBetasPredHelper(priorsBetas = priorsBetas,
                           namesBetas = namesBetas,
                           skeletonsBetas = skeletonsBetas,
                           adjustments = adjustments,
                           prefixAdjustments = "model",
                           filename = filename,
                           nIteration = nIteration,
                           lengthIter = lengthIter)
    for (i in seq_along(betas0)) {
        name <- namesBetas[i]
        beta1 <- fetch(filename,
                       where = c("model", "prior", name))
        name.adj <- paste("model.prior", name, sep = ".")
        expect_equal(beta1, betas0[[i]] + adjustments[[name.adj]])
    }
})

test_that("rescaleInFilePred works", {
    rescaleInFilePred <- demest:::rescaleInFilePred
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename.est <- tempfile()
    filename.pred <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex + time),
                        time ~ Exch()),
                  y = y,
                  exposure = exposure,
                  nBurnin = 2,
                  nSim = 5,
                  nThin = 1,
                  nChain = 2,
                  filename = filename.est)
    predictModel(filenameEst = filename.est,
                 filenamePred = filename.pred,
                 n = 5)    
    results.est <- fetchResultsObject(filename.est)
    results.pred <- fetchResultsObject(filename.pred)
    nIteration.est <- results.est@mcmc["nIteration"]
    nIteration.pred <- results.pred@mcmc["nIteration"]
    lengthIter.est <- results.est@control$lengthIter
    lengthIter.pred <- results.pred@control$lengthIter
    namesBetas <- results.pred@final[[1]]@model@namesBetas
    betas0 <- lapply(namesBetas,
                     function(x) fetch(filename.pred, c("model", "prior", x)))
    rescaleInFilePred(filenameEst = filename.est,
                      filenamePred = filename.pred)
    con <- file(filename.est, open = "rb")
    size.res <- readBin(con, what = "integer", n = 1L)
    size.adj <- readBin(con, what = "integer", n = 1L)
    res <- readBin(con, what = "raw", n = size.res)
    data <- readBin(con, what = "double", n = nIteration.est * lengthIter.est)
    adj.ser.est <- readBin(con, what = "raw", n = size.adj)
    adjustments.est <- unserialize(adj.ser.est)
    close(con)
    con <- file(filename.pred, open = "rb")
    size.res <- readBin(con, what = "integer", n = 1L)
    size.adj <- readBin(con, what = "integer", n = 1L)
    res <- readBin(con, what = "raw", n = size.res)
    data <- readBin(con, what = "double", n = nIteration.pred * lengthIter.pred)
    adj.ser.pred <- readBin(con, what = "raw", n = size.adj)
    adjustments.pred <- unserialize(adj.ser.pred)
    close(con)
    expect_equal(adjustments.pred, adjustments.est)
    for (i in seq_along(betas0)) {
        name <- namesBetas[i]
        beta1 <- fetch(filename.pred,
                       where = c("model", "prior", name))
        name.adj <- paste("model.prior", name, sep = ".")
        if (name == "time")
            expect_equal(beta1, betas0[[i]] + adjustments.pred[[name.adj]])
        else
            expect_equal(beta1, betas0[[i]])
    }
})

test_that("rescaleInFile works", {
    rescaleInFile <- demest:::rescaleInFile
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 2,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    rescaleInFile(filename)
    results <- fetchResultsObject(filename)
    nIteration <- results@mcmc[["nIteration"]]
    lengthIter <- results@control$lengthIter
    con <- file(filename, open = "rb")
    size.res <- readBin(con, what = "integer", n = 1L)
    expect_identical(size.res, length(serialize(results, connection = NULL)))
    size.adj <- readBin(con, what = "integer", n = 1L)
    res <- readBin(con, what = "raw", n = size.res)
    data <- readBin(con, what = "double", n = nIteration * lengthIter)
    adj.ser <- readBin(con, what = "raw", n = size.adj)
    close(con)
    adj <- unserialize(adj.ser)
    expect_true(setequal(names(adj),
                         c("model.prior.(Intercept)",
                           "model.prior.age",
                           "model.prior.sex")))
})

test_that("R version of overwriteValuesOnFile works", {
    overwriteValuesOnFile <- demest:::overwriteValuesOnFile
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    original <- as.double(1:200)
    writeBin(original, con = con)
    close(con)
    object <- Values(array(as.double(1001:1100),
                           dim = c(5, 20),
                           dimnames = list(reg = 1:5, iter = 1:20)))
    nIteration <- 20L
    lengthIter <- 10L
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:5)))
    skeleton <- new("SkeletonManyValues",
                    first = 6L,
                    last = 10L,
                    metadata = metadata)
    overwriteValuesOnFile(object = object,
                    skeleton = skeleton,
                    filename = filename,
                    nIteration = nIteration,
                    lengthIter = lengthIter,
                    useC = FALSE)
    con <- file(filename, open = "rb")
    readBin(con = con, what = "integer", n = 2L)
    readBin(con = con, what = "raw", n = length(results))
    ans.obtained <- readBin(con = con, what = "double", n = 200L)
    close(con)
    ans.expected <- matrix(original, nr = 10)
    ans.expected[6:10, ] <- object@.Data
    ans.expected <- as.double(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("C version of overwriteValuesOnFile works", {
    overwriteValuesOnFile <- demest:::overwriteValuesOnFile
    for (range in list(c(6L, 10L), c(1L, 5L), c(2L, 6L))) {
        filename <- tempfile()
        con <- file(filename, open = "wb")
        results <- new("ResultsModelEst")
        results <- serialize(results, connection = NULL)
        writeBin(length(results), con = con) # size results
        writeBin(10L, con = con) # size adjustments
        writeBin(results, con = con)
        original <- as.double(1:200)
        writeBin(original, con = con)
        close(con)
        object <- Values(array(as.double(1001:1100),
                               dim = c(5, 20),
                               dimnames = list(reg = 1:5, iter = 1:20)))
        nIteration <- 20L
        lengthIter <- 10L
        metadata <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals", dimvalues = 0:5)))
        skeleton <- new("SkeletonManyValues",
                        first = range[1],
                        last = range[2],
                        metadata = metadata)
        overwriteValuesOnFile(object = object,
                              skeleton = skeleton,
                              filename = filename,
                              nIteration = nIteration,
                              lengthIter = lengthIter,
                              useC = TRUE)
        con <- file(filename, open = "rb")
        readBin(con = con, what = "integer", n = 2L)
        readBin(con = con, what = "raw", n = length(results))
        ans.obtained <- readBin(con = con, what = "double", n = 200L)
        close(con)
        ans.expected <- matrix(original, nr = 10)
        ans.expected[seq(from = range[1], to = range[2]), ] <- object@.Data
        ans.expected <- as.double(ans.expected)
        expect_identical(ans.obtained, ans.expected)
    }
})


test_that("recordAdjustments works", {
    recordAdjustments <- demest:::recordAdjustments
    ## both priors Exchangeable; nothing in 'adjustments'
    priorHigh <- new("ExchNormZero")
    priorLow <- new("ExchNormZero")
    namesHigh <- c("country", "sex")
    namesLow <- "country"
    adj <- ValuesOne(as.numeric(1:3), labels = letters[1:3], name = "country")
    adjustments <- new.env(hash = TRUE)
    prefixAdjustments <- "model"    
    recordAdjustments(priorHigh = priorHigh,
                      priorLow = priorLow,
                      namesHigh = namesHigh,
                      namesLow = namesLow,
                      adj = adj,
                      adjustments = adjustments,
                      prefixAdjustments = prefixAdjustments)
    expect_identical(adjustments[["model.prior.country:sex"]], -1 * adj)
    expect_identical(adjustments[["model.prior.country"]], adj)
    ## neither priors Exchangeable; nothing in 'adjustments'
    priorHigh <- new("DLMNoTrendNormZeroNoSeason")
    priorLow <- new("DLMNoTrendNormZeroNoSeason")
    namesHigh <- c("country", "sex")
    namesLow <- "country"
    adj <- ValuesOne(as.numeric(1:3), labels = letters[1:3], name = "country")
    adjustments <- new.env(hash = TRUE)
    prefixAdjustments <- "model"    
    recordAdjustments(priorHigh = priorHigh,
                      priorLow = priorLow,
                      namesHigh = namesHigh,
                      namesLow = namesLow,
                      adj = adj,
                      adjustments = adjustments,
                      prefixAdjustments = prefixAdjustments)
    expect_null(adjustments[["model.prior.country:sex"]])
    expect_null(adjustments[["model.prior.country"]])
    ## second term Exchangeable; something in 'adjustments'
    priorHigh <- new("DLMNoTrendNormZeroNoSeason")
    priorLow <- new("ExchNormZero")
    namesHigh <- c("country", "sex")
    namesLow <- "country"
    adj <- ValuesOne(as.numeric(1:3), labels = letters[1:3], name = "country")
    adjustments <- new.env(hash = TRUE)
    adjustments[["model.prior.country:sex"]] <- 1
    adjustments[["model.prior.country"]] <- 1
    prefixAdjustments <- "model"    
    recordAdjustments(priorHigh = priorHigh,
                      priorLow = priorLow,
                      namesHigh = namesHigh,
                      namesLow = namesLow,
                      adj = adj,
                      adjustments = adjustments,
                      prefixAdjustments = prefixAdjustments)
    expect_identical(adjustments[["model.prior.country:sex"]], 1)
    expect_identical(adjustments[["model.prior.country"]], 1 + adj)
})

test_that("rescalePriorsHelper works with Exchangeable", {
    rescalePriorsHelper <- demest:::rescalePriorsHelper
    makeOutputPrior <- demest:::makeOutputPrior
    initialPrior <- demest:::initialPrior
    SkeletonBetaTerm <- demest:::SkeletonBetaTerm
    SkeletonBetaIntercept <- demest:::SkeletonBetaIntercept
    spec.int <- ExchFixed()
    spec.country <- Exch()
    spec.sex <- Exch()
    spec.country.sex <- Exch()
    beta.int <- rnorm(1)
    beta.country <- rnorm(5)
    beta.sex <- rnorm(2)
    beta.country.sex <- rnorm(10)
    metadata <- new("MetaData",
                    nms = c("country", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                     new("Sexes", dimvalues = c("F", "M"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 2),
                                   dimnames = list(country = letters[1:5],
                                                   sex = c("F", "M"))))
    prior.int <- initialPrior(spec.int,
                              beta = beta.int,
                              metadata = NULL,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 0L,
                              strucZeroArray = strucZeroArray)
    prior.country <- initialPrior(spec.country,
                                  beta = beta.country,
                                  metadata = metadata[1],
                                  sY = NULL,
                                  isSaturated = FALSE,
                                  margin = 1L,
                                  strucZeroArray = strucZeroArray)
    prior.sex <- initialPrior(spec.sex,
                              beta = beta.sex,
                              metadata = metadata[2],
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 2L,
                              strucZeroArray = strucZeroArray)
    prior.country.sex <- initialPrior(spec.country.sex,
                                      beta = beta.country.sex,
                                      metadata = metadata,
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 1:2,
                                      strucZeroArray = strucZeroArray)
    priors <- list(prior.int,
                   prior.country,
                   prior.sex,
                   prior.country.sex)
    margins <- list(0L, 1L, 2L, 1:2)
    skeletonsBetas <- list(SkeletonBetaIntercept(first = 10L),
                           SkeletonBetaTerm(first = 11L,
                                            metadata = metadata[1L]),
                           SkeletonBetaTerm(first = 16L,
                                            metadata = metadata[2L]),
                           SkeletonBetaTerm(first = 18L,
                                            metadata = metadata))
    skeletonsPriors <- list(makeOutputPrior(priors[[1]],
                                            metadata = NULL,
                                            pos = 28L),
                            makeOutputPrior(priors[[2]],
                                            metadata = metadata[1],
                                            pos = 29L),
                            makeOutputPrior(priors[[3]],
                                            metadata = metadata[2],
                                            pos = 30L),
                            makeOutputPrior(priors[[4]],
                                            metadata = metadata,
                                            pos = 31L))
    adjustments <- new.env(hash = TRUE)
    prefixAdjustments <- "model"
    nIteration <- 20L
    lengthIter <- 100L
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    data <- as.double(1:2000)
    writeBin(data, con = con)
    close(con)
    prefix.adjustments <- "model"
    rescalePriorsHelper(priors = priors,
                        margins = margins,
                        skeletonsBetas = skeletonsBetas,
                        skeletonsPriors = skeletonsPriors,
                        adjustments = adjustments,
                        prefixAdjustments = prefix.adjustments,
                        filename = filename,
                        nIteration = nIteration,
                        lengthIter = lengthIter)
    expect_true(setequal(names(adjustments),
                         paste("model.prior",
                               c("(Intercept)", "country", "sex", "country:sex"),
                               sep = ".")))
})

test_that("setCoefInterceptToZeroOnFile works", {
    setCoefInterceptToZeroOnFile <- demest:::setCoefInterceptToZeroOnFile
    filename <- tempfile()
    con <- file(filename, open = "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    writeBin(length(results), con = con) # size results
    writeBin(10L, con = con) # size adjustments
    writeBin(results, con = con)
    original <- as.double(1:200)
    writeBin(original, con = con)
    close(con)
    nIteration <- 20L
    lengthIter <- 10L
    metadata <- new("MetaData",
                    nms = "coef",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b"))))
    skeleton <- new("SkeletonCovariates",
                    first = 6L,
                    last = 8L,
                    metadata = metadata)
    setCoefInterceptToZeroOnFile(skeleton = skeleton,
                                 filename = filename,
                                 nIteration = nIteration,
                                 lengthIter = lengthIter)
    con <- file(filename, open = "rb")
    readBin(con = con, what = "integer", n = 2L)
    readBin(con = con, what = "raw", n = length(results))
    ans.obtained <- readBin(con = con, what = "double", n = 200L)
    close(con)
    ans.expected <- matrix(original, nr = 10)
    ans.expected[6, ] <- 0
    ans.expected <- as.double(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})


