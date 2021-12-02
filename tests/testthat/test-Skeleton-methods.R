
context("Skeleton-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


## classY ############################################################################

test_that("classY works with object of class SkeletonManyCounts", {
    classY <- demest:::classY
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    y <- new("SkeletonManyCounts",
             metadata = metadata,
             first = 3L,
             last = 5L)
    expect_identical(classY(y), "Counts")
})

test_that("fetchResults works with object of class SkeletonManyValues", {
    classY <- demest:::classY
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    y <- new("SkeletonManyValues",
             metadata = metadata,
             first = 3L,
             last = 5L)
    expect_identical(classY(y), "Values")
})



## fetchResults ######################################################################

test_that("default method for fetchResults works", {
    fetchResults <- demest:::fetchResults
    expect_identical(fetchResults(1:5, iterations = NULL), 1:5)
})


test_that("fetchResults works with object of class SkeletonOneCounts", {
    fetchResults <- demest:::fetchResults
    object <- new("SkeletonOneCounts", first = 3L)
    nameObject <- "obj"
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:100), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 10L)
    ans.expected <- Counts(array(as.double(seq.int(from = 3, by = 10, length = 10)),
                                 dim = 10,
                                 dimnames = list(iteration = 1:10)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonOneValues", {
    fetchResults <- demest:::fetchResults
    object <- new("SkeletonOneValues", first = 3L)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:100), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 10L)
    ans.expected <- Values(array(as.double(seq.int(from = 3, by = 10, length = 10)),
                                 dim = 10,
                                 dimnames = list(iteration = 1:10)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonManyCounts", {
    fetchResults <- demest:::fetchResults
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    object <- new("SkeletonManyCounts",
                  metadata = metadata,
                  first = 3L,
                  last = 5L)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:100), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 10L)
    ans.expected <- 3:5 + rep((0:9) * 10, each = 3)
    ans.expected <- Counts(array(ans.expected,
                                 dim = c(3, 10),
                                 dimnames = list(region = c("a", "b", "c"), iteration = 1:10)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonManyValues", {
    fetchResults <- demest:::fetchResults
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    object <- new("SkeletonManyValues",
                  metadata = metadata,
                  first = 3L,
                  last = 5L)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:100), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 10L)
    ans.expected <- 3:5 + rep((0:9) * 10, each = 3)
    ans.expected <- Values(array(ans.expected,
                                 dim = c(3, 10),
                                 dimnames = list(region = c("a", "b", "c"), iteration = 1:10)))
    expect_identical(ans.obtained, ans.expected)
})


test_that("fetchResults works with object of class SkeletonBetaIntercept", {
    fetchResults <- demest:::fetchResults
    object <- new("SkeletonBetaIntercept",
                  first = 3L,
                  last = 3L)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L)
    beta <- 3 + (0:9) * 100
    metadata <- new("MetaData",
                    nms = "iteration",
                    dimtypes = "iteration",
                    DimScales = list(new("Iterations", dimvalues = 1:10)))
    .Data = array(beta, dim = dim(metadata), dimnames = dimnames(metadata))
    ans.expected <- new("Values", .Data = .Data, metadata = metadata)
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonBetaTerm", {
    fetchResults <- demest:::fetchResults
    sweepAllMargins <- demest:::sweepAllMargins
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c"))))
    object <- new("SkeletonBetaTerm",
                  metadata = metadata,
                  first = 3L,
                  last = 5L)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L)
    beta <- 3:5 + rep((0:9) * 100, each = 3)
      metadata <- new("MetaData",
        nms = c("region", "iteration"),
        dimtypes = c("state", "iteration"),
        DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
            new("Iterations", dimvalues = 1:10)))
    .Data = array(beta, dim = dim(metadata), dimnames = dimnames(metadata))
    ans.expected <- new("Values", .Data = .Data, metadata = metadata)
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMu", {
    fetchResults <- demest:::fetchResults
    SkeletonMu <- demest:::SkeletonMu
    sweepAllMargins <- demest:::sweepAllMargins
    metadata <- new("MetaData",
                    nms = c("region", "age", "sex"),
                    dimtypes = c("state", "age", "sex"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
                        new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                        new("Sexes", dimvalues = c("f", "m"))))
    ## term for every dimension
    betas <- list("(Intercept)" = 0,
                  region = rnorm(3),
                  age = rnorm(3),
                  sex = rnorm(2),
                  "age:sex" = rnorm(6))
    margins <- list(0L, 1L, 2L, 3L, 2:3)
    object <- SkeletonMu(betas = betas,
                         margins = margins,
                         first = 11L,
                         metadata = metadata)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L)
    obj.intercept <- new("SkeletonBetaIntercept",
                         first = 11L,
                         last = 11L)
    obj.region <- new("SkeletonBetaTerm",
                      metadata = metadata[1],
                      first = 12L,
                      last = 14L)
    obj.age <- new("SkeletonBetaTerm",
                   metadata = metadata[2],
                   first = 15L,
                   last = 17L)
    obj.sex <- new("SkeletonBetaTerm",
                   metadata = metadata[3],
                   first = 18L,
                   last = 19L)
    obj.age.sex <- new("SkeletonBetaTerm",
                       metadata = metadata[2:3],
                       first = 20L,
                       last = 25L)
    intercept <- fetchResults(object = obj.intercept,
                              nameObject = "obj",
                              filename = filename,
                              iterations = 1:10,
                              nIteration = 10L,
                              lengthIter = 100L)
    region <- fetchResults(object = obj.region,
                           nameObject = "obj",
                           filename = filename,
                           iterations = 1:10,
                           nIteration = 10L,
                           lengthIter = 100L)
    age <- fetchResults(object = obj.age,
                        nameObject = "obj",
                        filename = filename,
                        iterations = 1:10,
                        nIteration = 10L,
                        lengthIter = 100L)
    sex <- fetchResults(object = obj.sex,
                        nameObject = "obj",
                        filename = filename,
                        iterations = 1:10,
                        nIteration = 10L,
                        lengthIter = 100L)
    age.sex <- fetchResults(object = obj.age.sex,
                            nameObject = "obj",
                            filename = filename,
                            iterations = 1:10,
                            nIteration = 10L,
                            lengthIter = 100L)
    ans.expected <- intercept + region + age + sex + age.sex
    ans.expected <- aperm(ans.expected, perm = names(ans.obtained))
    expect_identical(ans.obtained, ans.expected)
    ## term for only one dimension
    betas <- list("(Intercept)" = 0,
                  age = rnorm(3))
    margins <- list(0L, 2L)
    object <- SkeletonMu(betas = betas,
                         margins = margins,
                         first = 11L,
                         metadata = metadata)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L)
    obj.intercept <- new("SkeletonBetaIntercept",
                         first = 11L,
                         last = 11L)
    obj.age <- new("SkeletonBetaTerm",
                   metadata = metadata[2],
                   first = 12L,
                   last = 14L)
    intercept <- fetchResults(object = obj.intercept,
                              nameObject = "obj",
                              filename = filename,
                              iterations = 1:10,
                              nIteration = 10L,
                              lengthIter = 100L)
    age <- fetchResults(object = obj.age,
                        nameObject = "obj",
                        filename = filename,
                        iterations = 1:10,
                        nIteration = 10L,
                        lengthIter = 100L)
    sex.region <- Values(array(0, ## used only to get metadata right
                               dim = c(3, 2, 10),
                               dimnames = list(region = c("a", "b", "c"),
                                   sex = c("f", "m"),
                                   iteration = 1:10)))
    ans.expected <- intercept + age + sex.region
    ans.expected <- aperm(ans.expected, perm = names(ans.obtained))
    expect_identical(ans.obtained, ans.expected)
})



test_that("fetchResults works with object of class SkeletonCovariates", {
    fetchResults <- demest:::fetchResults
    metadata <- new("MetaData",
                    nms = "covariate",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("income", "education"))))
    object <- new("SkeletonCovariates",
                  metadata = metadata,
                  first = 3L,
                  last = 5L)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 10L)
    ans.expected <- 4:5 + rep((0:9) * 10, each = 2)
    ans.expected <- Values(array(ans.expected,
                                 dim = c(2, 10),
                                 dimnames = list(covariate = c("income", "education"), iteration = 1:10)))
    expect_identical(ans.obtained, ans.expected)
})


test_that("fetchResults works with object of class SkeletonStateDLM - Level", {
    fetchResults <- demest:::fetchResults
    sweepAllMargins <- demest:::sweepAllMargins
    metadata <- new("MetaData",
                    nms = c("region", "time"),
                    dimtypes = c("state", "time"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
                                     new("Points", dimvalues = as.numeric(1:10))))
    object <- new("SkeletonStateDLM",
                  metadata = metadata,
                  iAlong = 1L,
                  first = 11L,
                  last = 43L,
                  indicesShow = 4:33)
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:2000), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 200L)
    ans.expected <- 14:43 + rep(0:9, each = 30) * 200
    ans.expected <- Values(array(ans.expected,
                                 dim = c(3, 10, 10),
                                 dimnames = list(region = c("a", "b", "c"),
                                     time = 1:10,
                                     iteration = 1:10)),
                           dimscales = c(time = "Points"))
    expect_identical(ans.obtained, ans.expected)
})
    
test_that("fetchResults works with object of class SkeletonStateDLM - Trend", {
    fetchResults <- demest:::fetchResults
    metadata <- new("MetaData",
                    nms = c("region", "time"),
                    dimtypes = c("state", "time"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
                                     new("Points", dimvalues = as.numeric(1:10))))
    object <- new("SkeletonStateDLM",
                  metadata = metadata,
                  iAlong = 1L,
                  first = 41L,
                  last = 73L,
                  indicesShow = c(2:11, 13:22, 24:33))
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L)
    ans.expected <- c(42:51, 53:62, 64:73) + rep((0:9) * 100.0, each = 30)
    ans.expected <- Values(array(ans.expected,
                                 dim = c(3, 10, 10),
                                 dimnames = list(region = c("a", "b", "c"),
                                     time = 1:10,
                                     iteration = 1:10)),
                           dimscales = c(time = "Points"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonStateDLM - Season", {
    fetchResults <- demest:::fetchResults
    sweepAllMargins <- demest:::sweepAllMargins
    metadata <- new("MetaData",
                    nms = c("region", "time"),
                    dimtypes = c("state", "time"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
                                     new("Points", dimvalues = as.numeric(1:10))))
    object <- new("SkeletonStateDLM",
                  metadata = metadata,
                  first = 11L,
                  last = 76L,
                  iAlong = 1L,
                  indicesShow = c(seq.int(from = 7L, by = 2L, to = 65L)))
    nameObject <- "obj"
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L)
    ans.expected <- seq(from = 17L, by = 2L, to = 75L) + rep((0:9) * 100.0, each = 30)
    norm.fac <- seq(from = 57.5, by = 2, to = 115.5) + rep(0:9, each = 30) * 200
    ans.expected <- Values(array(ans.expected,
                                 dim = c(3, 10, 10),
                                 dimnames = list(region = c("a", "b", "c"),
                                     time = 1:10,
                                     iteration = 1:10)),
                           dimscales = c(time = "Points"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonAccept", {
    set.seed(1L)
    fetchResults <- demest:::fetchResults
    object <- new("SkeletonAccept",
                  first = 10L,
                  iFirstInChain = c(1L, 6L))
    nameObject <- "obj"
    filename <- tempfile()
    accept <- rep(c(0, 1), times = 5)
    data <- matrix(rnorm(90), ncol = 10)
    data <- as.double(rbind(data, accept))
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    adjustments <- new.env(hash = TRUE)
    results <- serialize(results, connection = NULL)
    adjustments <- serialize(adjustments, connection = NULL)
    size.results <- length(results)
    size.adjustments <- length(adjustments)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(data, con)
    writeBin(adjustments, con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 10L)
    ans.expected <- as.logical(accept)[-c(1, 6)]
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonNAccept", {
    set.seed(1L)
    fetchResults <- demest:::fetchResults
    object <- new("SkeletonNAccept",
                  nAttempt = 20L,
                  first = 10L,
                  iFirstInChain = c(1L, 6L))
    nameObject <- "obj"
    filename <- tempfile()
    n.accept <- as.double(sample.int(20, size = 10))
    data <- matrix(rnorm(90), ncol = 10)
    data <- as.double(rbind(data, n.accept))
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(data, con)
    writeBin(1, con)
    close(con)
    ans.obtained <- fetchResults(object = object,
                                 nameObject = "obj",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 10L)
    ans.expected <- n.accept[-c(1, 6)] / 20
    expect_identical(ans.obtained, ans.expected)
})


## Demographic objects with missing data

test_that("SkeletonMissingData creates valid object of class SkeletonMissingDataNormalVarsigmaKnown", {
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("NormalVaryingVarsigmaKnown")
    model@w <- rep(0.3, 6)
    model@varsigma <- new("Scale", 0.6)
    outputModel <- list(likelihood = list(mean = Skeleton(first = 1L, object = object)))
    ans.obtained <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = NULL)
    ans.expected <- new("SkeletonMissingDataNormalVarsigmaKnown",
                        data = object,
                        varsigma = new("Scale", 0.6),
                        w = rep(0.3, 6),
                        offsetsTheta = new("Offsets", c(1L, 6L)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDataNormalVarsigmaKnown", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("NormalVaryingVarsigmaKnown")
    model@w <- rep(0.3, 6)
    model@varsigma <- new("Scale", 0.6)
    outputModel <- list(likelihood = list(mean = Skeleton(first = 1L, object = object)))
    skeleton <- SkeletonMissingData(object = object,
                                        model = model,
                                        outputModel = outputModel,
                                        exposure = NULL)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    mean <- seq.int(from = 6, by = 100, length = 10)
    ans.expected[ 2, 3, ] <- rnorm(n = 10, mean = mean, sd = skeleton@varsigma)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDataNormalVarsigmaUnknown", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("NormalVaryingVarsigmaUnknown")
    model@w <- rep(0.3, 6)
    outputModel <- list(likelihood = list(mean = Skeleton(first = 1L, object = object),
                                          sd = Skeleton(first = 7L)))
    skeleton <- SkeletonMissingData(object = object,
                                    model = model,
                                    outputModel = outputModel,
                                    exposure = NULL)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    mean <- seq.int(from = 6, by = 100, length = 10)
    sd <- seq.int(from = 7, by = 100, length = 10)
    ans.expected[ 2, 3, ] <- rnorm(n = 10, mean = mean, sd = sd)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDataPoissonNotUseExp", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("PoissonVaryingNotUseExp")
    outputModel <- list(likelihood = list(count = Skeleton(first = 1L, object = object)))
    skeleton <- SkeletonMissingData(object = object,
                                    model = model,
                                    outputModel = outputModel,
                                    exposure = NULL)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    lambda <- seq.int(from = 6, by = 100, length = 10)
    ans.expected[ 2, 3, ] <- rpois(n = 10, lambda = lambda)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDataPoissonNotUseExpSubtotals", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:20, rep(NA, 5)),
                           dim = c(5, 5),
                           dimnames = list(region = c("a", "b", "c", "d", "e"),
                               age = 0:4)))
    subtotals <- CountsOne(values = 100L, name = "age", labels = "4")
    object <- attachSubtotals(object, subtotals = subtotals)
    model <- new("PoissonVaryingNotUseExp")
    outputModel <- list(likelihood = list(count = Skeleton(first = 1L, object = object)))
    skeleton <- SkeletonMissingData(object = object,
                                    model = model,
                                    outputModel = outputModel,
                                    exposure = NULL)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = NULL,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:20, rep(NA, 5)),
                                 dim = c(5, 5, 10),
                                 dimnames = list(region = c("a", "b", "c", "d", "e"),
                                     age = 0:4,
                                     iteration = 1:10)))
    theta <- matrix((21:25) + 100 * rep(0:9, each = 5), ncol = 10)
    for (i in 1:10)
        ans.expected[ , 5, i] <- rmultinom(n = 1, size = subtotals, prob = theta[ , i])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDataPoissonUseExp", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    exposure <- Counts(array(2:7,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    model <- new("PoissonVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 1L, object = object)))
    skeleton <- SkeletonMissingData(object = object,
                                    model = model,
                                    outputModel = outputModel,
                                    exposure = exposure)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    theta <- seq.int(from = 6, by = 100, length = 10)
    lambda <- 7 * theta
    ans.expected[ 2, 3, ] <- rpois(n = 10, lambda = lambda)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDataPoissonUseExpSubtotals", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:20, rep(NA, 5)),
                           dim = c(5, 5),
                           dimnames = list(region = c("a", "b", "c", "d", "e"),
                               age = 0:4)))
    subtotals <- CountsOne(values = 100L, name = "age", labels = "4")
    object <- attachSubtotals(object, subtotals = subtotals)
    exposure <- Counts(array(2:26,
                             dim = c(5, 5),
                             dimnames = list(region = c("a", "b", "c", "d", "e"),
                                 age = 0:4)))
    model <- new("PoissonVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 1L, object = object)))
    skeleton <- SkeletonMissingData(object = object,
                                    model = model,
                                    outputModel = outputModel,
                                    exposure = exposure)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:20, rep(NA, 5)),
                                 dim = c(5, 5, 10),
                                 dimnames = list(region = c("a", "b", "c", "d", "e"),
                                     age = 0:4,
                                     iteration = 1:10)))
    theta <- matrix((21:25) + 100 * rep(0:9, each = 5), ncol = 10)
    lambda <- (22:26) * theta
    for (i in 1:10)
        ans.expected[ , 5, i] <- rmultinom(n = 1, size = subtotals, prob = lambda[ , i])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDataCMPNotUseExp", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    rcmp1 <- demest:::rcmp1
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    model <- new("CMPVaryingNotUseExp")
    outputModel <- list(likelihood = list(count = Skeleton(first = 1L, object = object),
                                          dispersion = Skeleton(first = 7L, object = object)))
    skeleton <- SkeletonMissingData(object = object,
                                    model = model,
                                    outputModel = outputModel,
                                    exposure = NULL)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    mu <- seq.int(from = 6, by = 100, length = 10)
    nu <- seq.int(from = 12, by = 100, length = 10)
    for (i in 1:10)
        ans.expected[ 2, 3, i] <- rcmp1(mu = as.numeric(mu[i]),
                                       nu = as.numeric(nu[i]),
                                       maxAttempt = 1000L,
                                       useC = TRUE)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("fetchResults works with object of class SkeletonMissingDataCMPUseExp", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    rcmp1 <- demest:::rcmp1
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                                           age = 0:2)))
    exposure <- Counts(array(10,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                             age = 0:2)))
    model <- new("CMPVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 1L, object = object),
                                          dispersion = Skeleton(first = 7L, object = object)))
    skeleton <- SkeletonMissingData(object = object,
                                    model = model,
                                    outputModel = outputModel,
                                    exposure = exposure)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                                 age = 0:2,
                                                 iteration = 1:10)))
    mu <- seq.int(from = 6, by = 100, length = 10)
    nu <- seq.int(from = 12, by = 100, length = 10)
    for (i in 1:10)
        ans.expected[ 2, 3, i] <- rcmp1(mu = as.numeric(mu[i]) * exposure[6],
                                        nu = as.numeric(nu[i]),
                                        maxAttempt = 1000L,
                                        useC = TRUE)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})



test_that("fetchResults works with object of class SkeletonMissingDataBinomial", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingData <- demest:::SkeletonMissingData
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    exposure <- Counts(array(2:7,
                             dim = 2:3,
                             dimnames = list(sex = c("f", "m"),
                                 age = 0:2)))
    model <- new("BinomialVarying")
    outputModel <- list(likelihood = list(prob = Skeleton(first = 1L, object = object)))
    skeleton <- SkeletonMissingData(object = object,
                                    model = model,
                                    outputModel = outputModel,
                                    exposure = exposure)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000)/1000, con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    theta <- seq.int(from = 0.006, by = 0.1, length = 10)
    ans.expected[ 2, 3, ] <- rbinom(n = 10, size = 7, prob = theta)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDatasetPoissonUseExp", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    y <- Counts(array(1:12,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("PoissonVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 21L, object = object)))
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    skeleton <- SkeletonMissingDataset(object = object,
                                       model = model,
                                       outputModel = outputModel,
                                       skeletonComponent = skeletonComponent,
                                       transformComponent = transformComponent)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    theta <- seq.int(from = 26, by = 100, length = 10)
    exposure <- seq.int(from = 6, by = 100, length = 10) + seq.int(from = 12, by = 100, length = 10)
    lambda <- theta * exposure
    ans.expected[ 2, 3, ] <- rpois(n = 10, lambda = lambda)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDatasetPoissonSubtotals", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:20, rep(NA, 5)),
                           dim = c(5, 5),
                           dimnames = list(region = c("a", "b", "c", "d", "e"),
                               age = 0:4)))
    subtotals <- CountsOne(values = 100L, name = "age", labels = "4")
    object <- attachSubtotals(object, subtotals = subtotals)
    y <- Counts(array(1:50,
                      dim = c(5, 5, 2),
                      dimnames = list(region = c("a", "b", "c", "d", "e"),
                          age = 0:4, sex = c("f", "m"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("PoissonVaryingUseExp")
    outputModel <- list(likelihood = list(rate = Skeleton(first = 51L, object = object)))
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    skeleton <- SkeletonMissingDataset(object = object,
                                       model = model,
                                       outputModel = outputModel,
                                       skeletonComponent = skeletonComponent,
                                       transformComponent = transformComponent)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:20, rep(NA, 5)),
                                 dim = c(5, 5, 10),
                                 dimnames = list(region = c("a", "b", "c", "d", "e"),
                                     age = 0:4,
                                     iteration = 1:10)))
    theta <- matrix((71:75) + 100 * rep(0:9, each = 5), ncol = 10)
    exposure <- matrix((21:25) + 100 * rep(0:9, each = 5), ncol = 10) +
        matrix((46:50) + 100 * rep(0:9, each = 5), ncol = 10)
    lambda <- theta * exposure
    for (i in 1:10)
        ans.expected[ , 5, i] <- rmultinom(n = 1, size = subtotals, prob = lambda[ , i])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDatasetBinomial", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    y <- Counts(array(1:12,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("BinomialVarying")
    outputModel <- list(likelihood = list(prob = Skeleton(first = 21L, object = object)))
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    skeleton <- SkeletonMissingDataset(object = object,
                                       model = model,
                                       outputModel = outputModel,
                                       skeletonComponent = skeletonComponent,
                                       transformComponent = transformComponent)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    out <- matrix(1:1000, ncol = 10)
    out[21:26,] <- out[21:26,]/1000
    out <- as.double(out)
    writeBin(out, con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    theta <- seq.int(from = 26, by = 100, length = 10)/1000
    exposure <- seq.int(from = 6, by = 100, length = 10) + seq.int(from = 12, by = 100, length = 10)
    ans.expected[ 2, 3, ] <- rbinom(n = 10, size = exposure, prob = theta)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("fetchResults works with object of class SkeletonMissingDatasetPoissonBinomial", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(1:5, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                               age = 0:2)))
    y <- Counts(array(1:12,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("PoissonBinomialMixture", prob = 0.98)
    outputModel <- list(prob = 0.98)
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    skeleton <- SkeletonMissingDataset(object = object,
                                       model = model,
                                       outputModel = outputModel,
                                       skeletonComponent = skeletonComponent,
                                       transformComponent = transformComponent)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(1:5, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                     age = 0:2,
                                     iteration = 1:10)))
    exposure <- seq.int(from = 6, by = 100, length = 10) + seq.int(from = 12, by = 100, length = 10)
    ans.expected[ 2, 3, ] <- rbinom(n = 10, size = exposure, prob = 0.98) +
        rpois(n = 10, lambda = 0.02 * exposure)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("fetchResults works with object of class SkeletonMissingDatasetRound3", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    object <- Counts(array(c(3L, NA),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                                           age = 0:2)))
    y <- Counts(array(3L,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                                      age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    model <- new("Round3")
    outputModel <- list("<none>" = NULL)
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    skeleton <- SkeletonMissingDataset(object = object,
                                       model = model,
                                       outputModel = outputModel,
                                       skeletonComponent = skeletonComponent,
                                       transformComponent = transformComponent)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(3L, NA),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                                 age = 0:2,
                                                 iteration = 1:10)))
    exposure <- 1:12 + rep(seq.int(from = 0, by = 100, length = 10), each = 12)
    exposure <- Counts(array(exposure,
                             dim = c(2:3, 2, 10),
                             dimnames = list(sex = c("f", "m"),
                                             age = 0:2,
                                             region = c("a", "b"),
                                             iteration = 1:10)))
    exposure <- collapseDimension(exposure, dim = "region")
    ans.expected[ 2, , ] <- dembase:::round3(exposure[2, , ])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("fetchResults works with object of class SkeletonMissingDatasetNormalFixedUseExp", {
    fetchResults <- demest:::fetchResults
    SkeletonMissingDataset <- demest:::SkeletonMissingDataset
    Skeleton <- demest:::Skeleton
    initialModel <- demest:::initialModel
    makeOutputModel <- demest:::makeOutputModel
    mean <- Values(array(1:6,
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                                         age = 0:2)))
    object <- Counts(array(c(NA, 1L),
                           dim = 2:3,
                           dimnames = list(sex = c("f", "m"),
                                           age = 0:2)))
    y <- Counts(array(3L,
                      dim = c(2:3, 2),
                      dimnames = list(sex = c("f", "m"),
                                      age = 0:2, region = c("a", "b"))))
    transformComponent <- makeTransform(x = y, y = object)
    spec <- Model(dataset ~ NormalFixed(mean = mean, sd = 1, useExpose = TRUE))
    model <- initialModel(spec, y = object, exposure = collapseDimension(y, dim = "region"))
    outputModel <- list()
    skeletonComponent <-  Skeleton(first = 1L, object = y)
    skeleton <- SkeletonMissingDataset(object = object,
                                       model = model,
                                       outputModel = outputModel,
                                       skeletonComponent = skeletonComponent,
                                       transformComponent = transformComponent)
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con)
    writeBin(10L, con)
    writeBin(results, con)
    writeBin(as.double(1:1000), con)
    close(con)
    ## impute is FALSE
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = FALSE)
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## impute is TRUE
    set.seed(1)
    ans.obtained <- fetchResults(object = skeleton,
                                 nameObject = "y",
                                 filename = filename,
                                 iterations = 1:10,
                                 nIteration = 10L,
                                 lengthIter = 100L,
                                 impute = TRUE)
    set.seed(1)
    ans.expected <- Counts(array(c(NA, 1L),
                                 dim = c(2:3, 10),
                                 dimnames = list(sex = c("f", "m"),
                                                 age = 0:2,
                                                 iteration = 1:10)))
    exposure <- 1:12 + rep(seq.int(from = 0, by = 100, length = 10), each = 12)
    exposure <- Counts(array(exposure,
                             dim = c(2:3, 2, 10),
                             dimnames = list(sex = c("f", "m"),
                                             age = 0:2,
                                             region = c("a", "b"),
                                             iteration = 1:10)))
    exposure <- collapseDimension(exposure, dim = "region")
    ans.expected[ 1, , ] <- rnorm(n = 30, mean = ((1:6) * exposure)[1, , ], sd = 1)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})



test_that("fetchResults works with object of class SkeletonMissingDatasetLN2 - varsigma updated - add1 = TRUE", {
  fetchResults <- demest:::fetchResults
  SkeletonMissingDataset <- demest:::SkeletonMissingDataset
  Skeleton <- demest:::Skeleton
  initialModel <- demest:::initialModel
  getDataFromFile <- demest:::getDataFromFile
  addIterationsToTransform <- demest:::addIterationsToTransform
  object <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                                         age = 0:2)))
  y <- Counts(array(1:12,
                    dim = c(2:3, 2),
                    dimnames = list(sex = c("f", "m"),
                                    age = 0:2, region = c("a", "b"))))
  transformComponent <- makeTransform(x = y, y = object)
  constraints <- Values(array(NA_real_, dim = 2, dimnames = list(sex = c("f", "m"))))
  model <- Model(y ~ LN2(constraints))
  model <- initialModel(model, y = object, exposure = object + 1)
  outputModel <- list(likelihood = list(mean = Skeleton(first = 20L,
                                                        object = constraints,
                                                        margin = 1L),
                                        sd = Skeleton(first = 22L)))
  skeletonComponent <-  Skeleton(first = 1L, object = y)
  skeleton <- SkeletonMissingDataset(object = object,
                                     model = model,
                                     outputModel = outputModel,
                                     skeletonComponent = skeletonComponent,
                                     transformComponent = transformComponent)
  filename <- tempfile()
  con <- file(filename, "wb")
  results <- new("ResultsModelEst")
  results <- serialize(results, connection = NULL)
  size.results <- length(results)
  writeBin(size.results, con)
  writeBin(10L, con)
  writeBin(results, con)
  writeBin(as.double(1:1000), con)
  close(con)
  ## impute is FALSE
  ans.obtained <- fetchResults(object = skeleton,
                               nameObject = "y",
                               filename = filename,
                               iterations = 1:10,
                               nIteration = 10L,
                               lengthIter = 100L,
                               impute = FALSE)
  ans.expected <- object
  expect_identical(ans.obtained, ans.expected)
  ## impute is TRUE
  set.seed(1)
  ans.obtained <- fetchResults(object = skeleton,
                               nameObject = "y",
                               filename = filename,
                               iterations = 1:10,
                               nIteration = 10L,
                               lengthIter = 100L,
                               impute = TRUE)
  set.seed(1)
  ans.expected <- Counts(array(c(1:5, NA),
                               dim = c(2:3, 10),
                               dimnames = list(sex = c("f", "m"),
                                               age = 0:2,
                                               iteration = 1:10)))
  alpha.male <- 21 + seq(0, 900, 100)
  varsigma <- 22 + seq(0, 900, 100)
  exposure <- seq.int(from = 6, by = 100, length = 10) + seq.int(from = 12, by = 100, length = 10)
  mean <- log(exposure + 1) + alpha.male
  sd <- varsigma
  imputed <- exp(rnorm(n = 10, mean = mean, sd = sd)) - 1
  ans.expected[ 2, 3, ] <- imputed
  if (test.identity)
    expect_identical(ans.obtained, ans.expected)
  else
    expect_equal(ans.obtained, ans.expected)
})


test_that("fetchResults works with object of class SkeletonMissingDatasetLN2 - varsigma updated - add1 = FALSE", {
  fetchResults <- demest:::fetchResults
  SkeletonMissingDataset <- demest:::SkeletonMissingDataset
  Skeleton <- demest:::Skeleton
  initialModel <- demest:::initialModel
  getDataFromFile <- demest:::getDataFromFile
  addIterationsToTransform <- demest:::addIterationsToTransform
  object <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                                         age = 0:2)))
  y <- Counts(array(1:12,
                    dim = c(2:3, 2),
                    dimnames = list(sex = c("f", "m"),
                                    age = 0:2, region = c("a", "b"))))
  transformComponent <- makeTransform(x = y, y = object)
  constraints <- Values(array(NA_real_, dim = 2, dimnames = list(sex = c("f", "m"))))
  model <- Model(y ~ LN2(constraints, add1 = FALSE))
  model <- initialModel(model, y = object, exposure = object + 1)
  outputModel <- list(likelihood = list(mean = Skeleton(first = 20L,
                                                        object = constraints,
                                                        margin = 1L),
                                        sd = Skeleton(first = 22L)))
  skeletonComponent <-  Skeleton(first = 1L, object = y)
  skeleton <- SkeletonMissingDataset(object = object,
                                     model = model,
                                     outputModel = outputModel,
                                     skeletonComponent = skeletonComponent,
                                     transformComponent = transformComponent)
  filename <- tempfile()
  con <- file(filename, "wb")
  results <- new("ResultsModelEst")
  results <- serialize(results, connection = NULL)
  size.results <- length(results)
  writeBin(size.results, con)
  writeBin(10L, con)
  writeBin(results, con)
  writeBin(as.double(1:1000), con)
  close(con)
  ## impute is FALSE
  ans.obtained <- fetchResults(object = skeleton,
                               nameObject = "y",
                               filename = filename,
                               iterations = 1:10,
                               nIteration = 10L,
                               lengthIter = 100L,
                               impute = FALSE)
  ans.expected <- object
  expect_identical(ans.obtained, ans.expected)
  ## impute is TRUE
  set.seed(1)
  ans.obtained <- fetchResults(object = skeleton,
                               nameObject = "y",
                               filename = filename,
                               iterations = 1:10,
                               nIteration = 10L,
                               lengthIter = 100L,
                               impute = TRUE)
  set.seed(1)
  ans.expected <- Counts(array(c(1:5, NA),
                               dim = c(2:3, 10),
                               dimnames = list(sex = c("f", "m"),
                                               age = 0:2,
                                               iteration = 1:10)))
  alpha.male <- 21 + seq(0, 900, 100)
  varsigma <- 22 + seq(0, 900, 100)
  exposure <- seq.int(from = 6, by = 100, length = 10) + seq.int(from = 12, by = 100, length = 10)
  mean <- log(exposure) + alpha.male
  sd <- varsigma
  imputed <- exp(rnorm(n = 10, mean = mean, sd = sd))
  ans.expected[ 2, 3, ] <- imputed
  if (test.identity)
    expect_identical(ans.obtained, ans.expected)
  else
    expect_equal(ans.obtained, ans.expected)
})


test_that("fetchResults works with object of class SkeletonMissingDatasetLN2 - varsigma not updated", {
  fetchResults <- demest:::fetchResults
  SkeletonMissingDataset <- demest:::SkeletonMissingDataset
  Skeleton <- demest:::Skeleton
  initialModel <- demest:::initialModel
  getDataFromFile <- demest:::getDataFromFile
  addIterationsToTransform <- demest:::addIterationsToTransform
  object <- Counts(array(c(1:5, NA),
                         dim = 2:3,
                         dimnames = list(sex = c("f", "m"),
                                         age = 0:2)))
  y <- Counts(array(1:12,
                    dim = c(2:3, 2),
                    dimnames = list(sex = c("f", "m"),
                                    age = 0:2, region = c("a", "b"))))
  transformComponent <- makeTransform(x = y, y = object)
  constraints <- Values(array(NA_real_, dim = 2, dimnames = list(sex = c("f", "m"))))
  model <- Model(y ~ LN2(constraints, sd = 0.2))
  model <- initialModel(model, y = object, exposure = object + 1)
  outputModel <- list(likelihood = list(mean = Skeleton(first = 20L,
                                                        object = constraints,
                                                        margin = 1L),
                                        sd = 0.2))
  skeletonComponent <-  Skeleton(first = 1L, object = y)
  skeleton <- SkeletonMissingDataset(object = object,
                                     model = model,
                                     outputModel = outputModel,
                                     skeletonComponent = skeletonComponent,
                                     transformComponent = transformComponent)
  filename <- tempfile()
  con <- file(filename, "wb")
  results <- new("ResultsModelEst")
  results <- serialize(results, connection = NULL)
  size.results <- length(results)
  writeBin(size.results, con)
  writeBin(10L, con)
  writeBin(results, con)
  writeBin(as.double(1:1000), con)
  close(con)
  ## impute is FALSE
  ans.obtained <- fetchResults(object = skeleton,
                               nameObject = "y",
                               filename = filename,
                               iterations = 1:10,
                               nIteration = 10L,
                               lengthIter = 100L,
                               impute = FALSE)
  ans.expected <- object
  expect_identical(ans.obtained, ans.expected)
  ## impute is TRUE
  set.seed(1)
  ans.obtained <- fetchResults(object = skeleton,
                               nameObject = "y",
                               filename = filename,
                               iterations = 1:10,
                               nIteration = 10L,
                               lengthIter = 100L,
                               impute = TRUE)
  set.seed(1)
  ans.expected <- Counts(array(c(1:5, NA),
                               dim = c(2:3, 10),
                               dimnames = list(sex = c("f", "m"),
                                               age = 0:2,
                                               iteration = 1:10)))
  alpha.male <- 21 + seq(0, 900, 100)
  varsigma <- rep(0.2, 10)
  exposure <- seq.int(from = 6, by = 100, length = 10) + seq.int(from = 12, by = 100, length = 10)
  mean <- log(exposure + 1) + alpha.male
  sd <- varsigma
  imputed <- exp(rnorm(n = 10, mean = mean, sd = sd)) - 1
  ans.expected[ 2, 3, ] <- imputed
  if (test.identity)
    expect_identical(ans.obtained, ans.expected)
  else
    expect_equal(ans.obtained, ans.expected)
})









