
## context("FakePrior-generators")

## n.test <- 5
## test.identity <- FALSE
## test.extended <- TRUE


## test_that("fakePrior works with SpecExchFixed", {
##     fakePrior <- demest:::fakePrior
##     makeScale <- demest:::makeScale
##     ## valid inputs
##     spec <- ExchFixed(sd = 10)
##     metadata <- new("MetaData",
##                     nms = "reg",
##                     dimtypes = "state",
##                     DimScales = list(new("Categories", dimvalues = letters[1:10])))
##     set.seed(1)
##     ans.obtained <- fakePrior(spec, metadata = metadata, isSaturated = FALSE)
##     set.seed(1)
##     ans.expected <- new("FakeExchFixed",
##                         tau = new("Scale", 10),
##                         mean = new("Parameter", 0),
##                         isSaturated = new("LogicalFlag", FALSE),
##                         J = new("Length", 10L))
##     if (test.identity)
##         expect_identical(ans.obtained, ans.expected)
##     else
##         expect_equal(ans.obtained, ans.expected)
##     ## scale not specified
##     spec <- ExchFixed()
##     metadata <- new("MetaData",
##                     nms = "reg",
##                     dimtypes = "state",
##                     DimScales = list(new("Categories", dimvalues = letters[1:10])))
##     expect_error(fakePrior(spec, metadata = metadata, isSaturated = FALSE),
##                  "need to specify 'sd' in call to function 'ExchFixed'")
## })

## test_that("fakePrior works with SpecExchFixed - intercept", {
##     fakePrior <- demest:::fakePrior
##     makeScale <- demest:::makeScale
##     ## valid inputs
##     spec <- ExchFixed(sd = 10)
##     set.seed(1)
##     ans.obtained <- fakePrior(spec, metadata = NULL, isSaturated = FALSE)
##     set.seed(1)
##     ans.expected <- new("FakeExchFixed",
##                         tau = new("Scale", 10),
##                         mean = new("Parameter", 0),
##                         isSaturated = new("LogicalFlag", FALSE),
##                         J = new("Length", 1L))
##     if (test.identity)
##         expect_identical(ans.obtained, ans.expected)
##     else
##         expect_equal(ans.obtained, ans.expected)
##     ## scale not specified
##     spec <- ExchFixed()
##     expect_error(fakePrior(spec, metadata = NULL, isSaturated = FALSE),
##                  "need to specify 'sd' in call to function 'ExchFixed'")
## })


## test_that("fakePrior works with SpecExchNormZero", {
##     fakePrior <- demest:::fakePrior
##     makeScale <- demest:::makeScale
##     ## valid inputs
##     spec <- Exch(error = Error(scale = HalfT(scale = 0.01)))
##     metadata <- new("MetaData",
##                     nms = "reg",
##                     dimtypes = "state",
##                     DimScales = list(new("Categories", dimvalues = letters[1:10])))
##     set.seed(1)
##     ans.obtained <- fakePrior(spec, metadata = metadata, isSaturated = FALSE)
##     set.seed(1)
##     ans.expected <- new("FakeExchNormZero",
##                         ATau = new("Scale", 0.01),
##                         nuTau = new("DegreesFreedom", 7),
##                         tau = new("Scale", rhalft(n = 1, df = 7, scale = 0.01)),
##                         tauMax = new("Scale", qhalft(0.999, 7, 0.01)),
##                         isSaturated = new("LogicalFlag", FALSE),
##                         J = new("Length", 10L))
##     if (test.identity)
##         expect_identical(ans.obtained, ans.expected)
##     else
##         expect_equal(ans.obtained, ans.expected)
##     ## scale not specified
##     spec <- Exch()
##     metadata <- new("MetaData",
##                     nms = "reg",
##                     dimtypes = "state",
##                     DimScales = list(new("Categories", dimvalues = letters[1:10])))
##     expect_error(fakePrior(spec, metadata = metadata, isSaturated = FALSE),
##                  "need to specify scale of half-t distribution for 'scale' in call to function 'Error'")
## })

## test_that("fakePrior works with SpecDLMNoTrendNormZeroNoSeason", {
##     fakePrior <- demest:::fakePrior
##     spec <- DLM(level = Level(scale = HalfT(scale = 0.05)),
##                 trend = NULL,
##                 error = Error(scale = HalfT(scale = 0.06)))
##     metadata <- new("MetaData",
##                     nms = "time",
##                     dimtypes = "time",
##                     DimScales = list(new("Points", dimvalues = 2001:2010)))
##     ans.obtained <- fakePrior(spec,
##                               metadata = metadata,
##                               isSaturated = FALSE)
##     expect_true(validObject(ans.obtained))
##     expect_is(ans.obtained, "FakeDLMNoTrendNormZeroNoSeason")
##     expect_true(sd(ans.obtained@alphaDLM@.Data) > 0)
## })

## test_that("fakePrior works with SpecDLMWithTrendNormZeroNoSeason", {
##     fakePrior <- demest:::fakePrior
##     spec <- DLM(level = Level(scale = HalfT(scale = 0.05)),
##                 trend = Trend(initial = Initial(sd = 0.1),
##                               scale = HalfT(scale = 0.02)),
##                 error = Error(scale = HalfT(scale = 0.06)))
##     metadata <- new("MetaData",
##                     nms = "time",
##                     dimtypes = "time",
##                     DimScales = list(new("Points", dimvalues = 2001:2010)))
##     ans.obtained <- fakePrior(spec,
##                               metadata = metadata,
##                               isSaturated = FALSE)
##     expect_true(validObject(ans.obtained))
##     expect_is(ans.obtained, "FakeDLMWithTrendNormZeroNoSeason")
##     expect_true(sd(ans.obtained@alphaDLM@.Data) > 0)
##     expect_true(sd(ans.obtained@deltaDLM@.Data) > 0)
## })
