

context("AllClasses-SpecAggregate")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

test_that("can create object of class SpecAgCertain", {
    x <- new("SpecAgCertain",
             valueAg = new("ParameterVector", as.double(1:4)),
             weightAg = NULL,
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgCertain",
             valueAg = new("ParameterVector", as.double(1:4)),
             weightAg = Counts(array(1,
                 dim = 4:3,
                 dimnames = list(region = 1:4, time = c(0, 5, 10)))),
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgCertain",
             valueAg = new("ParameterVector", 1),
             weightAg = NULL,
             metadataAg = NULL)
    expect_true(validObject(x))
})

test_that("validityTests inherited from SpecWeightAgMixin work", {
    x <- new("SpecAgCertain",
             valueAg = new("ParameterVector", as.double(1:4)),
             weightAg = Counts(array(1,
                 dim = 4:3,
                 dimnames = list(region = 1:4, time = c(0, 5, 10)))),
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    ## 'weightAg' has no negative values
    x.wrong <- x
    x.wrong@weightAg <- Counts(array(c(-1, rep(1, 11)),
                                     dim = 4:3,
                                     dimnames = list(region = 1:4, time = c(0, 5, 10))))
    expect_error(validObject(x.wrong),
                 "'weightAg' has negative values")
})

test_that("can create object of class SpecAgNormal", {
    x <- new("SpecAgNormal",
             valueAg = new("ParameterVector", as.double(1:4)),
             scaleAg = new("Scale", 0.1),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             weightAg = NULL,
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgNormal",
             valueAg = new("ParameterVector", 1),
             scaleAg = new("Scale", 0.2),
             sdAg = new("ScaleVec", 0.5),
             weightAg = NULL,
             metadataAg = NULL)
    expect_true(validObject(x))
})

test_that("can create object of class SpecAgFun", {
    x <- new("SpecAgFun",
             funAg = function(x, weights) mean(x * weights),
             valueAg = new("ParameterVector", as.double(1:4)),
             scaleAg = new("Scale", 0.1),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             weightAg = NULL,
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgFun",
             funAg = function(x, weights) mean(x * weights),
             valueAg = new("ParameterVector", 1),
             scaleAg = new("Scale", 0.2),
             sdAg = new("ScaleVec", 0.5),
             weightAg = NULL,
             metadataAg = NULL)
    expect_true(validObject(x))
})

test_that("validity tests inherited from FunAgMixin work", {
    x <- new("SpecAgFun",
             funAg = function(x, weights) mean(x * weights),
             valueAg = new("ParameterVector", as.double(1:4)),
             scaleAg = new("Scale", 0.1),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             weightAg = NULL,
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    ## 'funAg' has arguments 'x' and 'weights'
    x.wrong <- x
    f.wrong <- function(wrong, weights) NULL
    x.wrong@funAg <- f.wrong
    expect_error(validObject(x.wrong),
                 "'funAg' does not have formal arguments 'x' and 'weights'")
})

test_that("can create object of class SpecAgPoisson", {
    x <- new("SpecAgPoisson",
             valueAg = new("ParameterVector", as.double(1:4)),
             weightAg = NULL,
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgPoisson",
             scaleAg = new("Scale", 0.1),
             valueAg = new("ParameterVector", 1),
             weightAg = NULL,
             metadataAg = NULL)
    expect_true(validObject(x))
})

test_that("can create object of class SpecAgPlaceholder", {
    x <- new("SpecAgPlaceholder")
    expect_true(validObject(x))
})
