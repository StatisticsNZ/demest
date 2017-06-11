
context("Mapping-generators")

## MAPPINGS TO POPULATION ################################################################

## component

test_that("Mapping creates object of class MappingCompToPopn from object of class Component", {
    Mapping <- demest:::Mapping
    ExitsMovements <- dembase:::ExitsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    exits <- Counts(array(1:24,
                          dim = c(4, 3, 2),
                          dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    population <- Population(population)
    ans.obtained <- Mapping(current = exits,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 4L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 12L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    exits <- Counts(array(1:72,
                          dim = c(3, 4, 3, 2),
                          dimnames = list(reg = 1:3,
                              age = c("0-4", "5-9", "10-14", "15+"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    population <- Population(population)
    ans.obtained <- Mapping(current = exits,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 1L,
                        stepSharedTargetVec = 1L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 3L,
                        stepAgeTarget = 3L,
                        stepTriangleCurrent = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    exits <- Counts(array(1:36,
                          dim = c(3, 4, 3),
                          dimnames = list(reg = 1:3,
                              eth = 1:4,
                              time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    population <- Population(population)
    ans.obtained <- Mapping(current = exits,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3:4,
                        stepSharedCurrentVec = c(1L, 3L),
                        stepSharedTargetVec = c(1L, 3L),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})


## births no parent

test_that("Mapping creates object of class MappingCompToPopn from object of class BirthsMovementNoParentChild", {
    Mapping <- demest:::Mapping
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    births <- Counts(array(1:12,
                          dim = c(2, 3, 2),
                          dimnames = list(age = c("5-9", "10-14"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = births,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 2L,
                        stepTimeTarget = 4L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    births <- Counts(array(1:36,
                          dim = c(3, 2, 3, 2),
                          dimnames = list(reg = 1:3,
                              age = c("5-9", "10-14"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = births,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 6L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 1L,
                        stepSharedTargetVec = 1L,
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    births <- Counts(array(1:36,
                          dim = c(3, 4, 3),
                          dimnames = list(reg = 1:3,
                              eth = 1:4,
                              time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = births,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3:4,
                        stepSharedCurrentVec = c(1L, 3L),
                        stepSharedTargetVec = c(1L, 3L),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})

## births has parent

test_that("Mapping creates object of class MappingCompToPopn from object of class BirthsMovementHasParentChild", {
    Mapping <- demest:::Mapping
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    births <- Counts(array(1:108,
                           dim = c(2, 3, 3, 3, 2),
                           dimnames = list(age = c("5-9", "10-14"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               eth_parent = 1:3,
                               eth_child = 1:3,
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = births,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 2L,
                        stepTimeTarget = 4L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 18L,
                        stepSharedTargetVec = 16L,
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    births <- Counts(array(1:1296,
                           dim = c(3, 2, 2, 3, 3, 2, 3, 2),
                           dimnames = list(reg = 1:3,
                               eth_parent = 1:2,
                               eth_child = 1:2,
                               occ_parent = 1:3,
                               occ_child = 1:3,
                               age = c("5-9", "10-14"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:288,
                               dim = c(3, 2, 3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = births,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 216L,
                        stepTimeTarget = 72L,
                        nSharedVec = c(3L, 2L, 3L),
                        stepSharedCurrentVec = c(1L, 6L, 36L),
                        stepSharedTargetVec = c(1L, 3L, 6L),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    births <- Counts(array(1:36,
                           dim = c(3, 4, 4, 3),
                           dimnames = list(reg = 1:3,
                               eth_parent = 1:4,
                               eth_child = 1:4,
                               time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = births,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 48L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3:4,
                        stepSharedCurrentVec = c(1L, 12L),
                        stepSharedTargetVec = c(1L, 3L),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})


## orig-dest

test_that("Mapping creates object of class MappingCompToPopn from object of class InternalMovementsOrigDest", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 eth_orig = 1:3,
                                 eth_dest = 1:3,
                                 triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingOrigDestToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 4L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        nOrigDestVec = 3L,
                        stepOrigCurrentVec = 12L,
                        stepDestCurrentVec = 36L,
                        stepOrigDestTargetVec = 16L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 108L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:2592,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:2,
                                 eth_dest = 1:2,
                                 occ_orig = 1:3,
                                 occ_dest = 1:3,
                                 sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 triangle = c("TL", "TU"))))
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   sex = c("f", "m"),
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingOrigDestToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 864L,
                        stepTimeTarget = 144L,
                        nSharedVec = c(3L, 2L),
                        stepSharedCurrentVec = c(1L, 108L),
                        stepSharedTargetVec = c(1L, 18L),
                        nOrigDestVec = c(2L, 3L),
                        stepOrigCurrentVec = c(3L, 12L),
                        stepDestCurrentVec = c(6L, 36L),
                        stepOrigDestTargetVec = c(3L, 6L),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 216L,
                        stepAgeTarget = 36L,
                        stepTriangleCurrent = 2592L)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    internal <- Counts(array(1:36,
                             dim = c(3, 4, 4, 3),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:4,
                                 eth_dest = 1:4,
                                 time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingOrigDestToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 48L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 1L,
                        stepSharedTargetVec = 1L,
                        nOrigDestVec = 4L,
                        stepOrigCurrentVec = 3L,
                        stepDestCurrentVec = 12L,
                        stepOrigDestTargetVec = 3L,
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})


## pool

test_that("Mapping creates object of class MappingCompToPopn from object of class InternalMovementsPool", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 eth_orig = 1:3,
                                 eth_dest = 1:3,
                                 triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 4L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 12L,
                        stepSharedTargetVec = 16L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:2592,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:2,
                                 eth_dest = 1:2,
                                 occ_orig = 1:3,
                                 occ_dest = 1:3,
                                 sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   sex = c("f", "m"),
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 144L,
                        stepTimeTarget = 144L,
                        nSharedVec = c(3L, 2L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 3L, 6L, 18L),
                        stepSharedTargetVec = c(1L, 3L, 6L, 18L),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 36L,
                        stepAgeTarget = 36L,
                        stepTriangleCurrent = 432L)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    internal <- Counts(array(1:36,
                             dim = c(3, 4, 4, 3),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:4,
                                 eth_dest = 1:4,
                                 time = c("2001-2005", "2006-2010", "2011-2015"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = c(3L, 4L),
                        stepSharedCurrentVec = c(1L, 3L),
                        stepSharedTargetVec = c(1L, 3L),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})


## net

test_that("Mapping creates object of class MappingCompToPopn from object of class InternalMovementsNet", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 eth_orig = 1:3,
                                 eth_dest = 1:3,
                                 triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 4L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 12L,
                        stepSharedTargetVec = 16L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:2592,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:2,
                                 eth_dest = 1:2,
                                 occ_orig = 1:3,
                                 occ_dest = 1:3,
                                 sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   sex = c("f", "m"),
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 144L,
                        stepTimeTarget = 144L,
                        nSharedVec = c(3L, 2L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 3L, 6L, 18L),
                        stepSharedTargetVec = c(1L, 3L, 6L, 18L),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 36L,
                        stepAgeTarget = 36L,
                        stepTriangleCurrent = 432L)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    internal <- Counts(array(1:36,
                             dim = c(3, 4, 4, 3),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:4,
                                 eth_dest = 1:4,
                                 time = c("2001-2005", "2006-2010", "2011-2015"))))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    population <- Population(population)
    ans.obtained <- Mapping(current = internal,
                            target = population)
    ans.expected <- new("MappingCompToPopn",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = c(3L, 4L),
                        stepSharedCurrentVec = c(1L, 3L),
                        stepSharedTargetVec = c(1L, 3L),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})


## MAPPINGS TO ACCESSION ################################################################

## component

test_that("Mapping creates object of class MappingCompToAcc from object of class Component", {
    Mapping <- demest:::Mapping
    ExitsMovements <- dembase:::ExitsMovements
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    exits <- Counts(array(1:24,
                          dim = c(4, 3, 2),
                          dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    accession <- Counts(array(1:12,
                              dim = c(3, 3),
                              dimnames = list(age = c("5", "10", "15"),
                                  time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = exits,
                            target = accession)
    ans.expected <- new("MappingCompToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 3L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 12L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    exits <- Counts(array(1:72,
                          dim = c(3, 4, 3, 2),
                          dimnames = list(reg = 1:3,
                              age = c("0-4", "5-9", "10-14", "15+"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    accession <- Counts(array(1:27,
                              dim = c(3, 3, 3),
                              dimnames = list(reg = 1:3,
                                  age = c("5", "10", "15"),
                                  time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = exits,
                            target = accession)
    ans.expected <- new("MappingCompToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 9L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 1L,
                        stepSharedTargetVec = 1L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 3L,
                        stepAgeTarget = 3L,
                        stepTriangleCurrent = 36L)
    expect_identical(ans.obtained, ans.expected)
})


## births no parent

test_that("Mapping creates object of class MappingCompToAcc from object of class BirthsMovementNoParentChild", {
    Mapping <- demest:::Mapping
    BirthsMovements <- dembase:::BirthsMovements
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    births <- Counts(array(1:12,
                           dim = c(2, 3, 2),
                           dimnames = list(age = c("5-9", "10-14"),
                                           time = c("2001-2005", "2006-2010", "2011-2015"),
                                           triangle = c("TL", "TU"))))
    population <- Counts(array(1:16,
                               dim = c(4, 4),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                               time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    accession <- Counts(array(1:12,
                              dim = c(4, 3),
                              dimnames = list(age = c("5", "10", "15", "20"),
                                              time = c("2001-2005", "2006-2010", "2011-2015"))))
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = births,
                            target = accession)
    ans.expected <- new("MappingCompToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 2L,
                        stepTimeTarget = 4L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    births <- Counts(array(1:36,
                           dim = c(3, 2, 3, 2),
                           dimnames = list(reg = 1:3,
                                           age = c("5-9", "10-14"),
                                           time = c("2001-2005", "2006-2010", "2011-2015"),
                                           triangle = c("TL", "TU"))))
    population <- Counts(array(1:48,
                              dim = c(3, 4, 4),
                              dimnames = list(reg = 1:3,
                                              age = c("0-4", "5-9", "10-14", "15+"),
                                              time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    accession <- Counts(array(1:36,
                              dim = c(3, 4, 3),
                              dimnames = list(reg = 1:3,
                                              age = c("5", "10", "15", "20"),
                                              time = c("2001-2005", "2006-2010", "2011-2015"))))
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = births,
                            target = accession)
    ans.expected <- new("MappingCompToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 6L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 1L,
                        stepSharedTargetVec = 1L,
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})

## births has parent

test_that("Mapping creates object of class MappingCompToAcc from object of class BirthsMovementHasParentChild", {
    Mapping <- demest:::Mapping
    BirthsMovements <- dembase:::BirthsMovements
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    births <- Counts(array(1:108,
                           dim = c(2, 3, 3, 3, 2),
                           dimnames = list(age = c("5-9", "10-14"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               eth_parent = 1:3,
                               eth_child = 1:3,
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    accession <- Counts(array(1:24,
                              dim = c(4, 3, 3),
                              dimnames = list(age = c("5", "10", "15", "20"),
                                   time = c("2001-2005", "2006-2010", "2011-2015"),
                                   eth = 1:3)))
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = births,
                            target = accession)
    ans.expected <- new("MappingCompToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 2L,
                        stepTimeTarget = 4L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 18L,
                        stepSharedTargetVec = 12L,
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    births <- Counts(array(1:1296,
                           dim = c(3, 2, 2, 3, 3, 2, 3, 2),
                           dimnames = list(reg = 1:3,
                               eth_parent = 1:2,
                               eth_child = 1:2,
                               occ_parent = 1:3,
                               occ_child = 1:3,
                               age = c("5-9", "10-14"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:288,
                               dim = c(3, 2, 3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    accession <- Counts(array(1:216,
                               dim = c(3, 2, 3, 4, 3),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   age = c("5", "10", "15", "20"),
                                   time = c("2001-2005", "2006-2010", "2011-2015"))))
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = births,
                            target = accession)
    ans.expected <- new("MappingCompToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 216L,
                        stepTimeTarget = 72L,
                        nSharedVec = c(3L, 2L, 3L),
                        stepSharedCurrentVec = c(1L, 6L, 36L),
                        stepSharedTargetVec = c(1L, 3L, 6L),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})


## orig-dest

test_that("Mapping creates object of class MappingCompToAcc from object of class InternalMovementsOrigDest", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 eth_orig = 1:3,
                                 eth_dest = 1:3,
                                 triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    accession <- Counts(array(1:36,
                               dim = c(3, 3, 3),
                               dimnames = list(age = c("5", "10", "15"),
                                   time = c("2001-2005", "2006-2010", "2011-2015"),
                                   eth = 1:3)))
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = internal,
                            target = accession)
    ans.expected <- new("MappingOrigDestToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 3L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        nOrigDestVec = 3L,
                        stepOrigCurrentVec = 12L,
                        stepDestCurrentVec = 36L,
                        stepOrigDestTargetVec = 9L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 108L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:2592,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:2,
                                 eth_dest = 1:2,
                                 occ_orig = 1:3,
                                 occ_dest = 1:3,
                                 sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 triangle = c("TL", "TU"))))
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   sex = c("f", "m"),
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    accession <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 3, 3),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   sex = c("f", "m"),
                                   age = c("5", "10", "15"),
                                   time = c("2001-2005", "2006-2010", "2011-2015"))))
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = internal,
                            target = accession)
    ans.expected <- new("MappingOrigDestToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 864L,
                        stepTimeTarget = 108L,
                        nSharedVec = c(3L, 2L),
                        stepSharedCurrentVec = c(1L, 108L),
                        stepSharedTargetVec = c(1L, 18L),
                        nOrigDestVec = c(2L, 3L),
                        stepOrigCurrentVec = c(3L, 12L),
                        stepDestCurrentVec = c(6L, 36L),
                        stepOrigDestTargetVec = c(3L, 6L),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 216L,
                        stepAgeTarget = 36L,
                        stepTriangleCurrent = 2592L)
    expect_identical(ans.obtained, ans.expected)
})


## pool

test_that("Mapping creates object of class MappingCompToAcc from object of class InternalMovementsPool", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 eth_orig = 1:3,
                                 eth_dest = 1:3,
                                 triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    accession <- Counts(array(1:27,
                               dim = c(3, 3, 3),
                               dimnames = list(age = c("5", "10", "15"),
                                   time = c("2001-2005", "2006-2010", "2011-2015"),
                                   eth = 1:3)))
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = internal,
                            target = accession)
    ans.expected <- new("MappingCompToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 3L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 12L,
                        stepSharedTargetVec = 9L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:2592,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:2,
                                 eth_dest = 1:2,
                                 occ_orig = 1:3,
                                 occ_dest = 1:3,
                                 sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   sex = c("f", "m"),
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    accession <- Counts(array(1L,
                               dim = c(3, 2, 3, 2, 3, 3),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   sex = c("f", "m"),
                                   age = c("5", "10", "15"),
                                   time = c("2001-2005", "2006-2010", "2011-2015"))))
    accession <- Accession(accession)
    ans.obtained <- Mapping(current = internal,
                            target = accession)
    ans.expected <- new("MappingCompToAcc",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 144L,
                        stepTimeTarget = 108L,
                        nSharedVec = c(3L, 2L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 3L, 6L, 18L),
                        stepSharedTargetVec = c(1L, 3L, 6L, 18L),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 36L,
                        stepAgeTarget = 36L,
                        stepTriangleCurrent = 432L)
    expect_identical(ans.obtained, ans.expected)
})



## MAPPINGS TO EXPOSURE ################################################################

## component

test_that("Mapping creates object of class MappingCompToExp from object of class Component", {
    Mapping <- demest:::Mapping
    ExitsMovements <- dembase:::ExitsMovements
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Exposure <- dembase:::Exposure
    ## has age; no shared dimensions
    exits <- Counts(array(1:24,
                          dim = c(4, 3, 2),
                          dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    exposure <- exposure(population, triangle = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exits,
                            target = exposure)
    ans.expected <- new("MappingCompToExp",
                        isOneToOne = TRUE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 4L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 12L,
                        stepTriangleTarget = 12L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    exits <- Counts(array(1:72,
                          dim = c(3, 4, 3, 2),
                          dimnames = list(reg = 1:3,
                              age = c("0-4", "5-9", "10-14", "15+"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    exposure <- exposure(population, triangle = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exits,
                            target = exposure)
    ans.expected <- new("MappingCompToExp",
                        isOneToOne = TRUE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 1L,
                        stepSharedTargetVec = 1L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 3L,
                        stepAgeTarget = 3L,
                        stepTriangleCurrent = 36L,
                        stepTriangleTarget = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    exits <- Counts(array(1:36,
                          dim = c(3, 4, 3),
                          dimnames = list(reg = 1:3,
                              eth = 1:4,
                              time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    exposure <- exposure(population)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exits,
                            target = exposure)
    ans.expected <- new("MappingCompToExp",
                        isOneToOne = TRUE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3:4,
                        stepSharedCurrentVec = c(1L, 3L),
                        stepSharedTargetVec = c(1L, 3L),
                        hasAge = FALSE,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_,
                        stepTriangleTarget = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})

## births no parent

test_that("Mapping creates object of class MappingBirthsToExp from object of class BirthsMovementNoParentChild", {
    Mapping <- demest:::Mapping
    BirthsMovements <- dembase:::BirthsMovements
    Exposure <- dembase:::Exposure
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    births <- Counts(array(1:12,
                          dim = c(2, 3, 2),
                          dimnames = list(age = c("5-9", "10-14"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = births,
                            target = exposure)
    ans.expected <- new("MappingBirthsToExp",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 2L,
                        stepTimeTarget = 4L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        hasAge = TRUE,
                        iMinAge = 2L,
                        nAgeCurrent = 2L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 6L,
                        stepTriangleTarget = 12L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    births <- Counts(array(1:36,
                          dim = c(3, 2, 3, 2),
                          dimnames = list(reg = 1:3,
                              age = c("5-9", "10-14"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = births,
                            target = exposure)
    ans.expected <- new("MappingBirthsToExp",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 6L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 1L,
                        stepSharedTargetVec = 1L,
                        hasAge = TRUE,
                        nAgeCurrent = 2L,
                        iMinAge = 2L,
                        stepAgeCurrent = 3L,
                        stepAgeTarget = 3L,
                        stepTriangleCurrent = 18L,
                        stepTriangleTarget = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    births <- Counts(array(1:36,
                          dim = c(3, 4, 3),
                          dimnames = list(reg = 1:3,
                              eth = 1:4,
                              time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = births,
                            target = exposure)
    ans.expected <- new("MappingBirthsToExp",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3:4,
                        stepSharedCurrentVec = c(1L, 3L),
                        stepSharedTargetVec = c(1L, 3L),
                        hasAge = FALSE,
                        iMinAge = NA_integer_,
                        nAgeCurrent = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_,
                        stepTriangleTarget = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})

## births has parent

test_that("Mapping creates object of class MappingBirthsToExp from object of class BirthsMovementHasParentChild", {
    Mapping <- demest:::Mapping
    BirthsMovements <- dembase:::BirthsMovements
    Exposure <- dembase:::Exposure
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    births <- Counts(array(1:108,
                           dim = c(2, 3, 3, 3, 2),
                           dimnames = list(age = c("5-9", "10-14"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               eth_parent = 1:3,
                               eth_child = 1:3,
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = births,
                            target = exposure)
    ans.expected <- new("MappingBirthsToExp",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 2L,
                        stepTimeTarget = 4L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 18L,
                        stepSharedTargetVec = 12L,
                        hasAge = TRUE,
                        nAgeCurrent = 2L,
                        iMinAge = 2L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 54L,
                        stepTriangleTarget = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    births <- Counts(array(1:1296,
                           dim = c(3, 2, 2, 3, 3, 2, 3, 2),
                           dimnames = list(reg = 1:3,
                               eth_parent = 1:2,
                               eth_child = 1:2,
                               occ_parent = 1:3,
                               occ_child = 1:3,
                               age = c("5-9", "10-14"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:288,
                               dim = c(3, 2, 3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = births,
                            target = exposure)
    ans.expected <- new("MappingBirthsToExp",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 216L,
                        stepTimeTarget = 72L,
                        nSharedVec = c(3L, 2L, 3L),
                        stepSharedCurrentVec = c(1L, 6L, 36L),
                        stepSharedTargetVec = c(1L, 3L, 6L),
                        hasAge = TRUE,
                        nAgeCurrent = 2L,
                        iMinAge = 2L,
                        stepAgeCurrent = 108L,
                        stepAgeTarget = 18L,
                        stepTriangleCurrent = 648L,
                        stepTriangleTarget = 216L)
    expect_identical(ans.obtained, ans.expected)
})

## orig-dest

test_that("Mapping creates object of class MappingOrigDestToExp from object of class InternalMovementsOrigDest", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Exposure <- dembase:::Exposure
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                             time = c("2001-2005", "2006-2010", "2011-2015"),
                                             eth_orig = 1:3,
                                             eth_dest = 1:3,
                                             triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                               time = c(2000, 2005, 2010, 2015),
                                               eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = internal,
                            target = exposure)
    ans.expected <- new("MappingOrigDestToExp",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 4L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        nOrigDestVec = 3L,
                        stepOrigCurrentVec = 12L,
                        stepDestCurrentVec = 36L,
                        stepOrigDestTargetVec = 12L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 108L,
                        stepTriangleTarget = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:2592,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                             eth_orig = 1:2,
                                             eth_dest = 1:2,
                                             occ_orig = 1:3,
                                             occ_dest = 1:3,
                                             sex = c("f", "m"),
                                             age = c("0-4", "5-9", "10-14", "15+"),
                                             time = c("2001-2005", "2006-2010", "2011-2015"),
                                             triangle = c("TL", "TU"))))
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                               eth = 1:2,
                                               occ = 1:3,
                                               sex = c("f", "m"),
                                               age = c("0-4", "5-9", "10-14", "15+"),
                                               time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = internal,
                            target = exposure)
    ans.expected <- new("MappingOrigDestToExp",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 864L,
                        stepTimeTarget = 144L,
                        nSharedVec = c(3L, 2L),
                        stepSharedCurrentVec = c(1L, 108L),
                        stepSharedTargetVec = c(1L, 18L),
                        nOrigDestVec = c(2L, 3L),
                        stepOrigCurrentVec = c(3L, 12L),
                        stepDestCurrentVec = c(6L, 36L),
                        stepOrigDestTargetVec = c(3L, 6L),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 216L,
                        stepAgeTarget = 36L,
                        stepTriangleCurrent = 2592L,
                        stepTriangleTarget = 432L)
    expect_identical(ans.obtained, ans.expected)
})


## pool

test_that("Mapping creates object of class MappingCompToExp from object of class InternalMovementsPool", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Exposure <- dembase:::Exposure
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 eth_orig = 1:3,
                                 eth_dest = 1:3,
                                 triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = internal,
                            target = exposure)
    ans.expected <- new("MappingCompToExp",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 4L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 12L,
                        stepSharedTargetVec = 12L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 36L,
                        stepTriangleTarget = 36L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:2592,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                 eth_orig = 1:2,
                                 eth_dest = 1:2,
                                 occ_orig = 1:3,
                                 occ_dest = 1:3,
                                 sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c("2001-2005", "2006-2010", "2011-2015"),
                                 triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   sex = c("f", "m"),
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = internal,
                            target = exposure)
    ans.expected <- new("MappingCompToExp",
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 144L,
                        stepTimeTarget = 144L,
                        nSharedVec = c(3L, 2L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 3L, 6L, 18L),
                        stepSharedTargetVec = c(1L, 3L, 6L, 18L),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        stepAgeCurrent = 36L,
                        stepAgeTarget = 36L,
                        stepTriangleCurrent = 432L,
                        stepTriangleTarget = 432L)
    expect_identical(ans.obtained, ans.expected)
})



## MAPPINGS FROM EXPOSURE ##############################################################

## component

test_that("Mapping creates object of class MappingExpToComp from objects of class Exposure, Component", {
    Mapping <- demest:::Mapping
    ExitsMovements <- dembase:::ExitsMovements
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Exposure <- dembase:::Exposure
    ## has age; no shared dimensions
    exits <- Counts(array(1:24,
                          dim = c(4, 3, 2),
                          dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    exposure <- exposure(population, triangle = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = exits)
    ans.expected <- new("MappingExpToComp",
                        isOneToOne = TRUE,
                        nSharedVec = 4:2,
                        stepSharedCurrentVec = c(1L, 4L, 12L),
                        stepSharedTargetVec = c(1L, 4L, 12L))
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    exits <- Counts(array(1:72,
                          dim = c(3, 4, 3, 2),
                          dimnames = list(reg = 1:3,
                              age = c("0-4", "5-9", "10-14", "15+"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    exposure <- exposure(population, triangle = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = exits)
    ans.expected <- new("MappingExpToComp",
                        isOneToOne = TRUE,
                        nSharedVec = c(3L, 4L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 3L, 12L, 36L),
                        stepSharedTargetVec = c(1L, 3L, 12L, 36L))
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    exits <- Counts(array(1:36,
                          dim = c(3, 4, 3),
                          dimnames = list(reg = 1:3,
                              eth = 1:4,
                              time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    exits <- ExitsMovements(exits, template = template, name = "exits")
    exposure <- exposure(population)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = exits)
    ans.expected <- new("MappingExpToComp",
                        isOneToOne = TRUE,
                        nSharedVec = c(3L, 4L, 3L),
                        stepSharedCurrentVec = c(1L, 3L, 12L),
                        stepSharedTargetVec = c(1L, 3L, 12L))
    expect_identical(ans.obtained, ans.expected)
})

## births no parent

test_that("Mapping creates object of class MappingExpToBirths from objects of class Exposure, BirthsMovementNoParentChild", {
    Mapping <- demest:::Mapping
    BirthsMovements <- dembase:::BirthsMovements
    Exposure <- dembase:::Exposure
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    births <- Counts(array(1:12,
                          dim = c(2, 3, 2),
                          dimnames = list(age = c("5-9", "10-14"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = births)
    ans.expected <- new("MappingExpToBirths",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 2L,
                        nSharedVec = integer(),
                        stepSharedCurrentVec = integer(),
                        stepSharedTargetVec = integer(),
                        hasAge = TRUE,
                        iMinAge = 2L,
                        nAgeCurrent = 4L,
                        nAgeTarget = 2L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 12L,
                        stepTriangleTarget = 6L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    births <- Counts(array(1:36,
                          dim = c(3, 2, 3, 2),
                          dimnames = list(reg = 1:3,
                              age = c("5-9", "10-14"),
                              time = c("2001-2005", "2006-2010", "2011-2015"),
                              triangle = c("TL", "TU"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = births)
    ans.expected <- new("MappingExpToBirths",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 6L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 1L,
                        stepSharedTargetVec = 1L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        nAgeTarget = 2L,
                        iMinAge = 2L,
                        stepAgeCurrent = 3L,
                        stepAgeTarget = 3L,
                        stepTriangleCurrent = 36L,
                        stepTriangleTarget = 18L)
    expect_identical(ans.obtained, ans.expected)
    ## no age; shared dimensions
    births <- Counts(array(1:36,
                          dim = c(3, 4, 3),
                          dimnames = list(reg = 1:3,
                              eth = 1:4,
                              time = c("2001-2005", "2006-2010", "2011-2015"))))
    population <- Counts(array(1:48,
                               dim = c(3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:4,
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = births)
    ans.expected <- new("MappingExpToBirths",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 12L,
                        stepTimeTarget = 12L,
                        nSharedVec = 3:4,
                        stepSharedCurrentVec = c(1L, 3L),
                        stepSharedTargetVec = c(1L, 3L),
                        hasAge = FALSE,
                        iMinAge = NA_integer_,
                        nAgeCurrent = NA_integer_,
                        nAgeTarget = NA_integer_,
                        stepAgeCurrent = NA_integer_,
                        stepAgeTarget = NA_integer_,
                        stepTriangleCurrent = NA_integer_,
                        stepTriangleTarget = NA_integer_)
    expect_identical(ans.obtained, ans.expected)
})


## births has parent

test_that("Mapping creates object of class MappingBirthsToExp from object of classes Exposure, BirthsMovementHasParentChild", {
    Mapping <- demest:::Mapping
    BirthsMovements <- dembase:::BirthsMovements
    Exposure <- dembase:::Exposure
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    births <- Counts(array(1:108,
                           dim = c(2, 3, 3, 3, 2),
                           dimnames = list(age = c("5-9", "10-14"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               eth_parent = 1:3,
                               eth_child = 1:3,
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015),
                                   eth = 1:3)))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = births)
    ans.expected <- new("MappingExpToBirths",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 4L,
                        stepTimeTarget = 2L,
                        nSharedVec = 3L,
                        stepSharedCurrentVec = 12L,
                        stepSharedTargetVec = 18L,
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        nAgeTarget = 2L,
                        iMinAge = 2L,
                        stepAgeCurrent = 1L,
                        stepAgeTarget = 1L,
                        stepTriangleCurrent = 36L,
                        stepTriangleTarget = 54L)
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    births <- Counts(array(1:1296,
                           dim = c(3, 2, 2, 3, 3, 2, 3, 2),
                           dimnames = list(reg = 1:3,
                               eth_parent = 1:2,
                               eth_child = 1:2,
                               occ_parent = 1:3,
                               occ_child = 1:3,
                               age = c("5-9", "10-14"),
                               time = c("2001-2005", "2006-2010", "2011-2015"),
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:288,
                               dim = c(3, 2, 3, 4, 4),
                               dimnames = list(reg = 1:3,
                                   eth = 1:2,
                                   occ = 1:3,
                                   age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = births)
    ans.expected <- new("MappingExpToBirths",
                        isOneToOne = FALSE,
                        nTimeCurrent = 3L,
                        stepTimeCurrent = 72L,
                        stepTimeTarget = 216L,
                        nSharedVec = c(3L, 2L, 3L),
                        stepSharedCurrentVec = c(1L, 3L, 6L),
                        stepSharedTargetVec = c(1L, 6L, 36L),
                        hasAge = TRUE,
                        nAgeCurrent = 4L,
                        nAgeTarget = 2L,
                        iMinAge = 2L,
                        stepAgeCurrent = 18L,
                        stepAgeTarget = 108L,
                        stepTriangleCurrent = 216L, 
                        stepTriangleTarget = 648L)
    expect_identical(ans.obtained, ans.expected)
})


## orig-dest

test_that("Mapping creates object of class MappingOrigDestToExp from object of class InternalMovementsOrigDest", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Exposure <- dembase:::Exposure
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                             time = c("2001-2005", "2006-2010", "2011-2015"),
                                             eth_orig = 1:3,
                                             eth_dest = 1:3,
                                             triangle = c("TL", "TU"))))
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                               time = c(2000, 2005, 2010, 2015),
                                               eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = internal)
    ans.expected <- new("MappingExpToComp",
                        isOneToOne = FALSE,
                        nSharedVec = c(4L, 3L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 4L, 12L, 36L),
                        stepSharedTargetVec = c(1L, 4L, 12L, 108L))
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:5184,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                             eth_orig = 1:2,
                                             eth_dest = 1:2,
                                             occ_orig = 1:3,
                                             occ_dest = 1:3,
                                             sex = c("f", "m"),
                                             age = c("0-4", "5-9", "10-14", "15+"),
                                             time = c("2001-2005", "2006-2010", "2011-2015"),
                                             triangle = c("TL", "TU"))))
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                               eth = 1:2,
                                               occ = 1:3,
                                               sex = c("f", "m"),
                                               age = c("0-4", "5-9", "10-14", "15+"),
                                               time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = internal)
    ans.expected <- new("MappingExpToComp",
                        nSharedVec = c(3L, 2L, 3L, 2L, 4L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 3L, 6L, 18L, 36L, 144L, 432L), 
                        stepSharedTargetVec = c(1L, 3L, 12L, 108L, 216L, 864L, 2592L))
    expect_identical(ans.obtained, ans.expected)
})


test_that("Mapping creates object of class MappingExpToComp from object of class InternalMovementsPool", {
    Mapping <- demest:::Mapping
    InternalMovements <- dembase:::InternalMovements
    Exposure <- dembase:::Exposure
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age; no shared dimensions
    internal <- Counts(array(1:216,
                             dim = c(4, 3, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                             time = c("2001-2005", "2006-2010", "2011-2015"),
                                             eth_orig = 1:3,
                                             eth_dest = 1:3,
                                             triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:24,
                               dim = c(4, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                               time = c(2000, 2005, 2010, 2015),
                                               eth = 1:3)))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = internal)
    ans.expected <- new("MappingExpToComp",
                        nSharedVec = c(4L, 3L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 4L, 12L, 36L),
                        stepSharedTargetVec = c(1L, 4L, 12L, 36L))
    expect_identical(ans.obtained, ans.expected)
    ## has age; shared dimensions
    internal <- Counts(array(1:2592,
                             dim = c(3, 2, 2, 3, 3, 2, 4, 3, 2),
                             dimnames = list(reg = 1:3,
                                             eth_orig = 1:2,
                                             eth_dest = 1:2,
                                             occ_orig = 1:3,
                                             occ_dest = 1:3,
                                             sex = c("f", "m"),
                                             age = c("0-4", "5-9", "10-14", "15+"),
                                             time = c("2001-2005", "2006-2010", "2011-2015"),
                                             triangle = c("TL", "TU"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:576,
                               dim = c(3, 2, 3, 2, 4, 4),
                               dimnames = list(reg = 1:3,
                                               eth = 1:2,
                                               occ = 1:3,
                                               sex = c("f", "m"),
                                               age = c("0-4", "5-9", "10-14", "15+"),
                                               time = c(2000, 2005, 2010, 2015))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal, template = template)
    exposure <- exposure(population, triangles = TRUE)
    exposure <- Exposure(exposure)
    ans.obtained <- Mapping(current = exposure,
                            target = internal)
    ans.expected <- new("MappingExpToComp",
                        nSharedVec = c(3L, 2L, 3L, 2L, 4L, 3L, 2L),
                        stepSharedCurrentVec = c(1L, 3L, 6L, 18L, 36L, 144L, 432L),
                        stepSharedTargetVec = c(1L, 3L, 6L, 18L, 36L, 144L, 432L))
    expect_identical(ans.obtained, ans.expected)
})
