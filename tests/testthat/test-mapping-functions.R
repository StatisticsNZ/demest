
context("mapping-functions")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE




## MAPPINGS TO POPULATION ################################################################

## component

test_that("getIPopnNextFromComp works with ordinary component", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    ExitsMovements <- dembase:::ExitsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## time is first dimension of two
    component <- Counts(array(1:8,
                              dim = c(2, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                  triangle = c("TL", "TU"),
                                  age = c("0-9", "10+"))))
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    template <- makeTemplateComponent(population)
    component <- ExitsMovements(exits = component,
                                template = template,
                                name = "exits")
    population <- Population(population)
    mapping <- Mapping(current = component,
                       target = population)
    ans.obtained <- getIPopnNextFromComp(i = 1L, mapping = mapping)
    ans.expected <- 2L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 5L, mapping = mapping)
    ans.expected <- 5L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 3L, mapping = mapping)
    ans.expected <- 5L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 3L, mapping = mapping)
    ans.expected <- 5L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of two
    component <- Counts(array(1:3,
                              dim = c(3, 1),
                              dimnames = list(reg = c("a", "b", "c"),
                                  time = "2001-2010")))
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- ExitsMovements(exits = component,
                                template = template,
                                name = "exits")
    population <- Population(population)
    mapping <- Mapping(current = component,
                       target = population)
    ans.obtained <- getIPopnNextFromComp(i = 1L, mapping = mapping)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 3L, mapping = mapping)
    ans.expected <- 6L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of three
    component <- Counts(array(1:36,
                              dim = c(3, 2, 2, 2),
                              dimnames = list(reg = c("a", "b", "c"),
                                  time = c("2001-2010", "2011-2020"),
                                  age = c("0-9", "10+"),
                                  triangle = c("TL", "TU"))))
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- ExitsMovements(exits = component,
                                template = template,
                                name = "exits")
    mapping <- Mapping(current = component,
                       target = population)
    ans.obtained <- getIPopnNextFromComp(i = 4L, mapping = mapping)
    ans.expected <- 7L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 8L, mapping = mapping)
    ans.expected <- 14L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 13L, mapping = mapping)
    ans.expected <- 13L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 24L, mapping = mapping)
    ans.expected <- 18L
    ## only has time dimension
    component <- Counts(array(1:11,
                              dim = 11,
                              dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                                  seq(2005, by = 5, len = 11),
                                                  sep = "-"))))
    population <- Counts(array(1:12,
                               dim = 12,
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- ExitsMovements(exits = component,
                                template = template,
                                name = "exits")
    mapping <- Mapping(current = component,
                       target = population)
    ans.obtained <- getIPopnNextFromComp(i = 3L, mapping = mapping)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 11L, mapping = mapping)
    ans.expected <- 12L
})

test_that("R and C versions of getIPopnNextFromComp give same answer with ordinary component", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    ExitsMovements <- dembase:::ExitsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## time is first dimension of two
    component <- Counts(array(1:8,
                              dim = c(2, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                  triangle = c("TL", "TU"),
                                  age = c("0-9", "10+"))))
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    template <- makeTemplateComponent(population)
    component <- ExitsMovements(exits = component,
                                template = template,
                                name = "exits")
    population <- Population(population)
    mapping <- Mapping(current = component,
                       target = population)
    ans.R <- getIPopnNextFromComp(i = 1L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 1L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromComp(i = 3L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 3L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromComp(i = 5L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 5L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of two
    component <- Counts(array(1:3,
                              dim = c(3, 1),
                              dimnames = list(reg = c("a", "b", "c"),
                                  time = "2001-2010")))
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- ExitsMovements(exits = component,
                                template = template,
                                name = "exits")
    population <- Population(population)
    mapping <- Mapping(current = component,
                       target = population)
    ans.R <- getIPopnNextFromComp(i = 1L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 1L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromComp(i = 3L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 3L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of three
    component <- Counts(array(1:36,
                              dim = c(3, 2, 2, 2),
                              dimnames = list(reg = c("a", "b", "c"),
                                  time = c("2001-2010", "2011-2020"),
                                  age = c("0-9", "10+"),
                                  triangle = c("TL", "TU"))))
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- ExitsMovements(exits = component,
                                template = template,
                                name = "exits")
    mapping <- Mapping(current = component,
                       target = population)
    ans.obtained <- getIPopnNextFromComp(i = 4L, mapping = mapping)
    ans.expected <- 7L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 8L, mapping = mapping)
    ans.expected <- 14L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 13L, mapping = mapping)
    ans.expected <- 13L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromComp(i = 24L, mapping = mapping)
    ans.expected <- 18L
    ans.R <- getIPopnNextFromComp(i = 4L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 4L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromComp(i = 8L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 8L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromComp(i = 13L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 13L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromComp(i = 24L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 24L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## only has time dimension
    component <- Counts(array(1:11,
                              dim = 11,
                              dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                                  seq(2005, by = 5, len = 11),
                                                  sep = "-"))))
    population <- Counts(array(1:12,
                               dim = 12,
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- ExitsMovements(exits = component,
                                template = template,
                                name = "exits")
    mapping <- Mapping(current = component,
                       target = population)
    ans.R <- getIPopnNextFromComp(i = 3L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 3L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromComp(i = 11L, mapping = mapping, useC = FALSE)
    ans.C <- getIPopnNextFromComp(i = 11L, mapping = mapping, useC = TRUE)
    expect_identical(ans.R, ans.C)
})


## births no parent

test_that("getIPopnNextFromComp works with BirthsMovementNoParentChild", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## time is first dimension of two
    births <- Counts(array(1:8,
                           dim = c(2, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020"),
                               triangle = c("TL", "TU"),
                               age = c("10-19", "20-29"))))
    population <- Counts(array(1:15,
                               dim = c(3, 5),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    population <- Population(population)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:8) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- 3L - (i %% 2L)
        expect_identical(ans.obtained, ans.expected)
    }
    ## time is second dimension of two
    births <- Counts(array(1:3,
                              dim = c(3, 1),
                              dimnames = list(reg = c("a", "b", "c"),
                                  time = "2001-2010")))
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    population <- Population(population)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:3) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- i + 3L
        expect_identical(ans.obtained, ans.expected)
    }
    ## time is second dimension of three
    births <- Counts(array(1:36,
                              dim = c(3, 2, 2, 2),
                              dimnames = list(reg = c("a", "b", "c"),
                                  time = c("2001-2010", "2011-2020"),
                                  age = c("10-19", "20-29"),
                                  triangle = c("TL", "TU"))))
    population <- Counts(array(1:36,
                               dim = c(3, 3, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10-19", "20-29", "30+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:24) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- rep(4:9, times = 4)[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## only has time dimension
    births <- Counts(array(1:11,
                           dim = 11,
                           dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                               seq(2005, by = 5, len = 11),
                                               sep = "-"))))
    population <- Counts(array(1:12,
                               dim = 12,
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:11) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- i + 1L
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versons of getIPopnNextFromComp give same answer with BirthsMovementsNoParentChild", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## time is first dimension of two
    births <- Counts(array(1:8,
                           dim = c(2, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020"),
                               triangle = c("TL", "TU"),
                               age = c("10-19", "20-29"))))
    population <- Counts(array(1:15,
                               dim = c(3, 5),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    population <- Population(population)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:8) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time is second dimension of two
    births <- Counts(array(1:3,
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                               time = "2001-2010")))
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    population <- Population(population)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:3) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time is second dimension of three
    births <- Counts(array(1:36,
                           dim = c(3, 2, 2, 2),
                           dimnames = list(reg = c("a", "b", "c"),
                               time = c("2001-2010", "2011-2020"),
                               age = c("10-19", "20-29"),
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:36,
                               dim = c(3, 3, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10-19", "20-29", "30+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:24) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## only has time dimension
    births <- Counts(array(1:11,
                           dim = 11,
                           dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                               seq(2005, by = 5, len = 11),
                                               sep = "-"))))
    population <- Counts(array(1:12,
                               dim = 12,
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:11) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})


## births with parent

test_that("getIPopnNextFromComp works with BirthsMovementHasParentChild", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one parent-child, has age
    births <- Counts(array(1:72,
                           dim = c(2, 3, 3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020"),
                               eth_parent = 1:3,
                               eth_child = 1:3,
                               triangle = c("TL", "TU"),
                               age = c("10-19", "20-29"))))
    population <- Counts(array(1:15,
                               dim = c(3, 3, 5),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   eth = 1:3,
                                   age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    population <- Population(population)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:72) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- rep(c(2:3, 2:3, 2:3, 5:6, 5:6, 5:6, 8:9, 8:9, 8:9), times = 4)[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## two parent-child, no age
    births <- Counts(array(1:36,
                           dim = c(3, 3, 2, 2, 1),
                           dimnames = list(reg_parent = c("a", "b", "c"),
                               reg_child = c("a", "b", "c"),
                               eth_parent = 1:2,
                               eth_child = 1:2,
                               time = "2001-2010")))
    population <- Counts(array(1:12,
                               dim = c(3, 2, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   eth = 1:2,
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    population <- Population(population)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:36) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- c(rep(rep(7:9, each = 3), times = 2),
                          rep(rep(10:12, each = 3), times = 2))[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## one parent-child, has age
    births <- Counts(array(1:72,
                           dim = c(3, 3, 2, 2, 2),
                           dimnames = list(reg_parent = c("a", "b", "c"),
                               reg_child = c("a", "b", "c"),
                               time = c("2001-2010", "2011-2020"),
                               age = c("10-19", "20-29"),
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:36,
                               dim = c(3, 3, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10-19", "20-29", "30+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:72) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- rep(c(rep(4:6, each = 3),
                              rep(7:9, each = 3)),
                            times = 4)[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## time dimension plus parent-child
    births <- Counts(array(1:44,
                           dim = c(11, 2, 2),
                           dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                               seq(2005, by = 5, len = 11),
                                               sep = "-"),
                               eth_parent = 1:2,
                               eth_child = 1:2)))
    population <- Counts(array(1:24,
                               dim = c(12, 2),
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12),
                                               eth = 1:2)))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:44) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- c(2:12, 2:12, 14:24, 14:24)[i]
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of getIPopnNextFromComp give same answer with BirthsMovementHasParentChild", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one parent-child, has age
    births <- Counts(array(1:72,
                           dim = c(2, 3, 3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020"),
                               eth_parent = 1:3,
                               eth_child = 1:3,
                               triangle = c("TL", "TU"),
                               age = c("10-19", "20-29"))))
    population <- Counts(array(1:15,
                               dim = c(3, 3, 5),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   eth = 1:3,
                                   age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    population <- Population(population)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:72) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## two parent-child, no age
    births <- Counts(array(1:36,
                           dim = c(3, 3, 2, 2, 1),
                           dimnames = list(reg_parent = c("a", "b", "c"),
                               reg_child = c("a", "b", "c"),
                               eth_parent = 1:2,
                               eth_child = 1:2,
                               time = "2001-2010")))
    population <- Counts(array(1:12,
                               dim = c(3, 2, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   eth = 1:2,
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    population <- Population(population)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:36) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## one parent-child, has age
    births <- Counts(array(1:72,
                           dim = c(3, 3, 2, 2, 2),
                           dimnames = list(reg_parent = c("a", "b", "c"),
                               reg_child = c("a", "b", "c"),
                              time = c("2001-2010", "2011-2020"),
                               age = c("10-19", "20-29"),
                               triangle = c("TL", "TU"))))
    population <- Counts(array(1:36,
                               dim = c(3, 3, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10-19", "20-29", "30+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:72) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time dimension plus parent-child
    births <- Counts(array(1:44,
                           dim = c(11, 2, 2),
                           dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                               seq(2005, by = 5, len = 11),
                                               sep = "-"),
                               eth_parent = 1:2,
                               eth_child = 1:2)))
    population <- Counts(array(1:24,
                               dim = c(12, 2),
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12),
                                               eth = 1:2)))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    mapping <- Mapping(current = births,
                       target = population)
    for (i in 1:44) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})


## origin-destination

test_that("getIPopnNextFromOrigDest works with InternalMovementsOrigDest", {
    getIPopnNextFromOrigDest <- demest:::getIPopnNextFromOrigDest
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one orig-dest; has age
    component <- Counts(array(1:72,
                              dim = c(2, 3, 3, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                  reg_orig = 1:3,
                                  reg_dest = 1:3,
                                  triangle = c("TL", "TU"),
                                  age = c("0-9", "10+"))))
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   reg = 1:3,
                                   age = c("0-9", "10+"))))
    template <- makeTemplateComponent(population)
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = component,
                       target = population)
    i <- seq_len(length(component))
    i <- i[slice.index(component, 2) != slice.index(component, 3)]
    d <- as.data.frame(component, direction = "long")
    d <- subset(d, reg_orig != reg_dest)
    d <- lapply(d[1:5], as.integer)
    i.orig <- with(d, time + 1L + 3L * (reg_orig-1L) + 9L * (age-1L) + 9L * (triangle == 2L & age == 1L))
    i.dest <- with(d, time + 1L + 3L * (reg_dest-1L) + 9L * (age-1L) + 9L * (triangle == 2L & age == 1L))
    for (j in seq_along(i)) {
        ans.obtained <- getIPopnNextFromOrigDest(i = i[j], mapping = mapping, useC = FALSE)
        ans.expected <- c(i.orig[j], i.dest[j])
        expect_identical(ans.obtained, ans.expected)
    }
    ## one orig-dest; no age
    component <- Counts(array(1:9,
                              dim = c(3, 3, 1),
                              dimnames = list(reg_orig = c("a", "b", "c"),
                                  reg_dest = c("a", "b", "c"),
                                  time = "2001-2010")))
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = component,
                       target = population)
    for (j in 1:6) {
        i <- c(2:3, 4L, 6L, 7:8)[j]
        ans.obtained <- getIPopnNextFromOrigDest(i = i, mapping = mapping)
        ans.expected <- c(c(5:6, 4L, 6L, 4:5)[j],
                          rep(4:6, each = 2)[j])
        expect_identical(ans.obtained, ans.expected)
    }
    ## time dimension and two orig-dest dimensions
    component <- Counts(array(1:176,
                              dim = c(11, 2, 2, 2, 2),
                              dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                                  seq(2005, by = 5, len = 11),
                                                  sep = "-"),
                                  reg_orig = 1:2,
                                  reg_dest = 1:2,
                                  eth_orig = 1:2,
                                  eth_dest = 1:2)))
    population <- Counts(array(1:48,
                               dim = c(12, 2, 2),
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12),
                                   reg = 1:2,
                                   eth = 1:2)))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- InternalMovements(internal = component,
                                   template = template)
    mapping <- Mapping(current = component,
                       target = population)
    i <- c(56:77, 100:121)
    i.orig <- c(38:48, 26:36, 14:24, 2:12)
    i.dest <- c(2:12, 14:24, 26:36, 38:48)
    for (j in seq_along(i)) {
        ans.obtained <- getIPopnNextFromOrigDest(i = i[j], mapping = mapping)
        ans.expected <- c(i.orig[j], i.dest[j])
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of getIPopnNextFromOrigDest give same answer InternalMovementsOrigDest", {
    getIPopnNextFromOrigDest <- demest:::getIPopnNextFromOrigDest
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one orig-dest; has age
    component <- Counts(array(1:72,
                              dim = c(2, 3, 3, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                  reg_orig = 1:3,
                                  reg_dest = 1:3,
                                  triangle = c("TL", "TU"),
                                  age = c("0-9", "10+"))))
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   reg = 1:3,
                                   age = c("0-9", "10+"))))
    template <- makeTemplateComponent(population)
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = component,
                       target = population)
    i <- seq_len(length(component))
    i <- i[slice.index(component, 2) != slice.index(component, 3)]
    d <- as.data.frame(component, direction = "long")
    d <- subset(d, reg_orig != reg_dest)
    d <- lapply(d[1:5], as.integer)
    i.orig <- with(d, time + 1L + 3L * (reg_orig-1L) + 9L * (age-1L) + 9L * (triangle == 2L & age == 1L))
    i.dest <- with(d, time + 1L + 3L * (reg_dest-1L) + 9L * (age-1L) + 9L * (triangle == 2L & age == 1L))
    for (j in seq_along(i)) {
        ans.R <- getIPopnNextFromOrigDest(i = i[j], mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromOrigDest(i = i[j], mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## one orig-dest; no age
    component <- Counts(array(1:9,
                              dim = c(3, 3, 1),
                              dimnames = list(reg_orig = c("a", "b", "c"),
                                  reg_dest = c("a", "b", "c"),
                                  time = "2001-2010")))
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = component,
                       target = population)
    for (j in 1:6) {
        i <- c(2:3, 4L, 6L, 7:8)[j]
        ans.R <- getIPopnNextFromOrigDest(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromOrigDest(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time dimension and two orig-dest dimensions
    component <- Counts(array(1:176,
                              dim = c(11, 2, 2, 2, 2),
                              dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                                  seq(2005, by = 5, len = 11),
                                                  sep = "-"),
                                  reg_orig = 1:2,
                                  reg_dest = 1:2,
                                  eth_orig = 1:2,
                                  eth_dest = 1:2)))
    population <- Counts(array(1:48,
                               dim = c(12, 2, 2),
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12),
                                   reg = 1:2,
                                   eth = 1:2)))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    component <- InternalMovements(internal = component,
                                   template = template)
    mapping <- Mapping(current = component,
                       target = population)
    i <- c(56:77, 100:121)
    i.orig <- c(38:48, 26:36, 14:24, 2:12)
    i.dest <- c(2:12, 14:24, 26:36, 38:48)
    for (j in seq_along(i)) {
        ans.R <- getIPopnNextFromOrigDest(i = i[j], mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromOrigDest(i = i[j], mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})


## pool

test_that("getIPopnNextFromComp works with InternalMovementsPool", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one orig-dest; has age
    internal <- Counts(array(1:72,
                              dim = c(2, 3, 3, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                  reg_orig = 1:3,
                                  reg_dest = 1:3,
                                  triangle = c("TL", "TU"),
                                  age = c("0-9", "10+"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   reg = 1:3,
                                   age = c("0-9", "10+"))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = internal,
                       target = population)
    ans.expected.all <- rep(c(2:3, 5:6, 8:9, 11:12, 14:15, 17:18,
                              11:12, 14:15, 17:18, 11:12, 14:15, 17:18),
                            times = 2)
    for (i in seq_along(internal)) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.expected.all[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## one orig-dest; no age
    internal <- Counts(array(1:9,
                              dim = c(3, 3, 1),
                              dimnames = list(reg_orig = c("a", "b", "c"),
                                  reg_dest = c("a", "b", "c"),
                                  time = "2001-2010")))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = internal,
                       target = population)
    ans.expected.all <- rep(4:6, times = 2)
    for (i in seq_along(internal)) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.expected.all[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## time dimension and two orig-dest dimensions
    internal <- Counts(array(1:176,
                              dim = c(11, 2, 2, 2, 2),
                              dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                                  seq(2005, by = 5, len = 11),
                                                  sep = "-"),
                                  reg_orig = 1:2,
                                  reg_dest = 1:2,
                                  eth_orig = 1:2,
                                  eth_dest = 1:2)))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:48,
                               dim = c(12, 2, 2),
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12),
                                   reg = 1:2,
                                   eth = 1:2)))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    mapping <- Mapping(current = internal,
                       target = population)
    ans.expected.all <- rep(c(2:12, 14:24, 26:36, 38:48), times = 2)
    for (i in seq_along(internal)) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.expected.all[i]
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of getIPopnNextFromComp give same answer with InternalMovementsPool", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one orig-dest; has age
    internal <- Counts(array(1:72,
                              dim = c(2, 3, 3, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                  reg_orig = 1:3,
                                  reg_dest = 1:3,
                                  triangle = c("TL", "TU"),
                                  age = c("0-9", "10+"))))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   reg = 1:3,
                                   age = c("0-9", "10+"))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = internal,
                       target = population)
    for (i in seq_along(internal)) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## one orig-dest; no age
    internal <- Counts(array(1:9,
                              dim = c(3, 3, 1),
                              dimnames = list(reg_orig = c("a", "b", "c"),
                                  reg_dest = c("a", "b", "c"),
                                  time = "2001-2010")))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = internal,
                       target = population)
    for (i in seq_along(internal)) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time dimension and two orig-dest dimensions
    internal <- Counts(array(1:176,
                              dim = c(11, 2, 2, 2, 2),
                              dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                                  seq(2005, by = 5, len = 11),
                                                  sep = "-"),
                                  reg_orig = 1:2,
                                  reg_dest = 1:2,
                                  eth_orig = 1:2,
                                  eth_dest = 1:2)))
    internal <- collapseOrigDest(internal, to = "pool")
    population <- Counts(array(1:48,
                               dim = c(12, 2, 2),
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12),
                                   reg = 1:2,
                                   eth = 1:2)))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    mapping <- Mapping(current = internal,
                       target = population)
    for (i in seq_along(internal)) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})


## net

test_that("getIPopnNextFromComp works with InternalMovementsNet", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one orig-dest; has age
    internal <- Counts(array(1:72,
                              dim = c(2, 3, 3, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                  reg_orig = 1:3,
                                  reg_dest = 1:3,
                                  triangle = c("TL", "TU"),
                                  age = c("0-9", "10+"))))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   reg = 1:3,
                                   age = c("0-9", "10+"))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = internal,
                       target = population)
    ans.expected.all <- c(2:3, 5:6, 8:9, 11:12, 14:15, 17:18,
                          11:12, 14:15, 17:18, 11:12, 14:15, 17:18)
    for (i in seq_along(internal)) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.expected.all[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## one orig-dest; no age
    internal <- Counts(array(1:9,
                              dim = c(3, 3, 1),
                              dimnames = list(reg_orig = c("a", "b", "c"),
                                  reg_dest = c("a", "b", "c"),
                                  time = "2001-2010")))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = internal,
                       target = population)
    ans.expected.all <- 4:6
    for (i in seq_along(internal)) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.expected.all[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## time dimension and two orig-dest dimensions
    internal <- Counts(array(1:176,
                              dim = c(11, 2, 2, 2, 2),
                              dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                                  seq(2005, by = 5, len = 11),
                                                  sep = "-"),
                                  reg_orig = 1:2,
                                  reg_dest = 1:2,
                                  eth_orig = 1:2,
                                  eth_dest = 1:2)))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:48,
                               dim = c(12, 2, 2),
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12),
                                   reg = 1:2,
                                   eth = 1:2)))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    mapping <- Mapping(current = internal,
                       target = population)
    ans.expected.all <- c(2:12, 14:24, 26:36, 38:48)
    for (i in seq_along(internal)) {
        ans.obtained <- getIPopnNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.expected.all[i]
        expect_identical(ans.obtained, ans.expected)
    }
})


test_that("R and C versions of getIPopnNextFromComp give same answer with InternalMovementsNet", {
    getIPopnNextFromComp <- demest:::getIPopnNextFromComp
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one orig-dest; has age
    internal <- Counts(array(1:72,
                              dim = c(2, 3, 3, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                  reg_orig = 1:3,
                                  reg_dest = 1:3,
                                  triangle = c("TL", "TU"),
                                  age = c("0-9", "10+"))))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   reg = 1:3,
                                   age = c("0-9", "10+"))))
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = internal,
                       target = population)
    for (i in seq_along(internal)) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## one orig-dest; no age
    internal <- Counts(array(1:9,
                              dim = c(3, 3, 1),
                              dimnames = list(reg_orig = c("a", "b", "c"),
                                  reg_dest = c("a", "b", "c"),
                                  time = "2001-2010")))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    population <- Population(population)
    mapping <- Mapping(current = internal,
                       target = population)
    for (i in seq_along(internal)) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time dimension and two orig-dest dimensions
    internal <- Counts(array(1:176,
                              dim = c(11, 2, 2, 2, 2),
                              dimnames = list(time = paste(seq(2001, by = 5, len = 11),
                                                  seq(2005, by = 5, len = 11),
                                                  sep = "-"),
                                  reg_orig = 1:2,
                                  reg_dest = 1:2,
                                  eth_orig = 1:2,
                                  eth_dest = 1:2)))
    internal <- collapseOrigDest(internal, to = "net")
    population <- Counts(array(1:48,
                               dim = c(12, 2, 2),
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12),
                                   reg = 1:2,
                                   eth = 1:2)))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    internal <- InternalMovements(internal = internal,
                                   template = template)
    mapping <- Mapping(current = internal,
                       target = population)
    for (i in seq_along(internal)) {
        ans.R <- getIPopnNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIPopnNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})











## MAPPINGS TO ACCESSION ################################################################

test_that("getIAccNextFromComp works with ordinary component", {
    getIAccNextFromComp <- demest:::getIAccNextFromComp
    ExitsMovements <- dembase:::ExitsMovements
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## time is first dimension of two
    component <- Counts(array(1:24,
                              dim = c(2, 2, 2, 3),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                              sex = c("Female", "Male"),
                                              triangle = c("TL", "TU"),
                                              age = c("0-9", "10-19", "20+"))))
    accession <- Counts(array(1:8,
                              dim = c(2, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                              sex = c("Female", "Male"),
                                              age = c("10", "20"))))
    accession <- Accession(accession)
    component <- ExitsMovements(exits = component,
                                template = component,
                                name = "exits")
    mapping <- Mapping(current = component,
                       target = accession)
    ans.obtained <- getIAccNextFromComp(i = 1L, mapping = mapping)
    ans.expected <- 2L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromComp(i = 5L, mapping = mapping)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromComp(i = 3L, mapping = mapping)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromComp(i = 10L, mapping = mapping)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromComp(i = 21L, mapping = mapping)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of three
    component <- Counts(array(1:36,
                              dim = c(3, 2, 2, 2),
                              dimnames = list(reg = c("a", "b", "c"),
                                  time = c("2001-2010", "2011-2020"),
                                  age = c("0-9", "10+"),
                                  triangle = c("TL", "TU"))))
    accession <- Counts(array(1:18,
                              dim = c(3, 2, 1),
                              dimnames = list(reg = c("a", "b", "c"),
                                              time = c("2001-2010", "2011-2020"),
                                              age = "10")),
                        dimscales = c(age = "Points"))
    accession <- Accession(accession)
    component <- ExitsMovements(exits = component,
                                template = component,
                                name = "exits")
    mapping <- Mapping(current = component,
                       target = accession)
    ans.obtained <- getIAccNextFromComp(i = 1L, mapping = mapping)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromComp(i = 4L, mapping = mapping)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromComp(i = 8L, mapping = mapping)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromComp(i = 13L, mapping = mapping)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getIAccNextFromComp give same answer with ordinary component", {
    getIAccNextFromComp <- demest:::getIAccNextFromComp
    ExitsMovements <- dembase:::ExitsMovements
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## time is first dimension of two
    component <- Counts(array(1:24,
                              dim = c(2, 2, 2, 3),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                              sex = c("Female", "Male"),
                                              triangle = c("TL", "TU"),
                                              age = c("0-9", "10-19", "20+"))))
    accession <- Counts(array(1:8,
                              dim = c(2, 2, 2),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                              sex = c("Female", "Male"),
                                              age = c("10", "20"))))
    accession <- Accession(accession)
    component <- ExitsMovements(exits = component,
                                template = component,
                                name = "exits")
    mapping <- Mapping(current = component,
                       target = accession)
    for (i in 1:24) {
        ans.R <- getIAccNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIAccNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time is second dimension of three
    component <- Counts(array(1:36,
                              dim = c(3, 2, 2, 2),
                              dimnames = list(reg = c("a", "b", "c"),
                                  time = c("2001-2010", "2011-2020"),
                                  age = c("0-9", "10+"),
                                  triangle = c("TL", "TU"))))
    accession <- Counts(array(1:18,
                              dim = c(3, 2, 1),
                              dimnames = list(reg = c("a", "b", "c"),
                                              time = c("2001-2010", "2011-2020"),
                                              age = "10")),
                        dimscales = c(age = "Points"))
    accession <- Accession(accession)
    component <- ExitsMovements(exits = component,
                                template = component,
                                name = "exits")
    mapping <- Mapping(current = component,
                       target = accession)
    for (i in 1:36) {
        ans.R <- getIAccNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIAccNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})


## births no parent

test_that("getIAccNextFromComp works with BirthsMovementNoParentChild", {
    getIAccNextFromComp <- demest:::getIAccNextFromComp
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## time is first dimension of two
    births <- Counts(array(1:8,
                           dim = c(2, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020"),
                                           triangle = c("TL", "TU"),
                                           age = c("10-19", "20-29"))))
    population <- Counts(array(1:15,
                               dim = c(3, 5),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    accession <- Counts(array(1:8,
                              dim = c(2, 4),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                              age = c("10", "20", "30", "40"))))
    accession <- Accession(accession)
    mapping <- Mapping(current = births,
                       target = accession)
    for (i in 1:8) {
        ans.obtained <- getIAccNextFromComp(i = i, mapping = mapping)
        ans.expected <- rep(c(2L, 0L), times = 4)[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## time is second dimension of three
    births <- Counts(array(1:36,
                           dim = c(3, 2, 2, 2),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = c("2001-2010", "2011-2020"),
                                           age = c("10-19", "20-29"),
                                           triangle = c("TL", "TU"))))
    population <- Counts(array(1:36,
                               dim = c(3, 3, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2010, 2020),
                                               age = c("0-9", "10-19", "20-29", "30+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    accession <- Counts(array(1:36,
                               dim = c(3, 2, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c("2001-2010", "2011-2020"),
                                               age = c("10", "20", "30", "40"))))
    accession <- Accession(accession)
    mapping <- Mapping(current = births,
                       target = accession)
    ans.exp <- rep(c(4:6, rep(0L, 3)), times = 4)
    for (i in 1:24) {
        ans.obtained <- getIAccNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.exp[i]
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of getIAccNextFromComp give same answer with BirthsMovementNoParentChild", {
    getIAccNextFromComp <- demest:::getIAccNextFromComp
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## time is first dimension of two
    births <- Counts(array(1:8,
                           dim = c(2, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020"),
                                           triangle = c("TL", "TU"),
                                           age = c("10-19", "20-29"))))
    population <- Counts(array(1:15,
                               dim = c(3, 5),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    accession <- Counts(array(1:8,
                              dim = c(2, 4),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                              age = c("10", "20", "30", "40"))))
    accession <- Accession(accession)
    mapping <- Mapping(current = births,
                       target = accession)
    for (i in 1:8) {
        ans.R <- getIAccNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIAccNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time is second dimension of three
    births <- Counts(array(1:36,
                           dim = c(3, 2, 2, 2),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = c("2001-2010", "2011-2020"),
                                           age = c("10-19", "20-29"),
                                           triangle = c("TL", "TU"))))
    population <- Counts(array(1:36,
                               dim = c(3, 3, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2010, 2020),
                                               age = c("0-9", "10-19", "20-29", "30+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    accession <- Counts(array(1:36,
                               dim = c(3, 2, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c("2001-2010", "2011-2020"),
                                               age = c("10", "20", "30", "40"))))
    accession <- Accession(accession)
    mapping <- Mapping(current = births,
                       target = accession)
    ans.exp <- rep(c(4:6, rep(0L, 3)), times = 4)
    for (i in 1:24) {
        ans.R <- getIAccNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIAccNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})


## births with parent

test_that("getIAccNextFromComp works with BirthsMovementHasParentChild", {
    getIAccNextFromComp <- demest:::getIAccNextFromComp
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one parent-child, has age
    births <- Counts(array(1:72,
                           dim = c(2, 3, 3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020"),
                                           eth_parent = 1:3,
                                           eth_child = 1:3,
                                           triangle = c("TL", "TU"),
                                           age = c("10-19", "20-29"))))
    population <- Counts(array(1:15,
                               dim = c(3, 3, 5),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               eth = 1:3,
                                               age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    accession <- Counts(array(1:30,
                              dim = c(2, 3, 5),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                              eth = 1:3,
                                              age = c("10", "20", "30", "40", "50"))))
    accession <- Accession(accession)
    mapping <- Mapping(current = births,
                       target = accession)
    ans.exp <- rep(c(2L, 0L, 2L, 0L, 2L, 0L,
                     4L, 0L, 4L, 0L, 4L, 0L,
                     6L, 0L, 6L, 0L, 6L, 0L),
                   times = 4)
    for (i in 1:72) {
        ans.obtained <- getIAccNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.exp[i]
        expect_identical(ans.obtained, ans.expected)
    }
    ## one parent-child, has age
    births <- Counts(array(1:72,
                           dim = c(3, 3, 2, 2, 2),
                           dimnames = list(reg_parent = c("a", "b", "c"),
                                           reg_child = c("a", "b", "c"),
                                           time = c("2001-2010", "2011-2020"),
                                           age = c("10-19", "20-29"),
                                           triangle = c("TL", "TU"))))
    population <- Counts(array(1:36,
                               dim = c(3, 3, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2010, 2020),
                                               age = c("0-9", "10-19", "20-29", "30+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    accession <- Counts(array(1:24,
                              dim = c(3, 2, 4),
                              dimnames = list(reg = c("a", "b", "c"),
                                              time = c("2001-2010", "2011-2020"),
                                              age = c("10", "20", "30", "40"))))
    accession <- Accession(accession)
    mapping <- Mapping(current = births,
                       target = accession)
    ans.exp <- rep(c(4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L,
                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
                   times = 4L)
    for (i in 1:72) {
        ans.obtained <- getIAccNextFromComp(i = i, mapping = mapping)
        ans.expected <- ans.exp[i]
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions getIAccNextFromComp give same answer with BirthsMovementHasParentChild", {
    getIAccNextFromComp <- demest:::getIAccNextFromComp
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    Accession <- dembase:::Accession
    makeTemplateComponent <- dembase:::makeTemplateComponent
    Mapping <- demest:::Mapping
    ## one parent-child, has age
    births <- Counts(array(1:72,
                           dim = c(2, 3, 3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020"),
                                           eth_parent = 1:3,
                                           eth_child = 1:3,
                                           triangle = c("TL", "TU"),
                                           age = c("10-19", "20-29"))))
    population <- Counts(array(1:15,
                               dim = c(3, 3, 5),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               eth = 1:3,
                                               age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    accession <- Counts(array(1:30,
                              dim = c(2, 3, 5),
                              dimnames = list(time = c("2001-2010", "2011-2020"),
                                              eth = 1:3,
                                              age = c("10", "20", "30", "40", "50"))))
    accession <- Accession(accession)
    mapping <- Mapping(current = births,
                       target = accession)
    ans.exp <- rep(c(2L, 0L, 2L, 0L, 2L, 0L,
                     4L, 0L, 4L, 0L, 4L, 0L,
                     6L, 0L, 6L, 0L, 6L, 0L),
                   times = 4)
    for (i in 1:72) {
        ans.R <- getIAccNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIAccNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## one parent-child, has age
    births <- Counts(array(1:72,
                           dim = c(3, 3, 2, 2, 2),
                           dimnames = list(reg_parent = c("a", "b", "c"),
                                           reg_child = c("a", "b", "c"),
                                           time = c("2001-2010", "2011-2020"),
                                           age = c("10-19", "20-29"),
                                           triangle = c("TL", "TU"))))
    population <- Counts(array(1:36,
                               dim = c(3, 3, 4),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2010, 2020),
                                               age = c("0-9", "10-19", "20-29", "30+"))))
    population <- Population(population)
    template <- makeTemplateComponent(population)
    births <- BirthsMovements(births = births,
                              template = template)
    accession <- Counts(array(1:24,
                              dim = c(3, 2, 4),
                              dimnames = list(reg = c("a", "b", "c"),
                                              time = c("2001-2010", "2011-2020"),
                                              age = c("10", "20", "30", "40"))))
    accession <- Accession(accession)
    mapping <- Mapping(current = births,
                       target = accession)
    ans.exp <- rep(c(4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L,
                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
                   times = 4L)
    for (i in 1:72) {
        ans.R <- getIAccNextFromComp(i = i, mapping = mapping, useC = FALSE)
        ans.C <- getIAccNextFromComp(i = i, mapping = mapping, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})





## MAPPINGS FROM EXPOSURE ################################################################


## test_that("getICellCompFromExp works with generic Component", {
##     getICellCompFromExp <- demest:::getICellCompFromExp
##     ExitsMovements <- dembase:::ExitsMovements
##     Exposure <- dembase:::Exposure
##     Population <- dembase:::Population
##     makeTemplateComponent <- dembase:::makeTemplateComponent
##     Mapping <- demest:::Mapping
##     ## time is first dimension of two
##     component <- Counts(array(1:8,
##                               dim = c(2, 2, 2),
##                               dimnames = list(time = c("2001-2010", "2011-2020"),
##                                   triangle = c("TL", "TU"),
##                                   age = c("0-9", "10+"))))
##     population <- Counts(array(1:6,
##                                dim = c(3, 2),
##                                dimnames = list(time = c(2000, 2010, 2020),
##                                    age = c("0-9", "10+"))))
##     population <- Population(population)
##     exposure <- exposure(population, triangles = TRUE)
##     exposure <- Exposure(exposure)
##     template <- makeTemplateComponent(population)
##     component <- ExitsMovements(exits = component,
##                                 template = template,
##                                 name = "exits")
##     mapping <- Mapping(current = exposure,
##                        target = component)
##     for (i in 1:8) {
##         ans.obtained <- getICellCompFromExp(i = i, mapping = mapping)
##         ans.expected <- i
##         expect_identical(ans.obtained, ans.expected)
##     }
## })





## test_that("getICellCompFromExp works with BirthsNoParentChild", {
##     getICellCompFromExp <- demest:::getICellCompFromExp
##     BirthsMovements <- dembase:::BirthsMovements
##     Population <- dembase:::Population
##     Exposure <- dembase:::Exposure
##     makeTemplateComponent <- dembase:::makeTemplateComponent
##     Mapping <- demest:::Mapping
##     ## time is first dimension of two
##     births <- Counts(array(1:8,
##                            dim = c(2, 2, 2),
##                            dimnames = list(time = c("2001-2010", "2011-2020"),
##                                triangle = c("TL", "TU"),
##                                age = c("10-19", "20-29"))))
##     population <- Counts(array(1:15,
##                                dim = c(3, 5),
##                                dimnames = list(time = c(2000, 2010, 2020),
##                                    age = c("0-9", "10-19", "20-29", "30-39", "40+"))))
##     population <- Population(population)
##     template <- makeTemplateComponent(population)
##     births <- BirthsMovements(births = births,
##                               template = template)
##     exposure <- exposure(population, triangles = TRUE)
##     exposure <- Exposure(exposure)
##     mapping <- Mapping(current = exposure,
##                        target = births)
##     for (i in 1:8) {
##         ans.obtained <- getICellCompFromExp(i = i, mapping = mapping)
##         ans.expected <- 3L - (i %% 2L)
##         expect_identical(ans.obtained, ans.expected)
##     }
##     ## time is second dimension of two
##     births <- Counts(array(1:3,
##                               dim = c(3, 1),
##                               dimnames = list(reg = c("a", "b", "c"),
##                                   time = "2001-2010")))
##     population <- Counts(array(1:6,
##                                dim = c(3, 2),
##                                dimnames = list(reg = c("a", "b", "c"),
##                                    time = c(2000, 2010))))
##     population <- Population(population)
##     template <- makeTemplateComponent(population)
##     births <- BirthsMovements(births = births,
##                               template = template)
##     population <- Population(population)
##     mapping <- Mapping(current = births,
##                        target = population)
##     for (i in 1:3) {
##         ans.obtained <- getICellCompFromExp(i = i, mapping = mapping)
##         ans.expected <- i + 3L
##         expect_identical(ans.obtained, ans.expected)
##     }
##     ## time is second dimension of three
##     births <- Counts(array(1:36,
##                               dim = c(3, 2, 2, 2),
##                               dimnames = list(reg = c("a", "b", "c"),
##                                   time = c("2001-2010", "2011-2020"),
##                                   age = c("10-19", "20-29"),
##                                   triangle = c("TL", "TU"))))
##     population <- Counts(array(1:36,
##                                dim = c(3, 3, 4),
##                                dimnames = list(reg = c("a", "b", "c"),
##                                    time = c(2000, 2010, 2020),
##                                    age = c("0-9", "10-19", "20-29", "30+"))))
##     population <- Population(population)
##     template <- makeTemplateComponent(population)
##     births <- BirthsMovements(births = births,
##                               template = template)
##     mapping <- Mapping(current = births,
##                        target = population)
##     for (i in 1:24) {
##         ans.obtained <- getICellCompFromExp(i = i, mapping = mapping)
##         ans.expected <- rep(4:9, times = 4)[i]
##         expect_identical(ans.obtained, ans.expected)
##     }
##     ## only has time dimension
##     births <- Counts(array(1:11,
##                            dim = 11,
##                            dimnames = list(time = paste(seq(2001, by = 5, len = 11),
##                                                seq(2005, by = 5, len = 11),
##                                                sep = "-"))))
##     population <- Counts(array(1:12,
##                                dim = 12,
##                                dimnames = list(time = seq(from = 2000,
##                                                    by = 5,
##                                                    length = 12))))
##     population <- Population(population)
##     template <- makeTemplateComponent(population)
##     births <- BirthsMovements(births = births,
##                               template = template)
##     mapping <- Mapping(current = births,
##                        target = population)
##     for (i in 1:11) {
##         ans.obtained <- getICellCompFromExp(i = i, mapping = mapping)
##         ans.expected <- i + 1L
##         expect_identical(ans.obtained, ans.expected)
##     }
## })

