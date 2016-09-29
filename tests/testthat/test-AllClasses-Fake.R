
context("AllClasses-Fake")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE



test_that("can create valid object of class FakeData", {
    y <- Counts(array(1:4, dim = 4, dimnames = list(age = 0:3)))
    theta <- y * 0.98
    x <- new("FakeData",
             y = y,
             model = list(likelihood = list(theta = theta),
             prior = list(param = list("(Intercept)" = 3),
             sd = 1.2)))
})

test_that("validity tests for FakeData inherited from FakeData work", {
    y <- Counts(array(1:4, dim = 4, dimnames = list(age = 0:3)))
    theta <- y * 0.98
    x <- new("FakeData",
             y = y,
             model = list(likelihood = list(theta = theta),
             prior = list(param = list("(Intercept)" = 3),
             sd = 1.2)))
    ## 'y' does not have iteration or quantile dimensions
    y <- Counts(array(1:8, dim = c(4, 2), dimnames = list(age = 0:3, iteration = 1:2)))
    theta <- y * 0.98
    expect_error(new("FakeData",
                     y = y,
                     model = list(likelihood = list(theta = theta),
                     prior = list(param = list("(Intercept)" = 3),
                     sd = 1.2))),
                 "'y' has dimension with dimtype \"iteration\"")
    ## all elements of 'model' have type "numeric"
    x.wrong <- x
    x.wrong@model$theta <- "wrong"
    expect_error(validObject(x.wrong),
                 "'model' has elements not of type \"numeric\"")
})


test_that("can create valid object of class FakeDataExposure", {
    y <- Counts(array(1:4, dim = 4, dimnames = list(age = 0:3)))
    exposure <- y + 1
    theta <- y / exposure
    x <- new("FakeDataExposure",
             y = y,
             exposure = exposure,
             model = list(likelihood = list(theta = theta),
             prior = list(param = list("(Intercept)" = 3),
             sd = 1.2)))
})

test_that("validity tests for FakeDataExposure inherited from FakeDataExposure work", {
    y <- Counts(array(1:4, dim = 4, dimnames = list(age = 0:3)))
    exposure <- y + 1
    theta <- y / exposure
    x <- new("FakeDataExposure",
             y = y,
             exposure = exposure,
             model = list(likelihood = list(theta = theta),
             prior = list(param = list("(Intercept)" = 3),
             sd = 1.2)))
    ## 'y', 'exposure' are "Counts
    x.wrong <- x
    x.wrong@y <- as(x.wrong@y, "Values")
    expect_error(validObject(x.wrong),
                 "'y' has class \"Values\"")
    ## 'exposure' has same metadata as 'y'
    x.wrong <- x
    x.wrong@exposure <- x.wrong@exposure[1:2]
    expect_error(validObject(x.wrong),
                 "'exposure' and 'y' have different metadata")
})

 
