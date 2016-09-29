
context("SpecPrior-generators")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE


## ## Known

## test_that("Known creates object of class SpecKnownCertain from valid inputs", {
##     mean <- Values(array(1:3, dim = 3, dimnames = list(age = 0:2)))
##     x <- Known(mean = mean)
##     expect_true(validObject(x))
##     expect_is(x, "SpecKnownCertain")
## })

## test_that("Known creates object of class SpecKnownUncertain from valid inputs", {
##     mean <- Values(array(1:3, dim = 3, dimnames = list(age = 0:2)))
##     sd <- sqrt(mean)
##     x <- Known(mean = mean, sd = sd)
##     expect_true(validObject(x))
##     expect_is(x, "SpecKnownUncertain")
##     x <- Known(mean = mean, sd = 1)
##     expect_true(validObject(x))
##     expect_is(x, "SpecKnownUncertain")
##     expect_identical(x@sd, rep(as.double(1), 3))
##     x <- Known(mean = mean, sd = 0)
##     expect_true(validObject(x))
##     expect_is(x, "SpecKnownUncertain")
## })

## test_that("Known throws appropriate errors", {
##     mean <- Values(array(1:3, dim = 3, dimnames = list(age = 0:2)))
##     sd <- sqrt(mean)
##     ## 'mean' is "Values"
##     expect_error(Known(mean = "wrong"),
##                  "'mean' has class \"character\"")
##     ## 'mean' has no missing values
##     mean.wrong <- mean
##     mean.wrong[1] <- NA
##     expect_error(Known(mean = mean.wrong),
##                  "'mean' has missing values")
##     ## 'sd' is compatible with 'mean'
##     expect_error(Known(mean = mean, sd = sd[1:2]),
##                  "'sd' and 'mean' not compatible :")
##     ## 'sd' has no missing values
##     sd.wrong <- sd
##     sd.wrong[1] <- NA
##     expect_error(Known(mean = mean, sd = sd.wrong),
##                  "'sd' has missing values")
##     ## 'sd' has no negative values
##     sd.wrong <- sd
##     sd.wrong[1] <- -1
##     expect_error(Known(mean = mean, sd = sd.wrong),
##                  "'sd' has negative values")
##     ## 'sd' has length 1
##     expect_error(Known(mean = mean, sd = 1:2),
##                  "'sd' is numeric but does not have length 1")
##     ## 'sd' is not missing
##     expect_error(Known(mean = mean, sd = as.numeric(NA)),
##                  "'sd' is missing")
##     ## 'sd' is non-negative
##     expect_error(Known(mean = mean, sd = -1),
##                  "'sd' is negative")
##     ## 'sd' is Values or numeric
##     expect_error(Known(mean = mean, sd = "wrong"),
##                  "'sd' has class \"character\"")
## })








