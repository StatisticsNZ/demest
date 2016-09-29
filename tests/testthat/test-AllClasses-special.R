

context("AllClasses-special")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("can create valid object of class Offsets", {
    x <- new("Offsets", 1:2)
    expect_true(validObject(x))
})

test_that("validity tets for Offsets inherited from Offsets work", {
    x <- new("Offsets", 1:2)
    ## 'object' has length 2
    expect_error(new("Offsets", 1:3),
                 "'offsets' does not have length 2")
    ## 'object' has no missing values
    expect_error(new("Offsets", c(1L, NA)),
                 "'offsets' has missing values")
    ## all elements of 'object' are positive
    expect_error(new("Offsets", c(0L, 1L)),
                 "'offsets' has non-positive elements")
    ## second element >= first element
    expect_error(new("Offsets", c(2L, 1L)),
                 "second element of 'offsets' less than first")
})

