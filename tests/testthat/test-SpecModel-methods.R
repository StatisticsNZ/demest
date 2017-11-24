
context("SpecModel-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

test_that("modelUsesWeights works", {
    modelUsesWeights <- demest:::modelUsesWeights
    spec <- new("SpecPoissonVarying")
    expect_false(modelUsesWeights(spec))
    spec <- new("SpecNormalVaryingVarsigmaKnown")
    expect_true(modelUsesWeights(spec))
})







