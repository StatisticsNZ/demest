
context("SpecPriors-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE




## show ########################################################

if (FALSE) {
    
    test_that("show works with Covariates", {
        data <- data.frame(region = letters[1:10],
                           income = rnorm(10),
                           area = rep(c("u", "r"), each = 5))
        covariates <- Covariates(mean ~ income + area,
                                 data = data)
        show(covariates)
        covariates <- Covariates(mean ~ income + area,
                                 data = data,
                                 contrastsArg = list(area = diag(2)))
        show(covariates)
        covariates <- Covariates(mean ~ income + area,
                                 data = data,
                                 contrastsArg = list(area = diag(2)),
                                 intercept = Norm(scale = 2),
                                 coef = HalfT(df = 4, scale = 2, max = 5))
        show(covariates)
    })

    test_that("show works with ErrorNorm", {
        error <- Error()
        show(error)
        error <- Error(scale = HalfT(scale = 3, max = 5))
        show(error)
    })

    test_that("show works with ErrorRobust", {
        error <- Error(robust = TRUE)
        show(error)
        error <- Error(robust = TRUE, df = 10, scale = HalfT(scale = 3, max = 5))
        show(error)
    })

}
