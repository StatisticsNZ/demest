



context("recover-parameters")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

## Create some fake data, and see if a model based on essentially the same
## specification can recover the original parameters


if (FALSE) {
    exposure <- 1000 * rbeta(n = 40, shape1 = 1, shape2 = 1)
    exposure <- array(exposure,
                      dim = c(2, 20),
                      dimnames = list(sex = c("Female", "Male"), time = 2000:2019))
    exposure <- Counts(exposure)
    sex.mean <- ValuesOne(c(0.1, -0.1), labels = c("Female", "Male"), name = "sex")
    sex.prior <- Known(sex.mean, sd = 0.1)
    order <- sample(1:3, size = 1)
    time.prior <-  Poly(order = order,
                        sdObs = runif(n = 1, min = 0, max = 1),
                        sdTrend = rnorm(n = order, min = 0, max = 0.5),
                        initialTrend = rnorm(n = order, sd = 0.2))
    model.fake <- Model(y ~ Poisson(mean ~ sex + time,
                                    priorSD = list(df = 5, scale = 0.1)),
                        sex ~ sex.prior,
                        time ~ time.prior)
    model.est <-  Model(y ~ Poisson(mean ~ sex + time),
                        time ~ Poly(order = 2))
    fake.data <- fakeData(model = model.fake,
                          intercept = -0.5,
                          templateY = exposure,
                          exposure = exposure)
    y.fake <- fetch(fake.data, where = "y")
    res <- estimateModel(model = model.est,
                         y = y.fake,
                         exposure = exposure,
                         nBurnin = 1000,
                         nSim = 1000,
                         nThin = 10,
                         filename = tempfile())

    theta.est <- fetch(res, where = c("model", "likelihood", "mean"))
    theta.fake <- fetch(fake.data, c("model", "likelihood", "mean"))
    theta.mean.diff <- mean(collapseIterations(theta.est, FUN = mean) - theta.fake)
    theta.coverage <- coverage(estimated = theta.est, true = theta.fake, percent = 95)
    dplot(~ time | sex,
          data = theta.est,
          col = "light blue",
          midpoints = "time",
          overlay = list(values = theta.fake, col = "red"))

    where <- c("model", "prior", "param", "sex")
    sex.est <- fetch(res, where = where)
    sex.fake <- fetch(fake.data, where = where)
    sex.mean.diff <- mean(collapseIterations(sex.est, FUN = mean) - sex.fake)
    sex.coverage <- coverage(estimated = sex.est, true = sex.fake, percent = 95)
    dplot(~ sex,
          data = sex.est,
          col = "light blue",
          overlay = list(values = sex.fake, col = "red"))


    where <- c("model", "prior", "param", "sex")
    sex.est <- fetch(res, where = where)
    sex.fake <- fetch(fake.data, where = where)
    sex.mean.diff <- mean(collapseIterations(sex.est, FUN = mean) - sex.fake)
    sex.coverage <- coverage(estimated = sex.est, true = sex.fake, percent = 95)
    dplot(~ sex,
          data = sex.est,
          col = "light blue",
          overlay = list(values = sex.fake, col = "red"))

    where <- c("model", "prior", "param", "time")
    time.est <- fetch(res, where = where)
    time.fake <- fetch(fake.data, where = where)
    time.mean.diff <- mean(collapseIterations(time.est, FUN = mean) - time.fake)
    time.coverage <- coverage(estimated = time.est, true = time.fake, percent = 95)
    dplot(~ time,
          data = time.est,
          col = "light blue",
          overlay = list(values = time.fake, col = "red"))


    where <- c("model", "prior", "sd")
    sd.est <- fetch(res, where = where)
    sd.fake <- fetch(fake.data, where = where)
    ## sd.mean.diff <- mean(sd.est) - sd.fake
    ## sd.coverage <- coverage(estimated = sd.est, true = sd.fake, percent = 95)
    dplot(~ sd,
          data = sd.est,
          col = "light blue",
          overlay = list(values = sd.fake, col = "red"))

}

## AR1
if (FALSE) {

    
    exposure <- 1000 * rbeta(n = 40, shape1 = 1, shape2 = 1)
    exposure <- array(exposure,
                      dim = c(10, 2, 51),
                      dimnames = list(region = letters[1:10],
                          sex = c("Female", "Male"),
                          time = 2000:2050))
    exposure <- Counts(exposure)
    region.prior <- Exch(sd = 0.2)
    sex.mean <- ValuesOne(c(0.1, -0.1), labels = c("Female", "Male"), name = "sex")
    sex.prior <- Known(sex.mean, sd = 0.1)
    time.prior <-  Poly(order = 1,
                        sdObs = runif(n = 1, min = 0, max = 0.25),
                        sdTrend = runif(n = 1, min = 0, max = 0.25),
                        initialTrend = rnorm(n = 1, sd = 0.4))
    region.time.prior <- AR1(coef = 0.7, sdObs = 0.05, sdTrend = 0.05)
    model.fake <- Model(y ~ Poisson(mean ~ sex + region * time,
                                    priorSD = list(df = 5, scale = 0.1)),
                        region ~ region.prior,
                        sex ~ sex.prior,
                        time ~ time.prior,
                        region:time ~ region.time.prior)
    model.est1 <-  Model(y ~ Poisson(mean ~ sex + region + time),
                        time ~ Poly(order = 1))
    model.est2 <-  Model(y ~ Poisson(mean ~ sex + region * time),
                        time ~ Poly(order = 1),
                        region:time ~ AR1())
    fake.data <- fakeData(model = model.fake,
                          intercept = -1.5,
                          templateY = exposure,
                          exposure = exposure)
    y.fake <- fetch(fake.data, where = "y")
    y.fake <- subarray(y.fake, time < 2030)
    res1 <- estimateModel(model = model.est1,
                         y = y.fake,
                         exposure = exposure,
                         nBurnin = 10000,
                         nSim = 10000,
                         nThin = 25,
                         nChain = 4,
                         filename = tempfile())
    res2 <- estimateModel(model = model.est2,
                         y = y.fake,
                         exposure = exposure,
                         nBurnin = 10000,
                         nSim = 10000,
                         nThin = 25,
                         nChain = 4,
                         filename = tempfile())
    pred1 <- predictModel(res1, labels = as.character(2030:2050))
    pred2 <- predictModel(res2, labels = as.character(2030:2050))
    theta.est1 <- dbind(fetch(res1, where = c("model", "likelihood", "mean")),
                        fetch(pred1, where = c("model", "likelihood", "mean")),
                        along = "time")
    theta.est2 <- dbind(fetch(res2, where = c("model", "likelihood", "mean")),
                        fetch(pred2, where = c("model", "likelihood", "mean")),
                        along = "time")
    theta.est <- dbind("No AR" = theta.est1, "Has AR" = theta.est2, along = "variant")
    theta.fake <- fetch(fake.data, c("model", "likelihood", "mean"))

    
    useOuterStrips(dplot(~ time | variant * region,
                         data = theta.est,
                         subarray = sex == "Female" & time > 2025 & time < 2035,
                         groups = variant,
                         col = "light blue",
                         midpoints = "time",
                         overlay = list(values = subarray(theta.fake, sex == "Female"),
                             col = "red")))


    theta.mean.diff <- mean(collapseIterations(theta.est, FUN = mean) - theta.fake)
#    theta.coverage <- coverage(estimated = theta.est, true = theta.fake, percent = 95)

    reg.time.est <- fetch(res, where = c("model", "prior", "param", "region:time"))
    reg.time.beta <- fetch(fake.data, where = c("model", "prior", "param", "region:time"))

    dplot(~ time | region,
          data = reg.time,
          col = "light blue",
          overlay = list(values = reg.time.beta, col = "red"))

                               

    

    where <- c("model", "prior", "param", "sex")
    sex.est <- fetch(res, where = where)
    sex.fake <- fetch(fake.data, where = where)
    sex.mean.diff <- mean(collapseIterations(sex.est, FUN = mean) - sex.fake)
    sex.coverage <- coverage(estimated = sex.est, true = sex.fake, percent = 95)
    dplot(~ sex,
          data = sex.est,
          col = "light blue",
          overlay = list(values = sex.fake, col = "red"))


    where <- c("model", "prior", "param", "sex")
    sex.est <- fetch(res, where = where)
    sex.fake <- fetch(fake.data, where = where)
    sex.mean.diff <- mean(collapseIterations(sex.est, FUN = mean) - sex.fake)
    sex.coverage <- coverage(estimated = sex.est, true = sex.fake, percent = 95)
    dplot(~ sex,
          data = sex.est,
          col = "light blue",
          overlay = list(values = sex.fake, col = "red"))

    where <- c("model", "prior", "param", "time")
    time.est <- fetch(res, where = where)
    time.fake <- fetch(fake.data, where = where)
    time.mean.diff <- mean(collapseIterations(time.est, FUN = mean) - time.fake)
    time.coverage <- coverage(estimated = time.est, true = time.fake, percent = 95)
    dplot(~ time,
          data = time.est,
          col = "light blue",
          overlay = list(values = time.fake, col = "red"))


    where <- c("model", "prior", "sd")
    sd.est <- fetch(res, where = where)
    sd.fake <- fetch(fake.data, where = where)
    ## sd.mean.diff <- mean(sd.est) - sd.fake
    ## sd.coverage <- coverage(estimated = sd.est, true = sd.fake, percent = 95)
    dplot(~ sd,
          data = sd.est,
          col = "light blue",
          overlay = list(values = sd.fake, col = "red"))

}
