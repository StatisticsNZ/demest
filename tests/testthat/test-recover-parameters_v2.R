

context("recover-parameters")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


## test mixture prior
if (FALSE) {

    drawData <- function(mean.level.component.weight = 0,
                         phi = 0.95,
                         omega.vectors = 0.5,
                         omega.component.weight = 0.1,
                         omega.level.component.weight = 0.1,
                         tau = 0.5,
                         index.class.max = 10) {
        rtnorm1 <- demest:::rtnorm1
        initialPrior <- demest:::initialPrior
        n.time <- 10
        dv.time <- seq(from = 2001, length = n.time)
        metadata <- new("MetaData",
                        nms = c("time", "age", "sex"),
                        dimtypes = c("time", "age", "sex"),
                        DimScales = list(new("Points", dimvalues = dv.time),
                                         new("Intervals", dimvalues = as.numeric(0:10)),
                                         new("Sexes", dimvalues = c("Female", "Male"))))
        n.beta.no.along <- as.integer(prod(dim(metadata)[-1]))
        J <- as.integer(prod(dim(metadata)))
        vectors <- list(numeric(),
                        rnorm(n = 10 * index.class.max, sd = omega.vectors),
                        rnorm(n = 2 * index.class.max, sd = omega.vectors))
        vec2 <- matrix(vectors[[2]], ncol = index.class.max)
        vec3 <- matrix(vectors[[3]], ncol = index.class.max)
        prod.vectors <- sapply(1:index.class.max,
                               function(i) outer(vec2[,i], vec3[,i]))
        level.component.weight <- matrix(nrow = n.time, ncol = index.class.max)
        level.component.weight[1,] <- rnorm(n = index.class.max,
                                            mean = mean.level.component.weight / (1 - phi),
                                            sd = omega.level.component.weight / sqrt(1 - phi^2))
        for (i in 2:n.time)
            level.component.weight[i,] <- rnorm(n = index.class.max,
                                                mean = (mean.level.component.weight
                                                    + phi * level.component.weight[i-1,]),
                                                sd = omega.level.component.weight)
        level.component.weight <- as.numeric(level.component.weight)
        component.weight <- rnorm(n = n.time * index.class.max,
                                  mean = level.component.weight,
                                  sd = omega.component.weight)
        makeWeight <- function(W) {
            gW <- pnorm(W)
            ans <- gW
            n <- length(ans)
            if (n > 1) {
                for (i in 2:n)
                    ans[i] <- ans[i] * prod(1-gW[1:(i-1)])
            }
            ans
        }
        weight <- apply(matrix(component.weight, ncol = index.class.max),
                        1,
                        makeWeight)
        weight <- t(weight)
        chooseIndexClass <- function(time)
            sample(1:index.class.max, size = 1, prob = weight[time, ])
        index.class <- sapply(rep(1:n.time, times = n.beta.no.along),
                              chooseIndexClass)
        makeLatCompWt <- function(index, time) {
            mean <- weight[time, index]
            ans <- numeric(length = index.class.max)
            if (index > 1) {
                for (i in 1:index)
                    ans[i] <- rtnorm1(mean = mean, sd = 1, upper = 0)
            }
            ans[index] <- rtnorm1(mean = mean, sd = 1, lower = 0)
            if (index < index.class.max) {
                for (i in (index + 1):index.class.max)
                    ans[i] <- rnorm(n = 1, mean = mean, sd = 1)
            }
            ans
        }
        i.time <- rep(1:n.time, times = n.beta.no.along)
        latent.component.weight <- matrix(nrow = J, ncol = index.class.max)
        for (i in 1:J) {
            latent.component.weight[i,] <- makeLatCompWt(index.class[i],
                                                         i.time[i])
        }
        v <- weight[cbind(rep(1:n.time, times = n.beta.no.along), index.class)]
        latent.weight <- runif(n = J, max = v)
        alpha <- prod.vectors[cbind(rep(1:n.beta.no.along, each = n.time), index.class)]
        beta.tilde <- rnorm(n = J, mean = alpha, sd = tau)
        spec <- Mix(maxClass = index.class.max)
        prior <- initialPrior(spec,
                              beta = beta.tilde,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        prior@alphaMix@.Data <- alpha
        prior@componentWeightMix@.Data <- component.weight
        prior@indexClassMix <- index.class
        prior@indexClassMaxUsedMix@.Data <- max(index.class)
        prior@indexClassMaxPossibleMix@.Data <- max(index.class)
        prior@latentWeightMix@.Data <- latent.weight
        prior@latentComponentWeightMix@.Data <- as.numeric(latent.component.weight)
        prior@levelComponentWeightMix@.Data <- level.component.weight
        prior@meanLevelComponentWeightMix@.Data <- mean.level.component.weight
        prior@omegaComponentWeightMix@.Data <- omega.component.weight
        prior@omegaLevelComponentWeightMix@.Data <- omega.level.component.weight
        prior@omegaVectorsMix@.Data <- omega.vectors
        prior@phiMix <- phi
        prior@prodVectorsMix@.Data <- as.numeric(prod.vectors)
        prior@vectorsMix <- lapply(vectors, function(x) new("ParameterVector", x))
        prior@weightMix@.Data <- as.numeric(weight)
        prior@tau@.Data <- tau
        stopifnot(validObject(prior))
        print(round(matrix(prior@weightMix@.Data, ncol = index.class.max), 2))
        print(table(index.class))
        list(prior = prior,
             beta.tilde = beta.tilde)
    }


    ## updateVectorsMixAndProdVectorsMix

    n.update <- 10
    l <- drawData(omega.vectors = 1)
    prior <- l$prior
    beta.tilde <- l$beta.tilde
    index.class.max <- prior@indexClassMaxMix@.Data
    dim.beta <- prior@dimBeta
    ans.age <- matrix(nrow = dim.beta[2] * index.class.max,
                      ncol = n.update) 
    ans.sex <- matrix(nrow = dim.beta[3] * index.class.max,
                      ncol = n.update)
    ans.prod <- matrix(nrow = dim.beta[2] * dim.beta[3] * index.class.max,
                       ncol = n.update)
    updateVectorsMixAndProdVectorsMix <- demest:::updateVectorsMixAndProdVectorsMix
    for (i in seq_len(n.update)) {
        prior1 <- updateVectorsMixAndProdVectorsMix(prior,
                                                    betaTilde = beta.tilde,
                                                    useC = TRUE)
        ans.age[,i] <- prior1@vectorsMix[[2]]@.Data
        ans.sex[,i] <- prior1@vectorsMix[[3]]@.Data
        ans.prod[,i] <- prior1@prodVectorsMix@.Data
    }
    ans.age <- rowMeans(ans.age)
    ans.sex <- rowMeans(ans.sex)
    ans.prod <- rowMeans(ans.prod)

    op <- par(mfrow = c(2, 2))
    plot(prior@vectorsMix[[2]]@.Data, ans.age)
    plot(prior@vectorsMix[[3]]@.Data, ans.sex)
    plot(prior@prodVectorsMix@.Data, ans.prod)
    par(op)

    n.comp <- 3
    op <- par(mfrow = c(2, 2))
    plot(prior@vectorsMix[[2]]@.Data[1 : (n.comp * dim.beta[2])],
         ans.age[1 : (n.comp * dim.beta[2])])
    plot(prior@vectorsMix[[3]]@.Data[1 : (n.comp * dim.beta[3])],
         ans.sex[1 : (n.comp * dim.beta[3])])
    plot(prior@prodVectorsMix@.Data[1 : (n.comp * dim.beta[2] * dim.beta[3])],
         ans.prod[1 : (n.comp * dim.beta[2] * dim.beta[3])]) 
    par(op)


    ## updateOmegaVectorsMix

    n.update <- 1000
    l <- drawData()
    prior <- l$prior
    ans <- numeric(length = n.update)
    updateOmegaVectorsMix <- demest:::updateOmegaVectorsMix
    for (i in seq_len(n.update)) {
        prior1 <- updateOmegaVectorsMix(prior,
                                        useC = TRUE)
        ans[i] <- prior1@omegaVectorsMix@.Data
    }
    plot(ans, type = "l")
    abline(h = prior@omegaVectorsMix@.Data)


    ## updateOmegaComponentWeightMix

    n.update <- 1000
    l <- drawData()
    prior <- l$prior
    ans <- numeric(length = n.update)
    updateOmegaComponentWeightMix <- demest:::updateOmegaComponentWeightMix
    for (i in seq_len(n.update)) {
        prior1 <- updateOmegaComponentWeightMix(prior,
                                                useC = TRUE)
        ans[i] <- prior1@omegaComponentWeightMix@.Data
    }
    plot(ans, type = "l")
    abline(h = prior@omegaComponentWeightMix@.Data)


    ## updateLevelComponentWeightMix

    n.update <- 1000
    l <- drawData()
    prior <- l$prior
    ans <- numeric(length = n.update)
    updateOmegaLevelComponentWeightMix <- demest:::updateOmegaLevelComponentWeightMix
    for (i in seq_len(n.update)) {
        prior1 <- updateOmegaLevelComponentWeightMix(prior,
                                                useC = TRUE)
        ans[i] <- prior1@omegaLevelComponentWeightMix@.Data
    }
    plot(ans, type = "l")
    abline(h = prior@omegaLevelComponentWeightMix@.Data)


    ## updateTau

    n.update <- 1000
    l <- drawData()
    prior <- l$prior
    ans <- numeric(length = n.update)
    updateTauNorm <- demest:::updateTauNorm
    for (i in seq_len(n.update)) {
        prior1 <- updateTauNorm(prior,
                                beta = beta.tilde,
                                useC = TRUE)
        ans[i] <- prior1@tau@.Data
    }
    plot(ans, type = "l")
    abline(h = prior@tau@.Data)


    ## updateMeanLevelComponentWeightMix

    n.update <- 1000
    l <- drawData()
    prior <- l$prior
    ans <- numeric(length = n.update)
    updateMeanLevelComponentWeightMix <- demest:::updateMeanLevelComponentWeightMix
    for (i in seq_len(n.update)) {
        prior1 <- updateMeanLevelComponentWeightMix(prior,
                                                    useC = TRUE)
        ans[i] <- prior1@meanLevelComponentWeightMix@.Data
    }
    plot(ans, type = "l")
    abline(h = prior@meanLevelComponentWeightMix@.Data)


    ## updatePhiMix

    n.update <- 1000
    l <- drawData()
    prior <- l$prior
    ans <- numeric(length = n.update)
    updatePhiMix <- demest:::updatePhiMix
    for (i in seq_len(n.update)) {
        prior1 <- updatePhiMix(prior,
                               useC = TRUE)
        ans[i] <- prior1@phiMix
    }
    plot(ans, type = "l")
    abline(h = prior@phiMix)






    n.update <- 10
    ans.age <- matrix(nrow = 10 * index.class.max, ncol = n.update)
    ans.sex <- matrix(nrow = 2 * index.class.max, ncol = n.update)
    ans.prod <- matrix(nrow = 10 * 2 * index.class.max, ncol = n.update)
    updateVectorsMixAndProdVectorsMix <- demest:::updateVectorsMixAndProdVectorsMix
    for (i in seq_len(n.update)) {
        prior1 <- updateVectorsMixAndProdVectorsMix(prior,
                                                    betaTilde = beta.tilde,
                                                    useC = TRUE)
        ans.age[,i] <- prior1@vectorsMix[[2]]@.Data
        ans.sex[,i] <- prior1@vectorsMix[[3]]@.Data
        ans.prod[,i] <- prior1@prodVectorsMix@.Data
    }
    ans.age <- rowMeans(ans.age)
    ans.sex <- rowMeans(ans.sex)
    ans.prod <- rowMeans(ans.prod)

    op <- par(mfrow = c(2, 2))
    plot(prior@vectorsMix[[2]]@.Data, ans.age)
    plot(prior@vectorsMix[[3]]@.Data, ans.sex)
    plot(prior@prodVectorsMix@.Data, ans.prod)
    par(op)


    


    ## updateLatentComponentWeightMix
    prior <- drawData()
    n.update <- 1000
    ans <- matrix(nrow = J * index.class.max, ncol = n.update)
    updateLatentComponentWeightMix <- demest:::updateLatentComponentWeightMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateLatentComponentWeightMix(prior1,
                                                 useC = TRUE)
        ans[,i] <- prior1@latentComponentWeightMix@.Data
    }
    ans.all <- rowMeans(ans)
    plot(prior@latentComponentWeightMix@.Data, ans.all)










    
## updatePrior
    n.update <- 10000
    ## ans <- matrix(nrow = J, ncol = n.update)
    ans <- matrix(nr = 8, ncol = n.update)
    for (i in seq_len(n.update)) {
        ## l <- updateBetaAndPriorBeta(prior,
        ##                             vbar = beta.tilde,
        ##                             n = 100L,
        ##                             sigma = 0.001,
        ##                             useC = TRUE)
        prior1 <- updateTauNorm(prior = prior,
                                beta = beta.tilde,
                                useC = TRUE) # sigma-delta
        ## vectors
        prior1 <- updateVectorsMixAndProdVectorsMix(prior = prior1,
                                                    betaTilde = beta.tilde,
                                                    useC = TRUE) # psi
        prior1 <- updateOmegaVectorsMix(prior1,
                                        useC = TRUE) # sigma-e
        ## weights
        prior1 <- updateLatentComponentWeightMix(prior1,
                                                 useC = TRUE) # z
        prior1 <- updateComponentWeightMix(prior1,
                                           useC = TRUE) # W
        prior1 <- updateWeightMix(prior1,
                                  useC = TRUE) # v; deterministic
        prior1 <- updateLatentWeightMix(prior1,
                                        useC = TRUE) # u
        prior1 <- updateOmegaComponentWeightMix(prior1,
                                                useC = TRUE) # sigma-epsilon
        prior1 <- updateOmegaLevelComponentWeightMix(prior1,
                                                     useC = TRUE) # sigma-eta 
        prior1 <- updateIndexClassMaxPossibleMix(prior1,
                                                 useC = TRUE) # k-tilde
        prior1 <- updateIndexClassMix(prior = prior1,
                                      betaTilde = beta.tilde,
                                      useC = TRUE) # k
        prior1 <- updateIndexClassMaxUsedMix(prior1,
                                             useC = TRUE) # k-star; deterministic
        prior1 <- updateLevelComponentWeightMix(prior1,
                                                useC = TRUE) # alpha
        prior1 <- updateMeanLevelComponentWeightgMix(prior1,
                                                    useC = TRUE) # mu
        prior1 <- updatePhiMix(prior1,
                               useC = TRUE) # phi
        prior1 <- updateAlphaMix(prior1,
                                 useC = TRUE)
        ## prior1 <- l[[2]]
        ## ans[,i] <- prior1@alphaMix@.Data
        ans[1,i] <- prior1@meanLevelComponentWeightMix@.Data
        ans[2,i] <- prior1@phiMix
        ans[3,i] <- prior1@omegaLevelComponentWeightMix@.Data
        ans[4,i] <- prior1@omegaComponentWeightMix@.Data
        ans[5,i] <- prior1@omegaVectorsMix@.Data
        ans[6,i] <- prior1@indexClassMix[1]
        ans[7,i] <- prior1@indexClassMaxUsedMix@.Data
        ans[8,i] <- prior1@foundIndexClassMaxPossibleMix@.Data
    }

    ## updatePrior
    n.update <- 1000
    ans <- matrix(nr = 8, ncol = n.update)
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateTauNorm(prior = prior1,
                                beta = beta.tilde,
                                useC = TRUE) # sigma-delta
        ## vectors
        prior1 <- updateVectorsMixAndProdVectorsMix(prior = prior1,
                                                    betaTilde = beta.tilde,
                                                    useC = TRUE) # psi
        prior1 <- updateLatentComponentWeightMix(prior1,
                                                 useC = TRUE) # z
        prior1 <- updateComponentWeightMix(prior1,
                                           useC = TRUE) # W
        prior1 <- updateWeightMix(prior1,
                                  useC = TRUE) # v; deterministic
        prior1 <- updateLatentWeightMix(prior1,
                                        useC = TRUE) # u
        prior1 <- updateIndexClassMaxPossibleMix(prior1,
                                                 useC = TRUE) # k-tilde
        prior1 <- updateIndexClassMix(prior = prior1,
                                      betaTilde = beta.tilde,
                                      useC = TRUE) # k
        prior1 <- updateIndexClassMaxUsedMix(prior1,
                                             useC = TRUE) # k-star; deterministic
        prior1 <- updateLevelComponentWeightMix(prior1,
                                                useC = TRUE) # alpha
        prior1 <- updateMeanLevelComponentWeightMix(prior1,
                                                    useC = TRUE) # mu
        prior1 <- updatePhiMix(prior1,
                               useC = TRUE) # phi
        prior1 <- updateOmegaVectorsMix(prior1,
                                        useC = TRUE) # sigma-e
        prior1 <- updateOmegaComponentWeightMix(prior1,
                                                useC = TRUE) # sigma-epsilon
        prior1 <- updateOmegaLevelComponentWeightMix(prior1,
                                                     useC = TRUE) # sigma-eta 
        prior1 <- updateAlphaMix(prior1,
                                 useC = TRUE)
        ans[1,i] <- prior1@meanLevelComponentWeightMix@.Data
        ans[2,i] <- prior1@phiMix
        ans[3,i] <- prior1@omegaLevelComponentWeightMix@.Data
        ans[4,i] <- prior1@omegaComponentWeightMix@.Data
        ans[5,i] <- prior1@omegaVectorsMix@.Data
        ans[6,i] <- prior1@indexClassMix[1]
        ans[7,i] <- prior1@indexClassMaxUsedMix@.Data
        ans[8,i] <- prior1@foundIndexClassMaxPossibleMix@.Data
    }
    quartz()
    plot(ans[2,], type = "l")

    
    plot(ans[1,], type = "l")    
    plot(ans[3,], type = "l")
    plot(ans[4,], type = "l")
    plot(ans[5,], type = "l")
    plot(ans[6,], type = "l")
    plot(ans[7,], type = "l")
    table(ans[8,4000:5000])





    ans.all <- matrix(ans.all, nr = 20,
                      dimnames = list(cell = 1:20, comp = seq_len(index.class.max)))
    ans.all <- Values(ans.all)
    
    dplot(~ comp | cell,
          ans.all,
          midpoints = T)

    

    plot(prior@prodVectorsMix@.Data[1:20], ans.all[1:20], asp = 1)
    abline(a = 0, b = 1)




    ## updateComponentWeightMix
    n.update <- 10000
    ans <- matrix(nrow = n.time * index.class.max, ncol = n.update)
    updateComponentWeightMix <- demest:::updateComponentWeightMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateComponentWeightMix(prior1,
                                           useC = TRUE)
        ans[,i] <- prior1@componentWeightMix@.Data
    }
    ans.all <- rowMeans(ans)
    plot(prior@componentWeightMix@.Data, ans.all)

    ## updateWeightMix
    updateWeightMix <- demest:::updateWeightMix
    prior1 <- prior
    prior1 <- updateWeightMix(prior1,
                              useC = TRUE)
    ans <- prior1@weightMix@.Data
    plot(prior@weightMix@.Data, ans)

    ## updateLatentWeightMix
    n.update <- 1000
    ans <- matrix(nrow = J, ncol = n.update)
    updateLatentWeightMix <- demest:::updateLatentWeightMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateLatentWeightMix(prior1,
                                        useC = TRUE)
        ans[,i] <- prior1@latentWeightMix@.Data
    }
    ans.all <- rowMeans(ans)
    plot(prior@latentWeightMix@.Data, ans.all)

    ## updateOmegaComponentWeightMix
    n.update <- 1000
    ans <- numeric(length = n.update)
    updateOmegaComponentWeightMix <- demest:::updateOmegaComponentWeightMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateOmegaComponentWeightMix(prior1,
                                                useC = TRUE)
        ans[i] <- prior1@omegaComponentWeightMix@.Data
    }
    summary(ans)
    prior@omegaComponentWeightMix@.Data

    ## updateOmegaLevelComponentWeightMix
    n.update <- 1000
    ans <- numeric(length = n.update)
    updateOmegaLevelComponentWeightMix <- demest:::updateOmegaLevelComponentWeightMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateOmegaLevelComponentWeightMix(prior1,
                                                     useC = TRUE)
        ans[i] <- prior1@omegaLevelComponentWeightMix@.Data
    }
    summary(ans)
    prior@omegaLevelComponentWeightMix@.Data



    ## updateIndexClassMaxPossibleMix
    updateIndexClassMaxPossibleMix <- demest:::updateIndexClassMaxPossibleMix
    prior1 <- prior
    prior1 <- updateIndexClassMaxPossibleMix(prior1)
    ans <- prior1@indexClassMaxPossibleMix@.Data
    c(ans, prior@indexClassMaxPossibleMix@.Data)
    

    ## updateIndexClassMix
    n.update <- 100
    ans <- matrix(nrow = J, ncol = n.update)
    updateIndexClassMix <- demest:::updateIndexClassMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateIndexClassMix(prior1,
                                      betaTilde = beta.tilde,
                                      useC = TRUE)
        ans[,i] <- prior1@indexClassMix@.Data
    }
    ans.all <- rowMeans(ans)
    plot(prior@indexClassMix@.Data, ans.all)



    ## updateIndexClassMaxUsedMix
    updateIndexClassMaxUsedMix <- demest:::updateIndexClassMaxUsedMix
    prior1 <- prior
    prior1 <- updateIndexClassMaxUsedMix(prior1)
    ans <- prior1@indexClassMaxUsedMix@.Data
    c(ans, prior@indexClassMaxUsedMix@.Data)


    ## updateLevelComponentWeightMix
    n.update <- 1000
    ans <- matrix(nrow = n.time * index.class.max, ncol = n.update)
    updateLevelComponentWeightMix <- demest:::updateLevelComponentWeightMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateLevelComponentWeightMix(prior1,
                                                useC = TRUE)
        ans[,i] <- prior1@levelComponentWeightMix@.Data
    }
    ans.all <- rowMeans(ans)
    plot(prior@levelComponentWeightMix@.Data, ans.all)
    abline(a = 0, b = 1)
    matrix(round((ans.all - prior@levelComponentWeightMix@.Data), 2), 10)

    ## updateMeanLevelComponentWeightMix
    n.update <- 100
    ans <- numeric(length = n.update)
    updateMeanLevelComponentWeightMix <- demest:::updateMeanLevelComponentWeightMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateMeanLevelComponentWeightMix(prior1,
                                                    useC = TRUE)
        ans[i] <- prior1@meanLevelComponentWeightMix@.Data
    }
    summary(ans)
    prior@meanLevelComponentWeightMix@.Data


    ## updatePhiMix
    n.update <- 1000
    ans <- numeric(length = n.update)
    updatePhiMix <- demest:::updatePhiMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updatePhiMix(prior1,
                               useC = TRUE)
        ans[i] <- prior1@phiMix
    }
    summary(ans)
    prior@phiMix


    ## updateAlphaMix
    n.update <- 1000
    ans <- matrix(nrow = J, ncol = n.update)
    updateAlphaMix <- demest:::updateAlphaMix
    prior1 <- prior
    for (i in seq_len(n.update)) {
        prior1 <- updateAlphaMix(prior1,
                                 useC = TRUE)
        ans[,i] <- prior@alphaMix@.Data
    }
    ans.all <- rowMeans(ans)
    plot(prior@alphaMix@.Data, ans.all)


}


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
