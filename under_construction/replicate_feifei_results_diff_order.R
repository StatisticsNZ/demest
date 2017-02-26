

setwd("/Users/johnbryant/Documents/demest/under_construction")
library(demest)
initialPrior <- demest:::initialPrior

setwd("try")
K_true <- read.csv("K_true.csv")$x
PsiA_true <- unlist(read.csv("PsiA_true.csv"))
PsiS_true <- unlist(read.csv("PsiS_true.csv"))
R_true <- read.csv("R_true.csv")$x
U_true <- read.csv("U_true.csv")$x
W_true <- unlist(read.csv("W_true.csv"))
alpha_true <- unlist(read.csv("alpha_true.csv"))


K_true <- array(K_true, dim = c(20, 24, 2))
K_true <- aperm(K_true, perm = c(1, 3, 2))
K_true <- as.integer(K_true)

R_true <- array(R_true, dim = c(20, 24, 2))
R_true <- aperm(R_true, perm = c(1, 3, 2))
R_true <- as.numeric(R_true)

U_true <- array(U_true, dim = c(20, 24, 2))
U_true <- aperm(U_true, perm = c(1, 3, 2))
U_true <- as.numeric(U_true)


metadata <- new("MetaData",
                nms = c("time", "sex", "age"),
                dimtypes = c("time", "sex", "age"),
                DimScales = list(new("Intervals", dimvalues = 0:20),
                                 new("Sexes", dimvalues = c("Female", "Male")),
                                 new("Intervals", dimvalues = 0:24)))
beta.tilde <- R_true
spec <- Mix()
prior <- initialPrior(spec,
                      beta = beta.tilde,
                      metadata = metadata,
                      sY = NULL,
                      multScale = 1)
prior@componentWeightMix@.Data <- W_true
prior@indexClassMix <- K_true
prior@indexClassMaxUsedMix@.Data <- max(K_true)
prior@latentWeightMix@.Data <- U_true
prior@levelComponentWeightMix@.Data <- alpha_true
prior@meanLevelComponentWeightMix@.Data <- 0.5
prior@omegaComponentWeightMix@.Data <- 1
prior@omegaLevelComponentWeightMix@.Data <- 1
prior@omegaVectorsMix@.Data <- 1
prior@phiMix <- 0.5
prior@priorMeanLevelComponentWeightMix@.Data <- 0
prior@priorSDLevelComponentWeightMix@.Data <- 1
prior@vectorsMix[[2]]@.Data <- PsiS_true
prior@vectorsMix[[3]]@.Data <- PsiA_true
vec.sex <- matrix(prior@vectorsMix[[2]], nc = 10)
vec.age <- matrix(prior@vectorsMix[[3]], nc = 10)
prod.vec <- lapply(1:10, function(i) outer(vec.sex[,i], vec.age[,i]))
prod.vec <- unlist(prod.vec)
prior@prodVectorsMix@.Data <- prod.vec
prior@tau@.Data <- 1
stopifnot(validObject(prior))

set.seed(1)
n.update <- 1
updateVectorsMixAndProdVectorsMix <- demest:::updateVectorsMixAndProdVectorsMix
ans.S <- matrix(nrow = 2 * 10, ncol = n.update)
ans.A <- matrix(nrow = 24 * 10, ncol = n.update)
for (i in 1:n.update) {
    prior1 <- updateVectorsMixAndProdVectorsMix(prior,
                                                betaTilde = beta.tilde,
                                                useC = TRUE)
    ans.S[,i] <- prior1@vectorsMix[[2]]@.Data
    ans.A[,i] <- prior1@vectorsMix[[3]]@.Data
}
ans.A.all <- rowMeans(ans.A)
ans.S.all <- rowMeans(ans.S)
plot(PsiS_true[1:8], ans.S.all[1:8])
plot(PsiA_true[1:96], ans.A.all[1:96])
cor(PsiS_true[1:8], ans.S.all[1:8])
cor(PsiA_true[1:96], ans.A.all[1:96])





## updatePrior
n.update <- 1000
ans <- matrix(nr = 8, ncol = n.update)
## prior1 <- prior
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


