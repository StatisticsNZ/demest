

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

metadata <- new("MetaData",
                nms = c("time", "age", "sex"),
                dimtypes = c("time", "age", "sex"),
                DimScales = list(new("Intervals", dimvalues = 0:20),
                                 new("Intervals", dimvalues = 0:24),
                                 new("Sexes", dimvalues = c("Female", "Male"))))
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
prior@vectorsMix[[2]]@.Data <- PsiA_true
prior@vectorsMix[[3]]@.Data <- PsiS_true
vec.age <- matrix(prior@vectorsMix[[2]], nc = 10)
vec.sex <- matrix(prior@vectorsMix[[3]], nc = 10)
prod.vec <- lapply(1:10, function(i) outer(vec.age[,i], vec.sex[,i]))
prod.vec <- unlist(prod.vec)
prior@prodVectorsMix@.Data <- prod.vec
prior@tau@.Data <- 1
stopifnot(validObject(prior))

updateVectorsMixAndProdVectorsMix <- demest:::updateVectorsMixAndProdVectorsMix

n.update <- 100
ans.A <- matrix(nrow = 24 * 10, ncol = n.update)
ans.S <- matrix(nrow = 2 * 10, ncol = n.update)
ans.prod <- matrix(nrow = 48 * 10, ncol = n.update)
for (i in 1:n.update) {
    prior1 <- updateVectorsMixAndProdVectorsMix(prior,
                                                betaTilde = beta.tilde,
                                                useC = FALSE)
    ans.A[,i] <- prior1@vectorsMix[[2]]@.Data
    ans.S[,i] <- prior1@vectorsMix[[3]]@.Data
    ans.prod[,i] <- prior1@prodVectorsMix@.Data
}
ans.A.all <- rowMeans(ans.A)
ans.S.all <- rowMeans(ans.S)
ans.prod.all <- rowMeans(ans.prod)
plot(PsiA_true[1:96], ans.A.all[1:96])
plot(PsiA_true, ans.A.all)
plot(PsiS_true[1:8], ans.S.all[1:8])
plot(PsiS_true, ans.S.all)
cor(PsiA_true[1:96], ans.A.all[1:96])
cor(PsiS_true[1:8], ans.S.all[1:8])



vec.age.1 <- matrix(prior1@vectorsMix[[2]], nc = 10)
vec.sex.1 <- matrix(prior1@vectorsMix[[3]], nc = 10)
prod.vec.1 <- lapply(1:10, function(i) outer(vec.age.1[,i], vec.sex.1[,i]))
prod.vec.1 <- unlist(prod.vec.1)

plot(prior@prodVectorsMix@.Data, prod.vec.1)

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


