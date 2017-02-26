
library(demest)

iAlong<-1
dim.beta<-c(2, 3, 2)  # two time periods, three age groups, two sexes
pos1<-2
pos2<-1

set.seed(1)
index.class<-sample(1:2,2*3*2,replace=TRUE) #k_{tas}
index.class.max.used<-max(index.class) #k-star
n.beta.no.along<-3*2 # three (age groups) * two (sexes)

#True values of psi's.  k-programmer=3
vectors.true<-list(NULL,c(-2,1,3,1,-1,2,2,1,-2),c(-1,1.5,1,-0.8,1.2,-1.7))
#Outer product of psi_A's and psi_S's
prod.vectors.true<-rep(3*2*3)
i.prod<-1
for (i.class in 1:3)
  for (i.sex in 1:2)
    for (i.age in 1:3)
    {
      prod.vectors.true[i.prod]<-vectors.true[[2]][(i.class-1)*3+i.age]*
        vectors.true[[3]][(i.class-1)*2+i.sex]
      i.prod<-i.prod+1
    }

#variance of psi
omega.vectors<-1.2
#variance of error for each tas
v<-rep(1,2*3*2)

betaTilde<-rep(0,2*3*2) #beta_{tas}
for (i.sex in 1:2)
  for (i.age in 1:3)
    for (i.time in 1:2)
    {
      index<-(i.sex-1)*6+(i.age-1)*2+i.time
      i.class<-index.class[index]
      betaTilde[index]<-rnorm(1,vectors.true[[2]][(i.class-1)*3+i.age]*
        vectors.true[[3]][(i.class-1)*2+i.sex],v[index])
    }

prec.prior <- 1 / omega.vectors^2

metadata <- new("MetaData",
                nms = c("time", "age", "sex"),
                dimtypes = c("time", "age", "sex"),
                DimScales = list(new("Intervals", dimvalues = 0:2),
                                 new("Intervals", dimvalues = 0:3),
                                 new("Sexes", dimvalues = c("Female", "Male"))))
initialPrior <- demest:::initialPrior
spec <- Mix(maxClass = 3)
prior <- initialPrior(spec,
                      beta = betaTilde,
                      metadata = metadata,
                      sY = NULL,
                      multScale = 1)

prior@vectorsMix[[2]]@.Data <- vectors.true[[2]]
prior@vectorsMix[[3]]@.Data <- vectors.true[[3]]
prior@prodVectorsMix@.Data <- prod.vectors.true
prior@omegaVectorsMix@.Data <- omega.vectors
prior@tau@.Data <- 1
prior@indexClassMix <- index.class
prior@indexClassMaxUsedMix@.Data <- index.class.max.used

n.samp<-100
ans.age <- matrix(nr=9,nc=n.samp)
ans.sex <- matrix(nr=6, nc = n.samp)
ans.prod <- matrix(nr = 18, nc = n.samp)
for (i.samp in 1:n.samp) {
    prior1 <- demest:::updateVectorsMixAndProdVectorsMix(prior,
                                                         betaTilde = betaTilde)
    ans.age[,i.samp] <- prior1@vectorsMix[[2]]@.Data
    ans.sex[,i.samp] <- prior1@vectorsMix[[3]]@.Data
    ans.prod[,i.samp] <- prior1@prodVectorsMix@.Data
}
mean.vector.age <- rowMeans(ans.age)
mean.vector.sex <- rowMeans(ans.sex)
mean.prod <- rowMeans(ans.prod)

par(mfrow=c(2,2))
plot(vectors.true[[2]],mean.vector.age)
plot(vectors.true[[3]],mean.vector.sex)
plot(prod.vectors.true,mean.prod)

cor(vectors.true[[2]],mean.vector.age)
cor(vectors.true[[3]],mean.vector.sex)
cor(prod.vectors.true,mean.prod)
