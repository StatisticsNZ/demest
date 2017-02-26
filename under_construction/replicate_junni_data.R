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


resetS<-function(iterator.beta,i)
{
  if (i==2)
    iterator.beta<-c(1:2,7:8)
  if (i==3)
    iterator.beta<-c(1:6)
  iterator.beta
}

advanceS<-function(iterator.beta,i)
{
  if (i==2)
  {
    if (identical(iterator.beta,c(1:2,7:8)))
      iterator.beta<-c(3:4,9:10)
    else if (identical(iterator.beta,c(3:4,9:10)))
      iterator.beta<-c(5:6,11:12)
    else if (identical(iterator.beta,c(5:6,11:12)))
      iterator.beta<-c(1:2,7:8)
  }
  if (i==3)
  {
    if (identical(iterator.beta,c(1:6)))
      iterator.beta<-c(7:12)
    else if (identical(iterator.beta,c(7:12)))
      iterator.beta<-c(1:6)
  }
  iterator.beta
}

resetM<-function(iterator.prod)
{
  iterator.prod<-c(1,1)
  iterator.prod
}  

advanceM<-function(iterator.prod)
{
  if (identical(iterator.prod,c(1,1)))
    iterator.prod<-c(2,1)
  else if (identical(iterator.prod,c(2,1)))
    iterator.prod<-c(3,1)
  else if (identical(iterator.prod,c(3,1)))
    iterator.prod<-c(1,2)
  else if (identical(iterator.prod,c(1,2)))
    iterator.prod<-c(2,2)
  else if (identical(iterator.prod,c(2,2)))
    iterator.prod<-c(3,2)
  else if (identical(iterator.prod,c(3,2)))
    iterator.prod<-c(1,1)
  
  iterator.prod
}  

## TRANSLATED
## HAS_TESTS
## 'vectors' are 'psi' in notes
updateVectorsMixAndProdVectorsMix <- function(n.iter) {
for (i.iter in 1:n.iter)
{
        yX <- rep(0,3)  #a vector of dimension k-programmer
        XX <- rep(0,3)  #a vector of dimension k-programmer
        ## loop through vectors, skipping position occupied by "along" dimension
        for (i in seq_along(vectors)) {
            if (i == iAlong)
                next
            vector <- vectors[[i]]
            n.element.vector <- dim.beta[i]
            if (i==2)
                iterator.beta <- rep(0,2*2)
            else iterator.beta <- rep(0,2*3)
            iterator.beta <- resetS(iterator.beta,i)
            ## update i'th vector
            for (i.element in seq_len(n.element.vector)) {
                ## reset vectors holding statistics for updating
                for (i.class in seq_len(index.class.max.used)) {
                    yX[i.class] <- 0
                    XX[i.class] <- 0
                }
                ## Loop along selected elements of 'betaTilde', 'v', and 'index.class',
                ## plus associated elements from prod.vector, to calculate
                ## statistics needed for updating i.element'th slice of i'th vector.
                indices.beta <- iterator.beta
                for (i.beta in indices.beta) {
                    beta.tilde.i.beta <- betaTilde[i.beta]
                    v.i.beta <- v[i.beta]
                    i.class <- index.class[i.beta]
                    i.vector <- (i.class - 1L) * n.element.vector + i.element
                    val.vector <- vector[i.vector] 
                    i.beta.no.along <- ((i.beta - 1L) %/% pos1) * pos2 + (i.beta - 1L) %% pos2 + 1L
                    i.prod <- (i.class - 1L) * n.beta.no.along + i.beta.no.along
                    val.prod.vector <- prod.vectors[i.prod]
                    X <- val.prod.vector / val.vector  ## val.vector could be 0?
                    yX[i.class] <- yX[i.class] + beta.tilde.i.beta * X  / v.i.beta
                    XX[i.class] <- XX[i.class] + X * X / v.i.beta
                }
                iterator.beta <- advanceS(iterator.beta,i)
                ## Update this slice.
                for (i.class in seq_len(index.class.max.used)) {
                    prec.data <- XX[i.class]
                    used.this.class <- prec.data > 0 ## This test is new.  Now only ************************** here *************************************
                    if (used.this.class) {           ## update where we have data
                        i.vector <- (i.class - 1L) * n.element.vector + i.element
                        var <- 1 / (prec.data + prec.prior)
                        sd <- sqrt(var)
                        mean <- var * yX[i.class]
                        vector[i.vector] <- rnorm(n = 1L,
                                                  mean = mean,
                                                  sd = sd)
                    }                               ## new
                }
            }
            vectors[[i]] <- vector
            ## Update 'prod.vectors'. We calculate 'prod.vectors' from scratch,
            ## rather than by scaling up or down in proportion to the change
            ## in vector i, to avoid the possibility of small numerical
            ## inconsistencies creeping in.
            for (i.class in seq_len(index.class.max.used)) {
                iterator.prod<-rep(0,2)
                iterator.prod <- resetM(iterator.prod)
                for (i.beta.no.along in seq_len(n.beta.no.along)) {
                    indices.vectors <- iterator.prod
                    i.prod.vectors <- (i.class - 1L) * n.beta.no.along + i.beta.no.along
                    prod.values <- 1
                    for (i.inner in seq_along(vectors)) {
                        if (i.inner == iAlong)
                            next
                        vector <- vectors[[i.inner]]
                        n.element <- dim.beta[i.inner]
                        i.element <- indices.vectors[i.inner-1]
                        i.vector <- (i.class - 1L) * n.element + i.element
                        value <- vector[i.vector]
                        prod.values <- prod.values * value
                    }
                    prod.vectors[i.prod.vectors] <- prod.values
                    iterator.prod <- advanceM(iterator.prod)
                }
            }
        }
}
  vectors<<-vectors
  prod.vectors<<-prod.vectors
}


vectors <- vectors.true
prod.vectors <- prod.vectors.true

n.samp<-1000
mean.vector.age<-rep(0,3*3)
mean.vector.sex<-rep(0,3*2)
mean.prod<-rep(0,3*3*2)
for (i.samp in 1:n.samp)
{
  updateVectorsMixAndProdVectorsMix(1)
  mean.vector.age<-mean.vector.age+vectors[[2]]
  mean.vector.sex<-mean.vector.sex+vectors[[3]]
  mean.prod<-mean.prod+prod.vectors
}
mean.vector.age<-mean.vector.age/n.samp
mean.vector.sex<-mean.vector.sex/n.samp
mean.prod<-mean.prod/n.samp

par(mfrow=c(2,2))
plot(vectors.true[[2]],mean.vector.age)
plot(vectors.true[[3]],mean.vector.sex)
plot(prod.vectors.true,mean.prod)

cor(vectors.true[[2]],mean.vector.age)
cor(vectors.true[[3]],mean.vector.sex)
cor(prod.vectors.true,mean.prod)
