#generate data to check the code
rm(list=ls(all=TRUE))
setwd("D:/my workfiles/project/bmf")
dir <- file.path(getwd(),"bmf")  #the directory where the bmf project is
dirc <- file.path(dir,"src/R") #the directory where the codes are
dirs  <-  file.path(dir,"results")
data <- read.csv(file.path(dir,"/data/newdata.csv"),header=T) #the real data


#we use Tindex,Aindex and Sindex in the real data
Tindex=data[,1]
Aindex=data[,2]
Sindex=data[,3]
#R=data[,7]
N=length(Tindex)
T=length(unique(Tindex))
A=length(unique(Aindex))
S=length(unique(Sindex))
source(file.path(dir,"src/useful functions_v3.R"))

#################
#do not update sigma2's
sigma2ETA=sigma2EPS=sigma2DEL=sigma2A=sigma2S=1
mu0=0
sigma20=1
phi_true=0.5
mu_true=0.5
tutaK=10
alpha_true=matrix(0,nrow=T,ncol=tutaK)
alpha_true[1,]=rnorm(tutaK,mu_true/(1-phi_true),sd=sqrt(1/(1-phi_true^2)))
for(t in 2:T)
{
 alpha_true[t,]=rnorm(tutaK,mu_true+phi_true*alpha_true[t-1,],1)
}
W_true=V_true=matrix(0,nrow=T,ncol=tutaK)
for(t in 1:T)
{
 W_true[t,]=rnorm(tutaK,alpha_true[t,],1)
}
V_true[,1]=pnorm(W_true[,1])
pond=1-pnorm(W_true[,1])
for(j in 2:tutaK)
{
  V_true[,j]=pnorm(W_true[,j])*pond
  pond=pond*(1-pnorm(W_true[,j]))
}
PsiA_true=matrix(rnorm(A*tutaK,0,sd=1),nrow=A,ncol=tutaK)
PsiS_true=matrix(rnorm(S*tutaK,0,sd=1),nrow=S,ncol=tutaK)

U_true <- runif(N,0,0.3)
#sample(seq(1,5),N,replace=T)
Alist <- get_Alist_using_U(Tindex,U_true,V_true)
length(which(unlist(lapply(Alist,length))==0))
K <- rep(0,N)
for(i in 1:N)
{
  thetrue <- Alist[[i]]
  K[i] <- sample(thetrue,1)
}

R=rep(0,N)
for(i in 1:N)
{
 thet=Tindex[i]
 thea=Aindex[i]
 thes=Sindex[i]
 thek=K[i]
 themean=PsiA_true[thea,thek]*PsiS_true[thes,thek]
 R[i]=rnorm(1,themean,sd=1)
}

write.csv(R,"D:/my workfiles/project/bmf/try/R_true.csv",row.names=F)
write.csv(K,"D:/my workfiles/project/bmf/try/K_true.csv",row.names=F)
write.csv(U_true,"D:/my workfiles/project/bmf/try/U_true.csv",row.names=F)
write.csv(PsiA_true,"D:/my workfiles/project/bmf/try/PsiA_true.csv",row.names=F)
write.csv(PsiS_true,"D:/my workfiles/project/bmf/try/PsiS_true.csv",row.names=F)
write.csv(W_true,"D:/my workfiles/project/bmf/try/W_true.csv",row.names=F)
write.csv(alpha_true,"D:/my workfiles/project/bmf/try/alpha_true.csv",row.names=F)