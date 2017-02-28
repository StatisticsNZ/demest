###############################
##The meaning of notations are listed below.
##index_w: the variable used for weighting. Here, index_W is Tindex, i.e, we use time for weighting
##index1: the first effect associated for each area. Here, this effect is age.
##index1: the second effect associated for each area. Here, this effect is sex.
##L_w,L1,L2: the number of unique values in index_w, index1 and index2
##N: the number of areas.
##R: log(mortality rate)
##U: the latent variables used in the slice sampler.
##K: the specific latent class for each sample.
##tutaK: the pre-defined maximum value of latent classes, i.e. 10
##Kstar: the real maximum value of K, i.e. max(K)
##Alist: the sets of possible latent classes for each sample.
##Psi1: the normally distributed variable for the first effect.
##Psi2: the normally distributed variable for the second effect.
##V: the weights for each latent class
##W: the variables used in probit transformation. We assume a state space model for W.
##Z: the variables introduced for data augmentation to improve efficiency. it's a normal latent variable with mean W and variance 1.
##sigma2Eff1: the variance for the first effect
##sigma2Eff2: the variance for the second effect
##sigma2DEL: the variance for delta, i.e. error terms in R=Psi1*Psi2+delta
##sigma2EPS: the variance for epsilon, i.e. error terms in state space model W=alpha+epsilon
##sigma2ETA: the variance for eta, i.e. error terms in AR(1) alpha_{t}=mu+phi*alpha_{t-1}+eta
##mu,phi: the variables used in AR(1)
##mDEL,qDEL: hyperparameters for sigma2DEL.
##mEPS,qEPS: hyperparameters for sigma2EPS.
##mETA,qETA: hyperparameters for sigma2ETA.
##mEff1,qEff1: hyperparameters for sigma2Eff1.
##mEff2,qEff2: hyperparameters for sigma2Eff2.
##mu0,sigma20: hyperparameters for mu.

#################################
##functions for updating step by step. 
##sample phi using Newton raphson and Metroplis Hasting
source(file.path(dirc,"compute_phimax_using_NR.R"))
##update Alist with the newest U
source(file.path(dirc,"get_Alist_using_U.R"))
##update the weights V using the newest W
source(file.path(dirc,"get_V_using_W.R"))
##kalman fiter
source(file.path(dirc,"kalman_filter.R"))
##the log full conditional distribution of phi
source(file.path(dirc,"phi_log_posterior.R"))
##the first-order derivative of log posterior of phi
source(file.path(dirc,"phi_log_posterior_first_order.R"))
##the second-order derivative of log posterior of phi
source(file.path(dirc,"phi_log_posterior_second_order.R"))
##sample K, the specific latent class for each area. 
source(file.path(dirc,"sample_K_from_multi.R"))
##sample mu in AR(1)
source(file.path(dirc,"sample_mu_from_norm.R"))
##sample phi using Metroplis Hasting
source(file.path(dirc,"sample_phi_using_MH.R"))
##sample Psi for each effect
source(file.path(dirc,"sample_Psi_from_norm.R"))
##sample sigma2DEL from inverse gamma
source(file.path(dirc,"sample_sigma2DEL_from_invgam.R"))
##update sigma2Eff from inverse gamma
source(file.path(dirc,"sample_sigma2Eff_from_invgam.R"))
##update sigma2EPS from inverse gamma
source(file.path(dirc,"sample_sigma2EPS_from_invgam.R"))
##update sigma2ETA from inverse gamma
source(file.path(dirc,"sample_sigma2ETA_from_invgam.R"))
##Sample latent variables U for the slice sampler
source(file.path(dirc,"sample_U_from_unif.R"))
##sample W in the state space model
source(file.path(dirc,"sample_W_from_norm.R"))
##sample latent variables Z from truncated normals
source(file.path(dirc,"sample_Z_from_truncnorm.R"))
##update alpha using kalman filter and the forward filtering backward sampling algorithm
source(file.path(dirc,"sample_alpha_using_forback.R"))
##sample from truncated normal distributions
source(file.path(dirc,"trandn.R"))
##useful functions used in trandn.R
source(file.path(dirc,"ntail.R"))
source(file.path(dirc,"tn.R"))
source(file.path(dirc,"trnd.R"))
##sample from inverse gamma distributions
source(file.path(dirc,"igamma.R"))