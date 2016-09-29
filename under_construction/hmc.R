

HMC = function (U, grad_U, epsilon, L, current_q)
{
  q = current_q
  p = rnorm(length(q),0,1)  # independent standard normal variates
  current_p = p

  # Make a half step for momentum at the beginning

  p = p - epsilon * grad_U(q) / 2

  # Alternate full steps for position and momentum

  for (i in 1:L)
  {
    # Make a full step for the position

    q = q + epsilon * p

    # Make a full step for the momentum, except at end of trajectory

    if (i!=L) p = p - epsilon * grad_U(q)
  }

  # Make a half step for momentum at the end.

  p = p - epsilon * grad_U(q) / 2

  # Negate momentum at end of trajectory to make the proposal symmetric

  p = -p

  # Evaluate potential and kinetic energies at start and end of trajectory

  current_U = U(current_q)
  current_K = sum(current_p^2) / 2
  proposed_U = U(q)
  proposed_K = sum(p^2) / 2

  # Accept or reject the state at end of trajectory, returning either
  # the position at the end of the trajectory or the initial position

  if (runif(1) < exp(current_U-proposed_U+current_K-proposed_K))
  {
    return (q)  # accept
  }
  else
  {
    return (current_q)  # reject
  }
p}

## U(q) = q^2 / 2; grad_U(q) = q
hmc <- function(q.curr, L = 20, epsilon = 0.05) {
    q.prop <- q.curr
    p.curr <- rnorm(1)
    ## Make a half step for momentum at the beginning
    p.prop <- p.curr - epsilon * q.curr / 2
    ## Alternate full steps for position and momentum
    for (i in seq_len(L)) {
        ## Make a full step for the position
        q.prop <- q.prop + epsilon * p.prop
        ## Make a full step for the momentum, except at end of trajectory
        if (i < L)
            p.prop <- p.prop - epsilon * q.prop
    }
    ## Make a half step for momentum at the end
    p.prop <- p.prop - epsilon * q.prop / 2
    ## Negate momentum at end of trajectory to make the proposal symmetric
    p.prop <- -p.prop
    ## Evaluate potential and kinetic energies at start and end of trajectory
    U.curr = q.curr^2 / 2
    K.curr = p.curr^2 / 2
    U.prop <- q.prop^2 / 2
    K.prop = p.prop^2 / 2
    ## Accept or reject the state at end of trajectory, returning either
    ## the position at the end of the trajectory or the initial position
    diff <- U.curr - U.prop + K.curr - K.prop
    accept <- (diff > 0) || (runif(1) < exp(diff))
    if (accept)
        q.prop
    else
        q.curr
}
    
    
ans <- numeric(1000)
q <- rnorm(1)
for (i in seq_along(ans)) {
    q <- hmc(q)
    ans[i] <- q
}
plot(ans, type = "l")

    
