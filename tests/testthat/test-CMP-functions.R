

test_that("dqcmp1 works", {


    q.pdf<- function(y,gamma,nu, log=FALSE){
  if(log==FALSE){
  fact <- factorial(y)
  (gamma^y/fact)^nu
    } else {
    nu*(y*log(gamma)-(sum(log(1:y))))
  }
}


})
