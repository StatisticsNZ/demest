
Move(classes = diag.matrix)

Move(classes = diag.matrix,
     covariates = Covariates(mean ~ orig, data = mydata),
     error = Error(robust = TRUE))

Move(classes = prog.matrix)




y ~ BinomialFixed(prob = xxx, sd = xxx)
