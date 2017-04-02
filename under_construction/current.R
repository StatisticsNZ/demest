> print(s)
-------------------------------------------------- 
model:
y ~ Poisson(mean ~ age * sex + sex * year),
age ~ DLM(covariates = Covariates(mean ~ is.infant, data = is.infant.df), 
    damp = NULL),
year ~ DLM(damp = NULL, level = Level(scale = HalfT(scale = 0.2)), 
    trend = Trend(scale = HalfT(scale = 0.2)), error = Error(scale = HalfT(scale = 0.2))),
sex:year ~ DLM(),
0.08
dimensions: age, sex, year
-------------------------------------------------- 
y:
Object of class "Counts"
dimensions: age, sex, year
n cells: 1920,  n missing: 0, integers: TRUE, n zeros: 0, median: 589
-------------------------------------------------- 
MCMC statistics:
nBurnin: 1000,  nSim: 1000,  nChain: 4,  nThin: 40,  nIteration: 100 

Metropolis-Hastings updates:
                      jump acceptance autocorr
model.likelihood.rate 0.08     0.4648   0.1426

parameters:
                                    Rhat    2.5%     50%   97.5% length
model.likelihood.rate              1.046  0.0001  0.0028  0.2592   1920
model.prior.(Intercept)            1.093 -5.6427 -5.6365 -5.6305      1
model.prior.age                    1.094 -3.1697 -0.1756  4.2894     20
model.prior.sex                    0.998 -0.2576  0.0000  0.2576      2
model.prior.year                .  1.122 -0.4347  0.0111  0.3972     48
model.prior.age:sex                1.067 -0.2094  0.0000  0.2094     40
model.prior.sex:year            .  1.139 -0.0242  0.0000  0.0242     96
model.prior.sd                     1.029  0.1134  0.1161  0.1184      1
model.hyper.age.level           . 11.934 -3.8588 -0.0447  3.8817     20
model.hyper.age.scaleLevel      .  4.469  0.0009  0.0250  0.8109      1
model.hyper.age.trend           .  2.052 -0.0264  0.4079  0.4342     20
model.hyper.age.scaleTrend      .  1.701  0.0005  0.0032  0.0255      1
model.hyper.age.coef            .  7.541 -0.1294  4.1514  4.7600      1
model.hyper.age.scaleError      .  3.532  0.0000  0.3819  0.6268      1
model.hyper.year.level          .  1.171 -0.4419  0.0009  0.4420     48
model.hyper.year.scaleLevel     .  1.524  0.0000  0.0001  0.0006      1
model.hyper.year.trend          .  1.148 -0.0200 -0.0194 -0.0189     48
model.hyper.year.scaleTrend     .  1.342  0.0000  0.0000  0.0000      1
model.hyper.year.scaleError     .  1.413  0.0006  0.0066  0.0118      1
model.hyper.age:sex.scaleError  .  7.521  0.1145  0.1547  1.0713      1
model.hyper.sex:year.level      .  1.267 -0.0087  0.0000  0.0087     96
model.hyper.sex:year.scaleLevel .  1.417  0.0000  0.0002  0.0007      1
model.hyper.sex:year.trend      .  2.288  0.0000  0.0001  0.0084     96
model.hyper.sex:year.scaleTrend .  1.832  0.0000  0.0000  0.0000      1
model.hyper.sex:year.damp       . 58.196  0.8161  0.8358  0.8795      1
model.hyper.sex:year.scaleError .  1.168  0.0063  0.0163  0.0218      1
-------------------------------------------------- 
> 
+ . + 
-------------------------------------------------- 
model:
y ~ Poisson(mean ~ age * sex + sex * year),
age ~ DLM(covariates = Covariates(mean ~ is.infant, data = is.infant.df), 
    damp = NULL),
year ~ DLM(damp = NULL, level = Level(scale = HalfT(scale = 0.2)), 
    trend = Trend(scale = HalfT(scale = 0.2)), error = Error(scale = HalfT(scale = 0.2))),
sex:year ~ DLM(),
0.08
dimensions: age, sex, year
-------------------------------------------------- 
y:
Object of class "Counts"
dimensions: age, sex, year
n cells: 1920,  n missing: 0, integers: TRUE, n zeros: 0, median: 589
-------------------------------------------------- 
MCMC statistics:
nBurnin: 12000,  nSim: 10000,  nChain: 4,  nThin: 40,  nIteration: 1000 

Metropolis-Hastings updates:
                      jump acceptance autocorr
model.likelihood.rate 0.08     0.4643     0.05

parameters:
                                    Rhat    2.5%     50%   97.5% length
model.likelihood.rate              1.007  0.0001  0.0028  0.2579   1920
model.prior.(Intercept)            1.002 -5.6450 -5.6377 -5.6320      1
model.prior.age                    1.008 -3.1718 -0.1681  4.2939     20
model.prior.sex                    1.001 -0.2573  0.0000  0.2573      2
model.prior.year                   1.009 -0.4357  0.0140  0.3954     48
model.prior.age:sex                1.022 -0.1995  0.0000  0.1995     40
model.prior.sex:year               1.037 -0.0224  0.0000  0.0224     96
model.prior.sd                     1.002  0.1105  0.1149  0.1187      1
model.hyper.age.level           . 14.274 -3.8253 -0.0923  3.8169     20
model.hyper.age.scaleLevel      .  5.814  0.0028  0.0234  0.7768      1
model.hyper.age.trend           .  2.412 -0.4583  0.4016  0.4434     20
model.hyper.age.scaleTrend      .  1.607  0.0003  0.0040  0.0408      1
model.hyper.age.coef            .  7.208 -0.1294  4.0680  4.6417      1
model.hyper.age.scaleError      .  3.141  0.0000  0.3849  0.5860      1
model.hyper.year.level             1.052 -0.4476 -0.0012  0.4493     48
model.hyper.year.scaleLevel        1.037  0.0000  0.0002  0.0006      1
model.hyper.year.trend             1.053 -0.0202 -0.0198 -0.0190     48
model.hyper.year.scaleTrend        1.041  0.0000  0.0000  0.0000      1
model.hyper.year.scaleError        1.015  0.0049  0.0091  0.0147      1
model.hyper.age:sex.scaleError  .  7.869  0.1213  0.1627  1.1011      1
model.hyper.sex:year.level      .  1.227 -0.0076  0.0000  0.0076     96
model.hyper.sex:year.scaleLevel    1.052  0.0000  0.0002  0.0006      1
model.hyper.sex:year.trend      .  2.751  0.0000  0.0002  0.0104     96
model.hyper.sex:year.scaleTrend .  1.419  0.0000  0.0000  0.0000      1
model.hyper.sex:year.damp       . 19.692  0.8166  0.8367  0.8786      1
model.hyper.sex:year.scaleError    1.018  0.0049  0.0149  0.0227      1
-------------------------------------------------- 
-------------------------------------------------- 
model:
y ~ Poisson(mean ~ age * sex + sex * year),
age ~ DLM(covariates = Covariates(mean ~ is.infant, data = is.infant.df), 
    damp = NULL),
year ~ DLM(damp = NULL, level = Level(scale = HalfT(scale = 0.2)), 
    trend = Trend(scale = HalfT(scale = 0.2)), error = Error(scale = HalfT(scale = 0.2))),
sex:year ~ DLM(),
0.08
dimensions: age, sex, year
-------------------------------------------------- 
y:
Object of class "Counts"
dimensions: age, sex, year
n cells: 1920,  n missing: 0, integers: TRUE, n zeros: 0, median: 589
-------------------------------------------------- 
MCMC statistics:
nBurnin: 32000,  nSim: 10000,  nChain: 4,  nThin: 40,  nIteration: 1000 

Metropolis-Hastings updates:
                      jump acceptance autocorr
model.likelihood.rate 0.08     0.4642   0.0481

parameters:
                                    Rhat    2.5%     50%   97.5% length
model.likelihood.rate              1.006  0.0001  0.0028  0.2586   1920
model.prior.(Intercept)            1.002 -5.6408 -5.6359 -5.6323      1
model.prior.age                    1.009 -3.1640 -0.1784  4.2899     20
model.prior.sex                    1.003 -0.2595  0.0000  0.2595      2
model.prior.year                   1.012 -0.4345  0.0163  0.3987     48
model.prior.age:sex                1.018 -0.2065  0.0000  0.2065     40
model.prior.sex:year               1.076 -0.0228  0.0000  0.0228     96
model.prior.sd                     1.002  0.1105  0.1152  0.1181      1
model.hyper.age.level           . 14.681 -3.8096 -0.0480  3.8524     20
model.hyper.age.scaleLevel      .  5.511  0.0025  0.0384  0.8429      1
model.hyper.age.trend           .  2.238  0.0000  0.3963  0.4578     20
model.hyper.age.scaleTrend      .  1.638  0.0011  0.0036  0.0469      1
model.hyper.age.coef            .  7.404 -0.1294  3.9057  4.6316      1
model.hyper.age.scaleError      .  3.215  0.0000  0.3311  0.5479      1
model.hyper.year.level          .  1.105 -0.4470  0.0003  0.4470     48
model.hyper.year.scaleLevel        1.020  0.0000  0.0002  0.0008      1
model.hyper.year.trend          .  1.106 -0.0203 -0.0198 -0.0185     48
model.hyper.year.scaleTrend        1.038  0.0000  0.0000  0.0000      1
model.hyper.year.scaleError        1.007  0.0039  0.0083  0.0183      1
model.hyper.age:sex.scaleError  .  7.681  0.1285  0.1739  1.1207      1
model.hyper.sex:year.level      .  1.478 -0.0082  0.0000  0.0082     96
model.hyper.sex:year.scaleLevel    1.079  0.0000  0.0002  0.0004      1
model.hyper.sex:year.trend      .  2.536  0.0000  0.0002  0.0107     96
model.hyper.sex:year.scaleTrend .  1.621  0.0000  0.0000  0.0000      1
model.hyper.sex:year.damp       . 17.887  0.8162  0.8400  0.9016      1
model.hyper.sex:year.scaleError    1.032  0.0048  0.0132  0.0238      1
-------------------------------------------------- 
-------------------------------------------------- 
model:
y ~ Poisson(mean ~ age * sex + sex * year),
age ~ DLM(covariates = Covariates(mean ~ is.infant, data = is.infant.df), 
    damp = NULL),
year ~ DLM(damp = NULL, level = Level(scale = HalfT(scale = 0.2)), 
    trend = Trend(scale = HalfT(scale = 0.2)), error = Error(scale = HalfT(scale = 0.2))),
sex:year ~ DLM(),
0.08
dimensions: age, sex, year
-------------------------------------------------- 
y:
Object of class "Counts"
dimensions: age, sex, year
n cells: 1920,  n missing: 0, integers: TRUE, n zeros: 0, median: 589
-------------------------------------------------- 
MCMC statistics:
nBurnin: 52000,  nSim: 10000,  nChain: 4,  nThin: 40,  nIteration: 1000 

Metropolis-Hastings updates:
                      jump acceptance autocorr
model.likelihood.rate 0.08     0.4646   0.0505

parameters:
                                    Rhat    2.5%     50%   97.5% length
model.likelihood.rate              1.013  0.0001  0.0028  0.2591   1920
model.prior.(Intercept)            1.002 -5.6438 -5.6362 -5.6319      1
model.prior.age                    1.007 -3.1638 -0.1736  4.2874     20
model.prior.sex                    0.999 -0.2558  0.0000  0.2558      2
model.prior.year                   1.009 -0.4281  0.0112  0.3993     48
model.prior.age:sex                1.014 -0.2111  0.0000  0.2111     40
model.prior.sex:year               1.082 -0.0227  0.0000  0.0227     96
model.prior.sd                     1.003  0.1125  0.1157  0.1207      1
model.hyper.age.level           . 14.213 -3.8908 -0.0587  3.8585     20
model.hyper.age.scaleLevel      .  6.311  0.0017  0.0427  0.7216      1
model.hyper.age.trend           .  2.624 -0.0763  0.4028  0.4647     20
model.hyper.age.scaleTrend      .  1.528  0.0005  0.0039  0.0549      1
model.hyper.age.coef            .  7.469 -0.1293  4.1142  4.7869      1
model.hyper.age.scaleError      .  2.916  0.0000  0.4033  0.5204      1
model.hyper.year.level          .  1.105 -0.4442  0.0001  0.4441     48
model.hyper.year.scaleLevel        1.045  0.0000  0.0002  0.0006      1
model.hyper.year.trend          .  1.106 -0.0205 -0.0195 -0.0182     48
model.hyper.year.scaleTrend        1.024  0.0000  0.0000  0.0000      1
model.hyper.year.scaleError        1.028  0.0040  0.0084  0.0139      1
model.hyper.age:sex.scaleError  .  7.390  0.1276  0.1673  1.0542      1
model.hyper.sex:year.level      .  1.436 -0.0075  0.0000  0.0075     96
model.hyper.sex:year.scaleLevel    1.040  0.0000  0.0002  0.0007      1
model.hyper.sex:year.trend      .  3.003  0.0000  0.0002  0.0091     96
model.hyper.sex:year.scaleTrend .  1.703  0.0000  0.0000  0.0000      1
model.hyper.sex:year.damp       . 24.694  0.8161  0.8351  0.9007      1
model.hyper.sex:year.scaleError    1.031  0.0075  0.0160  0.0210      1
-------------------------------------------------- 
-------------------------------------------------- 
model:
y ~ Poisson(mean ~ age * sex + sex * year),
age ~ DLM(covariates = Covariates(mean ~ is.infant, data = is.infant.df), 
    damp = NULL),
year ~ DLM(damp = NULL, level = Level(scale = HalfT(scale = 0.2)), 
    trend = Trend(scale = HalfT(scale = 0.2)), error = Error(scale = HalfT(scale = 0.2))),
sex:year ~ DLM(),
0.08
dimensions: age, sex, year
-------------------------------------------------- 
y:
Object of class "Counts"
dimensions: age, sex, year
n cells: 1920,  n missing: 0, integers: TRUE, n zeros: 0, median: 589
-------------------------------------------------- 
MCMC statistics:
nBurnin: 72000,  nSim: 10000,  nChain: 4,  nThin: 40,  nIteration: 1000 

Metropolis-Hastings updates:
                      jump acceptance autocorr
model.likelihood.rate 0.08     0.4648   0.0572

parameters:
                                    Rhat    2.5%     50%   97.5% length
model.likelihood.rate              1.008  0.0001  0.0028  0.2585   1920
model.prior.(Intercept)            1.000 -5.6423 -5.6360 -5.6331      1
model.prior.age                    1.006 -3.1636 -0.1647  4.2934     20
model.prior.sex                    0.999 -0.2577  0.0000  0.2577      2
model.prior.year                   1.011 -0.4300  0.0153  0.4043     48
model.prior.age:sex                1.015 -0.2058  0.0000  0.2058     40
model.prior.sex:year               1.090 -0.0234  0.0000  0.0234     96
model.prior.sd                     1.002  0.1109  0.1149  0.1190      1
model.hyper.age.level           . 14.736 -3.7847 -0.0454  3.7786     20
model.hyper.age.scaleLevel      .  5.136  0.0006  0.0529  1.1015      1
model.hyper.age.trend           .  2.496 -0.2869  0.4013  0.4380     20
model.hyper.age.scaleTrend      .  1.673  0.0005  0.0036  0.1204      1
model.hyper.age.coef            .  7.193 -0.1293  3.9613  4.8722      1
model.hyper.age.scaleError      .  3.203  0.0000  0.3534  0.5143      1
model.hyper.year.level          .  1.118 -0.4454  0.0002  0.4461     48
model.hyper.year.scaleLevel        1.028  0.0000  0.0001  0.0006      1
model.hyper.year.trend          .  1.119 -0.0204 -0.0194 -0.0190     48
model.hyper.year.scaleTrend        1.022  0.0000  0.0000  0.0000      1
model.hyper.year.scaleError        1.007  0.0038  0.0091  0.0149      1
model.hyper.age:sex.scaleError  .  7.683  0.1185  0.1908  1.1623      1
model.hyper.sex:year.level      .  1.412 -0.0101  0.0000  0.0101     96
model.hyper.sex:year.scaleLevel    1.054  0.0000  0.0002  0.0009      1
model.hyper.sex:year.trend      .  3.310 -0.0001  0.0002  0.0094     96
model.hyper.sex:year.scaleTrend .  1.693  0.0000  0.0000  0.0000      1
model.hyper.sex:year.damp       . 28.021  0.8167  0.8409  0.8949      1
model.hyper.sex:year.scaleError    1.012  0.0079  0.0149  0.0220      1
-------------------------------------------------- 
> > 
