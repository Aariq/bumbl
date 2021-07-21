##### Code for fitting growth-decay model to bumble bee colony weights, version 2
##### Elizabeth Crone, 5 October, 2017
# Throughout the comments, I write as if each "Round" were one week.  My memory is that rounds may be 10-day periods, which only slightly changes the interpretation.  Is there a word for a 10-day period (decaday?)
library(car)
library(MASS)
library(lme4)

setwd("C:/Users/ecrone01/Box Sync/Documents/bumble bees NSF/summer 2017 experiment")
wts = read.csv("ColonyWeights2.csv")
summary(wts)

sites = unique(wts$ColonyID)
sites

######## The code below loops over possible values for the break point from growth to decline in each colonly
# a separate curve is fit to each colony because I do not think there are any commonly used methods for fitting
#this kind of curve to multiple data sets at once

# This version of the model estimates different growth rates during weeks in which resources
# were supplemented and when they were not

lm(log(TrueColonyWt_g) ~ Round + Supp + post, data = dat)

taos = (seq(2,8,0.1)) #possible values for breakpoint
taoML = array()
for(j in 1:length(sites)){
  dat = wts[wts$ColonyID == sites[j],]
  LLs = array(NA, dim = c(length(taos),1))

  for(i in 1:length(taos)){
    usetao = taos[i]
    dat$post = dat$Round - usetao
    dat$post[(dat$post < 0) == T] = 0 #post is 0 if before tao, numeric if after tao

    m0 = try(lm(log(TrueColonyWt_g) ~ Round + Supp + post, data = dat))
    if(class(m0) != "try-error") LLs[i] = logLik(m0)
    LLs
  }
  taoML[j] = taos[which(LLs == max(LLs))]

}

taoML # show the list of maximum-likelihood breakpoints for each colony

###### the loop below extracts the colony-dynamic coefficients for each colony, given the ML value of tao
params = array(NA, dim = c(length(sites), 7))
for(k in 1:length(sites)){
  dat = wts[wts$ColonyID == sites[k],]
  usetao = taoML[k]
  dat$post = dat$Round - usetao
  dat$post[(dat$post < 0) == T] = 0
  m1 = lm(log(TrueColonyWt_g) ~ Round + Supp + post, data = dat) #refits best models with tao  Might be faster to save all models or just save best model instead of re-fitting
  print(sites[k])
  print(summary(m1))
  params[k,] = (c(sites[k],usetao, coef(m1), max(predict(m1), na.rm = T)))
  # plots of each colony's trajectory and fitted curve, for visual inspection
  # these lines of code can be turned off without loss of function
  # but I like to look at them to be sure nothing went wrong
  plot(dat$Round, dat$TrueColonyWt_g, main = sites[k])
  points(dat$Round[1:length(predict(m1))], exp(predict(m1)), type = "l", col = "red")
}# this loop returns `params` and prints models and plots as side-effect


#### this code converts the matrix of model parameters to a data frame, with treatment groups appended
# it may be that we want to append additional features, e.g., I'm not sure about the "site" and "pair" columns on the data frame
params = data.frame(params)
params
colnames(params) = c("ColonyID", "tao", "logNo", "loglam", "suppl", "decay", "logNmax")
# tao is the switchpoint, in weeks.  The colony grows for tao weeks, then begins to decline in week tao + 1
# logNo is the intercept of the growth function.  It reflects actual initial colony size, if the colony initially grows exponentiallyl.  It would also be lower if there were a few weeks lag before growth started in the field
# loglam is the average 10-day (log-scale) colony growth rate (i.e., rate of weight gain) during the growth period
# suppl is the additional growth during the supplemental resource period, i.e., the log-scale growth rate during the nectar supplementation = loglam + suppl
# decay reflects the rate of decline during the decline period.  In fact, the way this model is set up, the actual  rate of decline per 10 days is calculated as decay - loglam.
# logNmax is the maximum weight reached by each colony.  It is a function of tao, logNo and loglam
head(params)
param.dat = merge(params, wts[wts$Round == 0,c(2,4,5)], all.y = F) #full_join()
param.dat = param.dat[1:length(sites), ] #not sure why necessary
param.dat

##### this set of code compares parameters among treatments
# When I have a number of possibly-correlated variables to compare among groups, I like to start with a manova as an omnibus test for signifcant patterns, then move to univariate tests of input variables and possibly ecologically relevant derived variables
w0 = manova(cbind(tao,loglam,logNo, suppl) ~ TRT, data = param.dat) # omnibus test for significance across 4 variables
# NB max colony weight not included, since it is a function of the other 4 variables
summary(w0)
Anova(w0)

w1 = lm(tao ~ TRT, data = param.dat) # analysis of switch time
w1 = lmer(tao ~ TRT + (1|Site), data = param.dat)
Anova(w1)  # these p-values are only approximate since a normal model is not especially good here
hist(resid(w1))
w1b = lm(tao ~ -1 + TRT, data = param.dat)
w1b = lmer(tao ~ -1 + TRT + (1|Site), data = param.dat)
coef(w1b) # early colonies seem to grow longer
fixef(w1b)
confint(w1b)

w2 = lmer(loglam ~ TRT + (1|Site), data = param.dat) # analysis of non-pulse growth rate
Anova(w2)
hist(resid(w2))
w2b = lmer(loglam ~ -1 + TRT + (1|Site), data = param.dat)
fixef(w2b) # early colonies have faster growth rates
exp(fixef(w2b)) # back-transformed, early suppl. colonies grow by 20% per week (or by decaday?) and late suppl. ones by 10% when not supplemented
exp(confint(w2b)) # use this for growth rates
(confint(w2b)) # use this for variances
exp(0.7*fixef(w2b)) # growth rates per decaday converted to growth rates per week
coef(w2) # coefficients for each site
# EC quick test for effect of forbs
Forbs = c(1,1,0,1,1,0,1,0,0,1,0,0,1,0)
Anova(lm(coef(w2)$Site[,1] ~ Forbs))

w3 = lmer(logNo ~ TRT + (1|Site), data = param.dat)
Anova(w3)
hist(resid(w3))
w3b = lmer(logNo ~ -1 + TRT +(1|Site), data = param.dat)
fixef(w3b) # no difference in initial colony size after accounting for different growth rates
exp(fixef(w3b)) # NS difference is modest, even when back-transformed
confint(w3b)

# analysis of logNmax.  In this version of code, I include the estimated max of colonies that never declined, not the value at the breakpoint
w4 = lmer(logNmax ~ TRT + (1|Site), data = param.dat)
Anova(w4) # marginally signifciant result that ES colonies get bigger
hist(resid(w4))
w4b = lmer(logNmax ~ -1 + TRT + (1|Site), data = param.dat)
fixef(w4b) # although LS colonies might grow longer, they don't catch up
exp(fixef(w4b)) # difference is more than 1.5x higher when back-transformed
confint(w4b)
exp(confint(w4b))

# analysis of growth rate during supplementation
w6 = lmer(suppl+loglam ~ TRT + (1|Site), data = param.dat)
Anova(w6)
hist(resid(w6))
w6b = lmer(suppl+loglam ~ -1 + TRT +(1|Site), data = param.dat)
fixef(w6b) # highly nonsignificant, very small tendency towards faster growth during the pulse for late pulses
exp(fixef(w6b)) # NS effect is small even when back-transformed
(confint(w6b)) # use for variance terms
exp(confint(w6b)) # use for growth rates
coef(w6) # pulls out site-specific growth rates


# extract parameters and make a plot
# baseline growth rate
fixef(w2b)
#

