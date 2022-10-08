library(RcppArmadillo)
library(Rcpp)
library(xtable)
library(bcf)
library(bayeslm)
library(glmnet)
library(dbarts)
library(rpart)
library(rpart.plot)
library(party)
library(partykit)

# load in cleandata
load('cleandata.RData')
Y = as.matrix(cleandata$Y)
X = as.matrix(cleandata$X)
D = as.matrix(cleandata$D)

# Descriptive statistics 
summary(Y)
sd(Y)

summary(D)
sd(D)

# 1. Multiple linear regression model: what's the coef of D?
fitandprint = function(Y,X,D){
  Xd = cbind(D,X)  
  p = dim(X)[2]
  fit = lm(Y ~ Xd - 1)
  beta_hat = fit$coef[2:(p+1)]
  alpha_hat = fit$coef[1]
  out = summary(fit)
  sdOLS <- out$coefficients[1,2]
  qOLS = c(alpha_hat-1.96*sdOLS,alpha_hat+1.96*sdOLS)
  
  names(alpha_hat) = c('coefficient')
  names(qOLS) = c('lower','upper')
  
  print(round(alpha_hat,3))
  print(round(qOLS,3))
}

fitandprint(Y,X,D)

# 2. Two-step model to get the regularized treatment effect using Bayesian linear regression
fit_bayeslm = function(Y,X,D){
  # First step (the selection model)
  fittreat = bayeslm(D~X-1,prior='horseshoe',N=100000,burnin=10000) 
  beta = as.matrix(colMeans(fittreat$beta))
  Dhat = X%*%beta 
  DDhatX = cbind(D,Dhat,X) 
  p = dim(X)[2]
  # Second step (the outcome model)
  fitobs = bayeslm(Y = Y,X = DDhatX,prior='horseshoe',penalize = c(0,0,rep(1,p)),icept=FALSE,singular=FALSE,N=100000,burnin=10000) 
  return(fitobs)
}

fit_bayeslm2 <- fit_bayeslm(Y,X,D)
# Histogram of estimated causal effects of financial literacy on financial well-being
hist(fit_bayeslm2$beta[,1], 
     main="Histogram of Estimated Causal Effects",
     xlab = "Posterior of Regression Coefficient")
# Average estimated causal effect
mean(fit_bayeslm2$beta[,1]) 

# 3. Two-stage model using Bayesian causal forests
fit_bcf = function(Y,X,D,datnew){
  if(typeof(D)!="logical"){ # current version of bcf takes only binary variables
    D = as.numeric((D>median(D))) # 1 if it's greater than the median
  }
  # First stage
  fit_glmnet = cv.glmnet(x=X,y=D,family='binomial') 
  pihat = predict(fit_glmnet,newx=X)
  # Second stage  
  fit_bcf = bcf(y=Y,z=D,x_control=X,x_moderate=X,pihat=pihat,nburn=1000,nsim=9000)
    # the codes used for the original paper: nburn=10000, nsim=90000
    # but it takes much time so I replaced them with lower numbers here
  return(fit_bcf$tau)
}

bcf1 <- fit_bcf(Y,X,D,datnew)
summary(bcf1)
xdf = data.frame(X)
n = nrow(xdf)
weights <- rep(1, n) # assign no weight
tree <- rpart(colMeans(bcf1) ~ ., data=xdf, weights = weights,
              control=rpart.control(maxdepth=3, cp=-1))
# Plot the regression tree
rpart.plot(tree) 

# another way to show the tree
ptree = as.party(tree)
print(ptree)
subgp = predict(ptree, type='node')
subgp_id = sort(unique(subgp))

# To get the differences in average treatment effects between subgroups
get_sub_post = function(ix, fit, weights) {
  subtaus = fit[,ix]
  apply(subtaus, 1, weighted.mean, weights[ix])
}
subgp_post = lapply(subgp_id, function(x) get_sub_post(subgp==x, bcf1, weights))
names(subgp_post) = subgp_id

# Histogram of differences in average treatment effects between subgroups
hist((subgp_post[['4']] - subgp_post[['5']]), breaks = 50, xlim = c(-2, 1), main = "Subgroup 1 - Subgroup 2",
     xlab = "Differences in Treatment Effects between Subgroups")
hist((subgp_post[['7']] - subgp_post[['8']]), breaks = 50, xlim = c(-1, 0.5), main = "Subgroup 3 - Subgroup 4",
     xlab = "Differences in Treatment Effects between Subgroups")
hist((subgp_post[['11']] - subgp_post[['12']]), breaks = 50, xlim = c(-2, 1), main = "Subgroup 5 - Subgroup 6",
     xlab = "Differences in Treatment Effects between Subgroups")
hist((subgp_post[['14']] - subgp_post[['15']]), breaks = 50, xlim = c(-1, 0.5), main = "Subgroup 7 - Subgroup 8",
     xlab = "Differences in Treatment Effects between Subgroups")

# Compare Subgroup 1 and Subgroup 8
hist((subgp_post[['4']] - subgp_post[['15']]), breaks = 50, xlim = c(-4, 1), main = "Subgroup 1 - Subgroup 8",
     xlab = "Differences in Treatment Effects between Subgroups")
mean((subgp_post[['4']] - subgp_post[['15']]) < 0) 

# Compare Subgroups spending less than income and Subgroups spending more than income
hist((subgp_post[['4']] +subgp_post[['5']]+subgp_post[['7']]+subgp_post[['8']]
      - subgp_post[['11']] - subgp_post[['12']]- subgp_post[['14']] - subgp_post[['15']]), 
     breaks = 50, xlim = c(-10, 5), main = "Spend Less than Income - Spend More than Income",
     xlab = "Differences in Treatment Effects between Subgroups")
mean((subgp_post[['4']] +subgp_post[['5']]+subgp_post[['7']]+subgp_post[['8']]
      - subgp_post[['11']] - subgp_post[['12']]- subgp_post[['14']] - subgp_post[['15']]) < 0) 
