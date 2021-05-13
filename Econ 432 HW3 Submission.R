#Econ 432 HW3 R Questions 

#1. This exercise uses weekly data in Apple from the Örst week of January,
#2010 to the last week of January, 2021. Use the adjusted closing prices
#to compute weekly cc returns. Assume that the returns are i.i.d., even
#though there may be some autocorrelation and volatility clustering is
#likely. In bootstrap, let B = 1000 and set the random seed to 432, i.e.,
#set.seed("432") in R.

#a) Find the sample mean and the sample variance of the cc return,
#and also their standard errors (SEs) using the formula provided
#in the slides of statistical inference.
library(tseries)
Apple.prices = get.hist.quote(instrument="AAPL", start="2010-01-01",
                             end="2021-01-31", quote="AdjClose",
                             provider="yahoo", 
                             compression="d", retclass="zoo") 

set.seed(432)
AAPL.return = diff(log(Apple.prices))
T = length(AAPL.return)

mu.hat = mean(AAPL.return)
sigma.hat = sd(AAPL.return)
print(mu.hat)
print((sigma.hat)^2)

SE = sigma.hat/sqrt(T-1)
print(SE)



#b)Find the bootstrap SEs (the bootstrap SEs and the bootstrap IQR
#SEs) of the sample mean and the sample variance. Compare the
#bootstrap SEs with the SEs in (a). Are they very different from
#each other for the same estimator (i.e., the sample mean and the
                                   #sample variance)?

library(bootstrap)
nboot = 1000
sam_m = function(x){mean(x)}
Ret <- as.numeric(SBUX.return)
results = bootstrap(Ret, nboot, sam_m)


str(results$thetastar)
head(results$thetastar)
max(results$thetastar)

B_SE = sd(results$thetastar)
IQR_SE = IQR(results$thetastar)/(qnorm(0.75)-qnorm(0.25))

cbind(mu.hat, SE, B_SE, IQR_SE)
#We see here that the Standard errors are roughly close to each other

#c)Find the bootstrap SEs (the bootstrap SEs and the bootstrap IQR
#SEs) of the sample standard deviation.

B_SE_SD <- sd(results$thetastar)
B_SE_SD

Q_SE <- quantile(results$thetastar, c(0.25, 0.75))
B_IQRSE <- (Q_SE[2] - Q_SE[1])/(qnorm(0.75) - qnorm(0.25))
B_IQRSE

#d) Estimate the bias and the mean square error of the sample standard deviation
#using bootstrap
results2 = bootstrap(B_SE_SD, nboot, sam_m)

B_bias = mean(results2$thetastar) - sigma.hat
B_bias

MSE = mean((results$thetastar-sigma.hat)^2)
MSE


#e)
#Construct the equal tail and the symmetric 95% bootstrap confidence intervals of 
#the standard deviation of the cc return
alpha = .05
q_sym = quantile(abs(results$thetastar-sigma.hat),1-alpha)
q_et_1 = quantile(sigma.hat-results$thetastar,alpha/2)
q_et_2 = quantile(sigma.hat-results$thetastar,1-alpha/2)

CI_sym = c(sigma.hat-q_sym, sigma.hat+q_sym)
CI_et = c(sigma.hat+q_et_1, sigma.hat+q_et_2)

cbind(CI_sym,CI_et)

#f)
#Suppose we invest $1000 on Apple for one week. Find the parametric and the 
#nonparametric estimators of VaR(0.1) and ES(0.1)
#for this investment. For the parametric estimator you shall assume
#that the returns are normally distributed.
library(MASS)
fit <- fitdistr(AAPL.return, densfun="normal") 
str(fit) 
fit$estimate 

W0 <- 10000
alpha <- 0.1
L1 <- W0*(exp(SBUX.return)-1)
VaR_Para_Est <- W0*(exp(qnorm(alpha,mu.hat,sigma.hat))-1)
VaR_Para_Est

VaR_NonP_Est <- W0*(exp(quantile(AAPL.return,alpha))-1)
VaR_NonP_Est

plot(L1)
abline(h=c(VaR_Para_Est,VaR_NonP_Est), col=c("red","blue"))


ind <- as.numeric(L1 <= VaR_NonP_Est)
nonp_ES <- mean(L1*ind)/mean(ind); nonp_ES


#g)
#Find the bootstrap SEs (the bootstrap SEs and the bootstrap IQR
#SEs) of the nonparametric estimators of VaR(0.1) and ES(0.1) in part f

#ES Nonparametric estimator stuff 
ES_Est   <- function(x, p=alpha){
  var_est  = W0*(exp(quantile(x, p)) - 1)
  L1       = W0*(exp(x) - 1)
  ind      = as.numeric(L1 <= var_est)
  f_val    = mean(L1*ind)/mean(ind)
  return(f_val)
}

results_ES <- bootstrap(Ret,nboot,ES_Est)

nonp_ES # the ES based on the sample
str(results_ES$thetastar) # the bootstrap ES based on the bootstrap samples

Q_ES <- quantile(results_ES$thetastar, c(0.25, 0.75))
B_SE_ES <- sd(results_ES$thetastar)
B_IQRSE_ES <- (Q_ES[2] - Q_ES[1])/(qnorm(0.75) - qnorm(0.25))

cbind(Q_ES,B_SE_ES,B_IQRSE_ES)

#VAR non parametric SE stufff 
VaR_Est  <- function(x, p = alpha){ W0*(exp(quantile(x, p)) - 1) }

Ret <- as.numeric(SBUX.return) # turn the time series into a numeric vector
results_VaR <- bootstrap(Ret, nboot, VaR_Est)

VaR_NonP_Est # estimated VaR based on the sample
str(results_VaR$thetastar)

B_SE_VaR <- sd(results_VaR$thetastar); B_SE_VaR

Q_VaR <- quantile(results_VaR$thetastar, c(0.25, 0.75))
B_IQRSE_VaR <- (Q_VaR[2] - Q_VaR[1])/(qnorm(0.75) - qnorm(0.25)); B_IQRSE_VaR

#h)
#Find the 95% confdence intervals of VaR(0.1) and ES(0.1) using
#the nonparametric method in (f).

#For VAR 
q_symm <- quantile(abs(results_VaR$thetastar - VaR_NonP_Est), 0.95)
q_eqt <- quantile(VaR_NonP_Est - results_VaR$thetastar, c(0.025, 0.975))

CI_B_SE1 <- c(VaR_NonP_Est - B_SE_VaR*1.96,    VaR_NonP_Est + B_SE_VaR*1.96); CI_B_SE1

CI_B_SE2 <- c(VaR_NonP_Est - B_IQRSE_VaR*1.96, VaR_NonP_Est + B_IQRSE_VaR*1.96); CI_B_SE2
CI_B_SYM <- c(VaR_NonP_Est - q_symm,    VaR_NonP_Est + q_symm); CI_B_SYM

CI_B_EQT <- c(VaR_NonP_Est + q_eqt[1],  VaR_NonP_Est + q_eqt[2]); CI_B_EQT


#For ES 
q_symm <- quantile(abs(results_ES$thetastar - nonp_ES), 0.95)
q_eqt <- quantile(nonp_ES - results_ES$thetastar, c(0.025, 0.975))

CI_B_SE1 <- c(nonp_ES - B_SE_ES*1.96,    nonp_ES + B_SE_ES*1.96); CI_B_SE1

CI_B_SE2 <- c(nonp_ES - B_IQRSE_ES*1.96, nonp_ES + B_IQRSE_ES*1.96); CI_B_SE2

CI_B_SYM <- c(nonp_ES - q_symm,    nonp_ES + q_symm); CI_B_SYM

CI_B_EQT <- c(nonp_ES + q_eqt[1],  nonp_ES + q_eqt[2]); CI_B_EQT
