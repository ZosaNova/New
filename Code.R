#################################################################
#   Team:
#   - Mikhail Stukalo
#   - Rishabh Ghosh
#   - Wail Choudar
#

# Please read ReadMe.txt 
# The snapshots of the project are presented in Final.Rmd and the accompanying 
# Final.pdf
# The summary of the project is in Summary.pdf
# You can find the link to the video description at !!!!!!!!!!!!!



# Inspiration
# https://www.barrons.com/articles/no-your-etf-doesnt-track-the-vix-volatility-index-and-here-are-the-numbers-1403010972


# This is a quick check that you have all the packages we need

packages = c("quantmod", "PerformanceAnalytics", "xts", "dplyr",
             "tidyr", "ggplot2", "fitdistrplus", "tibble", "fGarch", "EnvStats", 
             "RColorBrewer", "reshape2", 'gridExtra', 'ggfortify')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}



# Load libriaries

library(quantmod) # Financial data
library(PerformanceAnalytics) # Financial analysis
library(xts) # Time indexed data frames
require(dplyr) # Data manipulation
library(tidyr) # Tidy data
library(ggplot2) # Plotting
library(fitdistrplus) # Fitting distributions
library(tibble)  # More advanced type of a data frame
library(fGarch) # Skewed normal distribution
library(RColorBrewer) # Brewer
library(reshape2) # Reshaping
library(gridExtra) # 
library(ggfortify) # Autoplot


# We download data for VIX index and VIXY ETF using quantmod package
# To ensure the consistency of the analysis, we saved the data into .RDS files
# We still provide the code for downloading the data sets, but we comment it out 
# Download data
# vix = getSymbols("^VIX", auto.assign = F) # Download VIX data
# Save for future use
# saveRDS(vix, "./data/vix.RDS")
# write.csv(fortify(vix), "./data/vix.csv", row.names = F)
vix = read.csv("./data/vix.csv")

# Convert to xts
# Helper function
to_xts = function(df){
  xts(df[,2:ncol(df)], order.by = as.Date(df[,1]))
}

vix = to_xts(vix)


# Calculate returns
vix_r = CalculateReturns(vix)$VIX.Adjusted


# Download VIXY
# vixy = getSymbols("VIXY", auto.assign = F)
# saveRDS(vixy, "./data/vixy.RDS") #Save for future use
# write.csv(fortify(vixy), "./data/vixy.csv", row.names = F)
vixy = read.csv("./data/vixy.csv")
vixy = to_xts(vixy)

# Calculate returns 
vixy_r = CalculateReturns(vixy)$VIXY.Adjusted


################################################################################
# Question 1: ETF monthly beta to VIX index
################################################################################

# Beta of VIXY monthly returns
# Calculate monthly returns
vix_m = monthlyReturn(vix)
vixy_m = monthlyReturn(vixy)

# Combine in one df
colnames(vix_m) = "VIX"
colnames(vixy_m) = "VIXY"

df = merge.xts(vixy_m, vix_m,  join ="left" )

# Drop first observation (incomplete month)
df = df[2:nrow(df),]
df = fortify(df) # Convert to a dataframe


# Helper function to plot. 
# Partially reused the code from https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

lm_eqn <- function(x, y, xlab="", ylab="", main=""){
  tt = data.frame(x = x, y = y)
  m <- lm(y ~ x, tt);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  eq = as.character(as.expression(eq))
  
  ggplot(tt, aes(x,y)) + geom_point(color=c('darkgreen')) + theme_minimal() + ggtitle(main) +
    xlab(xlab) + ylab(ylab) + geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
    geom_text(x = max(x)*0.8, y = max(y)*0.8, label = eq, parse = TRUE)
}



# Scatter plot
lm_eqn(df$VIX, df$VIXY, xlab='VIX', ylab='VIXY', main = 'Scatter plot of VIXY vs VIX monthly returns')

# We see that the ETF does not track the index well. VIXY beta of monthly returns is
# 0.6 although with a well-tracking ETF we should expect beta close to one.


################################################################################
# Question 2: ETF tracking quality (beta and correlation of returns)
################################################################################

# Question 2a
# Does daily beta depend on market regime

# Caluclate beta of daily returns
rr = merge.xts(vix_r[which(index(vix_r)>=min(index(vixy_r))),], vixy_r)
colnames(rr) = c("VIX","VIXY")


# Beta is a slope coefficient of the linear regression. We calculate it as cov/var

calcBeta = function(x,y){
  cov(x,y, use="complete.obs")/var(x, na.rm=T)
}


# Calculate monthly betas
df_m = apply.monthly(rr, function(x) calcBeta(x=x$VIX, y=x$VIXY)) 


# Example of monthly beta
lm_eqn(rr["2012-01",]$VIX, rr["2012-01",]$VIXY, xlab='VIX', ylab='VIXY', 
       main = 'Scatter plot of VIXY vs VIX daily returns Jan-2012')

# If we look at daily performance within months, it is even worse. For example, 
# beta of daily returns in Jan-2012 is 0.39

# Define market regimes. We look at the closing value of VIX for a certain month and
# compare it to the thresholds to define the regime
# We introduce two catergorical variables:
# OnOff - risk on/risk of; True if VIX closes above its 12 month moving average
# Risk level - High, Normal, Low. Whether VIX closes above, between or below threshold values

# We pick treshold values somewhat subjective based on the history of VIX
# Define market regimes

thresholds = c(15,20)
plot(fortify(vix$VIX.Adjusted), type='l', col='limegreen', main='VIX historic levels')
abline(h=thresholds[1], col='darkgreen')
abline(h=thresholds[2], col='red')


# Create vix dataframe
vix_df = to.monthly(vix)$vix.Adjusted
colnames(vix_df) = "VIX_Price"

# Calculate and add 12 month moving average
ma = rollapply(data = vix_df, width = 12, FUN = mean)
colnames(ma) = "MA"
vix_df = merge.xts(vix_df, ma)


# Create categorical variables
vix_df = data.frame(vix_df) %>% rownames_to_column('DT') %>% 
  mutate(OnOff = VIX_Price > MA, 
         RiskLevel = ifelse(VIX_Price > thresholds[2], 'High Risk',
                            ifelse(VIX_Price < thresholds[1], 'Low Risk',
                                   'Normal Risk')),
         RiskLevel = factor(RiskLevel),
         DT = as.yearmon(DT)
  )


# Add betas
colnames(df_m) = "Beta"
df_m = data.frame(df_m) %>% rownames_to_column('DT')
df_m$DT = as.yearmon(df_m$DT)
vix_df$DT = as.yearmon(vix_df$DT)
vix_df = merge(df_m, vix_df, by='DT', all.x=T)

# Boxplots (simple)
boxplot(Beta~OnOff, vix_df, main = 'Risk On/Off')
boxplot(Beta~RiskLevel, vix_df, main = 'Risk Level')

# It looks like during high risk environment ETF beta increases, i.e.
# ETF tracks index a bit better. It is still far from 1. But generally it
# is a good sign: you want the ETF to be close to index when index indicates
# high market volatility. 

# Contigency tables
vix_df %>% group_by(OnOff) %>% summarize(MeanBeta = mean(Beta))
vix_df %>% group_by(RiskLevel) %>% summarize(MeanBeta = mean(Beta))

# It is important to note that we had two different approaches 
# to creating categorical variables. Therefore, they are not identical 
# We can see it from the contingency table
table(vix_df$RiskLevel, vix_df$OnOff)

# Risk-on generally includes periods that we denote as High and Normal Risk
# However, there are a couple of High-risk periods that are labeled as Riks-off
# and one Low Risk period labeled as Risk-on. Normal risk levels are divided
# almost equally between risk-on and risk-off. If does not mean that the classification
# is inadequate, but rather that we are looking at two possible way to measure the
# level of risk based on VIX index. 


# Permutation test
# We check if the ETF beta differs in risk-on and risk-off months
tmp = vix_df %>% dplyr::select(OnOff, Beta)
N_on = nrow(tmp[tmp$OnOff,])

obs_diff = mean(tmp[tmp$OnOff==T,'Beta']) - mean(tmp[tmp$OnOff==F,'Beta'])

N = 10000
store = numeric(N)

for (i in 1:N){
  ind_on = sample(1:nrow(tmp), size = N_on, replace = F)
  mu_on = mean(tmp$Beta[ind_on])
  mu_off = mean(tmp$Beta[-ind_on])
  
  
  # Store simulated  diff
  store[i] = mu_on - mu_off
}


# Simple plot
hist(store, col='blue')
abline(v = obs_diff, col='red')


# P-value
(sum(store>obs_diff) + sum(store<(-obs_diff)))/N

# P-value is 0.6. So with 10% confidence level we can REJECT the null that there is 
# no difference between beta in risk-on and risk-off months

# Compare to built-in t-test
t.test(tmp[tmp$OnOff==TRUE,'Beta'], tmp[tmp$OnOff==FALSE,'Beta'])

# P-value is a bit higher. But in the same range. The reason might be that 
# our distributions are not normal and their variance may be different too


# Question 2b
# Do the same but with correelation of returns
# Correlation version


# Calculate correlation of daily returns
cor_df = apply.monthly(rr, function(x) cor(x[,1], x[,2], use="complete.obs"))


# Create one data frame. Convert price to end of month
df = to.monthly(vix)[,6]
colnames(df) = c('VIX_Price')

plot(df$VIX_Price, type='l')

# Create variables to define market regimes
# OnOff - risk on/risk off TRUE if average VIX is higher that 12 month moving average
# RiskLevel - factor denoting if the end of month price is higher than thresholds

# Calculate and add 12 month moving average
ma = rollapply(data = df, width = 12, FUN = mean)
colnames(ma) = "MA"
df = merge.xts(df, ma)


df = data.frame(df) %>% rownames_to_column('DT') %>% 
  mutate(OnOff = VIX_Price > MA, 
         RiskLevel = ifelse(VIX_Price > thresholds[2], 'High Risk',
                            ifelse(VIX_Price < thresholds[1], 'Low Risk',
                                   'Normal Risk')),
         RiskLevel = factor(RiskLevel),
         DT = as.yearmon(DT)
         )

# Add Correlations
cor_df = data.frame(cor_df) %>% mutate(DT = as.yearmon(rownames(.)))
colnames(cor_df) = c('Return.Correlation', 'DT')

df = merge(df, cor_df, by='DT', all.x=T)
df = df[complete.cases(df),] %>% arrange(DT)


# Boxplots (simple)
boxplot(Return.Correlation~OnOff, df, main = 'Risk On/Off')
boxplot(Return.Correlation~RiskLevel, df, main = 'Risk Level')

# Permutation test 
tmp = df %>% dplyr::select(OnOff, Return.Correlation)
N_on = nrow(tmp[tmp$OnOff,])

obs_diff = mean(tmp[tmp$OnOff==T,'Return.Correlation']) - mean(tmp[tmp$OnOff==F,'Return.Correlation'])

N = 10000
store = numeric(N)

for (i in 1:N){
  ind_on = sample(1:nrow(tmp), size = N_on, replace = F)
  mu_on = mean(tmp$Return.Correlation[ind_on])
  mu_off = mean(tmp$Return.Correlation[-ind_on])
  
  
  # Store simulated  diff
  store[i] = mu_on - mu_off
}


# Simple plot
hist(store, col='blue')
abline(v = obs_diff, col='red')


# P-value
(sum(store>obs_diff) + sum(store<(-obs_diff)))/N

# We conclude that mean correlation between VIX and VIXY daily
# returns is different for risk-on and risk-off times


################################################################################
# Question 3: Can we model daily VIX volatility
################################################################################

vix_vol = vix_r[-1,] # Remove the first day (it is NA)
colnames(vix_vol) = 'VIX'

# Convert to dataframe, calculate monthly volatility of daily returns
vix_vol = data.frame(vix_vol) %>% rownames_to_column('DT') %>% 
  mutate(Month = as.yearmon(DT)) %>% group_by(Month) %>%
  summarize(Vol = sd(VIX))

# Divide to train and test sets to check how well it works out-of-sample
date_split = as.yearmon(as.Date("2016-01-01"))
vix_train = vix_vol[which(vix_vol$Month<date_split),]
vix_test = vix_vol[which(vix_vol$Month>=date_split),]


# Plot histogram
hist(vix_train$Vol, breaks='FD', col='blue')


# Fit beta distribution
fd =  fitdist(vix_train$Vol, 'beta')

coefs = fd$estimate
shape1 = coefs[1]
shape2 = coefs[2]

# Plot fit
hist(vix_train$Vol, breaks='FD', col='blue', probability = T)
curve(dbeta(x, shape1, shape2), col='red', add=T)


# Check fit
h = hist(vix_train$Vol, breaks='FD', plot = F) # Get histogram object
cnt = h$counts
br  = h$breaks

# Create dataframe for test
test_df = data.frame(Breaks = br[2:length(br)],
                     Obs = cnt)

#Combine observations to get at lest 5
which(test_df$Obs<=5)
cut_off = 8
test_df$Breaks[cut_off] = sum(test_df$Obs[cut_off:nrow(test_df)])
test_df = test_df[1:cut_off,]

# Add expected
N_obs = sum(cnt)

tot = 0 # TO store cumulative probability
for (i in 1:nrow(test_df)){
  if (i!=nrow(test_df)){
    test_df[i,'Exp'] = N_obs * (pbeta(test_df$Breaks[i], shape1, shape2) - tot)
    tot = pbeta(test_df$Breaks[i], shape1, shape2)
  }
else {
    test_df[i,'Exp'] = N_obs * (1 - tot)
}
}


# Plot
barplot(rbind(test_df$Obs, test_df$Exp), beside = T, col=c('cornflowerblue', 'pink'))


# Calculate chisq
chisq.val = sum((test_df$Obs - test_df$Exp)^2/test_df$Exp)

# Calculate p-value
pval =  pchisq(chisq.val, df = length(cnt)-3, lower.tail = F)
pval

# p-value is 0.20. Therefore, We fail to reject the null that beta distribution 
# describes the volatility (standard deviation) of daily returns


# We perform the test on the test dataset, to check if the same parameters
# of distribution will be appricable to out-of-sample data


# Test on test df
# Plot fit
hist(vix_test$Vol, breaks='FD', col='blue', probability = T)
curve(dbeta(x, shape1, shape2), col='red', add=T)


# Check fit
h = hist(vix_test$Vol, breaks='FD', plot = F) # Get histogram object
cnt = h$counts
br  = h$breaks

# Create dataframe for test
test_df = data.frame(Breaks = br[2:length(br)],
                     Obs = cnt)

#Combine observations to get at lest 5
which(test_df$Obs<=5)
cut_off = 5
test_df$Breaks[cut_off] = sum(test_df$Obs[cut_off:nrow(test_df)])
test_df = test_df[1:cut_off,]

# Add expected
N_obs = sum(cnt)

tot = 0 # TO store cumulative probability
for (i in 1:nrow(test_df)){
  if (i!=nrow(test_df)){
    test_df[i,'Exp'] = N_obs * (pbeta(test_df$Breaks[i], shape1, shape2) - tot)
    tot = pbeta(test_df$Breaks[i], shape1, shape2)
  }
  else {
    test_df[i,'Exp'] = N_obs * (1 - tot)
  }
}


# Plot
barplot(rbind(test_df$Obs, test_df$Exp), beside = T, col=c('cornflowerblue', 'pink'))


# Calculate chisq
chisq.val = sum((test_df$Obs - test_df$Exp)^2/test_df$Exp)

# Calculate p-value
pval =  pchisq(chisq.val, df = length(cnt)-3, lower.tail = F)
pval

# The p-value is 0.54 so we fail to reject the null and conclude
# that beta distribution with the obtained parameters may be
# a good fit to desribe our data

# Find expectations of daily volatility using integration
exp_vol = integrate(function (x) x*dbeta(x, shape1, shape2), lower = 0, upper = Inf)$value
exp_vol

# Expected value of the return volatility is 7%.


# Find the the value for 95% observations to
mc_beta = qbeta(0.95, shape1, shape2); mc_beta 
# From beta distribution we see that 95% of observations will have volatility of 
# less than 12.01%

# CHeck it with integration
integrate(function(x) dbeta(x, shape1, shape2), 0, mc_beta)$value


################################################################################
# Question 4: Distribution of VIX returns
################################################################################
# Question 4a.
# Can the returns of VIX index be described by normal distribution?
# First, let's compare the distribution of VIX returns with the normal distribution
ret = as.vector(vix_r$VIX.Adjusted) # Extract vector
ret = ret[!is.na(ret)] # Remove NA
mu = mean(ret)
sigma = sd(ret)

hist(vix_r, col='darkgreen', main = "Distribution of VIX daily returns", 
     breaks = 'fd', probability = T)
curve(dnorm(x, mean=mu, sd=sigma), add=T, col='red')

# It looks positively skewed with higher kurtosis. We can check it using 'fitdistrplus' 
# package
descdist(ret)
# Probably it is going to be hard to find a well defined destribution for our set

# We can formally check the fit
# Using deciles

nob = length(ret) # Number of observations
dec = qnorm(seq(0.0, 1, by = 0.1), mu, sigma) # Deciles
Exp = rep(nob/10,10) # Number Of expected observations per bin


binsim = numeric(10) # Bins
for (i in 1:10){  
  binsim[i] <- sum((ret >= dec[i]) & (ret <= dec[i+1]) )
}



chisq.val = sum((binsim - Exp)^2/Exp)
pval =  pchisq(chisq.val, df = 7, lower.tail = F)
pval

# P-value is very small so we can conclude that normal distribution
# is a bad fit for describing returns of VIX index.

# Plot
barplot(rbind(binsim, Exp), beside=T, col = c('deeppink4', 'steelblue4'), 
        main = 'Deciles of Observed(red) and Expected(blue) returns')


# Question 4b. Test CLT
# !!!!!!!!!!!!!!!!! TALK TO  MICHAEL
# CLT DOES NOT WORK
# Just to use an opportunity to test CLT. 
# Our distribution is so badly behaved, so We will use larger sample size and
# check if the sample means follow normal distribution
n_sim = 10000 # 10k simulations
ss = 50 # sample size

res = numeric(n_sim) # TO store 

for (i in 1:n_sim){
  res[i] = mean(sample(ret, ss, replace = F))
}

hist(res, breaks='fd', probability = T, col='steelblue4', 
     main = 'Histogram of means of samples \n of VIX returns (sample size=50)', xlab="")

# Calculate parameters
mu_s = mean(res)
sigma_s = sd(res)
curve(dnorm(x, mean = mu_s, sd = sigma_s), add=T, col='red')

# Use deciles
nob = n_sim
dec = qnorm(seq(0.0, 1, by = 0.1), mu_s, sigma_s) # Deciles
Exp = rep(nob/10,10) # Number Of expected observations per bin


binsim = numeric(10) # Bins
for (i in 1:10){  
  binsim[i] <- sum((res >= dec[i]) & (res <= dec[i+1]) )
}



chisq.val = sum((binsim - Exp)^2/Exp)
pval =  pchisq(chisq.val, df = 7, lower.tail = F)
pval

# Plot
barplot(rbind(binsim, Exp), beside=T, col = c('deeppink4', 'steelblue4'), 
        main = 'Deciles of Observed(red) and Expected(blue) returns')
# P value is low. So Our distribution is so misbehaved (leptokutic and skewed),
# that to make CLT work, we will have to use very large samples



################################################################################
# Question 5: Lagrange interpolation and Monte Carlo simulation
################################################################################

# Use Lagrange interpolation (in fact, extrapolation) with confidence band based on
# beta-distribution of daily returns
# to model VIX for the next 5 days (trading week)

# We use exponensial 20-day moving average to smoothen the percent change (return) of 
# VIX index, then we use Lagrange 
# extrapolation  to forecast drift term and sample from beta distribution to 
# simmulate the noise/volatility term

# This approach is not intended to be completely accurate from the point of
# view of financial analysis, but rather is used to illustrate the use of 
# Lagrange interpolation and MonteCarlo simulation  

# First, an example script, and then we will put it into a function
tmp = vix_r$VIX.Adjusted[2:42]

# Calculate MA
tmp$MA = rollapply(tmp$VIX.Adjusted, 20, mean)


# Plot VIX vs EMA
plot(tmp, col=c("limegreen", "red"))

# Exctract vector of MA
vec = as.vector(tmp$MA)[(nrow(tmp)-19):nrow(tmp)]; vec

plot(vec)

pnt = c(1, 5, 10, 15, 20) # Fourth degreepolynomial

PInv = cbind(rep(1, length(pnt)), pnt, pnt^2, pnt^3, pnt^4); PInv
P = solve(PInv); P

# Interpolation function is based on Paul's lecture code
interp = function(x, val){
  c(1,x,x^2,x^3,x^4) %*% P %*% val
}

# Making the function accept vectors
vect_int = function(v){
  out = c()
  for (i in v){
    out= c(out, interp(i, vec[pnt]))
  }
  
  out
}


# Test interpolation function, extrapolate 
plot(vec) # Observed values
points(pnt, vec[pnt], pch=12, col="red") # Highlight points used to interpolate
lines(vect_int(c(1:20)), type='l')


# Use it to extrapolate
plot(c(vec, rep(NA,5)), ylim = c(min(vec)*0.8,max(vec)*4)) # Observed values
points(pnt, vec[pnt], pch=12, col="red") # Highlight points used to interpolate
lines(vect_int(c(1:20)), type='l')
points(21:25, vect_int(c(21:25)), pch=13, col='green')


# We can see that there is a risk, that everything can turn into an exponensial.
# But again, we don't use to trade it. Just to illustrate the math. 


# Lets put together a versatile function. 
ModelVol = function(observed, shape1, shape2, plot = TRUE, known = NA){
  # Calculate returns from prices
  ret = observed/lag(observed) -1
  ret = ret[2:nrow(observed),]
  
  # Calculate moving average
  
  
  # Check that we have enough data
  if (nrow(ret)<40){
    stop("Please supply at least 41 observations")
  }
  
  # Keep last 20 observations
  vec = as.vector(rollapply(ret, 20, mean))
  vec = vec[(length(vec)-19):length(vec)]
  
  # Use Lagrange extrapolation
  pnt = c(1, 5, 10, 15, 20) # Fourth degreepolynomial
  
  PInv = cbind(rep(1, length(pnt)), pnt, pnt^2, pnt^3, pnt^4)
  P = solve(PInv)
  
  
  # Making the function accept vectors
  vect_int = function(v){
    
    interp = function(x, val){
      c(1,x,x^2,x^3,x^4) %*% P %*% val
    }
    
    out = c()
    for (i in v){
      out= c(out, interp(i, vec[pnt]))
    }
    
    out
  }
  
  # Extrapolate ma for the next five days
  extra = vect_int(21:25)
  
  # Simulate 10000 return paths
  N = 10000
  
  noise = matrix(rbeta(5*N, shape1, shape2), ncol=5)
  
  paths = matrix(rep(extra, N), ncol=5, byrow = T) + noise * matrix(sample(c(-1,1),5*N, replace = T), ncol=5)
  
  paths = t(apply(paths ,1, function(x) cumprod(1+x))) # Compound return
  
  last_price = as.vector(observed[nrow(observed),])
  prices =  matrix(rep(last_price, 5*N), ncol=5) * paths
  
  # Find top/bottom quantiles
  bound = apply(prices,2, function(x) quantile(x, probs = c(0.025, 0.975)))
  
  
  
  # Create dataframe
  obs = as.vector(observed[(nrow(observed)-19):nrow(observed),])
  tmp = data.frame(Observed = c(obs,
                                rep(NA, 5)),
                   Trend = c(rep(NA,20), last_price * cumprod(1+extra)),
                   Upper = c(rep(NA,20), bound[2,]),
                   Lower = c(rep(NA,20), bound[1,])
                   )
                   
  
  
  # Add known if we have it
  if (!is.na(known)){
    tmp$Actual[21:25] = known[1:5,1]  
  } else {
    tmp$Actual = NA
  }
  
  # Plot
  if (plot==TRUE){
     g = ggplot(tmp, aes(x=1:25, y=Trend)) + geom_line(aes(y=Trend, color='limegreen'), size=2)  + 
      geom_line(aes(y=Observed, color = 'red'), size=2) + 
      geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.3) + 
      geom_point(aes(y=Actual, color='deeppink4')) +
      ylim(0,max(tmp))  + 
      xlim(1,25) + 
      scale_color_manual(values = c('deeppink4', 'red', 'limegreen'),
                                            labels=c("Actual","Projected","Observed")) +
      theme_minimal() + xlab("Days") + ylab("VIX") +
      geom_vline(aes(xintercept=20))
  }
  
  list(DF = tmp, Plot=g)
}

# Does terrible job for market shocks
ModelVol(vix$VIX.Adjusted[1:41], shape1, shape2, known = vix$VIX.Adjusted[42:47])$Plot

# But ok job for calm markets
ModelVol(vix$VIX.Adjusted[60:100], shape1, shape2, known = vix$VIX.Adjusted[101:105])$Plot


# Random sampling data
day_T = sample(40:(nrow(vix)-5),1)

ModelVol(vix$VIX.Adjusted[(day_T-40):day_T], shape1, shape2, 
         known = vix$VIX.Adjusted[(day_T+1):(day_T+5)])$Plot


# The overall performance is not great. But next day prediction is not completely 
# out of whack



################################################################################
# Question 6: Attempts to deal with kurtosis
################################################################################

# Question 6a: Describing VIX returns with skewed normal distribution
# For this we need to install fGarch package

# Get VIX returns
ret = vix_r$VIX.Adjusted
ret = as.vector(ret[!is.na(ret)])

# Fit skewed normal
sn_par = snormFit(ret) # Use built in function to fit skewed distribution
mu = sn_par$par['mean']
sigma = sn_par$par['sd']
xi = sn_par$par['xi'] #Skew

hist(ret, breaks="fd", col='darkgreen', main='Distribution of VIX returns', probability = T)
curve(dsnorm(x, mean=mu, sd=sigma, xi=xi), add=T, col="red")

# Maybe? a better fit. Let's check it with chisq
# Check fit
h = hist(ret, breaks=20, plot = F) # Get histogram object
cnt = h$counts
br  = h$breaks

# Create dataframe for test
test_df = data.frame(Breaks = br[2:length(br)],
                     Obs = cnt)

#Combine observations to get at lest 5
which(test_df$Obs<=5)
cut_off = 7
test_df$Breaks[cut_off] = sum(test_df$Obs[cut_off:nrow(test_df)])
test_df = test_df[1:cut_off,]

# Add expected
N_obs = sum(cnt)

tot = 0 # TO store cumulative probability
for (i in 1:nrow(test_df)){
  if (i!=nrow(test_df)){
    test_df[i,'Exp'] = N_obs * (psnorm(test_df$Breaks[i], mean = mu, sd = sigma, xi=xi) - 
                                  tot)
    tot = psnorm(test_df$Breaks[i], mean = mu, sd = sigma, xi=xi)
  }
  else {
    test_df[i,'Exp'] = N_obs * (1 - tot)
  }
}


# Plot
barplot(rbind(test_df$Obs, test_df$Exp), beside = T, col=c('cornflowerblue', 'pink'))


# Calculate chisq
chisq.val = sum((test_df$Obs - test_df$Exp)^2/test_df$Exp)

# Calculate p-value (estimate three parameters and lose another df due to aggregation)
pval =  pchisq(chisq.val, df = length(cnt)-4, lower.tail = F)
pval


# P-value is very low. So we reject the null that the skewqed normal distribution is a good fit.
# Probably, excessive kurtosis is hwat we have to blame 


# Question 6b: Trick with Gamma distribution
# We know that the return cannot be less than 100% (the VIX index cannot get negative.)
# Therefore, without the loss of generality we recenter the distribution of returns
ret_c = ret + 1 # Move distribution by one
ret_c = as.vector(ret_c)
hist(ret_c, breaks='fd', col='darkgreen', main='Distribution of returns recentered by 100%')

# Now we can fit gamma distribution
pars = fitdist(ret_c, distr = "gamma")
shape = pars$estimate['shape']
rate = pars$estimate['rate']

# Plot histogram and curve
hist(ret_c, breaks='fd', col='darkgreen', 
     main='Distribution of returns recentered by 100%', probability = T)
curve(dgamma(x, shape, rate), col='red', add=T)

# Check fit
h = hist(ret_c, breaks=20, plot = F) # Get histogram object
cnt = h$counts
br  = h$breaks

# Create dataframe for test
test_df = data.frame(Breaks = br[2:length(br)],
                     Obs = cnt)

#Combine observations to get at lest 5
which(test_df$Obs<=5)
cut_off = 7
test_df$Breaks[cut_off] = sum(test_df$Obs[cut_off:nrow(test_df)])
test_df = test_df[1:cut_off,]

# Add expected
N_obs = sum(cnt)

tot = 0 # TO store cumulative probability
for (i in 1:nrow(test_df)){
  if (i!=nrow(test_df)){
    test_df[i,'Exp'] = N_obs * (pgamma(test_df$Breaks[i], shape = shape, rate=rate) - 
                                  tot)
    tot = pgamma(test_df$Breaks[i],  shape = shape, rate=rate)
  }
  else {
    test_df[i,'Exp'] = N_obs * (1 - tot)
  }
}


# Plot
barplot(rbind(test_df$Obs, test_df$Exp), beside = T, col=c('cornflowerblue', 'pink'))


# Calculate chisq
chisq.val = sum((test_df$Obs - test_df$Exp)^2/test_df$Exp)

# Calculate p-value (estimate three parameters and lose another df due to aggregation)
pval =  pchisq(chisq.val, df = length(cnt)-4, lower.tail = F)
pval

# The trick did not work. P-value makes us reject the null. We still cannot deal with the 
# leptokurtosis


# Question 6c: Using BoxCox Transformation
# Box Cox (power) transformation transforms the data as (x^lambda - 1)/x
# The functions come from "EnvStats" package
# The promise of the power transformation is to make data normal-like

lam = boxcox(ret_c, optimize = T)$lambda # Find optimal lambda. We use returns +100% to make
                                        # sure that all data > 0 
bc_ret = boxcoxTransform(ret_c, lam)
mm = mean(bc_ret)
sig = sd(bc_ret)

hist(bc_ret, breaks='fd', probability = T, col='darkgreen', main='BoxCox Transformed')
curve(dnorm(x, mean=mm, sd = sig), add=T, col='red')

# Check fit
descdist(bc_ret)
# We see that it does not help that much with the kurtosis. 

# We can formally check the fit
# Using deciles
nob = length(bc_ret) # Number of observations
dec = qnorm(seq(0.0, 1, by = 0.1), mm, sig) # Deciles
Exp = rep(nob/10,10) # Number Of expected observations per bin


binsim = numeric(10) # Bins
for (i in 1:10){  
  binsim[i] <- sum((bc_ret >= dec[i]) & (bc_ret <= dec[i+1]) )
}



chisq.val = sum((binsim - Exp)^2/Exp)
pval =  pchisq(chisq.val, df = 7, lower.tail = F)
pval

# P-value is very small so we can conclude that normal distribution
# is a bad fit for describing of BoxCox transformed VIX index returns.

# Plot
barplot(rbind(binsim, Exp), beside=T, col = c('deeppink4', 'steelblue4'), 
        main = 'Deciles of Observed(red) and Expected(blue) returns')


## Question 6d: Cauchy distribution
# We try to use Cauchy distribution to describe VIX returns
ret = as.vector(vix_r$VIX.Adjusted)
ret = ret[2:length(ret)]
pars =fitdist(ret, "cauchy")
loc =  pars$estimate['location']
sc = pars$estimate['scale']

# Plotting
hist(ret, breaks="fd", probability = T, col='darkgreen', 
     main='Distribution of VIX returns')
curve(dcauchy(x, loc, sc), add=T, col='red',
      lwd=2)

# This looks promising, at least we can control for leptokurtosis

# Chisq test
# Check fit
h = hist(ret, breaks=20, plot = F) # Get histogram object
cnt = h$counts
br  = h$breaks

# Create dataframe for test
test_df = data.frame(Breaks = br[2:length(br)],
                     Obs = cnt)

#Combine observations to get at lest 5
which(test_df$Obs<=5)
cut_off = 7
test_df$Breaks[cut_off] = sum(test_df$Obs[cut_off:nrow(test_df)])
test_df = test_df[1:cut_off,]

# Add expected
N_obs = sum(cnt)

tot = 0 # TO store cumulative probability
for (i in 1:nrow(test_df)){
  if (i!=nrow(test_df)){
    test_df[i,'Exp'] = N_obs * (pcauchy(test_df$Breaks[i], loc, sc) - 
                                  tot)
    tot = pcauchy(test_df$Breaks[i], loc, sc)
  }
  else {
    test_df[i,'Exp'] = N_obs * (1 - tot)
  }
}


# Plot
barplot(rbind(test_df$Obs, test_df$Exp), beside = T, col=c('cornflowerblue', 'pink'))


# Calculate chisq
chisq.val = sum((test_df$Obs - test_df$Exp)^2/test_df$Exp)

# Calculate p-value (estimate two parameters and lose another df due to aggregation)
pval =  pchisq(chisq.val, df = length(cnt)-3, lower.tail = F)
pval

# Unfortunately, Cauchy distribution does not fit either. We did catch the kurtosis
# measure, but we overestimate tail-risk 



################################################################################
# Question 7: Using historic data to predict future
################################################################################
# Using historic returns to predict future returns
# Instead of trying to describe the distribution of returns, we use
# historic data from train dataset and apply it as a predictor for 
# test dataset

# Divide returns into train and test
spl_ind = max(which(as.yearmon(index(vix_r))<=date_split))
train = vix_r[2:spl_ind,] # Start with 2 to drop first NA
test = vix_r[(spl_ind+1):nrow(vix_r),]
test_vix = vix[(spl_ind+1):nrow(vix_r),]

# Calculate historic expectation and lower and upper percentiles
train_ret = as.vector(train$VIX.Adjusted)
exp_ret = mean(train_ret)
up_bound = quantile(train_ret, probs=c(0.025, (1-0.025)))[2]
low_bound = quantile(train_ret, probs=c(0.025, (1-0.025)))[1]


# Lets put together a versatile function. 
ModelVix = function(observed, exp_ret,  up_bound, 
                    low_bound, plot = TRUE, known = NA){
  
  observed = as.vector(observed[,1])
  
  # Create prediction for next 5 days
  last_pr = observed[length(observed)]
  e_p = last_pr * cumprod(1+rep(exp_ret,5)) # Using expectation
  u_p = last_pr * cumprod(1+rep(up_bound,5)) # Upper bound
  l_p = last_pr * cumprod(1+rep(low_bound,5)) # Lower bound
  
  
  # Create dataframe
  
  tmp = data.frame(Observed = c(observed,
                                rep(NA, 5)),
                   Trend = c(rep(NA,length(observed)), e_p),
                   Upper = c(rep(NA,length(observed)), u_p),
                   Lower = c(rep(NA,length(observed)), l_p)
  )
  
  
  
  # Add known if we have it
  if (!is.na(known)){
    tmp$Actual = c(rep(NA,length(observed)), as.vector(known[1:5,1]))  
  } else {
    tmp$Actual = NA
  }
  
  # Plot
  if (plot==TRUE){
    g = ggplot(tmp, aes(x=1:nrow(tmp), y=Trend)) + geom_line(aes(y=Trend, color='limegreen'), size=2)  + 
      geom_line(aes(y=Observed, color = 'red'), size=2) + 
      geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.3) + 
      geom_point(aes(y=Actual, color='deeppink4')) +
      ylim(0,max(tmp))  + 
      xlim(1,nrow(tmp)) + 
      scale_color_manual(values = c('deeppink4', 'red', 'limegreen'),
                         labels=c("Actual","Projected","Observed")) +
      theme_minimal() + xlab("Days") + ylab("VIX") +
      geom_vline(aes(xintercept=nrow(tmp)-5))
  }
  
  list(DF = tmp, Plot=g)
}


# Random sampling data
day_T = sample(1:(nrow(test_vix)-5),1)

ModelVix(test_vix$VIX.Adjusted[(day_T-40):day_T], exp_ret,  up_bound, 
         low_bound, known = test_vix$VIX.Adjusted[(day_T+1):(day_T+5)])$Plot


# The confidence bound does a great job, but in practice it might be useless 
# because the interval is huge

################################################################################
# Question 8: Using Fourier analysis. 
################################################################################


# Apply Furrier analysis to VIX returns
data = as.numeric(ret)
q <- length(data);q
plot(1:q,data, ylim = c(0,1.5),type = "l")


myCos <- function(m) cos((1:q)*m*2*pi/q)
mySin <- function(m) sin((1:q)*m*2*pi/q)
plot(1:q,data, ylim = c(0,1.5),type = "l")
points(1:q, 100*myCos(1),type = "l", col = "blue")
points(1:q, 100*mySin(1),type = "l", col = "blue")
points(1:q, 100*myCos(20),type = "l", col = "red")

#Currently looks ugly...

coeffA <- function(m){
  sum(data*myCos(m)/(q/2))
}
coeffB <- function(m){
  sum(data*mySin(m)/(q/2))
}

#We compute Fourier Coefficients
FourierA <- sapply(1:50,coeffA)
FourierB <- sapply(1:50,coeffB)
Fourier <- sqrt(FourierA^2+FourierB^2)
Fourier

plot(Fourier)
abline(v=33, col="blue")
abline(v=50, col="blue")
abline(v=43, col="blue")
abline(v=20, col="blue")
abline(v=19, col="blue")
abline(v=41, col="blue")
abline(v=42, col="blue")

#We See That A maximum is at 33, but there is no significant difference in 
# order between values, alluding to the fact that the data is
# most likely not sinusoidal


#Let's try reconstructing with 1 vector
plot(1:q,data, ylim = c(0,1.5),type = "l")
points(1:q,mean(data)+FourierA[33]*myCos(33)+FourierB[33]*mySin(33), type = "l", col = "red")

#What if we use more?
recon <- mean(data)   #this is a_0
for (m in 1:50) {
  recon <- recon + FourierA[m]*myCos(m)+FourierB[m]*mySin(m)
}
plot(1:q,data, ylim = c(0,1.5),type = "l")
points(1:q,recon, type = "l", col = "blue",lwd = 2)

#This doesn't look good, It can thus not be mapped periodically,
#What we conclude from this is that there is a significant amount of noise
#in daily VIX returns, if we look at the levels instead we can
# see a much better equation as \sigma^{2}=\frac{2}{T} \sum_{i} \frac{\Delta K_{i}}{K_{i}^{2}} e^{R T} Q\left(K_{i}\right)-\frac{1}{T}\left[\frac{F}{K_{0}}-1\right]^{2}
#is the formula used to calculate the VIX given by CBOE


# What if we look at VIX Index levels instead of returns
#Let us take a look at the backwardation effect 
data2 = as.numeric(vix$VIX.Close)
q1 <- length(data2);q1
plot(1:q1,data2, ylim = c(0,100),type = "l")


myCos2 <- function(m) cos((1:q1)*m*2*pi/q1)
mySin2 <- function(m) sin((1:q1)*m*2*pi/q1)
plot(1:q1,data2, ylim = c(0,89),type = "l")
points(1:q1, 100*myCos2(1),type = "l", col = "blue")
points(1:q1, 100*mySin2(1),type = "l", col = "blue")
points(1:q1, 100*myCos2(20),type = "l", col = "red")

#Currently looks ugly...
coeffA2 <- function(m){
  sum(data2*myCos(m)/(q1/2))
}
coeffB2 <- function(m){
  sum(data2*mySin(m)/(q1/2))
}

#We compute Fourier Coefficients
FourierA2 <- sapply(1:50,coeffA2)
FourierB2 <- sapply(1:50,coeffB2)
Fourier2 <- sqrt(FourierA2^2+FourierB2^2)
Fourier2

plot(Fourier2)
abline(v=1, col="blue")
abline(v=2, col="blue")
abline(v=4, col="blue")
abline(v=6, col="blue")
abline(v=19, col="blue")
abline(v=13, col="blue")
abline(v=5, col="blue")


#Let's try reconstructing with 1 vector
plot(1:q1,data2, ylim = c(0,151),type = "l")
points(1:q1,mean(data2)+FourierA2[33]*myCos2(33)+FourierB2[33]*mySin2(33), type = "l", col = "red")

#What if we use more?
recon2 <- mean(data2)   #this is a_0
for (m in 1:50) {
  recon2 <- recon2 + FourierA2[m]*myCos2(m)+FourierB2[m]*mySin2(m)
}
plot(1:q1,data2, ylim = c(0,150),type = "l")
points(1:q1,recon2, type = "l", col = "red",lwd = 2)



#This actually looks fairly accurate.

###!!!!!!!!!!!!!!!!!!!!!!
### Rishabh, can you add some conclusions here and into the RMD. File. I will leave
### a placeholder for this. 
### Also, can you clean up the graphs: put a title and axes labels?

# ┌───┬─┐┌─┬────┬───┬───┐┌───┬───┬───┬───┬──┬────┐
# │┌──┴┐└┘┌┤┌┐┌┐│┌─┐│┌─┐││┌─┐│┌─┐│┌──┴┐┌┐├┤├┤┌┐┌┐│
# │└──┐└┐┌┘└┘││└┤└─┘││░││││░└┤└─┘│└──┐││││││└┘││└┘
# │┌──┘┌┘└┐░░││░│┌┐┌┤└─┘│││░┌┤┌┐┌┤┌──┘││││││░░││
# │└──┬┘┌┐└┐░││░│││└┤┌─┐││└─┘│││└┤└──┬┘└┘├┤├┐░││
# └───┴─┘└─┘░└┘░└┘└─┴┘░└┘└───┴┘└─┴───┴───┴──┘░└┘

# Additional work done to target points 8-20. 

## 10. defining and using your own functions.
source("functions.R")
plotColors <- c("#222222", brewer.pal(12, "Paired"))

## 11. Nicely labeled graphics using ggplot, with good use of color, 
## line styles, etc., that tell a convincing story.

# Leveraged ETFs magnify the daily performance of the VIX index. 
# However, this tends to be a double edged sword which in the long
# often leads to serious losses. In the plot below, the VIX index, 
# is compared with the VIXY and the 2x ETF TVIX. 

base.symbol <- "^VIX"
startDate <- "2011-01-04"
endDate <- Sys.Date()
source <- "yahoo"

## load the dji index historical data (+1x, actuals)
xts.base <- loadSeries(base.symbol, source, startDate, endDate) 
type <- "Close" # use closing prices, to mirror the actual ETF daily target
base.name <- "VIX" 

## load the etf's and run comparable simulations
n1x <- loadETF(xts.base, source, "VIXY", type, "n1x")
n2x <- loadETF(xts.base, source, "TVIX", type, "n2x")

## merge all the series into a single xts
xts.merged <- merge(xts.base, n1x$etf, n2x$etf) 

## plot the results
xtsMultiPlot(xts.merged, base.name, plotColors, 
             "VIXY and TVIX")

# The plot clearly shows why ETFs are not used for long-term bets.
# This is due to things such as expenses, daily rebalancing, and volatility.
# These small differences, compounded over time, lead to divergent results
# seen in the plot.


#### Annual Return Comparison

# Let's look more closely at the differences in returns between the
# the two ETFs and the underlying index over longer periods, where
# the compounding effects cause divergences in performance.

n1x.merged <- merge(xts.base, n1x$etf, n2x$etf)
return.mon.col.idx <- grep("Return.Monthly", names(n1x.merged))
tc <- table.CalendarReturns(n1x.merged[,return.mon.col.idx])[, -(1:12)]
tc$VIX_v_VIXY <- round(tc$VIX.Return.Monthly / tc$VIXY.Return.Monthly, digits=2)
tc$VIX_v_TVIX <- round(tc$VIX.Return.Monthly / tc$TVIX.Return.Monthly, digits=2)
colnames(tc)[1:3] = c("VIX", "VIXY", "TVX")
tc

# Leveraged ETF returns for any period can diverge significantly from the target
# leverage multiplier. The table shows there was very significant variation in 
# the returns from year to year. Note, though, that this is due to coincidence 
# more than design, especially due to the existence of a bull market over the same period. 


#### Total Annualized Returns
return.col.index <- grep("Return$", names(n1x.merged))
returns.annual <- Return.annualized(n1x.merged[,return.col.index])

vix.return.annual <- returns.annual[1]
vixy.return.annual <- returns.annual[2]
tvix.return.annual <- returns.annual[3]

round(returns.annual * 100, digits=2)

# We can see that generally, the greater the leverage the higher the losses.

#### Performance Comparison

charts.PerformanceSummary(n1x.merged[,return.col.index],
                          colorset=plotColors, lwd=2, ylog=TRUE)

# The VIXY ETF appears to have done better than the 2x ETF TVIX
# in terms of risk. While the drawdown chart highlights the significant
# potential downsides leveraged ETFs have in general.


#### Performance Statistics Comparison

## 13. Appropriate use of novel statistics 
##(e.g. trimmed mean, maximum or minimum, skewness, ratios).

table.Stats(n1x.merged[,return.col.index])


#### Trailing 36-Month Returns

n1x.box <- chart.Boxplot(n1x.merged[,return.mon.col.idx], colorset=plotColors)


# The VIXY return boxplot reiterates the slightly better average performance 
# compared to the TVIX. Also note that the TVIX has
# large range of return distributions and the very wide confidence interval
# compared to the base index. Again, these indicate the potential risks of
# holding leveraged ETF for longer intervals (in this case, monthly return)                                             intervals).


#### Probability Density (Monthly Returns)

# Finally, lets look at the probability density function for the distribution
# of monthly returns for the underlying index vs. the ETFs.

## convert to dataframe 
df.merged <- as.data.frame(n1x.merged)
df.merged$Date <- as.Date(rownames(df.merged))
df.melted <- melt(df.merged, id.vars=c("Date"))
df.filtered <- df.melted[grep("Return.Monthly", df.melted$variable), ]
ggplot(df.filtered, aes(x=value*100, fill=variable)) +
  geom_density(alpha=0.3, position="identity") + 
  xlab("% Return") + ylab("Probability") + ggtitle("Probability Density - VIX v. VIXY v. TVIX") +
  scale_fill_manual("", values=plotColors)


# This curve represents the likelihood of various levels of monthly returns
# for VIXY compared to the VIX and TVIX, based on all daily returns since the 
# inception of VIXY. As expected, the functions are 'wider' (because of the 
# application of leverage to magnify returns) than the index and have more 
# skew (because of the compounding effects of leverage).

# Notice the very subtle difference between the shape of the TVIX and
# the VIX curves. Again, very small differences in the periodic
# returns compound into signficant differences over longer periods.

### Tracking Error

# While leveraged ETFs are very good at magnifying the daily returns
# of an underlying index, they arent 'perfect'.
# 
# Tracking errors are the differences between the actual and expected
# daily returns.
# 
# Tracking error can be visualized using a scatter plot of
# daily returns of the index vs. ETF. The slope of this relationship
# should equal the leverage factor.

  vixy <- ggplot(xts.merged, aes(x=VIXY.Return, y=VIX.Return)) +
    geom_point(alpha=0.9, color=plotColors[2]) +
    geom_smooth(method=lm, color="white", alpha=0.2) +
    ggtitle("Daily Index Return vs. ETF (VIXY) Return") +
    xlab("VIXY Daily Return") + ylab("VIX Daily Return")

  tvix <- ggplot(xts.merged, aes(x=TVIX.Return, y=VIX.Return)) +
    geom_point(alpha=0.9, color=plotColors[3]) +
    geom_smooth(method=lm, color="white", alpha=0.2) +
    ggtitle("Daily Index Return vs. 2x ETF (TVIX) Return") +
    xlab("2x ETF (TVIX) Return") + ylab("VIX Daily Return")

  grid.arrange(tvix, vixy, ncol=2)


#### Linear Model
  
## 14. Use of linear regression

# Linear models can be constructed from the actual daily returns of the ETFs
# versus the corresponding underlying index.

vixy.fit <- lm(data=xts.merged, VIXY.Return ~ VIX.Return)
vixy.fit

#### Residuals

# Residuals (the difference between the expected and actual value)
# of the daily returns can be visualized to determine if there is a
# discernable pattern in the tracking error of the ETFs.

vixy.resid <- resid(vixy.fit)

vixy <- ggplot(xts.merged, aes(x=VIXY.Return, y=vixy.resid)) + 
  geom_point(alpha=0.9, color=plotColors[2]) +
  geom_smooth(method=lm) + 
  ggtitle("Residuals: VIXY vs. VIX") +
  xlab("VIXY Daily Return") +
  ylab("Variation from Expected")

grid.arrange(vixy)

#### Diagnostics

##19. A graphical display that is different from those in the class scripts.

# A set of diagnostics plots is used to identify outliers in the residuals
# vs. fitted values.

autoplot(vixy.fit, data=as.data.frame(xts.merged),
         colour=plotColors[2], smooth.colour='gray', label.size=3)


# The Q-Q diagnostic plot indicates that the daily returns of the VIXY ETF 
# are not statistically normal. The curves exhibit 'heavy tails'. A T-distribution 
# might be more representative and more work could be done to characterize and 
# understand the distribution of leveraged daily returns.
# 
# The tracking error plots and linear models also identify consistent variances 
# from advertised leverage factors. While the variance is statistically significant, 
# the cause is not known,
# 
# The performance statistics comparison charts highlight some interesting statistical 
# differences between the VIXY and the VIX which could be elaborated on and explored further.

# For the remaining demonstrations, we have used the dataset VIX and there are variables 
# VIX.Open,	VIX.High, VIX.Low, VIX.Close, VIX.Volume and VIX.Adjusted.

able is changed and reduced compare to full data because very low and very high values are removed from the data.




##15. Calculation and display of a logistic regression curve.
##16. Appropriate use of covariance or correlation.
##17. Use of theoretical knowledge of chi-square, gamma, or beta distributions. 

Since we do not have any binary or categorical variable in the dataset, we first 
created a binary variable for Adjusted and Close. We created a new variable which 
divides the existing variable Adjusted to two categories such as greater than 
value 50 and less or equal to value 50. Similarly, we have created a new variable 
for Close by categorize this into two category such as greater than 30 and less 
than or equal to 30. 

For two categorical variables we will also use Chi-Square test to test the association 
between these variables. Chi-Square test is used to test the association between two 
categorical variables.

Similarly, if we have continuous variable, then we are using correlation analysis 
to find out any linear relationship between these variables.

The logistic regression is a technique to predict the dependent variable based on 
independent variable. Here the dependent variable is binary instead of continuous 
variable like in linear regression. First we tried to fit the logistic model with 
all independent variable and then we fitted logistic model considering only one 
variable which is Close (new categorical variable) to find out the model.
R Code and Output is given below:
  
  #contigency table 
  library(MASS)
vix <- read.csv(file = './data/vix.csv')
vix$adjbin <- ifelse(vix$VIX.Adjusted<=50, 0,ifelse(vix$VIX.Adjusted>50,1,99))
vix$closbin <- ifelse(vix$VIX.Close<=30, 0, ifelse(vix$VIX.Close>30,1,99))
sapply(vix,class)
table(vix$adjbin)
prop.table(table(vix$adjbin))
table(vix$closbin)
prop.table(table(vix$closbin ))
tab1<-table (vix$adjbin, vix$closbin )
tab1p<-prop.table(table (vix$adjbin, vix$closbin ))
rowSums(tab1)
colSums(tab1)
chisq.test(vix$adjbin, vix$closbin )



# Comments:

We can see that the chi-square approximates zero, it means that there is a statistically 
significant association between these two categorical variables. 

##Correlation analysis:

my_data <- vix[, c(2,3,4,5)]
head(my_data, 6)
res <- cor(my_data)
round(res, 2)

#Comments:

We can see from the correlation coefficient that there is a positive and strong 
relationship between all variables as the values of the correlation coefficients 
are close to 1.


##Logistic Regression:
#q15 logistic
```{R, include=TRUE}
#vix$adjbin #0,1 binary number
plot(vix$VIX.High, jitter(vix$adjbin, 0.15), pch=19)
logreg<-glm(vix$adjbin ~ vix$VIX.Open+vix$VIX.High+vix$VIX.Low+vix$VIX.Close, binomial, data = vix)
summary(logreg)
logreg$coef
b0 <- logreg$coef[1] # intercept
X1 <- logreg$coef[2]
X2 <- -logreg$coef[3]
X3 <- logreg$coef[4]
X4 <- logreg$coef[5]
plot(logreg)
```

Call:
  glm(formula = vix$adjbin ~ vix$VIX.Open + vix$VIX.High + vix$VIX.Low + 
        vix$VIX.Close, family = binomial, data = vix)

Deviance Residuals: 
  Min          1Q      Median          3Q         Max  
-1.674e-03  -2.000e-08  -2.000e-08  -2.000e-08   2.278e-03  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)   -1552.330  32662.947  -0.048    0.962
vix$VIX.Open     -3.122    680.567  -0.005    0.996
vix$VIX.High      3.272    880.989   0.004    0.997
vix$VIX.Low      -1.958    787.947  -0.002    0.998
vix$VIX.Close    32.603    847.872   0.038    0.969

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 7.1107e+02  on 3359  degrees of freedom
Residual deviance: 1.1196e-05  on 3355  degrees of freedom
AIC: 10

Number of Fisher Scoring iterations: 25


#Comments:

We can see from the logistic regression output that the model is not statistically significant as none of the independent variable is statistically significant at 5% level of significance. 

##18. Use of theoretical knowledge of sampling distributions.
##20. Calculation of a confidence interval.

Confidence intervals are very useful technique to find out the range of the data can take. It is useful when we want to test the significance of one or more variables in terms of their statistical significance. It gives us the confidence that we can depict with certain amount of certainty to say something about the variable of interest. Here we want to find out the confidence interval of mean for variable Adjusted to see what this value depicts. We are using 95% confidence interval; it will tell us that mean of this variable will lie in this confidence interval 95% of time even if we are considering the different sample. Considering the samples from the population, the technique we are using is known as sampling distribution. It means taking samples from populations again and again.
R Code and Output is given below:
  ```{R, include=TRUE}
summary.data.frame(vix)
mean(vix$VIX.Adjusted)
sd(vix$VIX.Adjusted)
n<-length(vix$VIX.Adjusted)
n
error <- qt(0.975,df=n-1)*sd(vix$VIX.Adjusted)/sqrt(n)
error
left <- mean(vix$VIX.Adjusted)-error
right <- mean(vix$VIX.Adjusted)+error
CI<-c(left, right)
CI
```

#Comments:

We can see from the 95% confidence interval that the mean of this variable which is Adjusted will lie between (19.31546, 19.98037) 95% of time even if we are considering the different sample




The function corrplot(), in the package of the same name, creates a graphical display of a correlation matrix, highlighting the most correlated variables in a data table. In this plot, correlation coefficients are colored according to the value. Correlation matrix can be also reordered according to the degree of association between variables.
The function corrplot() takes the correlation matrix as the first argument. The second argument (type=“upper”) is used to display only the upper triangular of the correlation matrix.
The function chart.Correlation()[ in the package PerformanceAnalytics], can be used to display a chart of a correlation matrix.


library(corrplot)

corrplot(res, type = "upper", order = "hclust", tl.col = "black")

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(my_data, histogram=TRUE, pch=19)
