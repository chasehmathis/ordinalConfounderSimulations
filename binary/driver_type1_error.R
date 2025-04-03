# DRIVER CLEAN NO COMMENTS

# T - > Y ; A -> Y; B -> Y; C -> Y
args <- commandArgs(trailingOnly = TRUE)
seed = as.numeric(args[1])
nsim <- as.numeric(args[2])
n <- as.numeric(args[3])
if(is.na(seed)){
  seed <- 1
}
set.seed(seed)
options(warn = -1)
library(tidyverse)
library(rje)
library(cmdstanr)
library(svMisc)
library(caret)
library(psych)
library(ordinal)
library(MASS)
# Load the necessary library
options(mc.cores = parallel::detectCores())
source("../funs.R")

# USE GPU 
SIMS <- matrix(ncol = 19, nrow = nsim)
colnames(SIMS) <- c("ATEBoot00", "ATEBoot01", "ATENaive", "RegressionTrue", "P2SLS",
                    "ATEBoot10", "ATEBoot11", "ATEBoot12", "PartialCorrTYU", "confounding", "ATEBoot00pval", "ATEBoot01pval",
                    "ATENaivepval", "ATEBoot10pval", "ATEBoot11pval", "ATEBoot12pval", "ATE2pval", "ground_truthpval", "bTY")

for(i in 1:nsim){
  
  progress(i,nsim)
  confounding_high <- sample(c(0,1), 1)
  dtt <- sample_params('d')
  dyy <- sample_params('d')
  duu <- sample_params('d')
  dzz <- sample_params('d')
  dww <- sample_params('d')
  
  bUY <- sample_params('b', confounding_high)
  bUT <- sample_params('b', confounding_high)
  bTY <- sample_params('b'); bTY <- 0
  bUZ <- sample_params('b')
  bUW <- sample_params('b')
  bZT <- sample_params('b')
  bWY <- sample_params('b');
  uFam <- sample(c(1:5), 1)
  uFam <- 1
  sUU <- sqrt(duu)
  links <- list("logit", "logit")
  
  params <- list(dtt, dyy,duu,dzz, dww,bUY, bTY, bUT, bUZ, bUW, bZT, bWY,uFam, links, n)
  
  # generate our data
  
  data <- generate_data(params)
  # partial correlation T, Y \mid U
  # Calculate partial correlation T, Y conditional on U
  partial_corr <- partial.r(data, c("T", "Y"), c("U"))[1,2]
  print(partial_corr)
  
  
  data['U_obs_scaled'] <- scale(data['U_obs']) * sUU
  
  
  N <- n
  C <- 5# length(unique(data$U_obs)) # Use U_obs instead of Y
  mod <- summary(clm(as.factor(U_obs) ~ Tstar, data = data))
  pars <- mod$coefficients
  beta <- pars[nrow(pars),1]
  g <- pars[1:(nrow(pars)-1),1]
  
  
  
  ez <- data$Tstar * beta
  z <- rep(0, length(ez))
  y <- data$U_obs; # make this 1,2,3,4 if i have values like 5, 7, 8, 10 just give me the order
  y <- as.numeric(as.factor(y))
  for (j in seq_along(ez)) {
    a <- max(-Inf, g[y[j] - 1], na.rm = TRUE)
    b <- min(Inf, g[y[j]], na.rm = TRUE)
    u <- runif(1, pnorm(a - ez[j]), pnorm(b - ez[j]))
    if(u == 1)
    {z[j] <- max(z)}
    else if(u ==0)
    {z[j] <- min(z)}
    else
    {z[j] <- ez[j] + qnorm(u)}
  }
  
  
  mod <- summary(clm(as.factor(U_obs) ~ Ystar, data = data))
  pars <- mod$coefficients
  beta <- pars[nrow(pars),1]
  g <- pars[1:(nrow(pars)-1),1]
  
  ez <- data$Ystar * beta
  z_1 <- rep(0, length(ez))
  for (j in seq_along(ez)) {
    a <- max(-Inf, g[y[j] - 1], na.rm = TRUE)
    b <- min(Inf, g[y[j]], na.rm = TRUE)
    u <- runif(1, pnorm(a - ez[j]), pnorm(b - ez[j]))
    if(u == 1)
    {z_1[j] <- max(z_1)}
    else if(u ==0)
    {z_1[j] <- min(z_1)}
    else
    {z_1[j] <- ez[j] + qnorm(u)}
  }
  
  data[,"z"] <- z
  data[,"z_1"] <- z_1
  #codec(data$z, data$U, cbind(data$T, data$U_obs))
  

  folds <- createFolds(data$Y, 1, returnTrain= FALSE)
  
  for(fold in folds){
    sub_data <- data[-fold,]
    if(dim(sub_data)[1] == 0){
      sub_data <- data
    }
    
    # only one proxy
    what_mod0 <- lm(z_1 ~ z + T + U_obs + Y, sub_data)
    coefs <- what_mod0$coefficients
    what00 <- as.matrix(cbind(1,data[fold,c("z", "T", "U_obs")],1)) %*% coefs
    data[fold, 'what00'] <- what00
    
  
  }
  # only one proxy
  mod <- glm(Y ~ what00 + T, data, family = binomial())
  ATEBayesBootstrap <- coef(mod)['T']
  ATEBayesBootstrappval <- summary(mod)$coefficients['T', 'Pr(>|z|)']

  
  mod <- glm(Y ~ factor(U_obs, ordered = TRUE) + T, data, family = binomial())
  ATENaive <- coef(mod)['T']
  ATENaivepval <- summary(mod)$coefficients['T', 'Pr(>|z|)']
  
  ground_truth <-  coef(glm(Y ~ T + U, data = data, family = binomial))['T']
  ground_truthpval <- summary(glm(Y ~ T + U, data = data, family = binomial))$coefficients['T', 'Pr(>|z|)']
  
  SIMS[i, "ATENaive"] <- ATENaive; 
  SIMS[i, "ATEBayesBootstrap"] <- ATEBayesBootstrap; 
  SIMS[i, "RegressionTrue"] <- ground_truth; 
  SIMS[i, "PartialCorrTYU"] <- partial_corr
  SIMS[i, "confounding"] <- confounding_high;
  SIMS[i, "ATEBayesBootstrappval"] <- ATEBayesBootstrappval; 
  SIMS[i, "ATENaivepval"] <- ATENaivepval; 
  SIMS[i, "ground_truthpval"] <- ground_truthpval
  SIMS[i,"bTY"] <- bTY
  
  if(i %% 100 == 0 || i == 10){
    data.frame(SIMS) |> 

    data.frame(SIMS) |> 
      write_csv(paste0("SIMStype1error/SIMS", "-", n, "-", seed, c, ".csv"))
  }
}
