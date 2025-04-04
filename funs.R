# HELPER FUNCTIONS FOR SIMULATIONS

sample_params <- function(type, small){

    if(type == "d"){
        return(sample_var())
    }
    else if (type == 'b'){
        return(sample_coef(small))
    }
    else if (type == 'n'){
        return(sample_size())
    }
    else if (type == 'f'){
      return(sample(1:7, 1))
    }
}


sample_pars <- function(family, idx){
  # idx is one it is marginal
  # idx is two it is conditional on hidden var
  # idx is three it is p(y | do(T))
  

  if(idx == 1){

    if(family == 1){
      return(list(sample_params('b'), sample_params('d')))
    }
    if(family == 2){
      return(list(sample_params('b'), sample_params('d'), 10))
    }
    if(family == 3){
      return(list(sample_params('b')^2, sample_params('d')))
    }
    if(family == 4){
      return(list(sample_params('d'), sample_params('d')))
    }
    if(family == 5){
      return(list(sample_params('d'), sample(seq(0,1, length.out = 100), 1)))
    }
    if(family == 6){
      return(list(sample_params('b'), sample_params('d')))
    }
  }else if(idx == 2 || idx == 3){
    if(family == 1){
      return(list(0, sample_params('b'), sample_params('d')))
    }
    if(family == 2){
      return(list(sample_params('b'), sample_params('d'), 10))
    }
    if(family == 3){
      return(list(sample_params('b')^2, sample_params('d')))
    }
    if(family == 4){
      return(list(sample_params('d'), sample_params('d')))
    }
    if(family == 5){
      return(list(sample_params('d'), sample(seq(0,1, length.out = 100), 1)))
    }
    if(family == 6){
      return(list(sample_params('b'), sample_params('d')))
    }
  }
  stop("idx must be in [1,2,3]")
}

sample_var <- function(){
  sample(seq(1,10, length.out = 100), 1)
}



sample_coef <- function(small){
  if(missing(small)){
    x <- seq(-5, 5, length.out = 1000)
  }else{
    if(small){
      x <- c(seq(-5,-1, length.out = 1000/2), seq(1,5, length.out = 1000/2))
    }else{
      x <- seq(-1,1, length.out = 1000)
    }
  }

    
    x <- x[which(x != 0)]
    res <- sample(x,1)
    return(res)
}
sample_size <- function(){
    return(sample(c(1e2,1e3,1e4), 1))
}



generate_data <- function(params) {
  dtt <- params[[1]]; dyy <- params[[2]]; duu <- params[[3]];
  dzz <- params[[4]]; dww <- params[[5]];
  bUY <- params[[6]]; bTY <- params[[7]]; bUT <- params[[8]];
  bUZ <- params[[9]]; bUW <- params[[10]]; bZT <- params[[11]]; bWY <- params[[12]]
  uFam <- params[[13]]; links <- params[[14]]; n <- params[[15]];
                
  if(uFam == 1){
    # normal
    U <- rnorm(n, 0, sqrt(duu))
  }else if(uFam == 2){
    # t distribution
    U <- rt(n, 15) * sqrt(duu)
  }else if(uFam == 3){
    # gamma
    U <- rgamma(n, duu, duu)
  }else if(uFam == 4){
    # beta
    U <- rbeta(n, duu, duu)
  }else if(uFam == 5){
    # log normal
    U <- rlnorm(n, 0, sqrt(duu))
  }

  Z <- U * bUZ + rnorm(n, 0,sqrt(dzz))
  U_obs <- cut_fun(U)
  res_data <- data.frame(U_obs, U, Z)
  if(links[1] == "logit"){
    Tstar <- cbind(U, Z) %*% c(bUT, bZT) + rlogis(n, 0, 1)
    T <- 1*(Tstar > 0)
    res_data <- cbind(res_data, Tstar)
  }else{
    T <- cbind(U, Z) %*% c(bUT, bZT) + rnorm(n, 0, sqrt(dtt))
  }
  res_data <- cbind(res_data, T)
  W <- U * bUW + rnorm(n, 0, sqrt(dww))
  res_data <- cbind(res_data, W)
  if(links[2] == "logit"){
    Ystar <- cbind(U, T, W) %*% c(bUY, bTY, bWY) + rlogis(n, 0,1)
    res_data <- cbind(res_data, Ystar)
    Y <- 1*(Ystar > 0)
  }else{
    Y <- cbind(U, T, W) %*% c(bUY, bTY, bWY) + dyy
  }
  res_data <- cbind(res_data, Y)
  #as.integer(as.character(B_obs2))

  
  return(res_data)
}


# cut_fun <- function(U){
# 
#     i <- sample(0:1,1)
#     i <- 1
#     if(i == 0){
#         probs <- rje::expit(U)
#         U_obs <- rbinom(n, 4, probs) + 1
#     }
#     else if(i == 1){
#         breaks <- sort(runif(4, min = min(U), max = max(U)))
#         breaks <- c(min(U), breaks, max(U))
#         
#         U_obs <-cut(U, breaks, include.lowest = TRUE)
#     }
#     return(as.numeric(U_obs))
# }
cut_fun <- function(U){
  # simulate ordinal data using ordinal regression framewor
  n <- length(U)
  betaU <- sample_coef()
  linear_predictor <- betaU * U
  # four evenly split up intercepts
  intercepts <- seq(min(U), max(U), length = 6) # need the intercepts to be in the middle of the bins
  intercepts <- intercepts[-c(1,6)]

  cum_probs <- sapply(intercepts, function(intercept) {
    plogis(intercept + linear_predictor)
  })

  response <- apply(cum_probs, 1, function(probs) {
    sum(runif(1) > probs) + 1
  })

  return(response)
}
# multinomial not ordinal
# cut_fun <- function(U){
#   n <- length(U)
#   K <- 5  
#   beta <- sapply(1:K, function(x) sample_coef())
#   U_star <- matrix(0, n, K)
#   for(k in 1:K){
#     epsilon_k <- rexp(n)- rexp(n)
#     U_star[,k] <- U * beta[k] + epsilon_k
#   }
#   U_obs = apply(U_star, 1, which.max)
#   return(U_obs)
# }



get_beta <- function(fits, data, duu){

    fit1 <- fits[[1]]; fit2 <- fits[[2]]; fit3 <- fits[[3]]
    #
    #sigmaUU_dotT<- duu - 0.5;
    #sigmaUU_dotY <- duu - 0.5;
    #params <- fit1$summary() |> 
    #  pull(mean)
    #beta_hat_1 <- params[2:3]
    #alpha_hat_1 <- params[4]
    #fitted1 <- matrix(cbind(data$T, data$Y), ncol = 2) %*% beta_hat_1 + alpha_hat_1
    #
    #beta_hat_1 <- (beta_hat_1 / sd(fitted1)) * sqrt(duu)
    
    params <- fit2$summary() |> 
      pull(mean)
    beta_hat_2 <- params[2]
    alpha_hat_2 <- params[3]
    # fitted2 <- data$T * beta_hat_2 + alpha_hat_2
    # beta_hat_2 <- (beta_hat_2 / sd(fitted2)) * sigmaUU_dotT

    # params <- fit3$summary() |> 
    #   pull(mean)
    # beta_hat_3 <- params[2]
    # alpha_hat_3 <- params[3]
    # # fitted3 <- data$Y * beta_hat_3 + alpha_hat_3
    # # beta_hat_3 <- (beta_hat_3 / sd(fitted3)) * sigmaUU_dotY
    
    # return(list(beta_hat_1, beta_hat_2, beta_hat_3))
    return(list(beta_hat_2))

}



est_sigmau <- function(beta_hats, data){
    Y <- data$Y; T <- data$T; 
    COV <- cov(cbind(T, Y))
    beta_hat_1 <- beta_hats[[1]]
    beta_hat_2 <- beta_hats[[2]]
    beta_hat_3 <- beta_hats[[3]]

    alpha =beta_hat_1[1]
    beta = beta_hat_1[2]

    k1 = cov(Y, T) / var(Y)
    k2 = schur(COV, 1, 1, 2)
    k3 = schur(COV, 2, 2, 1)
    k4 = cov(Y,T) / var(T)
    sigmaUY1 = (beta * k3 + k4 * k2 * alpha) / (1 - k1 * k4)
    sigmaUT1 = alpha * k2 + k1 * sigmaUY1

    # sigmaUT2 = beta_hat_2 * var(T)
    # sigmaUY2 = beta_hat_3 * var(Y)
# 
    # sigmaUT <- mean(c(sigmaUT1, sigmaUT2))
    # sigmaUY <- mean(c(sigmaUY1, sigmaUY2))

    return(list(sigmaUT1, sigmaUY1))
}






get_COVHat <- function(sigma_U, data, duu){
    COV <- cov(cbind(data$T, data$Y))
    sigmaUT <- sigma_U[[1]]; sigmaUY <- sigma_U[[2]]
    COVHat <- matrix(0, nrow = 3, ncol = 3)
    colnames(COVHat) <- c("T", "Y", "U")
    rownames(COVHat) <-  c("T", "Y", "U")
    # THIS DOES NOT Insure Positive Definiteness
    COVHat[1:2, 1:2] <- COV
    COVHat[1:2, 3] <- c(sigmaUT, sigmaUY)
    COVHat[lower.tri(COVHat)] = COVHat[upper.tri(COVHat)]
    # THIS SHOULD BE 
    COVHat[3,3] <- duu
    
    return(COVHat)
}

cpvalue <- function(Y, Z, X, bootsims = 1000) {
  n <- length(Y)
  
  vals <- sapply(seq_len(bootsims), function(i) {
    ids <- sample(n, n, replace = TRUE)
    Y_sub <- Y[ids]
    Z_sub <- Z[ids]
    X_sub <- X[ids, , drop = FALSE]  #// Ensure X_sub remains a matrix
    codec(Y_sub, Z_sub, X_sub)
  })
  
  return(mean(vals < 0))
}


