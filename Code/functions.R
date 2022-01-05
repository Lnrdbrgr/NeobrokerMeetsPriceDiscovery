

## function to compute the VECM, returns a list with the empricial cointegrating vector,
## the model selection parameters and the data with laged values and differences
## needs data as shown in the format above, i.e. date, LuS quote, Xetra quote
vecm_calculations <- function(data, company_name){
  
  ##### Add Differences and lagged variables to dataframe #####
  
  ## add first lag
  data$LuS_t1 <- c(data$LuS[2:nrow(data)], NA)
  data$Xetra_t1 <- c(data$Xetra[2:nrow(data)], NA)
  
  ## differences
  data$LuS_delta_t <- -1*c(diff(data$LuS), NA)
  data$Xetra_delta_t <- -1*c(diff(data$Xetra), NA)
  
  ## differences lag 1
  data$LuS_delta_t1 <- -1*c(data$LuS_delta_t[2:nrow(data)], NA)
  data$Xetra_delta_t1 <- -1*c(data$Xetra_delta_t[2:nrow(data)], NA)
  
  ## differences lag 2
  data$LuS_delta_t2 <- -1*c(data$LuS_delta_t[3:nrow(data)], NA, NA)
  data$Xetra_delta_t2 <- -1*c(data$Xetra_delta_t[3:nrow(data)], NA, NA)
  
  ## differences lag 3
  data$LuS_delta_t3 <- -1*c(data$LuS_delta_t[4:nrow(data)], NA, NA, NA)
  data$Xetra_delta_t3 <- -1*c(data$Xetra_delta_t[4:nrow(data)], NA, NA, NA)
  
  
  ## omit the last observations that have NA
  data <- na.omit(data)
  
  
  
  
  ##### Check theoretical Cointegrating Vector #####
  
  ## regression of prices on each other
  reslm <- lm(LuS ~ Xetra, data = data)
  
  
  ## check coefficients, to follow the theoretical cointegrated vector,
  ## coefficient should be (close to) 1
  empirical_cointegrating_vector <- reslm$coefficients[2]
  
  ## add residuals of (theoretical) cointegrating vector of lag 1 (this would be beta*p_t-1)
  data$cv_res_lag1 <- data$LuS_t1 - data$Xetra_t1
  
  
  
  ##### Equation-by-Equation estimation of VECM #####
  
  ## regression for L&S as left-hand-side variable
  
  # 2 lags
  res_LS_2lag <- lm(LuS_delta_t ~ 
                      cv_res_lag1 +
                      LuS_delta_t1 + Xetra_delta_t1,
                    data = data)
  
  # 3 lags
  res_LS_3lag <- lm(LuS_delta_t ~ 
                      cv_res_lag1 +
                      LuS_delta_t1 + Xetra_delta_t1 +
                      LuS_delta_t2 + Xetra_delta_t2,
                    data = data)
  
  # 4 lags
  res_LS_4lag <- lm(LuS_delta_t ~ 
                      cv_res_lag1 +
                      LuS_delta_t1 + Xetra_delta_t1 +
                      LuS_delta_t2 + Xetra_delta_t2 +
                      LuS_delta_t3 + Xetra_delta_t3,
                    data = data)
  
  
  
  ## regression for Xetra as left-hand-side variable
  
  # 2 lags
  res_Xetra_2lag <- lm(Xetra_delta_t ~ 
                         cv_res_lag1 +
                         LuS_delta_t1 + Xetra_delta_t1,
                       data = data)
  
  # 3 lags
  res_Xetra_3lag <- lm(Xetra_delta_t ~ 
                         cv_res_lag1 +
                         LuS_delta_t1 + Xetra_delta_t1 +
                         LuS_delta_t2 + Xetra_delta_t2,
                       data = data)
  
  # 4 lags
  res_Xetra_4lag <- lm(Xetra_delta_t ~ 
                         cv_res_lag1 +
                         LuS_delta_t1 + Xetra_delta_t1 +
                         LuS_delta_t2 + Xetra_delta_t2 +
                         LuS_delta_t3 + Xetra_delta_t3,
                       data = data)
  
  
  
  ## get AIC and BIC to check which models perform best
  model_selection <- data.frame("company" = company_name,
                                "AIC_LuS_lag2" = stats::AIC(res_LS_2lag),
                                "AIC_LuS_lag3" = stats::AIC(res_LS_3lag),
                                "AIC_LuS_lag4" = stats::AIC(res_LS_4lag),
                                "AIC_Xetra_lag2" = stats::AIC(res_Xetra_2lag),
                                "AIC_Xetra_lag3" = stats::AIC(res_Xetra_3lag),
                                "AIC_Xetra_lag4" = stats::AIC(res_Xetra_4lag),
                                "BIC_LuS_lag2" = stats::BIC(res_LS_2lag),
                                "BIC_LuS_lag3" = stats::BIC(res_LS_3lag),
                                "BIC_LuS_lag4" = stats::BIC(res_LS_4lag),
                                "BIC_Xetra_lag2" = stats::BIC(res_Xetra_2lag),
                                "BIC_Xetra_lag3" = stats::BIC(res_Xetra_3lag),
                                "BIC_Xetra_lag4" = stats::BIC(res_Xetra_4lag),
                                "Lag_min_AIC_LuS" = numeric(1),
                                "Lag_min_AIC_Xetra" = numeric(1),
                                "Lag_min_BIC_LuS" = numeric(1),
                                "Lag_min_BIC_Xetra" = numeric(1), stringsAsFactors = FALSE)
  
  ## get smallest AIC and BIC
  model_selection$Lag_min_AIC_LuS <- which.min(model_selection[1, 2:4])
  model_selection$Lag_min_AIC_Xetra <- which.min(model_selection[1, 5:7])
  model_selection$Lag_min_BIC_LuS <- which.min(model_selection[1, 8:10])
  model_selection$Lag_min_BIC_Xetra <- which.min(model_selection[1, 11:13])
  
  
  ## make list to return
  return_list <- list("empirical_cointegrating_vector" = empirical_cointegrating_vector,
                      "model_selection" = model_selection,
                      "data" = data)
  
}


hasbrouck <- function(data, lag){
  
  ## estimate VECM again given the best lag
  if (lag == 2){
    reslm_LuS <- lm(LuS_delta_t ~ cv_res_lag1 +
                          LuS_delta_t1 + Xetra_delta_t1, data = data)
    reslm_Xetra <- lm(Xetra_delta_t ~  cv_res_lag1 + 
                          LuS_delta_t1 + Xetra_delta_t1, data = data)
  } else if (lag == 3){
    reslm_LuS <- lm(LuS_delta_t ~ cv_res_lag1 +
                          LuS_delta_t1 + Xetra_delta_t1 +
                          LuS_delta_t2 + Xetra_delta_t2, data = data)
    reslm_Xetra <- lm(Xetra_delta_t ~  cv_res_lag1 + 
                          LuS_delta_t1 + Xetra_delta_t1 +
                          LuS_delta_t2 + Xetra_delta_t2, data = data)
  } else if (lag == 4){
    reslm_LuS <- lm(LuS_delta_t ~ cv_res_lag1 +
                          LuS_delta_t1 + Xetra_delta_t1 +
                          LuS_delta_t2 + Xetra_delta_t2 +
                          LuS_delta_t3 + Xetra_delta_t3, data = data)
    reslm_Xetra <- lm(Xetra_delta_t ~  cv_res_lag1 + 
                          LuS_delta_t1 + Xetra_delta_t1 +
                          LuS_delta_t2 + Xetra_delta_t2 +
                          LuS_delta_t3 + Xetra_delta_t3, data = data)
  }
    
    
  
  ## Compute big XI Matrix
  # get beta vector and its orthogonal complement
  beta <- matrix(c(1, -1),
                 ncol = 2, byrow = TRUE)
  beta_orth <- MASS::Null(t(beta))
  
  # get alpha vector and its orthogonal complement
  alpha <- t(c(reslm_LuS$coefficients[2], reslm_Xetra$coefficients[2]))
  alpha_orth <- MASS::Null(t(alpha))
  
  ## get GAMMA coefficients matrices
  GAMMA_1 <- matrix(c(reslm_LuS$coefficients[3:4], 
                      reslm_Xetra$coefficients[3:4]),
                    nrow = 2, ncol = 2, byrow = TRUE)
  
  GAMMA_2 <- matrix(c(reslm_LuS$coefficients[5:6], 
                      reslm_Xetra$coefficients[5:6]),
                    nrow = 2, ncol = 2, byrow = TRUE)
  
  GAMMA_3 <- matrix(c(reslm_LuS$coefficients[7:8], 
                      reslm_Xetra$coefficients[5:6]),
                    nrow = 2, ncol = 2, byrow = TRUE)
  
  ## get sum of gamma matrices
  if (lag == 2){
    sum_gamma <- GAMMA_1
  } else if (lag == 3){
    sum_gamma <- GAMMA_1 + GAMMA_2
  } else if (lag == 4){
    sum_gamma <- GAMMA_1 + GAMMA_2 + GAMMA_3
  }
  
  
  ## compute XI-matrix
  XI <- beta_orth %*% solve(t(alpha_orth) %*% (diag(2) - sum_gamma) %*% beta_orth) %*% t(alpha_orth)
  
  
  ## extract XI vector
  xi <- XI[1,]
  
  
  ## Compute (Co-)Variance Matrix of the Residuals
  # for LS first
  SIGMA_u_LS <- cov(cbind(reslm_LuS$residuals, reslm_Xetra$residuals))
  # for Xerta first
  SIGMA_u_Xetra <- cov(cbind(reslm_Xetra$residuals, reslm_LuS$residuals))
  
  
  ## use cholesky decomposition to obtain C
  C_LS <- t(chol(SIGMA_u_LS))
  C_Xetra <- t(chol(SIGMA_u_Xetra))
  
  ## compute Hasbrouck Information Share
  IS_LS_upper <- as.vector((xi %*% C_LS)^2) / c((t(xi) %*% (C_LS%*%t(C_LS)) %*% xi))
  IS_LS_lower <- as.vector((xi %*% C_Xetra)^2) / c((t(xi) %*% (C_Xetra%*%t(C_Xetra)) %*% xi))
  
  
  ## return stuff
  return_list <- list(LS_IS_upper = IS_LS_upper[1],
                      LS_IS_lower = IS_LS_lower[2],
                      XI = XI,
                      model_LuS = reslm_LuS,
                      model_Xetra = reslm_Xetra)
  
  return(return_list)
  
}


## function to compute log-likelihood value
loglike <- function(parameter){
  
  # define W and PSI matrix
  W <- matrix(c(parameter[1], parameter[2], parameter[3], parameter[4]), byrow = TRUE, ncol = 2)
  PSI <- matrix(c(parameter[5], 0, 0, parameter[6]), byrow = TRUE, ncol = 2)
  
  
  # initialize log-likelihood value
  L <- numeric(ncol(u))
  
  # compute function for all pairs of residuals, take log and multiply by -1 so later we can minimize
  for (i in 1:ncol(u)){
    
    l <- -1*log(
      parameter[7] * (2*pi)^(-2/2) * det(W)^(-1) * exp((t(u[,i]) %*% (solve((W%*%t(W)))) %*% u[,i])/-2)
      + (1-parameter[7]) * (2*pi)^(-2/2) * det(PSI)^(-0.5) * det(W)^(-1) * exp((t(u[,i]) %*% (solve((W%*%PSI%*%t(W)))) %*% u[,i])/-2)
    )
    
    # store overall value
    L[i] <- l
    
  }
  
  # return log-likelihood value
  return(sum(L))
}









