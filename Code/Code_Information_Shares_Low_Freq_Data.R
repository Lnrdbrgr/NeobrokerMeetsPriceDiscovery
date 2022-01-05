
###############################################################################
########### Estimation of Information Shares on Low Frequency Data ############
###############################################################################



#### Preparation ####
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("MASS")) install.packages("MASS")
if (!require("optimization")) install.packages("optimization")
if (!require("stats")) install.packages("stats")

## include the functions, needs according working directory
source("functions.R")



## get all stocks for which data is available
## needs the working directory to be inside the file path with low frequency data
stocks <- list.files() %>%
  strsplit("_") %>%
  unlist() %>%
  unique()
stocks <- stocks[!stocks %in% c("LuS.csv", "Xetra.csv")]


## define cut-off date
cutoff <- "2019-12-31"


## create empty dataframes to store in-between results
no_stocks <- length(stocks)
emp_cointegrating_vectors <- data.frame("comp" = character(no_stocks),
                                        "coint_v" = numeric(no_stocks), stringsAsFactors = FALSE)
model_selection <- data.frame("company" = character(no_stocks),
                              "AIC_LuS_lag2" = numeric(no_stocks),
                              "AIC_LuS_lag3" = numeric(no_stocks),
                              "AIC_LuS_lag4" = numeric(no_stocks),
                              "AIC_Xetra_lag2" = numeric(no_stocks),
                              "AIC_Xetra_lag3" = numeric(no_stocks),
                              "AIC_Xetra_lag4" = numeric(no_stocks),
                              "BIC_LuS_lag2" = numeric(no_stocks),
                              "BIC_LuS_lag3" = numeric(no_stocks),
                              "BIC_LuS_lag4" = numeric(no_stocks),
                              "BIC_Xetra_lag2" = numeric(no_stocks),
                              "BIC_Xetra_lag3" = numeric(no_stocks),
                              "BIC_Xetra_lag4" = numeric(no_stocks),
                              "Lag_min_AIC_LuS" = numeric(no_stocks),
                              "Lag_min_AIC_Xetra" = numeric(no_stocks),
                              "Lag_min_BIC_LuS" = numeric(no_stocks),
                              "Lag_min_BIC_Xetra" = numeric(no_stocks), stringsAsFactors = FALSE)
pre_information_shares <- data.frame("company" = character(no_stocks),
                                     LS_IS_upper = numeric(no_stocks),
                                     LS_IS_lower = numeric(no_stocks),
                                     LS_unique_IS = numeric(no_stocks),
                                     controll_unique_IS = numeric(no_stocks), stringsAsFactors = FALSE)
post_information_shares <- data.frame("company" = character(no_stocks),
                                      LS_IS_upper = numeric(no_stocks),
                                      LS_IS_lower = numeric(no_stocks),
                                      LS_unique_IS = numeric(no_stocks),
                                      controll_unique_IS = numeric(no_stocks), stringsAsFactors = FALSE)
XI_matrices <- list()



## loop over the stocks/data and obtain all metrics of importance
for (i in seq_along(stocks)){
  
  ## extract stock name
  stock <- stocks[i]
  
  ## read in data
  LS_data <- read_csv2(paste0(stock, "_LuS.csv"), col_types = cols())
  Xetra_data <- read_csv2(paste0(stock, "_Xetra.csv"), col_types = cols())
  
  ## check
  print(paste0(stock, ":    First Dates: ", LS_data$Datum[nrow(LS_data)], ", ", Xetra_data$Datum[nrow(Xetra_data)], "   Time: " ,Sys.time()))
  
  ## merge the datasets
  data <- dplyr::inner_join(LS_data[,c("Datum", "Schlusskurs")],
                            Xetra_data[,c("Datum", "Schlusskurs")],
                            by = "Datum")
  
  ## rename columns
  colnames(data) <- c("date", "LuS", "Xetra")
  
  
  ## calculate the VECM, returns the empirical cointegrating vector, the best model and
  ## the data with lagged and differenced values (more information in "functions.R" file)
  vecm_output <- vecm_calculations(data = data, company_name = stock)
  
  
  ## store the emp. cointegrating vector
  emp_cointegrating_vectors$comp[i] <- stock
  emp_cointegrating_vectors$coint_v[i] <- vecm_output$empirical_cointegrating_vector
  
  ## store the model selection parameters
  model_selection[i,] <- vecm_output$model_selection[1,]
  
  
  ## now continue with the data obtained from the function (i.e. that includes lagged values and so on)
  data <- vecm_output$data
  
  ## split into pre- and post-data
  pre_data <- data %>%
    filter(date >= "2018-01-01") %>%
    filter(date <= cutoff)
  post_data <- filter(data, date > cutoff)
  
  
  ## compute Hasbrouck IS on pre- and post data
  pre_hasbrouck_output <- hasbrouck(data = pre_data, lag = 2)
  post_hasbrouck_output <- hasbrouck(data = post_data, lag = 2)
  
  ## store IS
  pre_information_shares$company[i] <- stock; post_information_shares$company[i] <- stock
  pre_information_shares$LS_IS_upper[i] <- pre_hasbrouck_output$LS_IS_upper
  pre_information_shares$LS_IS_lower[i] <- pre_hasbrouck_output$LS_IS_lower
  post_information_shares$LS_IS_upper[i] <- post_hasbrouck_output$LS_IS_upper
  post_information_shares$LS_IS_lower[i] <- post_hasbrouck_output$LS_IS_lower
  
  ## store XI matrices
  XI_matrices[[length(XI_matrices) + 1]] <- pre_hasbrouck_output$XI
  names(XI_matrices)[length(XI_matrices)] <- paste0(stock, "_Xi_pre")
  XI_matrices[[length(XI_matrices) + 1]] <- post_hasbrouck_output$XI
  names(XI_matrices)[length(XI_matrices)] <- paste0(stock, "_Xi_post")
  
  
  
  
  
  
  #### Computation of Unique Information Shares ####
  
  
  ############## Pre-Data ##############
  
  ## set up residual matrix
  u <- matrix(c(pre_hasbrouck_output$model_Xetra$residuals,
                pre_hasbrouck_output$model_LuS$residuals), nrow = 2)

  ## no_tries can define how many attenmpts should be conducted to find a minima
  ## default value is one try, however, the optimization algorithm sometimes fails
  ## to find meaningfull extrema or to find any extrema at all
  no_tries <- 1
  
  pre_tries <- 0
  while (pre_tries < no_tries){

    ## maximize the log-likelihood function, also depends on u
    res_loglike <- try(
      optimization::optim_sa(loglike,
                             start = c(2, 0, 0, 2,    # W elements
                                       1.1, 1.1,    # PSI elements
                                       0.5),       # gamma value
                             lower= c(0.1, 0.1, 0.1, 0.1,
                                      1, 1,
                                      0.001),
                             upper=c(100, 100, 100, 100,
                                     100, 100,
                                     1))
    )

    ## if the optimization worked, results can be used to continue
    if (class(res_loglike) != "try-error"){
      ## use the obtained parameters to create W and PSi matrix and extract the gamma value
      W <- matrix(c(res_loglike$par[1], res_loglike$par[2], res_loglike$par[3], res_loglike$par[4]), byrow = TRUE, ncol = 2)
      PSI <- matrix(c(res_loglike$par[5], 0, 0, res_loglike$par[6]), byrow = TRUE, ncol = 2)
      gamma <- res_loglike$par[7]

      ## use gamma and PSI to calculate SIGMA_e
      SIGMA_e <- diag(2) * gamma + (1 - gamma) * PSI


      ## calculate unique Information Share
      xi <- pre_hasbrouck_output$XI
      IS_pre_unique <- ((xi %*% W %*% (SIGMA_e^0.5))^2) / as.vector((xi %*% W %*% SIGMA_e %*% t(W) %*% matrix(xi, nrow = 2)))

      ## if meaningfull unique information shares are found store the results
      if (IS_pre_unique[1,1]<1 & IS_pre_unique[1,1]>0){

        ## store
        pre_information_shares$LS_unique_IS[i] <- IS_pre_unique[1,1]
        pre_information_shares$controll_unique_IS[i] <- IS_pre_unique[1,2]
        pre_tries <- 10
        print("Pre Data unique IS successfull")

      } else{
        print(paste0("Failed Pre-try no. ", pre_tries, "    IS out of bounds  ", IS_pre_unique[1,]))
        pre_tries <- pre_tries + 1
      }


    } else {
      print(paste0("Failed Pre-try no. ", pre_tries, "    Error in the Maximization"))
      pre_tries <- pre_tries + 1
    }

  }
  
  ##### #####
  
  
  
  
  
  
  
  ############## Post-Data ##############
  
  ## set up residual matrix
  u <- matrix(c(post_hasbrouck_output$model_Xetra$residuals,
                post_hasbrouck_output$model_LuS$residuals), nrow = 2)
  
  ## no_tries can define how many attenmpts should be conducted to find a minima
  ## default value is one try, however, the optimization algorithm sometimes fails
  ## to find meaningfull extrema or to find any extrema at all
  no_tries <- 1
  
  post_tries <- 0
  while (post_tries < 1){

    ## maximize the log-likelihood function, also depends on u
    res_loglike <- try(
      optimization::optim_sa(loglike,
                             start = c(2, 0, 0, 2,    # W elements
                                       1.1, 1.1,    # PSI elements
                                       0.5),       # gamma value
                             lower= c(0.1, 0.1, 0.1, 0.1,
                                      1, 1,
                                      0.001),
                             upper=c(100, 100, 100, 100,
                                     100, 100,
                                     1))
    )
    
    ## if the optimization worked, results can be used to continue
    if (class(res_loglike) != "try-error"){
      ## use the obtained parameters to create W and PSi matrix and extract the gamma value
      W <- matrix(c(res_loglike$par[1], res_loglike$par[2], res_loglike$par[3], res_loglike$par[4]), byrow = TRUE, ncol = 2)
      PSI <- matrix(c(res_loglike$par[5], 0, 0, res_loglike$par[6]), byrow = TRUE, ncol = 2)
      gamma <- res_loglike$par[7]

      ## use gamma and PSI to calculate SIGMA_e
      SIGMA_e <- diag(2) * gamma + (1 - gamma) * PSI


      ## calculate unique Information Share
      xi <- post_hasbrouck_output$XI
      IS_post_unique <- ((xi %*% W %*% (SIGMA_e^0.5))^2) / as.vector((xi %*% W %*% SIGMA_e %*% t(W) %*% matrix(xi, nrow = 2)))

      ## if meaningfull unique information shares are found store the results
      if (IS_post_unique[1,1]<1 & IS_post_unique[1,1]>0){

        ## store
        post_information_shares$LS_unique_IS[i] <- IS_post_unique[1,1]
        post_information_shares$controll_unique_IS[i] <- IS_post_unique[1,2]
        post_tries <- 10
        print("post Data unique IS successfull")

      } else{
        print(paste0("Failed Post-try no. ", post_tries, "    IS out of bounds  ", IS_post_unique[1,]))
        post_tries <- post_tries + 1
      }


    } else {
      print(paste0("Failed Post-try no. ", post_tries, "    Error in the Maximization"))
      post_tries <- post_tries + 1
    }

  }
  
  ##### #####
  
  ## check
  print(paste0(stock, ":    NrowPre: ", nrow(pre_data), "   NrowPost: ", nrow(post_data), "    ", Sys.time()))
  
  
  
  
  ## call the garbage collector to free memory
  gc()
  
}


