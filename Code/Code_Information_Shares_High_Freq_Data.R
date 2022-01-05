
###############################################################################
########### Estimation of Information Shares on High Frequency Data ###########
###############################################################################

#### Preparation ####
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("MASS")) install.packages("MASS")

## include the functions, needs according working directory
source("functions.R")



## get all stocks for which data is available
## needs the working directory to be inside the file path with high frequency data
stocks <- list.files() %>%
  strsplit("_") %>%
  unlist() %>%
  unique()
stocks <- stocks[!stocks %in% c("LuS.csv", "Xetra.csv")]




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
information_shares <- data.frame("company" = character(no_stocks),
                                 LS_IS_upper = numeric(no_stocks),
                                 LS_IS_lower = numeric(no_stocks), stringsAsFactors = FALSE)
XI_matrices <- list()



for (i in seq_along(stocks)){
  
  ## extract stock name
  stock <- stocks[i]
  
  ## read in data
  LS_data <- read_csv(paste0(stock, "_LuS.csv"))
  Xetra_data <- read_csv(paste0(stock, "_Xetra.csv"))
  
  ## merge the datasets
  data <- dplyr::inner_join(LS_data,
                            Xetra_data,
                            by = "timestamp")
  
  ## rename columns
  colnames(data) <- c("date", "LuS", "Xetra")
  
  # ## get relative return data
  data$LuS <- c(diff(data$LuS), NA)/data$LuS
  data$Xetra <- c(diff(data$Xetra), NA)/data$Xetra
  data <- na.omit(data)
  
  
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
  
  ## compute Hasbrouck IS
  hasbrouck_output <- hasbrouck(data = data, lag = 3)
  
  ## store IS
  information_shares$company[i] <- stock
  information_shares$LS_IS_upper[i] <- hasbrouck_output$LS_IS_upper
  information_shares$LS_IS_lower[i] <- hasbrouck_output$LS_IS_lower
  
  ## store XI matrices
  XI_matrices[[length(XI_matrices) + 1]] <- hasbrouck_output$XI
  names(XI_matrices)[length(XI_matrices)] <- paste0(stock, "_Xi")
  
}











