
#### Result Preparations & Graphics ####
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readxl")) install.packages("readxl")



### Introductory Graphics ###
user_TR <- readxl::read_excel("Recherche/Facts and Figures TR und LS.xlsx",
                              sheet = "User_TR")
LuS_trading <- readxl::read_excel("Recherche/Facts and Figures TR und LS.xlsx",
                                  sheet = "LuS_tradingactivity")

## Graph TR user
ggplot(data = user_TR, 
       aes(x = Date, y = User/1000)) +
  geom_line(size = 1) +
  labs(y = expression("User (in thousand)"), 
       x = expression("Time"), 
       title = "Trade Republic User") 

## Graph trading activity L&S
ggplot(data = LuS_trading, 
       aes(x = Date, y = EUR)) +
  geom_line(size = 1) +
  ylim(0, 90) +
  labs(y = expression("Revenue (in thousand)"), 
       x = expression("Time"), 
       title = "Lang & Schwarz: Revenue from Trading Activity") 



### High Frequency Data ###

## read in data
highfreq_IS <- read.csv("Results/highfreq_information_shares.csv")
EVSales <- readxl::read_excel("Results/EVSales.xlsx")

## combine 
highfreq_IS <- dplyr::left_join(highfreq_IS, EVSales, by = "company")

## arrange
highfreq_IS <- highfreq_IS %>%
  dplyr::select(company, LS_IS_lower, LS_IS_upper, median, KUV2021) %>%
  arrange(desc(KUV2021))

## check correlation
cor(highfreq_IS$median, highfreq_IS$KUV2021) ## correlation between IS and KUV: 0.3160081

## round for nicer table
highfreq_IS[,-1] <- round(highfreq_IS[,-1], digits = 4)

## make LaTeX table
print(xtable::xtable(highfreq_IS, digits=c(0,0,4,4,4,2)), include.rownames = FALSE)


### Low Frequency Data ###

## read in data
lowfreq_IS_pre <- read.csv("Results/lowfreq_information_shares_pre.csv")
colnames(lowfreq_IS_pre) <- c("company", "LS_IS_upper_pre", "LS_IS_lower_pre", "LS_unique_IS_pre", "controll_unique_IS_pre")
lowfreq_IS_post <- read.csv("Results/lowfreq_information_shares_post.csv")
colnames(lowfreq_IS_post) <- c("company", "LS_IS_upper_post", "LS_IS_lower_post", "LS_unique_IS_post", "controll_unique_IS_post")

## manipulate
DAX_comp <- dplyr::left_join(lowfreq_IS_pre, lowfreq_IS_post, by = "company")
DAX_comp <- DAX_comp %>%
  mutate(LS_IS_median_pre = (LS_IS_upper_pre + LS_IS_lower_pre)/2) %>%
  mutate(LS_IS_median_post = (LS_IS_upper_post + LS_IS_lower_post)/2) %>%
  mutate(HIS_median_change = LS_IS_median_post - LS_IS_median_pre) %>% 
  mutate(UIS_change = LS_unique_IS_post - LS_unique_IS_pre) %>% 
  dplyr::select(company, LS_IS_lower_pre, LS_IS_upper_pre, LS_IS_median_pre, LS_unique_IS_pre,
                         LS_IS_lower_post, LS_IS_upper_post, LS_IS_median_post, LS_unique_IS_post,
                         HIS_median_change, UIS_change) %>%
  filter(!company %in% c("Tesla", "GameStop", "AMC")) %>%
  dplyr::left_join(EVSales, by = "company") %>%
  dplyr::select(-KGV10YAvg) %>%
  arrange(desc(KUV2021))


## round for nicer table
DAX_comp[,-1] <- round(DAX_comp[,-1], digits = 4)

## check for correlation
cor(DAX_comp$LS_IS_median_post, DAX_comp$KUV2021)  # HIS after with KUV: -0.008642457
cor(DAX_comp$LS_unique_IS_post, DAX_comp$KUV2021)  # UIS after with KUV: -0.03980963
cor(DAX_comp$HIS_median_change, DAX_comp$KUV2021)  # HIS change with KUV: -0.384266
cor(DAX_comp$UIS_change, DAX_comp$KUV2021, use="complete.obs")  # UIS change with KUV: -0.0582678

DAX_comp_pre <- DAX_comp %>%
  dplyr::select(company, LS_IS_lower_pre, LS_IS_upper_pre, LS_IS_median_pre, LS_unique_IS_pre, KUV2021)

DAX_comp_post <- DAX_comp %>%
  dplyr::select(company, LS_IS_lower_post, LS_IS_upper_post, LS_IS_median_post, LS_unique_IS_post, KUV2021)

DAX_comp_changes <- DAX_comp %>%
  dplyr::select(company, HIS_median_change, UIS_change, KUV2021)

print(xtable::xtable(DAX_comp_pre, digits=c(0,0,4,4,4,4,2)), include.rownames = FALSE)
print(xtable::xtable(DAX_comp_post, digits=c(0,0,4,4,4,4,2)), include.rownames = FALSE)
print(xtable::xtable(DAX_comp_changes, digits=c(0,0,4,4,2)), include.rownames = FALSE)

print(xtable::xtable(DAX_comp, digits=c(0,0,4,4,4,4,4,4,4,4,4,4,2)), include.rownames = FALSE)



### Hot stocks ####

## manipulate
Hotstocks <- dplyr::left_join(lowfreq_IS_pre, lowfreq_IS_post, by = "company")
Hotstocks <- Hotstocks %>%
  mutate(LS_IS_median_pre = (LS_IS_upper_pre + LS_IS_lower_pre)/2) %>%
  mutate(LS_IS_median_post = (LS_IS_upper_post + LS_IS_lower_post)/2) %>%
  mutate(HIS_median_change = LS_IS_median_post - LS_IS_median_pre) %>% 
  mutate(UIS_change = LS_unique_IS_post - LS_unique_IS_pre) %>% 
  dplyr::select(company, LS_IS_lower_pre, LS_IS_upper_pre, LS_IS_median_pre, LS_unique_IS_pre,
                LS_IS_lower_post, LS_IS_upper_post, LS_IS_median_post, LS_unique_IS_post,
                HIS_median_change, UIS_change) %>%
  filter(company %in% c("Tesla", "GameStop", "AMC")) %>%
  dplyr::left_join(EVSales, by = "company") %>%
  dplyr::select(-KGV10YAvg) %>%
  arrange(desc(KUV2021))


## round for nicer table
Hotstocks[,-1] <- round(Hotstocks[,-1], digits = 4)
Hotstocks <- Hotstocks %>%
  dplyr::select(-KUV2021)

print(xtable::xtable(Hotstocks, digits=c(0,0,4,4,4,4,4,4,4,4,4,4)), include.rownames = FALSE)


### Lowfreq: Xi matrices ###

lowfreq_XI <- read.csv("Results/lowfreq_XI_matrices.csv")
colnames(lowfreq_XI) <- c("company", "period", "Exchange", "perm_impact")
lowfreq_XI_Dax <- filter(lowfreq_XI, !company %in% c("Tesla", "GameStop", "AMC"))

lowfreq_XI_Dax %>%
  filter(period == "pre") %>%
  ggplot(aes(x=Exchange, y=perm_impact)) + 
  geom_boxplot() +
  guides(fill = FALSE)  +
  labs(y = expression("Permanent Impact"), 
       x = expression("Exchange"), 
       title = "Magnitude of permanent impact - 2018-2019") +
  ylim(-0.75, 1.75) 


lowfreq_XI_Dax %>%
  filter(period == "post") %>%
  ggplot(aes(x=Exchange, y=perm_impact)) + 
  geom_boxplot() +
  guides(fill = FALSE)  +
  labs(y = expression("Permanent Impact"), 
       x = expression("Exchange"), 
       title = "Magnitude of permanent impact - 2020-2021")  +
  ylim(-0.75, 1.75) 


lowfreq_XI_full <- read.csv("Results/XI_matrices_full.csv")
colnames(lowfreq_XI_full) <- as.character(c("comp","period",1,2,3,4))

lowfreq_XI_full_pre <- lowfreq_XI_full %>%
  filter(period == "pre") %>%
  dplyr::select(-2)

lowfreq_XI_full_post <- lowfreq_XI_full %>%
  filter(period == "post") %>%
  dplyr::select(-2)

print(xtable::xtable(lowfreq_XI_full_pre, digits=c(0,0,4,4,4,4)), include.rownames = FALSE)
print(xtable::xtable(lowfreq_XI_full_post, digits=c(0,0,4,4,4,4)), include.rownames = FALSE)




### Highfreq: Xi matrices ###
highfreq_XI <- XI_matrices_df %>%
  dplyr::select(-X2)
colnames(highfreq_XI) <- c("company", "Exchange", "perm_impact")


highfreq_XI %>%
  ggplot(aes(x=Exchange, y=perm_impact)) + 
  geom_boxplot() +
  guides(fill = FALSE)  +
  labs(y = expression("Permanent Impact"), 
       x = expression("Exchange"), 
       title = "Magnitude of permanent impact")




### cointegrating vectors ###
lowfreq_CVs <- read.csv("Results/lowfreq_emp_cointegrating_vectors.csv")
highfreq_CVs <- read.csv("Results/highfreq_emp_cointegrating_vectors.csv")

print(xtable::xtable(lowfreq_CVs, digits=c(0,0,6)), include.rownames = FALSE)
print(xtable::xtable(highfreq_CVs, digits=c(0,0,6)), include.rownames = FALSE)



### Model Selection ###
lowfreq_model_selection <- read.csv("Results/lowfreq_modelselection.csv")
lowfreq_model_selection_AIC <- lowfreq_model_selection %>%
  dplyr::select(company,
                AIC_LuS_lag2, AIC_LuS_lag3, AIC_LuS_lag4,
                AIC_Xetra_lag2, AIC_Xetra_lag3, AIC_Xetra_lag4,
                Lag_min_AIC_LuS, Lag_min_AIC_Xetra)

mean(lowfreq_model_selection_AIC$Lag_min_AIC_Xetra)

print(xtable::xtable(lowfreq_model_selection_AIC, digits=c(0,0,1,1,1,1,1,1,0,0)), include.rownames = FALSE)






