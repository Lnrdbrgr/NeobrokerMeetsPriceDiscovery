
###############################################################################
####################### Preparation File for Webscrpaing ######################
###############################################################################



######### Lang & Schwarz preparation #########

## L&S base url
lsbase_url <- "https://www.ls-tc.de/de/aktie/"


## stock names according to L&S website
ls_stocks <- c(paste0(c("basf", "bayer", "daimler", "infineon", "covestro", "e-on",
                        "sap", "siemens", "vonovia-se", "zalando-se", "deutsche-bank"), "-aktie"),
               "512752", "462306")   # 462306: Delivery Hero, 512752: Hellofresh

## stock names
ls_stocks_name <- c("basf", "bayer", "daimler", "infineon", "covestro", "e-on",
                    "sap", "siemens", "vonovia", "zalando", "deutsche-bank",
                    "delivery-hero", "hellofresh")

## prepare dataframe to store results
ls_df <- data.frame(matrix(ncol = 5))
colnames(ls_df) <- c("date", "time", "bid", "ask", "stock")
ls_df$date <- as.Date(ls_df$date)




######### Finanzen.net preparation #########

## fnet base url
fnetbase_url <- "https://www.finanzen.net/timesandsales/"

## stock names according to fnet website
fnet_stocks <- c("basf", "bayer", "daimler", "infineon", "covestro", "e_on",
                 "sap", "siemens", "vonovia", "zalando", "deutsche_bank",
                 "delivery_hero", "hellofresh")


## url vector
fnet_urls <- paste0(fnetbase_url, fnet_stocks)


## prepare dataframe to store results
fnet_data <- data.frame(matrix(ncol = 5))
colnames(fnet_data) <- c("date", "time", "price", "volume", "stock")
fnet_data$date <- as.Date(fnet_data$date)









