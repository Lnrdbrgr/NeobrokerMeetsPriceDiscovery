
###############################################################################
########################### Main Code for Webscraping #########################
###############################################################################

## prepare packages
if (!require("stringr")) install.packages("stringr")
if (!require("xml2")) install.packages("xml2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")

## include the files for preparation and function
source("scraping_functions.R")
source("Preparation.R")

## set counter for observations and to start indefinite while-loop
counter <- 1

while (counter > 0){
  
  ## first check what time it is
  c_time <- Sys.time()
  c_minute <- lubridate::minute(c_time)
  
  ##  between 1700 and 0900 and on the weekend the system can sleep
  if(((lubridate::hour(c_time)+c_minute/100) > 17.2 | (lubridate::hour(c_time)+c_minute/100) < 9.2) | lubridate::wday(c_time) %in% c(1,7)){
    ## if at night or weekend, system can sleep for 60 seconds, then check again
    Sys.sleep(60)
    print(paste0("It seems to be night-time - slept for a little while! Btw it's ", Sys.time()))
  } else {
    ## if not at night, scrape for your life
    for (i in 1:length(ls_stocks)){
      
      ############## L&S Scraping ##############
      
      ## get URL
      ls_url <- paste0(lsbase_url, ls_stocks[i])
      
      ## get website content
      ls_web_content <- try(xml2::read_html(ls_url))
      
      ## write as txt and read in again as csv (not elegant but effective)
      try(xml2::write_xml(xml2::xml_child(ls_web_content, 2), "ls_web_content.txt"))
      ls_web_data <- read.delim("ls_web_content.txt", sep = "|", header = FALSE,
                                stringsAsFactors = FALSE)
      
      ## create indices to extract values of interest
      ground_i <- which(ls_web_data$V1 == "<tr rel=template style=display:none;>")[2]
      times_i <- ground_i + cumsum(rep(7,9))
      bid_i <- ground_i+2 + cumsum(rep(7,9))
      ask_i <- ground_i+3  + cumsum(rep(7,9))
      
      ## extract times and prices
      times <- time_extract(ls_web_data$V1[times_i])
      bids <- price_extract(ls_web_data$V1[bid_i])
      asks <- price_extract(ls_web_data$V1[ask_i])
      
      ## create dataframe and store results
      d_ls <- data_frame(date = Sys.Date(),
                         time = times,
                         bid = bids,
                         ask = asks)
      
      d_ls$stock <- ls_stocks[i]
      
      ## bind the new observations to the master dataframe
      ## it is apparent to me that binding data in a loop is not recommended. However, starting
      ## an indefinite scraping process, the size of the dataframe cannot really be determined
      ls_df <- rbind(ls_df, d_ls)
      
      ## exculde duplicate prices and times and through out NAs
      ls_df <- ls_df %>%
        distinct() %>%
        na.omit()
      
      
    } ## closes for-loop to scrape through the stocks
    
    
    ############## FNet Scraping ##############
    
    for (i in 1:length(fnet_urls)){
      
      ## get data for stock from url
      ## other than for the L&S scraping, the music here plays within the function
      d <- try(get_data_fnet(fnet_urls[i]))
      
      ## add stock name to dataframe
      d$date <- Sys.Date()
      d$stock <- fnet_stocks[i]
      
      ## bind the data to the master dataframe
      fnet_data <- rbind(fnet_data, d)
      
      ## remove NA and duplicated rows
      fnet_data <- fnet_data %>%
        distinct() %>%
        na.omit()
    }
    
    
    ## safe file every couple times
    if (counter%%350==0){
      
      ## set name for csv-file
      fnet_name <- paste0("fnet_data_", counter, ".csv")
      ls_name <- paste0("ls_data_", counter, ".csv")
      
      ## write dataset to csv
      write.csv(fnet_data, file = fnet_name, row.names = FALSE)
      write.csv(ls_df, file = ls_name, row.names = FALSE)
      
      ## clean storage to free memory
      rm(fnet_data)
      rm(ls_df)
      gc()
      
      ## make new df to store data
      fnet_data <- data.frame(matrix(ncol = 5))
      colnames(fnet_data) <- c("date", "time", "price", "volume", "stock")
      fnet_data$date <- as.Date(fnet_data$date)
      ls_df <- data.frame(matrix(ncol = 5))
      colnames(ls_df) <- c("date", "time", "bid", "ask", "stock")
      ls_df$date <- as.Date(ls_df$date)
    }
    
    ## check how much time was needed, if not a lot sleep for a while,
    ## since the relevant data refreshes only every minute
    time_needed <- difftime(Sys.time(), c_time, units = "secs")
    if (time_needed < 60){
      Sys.sleep((60-time_needed))
    } 
    
    ## print progress to observe the process
    print(paste0(counter, "    ", "LS: ", nrow(ls_df), "  Fnet: ", nrow(fnet_data), "  ", time_needed, "  ", Sys.time()))
    
    ## increase the counter
    counter <- counter + 1
    
  } ## closes if/else statement that checks the time for trading times and weekdays
} ## closes while loop


