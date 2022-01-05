
###############################################################################
########################### Functions for Webscraping #########################
###############################################################################

## function to extract the time out of messy text data
time_extract <- function(string){
  
  s <- stringr::str_replace(string, ".*decimals=", "") %>%
    stringr::str_replace("</span>.*", "")
  s <- substr(s, 3, nchar(s))
  
  return(as.character(s))
}


## function to extract the price out of messy text data
price_extract <- function(string){
  
  k <- stringr::str_replace(string, ".*decimals=", "") %>%
    stringr::str_replace("???.*", "") %>%
    stringr::str_replace(",", ".")
  k <- substr(k, 3, nchar(k)-1)
  
  return(as.numeric(k))
  
}


## function to initialize an empty dataframe of given size
init_empty_df <- function(length_df){
  df <- data.frame(matrix(ncol = 4, nrow = length_df))
  colnames(df) <- c("Date", "Time", "bid", "ask")
  df$Date <- as.Date(df$Date)
  df$Time <- as.character(df$Time)
  df$bid <- as.numeric(df$bid)
  df$ask <- as.numeric(df$ask)
  return(df)
}








## function to conduct the scraping of the fnet webpage
get_data_fnet <- function(url){
  
  ## get website content
  web_content <- try(xml2::read_html(url))
  
  ## write as txt in read in again as csv^^
  try(xml2::write_xml(web_content, "xetra_web_content.txt"))
  web_data <- read.delim("xetra_web_content.txt", sep = "|", header = FALSE,
                         stringsAsFactors = FALSE)
  
  ## search for beginning/base index
  base_i <- which(web_data$V1 == "</tr></thead>")
  
  ## create indices for time, prices and volumne
  c <- cumsum(rep(6, 32))
  time_i <- c(base_i+2, base_i+2+c)
  price_i <- c(base_i+3, base_i+3+c)
  volume_i <- c(base_i+4, base_i+4+c)
  
  ## extract the objects of interest
  time_stamps <- web_data$V1[time_i]
  prices <- web_data$V1[price_i]
  volume <- web_data$V1[volume_i]
  
  ## store in data frame, for now all as ugly characters
  d <- data.frame(time = time_stamps,
                  price = prices,
                  volume = volume)
  
  ## return
  return(d)
  
}

