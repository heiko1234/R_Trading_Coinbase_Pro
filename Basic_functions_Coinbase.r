



####### Basic Functions in R to interact with API of Coinbase Pro

#libraries

require(stringi)
require(curl)
require(xts)
require(TTR)
library(quantmod)
library(httr)
library(jsonlite)


# Utility Function To Parse Message From Coinbase Pro API For Public Functions

parse_response <- function(path, query = NULL) {
  #define api base url
  api.url <- "https://api.pro.coinbase.com"

  #create final end point
  url <- modify_url(api.url, path = path, query = query)

  #fetch response
  response <- GET(url = url)

  #validate success
  if (response$status_code != 200) {
    content <- fromJSON(content(response,
                                as = "text"))
    message <- content$message
    stop(message)
  } else {
    content <- fromJSON(content(response,
                                as = "text"))
  }

  #return
  return(content)
}



# Get bids and asks for provided currency-pair (products) by Coinbase Pro API
#product_id = "ETH-USD"
#start = NULL
#end = NULL
#granularity = NULL


public_candles <- function(product_id = "ETH-USD",
                           start = NULL,
                           end = NULL,
                           granularity = NULL,
                           local_time = TRUE) {
  #case remediation
  product_id <- toupper(product_id)
  
  #get url extension
  req.url <- paste0("/products/", product_id, "/candles")
  
  #fetch response
  content <- parse_response(
    path = req.url,
    query = list(
      "start" = start,
      "end" = end,
      "granularity" = granularity
    )
  )
  
  #transform
  content <- as.data.frame(content)
  #content <-
  #  content[order(content$V1), ] # sort chorologically by time since epoch
  
  names(content) <-
    c("time", "low", "high", "open", "close", "volume")
  if (local_time == TRUE){
    content$time <-
      as.POSIXct(content$time, origin = "1970-01-01") #, tz = "GMT")
  }
  if (local_time != TRUE){
    content$time <-
      as.POSIXct(content$time, origin = "1970-01-01", tz = "GMT") 
  }
  #
  content <- content[order(content$time, decreasing = TRUE),]
  #return----
  return(content)
}


aggregate_public_candles<-function(data, aggregation = 5){
    rows = floor(length(data$time)/aggregation)
    #
    rowslist = NULL
    for (i in 0:(rows-1)){
        rowslist = c(rowslist, (i*aggregation+1))
    }
    rowslist
    output_time = data[rowslist, 1]
    output_close = data[rowslist, 5]
    output_open = data[(rowslist+(aggregation-1)), 4]
    #
    output_high = NULL
    output_low = NULL
    output_volume = NULL
    #
    for (i in 1:rows){
        data_slide = data[i:(i*aggregation), ]
        min_data = min(data_slide[,2])
        max_data = max(data_slide[,3])
        sum_vol_data = sum(data_slide[,6])
        output_high = c(output_high, max_data)
        output_low = c(output_low, min_data)
        output_volume = c(output_volume, sum_vol_data)
    }

    #construct dataframe with time, low, high, open, close, volume
    output = data.frame(output_time, output_low, output_high, output_open, output_close, output_volume)
    #
    colnames(output) = c("time", "low", "high", "open", "close", "volume")
    
    #return
    return(output)
}



save_data<-function(data, path){
    #
    
    CSV_ETH <- list.files(path = path, pattern = paste0(deparse(substitute(data)),".csv") )
    #CSV_ETH
    
    if (length(CSV_ETH) == 0){
        # file does not exist yet, need to create it
        your_path = paste0(path, "\\", deparse(substitute(data)),".csv")
        write.csv(data, your_path, row.names = FALSE)
    }
    
    # file does exist in folder and will be modified
    if (length(CSV_ETH) != 0){
        #
        existing_data<-read.csv(file =  paste0(path, "\\", deparse(substitute(data)),".csv"), )
        first.row = existing_data[1,]

        to_check.zeit<-as.character(first.row$time)
        data.zeit<-as.character(data$time)
        index_similar = which(data.zeit == to_check.zeit)

        # if no similarity add data to existing data
        if (length(index_similar) == 0 ) {
              write.table(data,
              file = paste0(path, "\\", deparse(substitute(data)),".csv"), 
              append = T,
              sep=",",
              row.names=F,
              col.names=F)
        }

        # add new data rows to existing data file
        if (length(index_similar) == 1) {
              write.table(data[1:(index_similar-1),],
              file = paste0(path, "\\", deparse(substitute(data)),".csv"), 
              append = T,
              sep=",",
              row.names=F,
              col.names=F)
        }

        #read, order, remove duplicated and save file again
        existing_data<-read.csv(file =  paste0(path, "\\", deparse(substitute(data)),".csv"), )
        existing_data <- existing_data[order(existing_data$time, decreasing = TRUE),]
        existing_data <- existing_data[!duplicated(existing_data$time),]
        #
        your_path = paste0(path, "\\", deparse(substitute(data)),".csv")
        write.csv(existing_data , your_path, row.names = FALSE)

    }

}




# Daten Abruf und aggregation
# 1 Min Data
ETH_USD_1 <- public_candles(product_id = "ETH-USD", granularity = 60)

# 5 Min aggregated Data
ETH_USD_5 <- aggregate_public_candles(data = ETH_USD_1, aggregation = 5)

#15 Min aggregated Data
ETH_USD_15 <- public_candles(product_id = "ETH-USD", granularity = 900)



### Test save 1 und 5 Min Data
path = "C:\\Users\\Heiko\\Visual.Studio\\R_Trading_Coinbase_Pro"


save_data(data = ETH_USD_1 , path = path)

save_data(data = ETH_USD_5 , path = path)

save_data(data = ETH_USD_15 , path = path)



