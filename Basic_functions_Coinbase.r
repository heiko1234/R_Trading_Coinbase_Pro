#! /usr/bin/r   



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


# check if data exist in path, if not create file, if exist, add new rows

save_data<-function(data, path, name = NULL){
    #
    if (length(name) == 0){
        name = deparse(substitute(data))
    }
    else{name = name}
    #
    CSV <- list.files(path = path, pattern = paste0(name, ".csv") )
    CSV
    #
    your_path = paste0(path, "\\", name, ".csv")
    your_path
    #
    if (length(CSV) == 0){
        # file does not exist yet, need to create it
        write.csv(data, your_path, row.names = FALSE)
    }
    # file does exist in folder it will be modified if rows differ
    if (length(CSV) != 0){
        existing_data <- read.csv(file = (paste0(path, "\\", name,".csv")), )
        first.row = existing_data[1,]
        first.row_time = as.POSIXlt(first.row$time)
        max_index = max(which(data$time >= first.row_time))
        max_index
        if (max_index > 1){
            data <- data[1:max_index,]
        }
        if (max_index == 1){
            data <- NULL
        }
        #
        #read, order, remove duplicated and save file again
        if (length(data[,1]) >= 2){
            existing_data<-read.csv(file =  paste0(path, "\\", name,".csv"), )
            existing_data <- rbind(data, existing_data)
            #head(existing_data)
            existing_data <- existing_data[order(existing_data$time, decreasing = TRUE),]
            existing_data <- existing_data[!duplicated(existing_data$time),]
            write.csv(existing_data , your_path, row.names = FALSE)
        }
    }
}




# Daten Abruf und aggregation
# 1 Min Data
ETH_USD_1 <- public_candles(product_id = "ETH-USD", granularity = 60)

# 5 Min aggregated Data
ETH_USD_5 <- aggregate_public_candles(data = ETH_USD_1, aggregation = 5)

#15 Min aggregated Data
ETH_USD_15 <- public_candles(product_id = "ETH-USD", granularity = 900)



head(ETH_USD_1)

### Test save 1 und 5 Min Data
path = "C:\\Users\\Heiko\\Visual.Studio\\R_Trading_Coinbase_Pro"


save_data(data = ETH_USD_1 , path = path)

save_data(data = ETH_USD_5 , path = path)

save_data(data = ETH_USD_15 , path = path)




head(ETH_USD_1)
existing_data<-read.csv(file = "C:\\Users\\Heiko\\Visual.Studio\\R_Trading_Coinbase_Pro\\ETH_USD_1.csv", sep = ",")

head(existing_data)
existing_data[1:10,]



