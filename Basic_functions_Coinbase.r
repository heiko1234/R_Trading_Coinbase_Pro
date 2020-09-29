



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
                           granularity = NULL) {
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
  content$time <-
    as.POSIXct(content$time, origin = "1970-01-01") #, tz = "GMT")
  #
  content <- content[order(content$time, decreasing = TRUE),]
  #return----
  return(content)
}



####
public_candles(product_id = "ETH-USD", granularity = 900)



public_candles(product_id = "ETH-USD", granularity = 60)


# Daten Abruf und aggregation
first_data <- public_candles(product_id = "ETH-USD", granularity = 60)
first_data[1:20,]



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


aggregate_public_candles(data = first_data, aggregation = 15)











