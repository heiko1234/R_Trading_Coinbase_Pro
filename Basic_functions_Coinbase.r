



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
  content <-
    content[order(content$V1), ] # sort chorologically by time since epoch
  
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









