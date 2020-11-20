








#libraries

require(stringi)
library(httr)
require(RCurl)
require(xts)
require(TTR)
library(quantmod)
library(digest)

library(jsonlite)
#
#install.packages("base64Decode")
#library(base64Decode)
#install.packages("rgdax")
library(rgdax)


######

access_data<-read.csv(file = "C:\\Users\\Heiko\\Visual.Studio\\R_Trading_Coinbase_Pro\\access.csv", sep = ",")

####

# my_api_key<- "63034e22f66cff68babbeb80348f2c12"
# my_secret<- "i0+pxGsvFv7rVLbpEP0MLQs0L376az32RBQQDFDd2PxgBbkMRb27JUGhDeLsj9fAS4z8UM2A7gaivzWwtYyQyw=="
# my_passphrase<- "2mcr04e5fs7"


my_api_key <- "r api request"
my_secret <- "Mxg7KOoCUi4GMFaV7HaP5OIbpDbOGYtGZkCibE3DBCyynsbRbY7ROg/iwZOc1ka8YMSg1SIo3qjk+97SyStx0w=="
my_passphrase <- "lbqggmtuvxm"


######

account <- function(acct_id,
                    api.key,
                    secret,
                    passphrase) {
  #get url extension
  req.url <-  paste0("/accounts/", acct_id)

  #define method
  method <- "GET"

  #fetch response
  response <- auth(
    method = method,
    req.url = req.url,
    api.key = api.key,
    secret = secret,
    passphrase = passphrase
  )

  #transform-
  response <- as.data.frame(response)

  #return-
  return(response)
}


account_hist <- function(currency = "ETH",
                         api.key,
                         secret,
                         passphrase) {
  #Determine account id
  raw_data <- accounts(api.key, secret, passphrase)

  #case remediation
  currency <- toupper(currency)

  #valid currency?
  if (currency %in% raw_data$currency) {
    acct_id <- raw_data$id[raw_data$currency == currency]

    #get url extension
    req.url <-  paste0("/accounts/", acct_id, "/ledger")

    #define method
    method <- "GET"

    #fetch response
    response <- auth(
      method = method,
      req.url = req.url,
      api.key = api.key,
      secret = secret,
      passphrase = passphrase
    )

    #transform
    response$id <- as.character(response$id)
    response$amount <- as.numeric(response$amount)
    response$balance <- as.numeric(response$balance)

    #return
    return(response)

  }
  #Invalid currency?
  else {
    stop("Invalid currency provided")
  }
}


accounts <- function(api.key, secret, passphrase)  {
  #get url extension
  req.url <-  "/accounts/"

  #define method
  method <- "GET"

  #fetch response
  response <- auth(
    method = method,
    req.url = req.url,
    api.key = api.key,
    secret = secret,
    passphrase = passphrase
  )

  #transform
  response$balance <- as.numeric(response$balance)
  response$available <- as.numeric(response$available)
  response$hold <- as.numeric(response$hold)

  #return
  return(response)
}


add_order <- function(api.key,
                      secret,
                      passphrase,
                      product_id = "LTC-USD",
                      type = "limit",
                      stop = NULL,
                      stop_price = NULL,
                      side = "b",
                      price = NULL,
                      size) {
  #get url extension
  req.url <- "/orders"

  #define method
  method <-  "POST"

  #transform params
  product_id <- toupper(product_id)
  price <- as.character(price)
  size <- as.character(size)

  if (side == "b") {
    side <- "buy"
  } else if (side == "s") {
    side <- "sell"
  } else {
    stop("Unrecognized sell or buy side. Please select either 'b' or 's'.")
  }

  #generate order
  if (is.null(stop)) {
    order_attribs <- list(
      type = type,
      price = price,
      size = size,
      side = side,
      product_id = product_id
    )
  } else {
    order_attribs <- list(
      price = price,
      size = size,
      side = side,
      product_id = product_id,
      stop = stop,
      stop_price = stop_price
    )
  }

  order <- toJSON(order_attribs, auto_unbox = TRUE)

  #fetch response
  response <-
    auth(
      method = method,
      req.url = req.url,
      api.key = api.key,
      secret = secret,
      passphrase = passphrase,
      order = order
    )

  #transform
  response <- as.data.frame(response)

  #return
  return(response)
}


# function definition

auth <- function(method,
                 req.url,
                 api.key,
                 secret,
                 passphrase,
                 order = NULL) {
  #define api base url----
  #api.url <- "https://api.gdax.com"   ###API.GDAX change to Coinbase
  
  api.url <- "https://api.pro.coinbase.com"
  #api.url <- "https://api-public.sandbox.pro.coinbase.com"
  
  
  #generate nonce and key encodings----
  url <- paste0(api.url, req.url)
  timestamp <-
    format(as.numeric(Sys.time()), digits = 13) # create nonce
  key <- base64Decode(secret, mode = "raw") # encode api secret
  
  #create final end point----
  if (method == "GET") {
    what <- paste0(timestamp, method, req.url) # get method
  } else if (method == "POST") {
    what <- paste0(timestamp, method, req.url, order)
  } else if (method == "DELETE") {
    what <- paste0(timestamp, method, req.url)
  }
  
  #create encoded signature----
  sign <-
    base64Encode(hmac(key, what, algo = "sha256", raw = TRUE)) # hash
  
  #define headers----
  httpheader <- list(
    'CB-ACCESS-KEY' = api.key,
    'CB-ACCESS-SIGN' = sign,
    'CB-ACCESS-TIMESTAMP' = timestamp,
    'CB-ACCESS-PASSPHRASE' = passphrase,
    'Content-Type' = 'application/json'
  )
  
  #generating GET results----
  if (method == "GET") {
    #Get test macOS----
    if (Sys.info()["sysname"] == "Darwin") {
      response <- fromJSON(rawToChar(
        getURLContent(
          url = url,
          curl = getCurlHandle(useragent = "R"),
          httpheader = httpheader
        )
      ))
    }
    #Get test windows----
    else {
      response <- fromJSON(getURLContent(
        url = url,
        curl = getCurlHandle(useragent = "R"),
        httpheader = httpheader
      ))
    }
  }
  # ###########################
  testjson<-GET(
        url = url,
        #curl = getCurlHandle(useragent = "R"),
        httpheader = httpheader)
    
    url
    getCurlHandle(useragent = "R")
    httpheader
    testjson
    # Hier passt es leider nicht #
########################################################
#######################################################
#######################################################

  #generating POST results----
  else if (method == "POST") {
    #Post test macOS----
    if (Sys.info()["sysname"] == "Darwin") {
      response <- fromJSON(rawToChar(
        getURLContent(
          url = url,
          curl = getCurlHandle(useragent = "R"),
          httpheader = httpheader,
          postfields = order
        )
      ))
    }
    #Post test windows----
    else{
      response <- fromJSON(
        getURLContent(
          url = url,
          curl = getCurlHandle(useragent = "R"),
          httpheader = httpheader,
          postfields = order
        )
      )
    }
  }
  #Generating DELETE results
  else if (method == "DELETE") {
    #Post test macOS----
    if (Sys.info()["sysname"] == "Darwin") {
      response <- fromJSON(rawToChar(httpDELETE(
        url = url,
        curl = getCurlHandle(useragent = "R"),
        httpheader = httpheader
      )))
    } else {
      response <- fromJSON(httpDELETE(
        url = url,
        curl = getCurlHandle(useragent = "R"),
        httpheader = httpheader
      ))
    }
  }
  
  #return----
  return(response)
}


cancel_order <- function(order_id = "all",
                         api.key,
                         secret,
                         passphrase) {

  #get url extension
  if (order_id == "all"){
    req.url <- "/orders/"
    print(req.url)
  } else {
    req.url <- paste0("/orders/",order_id)
    print(req.url)
  }


  #define method
  method = "DELETE"

  #fetch response
  response <-
    auth(
      method = method,
      req.url = req.url,
      api.key = api.key,
      secret = secret,
      passphrase = passphrase
    )

  #transform
  response <- as.data.frame(response)

  #return
  return(response)
}


fills <-function(api.key, secret, passphrase, product_id=NULL) {
    #
    #get url extension
    if (is.null(product_id)) {
      req.url = "/fills"
    } 
    else {
      product_id <- toupper(product_id)
      req.url = paste0("/fills?product_id=", product_id)
    }

    #get method
    method <- "GET"

    #fetch response
    fills <- auth(
      method = method,
      req.url = req.url,
      api.key = api.key,
      secret = secret,
      passphrase = passphrase
    )

    #transform
    fills$price <- as.numeric(fills$price)
    fills$size <- as.numeric(fills$size)
    fills$fee <- as.numeric(fills$fee)
    fills$usd_volume <- as.numeric(fills$usd_volume)
    fills$created_at <- strptime(fills$created_at, "%Y-%m-%dT%H:%M:%OS")

    #return
    return(fills)
}



holds <- function(currency = "ETH", api.key, secret, passphrase) {
  #
  #Determine account id
  raw_data <- accounts(api.key, secret, passphrase)

  #case remediation
  currency <- toupper(currency)

  #valid currency?
  if (currency %in% raw_data$currency) {
    acct_id <- raw_data$id[raw_data$currency == currency]

    #get url extension
    req.url <-  paste0("/accounts/", acct_id, "/holds")

    #define method
    method <- "GET"

    #fetch response
    response <- auth(
      method = method,
      req.url = req.url,
      api.key = api.key,
      secret = secret,
      passphrase = passphrase
    )

    #transform
    response <- as.data.frame(response)

    #return
    return(response)
  }
  #Invalid currency?
  else {
    stop("Invalid currency provided")
  }
}


open_orders <- function(api.key, secret, passphrase) {
  #
  #get url extension
  req.url <-  paste0("/orders/")

  #define method
  method <- "GET"

  #fetch response
  response <- auth(
    method = method,
    req.url = req.url,
    api.key = api.key,
    secret = secret,
    passphrase = passphrase
  )

  #transform
  # response$id <- as.character(response$id)
  # response$amount <- as.numeric(response$amount)
  # response$balance <- as.numeric(response$balance)

  #return
  return(response)

}



parse_response <- function(path, query = NULL) {
  #define api base url----
  #api.url <- "https://api.gdax.com"     #old API.GDAX change to Coinbase
  api.url <- "https://api.pro.coinbase.com"
  #api.url <- "https://api-public.sandbox.pro.coinbase.com"
  
  
  #create final end point----
  url <- modify_url(api.url, path = path, query = query)
  
  #fetch response----
  response <- GET(url = url)
  
  #validate success----
  if (response$status_code != 200) {
    content <- fromJSON(content(response,
                                as = "text"))
    message <- content$message
    stop(message)
  } else {
    content <- fromJSON(content(response,
                                as = "text"))
  }
  
  #return----
  return(content)
}




profile <- function(api.key, secret, passphrase) {
  #get url extension
  req.url = "/position"

  #define method
  method <- "GET"

  #fetch response
  response <- auth(
    method = method,
    req.url = req.url,
    api.key = api.key,
    secret = secret,
    passphrase = passphrase
  )

  #transform
  # drop accounts as they can be fetched by accounts.
  response <- response[-which(names(response) == "accounts")]
  response <- as.data.frame(response)

  #return
  return(response)
}




pymt_methods <- function(api.key, secret, passphrase) {
  #get url extension
  req.url = "/payment-methods"

  #get method
  method = "GET"

  #fetch response
  payment_methods <- auth(
    method = method,
    req.url = req.url,
    api.key = api.key,
    secret = secret,
    passphrase = passphrase
  )

  #return
  return(payment_methods)
}


#####


public_orderbook <- function(product_id = "ETH-EUR",level = 1) {
  #case remediation
  product_id <- toupper(product_id)

  #get url extension
  req.url <- paste0("/products/", product_id, "/book")

  #fetch response
  content <- parse_response(path = req.url,
                            query = list("level" = level))

  #transform
  if ((level == 1) || (level == 2)) {
    content$bids <- as.numeric(content$bids)
    content$asks <- as.numeric(content$asks)
    content$bids <- t(as.data.frame(content$bids))
    content$asks <- t(as.data.frame(content$asks))
    # names(content$bids) <- c("price", "size", "num_orders")
    # names(content$asks) <- c("price", "size", "num_orders")
  } else if ((level == 3)) {
    content$bids <- as.data.frame(content$bids)
    content$asks <- as.data.frame(content$asks)
    content$bids[, c(1, 2)] <-
      apply(content$bids[, c(1, 2)], 2, as.numeric)
    content$asks[, c(1, 2)] <-
      apply(content$asks[, c(1, 2)], 2, as.numeric)
    names(content$bids) <- c("price", "size", "id")
    names(content$asks) <- c("price", "size", "id")
  }

  #return
  return(content)
}

public_prices<-public_orderbook()

public_prices$asks[1]
public_prices$bids[1]
############

curr_bal_eur <- function(x){
    m <-  accounts(api.key = my_api_key, secret = my_secret, passphrase = my_passphrase) #[3,3]
    m <-  subset(m$available, m$currency == "EUR")  #USD
    m
  }

curr_bal_eur()    #Testen und debugging

#
api.key = my_api_key 
secret = my_secret 
passphrase = my_passphrase
# #
 api.key
 secret
 passphrase
#


curr_bal_eth <- function(x){
  n <- accounts(api.key = my_api_key, secret = my_secret, passphrase = my_passphrase)
  n <- subset(n$available, n$currency == 'ETH')
  n
}

curr_bal_eth()


# v.2
bid <- function(x){
  bid <- public_orderbook(product_id = "ETH-EUR", level = 1)
  bid <- bid$bids[1]
  bid
}

#bid()

ask <- function(x){
  ask <- public_orderbook(product_id = "ETH-EUR", level = 1)
  ask <- ask$asks[1]
  ask
}

ask()

######
#Account functions


eur_hold <- function(x){
  holds(currency = "EUR", my_api_key, my_secret, my_passphrase)
} 
eur_hold()


eth_hold <- function(x){
  holds <- holds(currency = "ETH", my_api_key, my_secret, my_passphrase)
  holds
}

eth_hold()

cancel_orders <- function(x){
  cancel_orders <- cancel_order(my_api_key, my_secret, my_passphrase)
  cancel_orders
}










