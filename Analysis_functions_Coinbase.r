#! /usr/bin/Rscript  








#libraries

require(stringi)
#require(curl)
require(xts)
require(TTR)
library(quantmod)
#library(httr)
#library(jsonlite)
require(MASS)



# Check if raspberry  or  Windows PC
raspberry = FALSE   # TRUE  or FALSE



#####
# Nelson Rules


sliding_chunker<-function(original, segment_len, slide_len){
    #split a series into a data_frame of chunks, that will be analysed
    chunks_output<-NULL
    max_chunks<-floor(length(original)/slide_len)
    for (i in 0:max_chunks){
        j <- i*slide_len
        if (j+segment_len <= length(original)){
        chunks<-original[(j+1) : (j+segment_len)]
        #
        if (i == 0){
            chunks_output<-chunks
        }
        if (i != 0){
            chunks_output <- rbind(chunks_output, chunks)
        }
        }
    }
    chunks_output <- as.data.frame(chunks_output)
    #chunks_output <- chunks_output[complete.cases(chunks_output),]
    return (chunks_output)    
}


fill_NA_results<-function(original, modified, filler = NA){
    #append NAs to fill chunker_output to original data length
    results <- modified
    max_len <- length(original)
    mod_len <- length(modified)
    mod_len
    diff <- max_len - mod_len
    if (diff > 0){
        for (i in 1: diff){
            results <- c(results, filler)
        }
    }
    return(results)
}


rule1<-function(original, mean = NULL, sigma = NULL){
    if (length(mean) == 0){
        mean = mean(original, na.rm = TRUE)
    }
    if (length(sigma) == 0){
        sigma = sd(original, na.rm = TRUE)
    }
    uplim <- mean+3*sigma
    lowlim <- mean-3*sigma
    results <- NULL
    for (i in 1:(length(original))){
        if (original[i]< lowlim){
        results <- c(results, TRUE)
        }
        if (original[i]> uplim){
            results <- c(results, TRUE)
            }
        else{
            results <- c(results, FALSE)
            }
        }
    return(results)
}



##########

rule2<-function(original, mean = NULL, sigma = NULL, filler = FALSE){
    # Nine (or more) points in a row are on the same side of mean
    if (length(mean) == 0){
        mean = mean(original, na.rm = TRUE)
    }
    if (length(sigma) == 0){
        sigma = sd(original, na.rm = TRUE)
    }
    segment_len = 9
    #
    side_of_mean = NULL
    for (i in 1:length(original)){
        if (original[i] > mean){
            side_of_mean <- c(side_of_mean, 1)
        }
        if (original[i] == mean){
            side_of_mean <- c(side_of_mean, 0)
        }
        if (original[i] < mean){
            side_of_mean <- c(side_of_mean, -1)
        }
    }
    #
    chunks <- sliding_chunker(side_of_mean, segment_len = segment_len, slide_len = 1)
    output = NULL
    Sum_chunks <- rowSums(chunks)
    #
    for (i in 1:length(Sum_chunks)){
        if (Sum_chunks[i] == segment_len){
            output<-c(output, TRUE)
        }
        else if (Sum_chunks[i] == (- segment_len)){
            output<-c(output, TRUE)
        }
        else{
            output<-c(output, FALSE)
        }
    }
    #return(chunks)
    output <- fill_NA_results(original = original, modified = output, filler = filler)
    return(output)
}

####


rule3<-function(original, mean = NULL, sigma = NULL, filler = FALSE){
    # Six or more points in a row are continually increasing / decreasing.
    #
    if (length(mean) == 0){
        mean = mean(original, na.rm = TRUE)
    }
    if (length(sigma) == 0){
        sigma = sd(original, na.rm = TRUE)
    }
    #
    segment_len = 6
    chunks <- sliding_chunker(original, segment_len = segment_len, slide_len = 1)
    #
    results <- NULL
    #
    for (i in 1:dim(chunks)[1]){
        i = 1
        chunk<- NULL
        # Test for direction 
        if (chunks[i,1] < chunks[i,2]){ #increasing
            for (j in 2:(segment_len-1)){
                if (chunks[i, j] < chunks[i, j+1]){
                    chunk<-c(chunk, 1)
                }
            }
        }
        if (chunks[i,1] > chunks[i,2]){ #decreasing
            for (j in 2:(segment_len-1)){
                if (chunks[i, j] > chunks[i, j+1]){
                    chunk<-c(chunk, 1)
                }
            }
        }
        if (sum(chunk) == (segment_len-1) ){
            results <- c(results, TRUE)
        }
        else {
            results <- c(results, FALSE)
        }
    }
    results <- fill_NA_results(original = original, modified = results, filler = filler)
    return(results)
}

####

rule4<-function(original, mean = NULL, sigma = NULL, filler = FALSE){
    #Fourteen (or more) points in a row alternate in direction, increasing, then decreasing
    if (length(mean) == 0){
        mean = mean(original, na.rm = TRUE)
    }
    if (length(sigma) == 0){
        sigma = sd(original, na.rm = TRUE)
    }
    #
    segment_len = 14
    chunks = sliding_chunker(original, 14, 1)
    chunks = sliding_chunker(original = original, segment_len = segment_len, slide_len = 1)
    if (dim(chunks)[1] == 0){
        results<-FALSE
    }
    #
    if (dim(chunks)[1] != 0){
    results <- NULL
    #
    for (i in 1:dim(chunks)[1]){
        current_state = 0
        for (j in 1:(segment_len-1)){
            if (chunks[i, j] < chunks[i, j+1]){
                direction = -1
            }
            else {
                direction = 1
            }
        }
        if (current_state != direction){
            current_state <- direction
            result <- TRUE
        }
        else {
            result <- FALSE
            break
        }
    }
    results <- c(results, result)
    }
    results<-fill_NA_results(original = original, modified = results, filler = filler)
    return(results)
}

#####

rule5<-function(original, mean = NULL, sigma = NULL, filler = FALSE){
    #Two (or three) out of three points in a row are more than 2 standard deviation from the mean in same direction
    if (length(mean) == 0){
        mean = mean(original, na.rm = TRUE)
    }
    if (length(sigma) == 0){
        sigma = sd(original, na.rm = TRUE)
    }
    #
    segment_len = 2
    chunks <- sliding_chunker(original, segment_len = segment_len, slide_len = 1)
    #
    result <- NULL
    #
    for (i in 1:dim(chunks)[1]){
        if ((chunks[i,1] > (mean + 2*sigma)) & (chunks[i,2] > (mean + 2*sigma)) ){
            result<-c(result, TRUE)
            }
        else if ((chunks[i,1] < (mean - 2*sigma)) & (chunks[i,2] < (mean - 2*sigma)) ){
            result<-c(result, TRUE)
        }
        else { result <- c(result, FALSE)}
    }
    result<-fill_NA_results(original = original, modified = result, filler = filler)
    return(result)
}

####

rule6<-function(original, mean = NULL, sigma = NULL, filler = FALSE){
    #Four or five out of five points in a row are more than 1 std in the same direction
    if (length(mean) == 0){
        mean = mean(original, na.rm = TRUE)
    }
    if (length(sigma) == 0){
        sigma = sd(original, na.rm = TRUE)
    }
    #
    segment_len = 4
    chunks <- sliding_chunker(original, segment_len = segment_len, slide_len = 1)
    #
    results <- NULL
    #
    for (i in 1: (dim(chunks)[1]) ){
        if (all(chunks[i,] > (mean+sigma)) |
            all(chunks[i,] < (mean-sigma)) ){
            results <- c(results, TRUE)
            }
        else {results <- c(results, FALSE)}
    }
    results<-fill_NA_results(original = original, modified = results, filler = filler)
    return(results)
}

###

rule7<-function(original, mean = NULL, sigma = NULL, filler = FALSE){
    # fifteen points in a row are all within 1 std of the mean
    if (length(mean) == 0){
        mean = mean(original, na.rm = TRUE)
    }
    if (length(sigma) == 0){
        sigma = sd(original, na.rm = TRUE)
    }
    #
    segment_len = 15  #15
    chunks <- sliding_chunker(original, segment_len = segment_len, slide_len = 1)
    #chunks
    #
    results <- NULL
    #
    for (i in 1: (dim(chunks)[1]) ){
        if (all( ((mean-sigma) < chunks[i,]) & (chunks[i,]< (mean+sigma)) )){
            results <- c(results, TRUE)
            }
        else {results <- c(results, FALSE)}
    }
    results<-fill_NA_results(original = original, modified = results, filler = filler)
    return(results)
}

####

rule8<-function(original, mean = NULL, sigma = NULL, filler = FALSE){
    # eight point in a row, none within 1 std
    if (length(mean) == 0){
        mean = mean(original, na.rm = TRUE)
    }
    if (length(sigma) == 0){
        sigma = sd(original, na.rm = TRUE)
    }
    #
    segment_len = 8
    chunks <- sliding_chunker(original, segment_len = segment_len, slide_len = 1)
    chunks
    #
    results <- NULL
    #
    for (i in 1: (dim(chunks)[1]) ){
        if (any( ((mean-sigma) < chunks[i,]) & (chunks[i,]< (mean+sigma)) )){
            results <- c(results, FALSE)
            }
        else {results <- c(results, TRUE)}
    }
    results<-fill_NA_results(original = original, modified = results, filler = filler)
    return(results)
}


#####

evaluate_rules<-function(original, mean = NULL, sigma = NULL, filler = FALSE){
    rule1<-rule1(original, mean = mean, sigma = sigma)
    rule2 <- rule2(original, mean = mean, sigma = sigma, filler = filler)
    rule3 <- rule3(original, mean = mean, sigma = sigma, filler = filler)
    rule4 <- rule4(original, mean = mean, sigma = sigma, filler = filler)
    rule5 <- rule5(original, mean = mean, sigma = sigma, filler = filler)
    rule6 <- rule6(original, mean = mean, sigma = sigma, filler = filler)
    rule7 <- rule7(original, mean = mean, sigma = sigma, filler = filler)
    rule8 <- rule8(original, mean = mean, sigma = sigma, filler = filler)

    result<-cbind(original, rule1)
    result<-cbind(result, rule2)
    result<-cbind(result, rule3)
    result<-cbind(result, rule4)
    result<-cbind(result, rule5)
    result<-cbind(result, rule6)
    result<-cbind(result, rule7)
    result<-cbind(result, rule8)
    
    return(result)
}


reorder<-function(data, up_till_down = TRUE){
    data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S")
    #
    if (up_till_down == TRUE){
        data<-data[order(data$time, decreasing = TRUE),] }
    else{
        data<-data[order(data$time, decreasing = FALSE),] }
    return(data)
}


slope<-function(data, number){
    num_vector <- c(number:1)
    #
    segment_len = number
    chunks <- sliding_chunker(original = data, segment_len = segment_len, slide_len = 1)
    #chunks
    #
    results <- NULL
    #
    i = 1
    unlist(chunks[i,])
    for (i in 1: (dim(chunks)[1]) ){
        unlisted<-unlist(chunks[i,])
        if (sum(is.na(unlisted)) <= 2){
            out<-lm(unlisted~num_vector)
            output<-as.numeric(out$coefficients[2])
        }
        if (sum(is.na(unlisted)) >2){
            output<-NA
        }
        results<-c(results, output)
        #results
    }
    results<-fill_NA_results(original = data, modified = results, filler = NA)
    return(results)
}


evaluate_TTR<-function(data){

    reed <- reorder(data = data, up_till_down = FALSE)
    adx <- TTR::ADX(reed[,c("high", "low", "close")])
    ema<- TTR::EMA(reed$open, n = 20)
    cema <- (data$close - ema)/ema
    macd <- TTR::MACD(reed$close)
    colnames(macd)[2]<-"macdsignal"
    rsi14 <- TTR::RSI(reed[,"close"], n = 14) 
    rsi25 <- TTR::RSI(reed[,"close"], n = 25) 
    rsi50 <- TTR::RSI(reed[,"close"], n = 50) 
    # Stochastics
    stocha <- TTR::stoch(reed[,c("high","low","close")])
    sma14 <- TTR::SMA(reed$close, n = 14)
    sma50 <- TTR::SMA(reed$close, n = 50)
    rsma14 <- reed$close/sma14
    rsma50 <- reed$close/sma50
    tdi <- TTR::TDI(reed$close)
    cmo <- TTR::CMO(reed$close, n = 14)
    kst <- TTR::KST(reed$close)
    colnames(kst)[2]<-"kstsignal"
    range <- (abs(reed$close-reed$open)/reed$close*100)
    #
    slope10<-slope(data$close, number = 10)
    slope25<-slope(data$close, number = 25)
    slope25_10<-slope(slope25, number = 10)
    slope25_5<-slope(slope25, number = 5)
    slope100<- slope(data$close, number = 100)

    # bind everything
    reed<-cbind(reed, range)
    reed<-cbind(reed, adx)
    reed<-cbind(reed, ema)
    reed<-cbind(reed, cema)
    reed<-cbind(reed, macd)
    reed<-cbind(reed, rsi14)
    reed<-cbind(reed, rsi25)
    reed<-cbind(reed, rsi50)
    reed<-cbind(reed, stocha)
    reed<-cbind(reed, sma14)
    reed<-cbind(reed, sma50)
    reed<-cbind(reed, rsma14)
    reed<-cbind(reed, rsma50)
    reed<-cbind(reed, tdi)
    reed<-cbind(reed, cmo)
    reed<-cbind(reed, kst)


    rereed<-reorder(reed, TRUE)
    
    rereed<-cbind(rereed, slope10)
    rereed<-cbind(rereed, slope25)
    rereed<-cbind(rereed, slope25_10)
    rereed<-cbind(rereed, slope25_5)
    rereed<-cbind(rereed, slope100)

    return(rereed)
}



save_data<-function(data, path, separator = "\\", name = NULL){
    #
    if (length(name) == 0){
        name = deparse(substitute(data))
    }
    if (length(name) != 0){
        name = name}
    #
    CSV <- list.files(path = path, pattern = paste0(name, ".csv") )
    CSV
    #
    your_path = paste0(path, separator, name, ".csv")
    your_path
    #
    if (length(CSV) == 0){
        # file does not exist yet, need to create it
        write.csv(data, your_path, row.names = FALSE)
    }
    # file does exist in folder it will be modified if rows differ
    if (length(CSV) != 0){
        existing_data <- read.csv(file = your_path, sep = ",")
        first.row = existing_data[1,]
        first.row_time = as.POSIXlt(first.row$time, format = "%Y-%m-%d %H:%M:%S")
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
            existing_data<-read.csv(file =  your_path, sep = ",")
            existing_data <- rbind(data, existing_data)
            #head(existing_data)
            existing_data <- existing_data[order(existing_data$time, decreasing = TRUE),]
            existing_data <- existing_data[!duplicated(existing_data$time),]
            write.csv(existing_data , your_path, row.names = FALSE)
        }
    }
}


#########

# Load data

if (raspberry == FALSE){

  existing_data1<-read.csv(file = "C:\\Users\\Heiko\\Visual.Studio\\R_Trading_Coinbase_Pro\\ETH_EUR_15.csv", sep = ",")

  existing_data2<-read.csv(file = "C:\\Users\\Heiko\\Visual.Studio\\R_Trading_Coinbase_Pro\\ETH_EUR_60.csv", sep = ",")
}

if (raspberry == TRUE){

  existing_data1<-read.csv(file = "home/pi/R/ETH.Data/ETH_EUR_15.csv", sep = ",")

  existing_data2<-read.csv(file = "home/pi/R/ETH.Data/ETH_EUR_60.csv", sep = ",")
} 



#dim(existing_data)


ed1<- existing_data1
ed1 <- reorder(data = ed1, up_till_down = TRUE)
#head(ed1)

ed1<- ed1[0:300,]

TTR_data1<-evaluate_TTR(data = ed1)


#
ed2<- existing_data2
ed2 <- reorder(data = ed2, up_till_down = TRUE)
#head(ed2)

ed2<- ed2[0:300,]

TTR_data2<-evaluate_TTR(data = ed2)


# Graphical Analysis

# plot(TTR_data$close, pch = 15, col = "blue")
# plot(TTR_data$close ~ TTR_data$macdsignal)
# plot(TTR_data$close ~ TTR_data$rsi14)
# plot(TTR_data$close ~ TTR_data$rsi25)
# plot(TTR_data$close ~ TTR_data$rsi50)
# plot(TTR_data$close ~ TTR_data$di)
# plot(TTR_data$close ~ TTR_data$range)
# plot(TTR_data$close ~ TTR_data$cmo)
# # plot(TTR_data$close ~ TTR_data$sma50)
# # plot(TTR_data$close ~ TTR_data$sma14)
# plot(TTR_data$close ~ TTR_data$rsma50)
# plot(TTR_data$close ~ TTR_data$rsma14)
# plot(TTR_data$close ~ TTR_data$cema)
# plot(TTR_data$close ~ TTR_data$DIp)
# plot(TTR_data$close ~ TTR_data$kstsignal)
# plot(TTR_data$close ~ TTR_data$slope100)
# plot(TTR_data$close ~ TTR_data$slope25)
# plot(TTR_data$close ~ TTR_data$slope25_10)
# plot(TTR_data$close ~ TTR_data$slope25_5)
# plot(TTR_data$close ~ TTR_data$slope10)
# #plot(TTR_data$close ~ TTR_data$rule8)


#head(TTR_data)


### Test save 1 und 5 Min Data

if (raspberry == FALSE){
  path = "C:\\Users\\Heiko\\Visual.Studio\\R_Trading_Coinbase_Pro"

  save_data(data = TTR_data1, path = path, separator = "\\", name = "ETH_EUR_15_Trading")
  save_data(data = TTR_data2, path = path, separator = "\\", name = "ETH_EUR_60_Trading")
}


if (raspberry == TRUE){
  path = "home/pi/R/ETH.Data"

  save_data(data = TTR_data1, path = path, separator = "/", name = "ETH_EUR_15_Trading")
  save_data(data = TTR_data2, path = path, separator = "/", name = "ETH_EUR_60_Trading")
}



