








#libraries

require(stringi)
require(curl)
require(xts)
require(TTR)
library(quantmod)
library(httr)
library(jsonlite)



#####
# Nelson Rules

existing_data<-read.csv(file = "C:\\Users\\Heiko\\Visual.Studio\\R_Trading_Coinbase_Pro\\ETH_USD_1.csv", sep = ",")
existing_data$open




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


original <- dd
modified <- rule2(dd)


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
    chunks = sliding_chunker(dd, 14, 1)
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
    segment_len = 5  #15
    chunks <- sliding_chunker(original, segment_len = segment_len, slide_len = 1)
    chunks
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




#####
dd<-existing_data$open

original <- dd

rule6(dd, sigma = 0.02)

rule7(dd, sigma = 0.3)



