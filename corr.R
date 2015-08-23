corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    over_threshold = complete_count_bigger_thr(complete(directory), threshold)
    cr <- c()
    for(i in over_threshold$id){
        myFile <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep = "")
        data_file <- read.csv(myFile)
        data_file <- data_file[complete.cases(data_file),]
        cr <- c(cr, cor(data_file$sulfate, data_file$nitrate))
    }
    return(cr)
}

complete_count_bigger_thr <- function(df, threshold){
    ## retuns the ids with a count bigger than threshold
    return(df[df["nobs"] > threshold,])
}