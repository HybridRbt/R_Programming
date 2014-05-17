corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    df <- complete(directory)
    
    r_cor <- df[,2] > threshold
    
    valid_df <- df[r_cor,]
    
    if (length(valid_df) == 0) {
        numeric()
        return
    }
   
    wd <- getwd()
    
    if (wd != directory) {
        my_path <- paste(wd, directory, sep = "/")
    }
    # choose the files according to threshold
    file_names <- list.files(path = my_path, full.names = TRUE)
    filter <- valid_df[,1]
    choosen_files <- file_names[filter]
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    result = c(numeric(0))
    
    for (each_file in choosen_files) {
        mydata <- read.csv(each_file)
        good <- complete.cases(mydata)
        mydata_valid <- mydata[good,]
        myN <- mydata_valid[,2]
        myS <- mydata_valid[,3]
        result <- cbind(result, cor(myN, myS))
    }
    
    ## Return a numeric vector of correlations
    fr <- as.numeric(result)
}