complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    wd <- getwd()
    
    if (wd != directory) {
        my_path <- paste(wd, directory, sep = "/")
    }
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    file_names <- list.files(path = my_path, full.names = TRUE)
    choosen_files <- file_names[id]
    
    df <- c(numeric(), numeric())
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    for (each_file in choosen_files) {
        mydata <- read.csv(each_file)
        good <- complete.cases(mydata)
        mydata_valid <- mydata[good,]
        df <- rbind(df, c(mydata_valid[1,4], length(mydata_valid[,1])))
    }
    
    colnames(df) <- c("id", "nobs")
    rownames(df) <- c(1:length(df[,1]))
    df <- data.frame(df)
    df
}