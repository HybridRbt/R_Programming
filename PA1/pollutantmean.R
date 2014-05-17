pollutantmean <- function(directory, pollutant, id) {
    ##pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    wd <- getwd()
    
    if (wd != directory) {
        my_path <- paste(wd, directory, sep = "/")
    }
    
    # read in the file names
    file_names <- list.files(path = my_path, full.names = TRUE)
    choosen_files <- file_names[id]
    
    total <- 0
    count <- 0

    for (each_file in choosen_files) {
        mydata <- read.csv(each_file)
        mydata_extracted <- mydata[pollutant]
        good <- complete.cases(mydata_extracted)
        mydata_valid <- mydata_extracted[good,]
        total <- total + sum(mydata_valid) 
        count <- count + length(mydata_valid)
    }
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    avg <- as.numeric(total / count)
    avg
}