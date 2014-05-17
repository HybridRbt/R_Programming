t1 <- function() {
    pollutantmean("specdata", "sulfate", 1:10)
}

t2 <- function() {
    pollutantmean("specdata", "nitrate", 70:72)
}

t3 <- function() {
    pollutantmean("specdata", "nitrate", 23)    
}

t4 <- function() {
    complete("specdata", 1)
}

t5 <- function() {
    complete("specdata", c(2, 4, 8, 10, 12))
}

t6 <- function() {
    complete("specdata", 30:25)  
}

t7 <- function() {
    complete("specdata", 3)
}

t8 <- function() {
    corr("specdata", 150)
}

t9 <- function() {
    corr("specdata", 400)
}

t10 <- function() {
    corr("specdata", 5000)
}

t11 <- function() {
    corr("specdata")
}