rankall <- function(outcome, num = "best") {
    ## Read outcome data
    oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## Check outcomes 
    valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% valid.outcomes)) {
        stop("invalid outcome")
    }
    
    check.num1 <- function(num, a) {
        if (num == "best") {
            a[1, 2]
        }
        else if (num == "worst") {
            good <- complete.cases(a[,1])
            completed.a <- a[good,]
            a[length(completed.a[,1]), 2]
        }
        else {
            n <- as.numeric(num)
            good <- complete.cases(a[,1])
            completed.a <- a[good,]
            if (n > length(completed.a[,1])){
                rank <- NA
                rank
            }
            else {
                rank <- n
                a[rank, 2]
            }
        }
    }
    
    check.num2 <- function(num, a) {
        if (num == "best") {
            a[1, 3]
        }
        else if (num == "worst") {
            good <- complete.cases(a[,1])
            completed.a <- a[good,]
            a[length(completed.a[,1]), 3]
        }
        else {
            n <- as.numeric(num)
            good <- complete.cases(a[,1])
            completed.a <- a[good,]
            if (n > length(completed.a[,1])){
                st <- completed.a$State
                st
            }
            else {
                rank <- n
                a[rank, 3]
            }
        }
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    oc.state.list <- split(oc, oc$State)
    output1 <- rep("", 54)
    output2 <- rep("", 54)
    n <- 1
    
    for (each in oc.state.list) {
        if (outcome == "heart attack") {
            heart.attack<-each[, c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.Name", "State")]
            order.heart.attack<-order(as.numeric(heart.attack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), heart.attack$Hospital.Name)
            best.ha <- heart.attack[order.heart.attack,]
            output1[n] <- check.num1(num, best.ha)
            output2[n] <- check.num2(num, best.ha)
            n <- n + 1
        }
        else if (outcome == "heart failure") {
            heart.failure<-each[, c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.Name", "State")]
            order.heart.failure<-order(as.numeric(heart.failure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), heart.failure$Hospital.Name)
            best.hf <- heart.failure[order.heart.failure,]
            output1[n] <- check.num1(num, best.hf)
            output2[n] <- check.num2(num, best.hf)
            n <- n + 1
        }
        else if (outcome == "pneumonia") {
            pneumonia<-each[, c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", "Hospital.Name", "State")]
            order.pneumonia<-order(as.numeric(pneumonia$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), pneumonia$Hospital.Name)
            best.p <- pneumonia[order.pneumonia,]
            output1[n] <- check.num1(num, best.p)
            output2[n] <- check.num2(num, best.p)
            n <- n + 1
        }
    }
    output <- cbind(output1, output2)
    output <- data.frame(output, row.names = output2)
    colnames(output) <- c("hospital", "state")
    output
}
