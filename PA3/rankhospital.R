rankhospital <- function(state, outcome, num = "best") {
    ## 'stateName' is a 2-character abbreviated name of a state, outcome is 3 
    ## kinds of diseases, num is the ranking of a hospital in that state for
    ## that outcome
    
    ## Read outcome data
    oc <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## Check that state and outcome are valid
    ## Check state first
    if (!(state %in% oc[, "State"])) {
        stop("invalid state")
    }
    
    ## Check outcomes 
    valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% valid.outcomes)) {
        stop("invalid outcome")
    }
    
    ## Check num, take num and a list and return the filtered item in a
    check.num <- function(num, a) {
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (outcome == "heart attack") {
        heart.attack<-oc[oc$State==state, c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.Name")]
        order.heart.attack<-order(as.numeric(heart.attack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), heart.attack$Hospital.Name)
        best.ha <- heart.attack[order.heart.attack,]
        check.num(num, best.ha)
    }
    else if (outcome == "heart failure") {
        heart.failure<-oc[oc$State==state, c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.Name")]
        order.heart.failure<-order(as.numeric(heart.failure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), heart.failure$Hospital.Name)
        best.hf <- heart.failure[order.heart.failure,]
        check.num(num, best.hf)
    }
    else if (outcome == "pneumonia") {
        pneumonia<-oc[oc$State==state, c("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", "Hospital.Name")]
        order.pneumonia<-order(as.numeric(pneumonia$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), pneumonia$Hospital.Name)
        best.p <- pneumonia[order.pneumonia,]
        check.num(num, best.p)
    }
}
