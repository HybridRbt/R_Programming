best <- function(state, outcome) {
    ## 'stateName' is a 2-character abbreviated name of a state
   
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if (outcome == "heart attack") {
        heart.attack<-oc[oc$State==state, c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.Name")]
        order.heart.attack<-order(heart.attack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, heart.attack$Hospital.Name)
        best.ha <- heart.attack[order.heart.attack,]
        best.ha[1, 2]
    }
    else if (outcome == "heart failure") {
        heart.failure<-oc[oc$State==state,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
        order.heart.failure<-order(as.numeric(heart.failure))
        best.hf<-oc[order.heart.failure[1], "Hospital.Name"]
        best.hf
    }
    else if (outcome == "pneumonia") {
        pneumonia<-oc[oc$State==state,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]
        order.pneumonia<-order(as.numeric(pneumonia[,2]))
        best.p<-oc[order.pneumonia[1], "Hospital.Name"]
        best.p
    }
}
