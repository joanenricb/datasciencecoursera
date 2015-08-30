rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character")
    ## Check that state and outcome are valid
    outcome_set <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outcome_ix <- outcome_set[outcome]
    states <- unique(data$State)
    if(!is.element(state,states)){
        stop("invalid state")
    }
    if(!is.element(outcome,names(outcome_set))){
        stop("invalid outcome")
    }
    data_set <- data[which(data$State == state), ]
    data_set[ ,outcome_ix] <- as.numeric(data_set[ ,outcome_ix])
    #print(data_set[,c("Hospital.Name", outcome_ix)])
    rank <- na.omit(data_set[order(data_set[,outcome_ix], data_set$Hospital.Name),])
    if(num == "best"){
        return(rank[1, "Hospital.Name"]);
    } else if(num == "worst") {
        return(tail(rank, 1)[, "Hospital.Name"]);
    } else {
        return(rank[num, "Hospital.Name"])
    }

}