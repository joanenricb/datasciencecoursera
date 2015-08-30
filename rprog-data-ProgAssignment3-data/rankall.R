rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character")
    ## Check that state and outcome are valid
    outcome_set <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    if(!is.element(outcome,names(outcome_set))){
        stop("invalid outcome")
    }
    outcome_ix <- outcome_set[outcome]
    states <- sort(unique(data$State))
    rank_all <- rep("", length(states))
    data[ ,outcome_ix] <- as.numeric(data[ ,outcome_ix])
    for(i in 1:length(states)){
        data_state <- data[which(data$State == states[i]),]
        data_state <- na.omit(data_state[order(data_state[,outcome_ix], data_state$Hospital.Name),])
        if(num == "best"){
            rank <- head(data_state, 1)["Hospital.Name"]
        } else if(num == "worst") {
            rank <- tail(data_state, 1)["Hospital.Name"]
        } else {
            rank <- data_state[num, "Hospital.Name"]
        }
        rank_all[i] <- rank[[1]]
    }
    df <- data.frame(hospital=rank_all,state=states)
    return(df)
    

}
