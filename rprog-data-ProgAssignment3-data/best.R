best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", 
                        colClasses = "character")
    ## Check that state and outcome are valid
    outcome_set <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    states <- unique(data$State)
    if(!is.element(state,states)){
        stop("invalid state")
    }
    if(!is.element(outcome,names(outcome_set))){
        stop("invalid outcome")
    }
    data_set <- data[which(data$State == state), ]
    data_set[ ,outcome_set[outcome]] <- as.numeric(data_set[ ,outcome_set[outcome]])
    list_of_hospitals <- data_set[which(data_set[ ,outcome_set[outcome]] == min(data_set[ ,outcome_set[outcome]], na.rm = TRUE)), "Hospital.Name"]
    return(sort(list_of_hospitals)[1])
}
