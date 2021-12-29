## best is a function to find best hospital for a given outcome (heart attack, heart failure or pneumonia) in a given state.

best <- function(state, outcome) {

        ## reads the data from working directory and save it in a variable called mydata.
        
        mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        ## converts required data into a data frame.
        
        sub_mydata <-as.data.frame(cbind(mydata[,2], 
                                         mydata[,7], 
                                         mydata[,11], 
                                         mydata[,17], 
                                         mydata[,23]), 
                                   stringsAsFactors = FALSE)

        ## changes the columns name of data stored in sub_mydata.
       
        colnames(sub_mydata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
       
        ## finds the unique (non-repeated) of states and stores them in unique_states.
        
        unique_states <- unique(sub_mydata$state)
        
        ## generates a list of valid outcomes.
        
        valid_outcomes <- c("heart attack", "heart failure","pneumonia")
        
        ## checks if the given state is valid or not.
        
        if (!state %in% unique_states){
                     print("Invalid State")
        }
        
        ## checks if the given outcome is valid or not.
        
        if (!outcome %in% valid_outcomes){
                print("Invalid Outcome")
        }
        
        ## matches the hospital data with the given state.
        
        hospital <- sub_mydata[(sub_mydata$state==state),]
        
        ## changes the data for given outcome into numeric.
        
        hospital[, outcome] <- as.numeric(hospital[, outcome])
        
        ## removes the NA values from data stored in the hospital.
        
        hospital <- hospital[!is.na(hospital[, outcome]),]
        
        ## reorders the data stored in hospital.
        
        hospital <- hospital[order(hospital[, outcome]),]
        
        ## gets the name of best hospital (minimum mortality rate for the given outcome).
        
        hospitalname <- hospital[hospital[, outcome] == min(hospital[,outcome]),1]
        
        ##prints the name of best hospital.
        print(hospitalname)
}