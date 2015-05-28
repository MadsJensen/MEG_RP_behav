# setwd("/home/mje/Projects/MEG_RP_behav")

setwd("/home/mje/Projects/MEG_RP_behav/Int_binding/data")


ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory

for (k in 1:length(listcsv)){
    ldf[[k]] <- read.csv(listcsv[k], header = FALSE)
}
