setwd("/home/mje/Projects/MEG_RP_behav/Int_binding/data")
# source("/Users/mads/Projects/int_bind_foot-hand/importFun.R")

library(dplyr)


subs = c(2, 3, 7, 8, 9, 10, 12, 13, 14, 15, 16)



importData_plan <- function(subject_id){  
  sub.data <- read.csv(paste(c("subject_",subject_id,"_singlePress",".csv")
                               ,collapse=''), sep=";",header=TRUE, 
                         stringsAsFactors = FALSE)
  sub.data$id <- as.factor(sub.data$id)
  
  
  recalc.1 <- ifelse(sub.data$ansAngle < 90 & sub.data$pressAngle > 270,T,F)
  sub.data$ansAngle[recalc.1] <- sub.data$ansAngle[recalc.1]+360             # +1,+H
  recalc.2 <- ifelse(sub.data$ansAngle>270 & sub.data$pressAngle<90,T,F)
  sub.data$pressAngle[recalc.2] <- sub.data$pressAngle[recalc.2]+360           # +H,+1
  sub.data$recalc <- recalc.1 | recalc.2
  
  sub.data$errorAngle <- sub.data$ansAngle-sub.data$pressAngle
  sub.data$errorTime <- sub.data$errorAngle*2560/360
  
  #   sub.data$condition <- replace(sub.data$condition, sub.data$condition=="Libet2","Libet")
  #   sub.data$condition <- replace(sub.data$condition, sub.data$condition=="selfChoice2","selfChoice")
  #   sub.data$condition <- replace(sub.data$condition, temp.dat$condition=="preChoice2","preChoice")
  
  return(sub.data)
}

importData_cls <- function(subject_id){  
  sub.data <- read.csv(paste(c("subject_",subject_id,
                               "_singlePress_classic",".csv")
                               ,collapse=''), sep=";",header=TRUE, 
                         stringsAsFactors = FALSE)
  sub.data$id <- as.factor(sub.data$id)
  
  
  recalc.1 <- ifelse(sub.data$ansAngle < 90 & sub.data$pressAngle > 270,T,F)
  sub.data$ansAngle[recalc.1] <- sub.data$ansAngle[recalc.1]+360             # +1,+H
  recalc.2 <- ifelse(sub.data$ansAngle>270 & sub.data$pressAngle<90,T,F)
  sub.data$pressAngle[recalc.2] <- sub.data$pressAngle[recalc.2]+360           # +H,+1
  sub.data$recalc <- recalc.1 | recalc.2
  
  sub.data$errorAngle <- sub.data$ansAngle-sub.data$pressAngle
  sub.data$errorTime <- sub.data$errorAngle*2560/360
  
  #   sub.data$condition <- replace(sub.data$condition, sub.data$condition=="Libet2","Libet")
  #   sub.data$condition <- replace(sub.data$condition, sub.data$condition=="selfChoice2","selfChoice")
  #   sub.data$condition <- replace(sub.data$condition, temp.dat$condition=="preChoice2","preChoice")
  
  return(sub.data)
}

for (ii in subs) {
  nam <- paste("subject_",ii,"_plan", sep = "")
  sub.data <- importData_plan(ii)
  sub.data <- tbl_df(sub.data)
  sub.data$condition <- "plan"
  assign(nam, sub.data)
}

subject_4_plan <- importData_plan("04")
subject_4_plan <- tbl_df(subject_4_plan)
subject_4_plan$condition <- "plan"



plan_list <- ls(pattern = "plan")
plan_list <- plan_list[2:13]
all_plan = get(plan_list[2:13])

for (j in 1:length(plan_list)) {
  if (j == 1) {
    all_plan <- get(plan_list[j])
  }
  else
  {
    all_plan <- bind_rows(all_plan, get(plan_list[j]))
  }
}



subs = c(7, 8, 9, 10, 12, 13, 14, 15, 16)

for (ii in subs) {
  nam <- paste("subject_",ii,"_classic", sep = "")
  sub.data <- importData_cls(ii)
  sub.data <- tbl_df(sub.data)
  sub.data$condition <- "classic"
  assign(nam, sub.data)
}

subject_4_classic <- importData_cls("04")
subject_4_classic <- tbl_df(subject_4_classic)
subject_4_classic$condition <- "classic"



classic_list <- ls(pattern = "classic")
classic_list <- classic_list[2:10]
all_classic = get(classic_list[1])

for (j in 1:length(classic_list)) {
if (j == 1) {
  all_classic <- get(classic_list[j])
}
else
{
  all_classic <- bind_rows(all_classic, get(classic_list[j]))
}
}

all_data <- bind_rows(all_classic, all_plan)
all_data %>% group_by(condition) %>%
  summarise(mean = mean(errorTime), sd=sd(errorTime))

classic_data <- all_data %>% filter(condition == "classic") %>%
  group_by(id) %>% 
  summarise(data = mean(errorTime))

plan_data <- all_data %>% filter(condition == "plan") %>%
  group_by(id) %>% 
  summarise(data = mean(errorTime))

t.test(plan_data$data, classic_data$data)
var.test(plan_data$data, classic_data$data)



