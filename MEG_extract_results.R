library(dplyr)
library(reshape2)
library(BayesFactor)
library(lme4)

# setwd("/home/mje/mnt/RP_meg/scratch/mne_analysis_5/exports")
setwd("~/Projects/MEG_RP_behav/")


data <- tbl_df(read.csv("MEG_exports_long.csv"))


#### Mixed models ####
m_null <- lmer(value ~ 1 + (1|id:condition) + (1|id:time) + (1|id:ROI) + (1|id),
              data = data2, REML=FALSE)


m_null <- lmer(value ~ 1 + (1|id),
              data = data2, REML=FALSE)
m_condition <- update(m_null, .~. + condition)
m_time <- update(m_condition, .~. + time)
m_roi <- update(m_time, .~. + ROI)

m_c_t <- update(m_roi, .~. + condition:time)
m_c_r <- update(m_c_t, .~. + condition:ROI)
m_t_r <- update(m_c_r, .~. + time:ROI)
m_full <- update(m_t_r, .~. + condition:time:ROI)

anova(m_null, m_condition, m_time, m_roi, m_c_t, m_c_r, m_t_r, m_full)

library(multcomp)
# linear testing
postHocs.cond<-glht(m_c_r, linfct = mcp(condition = "Tukey"))
summary(postHocs.cond)
confint(postHocs.cond)

postHocs.mod<-glht(m_c_r, linfct = mcp(ROI = "Tukey"))
summary(postHocs.mod)
confint(postHocs.mod)
 

# ~
data2 <- as.data.frame(data)
data2$id <- as.factor(data2$id)


bf <- anovaBF(value ~ condition*time*ROI, data = data2, whichRandom = "id")

full <- lmBF(value ~ condition*time*ROI+id, data = data2, whichRandom = "id")
no3inter <- lmBF(value ~ condition + time + ROI + 
                   condition:time + condition:ROI + time:ROI + id, 
                 data = data2, whichRandom = "id")
noInter1 <- lmBF(value ~ condition + time + ROI + 
                   condition:time + condition:ROI + id, 
                 data = data2, whichRandom = "id")
noInter2 <- lmBF(value ~ condition + time + ROI + 
                   condition:time + id, 
                 data = data2, whichRandom = "id")
noInter3 <- lmBF(value ~ condition + time + ROI + 
                  id, 
                 data = data2, whichRandom = "id")
onlyCondition <- lmBF(value ~ condition + id, 
                                 data = data2, whichRandom = "id")
onlyRois <- lmBF(value ~ ROI + id, 
                      data = data2, whichRandom = "id")
onlyTime <- lmBF(value ~ time + id, 
                      data = data2, whichRandom = "id")

all_bfs = c(full, no3inter, noInter1, noInter2, noInter3,
            onlyCondition, onlyRois, onlyTime)


#### permutation test ####
permutation_test <- function(a, b, number_of_permutations){
    combined = c(a,b)
  
    # Observed difference
    diff.observed = mean(b) - mean(a)
    
    number_of_permutations = number_of_permutations
    
    diff.random = NULL
    for (i in 1 : number_of_permutations) {
      
      # Sample from the combined dataset without replacement
      shuffled = sample (combined, length(combined))
      
      a.random = shuffled[1 : length(a)]
      b.random = shuffled[(length(a) + 1) : length(combined)]
      
      # Null (permuated) difference
      diff.random[i] = mean(b.random) - mean(a.random)
    }
    
    # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
    
    pvalue = sum(abs(diff.random) >= abs(diff.observed)) / number_of_permutations
    print (pvalue)
}


wide_data <- data %>%
      filter(time=="plan" & ROI=="BA39") %>% 
      spread(condition, value)

ROIS <- unique(data$ROI)
times <- unique(data$time)

pvals <- NULL
counter = 1
for (i in 1:length(ROIS)){
    for (h in 1:length(times)){
        wide_data <- data %>%
          filter(time==times[h] & ROI==ROIS[i]) %>% 
          spread(condition, value)
    pvals[counter] <- permutation_test(wide_data$classic, wide_data$plan, 100000)
    counter = counter + 1
    }
}


#### T-tests ####
ROIS <- unique(data$ROI)
times <- unique(data$time)

pvals <- NULL
counter = 1
for (i in 1:length(ROIS)){
    for (h in 1:length(times)){
        wide_data <- data2 %>%
          filter(time==times[h] & ROI==ROIS[i]) %>% 
          spread(condition, value) %>% 
          mutate(diff = classic - plan)
    pvals[counter] <- exp(ttestBF(x  = wide_data$diff)@bayesFactor$bf)
    counter = counter + 1
    }
}



