
library(tidyverse)
library(rstatix)
source("utilities/multiplot.R")
source("utilities/summarySE.R")

dat <- readRDS("data/replicas/all/all_reps.rds") %>% 
    mutate(calhar = calhar/10000) %>% 
    mutate(rand = ifelse(rand<0, 0, rand)) %>% 
    mutate(sil_ln = log(sil), rand_ln = log(rand), 
           calhar_ln = log(calhar), dunni_ln = log(dunni))
    


# Summary ----

ind_descrip <- function(x){
    min <- round(min(x),1)
    max <- round(max(x),1)
    medianr <- round(median(x),2)
    meanr <- round(mean(x),2)
    sdr <- round(sd(x),2)
    
    range <- paste(min, max, sep = "-")
    ctend <- paste(meanr, paste(sdr, ")", sep = ""), sep = " (")
    paste(range, medianr, ctend, sep = "; ")
}

mt <- dat %>% 
    group_by(method) %>%
    summarise_at(vars(sil, rand, calhar, dunni), ind_descrip) 


# Tests ---- 

num <- sample(x = c(1:100), size = 1); num

onerep <- dat %>% filter(seednum==num)
anova_test(sil ~ method*num_fact, data = onerep)

