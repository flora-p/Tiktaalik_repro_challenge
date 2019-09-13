##Data Reproducibility 
library(tidyverse)
library(ggplot2)

setwd("~/BSD-QBio5/tutorials/reproducibility/data")
arthro <- read.csv("cole_arthropod_data_1946.csv", stringsAsFactors = FALSE)
colnames(arthro) <- c("bug_counts", "spider_boards", "sow_boards")
View(arthro)

##Q1 and Q4
#make new columns for counting times certain # of spiders found per board and probability of finding a # of spiders per board
arthro$spider_counts <- arthro$bug_counts*arthro$spider_boards #total times x count of spiders found under board
sum_spiders <- sum(arthro$spider_counts)                         #
arthro$spider_prop <- arthro$spider_counts/sum_spiders

#find lambda
spider_lambda1 <- sum_spiders/sum(arthro$spider_boards)

#do Poisson dist and plot
arthro$spider_poisson <- dpois(arthro$bug_counts, spider_lambda1)
spider_plot <- ggplot(arthro) + aes(x = arthro$bug_counts, y = arthro$spider_prop) + 
  geom_line()  + 
  geom_line(aes(x = arthro$bug_counts, y = spider_poisson), col="blueviolet")+
  xlab("Number of spiders found per board") + 
  ylab("Probability of finding n spiders")
spider_plot
#barplot(arthro$spider_prop)
#ggplot(arthro, aes(spider_prop))+
         #geom_bar()+
         #xlab("Probability of finding spiders under a board") #don't know why i made this but here's this broken plot

##second lambda as defined by challenge
spider_lambda2 <- 0

##LGP
arthro$expec_lgp <- dLGP(arthro$bug_counts,spider_lambda1,spider_lambda2)

lgpplot <- spider_plot + 
  geom_line(aes(x=arthro$bug_counts, y=arthro$expec_lgp),col="orangered3")
lgpplot #they're the exact same and i am frustrate

lgpplot2 <- spider_plot + 
  geom_line(aes(x=arthro$bug_counts, y=arthro$expec_lgp), position = position_nudge(x=1),col="orangered3")
lgpplot2 #put some jitter on it to prove it...why is this like this
