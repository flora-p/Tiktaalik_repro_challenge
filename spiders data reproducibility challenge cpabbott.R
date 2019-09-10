##Data Reproducibility 
library(tidyverse)
library(ggplot2)

setwd("~/BSD-QBio5/tutorials/reproducibility/data")
arthro <- read.csv("cole_arthropod_data_1946.csv", stringsAsFactors = FALSE)
colnames(arthro) <- c("bug_counts", "spider_boards", "sow_boards")
View(arthro)

##Q1 and Q4
#
arthro$spider_counts <- (arthro$bug_counts*arthro$spider_boards) #total times x count of spiders found under board
sum_spiders <- sum(arthro$spider_counts)                         #
arthro$spider_prob <- (arthro$spider_counts/sum_spiders)

#find lambda
spider_lambda <- sum_spiders/sum(arthro$spider_boards)

#do Poisson dist and plot
spider_poisson <- dpois(arthro$bug_counts, spider_lambda)
ggplot(arthro, aes(x = arthro$bug_counts, y = arthro$spider_prob)) + geom_bar()
barplot(arthro$spider_prob)
plot(spider_poisson)
