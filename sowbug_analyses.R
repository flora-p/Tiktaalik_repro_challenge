#T. rosae challenge - reproducibility

#SOWBUG ANALYSIS - QUESTIONS 2 AND 5

library(tidyverse)
library(ggplot2)
library(RMKdiscrete)

#import data
raw <- read.csv("cole_arthropod_data_1946.csv")

#make new column for fraction of boards for each sowbug count:
sowboardfrac <- data.frame((raw$C_count_of_boards_with_k_sowbugs)/sum(raw$C_count_of_boards_with_k_sowbugs))
sowweightedcounts <- data.frame((raw$k_number_of_arthropods)*(sowboardfrac))
datasowbug <- data.frame(raw, sowboardfrac, sowweightedcounts)

colnames(datasowbug)[colnames(datasowbug)=="X.raw.C_count_of_boards_with_k_sowbugs..sum.raw.C_count_of_boards_with_k_sowbugs."] <- "sowboardfrac"
colnames(datasowbug)[colnames(datasowbug)=="X.raw.C_count_of_boards_with_k_sowbugs..sum.raw.C_count_of_boards_with_k_sowbugs..1"] <- "sowweightedcounts"

#determine mean sowbug count

meansow <- sum(datasowbug$sowweightedcounts)


#simple poisson distribution
poissow <-dpois(datasowbug$k_number_of_arthropods, meansow, log=FALSE)

#plot this
ggplot(datasowbug) + aes(x=datasowbug$k_number_of_arthropods, y=datasowbug$sowboardfrac) + geom_line() + geom_line(aes(x=datasowbug$k_number_of_arthropods, y=poissow), col="red") + xlab("k number of arthropods") + ylab("probability of finding a board with k arthropods")