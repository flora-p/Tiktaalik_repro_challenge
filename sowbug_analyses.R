#T. rosae challenge - reproducibility

#SOWBUG ANALYSIS - QUESTIONS 2 AND 5

library(tidyverse)
library(ggplot2)
library(RMKdiscrete)

#import data
raw <- read.csv("cole_arthropod_data_1946.csv",stringsAsFactors = F)

#make new column for fraction of boards for each sowbug count:
raw$sowboardfrac <- raw$C_count_of_boards_with_k_sowbugs/sum(raw$C_count_of_boards_with_k_sowbugs)
raw$sowweightedcounts <- (raw$k_number_of_arthropods)*(raw$sowboardfrac)

#sowboardfrac <- data.frame((raw$C_count_of_boards_with_k_sowbugs)/sum(raw$C_count_of_boards_with_k_sowbugs))
#sowweightedcounts <- data.frame((raw$k_number_of_arthropods)*(sowboardfrac))
#datasowbug <- data.frame(raw, sowboardfrac, sowweightedcounts)
#colnames(datasowbug)[colnames(datasowbug)=="X.raw.C_count_of_boards_with_k_sowbugs..sum.raw.C_count_of_boards_with_k_sowbugs."] <- "sowboardfrac"
#colnames(datasowbug)[colnames(datasowbug)=="X.raw.C_count_of_boards_with_k_sowbugs..sum.raw.C_count_of_boards_with_k_sowbugs..1"] <- "sowweightedcounts"

#determine mean sowbug count

meansow <- sum(raw$sowweightedcounts)

#simple poisson distribution
raw$poissow <-dpois(raw$k_number_of_arthropods, meansow, log=FALSE)

#plot this
naiveplot <- ggplot(raw) + aes(x=raw$k_number_of_arthropods, y=raw$sowboardfrac) + geom_line() + geom_line(aes(x=raw$k_number_of_arthropods, y=raw$poissow), col="red") + xlab("k number of arthropods") + ylab("probability of finding a board with k arthropods")

ggsave("naivepoissonsowbug.png", naiveplot, dpi=1000)

##LGP 
lambda2=0.53214
lambda1=meansow*(1-lambda2)
raw$expected_lgp=dLGP(raw$k_number_of_arthropods,lambda1,lambda2)

lgpplot <- ggplot(data=raw)+ 
  geom_line(aes(x=raw$k_number_of_arthropods,y=raw$sowboardfrac,color="Real"))+
  geom_line(aes(x=raw$k_number_of_arthropods,y=raw$poissow,color="Poisson"))+
  geom_line(aes(x=raw$k_number_of_arthropods,y=raw$expected_lgp,color="LGP")) +
  xlab("number of arthropods")+ylab("Probability")+
  scale_color_manual(values = c(
    'Real' = 'black',
    'Poisson' = 'blue',
    'LGP' = 'red')) +
    labs(color = 'Distribution')

ggsave("withlgpsowbug.png", lgpplot, dpi=1000) 