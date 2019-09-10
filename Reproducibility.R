##Title : Data Reproducibility Problem Set
getwd()
weevil_data <- read.csv(file =
           "mitchell_weevil_egg_data_1975.csv", )
##Poisson distribution
$P_n(k)_{n\to \infty} = \frac{\lambda^k}{k!} e^{-\lambda}$

##Install Package
install.packages("RMKdiscrete")
##Load Library
library(RMKdiscrete)

##Calculate mean (lambda1)
sum(weevil_data$C_count_of_beans_with_k_eggs)
sum(weevil_data$k_number_of_eggs)

##Use function dLGP from RMKdiscrete
output <- dLGP(x = weevil_data$k_number_of_eggs, theta = mean, lambda = 0)
