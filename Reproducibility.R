##Title : Data Reproducibility Problem Set
getwd()
weevil_data <- read.csv(file = "mitchell_weevil_egg_data_1975.csv")

##Poisson distribution
$P_n(k)_{n\to \infty} = \frac{\lambda^k}{k!} e^{-\lambda}$

##Install Package
install.packages("RMKdiscrete")
##Load Library
library(RMKdiscrete)

##Calculate mean (lambda1)
total_beans <- sum(weevil_data$C_count_of_beans_with_k_eggs)

##Use function dLGP from RMKdiscrete
output <- dLGP(x = weevil_data$k_number_of_eggs, theta = mean, lambda = 0)

# Creating vectors of data columns
weevil_egg_number <- c(weevil_data$k_number_of_eggs)
eggs_found_in_beans <- c(weevil_data$C_count_of_beans_with_k_eggs)

# Sum to find the total number of eggs
total_eggs <- sum(weevil_egg_number * eggs_found_in_beans)

# Mean of eggs per bean
mean_of_eggs <- total_eggs/total_beans

