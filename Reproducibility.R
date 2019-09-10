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


library(dplyr)
n_eggs <- weevil_data %>%
  transmute(n_egg =C_count_of_beans_with_k_eggs*k_number_of_eggs) 

n_eggs_total= sum(n_eggs)

mean_egg_per_bean <- n_eggs_total/n_bean_total

mean(weevil_data$k_number_of_eggs)

##Use function dLGP from RMKdiscrete
output <- dLGP(x = weevil_data$k_number_of_eggs, theta =mean_egg_per_bean, lambda = 0)

# Creating vectors of data columns
weevil_egg_number <- c(weevil_data$k_number_of_eggs)
eggs_found_in_beans <- c(weevil_data$C_count_of_beans_with_k_eggs)

# Sum to find the total number of eggs
total_eggs <- sum(weevil_egg_number * eggs_found_in_beans)

# Mean of eggs per bean
mean_of_eggs <- total_eggs/total_beans


plot_dLGP <- as.data.frame(cbind(weevil_data$k_number_of_eggs, output)) 

### PLOTTING !!

## library for plotting 
library(ggplot2)

## basic plot with real datapoints 
plot_points <- ggplot(weevil_data, aes(x=k_number_of_eggs, y=C_count_of_beans_with_k_eggs))+
  geom_point(size=3) +
  
  theme_minimal() +
  p2
plot_points +p2

## basic plot with real datapoints 
p2 <- ggplot(plot_dLGP, aes(x=V1, y=output, color="red"))+
  geom_point(size=3) +
  theme_minimal()
p2

