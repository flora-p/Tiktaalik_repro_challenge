##Title : Data Reproducibility Problem Set
getwd()
weevil_data <- read.csv(file = "mitchell_weevil_egg_data_1975.csv")

##Install Package
# install.packages("RMKdiscrete")
##Load Library
library(RMKdiscrete)

## libraries for data manipulation and plotting 
library(ggplot2)
library(tidyr)
library(dplyr)


#############################################################################
## WEEVILS !!! 


#####Step 1 : compute mean number of weevil eggs per bean ###########

# Calculate total number of beans 
total_beans <- sum(weevil_data$C_count_of_beans_with_k_eggs)

# Creating vectors of data columns
weevil_egg_number <- c(weevil_data$k_number_of_eggs)
eggs_found_in_beans <- c(weevil_data$C_count_of_beans_with_k_eggs)

# Sum to find the total number of eggs
total_eggs <- sum(weevil_egg_number * eggs_found_in_beans)

# Mean of eggs per bean
mean_of_eggs <- total_eggs/total_beans

######## Step 2: compute the Poisson/dLGP distribution with the dLGP function



##Use function dLGP from RMKdiscrete
##  initialize input as x_input

## setting lambda2 parameter 


get_dLGP_df <- function(x_input, theta, lambda2, type) {
  
  ## df with the theoretical  dLGP output
  dLGP_output <- dLGP(x = x_input, theta, lambda2)
  dLGP_output <- dLGP_output %>%
    cbind(x_input) %>%
    as.data.frame() %>%
    mutate(lambda2=lambda2, k_number_of_eggs = x_input, type=type) %>%
    select(-x_input)
  colnames(dLGP_output)[1] <- "bean_count_density"
  
  dLGP_output <- select(dLGP_output, k_number_of_eggs, bean_count_density, type, lambda2)
  
  return(dLGP_output) 
}


## OUTPUT 1 with lambda 0, i.e. Poisson distribution 

Poisson_output<- get_dLGP_df(x_input = weevil_data$k_number_of_eggs,
                             theta = mean_of_eggs, 
                             lambda2 = 0, 
                             type="Poisson fit")

## OUTPUT 2 with lambda2 as -0.24

lambda2 <- -0.24
# This parameter gives a good fit, it is negative, suggesting 
# that weevil have a repulsive attraction to beans other weevils
# have laid eggs into

dLGP_output <- get_dLGP_df(x_input = weevil_data$k_number_of_eggs,
                           theta = mean_of_eggs, 
                           lambda2 = lambda2, 
                           type=paste0("dLGP - lambda2 ", lambda2))



#### Step 3 Compute density for the original experimental weevil data 
weevil_data$bean_count_density <- weevil_data$C_count_of_beans_with_k_eggs/total_beans
weevil_data$type <- "exp_data"
weevil_data$lambda2 <- "none"

## binding rows in one dataframe 
merged <- bind_rows(Poisson_output, dLGP_output)

############## PLOTS 
## basic plot with real datapoints 
p1 <- ggplot(weevil_data, aes(x=k_number_of_eggs, y=bean_count_density))+
  geom_point(size=3, color="blue") +
  theme_minimal() +
  geom_line(color="blue") 
p1

p2 <- p1 +
  geom_point(data=merged, aes(x=k_number_of_eggs, y=bean_count_density, color=type ), size=1) +
  geom_line (data=merged, aes(x=k_number_of_eggs, y=bean_count_density, color=type ), size=1)
p2



#### Plot 2 
## Merged in one data frame for plotting everything


## setting up lambda as a character otherwise it won't merge with the weevil dataset
merged$lambda2 <- merged$lambda2 %>% as.character()

merged_full <- bind_rows(merged, weevil_data)


## Step 4 plot everything at once

p3 <- ggplot(merged_full, aes(x=k_number_of_eggs, y=bean_count_density, color=type))+
  geom_point(size=3) +
  theme_minimal() +
  geom_line() +
  labs(title="Weevil dataset - eggs in bean distribution \nwith experimental data, Poisson and dLGP fit")
p3




