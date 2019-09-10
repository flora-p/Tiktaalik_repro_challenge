
# Title: Data Reproducibility Problem Set, Weevil Questions

# Authors: Flora Plessier, Emma Wilkinson, Christopher Luong

# Check working directory
getwd()

# Opening Weevil dataframe
weevil_data <- read.csv(file = "mitchell_weevil_egg_data_1975.csv")

## library RMKdiscrete
library(RMKdiscrete)

## library for plotting 
library(ggplot2)

## basic plot with real datapoints 
plot_points <- ggplot(weevil_data, aes(x=k_number_of_eggs, y=C_count_of_beans_with_k_eggs))+
  geom_point(size=3) +
  theme_minimal()
plot_points

  