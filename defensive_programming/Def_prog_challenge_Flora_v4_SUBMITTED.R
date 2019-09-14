## Defensive programming 
## Tiktaalik group 

library(tidyverse)
setwd("C:/Users/Flora/Documents/BSD_Bootcamp_Git/Defensive prog challenge/")


## Make a function that takes the birthyear into account, the selected subtype and the data table about the prevalence of each strain
## returns a vector with the cumulative proba along the lifetime of the individual



## PSEUDO CODE OVERVIEW  
#  Compute sum of possibilities for someone to have had the flu from age 0 to max_age with a specific strain
#  at each age, the proba of 
#  max age is age the person would be when the data for the parameter estimation ends)
#  Then sum over years from birth to end of data 

## stopif max_age isn't numeric, or is <0
## p_flu_yearly needs to be numeric, >0 and <1 


## case limit testing 
# test with birthyear = or > max_year
## test with birthyear = or < min_year

#####################" FUNCTION

## INPUT INITIALIZATION
df <- read_csv("subtype_counts.csv")
p_flu_yearly <- 0.28
max_year <- max(df$Year)

## df should have a Year numerical column 



table_by_flutype <- function(df, max_Year, p_flu_yearly, flu_type_here, year_beginning, year_end) {
  # STEP 1 EARLY CHECKS
  #check if disease prevalence is within 0-1
  if (p_flu_yearly > 1 | p_flu_yearly < 0 | !is.numeric(p_flu_yearly))
    stop("Yearly flu proba should be between 0 and 1")
  
  #check if the years are numeric
  if (!is.numeric(df$Year))
    stop("The years should be numerical values")
  
  #check if the range specified is in the data
  if (year_end > max(df$Year) | year_beginning <min(df$Year))
    stop("The year range is not present in the data")
  
  ## STEP 2 
  ## this part needs to be changed if we have more flu type 
  ## COMPUTING PROBA OF GETTING EACH FLU SUBTYPE PER YEAR 
  ## proba of HXNX in year y
  df$p_H1N1_yearly = df$H1N1/(df$H1N1+df$H2N2+df$H3N2)
  df$p_H2N2_yearly = df$H2N2/(df$H1N1+df$H2N2+df$H3N2)
  df$p_H3N2_yearly = df$H3N2/(df$H1N1+df$H2N2+df$H3N2)
  
  ## putting  the dataframe into a long format for getting the yearly flu subtype probas in one column
  df_long <- df %>%
    dplyr::select(Year, p_H1N1_yearly, p_H2N2_yearly, p_H3N2_yearly) %>%
    tidyr::gather(key="flu_type", value="yearly_proba", 2:4)


## STEP 3 FOR LOOP 
  ## side note: this part is robust to a changing number of flu types ;)
## Initializing the p_flu dataframe, which will store the proba of getting type X of flu if you are born in year Y
  p_flu <- data.frame(year_birth=numeric(),
                   flu_type = character(),
                   proba_HXNX = numeric())

  
  # We loo over the birth year range we want to output 
for (year_birth in year_beginning:year_end) {
  
  ## max_age is the max age of the person for a given birthdate 
  max_age = max_year - year_birth
  
  # initialize proba 
  p_first_flu_HXNX_for_birth_Year = 0
  

  ## here, we consider 0 as the min age to get the flu ... 
  ## looping over all the years that a person born in year_birth can be 
  for (age in 0:max_age) {
    
    year_for_age <- year_birth+age

    ## getting the right proba from the yearly dataframe table 
    p_HXNX_for_year_here <- df_long %>%
      dplyr::filter(flu_type == flu_type_here & Year == year_for_age) %>%
      dplyr::select(yearly_proba) %>%
      as.numeric()

    # proba to get the H1N1 flu for the first time at age k
    p_first_HXNXflu_at_age <- (1-p_flu_yearly)^age *(p_flu_yearly*p_HXNX_for_year_here)
    
    ## Summing the probas, i.e. looping over age 
    p_first_flu_HXNX_for_birth_Year <- p_first_flu_HXNX_for_birth_Year + p_first_HXNXflu_at_age
    
    
  }
  
  #######################################"
  
  table_proba_for_birth_year <- data.frame(year_birth=year_birth,
                                           flu_type = flu_type_here,
                                           proba_HXNX = p_first_flu_HXNX_for_birth_Year)
  
  ## using bind_rows matches the column names, more rubust than a cbind
  p_flu <- dplyr::bind_rows(p_flu, table_proba_for_birth_year)
  
}

return(p_flu) 
}

## Calculating the proba by birthyear for all the range available

p_flu2 <- table_by_flutype(df, max_year, p_flu_yearly, "p_H2N2_yearly", 1960, 2017)
p_flu1 <- table_by_flutype(df, max_year, p_flu_yearly, "p_H1N1_yearly", 1960, 2017)
p_flu3 <- table_by_flutype(df, max_year, p_flu_yearly, "p_H3N2_yearly", 1960, 2017)

p_flu_all <- bind_rows(p_flu1, p_flu2, p_flu3)




###################### EXTRA STUFF 
## Independent way of testing the total Pflu 

table_first_flu <- function(df_long, max_year, p_flu_yearly, year_beginning, year_end) {
  
  p_flu <- data.frame(year_birth=numeric(),
                      flu_type = character(),
                      proba_HXNX = numeric())
  
  for (year_birth in year_beginning:year_end) {
    
    ## max_age is the max age of the person for a given birthdate 
    max_age = max_year - year_birth
    
    # initialize proba 
    p_first_flu_HXNX_for_birth_Year = 0
    
    ## here, we consider 0 as the min age to get the flu 
    ## looping over all the years 
    for (age in 0:max_age) {
      
      year_for_age <- year_birth+age
      
      # proba to get the H1N1 flu for the first time at age k
      p_first_HXNXflu_at_age <- (1-p_flu_yearly)^age *(p_flu_yearly)
      
      ## Summing the probas, i.e. looping over age 
      p_first_flu_HXNX_for_birth_Year <- p_first_flu_HXNX_for_birth_Year + p_first_HXNXflu_at_age
      
    }
    
    #######################################"
    
    table_proba_for_birth_year <- data.frame(year_birth=year_birth,
                                             proba_HXNX = p_first_flu_HXNX_for_birth_Year)
    
    p_flu <- dplyr::bind_rows(p_flu, table_proba_for_birth_year)
    
  }
  
  return(p_flu) 
}


### computing the first flu proba 
p_first_flu <- table_first_flu(df_long, max_year, p_flu_yearly, 1960, 2017)
p_first_flu$flu_type <- "first_flu_proba"



## Computing the sum of the three probas from the previous data frames

p_flu_sum <- data.frame("year_birth" = p_flu2$year_birth, 
                        "flu_type" = "sum_xp_pH1N1_pH2N2_pH3N2", 
                        "proba_HXNX" = p_flu1$proba_HXNX + p_flu2$proba_HXNX + p_flu3$proba_HXNX)


p_flu_all_with_check <- bind_rows(p_flu1, p_flu2, p_flu3, p_flu_sum, p_first_flu)


## Plotting a global graph :)
p4 <- ggplot(p_flu_all_with_check, aes(x=year_birth, y=proba_HXNX, color=flu_type)) +
  geom_point() +
  geom_line()+
  theme_minimal()+
  scale_x_discrete(limits=min(p_flu_all_with_check$year_birth):max(p_flu_all_with_check$year_birth))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.2))+
  labs(title="Probas by flu type")
p4



## ACTUAL CHALLENGE QUESTIONS 

# Probability that a person born in 1976 had primary infection with H1N1 *
  p <- p_flu_all_with_check %>%
    dplyr::filter(year_birth==1976 & flu_type =="p_H1N1_yearly")
  print(p)
  
# Probability that a person born in 1986 had primary infection with H2N2 *
  
  p <- p_flu_all_with_check %>%
    dplyr::filter(year_birth==1986 & flu_type =="p_H2N2_yearly")
  print(p)
