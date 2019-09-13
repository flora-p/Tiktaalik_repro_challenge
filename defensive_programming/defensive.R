data = read.csv("/Users/xli/Documents/xinyi/UChicago/BSD-QBio5/tutorials/defensive_programming/data/subtype_counts.csv",stringsAsFactors = F)

calculate_prob <-function (birthyear, prob_data, subtype)
{
 #check if birthyear is positive and integer 
  if (birthyear < 0 | birthyear!=round(birthyear)) 
    stop("Your birthyear should be positive integer")
  #check if the year is in the data 
  if (!birthyear %in% prob_data$Year)
    stop("Your birthyear data is not in the dataset")
  #check if your subtype is in the dataset 
  if (!subtype %in% colnames(prob_data))
    stop("We don't have your subtype information")
 
  #find the maximum year in calculating probability  
  endyear =max(prob_data$Year)
  #find the number of years from birth to the maximum year
  year_range = endyear-birthyear 
  #initialize an empty probability vector 
  prob = rep(0,year_range) 
  subtype_col = which(colnames(data)==subtype)
  #for loop to calculate each year's probability of getting disease 
  for (i in 1:year_range)
  {
    #first year probability: 0.28*P 
    if (i==1)
      prob[i] = 0.28* data[data$Year==birthyear,subtype_col]/sum(data[data$Year==birthyear,c(2:4)])
    #the rest of year probability: last_year_probability + (1-0.28)^(n-1)*0.28*P 
    else 
      prob[i] = prob[i-1]+ (1-0.28)^(i-1)* 0.28 *
        data[data$Year==(birthyear+i-1),subtype_col]/sum(data[data$Year==(birthyear+i-1),c(2:4)])
  }
  return(prob)
}