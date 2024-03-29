data = read.csv("/Users/xli/Documents/xinyi/UChicago/BSD-QBio5/tutorials/defensive_programming/data/subtype_counts.csv",stringsAsFactors = F)

<<<<<<< Updated upstream
calculate_prob <-function (dz_prev, birthyear, prob_data, subtype)
=======
data = read.csv("C:/Users/Flora/Desktop/MBL_Bootcamp/BSD-QBio5/tutorials/defensive_programming/data/subtype_counts.csv",stringsAsFactors = F)
getwd()

calculate_prob <-function (birthyear, prob_data, subtype)
>>>>>>> Stashed changes
{
 #dz_prev: disease prevalence; birthyear: the birth year of the individual ; prob_data: count data of each subtype 
 #subtype: "H1N1", "H3N2", or "H2N2" 
 #check if birthyear is positive and integer 
  if (birthyear < 0 | birthyear!=round(birthyear)) 
    stop("Your birthyear should be positive integer")
  #check if the year is in the data 
  if (!birthyear %in% prob_data$Year)
    stop("Your birthyear data is not in the dataset")
  #check if your subtype is in the dataset 
  if (!subtype %in% colnames(prob_data))
    stop("We don't have your subtype information")
  #check if row sums are zero
  if (sum(rowSums(prob_data[,c(2:4)])==0)!=0)
    stop("The row sum of count data should not be zero")
  #check if disease prevalence is 0-1 
  if (dz_prev > 1 | dz_prev < 0 )
    stoop("disease prevalence should be between 0-1")
 
  #find the maximum year in calculating probability  
  endyear = max(prob_data$Year)
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
      prob[i] = dz_prev * data[data$Year==birthyear,subtype_col]/sum(data[data$Year==birthyear,c(2:4)])
    #the rest of year probability: last_year_probability + (1-0.28)^(n-1)*0.28*P 
    else 
      prob[i] = prob[i-1]+ (1-dz_prev)^(i-1)* dz_prev *
        data[data$Year==(birthyear+i-1),subtype_col]/sum(data[data$Year==(birthyear+i-1),c(2:4)])
  }
  return(prob)
}

<<<<<<< Updated upstream
#calculate_prob(0.28, 1967, data, "H1N1")
#create a table to store all the birthyear probability 
all_pre = matrix(0, nrow= max(data$Year)-min(data$Year)+1,ncol=3)
subtype = c("H3N2","H1N1","H2N2")
birthyear=c(min(data$Year):max(data$Year))

#loop through each year 
for (i in 1:(length(birthyear)))
{
#loop through each subtype 
  for (j in 1:length(subtype))
  {
    all_pre[i,j] = max(calculate_prob(0.28,birthyear[i],data,subtype[j]))
  }
}
colnames(all_pre) = c("H3N2","H1N1","H2N2")
rownames(all_pre) = birthyear
=======

p <- calculate_prob(1962, data, "H1N1")
p_sum <- sum(p)
p_sum

p <- calculate_prob(1962, data, "H3N2")
p_sum <- sum(p)
p_sum

p <- calculate_prob(1960, data, "H2N2")
p <- max(p)
p
>>>>>>> Stashed changes
