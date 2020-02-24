# R Programming Week 2 Assignment 1: Quiz

# Part 1
# Write a function named 'pollutantmean' that calculates 
# the mean of a pollutant (sulfate or nitrate) across 
# a specified list of monitors. The function 'pollutantmean' 
# takes three arguments: 'directory', 'pollutant', and 'id'. 
# Given a vector monitor ID numbers, 'pollutantmean' reads 
# that monitors' particulate matter data from the directory 
# specified in the 'directory' argument and returns the mean 
# of the pollutant across all of the monitors, ignoring any 
# missing values coded as NA.

source_path <- function (directory) {
  paste(getwd(),"/",directory, "/", sep="")
} 

source_files <- function(directory) {
  list.files(source_path(directory))
} 

pollutantmean <- function(directory, pollutant, id = 1:332) {
  file_names <- source_files(directory)
  
  # initiate an empty data frame
  data <- NA
  
  for (i in id) {
    # contruct file name
    file_name <- paste(source_path(directory), file_names[i], sep="")
    
    # open csv file
    source_data <- read.csv(file_name)
    
    # add to data frame
    data <- rbind(data,source_data)
  }
  
  # calculate mean
  mean(data[[pollutant]],na.rm = TRUE)
}

# ====== PART 1 TEST SCENARIO ======
# > source("air-pollution.R")
# > pollutantmean("specdata", "sulfate", 1:10)
# [1] 4.064128
# > pollutantmean("specdata", "nitrate", 70:72)
# [1] 1.706047
# > pollutantmean("specdata", "nitrate", 23)
# [1] 1.280833


# Part 2
# Write a function that reads a directory full of files and 
# reports the number of completely observed cases in each 
# data file. The function should return a data frame where 
# the first column is the name of the file and the second 
# column is the number of complete cases.

complete <- function(directory, id = 1:332) {
  file_names <- source_files(directory)
  
  # initiate empty vector
  ids <- c() #vector to store Id's value
  nobs <- c() #vector to store number of complete object (no missing value)
  
  # loops for each Id
  for (i in id) {
    # contruct file name
    file_name <- paste(source_path(directory), file_names[i], sep="")
    
    # open csv file
    source_data <- read.csv(file_name)
    
    # add Id to ids vector
    ids <- c(ids, i)
    
    # sum total complete cases and add it to nobs vector
    nobs <- c(nobs, sum(complete.cases(source_data)))
  }
  
  #return as data frame
  data.frame(id = ids, nobs = nobs)
}

# ====== PART 2 TEST SCENARIO ======
# > source("air-pollution.R")
# > complete("specdata", 1)
#   id nob
# 1  1 117
# > complete("specdata", c(2, 4, 8, 10, 12))
#   id  nob
# 1  2 1041
# 2  4  474
# 3  8  192
# 4 10  148
# 5 12   96
# > complete("specdata", 30:25)
#   id nob
# 1 30 932
# 2 29 711
# 3 28 475
# 4 27 338
# 5 26 586
# 6 25 463
# > complete("specdata", 3)
#   id nob
# 1  3 243


# Part 3
# Write a function that takes a directory of data files 
# and a threshold for complete cases and calculates 
# the correlation between sulfate and nitrate for monitor 
# locations where the number of completely observed cases 
# (on all variables) is greater than the threshold. The 
# function should return a vector of correlations for the 
# monitors that meet the threshold requirement. If no monitors 
# meet the threshold requirement, then the function should 
# return a numeric vector of length 0.

corr <- function(directory, threshold = 0) {
  #Get data and filter number of complete obj by threshold
  dfobservations <- complete(directory)
  dfobservations_filtered = subset(dfobservations, dfobservations$nobs > threshold)
  
  file_names <- source_files(directory)
  
  # initiate a vector
  correlation <- c()
  
  # loop each id in dfobservations_filtered:
  for (i in dfobservations_filtered$id) {
    # contruct file name
    file_name <- paste(source_path(directory), file_names[i], sep="")
    
    # open csv
    source_data <- read.csv(file_name)

    # remove NA,
    source_data <- subset(source_data,complete.cases(source_data))        
    
    # and calculate the cor and accumulate it in the corellation vector.
    correlation <- c(correlation,cor(source_data$nitrate, source_data$sulfate))    
  }
  #Finally, return the vector
  correlation
}

# ====== PART 3 TEST SCENARIO ======
# > source("air-pollution.R")
# > cr <- corr("specdata", 150)
# > head(cr)
# [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
# > summary(cr)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 
# > cr <- corr("specdata", 400)
# > head(cr)
# [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
# > summary(cr)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313 
# > cr <- corr("specdata", 5000)
# > summary(cr)
# Length  Class   Mode 
# 0   NULL   NULL 
# > length(cr)
# [1] 0
# > cr <- corr("specdata")
# > summary(cr)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 
# > length(cr)
# [1] 323