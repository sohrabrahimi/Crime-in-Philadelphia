#############################################################################
#### This code prepares the block-year-month data to conduct RD analysis ####
#############################################################################

#### Author(s): SSP & RBM

#### Date Last Modified: 11/29/2016
#### Modified by: SSP
#### File: "~/Code/R/02-data_cleaning/02-airbnb_crime_block_group_merging.R"

rm(list=ls())
options(stringsAsFactors = F)
gc()

library(plyr)
library(stringr)
library(reshape2)
library(zoo)


##############################
### 1) SET UP
##############################

setwd("~/SODA502") #SSP

#### Bring-in required data
abb <- read.csv(file = "./Data/Airbnb/Airbnb_block.csv")
abb_r <- read.csv(file = "./Data/Airbnb/Philly_Reviews.csv")
crime <- read.csv(file = "./Data/Crime/crime_year_month_type_block.csv")

#### Variable for the number of months before and after AirBnB introduction
####  to evaluate crime for:
k <- 3
#### In other words: evaluate crime at 
####  t-k, t-(k-1), t-(k-1), ... t0, t+1, t+2, ... t + k, 
####  where t0 = introduction of AirBnB.

##############################
### 2) CLEAN AIRBNB DATA:
##############################

#### We first go into each block group and find the first AirBnb there

# Make the "Review Created At" variable into POSIX format:
abb_r$created_at <- gsub(pattern = "T", replacement = " ", 
                         x = abb_r$created_at, ignore.case = F)
abb_r$created_at <- strptime(abb_r$created_at, format = "%Y-%m-%d %H:%M:%S")

# Create a variable to join on
names(abb)[which(names(abb) == "id")] <- "listing_id"

# Create a join that includes "listing_id", "created_at", and "GEOID":
abb_join <- join(abb, abb_r, by = "listing_id")
abb <- abb_join[is.na(abb_join$created_at) == FALSE, c(45,125,150)]

# Clean up unneccessary data frames:
rm('abb_r', 'abb_join')


## Produce the "final" AirBnB data set:

# For each listing id, find the date of first review:
abb_final <- by(data = abb, 
                INDICES = abb$GEOID, 
                FUN = function(x) x[order(x$created_at),][1,])

# Move list into data frame:
abb_final <- do.call(rbind, abb_final)

# Make listing ID a character vector:
abb_final$GEOID <- as.character(abb_final$GEOID)

# Remove unneccesary data frame:
rm(abb)


# Transform the "created_at" date to year-month formatting (to match crime data):
abb_final$yearmon <- as.yearmon(x = abb_final$created_at, 
                                format = "%Y-%m-%d %H:%M:%S")



##############################
### 3) CLEAN CRIME DATA
##############################

## Remove the second column ("X.1"), which serves no purpose:
crime <- crime[,-2]

#### Pull the GEOID and year-month from the id variable "X":
# GEOID extraction:
crime$GEOID <- substr(x = crime$X, start = 1, stop = 12)

# year-month extraction:
crime$yearmon <- substr(x = crime$X, start = 14, stop = 20)
crime$yearmon <- gsub(pattern = "_", replacement = "-", x = crime$yearmon)
crime$yearmon <- as.yearmon(x = crime$yearmon, format = "%Y-%m")




################################
### 4) DATA SUBSETTING
################################

## Subsetting by date:

## NOTE: The variable "k" is set at the top of the script. Default is 3.

# first airbnb date:
min.date.abb <- min(abb_final$yearmon)

# start cutoff for crime:
min.date.crime <- min.date.abb - (k/12)

# end cutoffs for airbnb and crime:
end.date.abb <- as.yearmon("2016-05") # Up to May, 2016
end.date.crime <- as.yearmon("2016-08") # Up to Aug, 2016


# Remove the AirBnB data that is too new:
abb_final <- abb_final[which(abb_final$yearmon <= end.date.abb),] 

# Remove the crime data for months that haven't yet occurred:
crime <- crime[which(crime$yearmon <= end.date.crime),] 

# Remove the crime data for months way before the airbnb data starts:
crime <- crime[which(crime$yearmon >= min.date.crime),]

# Remove airbnb data that we don't have crime for:
abb_final <- abb_final[which(abb_final$GEOID %in% crime$GEOID),]

# Remove crime data that we don't have airbnb data for:
crime <- crime[which(crime$GEOID %in% abb_final$GEOID),]



################################
### 5) EXPAND AIRBNB DATA
################################



## Here we're going to expand the AirBnb data so that each unit appears 
##  (2*k) + 1 times in the data (k months before to k months after)
##  intro of unit:


# Sequence of time before and after airbnb treatment:
time.seq <- c(-k:k)[-(k+1)]


# Make a temp version of AirBnB data:
abb.temp <- abb_final

# Generate new column for time variable:
abb.temp$t <- 0

# Put AirBnB data into list where each element is a block group:
abb.temp <- by(data = abb.temp, INDICES = abb.temp$GEOID, FUN = subset)

# loop over block groups and expand the data:
for (i in seq_along(abb.temp)){
  
  block <- abb.temp[[i]] # isolate element
  
  for (j in seq_along(time.seq)){
    
    temp.block <- block[1,] # create the baseline
    
    # calc lag(lead) year-month
    temp.block$yearmon <- temp.block$yearmon + (time.seq[j]/12) 
    temp.block$t <- time.seq[j]
    block <- rbind(block, temp.block)
    
  }
  abb.temp[[i]] <- block
}

# Remove junk:
rm('i', 'j', 'block', 'temp.block')

## Combine list of expanded data frames into one big data frame:
abb_final <- do.call(rbind, abb.temp)

# Remove temp airbnb data frame:
rm(abb.temp)


################################
### 6) MERGE AIRBNB & CRIME
################################


## New dataframe "df" from merged airbnb and crime:
df <- merge(x = abb_final, y = crime, by = c("GEOID", "yearmon"))

## Remove the variable "X" from dataframe (leftover blockgroup-date ID from crime):
df <- df[,-which(colnames(df)=="X")]

## Rename a column:
to.change <- which(colnames(df) == "Other.Sex.Offenses..Not.Commercialized.")
colnames(df)[to.change] <- "Sex.Offend.Other"
rm(to.change)

## Generate a "month" variable (control for seasonality):
df$month <- format(x = df$yearmon, format = "%m")

## Remove the crime and airbnb dataframes:
rm('abb_final', 'crime')


################################
### 7) WRITE OUT DATA:
################################

# dir.create(path = "./Data/Merged/")
setwd("./Data/Merged/")
write.csv(x = df, file = "./01-airbnb_crime_month_block_group_clean.csv", 
          row.names = F)




#####################################################
#### Repeat steps 4-7 with different values of k ####
##### We consider 1 and 6 months prior and post #####
######## All but one line in Step 4 the same ########
#####################################################

####
#### 1 month prior and post
####

k <- 1

################################
### 4) DATA SUBSETTING
################################

## Subsetting by date:

## NOTE: The variable "k" is set at the top of the script. Default is 3.

# first airbnb date:
min.date.abb <- min(abb_final$yearmon)

# start cutoff for crime:
min.date.crime <- min.date.abb - (k/12)

## end.date.abb changed to accommodate the fact that we are interestd in 6 months prior
# end cutoffs for airbnb and crime:
end.date.abb <- as.yearmon("2016-07") # Up to May, 2016
end.date.crime <- as.yearmon("2016-08") # Up to Aug, 2016


# Remove the AirBnB data that is too new:
abb_final <- abb_final[which(abb_final$yearmon <= end.date.abb),] 

# Remove the crime data for months that haven't yet occurred:
crime <- crime[which(crime$yearmon <= end.date.crime),] 

# Remove the crime data for months way before the airbnb data starts:
crime <- crime[which(crime$yearmon >= min.date.crime),]

# Remove airbnb data that we don't have crime for:
abb_final <- abb_final[which(abb_final$GEOID %in% crime$GEOID),]

# Remove crime data that we don't have airbnb data for:
crime <- crime[which(crime$GEOID %in% abb_final$GEOID),]



################################
### 5) EXPAND AIRBNB DATA
################################



## Here we're going to expand the AirBnb data so that each unit appears 
##  (2*k) + 1 times in the data (k months before to k months after)
##  intro of unit:


# Sequence of time before and after airbnb treatment:
time.seq <- c(-k:k)[-(k+1)]


# Make a temp version of AirBnB data:
abb.temp <- abb_final

# Generate new column for time variable:
abb.temp$t <- 0

# Put AirBnB data into list where each element is a block group:
abb.temp <- by(data = abb.temp, INDICES = abb.temp$GEOID, FUN = subset)

# loop over block groups and expand the data:
for (i in seq_along(abb.temp)){
  
  block <- abb.temp[[i]] # isolate element
  
  for (j in seq_along(time.seq)){
    
    temp.block <- block[1,] # create the baseline
    
    # calc lag(lead) year-month
    temp.block$yearmon <- temp.block$yearmon + (time.seq[j]/12) 
    temp.block$t <- time.seq[j]
    block <- rbind(block, temp.block)
    
  }
  abb.temp[[i]] <- block
}

# Remove junk:
rm('i', 'j', 'block', 'temp.block')


## Combine list of expanded data frames into one big data frame:
abb_final <- do.call(rbind, abb.temp)


# Remove temp airbnb data frame:
rm(abb.temp)


################################
### 6) MERGE AIRBNB & CRIME
################################


## New dataframe "df" from merged airbnb and crime:
df <- merge(x = abb_final, y = crime, by = c("GEOID", "yearmon"))

## Remove the variable "X" from dataframe (leftover blockgroup-date ID from crime):
df <- df[,-which(colnames(df)=="X")]

## Rename a column:
to.change <- which(colnames(df) == "Other.Sex.Offenses..Not.Commercialized.")
colnames(df)[to.change] <- "Sex.Offend.Other"
rm(to.change)

## Generate a "month" variable (control for seasonality):
df$month <- format(x = df$yearmon, format = "%m")


## Remove the crime and airbnb dataframes:
rm('abb_final', 'crime')


################################
### 7) WRITE OUT DATA:
################################

# dir.create(path = "./Data/Merged/")
setwd("./Data/Merged/")
write.csv(x = df, file = "./02-airbnb_crime_month_block_group_clean_1mo.csv", 
          row.names = F)




####
#### 6 months prior and post
####

k <- 6

################################
### 4) DATA SUBSETTING
################################

## Subsetting by date:

## NOTE: The variable "k" is set at the top of the script. Default is 3.

# first airbnb date:
min.date.abb <- min(abb_final$yearmon)

# start cutoff for crime:
min.date.crime <- min.date.abb - (k/12)

## end.date.abb changed to accommodate the fact that we are interestd in 6 months prior
# end cutoffs for airbnb and crime:
end.date.abb <- as.yearmon("2016-02") # Up to May, 2016
end.date.crime <- as.yearmon("2016-08") # Up to Aug, 2016


# Remove the AirBnB data that is too new:
abb_final <- abb_final[which(abb_final$yearmon <= end.date.abb),] 

# Remove the crime data for months that haven't yet occurred:
crime <- crime[which(crime$yearmon <= end.date.crime),] 

# Remove the crime data for months way before the airbnb data starts:
crime <- crime[which(crime$yearmon >= min.date.crime),]

# Remove airbnb data that we don't have crime for:
abb_final <- abb_final[which(abb_final$GEOID %in% crime$GEOID),]

# Remove crime data that we don't have airbnb data for:
crime <- crime[which(crime$GEOID %in% abb_final$GEOID),]



################################
### 5) EXPAND AIRBNB DATA
################################



## Here we're going to expand the AirBnb data so that each unit appears 
##  (2*k) + 1 times in the data (k months before to k months after)
##  intro of unit:


# Sequence of time before and after airbnb treatment:
time.seq <- c(-k:k)[-(k+1)]


# Make a temp version of AirBnB data:
abb.temp <- abb_final

# Generate new column for time variable:
abb.temp$t <- 0

# Put AirBnB data into list where each element is a block group:
abb.temp <- by(data = abb.temp, INDICES = abb.temp$GEOID, FUN = subset)

# loop over block groups and expand the data:
for (i in seq_along(abb.temp)){
  
  block <- abb.temp[[i]] # isolate element
  
  for (j in seq_along(time.seq)){
    
    temp.block <- block[1,] # create the baseline
    
    # calc lag(lead) year-month
    temp.block$yearmon <- temp.block$yearmon + (time.seq[j]/12) 
    temp.block$t <- time.seq[j]
    block <- rbind(block, temp.block)
    
  }
  abb.temp[[i]] <- block
}

# Remove junk:
rm('i', 'j', 'block', 'temp.block')


## Combine list of expanded data frames into one big data frame:
abb_final <- do.call(rbind, abb.temp)


# Remove temp airbnb data frame:
rm(abb.temp)


################################
### 6) MERGE AIRBNB & CRIME
################################


## New dataframe "df" from merged airbnb and crime:
df <- merge(x = abb_final, y = crime, by = c("GEOID", "yearmon"))

## Remove the variable "X" from dataframe (leftover blockgroup-date ID from crime):
df <- df[,-which(colnames(df)=="X")]

## Rename a column:
to.change <- which(colnames(df) == "Other.Sex.Offenses..Not.Commercialized.")
colnames(df)[to.change] <- "Sex.Offend.Other"
rm(to.change)

## Generate a "month" variable (control for seasonality):
df$month <- format(x = df$yearmon, format = "%m")


## Remove the crime and airbnb dataframes:
rm('abb_final', 'crime')


################################
### 7) WRITE OUT DATA:
################################

# dir.create(path = "./Data/Merged/")
setwd("./Data/Merged/")
write.csv(x = df, file = "./03-airbnb_crime_month_block_group_clean_6mo.csv", 
          row.names = F)