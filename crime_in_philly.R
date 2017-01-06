library(ggmap)
library(ggplot2)
library(dplyr)
library(data.table)
library(bit64)
library(sqldf)
library(heatmaply)
library(plotly)
library(ggvis)
library('tidyr')     
library('ggplot2')    
library('scales')     
library("maps")

# Read the crime data
phildata <- read.csv("C:/my files/airbnb_crime/crime.csv")
head(phildata)
# summary of crime categories - this will show the Top 10 categories
sqrt(var(summary(phildata$Text_General_Code)))
unique(phildata$Text_General_Code)
# basic contour map for Philadelphia
phil = c(lon = -75.19, lat = 39.98)

lat<- c(39.952583)
lon<- c(-75.165222)
philm = get_map(location = c(lon,lat), zoom = 12, color = "bw", source= "osm")
plot(philmap)

# function to view crime density contour map
map_crime <- function(crime_data, crime_category_name) 
{
  titlestr <- paste("Heatmap for crimes related to", crime_category_name )
  filterdf <- filter(crime_data, Text_General_Code %in% crime_category_name)
  plotimg <- ggmap(philm, extent = "device") +
    geom_density2d(data = filterdf, aes(x = Lon, y = Lat), size = 0.3) +
    stat_density2d(data = filterdf, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..)
                   , size = 0.01, bins = 16, geom = 'polygon') +
    scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
    ggtitle(titlestr)
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) 
  
  
  return(plotimg)
}

map_crime(phildata, c('Theft from Vehicle'))

# Description - Data Exploration and Visualization
# Dataset - Philadelphia Crime Data from Kaggle




# Read the crime data
phildata <- fread("../input/crime.csv")


# summarise latitude and longitude data
summary(phildata$Lon)
summary(phildata$Lat)


# checkin for NAs
sapply(phildata, function(x) sum(is.na(x)))

# Summarise the data for crime categories
summary(phildata$Text_General_Code)

# checking for date time manipulation
phildata$dt = as.Date(phildata$Dispatch_Date)
phildata$year = as.numeric(format(phildata$dt, "%Y"))
phildata$mth = as.numeric(format(phildata$dt, "%m"))
phildata$day = as.numeric(format(phildata$dt, "%d"))


# Group crime by dates and Police Districts
crimephilly = sqldf("select Police_Districts as 'District', 
                    day, year, mth, Hour,
                    Text_General_Code as 'Category', 
                    count(*) as 'count' 
                    from phildata
                    group by Police_Districts, day, year, mth, Hour, Text_General_Code")


# ----------- Chart No. 1 -------------- #
# Graphical Visualization of crimes by category
data_plot = crimephilly %>%
  group_by(Category) %>%
  summarise(count = n()) %>%
  transform(Category = reorder(Category,-count))
summary(data_plot)
ggplot(data_plot) + 
  geom_bar(aes(x=Category, y=count),
           stat="identity")+
  coord_flip()+
  theme(legend.position="None")+
  ggtitle("Number of crimes in individual category")+
  ylab("Number of crimes")+
  xlab("Category of crime")


# ----------- Chart No. 2 -------------- #
# Graphical Visualization of crimes by Police district
data_plot2 = crimephilly %>%   group_by(District) %>%   summarise(count = n()) %>%
  transform(District = reorder(District,-count))
ggplot(data_plot2) +   geom_bar(aes(x=District, y=count), stat="identity") +
  coord_flip()+  theme(legend.position="None") + ggtitle("Number of crimes in each District") +
  ylab("Number of crimes") +  xlab("Philadelphia District")


# ----------- Chart No. 3 -------------- #
# Crime count by category and year 
ggplot(data=crimephilly, aes(x=year)) +   geom_bar(colour="black", fill="purple") +
  ylab('Count') +   facet_wrap(~Category)
# The graph also shows that vandalism and narcotics have decreased in the past few years.


# ----------- Chart No. 4 -------------- #
# Crime count by Ploice District and year 
ggplot(data=crimephilly, aes(x=year)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~District)
# the graph shows that crime is maximum in districts 11,15,16.
# However, crime rates overall show a declining rate. 



# ----------- Chart No. 5,6 -------------- #
# Crime count by category and Police District for 2014 and 2015 
crime2014 <- subset(crimephilly, year == 2014)
crime2015 <- subset(crimephilly, year == 2015)

# Crime chart for year 2014
ggplot(data=crime2014, aes(x=District)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~Category) +
  ggtitle("Crime Report 2014")


# Crime chart for year 2015
ggplot(data=crime2015, aes(x=District)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~Category) +ggtitle("Crime Report 2015")