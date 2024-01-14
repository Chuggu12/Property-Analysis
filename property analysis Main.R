# What is Statistics?
# Statistics is the study of how to collect, analyze, and interpret data.

#  Data collection: The process of gathering information about a population or phenomenon.
#  Data analysis: The process of using statistical methods to summarize and understand data.
#  Data interpretation: The process of drawing conclusions from data and communicating those conclusions to others.

# Statistics is a powerful tool that can be used to answer a wide variety of questions, such as:

# What is the average height of American adults?
#   What is the probability of winning the lottery?
#   How does the effectiveness of a new drug compare to existing treatments?
#   Statistics is used in a wide variety of fields, including business, government, healthcare, and education. 

# It is a valuable tool for making informed decisions and understanding the world around us.




# There are two main types of statistics: descriptive statistics and inferential statistics.

# Descriptive statistics summarize data and describe its main features. 
# They are used to describe the data itself, without making any inferences about the population from which the data was 
# collected. Some common descriptive statistics include:

# Mean: The average of a set of data.
# Median: The middle value in a set of data when arranged in order from least to greatest.
# Mode: The most frequent value in a set of data.
# Range: The difference between the highest and lowest values in a set of data.
# Variance: A measure of how spread out the data is.
# Standard deviation: A measure of how much variation there is from the mean.


# Inferential statistics are used to make inferences about a population based on data collected from a sample. 
# They are used to test hypotheses, estimate population parameters, and make predictions. 
# Some common inferential statistical methods include:

# Hypothesis testing: A statistical method used to determine whether there is enough evidence to reject a null hypothesis.
# Estimation: A statistical method used to estimate the value of a population parameter.
# Prediction: A statistical method used to predict the value of a variable based on other variables.




# MEASURES OF CENTER (MEASURES OF CENTRAL TENDENCY)

# Measures of center: These are statistical values that represent the central tendency or average of a data set, 
# such as the mean, median, or mode.

# Normal distribution: It is a symmetric probability distribution where the majority of data points 
# cluster around the mean, resulting in a bell-shaped curve.

# Skewness: It is a measure of the asymmetry of a distribution, indicating whether the data is skewed to the left 
# (negative skewness) or to the right (positive skewness).



# Symmetric Curve:

# Mean: The mean is a measure of center that represents the average value of the data. 
# In a symmetric curve, the mean is equal to the median, as both values lie at the center of the distribution.

# Median: The median is the middle value in a data set when it is arranged in ascending or descending order. 
# In a symmetric curve, the median is the same as the mean, as both values are located at the center of the distribution.

# Mode: The most frequent value in a set of data.For a normal distribution, the mode is the same value as the mean and median. 
# This is because a normal distribution is symmetrical, with the highest point of the curve (the mode) at the center.



# Skewed Curve:

# Mean: The mean is influenced by extreme values (outliers) in a skewed curve. 
# If the curve is positively skewed (skewed to the right, data piled on left), the mean is pulled towards the higher values. 
# If the curve is negatively skewed (skewed to the left, data piled on right), the mean is pulled towards the lower values.

# Median: The median is less affected by extreme values in a skewed curve. 
# It represents the middle value, unaffected by the skewness. 
# In a positively skewed curve, the median is usually less than the mean, 
# while in a negatively skewed curve, the median is typically greater than the mean.


# Mode: The point at which the curve reaches its highest point.
# If the curve is positively skewed (skewed to the right, data piled on left), the mode is on left of median. 
# If the curve is negatively skewed (skewed to the left, data piled on right), the mode is on right of median. 


#installing the packages
# Install packages 
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("stringr")
install.packages("magrittr")
install.packages("ggplot2")


#calling out the main library we needed during ananlysis of plots at diffrent location

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tmaptools)
library(stringr)

# ggplot2: A powerful visualization package that makes it easy to create beautiful and informative plots.
# dplyr: A powerful data manipulation package that makes it easy to wrangle and clean data.
# tidyr: A package for tidying data, which means formatting it in a way that is easy to work with.
# tibble: A modern re-imagining of data frames in R.
# stringr: A package for working with strings in R.
#tmaptools:
# lubridate: A package for working with dates and times in R.

#Location Analysis
#here we are analysing properties in diff loaction in New york Cities
#getting the location
getwd()

dat = read.csv("Main_city.csv")
dat2 = read.csv("Noda_fixed.csv")
dat3 = read.csv("dill_fixed.csv")



#Property Type Analytics
pat1 = read.csv("Noda_housey.csv")
pat2 = read.csv("Noda_town.csv")
pat3 = read.csv("Condos.csv")

#Outlier Analysis: area near highways
out1 = read.csv("Myers.csv")
out2 = read.csv("Plaza.csv")


##################------------------------SORTING UP THE DATA-----------------------------------------#########
# Extract the numeric part of the lot size and price information
dat$Square.ft = as.numeric(gsub("[^0-9\\.]", "", dat$Square.ft))
dat$Square.ft
dat$Price = as.numeric(gsub("[^0-9\\.]", "", dat$Price))
dat$Sale.Date = substr(dat$Sale.Date, 6, 15)
dat$Sale.Date = as.Date(dat$Sale.Date, format = "%m/%d/%Y")
dat$day = day(dat$Sale.Date)
dat$month = month(dat$Sale.Date)
dat$year = year(dat$Sale.Date)
dat$quarter = ((as.numeric(dat$month)-1)%/%3) +1

# introducing price per area  statistic variable to form an analysis during which time and which BHK is favourable for
#a particular time being
dat$Ppa = dat$Price / dat$Square.ft

---------------------------------#Cleaning up the data#------------------------------------------------

#getting rid of none sqfts and dupes
dat = filter(dat, Price > 1000, Square.ft > 10)
#, Beds != "--", Beds != "Studio")
dat = unique(dat)

dat = filter(dat, Ppa < 1000)

dat = filter(dat, Square.ft < 100000)


#transforming BEDS column into numeric form
Cleaned = dat
Cleaned$Beds = as.numeric(gsub("[^0-9\\.]", "", Cleaned$Beds))
Cleaned = filter(Cleaned, Beds < 4)
Cleaned$Beds = as.character(Cleaned$Beds)
Cleaned = unique(Cleaned)

#transforming BATH coloumn into numeric form
extra = dat
extra$Baths = as.numeric(gsub("[^0-9\\.]", "", extra$Baths))
extra = filter(extra, Baths < 4)
extra$Baths = as.character(extra$Baths)
extra = unique(extra)

#Removing M in price coloumn 
for(i in 1:nrow(dat)){
  if(dat$Price[i] < 1000){
    dat$Price[i] = dat$Price[i] * 1000000
  }
}

---------------------#making new statistics on the basis of our sorted data#-------------------------------

New_dat = dat  %>% group_by(Sale.Date) %>% summarize(avg = mean(Ppa), sd = sd(Ppa))
Numbers = Cleaned  %>% group_by(year, quarter) %>% summarize(avg = mean(Ppa), sd = sd(Ppa))
Bedds = Cleaned %>%  group_by(Beds, Sale.Date) %>% summarize(avg = mean(Ppa), sd = sd(Ppa))
bats = extra %>%  group_by(Baths, Sale.Date) %>% summarize(avg = mean(Ppa), sd = sd(Ppa))





#_____________________Overall statistics ____________________

#Bar graph
#facet wrap ppa per quarter
ggplot(data = Numbers, aes(x=quarter, y = avg, fill = year)) +
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=avg-sd, ymax = avg+sd))+
  facet_wrap(~year)+
  xlab("Quarter")+
  ylab('Ppa')
  

#Scatter plot
#Ppa vs time
ggplot(data = New_dat, aes(x = Sale.Date, y = avg))+
  geom_smooth(color = "DarkBlue", method = lm)+
  geom_point(color = "Green")+
  theme_gray()+
  xlab('Year')+
  ylab('Price Per Square Foot ($)')

regression = lm(New_dat$avg ~ New_dat$Sale.Date)
market_slope = coef(regression)[2]
summary(regression)

#density plot
density_obj = density(dat$Ppa)  # Compute density estimates
plot(density_obj, main = "Density Plot of Ppa", xlab = "Values of Ppa", ylab = "Density")
plot(regression)
#here we get to know most of the ppa is concentrated under the value 300-400
#and have a random norml distribution

boxplot(dat$Ppa, main = "Box Plot of ppa", xlab = "ppa", horizontal = TRUE)


#___________________________________________________________

#_____________________Specific statistics ___________________

Similar = filter(dat, Beds ==3, Baths == 2, )
boxplot(Similar$Ppa, main = "Box Plot of ppa", xlab = "ppa", horizontal = TRUE)
#showing the min, max and the range of values where it concetrated



#____________________________________________________________

#________________________Bed Analytics_______________________

#scatter plot of all PPA vs time
ggplot(data = Bedds, aes(x = Sale.Date, y = avg, color = Beds))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_fill_manual(values = c("yellow", "pink", "blue"))+
  theme_gray()+
  xlab('Year')+
  ylab('Price Per Square Foot ($)')
#from this we conclude that only for 3BHK ppa decreased over years
#whereas 1 and 2 BHK have an positive relation where 1BHK have rampant increase in price or have more value of R
#w.r.t to price than 2BHK

#data of PPA/time vs beds

one = filter(Bedds, Beds == 1)
model = lm(one$avg ~ one$Sale.Date)
one_slope = coef(model)[2]

two = filter(Bedds, Beds == 2)
model = lm(two$avg ~ two$Sale.Date)
two_slope = coef(model)[2]

three = filter(Bedds, Beds == 3)
model = lm(three$avg ~ three$Sale.Date)
three_slope = coef(model)[2]

Bed_slope_vs_beds = Bedds %>% group_by(Beds) %>% summarize()
Bed_slope_vs_beds$Slopes = c(one_slope, two_slope, three_slope)
Bed_slope_vs_beds

#graph of PPA/time vs beds
ggplot(data = Bed_slope_vs_beds, aes(x=Beds, y = Slopes, fill = Beds)) +
  geom_bar(stat = "identity", color = "black")+
  xlab("Number of Beds")+
  ylab('Change in Ppa Over Time')+
  geom_hline(yintercept = 0, color = "black")


#____________________________________________________________
#Baths

#scatter plot of all PPA vs time
ggplot(data = bats, aes(x = Sale.Date, y = avg, color = Baths))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_fill_manual(values = c("yellow", "cyan", "purple"))+
  theme_gray()+
  xlab('Year')+
  ylab('Price Per Square Foot ($)')
#scatterplot and regression line between PPA and baths


#data of PPA/time vs beds

#separating data frames wrt to bathrooms available in an flat
#having single bathrooms
one = filter(bats, Baths == 1)
model = lm(one$avg ~ one$Sale.Date)
one_slope = coef(model)[2]

#having double bathrooms
two = filter(bats, Baths == 2)
model = lm(two$avg ~ two$Sale.Date)
two_slope = coef(model)[2]

#having three bathrooms
three = filter(bats, Baths == 3)
model = lm(three$avg ~ three$Sale.Date)
three_slope = coef(model)[2]

Bed_slope_vs_beds = bats %>% group_by(Baths) %>% summarize()
Bed_slope_vs_beds$Slopes = c(one_slope, two_slope, three_slope)


#graph of PPA/time vs beds
ggplot(data = Bed_slope_vs_beds, aes(x=Baths, y = Slopes, fill = Baths)) +
  geom_bar(stat = "identity", color = "black")+
  xlab("Number of Beds")+
  ylab('Change in Ppa Over Time')+
  geom_hline(yintercept = 0, color = "black")




#______________________Custom stats__________________________
dat = filter(dat, Price < 2000000)
#Price Histogram
hist(dat$Price, main = "Property Price Distribution", xlab = "Price", ylab = "Frequency")

#from this histogram we conclude that most of the people buying properties less than 450000

-----------#Price overtime#--------------------------------

#through the graph and regression line with a no. of hypothesis testing we find out that 
#price slightly increase over year not that rampant increase

ggplot(data = dat, aes(x = Sale.Date, y = Price))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_linedraw()+
  xlab('Year')+
  ylab('Price')

regression = lm(dat$Price ~ dat$Sale.Date)
summary(regression)
#finding a relation between sale price and date

#Price vs Square ft
ggplot(data = dat, aes(x = Square.ft, y = Price))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_linedraw()+
  xlab('Square ft')+
  ylab('Price')
#plotting regression line through dotted graph, showing a positive relation between price and square ft.


