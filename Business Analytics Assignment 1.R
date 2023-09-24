library(readr)

CMR <- read.csv("Business Analytics/Assignments/Business Analytics/archive/US Cancer Mortality and Incidence Rate.csv")

head(CMR)

#calculating the column mean of the CMR dataset

mean <- colMeans(CMR, na.rm = T)

#Arranging the dataset by the County Column

install.packages("dplry")
library(dplyr)

CMR %>%
  arrange(County)


#Calculating the mean of Age Adjusted Incidence Rate

CMR_Age_mean <- sapply(CMR$Age.Adjusted.Incidence.Rate.Ê....cases.per.100.000, mean, na.rm = T)

CMR_Age_mean


#Calculating the median of the Lower Confidence Interval

CMR_Lower_median <- sapply(CMR$Lower.95..Confidence.Interval, median, na.rm = T)

CMR_Lower_median


#Calculating the mode of the Average Annual Count

CMR_Mode_AvgCount <- mlv(CMR$Average.Annual.Count, method = "mfv")

CMR_Mode_AvgCount


#Calculating the mean of Upper 95% Confidence Level

CMR_Upper_mean <- mean(CMR$Upper.95..Confidence.Interval.1, na.rm = T)

CMR_Upper_mean



#Creating histogram of the County 

hist(CMR$FIPS,
     breaks = 10,
     main = "FIPS",
     xlab = "FIPS",
     ylab = "Frequency",
     col = "darkblue" 
     )

#creating a Scatterplot for the Lower 95 Confidence Level 

plot(CMR$Lower.95..Confidence.Interval, 
     main = "Scatter Plot of Lower 95 Confidence",
     xlab = "95 Lower Confidence",
     ylab = "Frequency",
     pch = 20, 
     col = "black"
     )


#calculating a range from between the upper 95% and lower 95% region

CMR_DataCol <- sapply(CMR, function (col) {
  range_region <- max(Upper.95..Confidence.Interval.1, na.rm = T) - min(Lower.95..Confidence.Interval, na.rm = T)
  variance_Avg_Count <- var(Average.Annual.Count, na.rm = T)
  std_dev_AvgCount <- sd(Average.Annual.Count, na.rm = T)
  
  list (
    Range = range_region,
    Variance = variance_Avg_Count,
    StandardDeviation = std_dev_AvgCount
  )
})

