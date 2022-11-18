getwd()
setwd("D:/GOOGLE(DA)Coursera/Capstone Coursera DA/Case Study/Case Study 2/Fitabase Data 4.12.16-5.12.16")


          #Importing required packages

install.packages('tidyverse')
library(tidyverse)
library(dplyr)


          #We are only importing only the required files for the analysis

daily_activity <- read.csv("dailyActivity_merged.csv")
daily_calories <- read.csv('dailyCalories_merged.csv')
daily_intensities <- read.csv('dailyIntensities_merged.csv')
daily_steps <- read.csv('dailySteps_merged.csv')
hourly_calories <- read.csv('hourlyCalories_merged.csv')
hourly_intensities <- read.csv('hourlyIntensities_merged.csv')
hourly_steps <- read.csv('hourlySteps_merged.csv')
daily_sleep <- read.csv('sleepDay_merged.csv')
weight_log <- read.csv('weightLogInfo_merged.csv')

        
         #Reviewing the dataframes

head(daily_activity)
head(daily_calories)
head(daily_intensities)
head(daily_sleep)
head(daily_steps)
head(hourly_intensities)
head(hourly_steps)
head(hourly_calories)
head(weight_log)


        #Using the glimpse and summary to shows the data structures and statistical summary

glimpse(daily_activity)
summary(daily_activity)
glimpse(daily_calories)
summary(daily_calories)
glimpse(daily_intensities)
summary(daily_intensities)
glimpse(daily_sleep)
summary(daily_sleep)
glimpse(daily_steps)
summary(daily_steps)
glimpse(hourly_calories)
summary(hourly_calories)
glimpse(hourly_intensities)
summary(hourly_intensities)
glimpse(hourly_steps)
summary(hourly_steps)
glimpse(weight_log)
summary(weight_log)

        #Lets clean the dataframes using the janitor and skimr packages

install.packages('skimr')
install.packages("janitor")
library(skimr)
library(janitor)

  
        #Lets check the null values and remove the null values for better analysis

daily_activity %>% 
  is.na() %>% 
  sum()

daily_calories %>% 
  is.na() %>% 
  sum()

daily_intensities %>% 
  is.na() %>% 
  sum()

daily_sleep %>% 
  is.na() %>% 
  is.na() %>% 
  sum()

daily_steps %>% 
  is.na() %>% 
  sum()

hourly_calories %>% 
  is.na() %>% 
  sum()

hourly_intensities %>% 
  is.na() %>% 
  sum()

hourly_steps %>% 
  is.na() %>% 
  sum()

weight_log %>% 
  is.na() %>% 
  sum()

            #from checking the dataset we can see that weight_log dataset has 65 null values in "FAT" col lets drop that values

weight_log %>% 
  is.na()

           #lets remove the "FAT" col from weight_log as it is not necessary for our analysis

weight_log <- weight_log %>% 
  select(-c("Fat"))

head(weight_log)  


            #The next step is to verify the number of unique users as the ID column acts as a foreign key across the whole dataset,
            #therefore I could merge the whole datasets using ID as it is shared by each dataframe.



n_distinct(daily_activity$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(daily_sleep$Id)
n_distinct(daily_steps$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_steps$Id)
n_distinct(weight_log$Id)


            #Based on the results, there are 24 unique daily users that provided their health metrics info (SleepDay_merged dataframe), 8 unique users provided their daily weight_log_Info health metrics and 33 unique users provided the rest of the health metrics. Hence,
            #the weight_log_info data frame could be dropped as the unique users are too few to give me any insightful information.


          
            #lest check the duplicate rows in our dataframes


sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_intensities))
sum(duplicated(daily_sleep))
sum(duplicated(daily_steps))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))



             #i noticed that daily_sleep has 3 duplicated rows where as none of other has duplicated rows
              #lets merge the duplicatd rows in dail_sleep

daily_sleep_1 <- daily_sleep[!duplicated(daily_sleep),]

sum(duplicated(daily_sleep_1))


              #So, I decided to combine the data_sleep_1 and daily_activity but i saw the in daily_activity it is,
              #activitydate but in daily_sleep its is sleepdate lets rename it


daily_sleep_1 <-daily_sleep_1 %>% 
  rename(ActivityDate = SleepDay)

              #There is disparancies in all of the date formats in all of these dataframes lets change into  same format using lubridate package
              # In hourly dataframes the Ther is disparencies in the timstamps so lets also correct that



library(lubridate)

daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")
daily_sleep_1$ActivityDate <- as.Date(daily_sleep_1$ActivityDate, format = "%m/%d/%Y")

hourly_calories$ActivityHour <- mdy_hms(hourly_calories$ActivityHour)

hourly_intensities$ActivityHour <- mdy_hms(hourly_intensities$ActivityHour)

hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour)


#ANALYZE PHASE


                  #I merge the dailyActivity_merged table and daily_sleep_1 into a new data frame called “daily_activity_and_sleep”
              
                  #I merge the hourlyCalories, hourlyIntensities these 2 data frames into a single data frame called "hourly_activity". This is done via "Id" and "ActivityHour"

                  # I merge the hourly activity dataframe and hourly steps in to a new dataframe called "hourly_act"


daily_activity_and_sleep <-merge(daily_activity,daily_sleep_1,by=c("Id","ActivityDate"))

hourly_activity <- merge(hourly_calories,hourly_intensities, by = c("Id","ActivityHour"))

hourly_act <- merge(hourly_activity,hourly_steps, by = c("Id","ActivityHour"))



#Data visualization Phase


                  #Determination of relationship between TotalSteps and Calories of each users using the daily_activity_with_sleep dataframe

library(ggplot2)



ggplot(data=daily_activity_and_sleep)+geom_point(mapping = aes(x=TotalSteps, y=Calories))


                  #Determine the relationship between the distance and the calories burnt



ggplot(data=daily_activity_and_sleep)+geom_point(mapping = aes(x=TotalDistance, y=Calories),color="purple")


    
                                             
                                             
                                             
                                             