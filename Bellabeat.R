## Loading the library
```{r}
library(tidyverse)
library(lubridate)
```

## Reading the CSV
```{r}
d_activity <- read.csv("C:/programming languages/R/Casestudy/dailyActivity_merged.csv")
d_calories <- read.csv("C:/programming languages/R/Casestudy/dailyCalories_merged.csv")

d_intensities <- read.csv("C:/programming languages/R/Casestudy/dailyIntensities_merged.csv")

d_steps <- read.csv("C:/programming languages/R/Casestudy/dailySteps_merged.csv")
```

## Getting a clear picture of loaded datasets
```{r}
glimpse(d_activity) 
glimpse(d_calories) 
glimpse(d_intensities) 
glimpse(d_steps)
```

## Finding the customers who are unique to dataset
```{r}
n_distinct(d_activity$Id)

n_distinct(d_calories$Id)

n_distinct(d_intensities$Id)

n_distinct(d_steps$Id)
```

## Judging the duplicate values to remove them
```{r}
sum(duplicated(d_activity))

sum(duplicated(d_calories))

sum(duplicated(d_intensities))

sum(duplicated(d_steps))
```

## Adding additional datasets to load and see what values can be explored
```{r}
d_sleep <- read.csv("C:/programming languages/R/Casestudy/sleepDay_merged.csv")

h_steps <- read.csv("C:/programming languages/R/Casestudy/hourlySteps_merged.csv")

h_calories <- read.csv("C:/programming languages/R/Casestudy/hourlyCalories_merged.csv")
```

## Getting the clear picture of new datasets
```{r}
glimpse(d_sleep)
glimpse(h_steps)
glimpse(h_calories)
```

## Seeing unique customers
```{r}
n_distinct(d_sleep$Id)

n_distinct(h_steps$Id)

n_distinct(h_calories$Id)
```

## Finding duplicate values
```{r}
sum(duplicated(d_sleep))

sum(duplicated(h_steps))

sum(duplicated(h_calories))
```

## Dropping the null data
```{r}
d_activity <- d_activity %>% distinct() %>% drop_na()

d_sleep <- d_sleep %>% distinct() %>% drop_na()

h_steps <- h_steps %>% distinct() %>% drop_na()

h_calories <- h_calories %>%  distinct() %>% drop_na()
```

## Renaming the complete dataset to lower caps
```{r}
d_activity <- rename_with(d_activity,tolower)

d_sleep <- rename_with(d_sleep,tolower)

h_steps <- rename_with(h_steps,tolower)

h_calories <- rename_with(h_calories,tolower)
```

## Making the date time consistent
```{r}
d_activity <- d_activity %>% rename(date=activitydate) %>% mutate(date=as_date(date,format="%m/%d/%Y"))

d_sleep <- d_sleep %>% rename(date=sleepday) %>% mutate(date=as_date(date,format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))

h_steps <- h_steps %>% rename(date=activityhour) %>% mutate(date=as.POSIXct(date,format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))

h_calories <- h_calories %>% rename(date=activityhour) %>% mutate(date=as.POSIXct(date,format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))
```

## Merging the datasets for complete analysis & making an aspect of dividing customers on basis of their use of our app
```{r}
d_ac_sleep <- merge(d_activity,d_sleep,by=c("id","date"))

d_use <- d_ac_sleep %>% group_by(id) %>% summarize(days=sum(n())) %>% mutate(use_level=case_when(days>=1 & days<=5 ~ "Low Use", days>=6 & days<=15 ~ "Moderate Use", days>=16 & days<=31 ~ "High Use"))

print(d_use)
```

## Getting the percentage of users segmented on basis of usage of our app
```{r}
d_use_per <- d_use %>% group_by(use_level) %>% summarize(total=n()) %>% mutate(totals=sum(total)) %>% group_by(use_level) %>% summarize(total_per=total/totals)

print(d_use_per)
```

## Pie chart for a visual of users
```{r}
ggplot(data=d_use_per,aes(x="",y=total_per,fill=use_level))+geom_col()+coord_polar(theta="y")+ labs(title = "Use of our product",subtitle ="Low usage should be dealt",x="",y="")
```

## Formatting the data based on days of week
```{r}
d_ac_sleep <- d_ac_sleep %>% mutate(weekday=weekdays(date))

d_ac_sleep$weekday <- ordered(d_ac_sleep$weekday, levels=c("Monday","Tuesday","Wednesday","Thursday", "Friday","Saturday","Sunday"))
```

## Finding mean of total steps and total sleep minutes users take day-wise
```{r}
d_steps_sleep <- d_ac_sleep %>% group_by(weekday) %>% summarize(weeks_steps=mean(totalsteps),weeks_sleep=mean(totalminutesasleep))
```

## Plotting it with ranges depicted from red to green color, with red being lowest
```{r}
ggplot(d_steps_sleep)+geom_col(aes(x=weekday,y=weeks_steps,fill=weeks_steps))+ scale_fill_gradient(low="red",high="green")
```

## Plotting it with ranges depicted from red to green color, with red being lowest
```{r}
ggplot(d_steps_sleep)+geom_col(aes(x=weekday,y=weeks_sleep,fill=weeks_sleep))+ scale_fill_gradient(low="red",high="green")
```

## Seperating the date and time and then merging to get a clear understanding
```{r}
h_steps <- h_steps %>% separate(date,into=c("date","time"),sep=" ") %>% mutate(date=ymd(date))

h_calories <- h_calories %>% separate(date,into=c("date","time"),sep=" ") %>% mutate(date=ymd(date))

h_cal_steps <- merge(h_calories,h_steps,by=c("id","date","time"))
```

## Seeing the relation of steps with calories
```{r}
ggplot(h_cal_steps,aes(x=calories,y=steptotal))+geom_point()+geom_smooth()
```

## Loading the weight dataset
```{r}
w_log <- read.csv("C:/programming languages/R/Casestudy/weightLogInfo_merged.csv")
```

## Getting the clear dataset picture
```{r}
glimpse(w_log)
```

## Finding dataset for distinct customers
```{r}
n_distinct(w_log$Id)
```

## Find sum of duplicated data
```{r}
sum(duplicated(w_log))
```

## Lower caps with column names
```{r}

w_log <- rename_with(w_log,tolower)
```

## Formatting dataset for the date column and seperating date and time
```{r}
w_log <- w_log %>% mutate(date=as.POSIXct(date,format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))

w_log <- w_log %>% separate(date,into=c("date","time"),sep=" ") %>% mutate(date=ymd(date))
```

## Create a boxplot
```{r}
boxplot.default(w_log$weightkg)
```

## Removing outliers
```{r}
library(dplyr)
w_log <- subset(w_log,weightkg<120)
```

## Merging data sets
```{r}
h_cal_steps2 <- h_cal_steps %>% group_by(id,date) %>% summarize(total_cal= sum(calories),total_steps=sum(steptotal),.groups='drop')

w_log_cal_Steps <- merge(w_log,h_cal_steps2,by=c("id","date"))
```

## Plotting total_cal and weight to find corrleation
```{r}
ggplot(data=w_log_cal_Steps)+geom_point(mapping=aes(total_cal,weightkg))
```

## Seeing the correlation
```{r}
cor(w_log_cal_Steps$total_cal,w_log_cal_Steps$weightkg)
```


# CONCLUSIONS

### 1. Since maximum users use our app well,there is still 33.33 % using our app in less usage
### 2. People on sunday tend to sleep more than walking
### 3. Calories burnt and people having steps are positively correlated
### 4. Weight of a person hihly depends on steps he/she takes


# RECOMMENDATIONS

### 1. Awarness can be made among users with less engagement about health benifits to turn users into high use
### 2. To engage people more on sunday, some extra benifits be given so that people have healthy lifestyle
### 3. Towards week ends near to thursday people take less sleep so more sleep can be encouraged by providing users stress busting excersises.