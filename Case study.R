## Require ggplot



## REad file
CrimeData <- read.csv('CrimeData2.csv')

str(CrimeData)

# 1. Overall trend in crimes for the whole period of time in the dataset. The
# granularity should be at the Day level.

head(CrimeData$Dispatch_Date_Time)

CrimeData$Datetime <- as.POSIXct(CrimeData$Dispatch_Date_Time, format = "%Y-%m-%d %H:%M:%S", t2 = "EST")
head(CrimeData)

#Crime Data column
CrimeData$Date <- as.Date(CrimeData$Datetime, tz = "EST")
head(CrimeData)

by_date <- aggregate(CrimeData$Date, by = list(CrimeData$Datetime), FUN = length)

##Count number of crimes per data
require(dplyr)
by_dplyr <- CrimeData %>% group_by(Date) %>% count(Date)
by_dplyr <- as.data.frame(by_dplyr)
tail(by_dplyr)

ggplot(by_dplyr, aes(x=Date, y=n)) + geom_line(aes(color = n))

# 2. Which are the most and the least dangerous hours in Philadelphia?
CrimeData$Hour <- strftime(CrimeData$Datetime, format = '%H', tz = 'EST')
CrimeData$Hour <- as.numeric(CrimeData$Hour)
str(CrimeData)
by_hour <- CrimeData %>% group_by(Hour) %>% count(Hour)
head(by_hour)

by_hour <- as.data.frame(by_hour)

head(by_hour)
ggplot(by_hour,aes(x=Hour,y=n)) + geom_line()


# 3. Is there any seasonality in the crime rate?

CrimeData$Month <- strftime(CrimeData$Datetime, format = '%m', tz = 'EST')

by_month <- CrimeData %>% group_by(Month) %>% count(Month)
by_month <- as.data.frame(by_month)
by_month
ggplot(by_month, aes(x=Month, y=n)) + geom_point()


# 4. What are the top 10 crimes crime types?
by_crime_data <- CrimeData %>% group_by(Text_General_Code) %>% count(Total=Text_General_Code)
by_crime_data <- as.data.frame(by_crime_data)
by_crime_data <- by_crime_data[order(by_crime_data$n, decreasing = T),]
by_crime_data
ggplot(by_crime_data, aes(x=reorder(Text_General_Code,n), y = n)) + geom_point() + coord_flip()

head(CrimeData)


# 5. Which police HQ is in the most need of strengthening?
head(CrimeData)

by_district <- CrimeData %>% group_by(Dc_Dist) %>% count(Dc_Dist)
head(by_district)
by_district <- as.data.frame(by_district)
by_district <- by_district[order(by_district$n, decreasing = TRUE), ]
by_district
