library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)

set1 <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
set2 <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
#TASK 1: Remove columns:
set1 <- set1[, !c('Province/State','Lat','Long')]
set2 <- set2[, !c('Province/State','Lat','Long')]
#TASK 2: Convert data from wide to long format.
dt1 <- melt(set1, id.vars = "Country/Region")
dt2 <- melt(set2, id.vars = "Country/Region")
#TASKS 3-4: Rename Variables
setnames(dt1, c("Country/Region","variable","value"),c("Country","Date","Confirmed"))
setnames(dt2, c("Country/Region","variable","value"),c("Country","Date","Deaths"))
#TASK 5: convert date format
dt1$Date <- mdy(dt1$Date)
dt2$Date <- mdy(dt2$Date)
#TASK 6: Group by country and date
dt1 <- dt1[order(Country, Date)]
dt2 <- dt2[order(Country,Date)]

#TASK 7: Merge the two datasets into one.
#SUM OVER COUNTRIES AND DATES 
#SO WE DONT HAVE MULTIPLE ENTRIES FOR THE SAME DAY AND COUNTRY
#BECAUSE WE DROPPED THE STATES VARIABLE
dt1 <- dt1[,sum(Confirmed), by = .(Country, Date)] 
dt2 <- dt2[,sum(Deaths), by = .(Country, Date)] 
#Since the two datasets have the same 2 columns, and are grouped the same way,
#we can just bind the deaths column.
dt <- cbind(dt1, dt2$V1)
names(dt) <- c("Country","Date","Confirmed","Deaths")

#TASK 8: Calculate counts for the whole world
#this keeps only the latest date for every country, 
#because it has the maximum confirmed cases and deaths
grouped_ <- dt %>% group_by(Country) %>% slice_max(order_by = Date, n = 1)
grouped_ <- as.data.table(grouped_)
#so we use that variable to sum over all of the countries 
#and get a number for the counts and deaths all over the world
dt_clean <- as.data.table(grouped_)
world <- apply(dt_clean[,3:4],2,sum)

#TASK 9: sort (again) by country and date.
dt <- dt[order(Country, Date)]

#TASK 10:Create two extra variables: confirmed.ind and
#deaths.inc with the daily confirmed cases and daily deaths respectively
#calculate the daily confirmed cases and deaths
dt$Confirmed.ind <- dt$Confirmed - lag(dt$Confirmed, n = 1)
dt$Deaths.inc <- dt$Deaths - lag(dt$Deaths, n = 1)
#set the first daily confirmed cases and deaths as zero, 
#because the lag function calculates another country's numbers.
dt[Date == '2020-01-22']$Confirmed.ind <- 0
dt[Date == '2020-01-22']$Deaths.inc <- 0

#We don't lose any important data this way,
#with: dt %>% filter(Date=='2020-01-22') %>% filter(Confirmed>0)
#only 6 countries had non-zero confirmed cases
#and only China had more than 2.
