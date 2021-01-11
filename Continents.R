source('C:/Users/nikos/Desktop/MSc/Programming for Data Science/R/PROJECT/r_project.r')
library(countrycode)
library(scales)
library(gridExtra)
dt <- dt[Country != 'Diamond Princess']#THIS IS A CRUISE SHIP!NOT A COUNTRY!
dt <- dt[Country != 'MS Zaandam']#THIS IS A CRUISE SHIP!NOT A COUNTRY!
dt$continent <- countrycode(sourcevar = 
    dt$Country, origin = "country.name", destination = 'continent')
dt[Country == 'Kosovo']$continent <- 'Europe'#this was a leftover.
dt <- as.data.table(dt)
dt_1st_qt <- dt[Date <= '2020-04-01']
dt_2nd_qt <- dt[Date <= '2020-09-01']
dt_3rd_qt <- dt[Date <= '2021-01-01']
cont_1 <- dt_1st_qt[,sum(Confirmed), by = .(continent)]
cont_2 <- dt_2nd_qt[,sum(Confirmed), by = .(continent)]
cont_3 <- dt_3rd_qt[,sum(Confirmed), by = .(continent)]
names(cont_1) <- c("Continent","Cumulative_cases")
names(cont_2) <- c("Continent","Cumulative_cases")
names(cont_3) <- c("Continent","Cumulative_cases")
cont_1$Per <- round(cont_1$Cumulative_cases/
                                   sum(cont_1$Cumulative_cases),3)
cont_2$Per <- round(cont_2$Cumulative_cases/
                                   sum(cont_2$Cumulative_cases),3)
cont_3$Per <- round(cont_3$Cumulative_cases/
                                   sum(cont_3$Cumulative_cases),3)

bar1 <- ggplot(cont_1) +
  geom_bar(stat = 'identity' ,aes(x = Continent, y = Per), 
           color="navyblue", fill="deepskyblue4") +
 xlab("<= 1 April 2020") + ylab("Cumulative Cases (%)") +
   scale_y_continuous(labels=percent)

bar2 <- ggplot(cont_2) +
  geom_bar(stat = 'identity' ,aes(x = Continent, y = Per), 
           color="navyblue", fill="deepskyblue4") +
  xlab("<= 1 September 2020")  + ylab("") + scale_y_continuous(labels=percent)

bar3 <- ggplot(cont_3) +
  geom_bar(stat = 'identity' ,aes(x = Continent, y = Per), 
           color="navyblue", fill="deepskyblue4") +
  xlab("<= 1 January 2021")  + ylab("")+ scale_y_continuous(labels=percent)

grid.arrange(bar1,bar2,bar3, nrow = 1, top = 'Covid-19 Cumulative Cases(%) per continent and each quarter')

hists <- ggplot(dt, aes(x = Date, y = Confirmed, fill = continent))+
  geom_histogram(stat='identity')+
  scale_x_date(date_breaks = '30 day', date_labels = '%b')


hists2 <- ggplot(dt, aes(x = Date, y = Deaths, fill = continent))+
  geom_histogram(stat='identity')+
  scale_x_date(date_breaks = '30 day', date_labels = '%b')

grid.arrange(hists,hists2,nrow = 1)
