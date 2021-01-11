source('C:/Users/nikos/Desktop/MSc/Programming for Data Science/R/PROJECT/r_project.r')

conf <- dt[,sum(Confirmed), by = Date]#Sum - Total Cases
death <- dt[,sum(Deaths),by = Date]#Sum - Total Deaths
all <- cbind(conf,death$V1) #Merge
names(all) <- c("Date","Total_Confirmed_Cases","Total_Deaths")

all$Fatality_Rate <- 100*all$Total_Deaths/all$Total_Confirmed_Cases
all$Date <- as.Date(all$Date)

fatal_rate <- ggplot(all, aes(x=Date, y = Fatality_Rate)) + 
      geom_line(linetype = 1, color = 'darkred', size = .9)+
      xlab("") +ylab("Mortality Rate (%)")+
      scale_x_date(date_breaks = '30 day', date_labels = '%b')+
      labs(title = 'Worldwide Case Fatality Rate of Covid-19')


both <-  ggplot(all,aes(x=Date))+
  geom_line(aes(y=Total_Confirmed_Cases,color = 'darkred'),size=.9,linetype="twodash") +
  geom_line(aes(y=Total_Deaths,colour='steelblue'),size=.9,linetype="twodash")+
  scale_y_log10() + ylab("") +xlab("") + labs(title='Total Covid-19 Confirmed Cases and Deaths')+
  scale_color_identity(name="",breaks=c("darkred","steelblue"),
  labels=c("Total Confirmed Cases","Total Deaths"),guide='legend')

both  

# histogr <- ggplot(all, aes(x = Date))+
#   geom_bar(aes(y=Total_Confirmed_Cases,color='darkred'),stat='identity')+
#   geom_bar(aes(y=Total_Deaths,color='steelblue'),stat='identity')+
#   scale_x_date(date_breaks = '30 day', date_labels = '%b')+
#   scale_color_identity(name="",breaks=c("darkred","steelblue"),
#                        labels=c("Total Confirmed Cases","Total Deaths"),guide='legend')+
#   scale_y_continuous(breaks=c(seq(0,8.5e+07,5e+06)))

