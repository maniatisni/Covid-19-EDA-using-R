dt$Growth_factor_cases <- 
  (dt$Confirmed.ind-lag(dt$Confirmed.ind, n = 1))/lag(dt$Confirmed.ind, n = 1)
dt$Growth_factor_deaths <- 
  (dt$Deaths.inc-lag(dt$Deaths.inc, n = 1))/lag(dt$Deaths.inc, n = 1)
dt$Growth_factor_cases[is.infinite(dt$Growth_factor_cases)] <- 0
dt$Growth_factor_deaths[is.infinite(dt$Growth_factor_deaths)] <- 0

growthfactor <- ggplot(dt,aes(colour = Country),na.rm=TRUE)+
  geom_point(data=dt[Country == 'Greece' & Date >= '2020-12-12'],
  aes(x = Date, y = Growth_factor_cases),na.rm=TRUE)+
  geom_line(data=dt[Country == 'Greece' & Date >= '2020-12-12'],
  aes(x = Date, y = Growth_factor_cases),linetype='solid') +
  geom_point(data=dt[Country == 'Portugal' & Date >= '2020-12-12'],
  aes(x = Date, y = Growth_factor_cases),na.rm=TRUE)+
  geom_line(data=dt[Country == 'Portugal' & Date >= '2020-12-12'],
  aes(x = Date, y = Growth_factor_cases),linetype='solid')+
  geom_point(data=dt[Country == 'Azerbaijan' & Date >= '2020-12-12'],
  aes(x = Date, y = Growth_factor_cases),na.rm=TRUE)+
  geom_line(data=dt[Country == 'Azerbaijan' & Date >= '2020-12-12'],
  aes(x = Date, y = Growth_factor_cases),linetype='solid')+
  geom_point(data=dt[Country == 'Czechia' & Date >= '2020-12-12'],
             aes(x = Date, y = Growth_factor_cases),na.rm=TRUE)+
  geom_line(data=dt[Country == 'Czechia' & Date >= '2020-12-12'],
            aes(x = Date, y = Growth_factor_cases),linetype='solid')+
  xlab("")+ylab("")+labs(title='Confirmed Cases Growth Rate')+
  scale_y_continuous(labels = scales::percent,breaks = c(seq(-0.5,2,0.5)))+theme_dark()
growthfactor
growthfactor_deaths <- ggplot(dt,aes(colour = Country),na.rm=TRUE)+
  geom_point(data=dt[Country == 'Greece' & Date >= '2020-12-12'],
             aes(x = Date, y = Growth_factor_deaths),na.rm=TRUE)+
  
  geom_line(data=dt[Country == 'Greece' & Date >= '2020-12-12'],
            aes(x = Date, y = Growth_factor_deaths),linetype='solid',na.rm=TRUE) +
  
  geom_point(data=dt[Country == 'Portugal' & Date >= '2020-12-12'],
             aes(x = Date, y = Growth_factor_deaths),na.rm=TRUE)+
  geom_line(data=dt[Country == 'Portugal' & Date >= '2020-12-12'],
            aes(x = Date, y = Growth_factor_deaths),linetype='solid',na.rm=TRUE)+
  
  geom_point(data=dt[Country == 'Azerbaijan' & Date >= '2020-12-12'],
             aes(x = Date, y = Growth_factor_deaths),na.rm=TRUE)+
  geom_line(data=dt[Country == 'Azerbaijan' & Date >= '2020-12-12'],
            aes(x = Date, y = Growth_factor_deaths),linetype='solid',na.rm=TRUE)+
  
  geom_point(data=dt[Country == 'Czechia' & Date >= '2020-12-12'],
             aes(x = Date, y = Growth_factor_deaths),na.rm=TRUE)+
  geom_line(data=dt[Country == 'Czechia' & Date >= '2020-12-12'],
            aes(x = Date, y = Growth_factor_deaths),linetype='solid',na.rm=TRUE)+
  xlab("")+ylab("")+labs(title='Deaths Growth Rate')+
  
  
  
  scale_y_continuous(labels = scales::percent)+theme_dark()

growthfactor_deaths


plotcases <- ggplot(dt, aes(colour = Country),legend=FALSE)+
  geom_line(data = dt[Country == 'Greece'],aes(x=Date,y=Confirmed),size=.8)+
  geom_line(data = dt[Country == 'Azerbaijan'],aes(x=Date,y=Confirmed),size=.8)+
  geom_line(data = dt[Country == 'Czechia'],aes(x=Date,y=Confirmed),size=.8)+
  geom_line(data = dt[Country == 'Sweden'],aes(x=Date,y=Confirmed),size=.8)+
  geom_line(data = dt[Country == 'Portugal'],aes(x=Date,y=Confirmed),size=.8)+
  xlab("")+ylab("Confirmed Cases")+
  scale_x_date(date_breaks = '30 day', date_labels = '%b')

plotdeaths <- ggplot(dt, aes(colour = Country))+
  geom_line(data = dt[Country == 'Greece'],aes(x=Date,y=Deaths),size=.8)+
  geom_line(data = dt[Country == 'Azerbaijan'],aes(x=Date,y=Deaths),size=.8)+
  geom_line(data = dt[Country == 'Czechia'],aes(x=Date,y=Deaths),size=.8)+
  geom_line(data = dt[Country == 'Sweden'],aes(x=Date,y=Deaths),size=.8)+
  geom_line(data = dt[Country == 'Portugal'],aes(x=Date,y=Deaths),size=.8)+
  xlab("")+ylab("Deaths")+
  scale_x_date(date_breaks = '30 day', date_labels = '%b')

library(ggpubr)
ggarrange(plotcases,plotdeaths,nrow=1, common.legend = TRUE, legend="bottom")


plotGreece <- ggplot(dt[Country == 'Greece'])+
  geom_line(stat='identity',aes(x=Date,y=Confirmed),size=.8)+
  scale_x_date(date_breaks = '30 day', date_labels = '%b')+
  geom_vline(xintercept=as.numeric(dt[Date=='2020-03-23']$Date), linetype='solid')+
  geom_vline(xintercept=as.numeric(dt[Date=='2020-05-04']$Date), linetype='dashed')+
  geom_vline(xintercept=as.numeric(dt[Date=='2020-11-07']$Date), linetype=4)+
  scale_y_log10()
plotGreece

