library(countrycode)
library(gganimate)

dt <- dt[Country != 'Diamond Princess'] 
dt <- dt[Country != 'MS Zaandam'] 
dt$continent <- countrycode(sourcevar = dt$Country, origin = "country.name", destination = 'continent')
dt[Country == 'Kosovo']$continent <- 'Europe'
dt <- as.data.table(dt)
cases_by_continent <- dt[,sum(Confirmed), by = .(continent,Date)]
names(cases_by_continent) <- c("Continent","Date","Cumulative_cases")
cases_by_continent$Per <- 
  round(100*cases_by_continent$Cumulative_cases/sum(cases_by_continent$Cumulative_cases),3)
xists <-  ggplot(cases_by_continent, aes(x = Date, y = Cumulative_cases, colour = Continent))+
  geom_freqpoly(stat = 'identity',size = 1.5)+
  scale_x_date(date_breaks = '30 day', date_labels = '%b')

anim <- xists + transition_reveal(Date)+view_follow(fixed_x = TRUE) 
animate(anim, fps = 20, duration = 15)
