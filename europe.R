library(countrycode)
dt <- dt[Country != 'Diamond Princess']#THIS IS A CRUISE SHIP!NOT A COUNTRY!
dt <- dt[Country != 'MS Zaandam']#THIS IS A CRUISE SHIP!NOT A COUNTRY!
dt$continent <- countrycode(sourcevar = dt$Country, origin = "country.name", destination = 'continent')
dt[Country == 'Kosovo']$continent <- 'Europe'#this was a leftover.
dt <- as.data.table(dt)


library(latex2exp)

Europe_continent <- dt[continent == 'Europe']
Europe_cases <- Europe_continent %>% slice_max(order_by = Date, n = 1)
Europe_cases$Confirmed_normalized <- 
  round((Europe_cases$Confirmed-mean(Europe_cases$Confirmed))/sd(Europe_cases$Confirmed),2)
Europe_cases$flag_cases <- ifelse(Europe_cases$Confirmed_normalized < 0, "Below","Above")
Europe_cases <- Europe_cases[order(Europe_cases$Confirmed),]
Europe_cases$Country <- factor(Europe_cases$Country, levels = Europe_cases$Country)
as_above_so_below <- ggplot(Europe_cases, aes(x=Country, y = Confirmed_normalized, 
                                          label = Confirmed_normalized))+
  geom_bar(stat = 'identity', width = .5, aes(fill = flag_cases))+
  scale_fill_manual(name = 'Confirmed Cases', labels = c("Above Average", "Below Average"), 
                    values = c("Above"="darkred", "Below"="steelblue")) +
  labs(title="Normalised Confirmed Cases for European Countries") +
  ylab(TeX("$\\frac{Cases_i-< Cases >}{\\sigma_{Cases}}$"))+
  coord_flip()
Europe_deaths <- Europe_continent %>% slice_max(order_by = Date, n = 1)
Europe_deaths$Deaths_normalized <- 
  round((Europe_deaths$Deaths-mean(Europe_deaths$Deaths))/sd(Europe_deaths$Deaths),2)
Europe_deaths$flag_deaths <- ifelse(Europe_deaths$Deaths_normalized < 0, "Below","Above")
Europe_deaths <- Europe_deaths[order(Europe_deaths$Deaths),]
Europe_deaths$Country <- factor(Europe_deaths$Country, levels = Europe_deaths$Country)
as_above_so_below_2 <- ggplot(Europe_deaths, aes(x=Country, y = Deaths_normalized, 
                                                 label = Deaths_normalized))+
  geom_bar(stat = 'identity', width = .5, aes(fill = flag_deaths))+
  scale_fill_manual(name = 'Deaths', labels = c("Above Average", "Below Average"), 
                    values = c("Above"="darkred", "Below"="steelblue")) +
  labs(title="Normalised Deaths for European Countries") +
  ylab(TeX("$\\frac{Deaths-< Deaths >}{\\sigma_{Deaths}}$"))+
  coord_flip()
as_above_so_below_2

