library(countrycode)
dt <- dt[Country != 'Diamond Princess']#THIS IS A CRUISE SHIP!NOT A COUNTRY!
dt <- dt[Country != 'MS Zaandam']#THIS IS A CRUISE SHIP!NOT A COUNTRY!
dt$continent <- countrycode(sourcevar = dt$Country, origin = "country.name", destination = 'continent')
dt[Country == 'Kosovo']$continent <- 'Europe'#this was a leftover.
dt <- as.data.table(dt)



library(rworldmap);library(ggplot2);library(dplyr);library(data.table)
worldMap <- getMap()
#we made the continent column on a previous plot
Europe_continent <- dt[continent == 'Europe']#Fix inconsistencies
Europe_continent[Country == 'Czechia']$Country <- 'Czech Rep.'#between namings
Europe_continent[Country == 'North Macedonia']$Country <- 'Macedonia'#Ouch  
Europe_cases <- Europe_continent %>% slice_max(order_by = Date, n = 1)
countries <- c(unique(Europe_continent$Country))
indEU <- which(worldMap$NAME%in%countries)
# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region");return(df)})

europeCoords <- as.data.table(do.call("rbind", europeCoords))
europeCoords <- europeCoords[order(region)]
europeCoords <- setnames(europeCoords,'region','Country')
Europe_cases <- Europe_cases[order(Country)]
breaks <- c(seq(0,max(Europe_cases$Confirmed),0.5e+06))
try <- merge(europeCoords, Europe_cases, all = TRUE)
E <- ggplot() + geom_polygon(data = try, aes(x = long, y = lat,
     group = Country, fill = Confirmed/1e+06),
     colour = "black", size = 0.1,show.legend = TRUE) +
     coord_map(xlim = c(-19, 60),  ylim = c(32, 69)) +
     scale_fill_continuous(name = 'Number of Cases \n in Millions',
     breaks=breaks/1e+06)+theme_void()