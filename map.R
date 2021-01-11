library(ggplot2);library(data.table);library(lubridate);library(dplyr)
library(grid);library(rworldmap)

worldMap <- getMap()
Cases <- dt %>% slice_max(order_by = Date, n = 1)#get max date
Cases[Country == 'US']$Country <- 'United States' #to fix inconsistency between names
countries <- c(unique(Cases$Country))#get names of countries
indworld <- which(worldMap$NAME%in%countries)#get the countries for which i have data
# Extract longitude and latitude border's coordinates of countries
coords <- lapply(indworld, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

coords <- do.call("rbind", coords); coords <- as.data.table(coords)
coords <- setnames(coords,'region','Country')
coords <- coords[order(Country)]#order both sets
Cases <- Cases[order(Country)]#to prepare for merging
merged <- merge(coords, Cases, all = TRUE)
breaks <- c(seq(0,max(Cases$Confirmed),2.5e+06))
P <- ggplot() + geom_polygon(data = merged, aes(x = long, y = lat,
    group = Country, fill = Confirmed/1e+06),
    colour = 'black',size = 0.1,show.legend = TRUE) +
    coord_map(xlim = c(-170, 170),  ylim = c(-50, 100))+
    scale_fill_continuous(name = 'Number of Cases \n in Millions',
    type='viridis',breaks=breaks/1e+6) + theme_void()
P

