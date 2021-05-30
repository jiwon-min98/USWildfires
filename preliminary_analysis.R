library(tidyverse)
library(ggcorrplot)
library(plyr)
library(purrr)
library(dplyr)
library(sp)
library(maps)
library(maptools)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
library(ggpubr)

load("~/Desktop/Project/data_train.RData")
summary(data_train_DF)

min(data$lon)
max(data$lon)
max(data$lat)
min(data$lat)
#data <- na.omit(data_train_DF) #removing rows with NA values

na <- data_train_DF[!complete.cases(data_train_DF),]
na
# taking only odd year values
fire <- data %>% filter(CNT > 0)#filtering 0 counts
nofire <- data %>% 
  filter(CNT == 0)

#longitude against latitude
plot(data_train_DF$lon, data_train_DF$lat)
plot(data_train_DF$CNT~data_train_DF$BA)


size_classes <- c('A' = '0-0.25', 'B' = '0.26-9.9', 'C' = '10.0-99.9', 'D' = '100-299', 'E' = '300-999',
                  'F' = '1000-4999', 'G' = '5000+')


#histogram of counts
hist(data$CNT, breaks = 10000, main = "histogram of count data", xlab = "Count")

#total counts per year
CNT_year <- data %>% dplyr::count(year, wt = CNT) 
ggplot(CNT_year, aes(year, n)) + 
  geom_bar(stat = "identity") + 
  labs(title = "total counts per year", y = "counts") + 
  theme_classic()



BA_year <- data %>% dplyr::count(year, wt = BA)

ggplot(BA_year, aes(year, n)) + 
  geom_bar(stat = "identity") + 
  labs(title = "total burnt area per year", y = "counts") + 
  theme_classic()

BA_year$n <- BA_year$n/100
CNT_year$BA_year <- BA_year$n
ggplot(CNT_year, aes(x=year)) + 
  geom_line(stat = "identity", aes(y = n), colour = "red") + 
  geom_area(stat = "identity", aes(y = BA_year)) +
  labs(title = "total count & burnt area per year", y = "counts & burned area (100acres)") + 
  theme_classic()

BA_year

#mean counts per year
mean_CNT_year <- data%>%
  group_by(year) %>%
  summarise_at(vars(CNT), list(mean = mean))
ggplot(mean_CNT_year, aes(x = year, y = mean)) +
  geom_bar(stat = "identity") +
  theme_classic()


#total counts per month
CNT_month <- data %>% dplyr::count(month, wt = CNT)
ggplot(CNT_month, aes(month, n)) + 
  geom_bar(stat = "identity") + 
  labs(title = "total counts per month", y = "counts") +
  theme_classic()

BA_month <- data %>% dplyr::count(month, wt = BA)
ggplot(BA_month, aes(month, n)) + 
  geom_bar(stat = "identity") + 
  labs(title = "total counts per month", y = "counts") +
  theme_classic()

#mean counts per month
mean_CNT_month <- data%>%
  group_by(month) %>%
  summarise_at(vars(CNT), list(mean = mean))
ggplot(mean_CNT_month, aes(x = month, y = mean)) +
  geom_bar(stat = "identity") +
  theme_classic()


data$date <- with(data, sprintf("%d-%02d", year, month))


CNT_month_year <- data %>% 
  group_by(date) %>%
  dplyr::summarise(sum_CNT = sum(CNT)) %>%
  separate(date, sep="-", into = c("year", "month"))


ggplot(CNT_month_year, aes(month, sum_CNT, group=factor(year), colour=factor(year))) +
  geom_line() +
  geom_point() +
  labs(x="Month", colour="Year", y = "Fire count") +
  theme_classic()



ggplot(data, aes(month, CNT)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  facet_wrap(vars(year))

ggplot(data, aes(month, BA)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  facet_wrap(vars(year))


CNTyear <- function(x){
  x <- filter(data, year == x)
  ggplot(x, aes(month, CNT)) +
    geom_bar(stat = "identity") +
    theme_classic()
}

CNTyear(2007)


#----------------------------------

#altitude
par(mfrow=c(1,2))
plot(CNT ~ altiMean, data, main = "mean altitude vs count")
plot(BA ~ altiMean, data, main = "mean altitude vs burnt area")
summary(data$altiMean)


#----------------------------------

#locations of fires occurred (with count)
ggplot(data, aes(x = lon, y = lat, colour = CNT)) + 
  geom_point() + 
  scale_colour_gradientn(colours = rev(rainbow(3)), trans = "log10") +
  theme_classic()

#locations of fires occurred by month (with count)
ggplot(data, aes(x = lon, y = lat, colour = CNT)) + 
  geom_point() + 
  scale_colour_gradientn(colours=rev(rainbow(3)), trans = "log10") + 
  facet_wrap(vars(month)) +
  theme_classic()

#locations of fires occurred by year (with count)
ggplot(data, aes(x = lon, y = lat, colour = CNT)) + 
  geom_point() + 
  scale_colour_gradientn(colours=rev(rainbow(3)), trans = "log10") + 
  facet_wrap(vars(year)) +
  theme_classic()


#locations of fires occurred (with burnt area) by year
ggplot(fire, aes(x = lon, y = lat, colour = BA)) + 
  geom_point() + 
  scale_colour_gradientn(colours = rev(rainbow(3)), trans = "log10") +
  facet_wrap(vars(year))

#locations of fires occurred (with burnt area) by month
ggplot(fire, aes(x = lon, y = lat, colour = BA)) + 
  geom_point() + 
  scale_colour_gradientn(colours = rev(rainbow(3)), trans = "log10") +
  facet_wrap(vars(month))


CNT_between <- function(x, y){
  CNT_x <- filter(data, CNT < y & CNT > x)
  ggplot(CNT_x, aes(x = lon, y = lat, colour = CNT)) + 
    geom_point() + 
    scale_colour_gradientn(colours=rev(rainbow(3)), trans = "log10") + 
    facet_wrap(vars(year)) +
    theme_classic()
}

CNT_between(0,10)

map_cnt <- function(year, month) {
  year_month <- data %>% 
    filter(year == year, month == month)
  ggplot(year_month, aes(x = lon, y = lat, colour = CNT)) + 
    geom_point() + 
    scale_colour_gradientn(colours=rev(rainbow(3)), trans = "log10")+
    theme_classic()
}
map_cnt(2007,7)

map_ba <- function(year, month) {
  year_month <- data %>% 
    filter(year == year, month == month)
  ggplot(year_month, aes(x = lon, y = lat, colour = BA)) + 
    geom_point() + 
    scale_colour_gradientn(colours=rev(rainbow(3)), trans = "log10")+
    theme_classic()
}
map_ba(2007,7)

#----------------------------- CLIMATE ------------
climate_data <- dplyr::rename(data, east_wind = clim1,
    north_wind = clim2,
    dewpoint_temp = clim3,
    temp = clim4,
    potential_evaporation = clim5,
    solar_radiation = clim6,
    thermal_radiation = clim7,
    pressure = clim8,
    evaporation = clim9,
    precipitation = clim10
)

climate <- data %>% 
  mutate(wind = sqrt(clim1^2 + clim2^2), dewpoint_temp = clim3-273.15, temp = clim4-273,15) %>%
  select(starts_with("clim"),-c(clim1, clim2, clim3, clim4), CNT, BA, wind, altiMean, temp, dewpoint_temp) %>% 
  dplyr::rename(
         potential_evaporation = clim5,
         solar_radiation = clim6,
         thermal_radiation = clim7,
         pressure = clim8,
         evaporation = clim9,
         precipitation = clim10
  )

#reordering data
climate <- climate[, c(9, 12, 11, 1, 5, 2, 3, 4, 6, 10, 7, 8)]
pearson_corr <- round(cor(climate, use="complete.obs"),2)
spearman_corr <- round(cor(climate, use="complete.obs", method = "spearman"))

res2 <- cor(climate, use="complete.obs", method = "kendall")

# Compute a matrix of correlation p-values
p.mat1 <- cor_pmat(climate, method = "pearson")
p.mat2 <- cor_pmat(climate, method = "spearman")


par(mfrow=c(1,2))
corrplot.mixed(pearson_corr, lower.col = "black", number.cex = .7, tl.col = "black", p.mat = p.mat1, tl.cex = 0.6)
corrplot.mixed(spearman_corr, lower.col = "black", number.cex = .7, tl.col = "black", p.mat = p.mat2,  tl.cex = 0.6,
               order = "original")







#ANOTHER METHOD FOR PLOTTING CORRELATION COEFFICIENTS
# ggcorrplot(pearson_corr, hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank") +
#   labs(title = "Pearson's correlation coefficients for weather variables")
# 
# ggcorrplot(spearman_corr, hc.order = FALSE, type = "lower", lab = TRUE, insig = "blank") +
#   labs(title = "Spearman's correlation coefficients for weather variables")





par(mfrow=c(2,5))
plot(CNT ~ wind, climate)
plot(CNT ~ dewpoint_temp, climate)
plot(CNT ~ temp, climate)
plot(CNT ~ potential_evaporation, climate)
plot(CNT ~ solar_radiation, climate)
plot(CNT ~ thermal_radiation, climate)
plot(CNT ~ pressure, climate)
plot(CNT ~ evaporation, climate)
plot(CNT ~ precipitation, climate)


# ggplot(climate, aes(wind,CNT)) +
#   geom_point()
# 
# ggplot(climate, aes(temp,CNT)) +
#   geom_point()
# 
# ggplot(climate, aes(precipitation, CNT)) +
#   geom_point()

#average temp by month (entire US)
meantemp <- data %>%
  dplyr::group_by(month) %>%
  dplyr::summarize(meantemp = mean(clim4, na.rm=TRUE))

mean_dewpoint_temp <- data %>%
  dplyr::group_by(month) %>%
  dplyr::summarize(mean = mean(clim3, na.rm = TRUE))

meantemp$mean_dewpoint <- mean_dewpoint_temp$mean
  
plot(data = climate, temp ~ dewpoint_temp) #spearman's correlation was +1
plot(data = climate, dewpoint_temp ~ evaporation) #spearman's correlation was -1
plot(data = climate, solar_radiation ~ thermal_radiation) #spearmans' correlation was -1
plot(data = climate, pressure ~ dewpoint_temp)
plot(data = climate, altiMean ~ dewpoint_temp)
plot(data = climate, pressure ~ altiMean)



#coordinates to state name

lonlat_to_state_sp <- function(pointsDF) {
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))

  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
 
  indices <- over(pointsSP, states_sp)

  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

coord <- cbind(data$lon, data$lat)
state <- lonlat_to_state_sp(coord)

data$state <- state

ggplot(data, aes(lon,lat, colour = state)) +
  geom_point()

# plot_state <- function(ste){
#   state_data <- data %>% dplyr::filter(state == ste)
#   x<- ggplot(state_data, aes(x=lon,y=lat), colour='red') +
#     geom_point()
#   print(x)
# }
# 
# plot_state("washington")


#total counts per state
area_state <- data %>% dplyr::count(state, wt = area)
area_state$n <- area_state$n/84 #7months * 12 years


CNT_state <- data %>% dplyr::count(state, wt = CNT) %>% arrange(desc(n))
head(CNT_state)
tail(CNT_state)
ggplot(CNT_state, aes(state, n)) + 
  geom_bar(stat = "identity") + 
  labs(title = "total counts per state", y = "counts")  +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#total burnt area by state
BA_state <- data %>% dplyr::count(state, wt = BA) %>% arrange(desc(n))
ggplot(BA_state, aes(state, n)) + 
  geom_bar(stat = "identity") + 
  labs(title = "total burnt area by state", y = "BA (acres)")  +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
head(BA_state)
tail(BA_state)

fire_state <- area_state %>%
  mutate(area = area_state$n) %>%
  mutate(CNT = CNT_state$n) %>%
  mutate(BA = BA_state$n)%>%
  mutate(CNT_per_area = CNT/area) %>%
  mutate(BA_per_area = BA/area)

ggplot(fire_state, aes(state, CNT_per_area)) + 
  geom_bar(stat = "identity") + 
  labs(title = "avg counts per area by state", y = "counts")  +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplot(fire_state, aes(state, BA_per_area)) + 
  geom_bar(stat = "identity") + 
  labs(title = "avg BA per area by state", y = "BA (acres)")  +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplot(fire_state, aes(state, area)) + 
  geom_bar(stat = "identity") + 
  labs(title = "state area")  +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#mean altitude by state
altiMean_state <- data %>% 
  group_by(state) %>%
  summarise_at(vars(altiMean), list(mean = mean))

ggplot(altiMean_state, aes(state, mean)) +
  geom_bar(stat = "identity") +
  labs(title = "mean altitude by state", y = "mean altitude") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#certain state by month
state_CNT_month <- function(x){
  state_CNT <- data %>% 
    dplyr::filter(state == x) %>%
    dplyr::count(month, wt = CNT)
  
  y <- ggplot(state_CNT, aes(month, n)) + 
    geom_bar(stat = "identity") +
    labs(y = "total count") +
    theme_classic()
  
  print(y)
}


state_BA_month <- function(x){
  state_BA <- data %>% 
    dplyr::filter(state == x) %>%
    dplyr::count(month, wt = BA)
  
  y <- ggplot(state_BA, aes(month, n)) + 
    geom_bar(stat = "identity") +
    labs(y = "total burnt area") +
    theme_classic()
  
  print(y)
}

state_avgtemp_month <- function(x){
  state_meantemp <- data %>% 
    dplyr::filter(state == x) %>%
    dplyr::group_by(month) %>%
    dplyr::summarize(mean = mean(clim4, na.rm=TRUE))
  
  state_meandewtemp <- data %>% 
    dplyr::filter(state == x) %>%
    dplyr::group_by(month) %>%
    dplyr::summarize(mean = mean(clim3, na.rm=TRUE))
  
  state_meantemp$meandewpoint <- state_meandewtemp$mean
  
  y <- ggplot(state_meantemp, aes(x = month)) + 
    geom_line(aes(y = mean, colour = "Temp." )) +
    geom_line(aes(y = meandewpoint, colour = "Dewpoint Temp.")) +
    labs(y = "temp (K)", title = "Average temperature and dewpoint temperature")+
    theme_classic()
  print(y)
}


state_avgprecip_month <- function(x){
  state_precip <- data %>% 
    dplyr::filter(state == x) %>%
    dplyr::group_by(month) %>%
    dplyr::summarize(mean = mean(clim10, na.rm=TRUE))
  
  y <- ggplot(state_precip, aes(month, mean)) + 
    geom_bar(stat = "identity") +
    labs(y = "avg precipitation")+
    theme_bw()
  print(y)
}


state <- function(x){
  p1 <- state_CNT_month(x)
  p2 <- state_BA_month(x)
  p3 <- state_avgtemp_month(x)
  p4 <- state_avgprecip_month(x)
  grid.arrange(p1, p2,p3, p4, ncol=2)
}

state("idaho")
state("california")
state("texas")



climate_in_state <- function(x){
  state<-data %>% 
  filter(state == x) %>%
  mutate(wind = sqrt(clim1^2 + clim2^2)) %>%
  select(starts_with("clim"),-c(clim1, clim2), CNT, BA, wind, altiMean) %>% 
  dplyr::rename(dewpoint_temp = clim3,
         temp = clim4,
         potential_evaporation = clim5,
         solar_radiation = clim6,
         thermal_radiation = clim7,
         pressure = clim8,
         evaporation = clim9,
         precipitation = clim10
  )
  print(state)
}

california_climate <- climate_in_state("california")
r <- cor(california_climate, use="complete.obs")
round(r,2)
ggcorrplot(r, hc.order = TRUE, type = "lower", lab = TRUE, pch.cex = 0.5, insig = "blank")

# par(mfrow=c(2,5))
# plot(CNT ~ wind, california_climate)
# plot(CNT ~ dewpoint_temp, california_climate)
# plot(CNT ~ temp, california_climate)
# plot(CNT ~ potential_evaporation, california_climate)
# plot(CNT ~ solar_radiation, california_climate)
# plot(CNT ~ thermal_radiation, california_climate)
# plot(CNT ~ pressure, california_climate)
# plot(CNT ~ evaporation, california_climate)
# plot(CNT ~ precipitation, california_climate)


newyork_climate <- climate_in_state("new york")
r <- cor(newyork_climate, use="complete.obs")
round(r,2)
ggcorrplot(r, hc.order = TRUE, type = "lower", lab = TRUE, pch.cex = 0.5, insig = "blank")



############BURNT AREA

ggplot(data, aes(CNT, BA)) +
  geom_point()

ggplot(climate, aes(wind, BA)) +
  geom_point()

ggplot(climate, aes(temp,BA)) +
  geom_point()

ggplot(climate, aes(precipitation, BA)) +
  geom_point()




#land cover

land_cover <- data %>% 
  select(starts_with("lc"), CNT, BA) %>% 
  dplyr::rename(cropland_rainfed = lc1,
                cropland_rainfed_herbaceous = lc2,
                mosaic_cropland = lc3,
                mosaic_natural_vegetation = lc4,
                tree_broadleaved_evergreen = lc5,
                tree_broadleaved_deciduous = lc6,
                tree_needleleave_evergreen = lc7,
                tree_needleleve_deciduous = lc8,
                tree_mixed = lc9,
                mosaic_tree_shrub = lc10,
                shrubland = lc11,
                grassland = lc12,
                sparse_vegetation = lc13,
                tree_cover_flooded_fresh_brakish_water = lc14,
                tree_cover_flooded_saline_water = lc15,
                shrub_herbaceous_cover_flooded = lc16,
                urban = lc17,
                water = lc18
  )


data[which.max(data$CNT),]
#max count was in new york [coord:(-73.75, 40.75 )] in 2015 may with burnt area of 70.2

data[which.max(data$BA),]
#max burnt area was in arizona [coord: (-109.25 33.7)] with total burnt area of 538052.4 with 6 fires


data[which.max(data$altiMean),]
#3369.013
data[which.min(data$altiMean),]
#1.109576

area_of_state <- data %>%
    dplyr::group_by(state) %>%
    dplyr::count(state, wt = area)

ggplot(area_of_state, aes(x=state, y=n)) +
  geom_bar(stat = "identity") +
  labs(y = "area", x = "state", title = "total area of state") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



land_cover_avg <- data %>%
  group_by(state) %>%
  dplyr::summarize(across(starts_with("lc"), mean))

land_cover_avg.melt <- melt(land_cover_avg, id.vars = "state")
land_cover_avg.melt <- filter(land_cover_avg.melt, value > 0.01)


ggplot(land_cover_avg.melt, aes(x=state, y=value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity",color='black') +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "land cover average by state", y = "land cover (%)")
# geom_text(aes(label = paste0(100*value,"%")),size = 3)

land_cover <- data %>%
  select(starts_with("lc"))%>% 
  dplyr::rename(cropland_rain = lc1,
                cropland_rainherbaceous = lc2,
                mosaic_cropland = lc3,
                mosaic_natural_veg = lc4,
                tree_broad_evergreen = lc5,
                tree_broad_deciduous = lc6,
                tree_needle_evergreen = lc7,
                tree_needle_deciduous = lc8,
                tree_mixed = lc9,
                mosaic_tree_shrub = lc10,
                shrubland = lc11,
                grassland = lc12,
                sparse_vegetation = lc13,
                tree_flooded_fresh_brakish = lc14,
                tree_flooded_saline = lc15,
                shrub_herbaceous_flooded = lc16,
                urban = lc17,
                water = lc18
  )
largest_lc <- colnames(land_cover)[max.col(land_cover,ties.method="first")]
data$largest_lc <- largest_lc

fire_by_landcover <- data %>% 
  dplyr::count(largest_lc, wt = CNT) %>% arrange(desc(n))

ggplot(fire_by_landcover, aes(largest_lc, n)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(y = "count") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))

ba_by_landcover <- data %>% dplyr::count(largest_lc, wt = BA) %>% arrange(desc(n))
ggplot(ba_by_landcover, aes(largest_lc, n)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(y = "BA") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

area_lc <- data %>% dplyr::count(largest_lc, wt = area)

lc_summary <- area_lc %>%
  mutate(area = n/84) %>% #7months * 12 years) %>%
  mutate(CNT = fire_by_landcover$n) %>%
  mutate(BA = ba_by_landcover$n) %>%
  mutate(CNT_by_area = CNT/n) %>%
  mutate(BA_by_area = BA/n)

ggplot(lc_summary, aes(largest_lc, n)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(y = "area") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

ggplot(lc_summary, aes(largest_lc, CNT_by_area)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(y = "Count", title = "Count per area by land cover" ) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

ggplot(lc_summary, aes(largest_lc, BA_by_area)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(y = "BA", title = "burnt area per area by land cover" ) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))


lc10 <- data %>%
  filter(largest_lc == "mosaic_tree_shrub")
ggplot(lc10, aes(lat, lon)) +
  geom_point()






# land_cover_avg <- data %>%
#   group_by(year) %>%
#   summarize(across(starts_with("lc"), mean))
# Land cover did change over time!! (though not a lot)


#######




