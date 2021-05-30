


load("~/Desktop/Project/data_train.RData")

data_train_DF<- data_train_DF %>%
  mutate(wind = sqrt(clim1^2 + clim2^2))


# include this if you want to change names of climate covariates
#data_train_DF<- data_train_DF %>%
# mutate(wind = sqrt(clim1^2 + clim2^2)) %>%
# dplyr::rename(dew_temp = clim3,
#               temp = clim4,
#               pot_evap = clim5,
#               solar_rad = clim6,
#               thermal_rad = clim7,
#               pressure = clim8,
#               evap = clim9,
#               precip = clim10
# )



data_train_DF <- subset(data_train_DF, select = -c(clim1, clim2) )


dat_scaled <- as.data.frame(scale(data_train_DF[,c(5,8:36)]))
data_scaled <- cbind(data_train_DF[,c(1:4,6,7)], dat_scaled)
rm(dat_scaled)




