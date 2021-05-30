library(pscl)

library(dplyr)
load("~/Desktop/Project/data_train.RData")

data_train_DF<- data_train_DF %>%
  mutate(wind = sqrt(clim1^2 + clim2^2)) %>%
  dplyr::rename(dew_temp = clim3,
                temp = clim4,
                pot_evap = clim5,
                solar_rad = clim6,
                thermal_rad = clim7,
                pressure = clim8,
                evap = clim9,
                precip = clim10
  ) %>%
  subset(select = -c(clim1, clim2))


no_na <- na.omit(data_train_DF)  
odd_year <- filter(data_train_DF, year %% 2 == 1)

data <- subset(odd_year, select = -c(BA, lon, lat, month, year, area))

model1 <- glm(CNT ~., data = data, family = "poisson")
summary(model1)
aov(model1)
vif(model1)

data2 <- subset(data, select = -c(lc11, lc2))
model2 <- glm(CNT ~., data = data2, family = "poisson")
summary(model2)

#selecting only meteorological variables
data3 <- data %>% select(!starts_with("lc"))
model3 <- glm(CNT ~., data = data3, family = "poisson")
summary(model3)

anova(model1, model2, model3)

model3_back_aic <- step(model3, direction = "backward")
model3_start <- glm(CNT ~ 1, data = data3, family = "poisson")
model3_forward_aic <- step(
  model3_start, 
  scope = CNT ~ altiMean + dew_temp + temp + pot_evap + solar_rad+thermal_rad+pressure+evap+precip+wind, 
  direction = "forward")

