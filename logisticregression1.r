library(dplyr) 
library(InformationValue)
library(car)

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
  )


data_train_DF <- subset(data_train_DF, select = -c(clim1, clim2) )


#temperature kelvin to celsius
data_train_DF$temp <- data_train_DF$temp - 273.15
data_train_DF$dew_temp <- data_train_DF$dew_temp - 273.15

#radiation j/(m^2) to kj/(m^2)
data_train_DF$solar_rad <- data_train_DF$solar_rad/1000
data_train_DF$thermal_rad <- data_train_DF$thermal_rad/1000


no_na <- na.omit(data_train_DF)  
data <- filter(data_train_DF, year %% 2 == 1)
data$fire <- ifelse(data$CNT>0, 1, 0)


#Logistic regression model for fire with all variables
fire_data <- subset(data, select = -c(CNT, BA, month, year, area))
no_na <- subset(data, select = -c(CNT, BA, month, year, area) )
table(fire_data$fire)
#0      1 
#182767 111485 

fit <- glm(fire ~., data = fire_data, family = "binomial")
summary(fit)
aov(fit)

vif(fit)
#higher the values, higher the multicolinearity 

predicted <- predict(fit, no_na, type="response") #prediction with data with no na's

optCutOff <- optimalCutoff(no_na$fire, predicted)[1]
misClassError(no_na$fire, predicted, threshold = optCutOff)
#0.2577
plotROC(no_na$fire, predicted)
#percentage of true positives accurately predicted by a given logit model 
#Greater the area under the ROC curve, better the predictive ability of the model




#Logistic regression model for fire with meteorological variables
climate_data <- fire_data %>% select(!starts_with("lc"))
fit2 <- glm(fire ~., data = climate_data, family = "binomial")
summary(fit2)
aov(fit2)
vif(fit2)

predicted2 <- predict(fit2, no_na, type="response") #prediction with data with no na's





#removing pressure (due to high negative correlation with altimean)
climate_data2 <- climate_data %>% select(!pressure)
fit2a <- glm(fire ~., data = climate_data2, family = "binomial")
summary(fit2a)
aov(fit2a)
vif(fit2a)

anova(fit, fit2, fit2a)




#removing dew_temp
climate_data3 <- climate_data2 %>% select(!dew_temp)
fit2b <- glm(fire ~., data = climate_data3, family = "binomial")
summary(fit2b)
aov(fit2b)
vif(fit2b)


#removing precipitation
climate_data4 <- climate_data3 %>% select(!precip)
fit2c <- glm(fire ~., data = climate_data4, family = "binomial")
summary(fit2c)

aov(fit2c) 
vif(fit2c)



climate_back_aic <- step(fit2, direction = "backward")
climate_mod_start <- glm(fire ~ 1, data = climate_data, family = "binomial")
climate_forward_aic <- step(
  climate_mod_start, 
  scope = fire ~ altiMean + dew_temp + temp + pot_evap + solar_rad+thermal_rad+pressure+evap+precip+wind, 
  direction = "forward")

climate_both_aic = step(
  climate_mod_start, 
  scope = fire ~ altiMean + dew_temp + temp + pot_evap + solar_rad+thermal_rad+pressure+evap+precip+wind, 
  direction = "both")


data_back_aic <- step(fit, direction = "backward")
