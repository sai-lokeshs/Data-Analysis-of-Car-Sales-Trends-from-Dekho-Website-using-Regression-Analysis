# import libraries
library(tidyverse)
library(dplyr)
library(inspectdf)
library(magicfor)
library(GGally)
library(ggplot2)
library(viridis)
library(MLmetrics)
library(car)
library(caret)
library(vtreat)
library(lmtest)
library(pastecs)
library(magrittr)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(forecast)
library(tidyverse)
library(fpp)
library(forecast)
library(backtest)
library(quantmod)
library(lubridate)
library(sos)
library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
findFn("starts_with")
# import data - Data imported from the System
car <- read.csv("/Users/sailokeshsiddanathi/Documents/stats project/CarFinal.csv", na.strings=c("null", "","NA"), stringsAsFactors = TRUE)

#Data Transformations - To Perform Data cleaning and Transformation operations
DT::datatable(car, rownames = FALSE)


# check duplicates
table(duplicated(car))

# check Not Available Values
colSums(is.na(car))



#Reading Data and getting the Summary
summary(car)


#Quick data summary of any vehicle
glimpse(car-3)

#Checking if our dependent variable follows a Normal Distribution
hist(car$selling_price)
hist(car$year)
hist(car$km_driven)


#Checking the linearity between the dependent and Linear variable
plot(selling_price ~ year, data = car)

#Checking the correlation
cor(car$year,car$selling_price)


#Bar graph of Seller Types

ggplot(data = car, aes(x=reorder(seller_type, seller_type, function(x)-length(x)), 
                       fill = seller_type)) +
  geom_bar() + labs(x='Seller Type') + labs(title = "Bar Graph of Seller Type")

#Bar graph of FuelType cars Available

ggplot(data = car, aes(x=reorder(fuel, fuel, function(x)-length(x)), fill = fuel)) +
  geom_bar() + labs(x='FuelType') + labs(title = "Bar Graph of Fuel") 

# Bar graph by Owner number
ggplot(data = car, aes(x=reorder(owner, owner, function(x)-length(x)), fill = owner)) +
  geom_bar() + labs(x='Owner') + labs(title = "Bar Graph of Owner") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Model 1: Linear Regression - To validate Selling_Price vs Km_Driven

km_driven.selling_price.lm <- lm(selling_price ~ km_driven, data = car)

summary(km_driven.selling_price.lm)

#Result ----------------------------------------------------------------------------
#- The considerable thing to be observed here is the p value is <2.2e-16)
#which says whether the model fits the data well or not

#Value of y-intercept is 6.621
#Estimated effect on Kms driven is -2.385e+00
# T-value us -12.91

#-------------------------------------------------------------------------------------

# Multiple Regression : To Validate Km_driven vs Year vs Selling_Price

km_driven.year.lm <- lm(km_driven ~ year + selling_price, data = car)

summary(km_driven.year.lm)

#Result ----------------------------------------------------------------------------
#The estimated effect of Year on kilometers Driven is -0.00454, 
# while The estimated effect of Selling Price on  -0.001807

# The standard errors for these both are very high and t Values are very small(-27 and -1 respectively)
# P Value refers these small errors and the high T statisticS



#so these correlations would not be nearly as evident in real life!
------------------------------------------------------------------------------------
  #### - Checking   homoscedasticity
  #Simple Regression - To make sure that our models fit the homoscedasticity assumption of the linear model.
  
  par(mfrow=c(2,2)) 
## To Divide the Plots into 2 rows and 2 Columns
plot(km_driven.selling_price.lm) ## To check and validate if our data meets the model expectations
par(mfrow=c(1,1))

# Multiple Regression  - We should run the following code to ensure that our model is a good fit for the data 
#and that there is no large variation in the model error:

par(mfrow=c(2,2))
plot(km_driven.year.lm)
par(mfrow=c(1,1))


#Visualizations: Simple Regression - Plotting the data on graph

year.graph<-ggplot(car, aes(x=selling_price, y=km_driven))+
  geom_point()
year.graph

## Adding Linear regression Line to  Plotted Data

year.graph <- year.graph + geom_smooth(method="lm", col="black")

# geom_smooth -- for adding Regression Line

year.graph

## Adding equation for regression Line

year.graph <- year.graph + stat_regline_equation(label.x = 5, label.y = 7)

year.graph

# 
year.graph +
  theme_bw() +
  labs(title = "Reporting selling_price as function of Km_driven",
       x = "selling_price (Indian Rupees_INR)",
       y = " km_driven")


#-------------------------------------------------------------------------------------
## Visualization: For Multiple Regression 

plottingnew.data <- expand.grid(km_driven = seq(min(car$km_driven), max(car$km_driven), length.out=30),
                                Year=c(min(car$year), mean(car$year), max(car$year)))

plottingnew.data$km_driven <- round(plottingnew.data$km_driven, digits = 5)

plottingnew.data$km_driven <- as.factor(plottingnew.data$km_driven)

# Plotting the final data

km_driven.plot <- ggplot(car, aes(x=year, y=km_driven)) +
  geom_point()

km_driven.plot

#Time Series Forecast

car_sale <- read.csv("C:/Users/Admin/Desktop/cars.data.csv", 
                     na.strings=c("null", "","NA"), 
                     stringsAsFactors = TRUE)

glimpse(car_sale)

#Creating ID Variable
car_sale$id <- 1:nrow(car_sale) 

#Making this reproducable by setting Random seed
set.seed(899)

#Use 70% of dataset as training set and remaining 30% as testing set 
train <- car_sale %>% dplyr::sample_frac(0.7)
test  <- dplyr::anti_join(car_sale, train, by = 'selling_price')

#Printing number of rows in testing and training data
nrow(train); nrow(test)

car_tsobject <- ts(train[, 6], start = c(1950, 1), end = c(2022, 12), frequency = 12)

# create a utility function for calculating Mean Absolute Percentage Error (or MAPE), 
#which shall be used to evaluate the performance of the forecasting models.

mape <- function(actual,pred)
{
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#Naive Forecasting Method

naive_mod <- naive(car_tsobject, h = 53)
summary(naive_mod)

test$naive = 52000
mape(test$unemploy,test$naive)  ## NaN -- Almost Zero


#Simple Exponential Smoothing
simpleExp_model <- ses(car_tsobject, h = 53)
summary(simpleExp_model)

df_fc = as.data.frame(simpleExp_model)
test$simplexp = df_fc$`Point Forecast`
mape(test$unemploy, test$simplexp) #NaN -- Mape is Almost Zero

#Arima Model
arima_model <- auto.arima(car_tsobject)
summary(arima_model)
fore_arima = forecast::forecast(arima_model, h=53)
df_arima = as.data.frame(fore_arima)
test$arima = df_arima$`Point Forecast`
mape(test$unemploy, test$arima)  #NaN -- Mape is Almost Zero


#TBATS -- Model
model_tbats <- tbats(car_tsobject)
summary(model_tbats)
for_tbats <- forecast::forecast(model_tbats, h = 53)
df_tbats = as.data.frame(for_tbats)
test$tbats = df_tbats$`Point Forecast`
mape(test$unemploy, test$tbats)  #NaN -- Mape is Almost Zero
