# Lab 2
# Max Troeger (troegm@rpi.edu)
# January 31, 2025

library(sandwich)
library(lmtest)
library(readr)
library(ggplot2)
NY_House_Dataset <- read_csv("Documents/School/data_analytics/repo/Lab2/NY-House-Dataset.csv")

# Remove outliers and erroneous values
NY_House_Dataset <- na.omit(NY_House_Dataset)
NY_House_Dataset <- subset(NY_House_Dataset,PRICE<195000000)
NY_House_Dataset <- subset(NY_House_Dataset,PROPERTYSQFT!=2184.207862)

NY_House_Dataset$LOGPRICE <- log10(NY_House_Dataset$PRICE)
NY_House_Dataset$LOGPROPERTYSQFT <- log10(NY_House_Dataset$PROPERTYSQFT)

plot(NY_House_Dataset$LOGPROPERTYSQFT,NY_House_Dataset$LOGPRICE)

##
# Model 1 - LOGPRICE vs LOGPROPERTYSQFT
##

fit1 <- lm(LOGPRICE~LOGPROPERTYSQFT,data=NY_House_Dataset)
#coeftest(fit1)
summary(fit1)

## Plotting LOGPRICE vs LOGPRICE
ggplot(NY_House_Dataset, aes(x = LOGPROPERTYSQFT, y = LOGPRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(fit1, aes(x = .fitted, y = .resid)) + geom_point()

##
# Model 2 - LOGPRICE vs LOGPROPERTYSQFT & BEDS
##

fit2 <- lm(LOGPRICE~LOGPROPERTYSQFT + BEDS,data=NY_House_Dataset)
#coeftest(fit2)
summary(fit2)

## Plotting LOGPRICE vs BEDS
ggplot(NY_House_Dataset, aes(x = BEDS, y = LOGPRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(fit2, aes(x = .fitted, y = .resid)) + geom_point()

##
# Model 3 - LOGPRICE vs LOGPROPERTYSQFT, BEDS, & BATH
##

fit3 <- lm(LOGPRICE~LOGPROPERTYSQFT+BATH+BEDS,data=NY_House_Dataset)
#coeftest(fit3)
summary(fit3)

## Plotting LOGPRICE vs BATH
ggplot(NY_House_Dataset, aes(x = BATH, y = LOGPRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(fit3, aes(x = .fitted, y = .resid)) + geom_point()


##
# Boxplot & QQ Plots
##
boxplot(NY_House_Dataset$LOGPROPERTYSQFT,NY_House_Dataset$BEDS,NY_House_Dataset$BATH)

qqnorm(NY_House_Dataset$LOGPROPERTYSQFT)
qqline(NY_House_Dataset$LOGPROPERTYSQFT) 

qqnorm(NY_House_Dataset$BATH)
qqline(NY_House_Dataset$BATH) 

qqbeta(NY_House_Dataset$BEDS)
qqline(NY_House_Dataset$BEDS) 

ggplot(NY_House_Dataset, aes(x=LOGPROPERTYSQFT)) +
  stat_ecdf() +
  
ggplot(NY_House_Dataset, aes(x=BATH)) +
  stat_ecdf()

ggplot(NY_House_Dataset, aes(x=BEDS)) +
  stat_ecdf()