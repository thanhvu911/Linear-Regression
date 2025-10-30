library(readr);library(tidyverse)
#install.packages("AICcmodavg")
library(AICcmodavg)

FuelConsumption <- read_csv("C:/mat374/FuelConsumption.csv")

FuelConsumption <- FuelConsumption %>% mutate(Dlic = Drivers/Pop, Fuel = FuelC/Pop)

Fuel_m1 <- lm(Fuel ~ Dlic + Income + Miles + Tax + Pop, data=FuelConsumption)
Fuel_m2 <- lm(Fuel ~ Dlic + Income + Miles + Tax, data=FuelConsumption)
Fuel_m3 <- lm(Fuel ~ Dlic + Income + Miles, data=FuelConsumption)
Fuel_m4 <- lm(Fuel ~ Dlic + Income + Tax, data=FuelConsumption)
Fuel_m5 <- lm(Fuel ~ Dlic + Miles, data=FuelConsumption)

AIC(Fuel_m1)
AICc(Fuel_m1)
BIC(Fuel_m1)

summary(Fuel_m1)$adj.r.squared

models <- list(Fuel_m1, Fuel_m2, Fuel_m3, Fuel_m4, Fuel_m5)
sapply(models, function(x) paste(attr(terms(x), "term.labels"), collapse = ", ") )

comp.table.fn
data.frame(
  Variables = sapply(models, function(x) paste(attr(terms(x), "term.labels"), collapse = ", ")) ,
  p = sapply(models, function(x) length(coef(x))),
  Adj_R2 = sapply(models, function(x) summary(x)$adj.r.squared),
  AIC = sapply(models, AIC),
  AICc = sapply(models, AICc),
  BIC = sapply(models, BIC)
)

# EX1
IQsize <- read_csv("C:/mat374/IQsize.csv")

paste(colnames(IQsize)[2:4], collapse = " + ")
IQ_ml1 <- lm(PIQ~Brain + Height + Weight, data = IQsize)
IQ_ml2 <- lm(PIQ~Brain + Height, data = IQsize)
IQ_ml3 <- lm(PIQ~Brain + Weight, data = IQsize)
IQ_ml4 <- lm(PIQ~Brain, data = IQsize)
IQ_ml5 <- lm(PIQ~Height, data = IQsize)

models <- list(IQ_ml1, IQ_ml2, IQ_ml3, IQ_ml4, IQ_ml5)

summary(IQ_ml)

data.frame(
  Variables = sapply(models, function(x) paste(attr(terms(x), "term.labels"), collapse = ", ")) ,
  p = sapply(models, function(x) length(coef(x))),
  Adj_R2 = sapply(models, function(x) summary(x)$adj.r.squared),
  AIC = sapply(models, AIC),
  AICc = sapply(models, AICc),
  BIC = sapply(models, BIC)
)

IQ_m2