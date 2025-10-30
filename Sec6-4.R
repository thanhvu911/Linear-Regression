library(readr);library(car);library(tidyverse);library(ggfortify)

Salary1 <- read_csv("Data/Ch5/Salary1.csv")
Salary_lm <- lm(salary~gender+years+gender:years, data = Salary1)


vif(Salary_lm, type="predictor")

vif(lm(salary~gender+years,Salary1))


FuelConsumption <- read_csv("Data/Ch6/FuelConsumption.csv")

FuelConsumption <- FuelConsumption %>% mutate(Dlic = Drivers/Pop, Fuel = FuelC/Pop)

Fuel_lm <- lm(Fuel ~ Dlic + Income + Miles + Tax, data=FuelConsumption)

vif(Fuel_lm)

vif(lm(Fuel ~ Dlic + Income + Miles + Tax + Pop, data=FuelConsumption) )
