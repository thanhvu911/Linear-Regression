library(readr);library(tidyverse);library(AICcmodavg);library(MASS);library(ggfortify)


FuelConsumption <- read_csv("C:/mat374/FuelConsumption.csv")

FuelConsumption <- FuelConsumption %>% mutate(Dlic = Drivers/Pop, Fuel = FuelC/Pop)

Fuel.df <- FuelConsumption[, c("Fuel", "Income", "Miles", "Pop", "Tax")]

# Full model with all predictors
Fuel_full_m <- lm(Fuel ~ ., data = Fuel.df)

# Null (intercept-only) model
Fuel_null_m <- lm(Fuel ~ 1, data = Fuel.df)

#Forward
Fuel_ml <- stepAIC(Fuel_null_m, scope = list(lower = Fuel_null_m, upper = Fuel_full_m), 
     direction = "forward", trace=T)

summary(Fuel_ml)

stepAIC(Fuel_null_m, scope = list(lower = Fuel_null_m, upper = Fuel_full_m), 
        direction = "forward", trace=F, k = log(nrow(Fuel.df)))

#Backward
stepAIC(Fuel_full_m, scope = list(lower = Fuel_null_m, upper = Fuel_full_m), direction = "backward", trace=T)

#Stepwise
stepAIC(Fuel_null_m, scope = list(lower = Fuel_null_m, upper = Fuel_full_m), direction = "both", trace=T)
stepAIC(Fuel_full_m, scope = list(lower = Fuel_null_m, upper = Fuel_full_m), direction = "both", trace=T)

#1

Cement <- read_csv("C:/mat374/Cement.csv")

Cement_full_m <- lm(Heat ~ ., data = Cement)

Cement_null_m <- lm(Heat ~ 1, data = Cement)

stepAIC(Cement_null_m, scope = list(lower = Cement_null_m, upper = Cement_full_m), 
                   direction = "forward", trace=T)

stepAIC(Cement_full_m, scope = list(lower = Cement_null_m, upper = Cement_full_m), 
        direction = "backward", trace=T)

stepAIC(Cement_full_m, scope = list(lower = Cement_null_m, upper = Cement_full_m), 
        direction = "both", trace=T)

Cement_ml <- stepAIC(Cement_null_m, scope = list(lower = Cement_null_m, upper = Cement_full_m), 
        direction = "both", trace=T)

summary(Cement_ml)
car::vif(Cement_ml)

Cement_m2 <- lm(Heat ~ Pct_Alumina + Pct_Silicate, data = Cement)
summary(Cement_m2)
car::vif(Cement_m2)

autoplot(Cement_m2)

shapiro.test(residuals(Cement_m2))

