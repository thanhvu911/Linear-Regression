library(readr);library(car);library(tidyverse);library(ggfortify)

Salary1 <- read_csv("C:/mat374/Salary1.csv")
Salary_lm <- lm(salary~gender+years+gender:years, data = Salary1)

autoplot(Salary_lm)

car::avPlots(lm(salary~gender+years+gender:years, data = Salary1), pch=16)

## std. res v.s. predictors

# Extract fitted values and standardized residuals
diag_df <- Salary1 %>%
  mutate(fitted = fitted(Salary_lm),
         std_resid = rstandard(Salary_lm),
         gender = ifelse(gender == "m", 1, 0) )

ggplot(diag_df, aes(x=years, y=std_resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "hotpink2", linetype = "dashed") +
  labs(x = "Predictor: Years ", y = "Standardized Residuals")


diag_df %>%
  filter(gender == 1) %>%
  ggplot(aes(x=years, y=std_resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "hotpink2", linetype = "dashed") +
  labs(x = "Predictor: Years ", y = "Standardized Residuals")


ggplot(diag_df, aes(x=years, y=std_resid)) +
  geom_point(alpha = 0.9) +
  geom_hline(yintercept = 0, color = "hotpink2", linetype = "dashed") + 
  facet_wrap(~ gender , scales = "free_x", labeller = labeller(gender = c("1" = "Male", "0" = "Female")))  +
  labs(x = "Predictor: Years ", y = "Standardized Residuals")


# Plot standardized residuals vs each predictor and fitted values 
diag_df %>%
  pivot_longer(cols = c(gender, years, fitted), names_to = "variable", values_to = "xval") %>%
  ggplot(aes(x = xval, y = std_resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "hotpink2", linetype = "dashed") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x="",y = "Standardized Residuals", title = "Standardized Residuals vs Predictors and Fitted Values") 

#EX1
library(readr)
NYC <- read_csv("C:/mat374/NYC.csv")

NYC_lm_final <- lm(Price~ Food + Decor + East,data = NYC)
autoplot(NYC_lm_final)

car::avPlots(lm(Price~ Food + Decor + East,data = NYC), pch=16)


## std. res v.s. predictors

# Extract fitted values and standardized residuals
diag_df <- NYC %>%
  mutate(fitted = fitted(NYC_lm_final),
         std_resid = rstandard(NYC_lm_final))


# Plot standardized residuals vs each predictor and fitted values 
diag_df %>%
  pivot_longer(cols = c(Food, Decor, East), names_to = "variable", values_to = "xval") %>%
  ggplot(aes(x = xval, y = std_resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "hotpink2", linetype = "dashed") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x="",y = "Standardized Residuals", title = "Standardized Residuals vs Predictors and Fitted Values") 

avPlots(NYC_lm_final, pch=16)

#EX3
library(readr)
FuelConsumption <- read_csv("C:/mat374/FuelConsumption.csv")
FuelConsumption <- FuelConsumption %>% mutate(Dlic = Drivers/Pop, Fuel = FuelC/Pop)

#b
library(GGally)
ggpairs(FuelConsumption[c("Fuel", "Dlic", "Tax", "Income", "Miles")])

#c
Fuel_lm <- lm(Fuel ~ Dlic + Income + Miles + Tax, data = FuelConsumption)

summary(Fuel_lm)

#d
autoplot(Fuel_lm)

#e
car::avPlots(Fuel_lm,pch = 16)

#EX1

res <- residuals(Salary_lm)
library(nortest)
ad.test(res)
shapiro.test(res)
lillie.test(res)

autoplot(NYC_lm_final)
res <- residuals(NYC_lm_final)
ad.test(res)
shapiro.test(res)
lillie.test(res)
