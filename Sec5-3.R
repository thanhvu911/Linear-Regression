library(readr); library(ggplot2)
Salary1 <- read_csv("C:/mat374/Salary1.csv")

Salary_lm <- lm(salary~gender+years+gender:years,Salary1)

summary(Salary_lm)

Salary1$fit <- fitted(Salary_lm)

ggplot(Salary1, aes(x = years, y = salary, color = gender)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = fit, linetype = gender)) +
  theme_minimal()

predict(Salary_lm, newdata = data.frame(gender=c("f","m","n"),years=c(5,5,10)))

Salary1

Salary_lm_r1 <- lm(salary~gender+years,Salary1)

summary(Salary_lm_r1)

anova(Salary_lm_r1, Salary_lm)


library(readr)
Salary2 <- read_csv("C:/mat374/Salary2.csv")

Salary2_lm <- lm(salary~gender+years+gender:years,Salary2)

summary(Salary2_lm)

Salary2$fit <- fitted(Salary2_lm)

ggplot(Salary2, aes(x = years, y = salary, color = gender)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = fit, linetype = gender)) +
  theme_minimal()

Salary_lm_r2 <- lm(salary~gender+years,Salary2)
summary(Salary_lm_r2)

predict(Salary_lm_r2, newdata = data.frame(gender=c("f","m","n"),years=c(5,5,10)))

library(readr)
Travel <- read_csv("C:/mat374/Travel.csv")

Travel_lm <- lm(Amount~Age+C+Age:C,data=Travel)
summary(Travel_lm)

Travel$fit <- fitted(Travel_lm)
Travel$C <- as.factor(Travel_lm)


ggplot(Travel, aes(x = Age, y = Amount, color = C)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = fit, linetype = C)) +
  theme_minimal()

predict(Travel_lm, newdata = data.frame(Age=c(40,40,50),C=c(0,1,1)))

library(readr)
NYC <- read_csv("C:/mat374/NYC.csv")

NYC_lm_full <- lm(Price~Food+Decor+Service+East,data=NYC)
summary(NYC_lm_full)

paste(colnames(NYC)[4:6],"East",collapse = " + ")

NYC_lm_full_int <- lm(Price ~ Food + Decor + Service + East + Food:East + Decor :East + Service :East)


#EX4
