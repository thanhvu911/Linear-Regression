library(readr)
Production <- read_delim("C:/mat374/Production.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)
View(Production)

prod_lm <- lm(RunTime ~ RunSize, data=Production)

summary(prod_lm)

predict(prod_lm,newdata = data.frame((RunSize=c(150,200))))

library(ggplot2)
ggplot(data=Production, aes(x=RunSize, y= RunTime)) +
  geom_point() +
  geom_smooth(method="lm", se=F, color = "darkblue")
  
#geom_abline(slope =prod_lm$coefficients[2])

#mse
mse <- sum(prod_lm$residuals^2)/(length(prod_lm$residuals)-2)
rmse <- sqrt(mse)
rmse

colnames(FreshmanGPA) <- c("GPA","ACT")


GPA_lm <- lm(GPA ~ ACT,data = FreshmanGPA)
summary(GPA_lm)

library(ggplot2)
ggplot(data=FreshmanGPA, aes(x=ACT, y= GPA)) +
  geom_point() +
  geom_smooth(method="lm", se=F, color = "darkblue")

predict(GPA_lm,newdata = data.frame((ACT=c(26))))



