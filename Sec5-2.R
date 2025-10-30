library(readr);library(ggplot2)


IQsize <- read_csv("Data/Ch5/IQsize.csv")

library(GGally)
ggpairs(IQsize)

IQ_lm <- lm(PIQ~., data=IQsize)

#paste(colnames(IQsize)[2:4], collapse = " + ")

#EX1-a-e
summary(IQ_lm)

#EX1-f
IQ_lm_r1 <- lm(PIQ ~ Brain, data = IQsize)
anova(IQ_lm_r1, IQ_lm)


F_stat <- (2875.7/2)/(13322/34)
pf(F_stat, 2, 34, lower.tail = F)

library(readr)
NYC <- read_csv("C:/mat374/NYC.csv")

paste(colnames(NYC)[3:7],collapse = " + ")

NYC_lm <- lm(Price ~ Food + Decor + Service + East, data = NYC)
summary(NYC_lm)

NYC_lm_m1 <- lm(Price ~ East, data = NYC)

anova(NYC_lm_m1, NYC_lm)

NYC_lm_m2 <- lm(Price ~ Food + Decor, data = NYC)

anova(NYC_lm_m2, NYC_lm)

NYC_lm_m3 <- lm(Price ~ Food + Decor + East, data = NYC)

anova(NYC_lm_m2, NYC_lm_m3, NYC_lm)



