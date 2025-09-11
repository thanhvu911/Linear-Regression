library(readr)
birthsmokers <- read_delim("C:/mat374/birthsmokers.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)
library(tidyverse)

ggplot(birthsmokers, aes(x=Smoke, y=Wgt)) +
  geom_point() +
  geom_smooth(method="lm", se=F)

birthsmokers %>%
  mutate(Smoke = as.factor(Smoke)) %>%
  ggplot(aes(x=Smoke, y=Wgt, fill=Smoke)) +
  geom_boxplot() + 
  theme_light() +
  theme(legend.position = "none") + 
  scale_x_discrete(labels = c("0" = "Non-smokers", "1" = "Smokers")) +
  labs(y="Baby's Birth Weight", x="")


birth_lm <- lm(Wgt ~ Smoke, data=birthsmokers)

summary(birth_lm)

confint(birth_lm)

predict(birth_lm, newdata=data.frame(Smoke=c(1)), interval = "confidence", level=.95)

predict(birth_lm, newdata=data.frame(Smoke=c(1)), interval = "prediction", level=.95)

library(readr)
changeovertimes <- read_csv("C:/mat374/changeovertimes.csv")
View(changeovertimes)

changeovertimes %>%
  mutate(Method = as.factor(Method)) %>%
  ggplot(aes(x=Method, y=Changeover, fill=Method)) +
  geom_boxplot() + 
  theme_light() +
  theme(legend.position = "none") + 
  scale_x_discrete(labels = c("0" = "Existing", "1" = "New")) +
  labs(y="Change-over time", x="")


change_lm <- lm(Changeover~Method, data = changeovertimes)

summary(change_lm)

confint(change_lm)

predict(change_lm, newdata=data.frame(Method=c("New")), interval = "prediction", level=.95)


