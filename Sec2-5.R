library(readr)
mccoo <- read_delim("C:/mat374/mccoo.txt", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
prod_lm <- lm(RunTime ~ RunSize, data=production)

anova(prod_lm)

summary(prod_lm)

# McCoo Dataset
mccoo <- read_table("C:/mat374/mccoo.txt")

mccoo_lm <- lm(Score~McCoo,data = mccoo)

library(tidyverse)
ggplot(data=mccoo,aes(x=McCoo,y=Score)) +
  geom_point() +
  geom_smooth(method ="lm", se =F)

summary(mccoo_lm)$r.squared

# Keep the points with score > 3
mccoo_r <- mccoo[mccoo$Score> 3, ]
#mccoo_r <- mccoo %>% filter(Score > 3)

mccoo_r <- mccoo[mccoo$Score>=5, ]


mccoo_lm_r <- lm(Score~McCoo,data = mccoo_r)

library(tidyverse)
ggplot(data=mccoo_r,aes(x=McCoo,y=Score)) +
  geom_point() +
  geom_smooth(method ="lm", se =F)

summary(mccoo_lm_r)$r.squared


