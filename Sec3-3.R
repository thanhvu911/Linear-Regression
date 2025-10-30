library(readr);library(ggplot2);library(ggfortify);library(MASS)

#EX1
Performance <- read_csv("C:/mat374/Performance.csv")
ggplot(data = Performance, aes(x=Days, y=Score))+
  geom_point() +
  geom_smooth(se=F) 

ggplot(data = Performance, aes(x=sqrt(Days), y=Score))+
  geom_point() +
  geom_smooth(se=F) 

Performance_lm <- lm(Score~Days, data=Performance)
summary(Performance_lm)
autoplot(Performance_lm, label.repel = TRUE, label.size = 3)

Performance_lm_sqrt <- lm(Score~I(sqrt(Days)), data=Performance)
summary(Performance_lm_sqrt)
autoplot(Performance_lm_sqrt, label.repel = TRUE, label.size = 3)

pt_res_lm <- ggplot(Performance_lm, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0) +
  xlab("Fitted Values") + ylab("Residuals") + geom_smooth(se=F) +
  coord_cartesian(ylim = c(-15, 15))

pt_res_lm_sqrt <- ggplot(Performance_lm_sqrt, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0) +
  xlab("Fitted Values") + ylab("Residuals") + geom_smooth(se=F) +
  coord_cartesian(ylim = c(-15, 15))


library(gridExtra)
grid.arrange(pt_res_lm, pt_res_lm_sqrt , ncol = 2)

predict(Performance_lm_sqrt, newdata = data.frame(Days=c(2)),interval="confidence")


#EX2
library(readr)
WordRecall <- read_csv("C:/mat374/WordRecall.csv")
ggplot(data = WordRecall, aes(x=Time, y=Prop))+
  geom_point() +
  geom_smooth(se=F) 

ggplot(data = WordRecall, aes(x=log(Time), y=Prop))+
  geom_point() +
  geom_smooth(se=F) 

WordRecall_lm_sqrt <- lm(Prop~I(log(Time)), data=WordRecall)
summary(WordRecall_lm_sqrt)
autoplot(WordRecall_lm_sqrt, label.repel = TRUE, label.size = 3)

pt_res_lm_sqrt <- ggplot(WordRecall_lm_sqrt, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0) +
  xlab("Fitted Values") + ylab("Residuals") + geom_smooth(se=F) +
  coord_cartesian(ylim = c(-15, 15))

predict(WordRecall_lm_sqrt, newdata = data.frame(Time=c(1000)),interval="confidence")


#EX3
PlasmaLevel <- read_csv("C:/mat374/PlasmaLevel.csv")
library(MASS)
boxcox(lm(Plasma~Age, data=PlasmaLevel))
autopilot(PlasmaLevel,repel = T)

#Find lambda
bc <- boxcox(lm(Plasma~Age, data=PlasmaLevel))
lambda <- bc$x[which.max(bc$y)]
lambda

bc_caret <- caret::BoxCoxTrans(PlasmaLevel$Plasma)

plasma_lm_bc <- lm(I(Plasma^lambda)~Age, data=PlasmaLevel)

autopilot(plasma_lm_bc,label.repel = T, label.size = 2)

predict(plasma_lm_bc, newdata = data.frame(Age=c(2.34)),interval = "confidence")^(-.5)
summary(plasma_lm_bc)

#EX4
library(readr)
Mammal <- read_csv("C:/mat374/Mammal.csv")
Mammal_lm <- lm(Gestation~Birthwgt)

bc <- boxcox(lm(Gestation~log(Gestation), data=Mammals))
lambda <- bc$x[which.max(bc$y)]


Mammal_lm_log <- lm(I(log(Gestation))~Birthwgt, data =Mammal)

exp(predict(Mammal_lm_log, newdata = data.frame(Birthwgt = c(50), interval = "confidence")))

#EX5
library(readr)
ShortLeaf <- read_csv("C:/mat374/ShortLeaf.csv")

ShortLeaf_lm <- lm(Volume ~ Diameter, data = ShortLeaf)

autoplot(ShortLeaf_lm, label.repel = T, label.size =2, size =.5)

ShortLeaf_lm_log <- lm(I(log(Volume)) ~ I(log(Diameter)), data = ShortLeaf)

autoplot(ShortLeaf_lm_log, label.repel = T, label.size =2, size =.5)

summary(ShortLeaf_lm_log)

predict(ShortLeaf_lm_log, newdata = data.frame(Diameter=c(10)))

exp(predict(ShortLeaf_lm_log, newdata = data.frame(Diameter=c(10,20))))

2^2.5644

#EX6
library(readr)
advertising <- read_csv("C:/mat374/advertising.csv")

advertising_lm <- lm(Sales ~ TV, data = advertising)

summary(advertising_lm)

predict(advertising_lm, newdata = data.frame(TV=c(5000,10000)))

predict(advertising_lm, newdata = data.frame(TV=c(10000))) - predict(advertising_lm, newdata = data.frame(TV=c(5000)))

predict(advertising_lm, newdata = data.frame(TV=c(100)),interval="prediction")

autoplot(advertising_lm, label.repel = T, label.size =2, size =.5)

advertising_lm_log <- lm(I(log(Sales)) ~ I(log(TV)), data = advertising)

predict(advertising_lm_log, newdata = data.frame(TV=c(5000,10000)))

2^(summary(advertising_lm_log)$coefficient[2])
