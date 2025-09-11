library(readr)
production <- read_delim("Data/Ch2/production.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
prod_lm <- lm(RunTime ~ RunSize, data=production)

#CI for the regression line

predict(prod_lm, newdata = data.frame(RunSize=c(200)) , interval = "confidence", level = 0.95)


#using formula
sxx <- sum((production$RunSize-mean(production$RunSize))^2)
se <- sigma(prod_lm) *  
  sqrt((1/length(production$RunSize)) + ((200-mean(production$RunSize))^2/sxx ))

201.5963-qt(.975, df = prod_lm$df.residual)*se
201.5963+qt(.975, df = prod_lm$df.residual)*se



#PI
predict(prod_lm, newdata = data.frame(RunSize=c(200)) , interval = "prediction", level = 0.95)

#using formula
sxx <- sum((production$RunSize-mean(production$RunSize))^2)
se <- sigma(prod_lm) *  
  sqrt(1+(1/length(production$RunSize)) + ((200-mean(production$RunSize))^2/sxx ))

201.5963-qt(.975, df = prod_lm$df.residual)*se
201.5963+qt(.975, df = prod_lm$df.residual)*se


#GPA
#CI For regression
GPA_lm <- lm(GPA ~ ACT,data = FreshmanGPA)

predict(GPA_lm, newdata = data.frame(ACT=c(30)) , interval = "confidence", level = 0.95)

#PI
predict(GPA_lm, newdata = data.frame(ACT=c(30)) , interval = "prediction", level = 0.95)


