library(readr)
production <- read_delim("C:/mat374/Production.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

prod_lm <- lm(RunTime ~ RunSize, data=production)

#CI for beta 1 and beta 0

round(confint(prod_lm, level = .95),2)

#using formula
sxx <- sum((production$RunSize-mean(production$RunSize))^2)
se_b1 <- sigma(prod_lm) / sqrt(sxx)

#or use summary(prod_lm)$coef[4] 

coef(prod_lm)[2]-qt(.975, df = prod_lm$df.residual)*se_b1
coef(prod_lm)[2]+qt(.975, df = prod_lm$df.residual)*se_b1

#CI for beta 0
#using formula

se_b0 <- sigma(prod_lm) *  
  sqrt((1/length(production$RunSize)) + (mean(production$RunSize)^2/sxx ))

#or use summary(prod_lm)$coef[3] 

coef(prod_lm)[1]-qt(.975, df = prod_lm$df.residual)*se_b0
coef(prod_lm)[1]+qt(.975, df = prod_lm$df.residual)*se_b0

summary(prod_lm)$coef[3] 

## p-val

test_b1 <- (coef(prod_lm)[2] - .25 ) / summary(prod_lm)$coef[4]
pval_b1 <- pt(test_b1, df = prod_lm$df.residual, lower.tail = F)

GPA_lm <- lm(GPA ~ ACT,data = FreshmanGPA)

#CI for beta 1 and beta 0

round(confint(GPA_lm, level = .95),2)

#using formula
sx <- sum((FreshmanGPA$ACT-mean(FreshmanGPA$ACT))^2)
se_b1 <- sigma(GPA_lm) / sqrt(sx)

summary(GPA_lm)

summary(GPA_lm)$coef[2] 

summary(GPA_lm)

b1_hat <- coef(GPA_lm)[2]
se_b1  <- summary(GPA_lm)$coef[2,2]

# Test statistic
t_b1 <- (b1_hat - 0) / se_b1


test_val <- 0.02   # slope under H0

t_b1_c <- (b1_hat - test_val) / se_b1

pval_c <- pt(t_b1_c, df = GPA_lm$df.residual, lower.tail = FALSE)
pval_c

summary(prod_lm)$coef[3]

#a 
round(confint(GPA_lm,level = .95),2)