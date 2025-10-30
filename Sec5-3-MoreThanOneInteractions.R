library(readr);library(ggplot2)
NYC <- read_csv("C:/mat374/NYC.csv")
NYC_lm_full_int <- lm(Price ~ Food + Decor + Service + East + Food :East + Decor :East + Service :East, data=NYC)
summary(NYC_lm_full_int)

# Convert East to a factor
NYC$East <- as.factor(NYC$East)
coefs <- coef(NYC_lm_full_int)

# Fix Food, Decor, Service at mean
food_mean <- mean(NYC$Food)
decor_mean <- mean(NYC$Decor)
service_mean <- mean(NYC$Service)

# Intercept & slope for East = 0
int0 <- coefs["(Intercept)"] + coefs["Food"]*food_mean + coefs["Decor"]*decor_mean

slope0 <- coefs["Service"]

# Intercept & slope for East = 1
int1 <- coefs["(Intercept)"] + coefs["East"] + 
  (coefs["Food"] + coefs["Food:East"]) * food_mean +
  (coefs["Decor"] + coefs["Decor:East"]) * decor_mean

slope1 <- coefs["Service"] + coefs["Service:East"]


lines_df <- data.frame(East = c("0", "1"), intercept = c(int0, int1), slope = c(slope0, slope1) )

# Plot
ggplot(NYC, aes(x = Service, y = Price, color = East)) +
  geom_point(alpha = 0.6) +
  geom_abline(data = lines_df, 
              aes(intercept = intercept, slope = slope, color = East, linetype = East), size = .75) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("West", "East")) +
  scale_linetype_manual(values = c("dotted", "dashed"), labels = c("West", "East")) +
  labs(title = "Fitted Interaction Lines: Service × East", color = "Region", linetype = "Region")



