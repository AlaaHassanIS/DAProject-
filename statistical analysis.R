#import data into Rstudio
WholeSCutmr<-read.csv(file.choose(),header = TRUE)
#
View(WholeSCutmr)
str(WholeSCutmr)

# Convert Region to a categorical variable
WholeSCutmr$Region <- as.factor(WholeSCutmr$Region)

# Display summary of the dataset
summary(WholeSCutmr)


# Perform ANOVA to compare Milk consumption across different regions
anova_result <- aov(Milk ~ Region, data = WholeSCutmr)

# Display ANOVA results
summary(anova_result)

kruskal.test(Milk ~ Region, data = WholeSCutmr)

shapiro.test(WholeSCutmr$Milk)   # Normality test
bartlett.test(Milk ~ Region, data = WholeSCutmr)  # Variance homogeneity test


oneway.test(Milk ~ Region, data = WholeSCutmr, var.equal = FALSE)

boxplot(Milk ~ Region, data = WholeSCutmr, main = "Milk Consumption by Region", col = c("lightblue", "lightgreen", "pink"))


# Load necessary library
install.packages("ggplot2")
library(ggplot2)

# Fit linear regression model
lm_model <- lm(Milk ~ Region, data = WholeSCutmr)

# Display summary of regression model
summary(lm_model)




# Create a binary variable for Milk consumption
WholeSCutmr$HighMilk <- ifelse(WholeSCutmr$Milk > median(WholeSCutmr$Milk), 1, 0)

# Convert to a factor for logistic regression
WholeSCutmr$HighMilk <- as.factor(WholeSCutmr$HighMilk)

# Check distribution of High vs. Low
table(WholeSCutmr$HighMilk)


# Fit the logistic regression model
logit_model <- glm(HighMilk ~ Region, data = WholeSCutmr, family = binomial)

# Display model summary
summary(logit_model)

exp(coef(logit_model))  # Get odds ratios

# Make predictions
predicted_probs <- predict(logit_model, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Create a confusion matrix
table(Predicted = predicted_classes, Actual = WholeSCutmr$HighMilk)


accuracy <- (29 + 202) / (29 + 202 + 191 + 18)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))


install.packages("ggplot2", dependencies=TRUE)

library(ggplot2)

ggplot(WholeSCutmr, aes(x = as.factor(Region), y = Milk)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5) +  
  geom_jitter(color = "darkblue", alpha = 0.5, width = 0.2) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 
  labs(title = "Linear Regression: Milk Consumption by Region",
       x = "Region",
       y = "Milk Consumption") +
  theme_minimal()





