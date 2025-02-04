install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
library(ggplot2)    # For data visualization
library(dplyr)      # For data manipulation
library(corrplot)   # For correlation matrix visualization

# Load the dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv"
data <- read.csv(url)

# Display the first few rows
head(data)

# Check dataset structure
str(data)

# Check for missing values
sum(is.na(data))

# Summary statistics
summary(data)

#Histogram for Fresh Products Spending
hist(data$Fresh, 
     main = "Distribution of Fresh Products", 
     xlab = "Fresh Products", 
     col = "blue", 
     border = "black")
#Boxplot for Fresh Products Spending
boxplot(data$Fresh, 
        main = "Boxplot of Fresh Products", 
        ylab = "Fresh Products", 
        col = "green")
#Scatter Plot (Fresh vs. Milk Spending)
plot(data$Fresh, data$Milk, 
     main = "Scatter Plot of Fresh vs Milk", 
     xlab = "Fresh Products", 
     ylab = "Milk Products", 
     col = "red", 
     pch = 19)
# Install corrplot 
install.packages("corrplot")
#Load the corrplot Library
library(corrplot)
#Compute Correlation Matrix
cor_matrix <- cor(data[, sapply(data, is.numeric)])
print(cor_matrix)
#Visualizing Correlation Matrix
corrplot(cor_matrix, 
         method = "circle", 
         type = "upper", 
         tl.cex = 0.8)
#Identify Outliers in Fresh Spending
outliers <- boxplot.stats(data$Fresh)$out
print(outliers)
# Boxplot Highlighting Outliers
boxplot(data$Fresh, 
        main = "Boxplot with Outliers", 
        ylab = "Fresh Products", 
        col = "orange")

text(x = 1, y = outliers, labels = outliers, col = "red", pos = 3)
# Distribution of Customers by Channel
table(data$Channel)
barplot(table(data$Channel), 
        main = "Customer Distribution by Channel", 
        xlab = "Channel", 
        ylab = "Count", 
        col = c("blue", "red"))
#Distribution of Customers by Region
table(data$Region)
barplot(table(data$Region), 
        main = "Customer Distribution by Region", 
        xlab = "Region", 
        ylab = "Count", 
        col = c("green", "purple", "orange"))


# Statistical Tests

# Normality Test (Shapiro-Wilk) for Fresh Spending
shapiro.test(data$Fresh)

# Independent t-test: Comparing Milk Spending Between Channels
t.test(Milk ~ Channel, data = data)

# ANOVA: Comparing Grocery Spending Across Different Regions
anova_result <- aov(Grocery ~ as.factor(Region), data = data)
summary(anova_result)

# Correlation Analysis
cor_matrix <- cor(data[, 3:8])
print(cor_matrix)



# Linear Regression: Predicting Grocery Spending using Milk and Fresh
lm_model <- lm(Grocery ~ Milk + Fresh, data = data)
summary(lm_model)

# Logistic Regression: Predicting Channel based on Spending Patterns
data$Channel <- as.factor(data$Channel)  # Ensure Channel is a factor
log_model <- glm(Channel ~ Fresh + Milk + Grocery + Frozen + Detergents_Paper + Delicassen, 
                 data = data, family = binomial)
summary(log_model)



# Interpretation of Linear Regression
cat("\nLinear Regression Interpretation:\n")
cat("- Milk spending is a strong predictor of Grocery spending (p < 0.001), suggesting that customers who spend more on Milk tend to also spend more on Grocery items.\n")
cat("- Fresh spending has a small but significant negative impact on Grocery spending (p < 0.01).\n")
cat("- The model explains about 53.78% of the variance in Grocery spending (Adjusted R-squared = 0.5357).\n")

# Logistic Regression: Predicting Channel based on Spending Patterns
data$Channel <- as.factor(data$Channel)  # Ensure Channel is a factor
log_model <- glm(Channel ~ Fresh + Milk + Grocery + Frozen + Detergents_Paper + Delicassen, 
                 data = data, family = binomial)
log_summary <- summary(log_model)
print(log_summary)

# Interpretation of Logistic Regression
cat("\nLogistic Regression Interpretation:\n")
cat("- Detergents_Paper spending is the strongest predictor of Channel assignment (p < 0.001).\n")
cat("- Grocery spending also influences Channel assignment (p < 0.05), while other predictors are not statistically significant.\n")
cat("- The model significantly reduces deviance, indicating a good fit for predicting customer channels.\n")

