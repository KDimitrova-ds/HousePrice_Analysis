# Practical Data Science Coursework
### House Price Analysis and Prediction ###
#===========================================

rm(list = ls())

#----------------------------------
# 1. Loading Libraries
#----------------------------------

library(readxl)
library(car) 
library(corrplot)
library(ggplot2)
library(dplyr)


#----------------------------------
# 2. Loading Data
#----------------------------------

train_raw <- read_excel("House price.xlsx", sheet = "Train")
test_data <- read_excel("House price.xlsx", sheet = "Test")

#----------------------------------
# 3. Data Preparation and Cleaning
#----------------------------------

# Adjusting Neighborhood Quality (11->10)
train_raw$Neighborhood_Quality <- ifelse(train_raw$Neighborhood_Quality == 11, 10, train_raw$Neighborhood_Quality)

# Removing unrealistic 0-bedroom properties over 884sq ft
house_data <- train_raw[!(train_raw$Num_Bedrooms == 0 & train_raw$Square_Footage > 884),]

# Quick checks
nrow(train_raw)
nrow(house_data)
range(house_data$Neighborhood_Quality)
sum(house_data$Num_Bedrooms == 0)

#-----------------------------------
# 4. Summary of Variables
#-----------------------------------
#Descriptive table 
desc_table <- data.frame(Variable = names(house_data), Mean = sapply(house_data, function(x) if(is.numeric(x)) mean(x) else NA), Median = sapply(house_data, function(x) if(is.numeric(x)) median(x) else NA), SD = sapply(house_data, function(x) if(is.numeric(x)) sd(x) else NA), Min = sapply(house_data, function(x) if(is.numeric(x)) min(x) else NA), Max = sapply(house_data, function(x) if(is.numeric(x)) max(x) else NA))
#Remove ID column
desc_table <- desc_table[desc_table$Variable != "No",]
desc_table

#Quick overview of the dataset
summary(house_data)


#-----------------------------------
# 5. Descriptive Analysis - Graphs
#-----------------------------------

# Figure 1: Boxplot of House Price
# Calculating quartiles
Q1 <- quantile(house_data$House_Price, 0.25)
median_price <- median(house_data$House_Price)
Q3 <- quantile(house_data$House_Price, 0.75)

ggplot(house_data, aes(x = "Housing Dataset", y = House_Price)) + geom_boxplot(fill = "lightblue") + scale_y_continuous(breaks = c(Q1, median_price, Q3), labels = scales::comma) + labs(title = "Boxplot of House Prices", x = "Housing Dataset", y = "House Price (£)")

# Figure 2: Histogram of House price
ggplot(house_data, aes(x = House_Price)) + geom_histogram(fill = "lightblue", color = "white", bins = 30) + scale_x_continuous(labels = scales::comma) + labs(title = "Distribution of House Prices", x = "House Price (£)", y = "Frequency")

# Figure 3: Scatterplot - Price vs Square Footage
ggplot(house_data, aes(x = Square_Footage, y = House_Price)) + geom_point(color = "blue", alpha = 0.6) + scale_y_continuous(labels = scales::comma) + labs(title = "House Price vs Square Footage", x = "Square Footage", y = "House Price (£)")

# Figure 4: Scatterplot - Price vs Year Built
ggplot(house_data, aes(x = Year_Built, y = House_Price)) + geom_point(color = "darkgreen", alpha = 0.6) + scale_y_continuous(labels = scales::comma) + labs(title = "House Price vs Year Built", x = "Year Built", y = "House Price (£)")

# Figure 5: Scatterplot - Price vs Lot Size
ggplot(house_data, aes(x = Lot_Size, y = House_Price)) + geom_point(color = "purple", alpha = 0.6) + scale_y_continuous(labels = scales::comma) + labs(title = "House Price vs Lot Size", x = "Lot Size (acres)", y = "House Price (£)")

# Figure 6: Boxplot - Price vs Bedrooms
ggplot(house_data, aes(x = factor(Num_Bedrooms), y = House_Price)) + geom_boxplot(fill = "red") + scale_y_continuous(labels = scales::comma) + labs(title = "House Price vs Number of Bedrooms", x = "Number of Bedrooms", y = "House Price (£)")
 
# Figure 7: Boxplot - Price vs Bathrooms
ggplot(house_data, aes(x = factor(Num_Bathrooms), y = House_Price)) + geom_boxplot(fill = "orange") + scale_y_continuous(labels = scales::comma) + labs(title = "House Price vs Number of Bathrooms", x = "Number of Bathrooms", y = "House Price (£)")

# Figure 8: Boxplot - Price vs Garage Size
ggplot(house_data, aes(x = factor(Garage_Size), y = House_Price)) + geom_boxplot(fill = "lightgreen") + scale_y_continuous(labels = scales::comma) + labs(title = "House Price vs Garage Size", x = "Garage Size", y = "House Price (£)")

# Figure 9: Pie chart - Garage Size Distribution
garage_counts <- house_data %>%
  count(Garage_Size)

piechart <- ggplot(garage_counts, aes(x = "", y = n, fill = factor(Garage_Size)))
piechart + geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0, direction = 1) + geom_text(aes(label = paste0(n, " (", round(n/sum(n)*100, 1), "%)")), position = position_stack(vjust = 0.5), size = 4, colour = "black") + theme_void() + labs(title = "Distribution of Garage Sizes", fill = "Garage Size")

#------------------------------------
# 6. Correlation Analysis
#------------------------------------

# Correlation matrix
cor_matrix <- cor(house_data[,-1], use = "complete.obs", method = "pearson")
round(cor_matrix, 3)

# Correlation plot
corrplot(cor_matrix, type = "upper", tl.pos = "d")

# Pearson test: Price vs Square Footage
cor_test_sqft <- cor.test(house_data$House_Price, house_data$Square_Footage, method = "pearson")
cor_test_sqft

# Correlation of House Price with all other variables
variables <- c("Square_Footage", "Num_Bedrooms", "Num_Bathrooms", "Year_Built", "Lot_Size", "Garage_Size", "Neighborhood_Quality")
cor_results <- data.frame()

for (v in variables) {test <- cor.test(house_data$House_Price, house_data[[v]], method = "pearson")
cor_results <- rbind(cor_results, data.frame(Variable = v, Correlation = unname(test$estimate), p_value = test$p.value))}

cor_results

#----------------------------------
# 7. Regression Analysis
#----------------------------------

# Simple Regression
simple_model <- lm(House_Price ~ Square_Footage, data = house_data)
summary(simple_model)

# Full multiple regression model
full_model <- lm(House_Price ~ Square_Footage + Year_Built + Lot_Size + Num_Bedrooms + Num_Bathrooms + Garage_Size + Neighborhood_Quality, data = house_data)
summary(full_model)

# Reduced (Final) Model (after removing non-significant variables)
final_model <- lm(House_Price ~ Square_Footage + Year_Built + Lot_Size + Num_Bedrooms + Num_Bathrooms, data = house_data)
summary(final_model)

# Compare models
anova(simple_model, final_model)

# Compare models using AIC
AIC(simple_model, full_model, final_model)
 
#---------------------------------
# 8. Model Diagnostics
#---------------------------------

# Multicollinearity
vif(final_model)

#Standard diagnostic plots
par(mfrow = c(2,2))
plot(final_model)
par(mfrow = c(1,1))
 
# Cook's Distance
plot(cooks.distance(final_model), type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
abline(h = 4/length(cooks.distance(final_model)), lty = 2)

# Leverage (Hat values)
leverage <- hatvalues(final_model)
plot(leverage, type = "h", main = "Leverage Values", ylab = "Leverage")
abline(h = 2 * mean(leverage), lty = 2)

#----------------------------------
# 9. Prediction
#----------------------------------

predicted_prices <- predict(final_model, newdata = test_data)

table7 <- data.frame(Property = 1:nrow(test_data), Square_Footage = test_data$Square_Footage, Year_Built = test_data$Year_Built, Lot_Size = test_data$Lot_Size, Num_Bedrooms = test_data$Num_Bedrooms, Num_Bathrooms = test_data$Num_Bathrooms, Predicted_House_price = round(predicted_prices, 0))

View(table7)

write.csv(table7, "HousePrice_Predictions.csv", row.names = FALSE)
 
 




