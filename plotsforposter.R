# Load required libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(caret)

# Load the cleaned dataset
superstore <- read.csv("C:/Users/rithv/Downloads/cleaned_superstore_dataset.csv")

# Convert date columns to Date format
superstore$Order.Date <- as.Date(superstore$Order.Date, format = "%Y-%m-%d")
superstore$Ship.Date <- as.Date(superstore$Ship.Date, format = "%Y-%m-%d")

# Set consistent color palette
colors <- brewer.pal(9, "Set2")

# 1. Profitability by Region and Segment
region_segment <- superstore %>%
  group_by(Region, Segment) %>%
  summarise(TotalProfit = sum(Profit), .groups = "drop")

ggplot(region_segment, aes(x = Region, y = TotalProfit, fill = Segment)) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Profitability by Region and Segment",
    x = "Region",
    y = "Total Profit",
    fill = "Segment"
  ) +
  theme_minimal()

# 2. Category Performance: Profit vs. Discount

discount_profit_summary <- superstore %>%
  group_by(Category, DiscountBin) %>%
  summarise(AverageProfit = mean(Profit, na.rm = TRUE), .groups = "drop")


discount_profit_summary <- superstore %>%
  group_by(Category, DiscountBin) %>%
  summarise(AverageProfit = mean(Profit, na.rm = TRUE), .groups = "drop")

# Faceted Bar Chart: Average Profit by Discount Bin
# Rename Discount Bins with Percentage Labels
superstore <- superstore %>%
  mutate(DiscountBin = recode(DiscountBin,
                              "[0,0.1]" = "0-10%",
                              "(0.1,0.2]" = "10-20%",
                              "(0.2,0.3]" = "20-30%",
                              "(0.3,0.4]" = "30-40%",
                              "(0.4,0.5]" = "40-50%",
                              "(0.5,0.6]" = "50-60%",
                              "(0.6,0.7]" = "60-70%",
                              "(0.7,0.8]" = "70-80%"))

# Faceted Bar Chart: Average Profit by Renamed Discount Bins
ggplot(discount_profit_summary, aes(x = DiscountBin, y = AverageProfit, fill = Category)) +
  geom_col(color = "black", alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ Category, scales = "free") +
  labs(
    title = "Average Profit by Discount Levels",
    x = "Discount Level",
    y = "Average Profit",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    legend.position = "none"
  )



# 3. Sales by Sub-Category
# Create a lollipop chart for Total Sales by Sub-Category
# Aggregate sales by subcategory and broader category
subcategory_grouped <- superstore %>%
  group_by(Category, Sub.Category) %>%
  summarise(TotalSales = sum(Sales), .groups = "drop")

# Lollipop chart with grouping by broader categories
ggplot(subcategory_grouped, aes(x = reorder(Sub.Category, TotalSales), y = TotalSales, color = Category)) +
  geom_segment(aes(x = Sub.Category, xend = Sub.Category, y = 0, yend = TotalSales), size = 1) +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "Total Sales by Sub-Category",
    subtitle = "Grouped by Broader Categories",
    x = "Sub-Category",
    y = "Total Sales",
    color = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )
##

# Aggregate sales by subcategory and broader category
subcategory_grouped <- superstore %>%
  group_by(Category, Sub.Category) %>%
  summarise(TotalSales = sum(Sales), .groups = "drop") %>%
  arrange(Category, desc(TotalSales))  # Sort by Category and Total Sales

# Add a sorting factor to maintain order
subcategory_grouped <- subcategory_grouped %>%
  mutate(Sub.Category = factor(Sub.Category, levels = unique(Sub.Category)))
options(scipen = 999)

# Lollipop chart grouped and sorted by Category
ggplot(subcategory_grouped, aes(x = Sub.Category, y = TotalSales, color = Category)) +
  geom_segment(aes(x = Sub.Category, xend = Sub.Category, y = 0, yend = TotalSales), size = 1) +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "Total Sales by Sub-Category",
    subtitle = "Grouped and Sorted by Broader Categories",
    x = "Sub-Category",
    y = "Total Sales",
    color = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    panel.grid.major.y = element_blank()  # Remove grid lines for cleaner grouping
  )



##
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Define seasonal_sales data
seasonal_sales <- superstore %>%
  mutate(Month = format(Order.Date, "%m")) %>%  # Extract month from Order.Date
  group_by(Month) %>%  # Group by month
  summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop")  # Summarize total sales

# Convert Month to a factor with month names for proper ordering
seasonal_sales <- seasonal_sales %>%
  mutate(MonthNumeric = as.numeric(Month),  # Numeric for plotting
         Month = factor(as.numeric(Month), levels = 1:12, labels = month.name),  # Factor for labels
         TotalSales = round(TotalSales, 2))  # Round TotalSales to 2 decimals

# Identify peak and valley points
peak_point <- seasonal_sales[which.max(seasonal_sales$TotalSales), ]
valley_point <- seasonal_sales[which.min(seasonal_sales$TotalSales), ]
average_sales <- mean(seasonal_sales$TotalSales)  # Calculate average sales

# Enhanced Seasonal Sales Trends Plot
ggplot(seasonal_sales, aes(x = MonthNumeric, y = TotalSales)) +
  # Add sales range band for average context
  annotate("rect", xmin = 0.5, xmax = 12.5, ymin = average_sales - 25000, ymax = average_sales + 25000, 
           alpha = 0.2, fill = brewer.pal(8, "Set2")[6]) +
  # Smoothed trend line
  geom_smooth(se = FALSE, color = brewer.pal(8, "Set2")[4], size = 2, method = "loess") +
  # Data points
  geom_point(size = 5, color = brewer.pal(8, "Set2")[3]) +
  # Highlight peak and valley points
  geom_point(data = peak_point, aes(x = MonthNumeric, y = TotalSales), size = 6, shape = 8, color = "darkgreen") +
  geom_point(data = valley_point, aes(x = MonthNumeric, y = TotalSales), size = 6, shape = 8, color = "red") +
  # Add annotations for peak and valley
  geom_text(data = peak_point, aes(x = MonthNumeric, y = TotalSales + 20000, label = paste("Peak:", Month, "\n$", TotalSales)), 
            color = "darkgreen", size = 4, fontface = "bold") +
  geom_text(data = valley_point, aes(x = MonthNumeric, y = TotalSales - 20000, label = paste("Valley:", Month, "\n$", TotalSales)), 
            color = "red", size = 4, fontface = "bold") +
  # Annotate total sales for each month
  geom_text(aes(label = paste("$", TotalSales)), size = 3, vjust = -1, color = brewer.pal(8, "Set2")[2]) +
  # Add title and labels
  labs(
    title = "Enhanced Seasonal Sales Trends",
    x = "Month",
    y = "Total Sales ($)"
  ) +
  # Custom X-axis with month names
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  # Minimal theme with enhanced visuals
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none",  # Remove unnecessary legend
    panel.grid.major.y = element_blank(),  # Minimal gridlines
    panel.grid.minor = element_blank()
  )



# 5. Predictive Model: Profitability



##

# Load necessary libraries
library(randomForest)
library(caret)
library(doParallel)

# Set up parallel backend
num_cores <- parallel::detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Prepare model data
model_data <- superstore %>%
  select(Sales, Discount, Quantity, Shipping.Cost, Profit) %>%
  na.omit()

# Train-test split
set.seed(42)
train_index <- createDataPartition(model_data$Profit, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Hyperparameter tuning with parallel processing
control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)  # Enable parallel processing

# Grid of hyperparameters
tune_grid <- expand.grid(
  mtry = c(1, 2, 3)  # Number of features at each split
)

# Train Random Forest with tuning
tuned_rf <- train(
  Profit ~ Sales + Discount + Quantity + Shipping.Cost,
  data = train_data,
  method = "rf",
  tuneGrid = tune_grid,
  trControl = control,
  ntree = 500  # Fixed number of trees
)

# Stop parallel backend
stopCluster(cl)

# Best parameters
cat("Best Parameters:\n")
print(tuned_rf$bestTune)

# Train final Random Forest model with optimized parameters
optimized_rf_model <- randomForest(
  Profit ~ Sales + Discount + Quantity + Shipping.Cost,
  data = train_data,
  ntree = 500,  # Fixed number of trees
  mtry = tuned_rf$bestTune$mtry,
  importance = TRUE
)

# Predictions on test data
test_data$Predicted_Profit <- predict(optimized_rf_model, newdata = test_data)

# Calculate performance metrics
mae <- mean(abs(test_data$Profit - test_data$Predicted_Profit))
mse <- mean((test_data$Profit - test_data$Predicted_Profit)^2)
rmse <- sqrt(mse)
r_squared <- 1 - (sum((test_data$Profit - test_data$Predicted_Profit)^2) / 
                    sum((test_data$Profit - mean(test_data$Profit))^2))

# Print metrics
cat("Optimized Random Forest Model Performance Metrics:\n")
cat("Mean Absolute Error (MAE):", round(mae, 2), "\n")
cat("Mean Squared Error (MSE):", round(mse, 2), "\n")
cat("Root Mean Squared Error (RMSE):", round(rmse, 2), "\n")
cat("R-squared (R²):", round(r_squared, 2), "\n")



##

library(tidyr)
# Categorize Sales into Ranges
test_data <- test_data %>%
  mutate(Sales_Range = cut(Sales, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High")))

# Aggregate Data
profit_summary <- test_data %>%
  group_by(Sales_Range) %>%
  summarise(
    Actual = mean(Profit, na.rm = TRUE),
    Predicted = mean(Predicted_Profit, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Actual, Predicted), names_to = "Type", values_to = "Profit")

# Plot
ggplot(profit_summary, aes(x = Sales_Range, y = Profit, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Comparison of Actual vs Predicted Profits",
    subtitle = "Grouped by Sales Range",
    x = "Sales Range",
    y = "Average Profit ($)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






