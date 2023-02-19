# Part 1 Linear Regression to Predict MPG

# Use library() function to load dplyr package
library(dplyr)

# Import the data.
mpg.data <- read.csv("MechaCar_mpg.csv")

# Perform linear regression.
mpg.linear.model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mpg.data)

# Summarize model
mpg.lm.summary <- summary(mpg.linear.model)
 

# Part 2 Create Visualizations for Trip Analysis

# Import data.
Suspension.data <- read.csv("Suspension_Coil.csv")

# Create summary dataframe for suspension coil PSI
total_summary <- Suspension.data %>%
  summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = "keep")

# Create summary dataframe group by lot.
lot_summary <- suspension.data %>%
  group_by(Manufacturing_Lot) %>%
  summarize(Mean = mean(PSI),
            Median = median(PSI),
            Variance = var(PSI),
            SD = sd(PSI))

# Part 3 T-Tests on Suspension Coils

# Determine if PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.
t.test(Suspension.data$PSI, mu = 1500)

# For each manufacturing lot
t.test(subset(Suspension.data, Manufacturing_Lot == 'Lot1')$PSI, mu = 1500)
t.test(subset(Suspension.data, Manufacturing_Lot == 'Lot2')$PSI, mu = 1500)
t.test(subset(Suspension.data, Manufacturing_Lot == 'Lot3')$PSI, mu = 1500)
