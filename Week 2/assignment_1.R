# Assignment related to week 2
# https://nhh.instructure.com/courses/2222/files/352423?wrap=1


# Task 1 ------------------------------------------------------------------
# Open RStudio manually
# Set the working directory
setwd("/Users/sinfjell/Library/CloudStorage/OneDrive-NorgesHandelshøyskole/FIE401/Github")

# Install and load the "plm" package
# installed it in the console with install.packages
library(plm)

# Declare two variables X and Y
X <- rnorm(100)
Y <- rnorm(100)

# Check if variables are in working memory
ls()

# Make a scatterplot
plot(x = X, y = Y)

# Delete variables
rm(X, Y)

# Check if variables are still in memory
ls()


# Task 2 ------------------------------------------------------------------
# Create a vector
my_vector <- 1:100

# Create a dataframe
my_dataframe <- data.frame(digits = my_vector, year = rep(1843, 100), text = "some_text_here")


# Task 3 ------------------------------------------------------------------

# Read the CSV file
factor_returns <- read.csv("factor_returns.csv")

# Save the data
save(factor_returns, file = "factor_returns.RData")

# Write the data as a .txt file
write.table(factor_returns, "factor_returns.txt", sep = "\t")

# Check if files are in the working directory
list.files()


# Task 4 ------------------------------------------------------------------
# Calculate the t-statistic for market returns
t_stat <- (mean(factor_returns$mkt) / sd(factor_returns$mkt)) * 
  sqrt(length(factor_returns$mkt))

# Print the t-statistic
print(t_stat)

# Interpretation:
# The t_stat is higher than 1.96, which means we can reject the null hypothesis
# that the mean market return is zero at the 5% significance level.
# This suggests that the market returns are significantly positive.
# Therefore, investing in the market portfolio could be a good idea.


# Task 5 ------------------------------------------------------------------
# Task 5: Manipulation

# Load the data
factor_returns <- read.csv("factor_returns.csv")

# Convert the date to numeric for easier filtering
factor_returns$date <- as.numeric(factor_returns$date)

# Extract SMB, mkt, and HML for all days starting from 2000
subset_data <- subset(factor_returns, date >= 20000101)

# Compute t-statistics for SMB
t_stat_smb <- (mean(subset_data$smb) / sd(subset_data$smb)) * sqrt(length(subset_data$smb))
print(paste("t-statistic for SMB: ", t_stat_smb))

# Interpretation: If t_stat_smb is greater than 1.96 or less than -1.96, SMB has significantly positive/negative returns
if (abs(t_stat_smb) > 1.96) {
  print("SMB has significantly different returns from zero.")
} else {
  print("SMB does not have significantly different returns from zero.")
}

# Compute t-statistics for mkt
t_stat_mkt <- (mean(subset_data$mkt.rf) / sd(subset_data$mkt.rf)) * sqrt(length(subset_data$mkt.rf))
print(paste("t-statistic for mkt: ", t_stat_mkt))

# Interpretation: If t_stat_mkt is greater than 1.96 or less than -1.96, mkt has significantly positive/negative returns
if (abs(t_stat_mkt) > 1.96) {
  print("Market has significantly different returns from zero.")
} else {
  print("Market does not have significantly different returns from zero.")
}

# Compute t-statistics for HML
t_stat_hml <- (mean(subset_data$hml) / sd(subset_data$hml)) * sqrt(length(subset_data$hml))
print(paste("t-statistic for HML: ", t_stat_hml))

# Interpretation: If t_stat_hml is greater than 1.96 or less than -1.96, HML has significantly positive/negative returns
if (abs(t_stat_hml) > 1.96) {
  print("HML has significantly different returns from zero.")
} else {
  print("HML does not have significantly different returns from zero.")
}

# Summary statistics
summary(subset_data[, c("smb", "mkt.rf", "hml")])

# Histograms
hist(subset_data$smb, main="Histogram of SMB", xlab="SMB")
hist(subset_data$mkt.rf, main="Histogram of Market Returns", xlab="Market Returns")
hist(subset_data$hml, main="Histogram of HML", xlab="HML")

# Task 6 ------------------------------------------------------------------

# Extract the year from the date, add it to factor_returns
factor_returns$year <- substr(factor_returns$date, 1, 4)

# Initialize an empty data frame to store the results
results <- data.frame(Year=integer(), Mean_SMB=numeric(), SD_SMB=numeric())

for (year in 2000:2005) {
  subset_data_year <- subset(factor_returns, factor_returns[["year"]] == as.character(year))
  
  # Save the subset data to a variable
  data_for_year <- subset_data_year
  
  print(paste("Processing year:", year))
  print(paste("Number of rows:", nrow(data_for_year)))
  
  mean_smb <- mean(data_for_year$smb, na.rm = TRUE)
  sd_smb <- sd(data_for_year$smb, na.rm = TRUE)
  
  print(paste("Computed mean_smb for year ", year, ": ", mean_smb))
  print(paste("Computed sd_smb for year ", year, ": ", sd_smb))
  
  # Add the results to the results data frame
  results <- rbind(results, data.frame(Year=year, Mean_SMB=mean_smb, SD_SMB=sd_smb))
}


# Display the results data frame
print(results)

# Unfortunately I am not getting results I want here, need to see fasit


