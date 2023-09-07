# Load required packages
library(dplyr)
library(stargazer)

# Read the CSV file into a data frame
data <- read.csv("Data_lab_simple_regression.csv")

# Display the structure and summary of the data to understand its format and variables
str(data)
summary(data)


# Task 2: Construct New Variables ------------------------------------------------

# Convert 'datadate' to Date format
# The original date format is day/month/year
data$datadate <- as.Date(data$datadate, format="%d/%m/%Y")

# Calculate Book-to-Market ratio (BM)
# BM is defined as the book value of equity divided by the 
# market value as of June of the current fiscal year
data$BM <- data$BE / data$MKTV.june.of.current.fiscal.year

# Calculate Return for the Next Fiscal Year
# This is calculated as the percentage change in market capitalization 
# between the end of the subsequent fiscal year and the end of the current fiscal year
data$ReturnNextFiscalYear <- ((data$MKTV.end.of.subsequent.fiscal.year - 
                                 data$MKTV.end.of.current.fiscal.year) / data$MKTV.end.of.current.fiscal.year)

# Task 3: Prepare Variables for Analysis ----------------------------------------

# Create a histogram for log(MKTV) to visualize its distribution
hist(log(data$MKTV.end.of.current.fiscal.year))

# Winsorize the BM variable to limit the effect of outliers
# Calculate the 2.5% and 97.5% quantiles for BM
lower_bound <- quantile(data$BM, 0.025)
upper_bound <- quantile(data$BM, 0.975)

# Replace BM values below the lower bound with the lower bound, and above the upper bound with the upper bound
data$BM[data$BM < lower_bound] <- lower_bound
data$BM[data$BM > upper_bound] <- upper_bound

# Create a histogram for the winsorized BM variable
hist(data$BM)

# Task 4: Estimate Regression Equations ------------------------------------------

# Filter the data to include only observations from the fiscal year 2014
data_2014 <- subset(data, fyear == 2014)

# Estimate the first regression model with ReturnNextFiscalYear as the dependent variable and log(BM) as the independent variable
model1 <- lm(ReturnNextFiscalYear ~ log(BM), data = data_2014)

# Estimate the second regression model with ReturnNextFiscalYear as the dependent variable and log(MKTV) as the independent variable
model2 <- lm(ReturnNextFiscalYear ~ log(MKTV.end.of.current.fiscal.year), data = data_2014)

# Display the summary statistics for both models to understand the fit
summary(model1)
summary(model2)

# Task 5: Combine Models in Output Table -----------------------------------------

# Use the stargazer package to create a well-formatted table that combines both models
stargazer(model1, model2, type = "text", title = "Regression Results", align = TRUE)

# change 1646
