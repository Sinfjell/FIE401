data <- read.csv("Data_lab_simple_regression.csv")

str(data)
summary(data)

# modifying date to 
data$datadate <- as.Date(data$datadate, format="%d/%m/%Y")


data$BM <- data$BE / data$MKTV.june.of.current.fiscal.year
