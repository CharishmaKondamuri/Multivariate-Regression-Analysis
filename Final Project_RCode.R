# Read the CSV file into a data frame
data <- read.csv("C:/Users/Owner/Desktop/Durham/Semester 1/Statistics Predictive modelling/MultiRegDataset.csv")
# View the first few rows of the data frame
head(data)
summary(data)
# Get the standard deviation of all variables using sapply() and sd()
sapply(data, sd)

# Load the ggplot2 package
library(ggplot2)

# Create a histogram of the Expenses variable
ggplot(data, aes(x = expenses))+geom_histogram(binwidth = 1000, fill = "blue", color = "black")+labs(title = "Histogram of Expenses", x = "Expenses", y = "Count")
ggplot(data, aes(x = "", y = age))+geom_boxplot(fill = "blue", color = "black")+labs(title = "Boxplot of Age", x = "", y = "Age")
ggplot(data, aes(x = factor(`children`)))+geom_bar(fill = "blue", color = "black")+labs(title = "Bar Chart of No. of Children", x = "No. of Children", y = "Count")
pt(9.87715, df = 1337, lower.tail = FALSE)
table <- table(data$sex, data$smoker)
# Conduct a chi-square test of independence
chisq.test(table)
# Perform a simple linear regression of expenses on smoker
model <- lm(expenses ~ smoker, data = data)
# Print the model summary
summary(model)

# Perform a multiple linear regression of expenses on all variables
model <- lm(expenses ~ age + sex + bmi + children + smoker+ region, data = data)
summary(model)
