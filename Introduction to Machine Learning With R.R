setwd("D:/USER/Desktop/R CLASS/EDA")
library(ggplot2)
library(ggpubr)
library(corrplot)

# Load the Data.
df <- read.csv("Budget.csv", header = TRUE)
head(df, n = 5)    # Viewing the first 5.


# Understanding the Data in df.
summary(df)


# Use Scatter Plots To Visualise The Relationship.
Spend_S <- ggplot(df, aes(Spend, y = Sales)) + geom_point() +
  ggtitle('Spending vs Sales')
Spend_S
## From the scatter plot below, it can be seen that most of the data points fall exactly on straight line. 


# Using BoxPlot To Check For Outliers.
sales <- ggplot(df, aes(x = Sales))
bx_sales <- sales + geom_boxplot(color = "#E46726")

spend <- ggplot(df, aes(x = Spend))
bx_spend <- spend + geom_boxplot(color = '#2E9FDF')

Sal_Spend <- ggplot(df, aes(x= Spend, y = Sales))
bx_SalSp <- Sal_Spend + geom_boxplot(color = '#999999')


figure <- ggarrange(bx_sales, bx_spend, bx_SalSp,
                    labels = c("Sales", "Expenditure", "Spend vs Sales"),
                    ncol = 2, nrow = 2)
figure


# Using Density Plot To Check If Response Variable Is Close To Normal.
Density_Sales <- density(df$Sales)
plot(Density_Sales, main="Density of Sales")
polygon(Density_Sales, col="red", border="blue")


# Checkthe Correlation Analysis.
## cor() computes the correlation coefficient
cor(df$Sales,df$Spend)
## cor.test() test for association/correlation between paired samples. It returns both the correlation coefficient and the significance level(or p-value) of the correlation.
cor.test(df$Sales,df$Spend)
## Correlation Matrix
M <- cor(df)
corrplot(M, method = "number")


# Build the Linear Regression Model.
## We build a model to predict sales on the basis of spending pattern.
model <- lm(Sales~Spend, data = df)
model
## Our regression equation is: y = 1383.47 + 10.627*x, 
## that is Sales = 1383.47 + 10.627*Spend.


# Here we calculate model summary as an object.
mod_summary <- summary(model)
mod_summary
estimate <- mod_summary$coefficients['Spend', 'Estimate']
std.error <- mod_summary$coefficients['Spend', 'Std. Error']
## t-statistics
t_val = estimate/std.error
t_val
# Using p-value Check For Statistical Significance.
p_value <- 2*pt(-abs(t_val), df = length(df$Sales)-1)
p_value
## Here since the p-value is 1.329412e-15 we can say that the 
## p-value is significantly less than 0.05.


# Capture the summary of the linear model
summary(model)
## Here we can also see that the significance stars at the end of the Spend variable row is 3 (***).
## Thus we can say that the linear model is statistically significant.


# Perform the Linear Diagnostics for the given data set
#### In R, you can easily augment your data to add fitted values 
#### and residuals by using the function augment() [broom package]. 
library(broom)
model.diag.metrics <- augment(model)
head(model.diag.metrics)

## Among the table columns, there are:
### * Spend: the amount spend for marketing.
### * Sales: the observed sale values.
### * .fitted: the fitted sale values.
### * .resid: the residual errors.

#### Plotting the residuals error (in red color) 
#### between observed values and the fitted regression line. 
ggplot(model.diag.metrics, aes(Spend, Sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Spend, yend = .fitted), color = "red", size = 0.3)
#### Each vertical red segments represents the residual error 
#### between an observed sale value and the corresponding 
#### predicted (i.e. fitted) value.

par(mar=c(2,2,2,2), mfrow = c(2, 2))
plot(model)



# Create the training and test data (70:30)
set.seed(42) ## It ensures that you get the same result if you start with that same seed each time you run the same process. 

trainingRow <- sample(1:nrow(df), 0.7*nrow(df))
train <- df[trainingRow,]
train
test <- df[-trainingRow,]
test

# Fit the model on training data and predict sales on test data
Fit_Model <- lm(Sales~Spend, data = train)  #Building Model
pred_Model <- predict(Fit_Model, test)  #Predict Model
pred_Model
summary(Fit_Model)
