#Working with the Ecommerce Customers csv file from the company. It has Customer info, suchas Email,
#Address, and their color Avatar. Then it also has numerical value columns:

#Avg. Session Length: Average session of in-store style advice sessions.
#Time on App: Average time spent on App in minutes
#Time on Website: Average time spent on Website in minutes
#Length of Membership: How many years the customer has been a member.

#Importing the Dataset

data=read.csv("C:/Users/SANDHYA/Downloads/Datasets/ecommerce-data/e-commerce.csv")
head(data)

#table(data$Avatar)
#unique(data$Avatar)


data$Address=NULL
data$Avatar=NULL

#Pre-checks

names(data)
summary(data)
View(data)   #500rows and 5 columns

library(corrplot)
corrplot(cor(data))

#target variable should be independent. 
plot(data$Time.on.Website,data$Yearly.Amount.Spent)      #Scatter plot - Doesn't follow any pattern, that they 
                                        #are independent of each other. 

#target variable should follow normal distribution
hist(data$Yearly.Amount.Spent) #- Shows it follows normal distribution & also obeys equal variance

#Splitting the data into training and test sets

library(caTools)
set.seed(64)
split=sample.split(data$Yearly.Amount.Spent,SplitRatio = 0.8)


training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

View(test_set)

#Fitting multiple Linear regression to the model
regressor = lm(formula = Yearly.Amount.Spent ~ .,
               data = training_set)

#Predicting the test results
y_pred=predict(regressor,newdata = test_set)

summary(regressor)
summary(y_pred)


#From the summary, p-values are very minute showing it a good model and the best fit. 
#Only the "Time on Website variable" is less than 90% accuracy and its its infuence can be checked by
#Forward Elimination method

#Visualisation
plot(regressor)
#It gives me 4 plots:

#a.Residuals vs Fitted values - which is a scalar plot that shows the variables are independent of each other.
#b.Quantile plot- standard residuals and Quantiles which is a line plot obeying normality.
#c.Fitted values to the root of Standard residuals plot
#d.Standarised residuals vs Leverage plot - this gives the Cooks distance from the plot.
#As the Cooks distance is very low, the "outliers" have null or nearly zero influence on the model.

#Some Important Co-efficients
residuals(regressor)

anova(regressor)
vcov(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Building the optimal model using Forward Elimination
regressor = lm(formula = Yearly.Amount.Spent ~ Avg..Session.Length + Time.on.App +
                 Time.on.Website + Length.of.Membership,data = training_set)
summary(regressor)
#Multiple R-squared:  0.9831,	Adjusted R-squared:  0.983 - Values for multiple linear regression model

regressor = lm(formula = Yearly.Amount.Spent ~ Time.on.App +
                 Time.on.Website + Length.of.Membership,data = training_set)
summary(regressor)
#Multiple R-squared:  0.8786,	Adjusted R-squared:  0.8777 - Decrease in value shows that the 
#variable "Avg session Length" is important and has its influence

regressor = lm(formula = Yearly.Amount.Spent ~ Time.on.Website + Length.of.Membership,
               data = training_set)
summary(regressor)
#Multiple R-squared:   0.67,	Adjusted R-squared:  0.6683 - Decrease in value shows that the 
#variables - Time on website, app and correspondingly the membership has significant influence on the 
#yearly amount spent and the profits earned thereby.

regressor = lm(formula = Yearly.Amount.Spent ~ Length.of.Membership,data = training_set)
summary(regressor)
#Multiple R-squared:  0.6695,	Adjusted R-squared:  0.6686 - Decrease in value shows that the 
#variable is important and has its significant influence

y_pred = predict(regressor, newdata = test_set)
summary(y_pred)

#Hence, as the p-value is minute i.e very small, it is consider as a good model and the most 
#influential variables were found by the "Forward Elimination method"


# Visualising the Training set results
plot(data$Length.of.Membership,data$Yearly.Amount.Spent)

library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$Length.of.Membership, y = training_set$Yearly.Amount.Spent),
             colour = 'red') +
  geom_line(aes(x = training_set$Length.of.Membership, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Yearly amount Spent vs Length of membership (Training set)') +
  xlab('Length of Membership') +
  ylab('Yearly amount spent')


# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$Length.of.Membership, y = test_set$Yearly.Amount.Spent),
             colour = 'red') +
  geom_line(aes(x = training_set$Length.of.Membership, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Yearly amount Spent vs Length of membership (Test set)') +
  xlab('Length of Membership') +
  ylab('Yearly amount spent')
