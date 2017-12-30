library(readr)
soup <- read_csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/Progresso_Soup.csv")
View(soup)

# We need to create the dummy variables first for the winter months. This requires converting the months column
# from 1 to 12 assigning to months from Jan to Dec respectively. 

View(soup$Month)
soup$Month <- as.factor(soup$Month)
months <- factor(soup$Month, levels = 1:12, labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "Novemeber", "December"))
soup$Month <- months
summary(soup$Month)

# We need to divide the months into winter months and summer months or as mentioned in the question summer months = non winter months 
# We need to define winter and non winter months using as.logical to write a code using dummy variables

soup$highincome <- as.logical(soup$High_Income)
soup$lowincome <- as.logical(soup$Low_Income)
soup$nonwintermonths <- as.logical(0)
soup$wintermonths <- as.logical(0)
for (i in 1:nrow(soup)) {
  if (soup$Month[i] == "March" || 
      soup$Month[i] == "April"|| 
      soup$Month[i] == "May" || 
      soup$Month[i] == "June")
    soup$nonwintermonths[i] <- as.logical(1)
  else if (soup$Month[i] == "October" || 
           soup$Month[i] == "November" || 
           soup$Month[i] == "December" || 
           soup$Month[i] == "January" || 
           soup$Month[i] == "February")
    soup$wintermonths[i] <- as.logical(1)
}
summary(soup$nonwintermonths)
summary(soup$wintermonths)
summary(soup$highincome)
summary(soup$lowincome)

#
#
#
# Similarly, performing the above procedure for the region category as the market share of the soup in winter and 
# non - winter months needs to be known according to the location. 
# Naming all regions as follows: 

#For non-winter months: 

soup$east.nonwinter <- as.logical(0)
soup$middle.nonwinter <- as.logical(0)
soup$west.nonwinter <- as.logical(0)
soup$south.nonwinter <- as.logical(0)
#For Winter months: 

soup$east.winter <- as.logical(0)
soup$middle.winter <- as.logical(0)
soup$west.winter <- as.logical(0)
soup$south.winter <-as.logical(0)
#Using for loop: 

for (i in 1:nrow(soup)) {
  if (soup$nonwintermonths[i] == TRUE & soup$Region[i] =="East" )
    soup$east.nonwinter[i] <- TRUE
  else if (soup$nonwintermonths[i] == TRUE & soup$Region[i] == "Midwest")
    soup$middle.nonwinter[i] <- TRUE 
  else if (soup$nonwintermonths[i] == TRUE & soup$Region[i] == "South")
    soup$south.nonwinter[i] <- TRUE
  else if (soup$nonwintermonths[i] == TRUE & soup$Region[i] == "West")
    soup$west.nonwinter[i] <- TRUE
}
summary(soup$west.nonwinter)
summary(soup$east.nonwinter)
summary(soup$south.nonwinter)
summary(soup$middle.nonwinter)
for (i in 1:nrow(soup)) {
  if (soup$wintermonths[i] == TRUE & soup$Region[i] =="East" )
    soup$east.winter[i] <- TRUE
  else if (soup$wintermonths[i] == TRUE & soup$Region[i] == "Midwest")
    soup$middle.winter[i] <- TRUE 
  else if (soup$wintermonths[i] == TRUE & soup$Region[i] == "South")
    soup$south.winter[i] <- TRUE
  else if (soup$wintermonths[i] == TRUE & soup$Region[i] == "West")
    soup$west.winter[i] <- TRUE
}
summary(soup$west.winter)
summary(soup$east.winter)
summary(soup$south.winter)
summary(soup$middle.winter)

#
#
#
# in order to calculater market share we need to know the individual sales in winter and non winter nonths

Sales_nonwinter <- 0
Sales_winter <- 0
for(i in 1:nrow(soup)){
  if (soup$nonwintermonths[i] == "TRUE")
    Sales_nonwinter <- Sales_nonwinter + sum(soup$Sales.Progresso[i])
  else if (soup$wintermonths[i] == "TRUE")
    Sales_winter <- Sales_winter + sum(soup$Sales.Progresso[i])
} 
Sales_nonwinter
Sales_winter
Sales_nonwinter <- 28716829
Sales_winter <- 54237361
max(soup$Sales.Progresso)
min(soup$Sales.Progresso)
total_soup_sales <- sum(soup$Sales.Progresso)
total_soup_sales

#
#
#

Sales_nonwinter
Sales_winter
marketshare_nonwinter <- (Sales_nonwinter*100)/total_soup_sales
marketshare_nonwinter
marketshare_winter <- (Sales_winter*100)/total_soup_sales
marketshare_winter

# Market share is more in winter months than non winter months.

linear_model <- lm(soup$Sales.Progresso ~  soup$Price.Campbell + soup$Price.PL + soup$Price.Progresso + soup$Region + soup$wintermonths + soup$Low_Income + soup$High_Income)
linear_model
summary(linear_model)

#
#
#

data.training <- c(rep(1, trunc((4/5)*nrow(soup))), rep(2, trunc((1/5)*nrow(soup))))
soup$data.training <- sample(data.training)
soup$data.training <- factor(soup$data.training, levels = c(1,2), labels = c ("TRAIN", "TEST"))
datatrain_soup <- subset(soup, data.training == "TRAIN")
datatest_soup <- subset(soup, data.training == "TEST")
train.model.fit <- lm(linear_model, data = soup)

# Fitting the model

Predict_Sales_Train <- predict(train.model.fit)
Predict_Sales_Test <- predict(train.model.fit, newdata = datatest_soup)

## the accuracy of the model is 0.94 or 94%.

# All values are significantly significant. 
# the sales of the progresso soup varies with and is affected by the competitor sales. 