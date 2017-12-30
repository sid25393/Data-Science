
library(readr)
bank_marketing <- read_csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/bank_marketing.csv")
View(bank_marketing)

#Exploratory Analysis

str(bank_marketing)
head(bank_marketing,10)
sapply(bank_marketing, class)
hist(bank_marketing$age, xlab = "ages", ylab = "frequency", col = "yellow", breaks = "Sturges", main = paste("Histogram of ages - Bank Marketing Dataset"))

#Decision Tree 

library(rpart)
decision_tree <- rpart(y ~ age + job + marital + education + default + housing +
                         loan + contact + month + day_of_week + duration + campaign + pdays + previous +
                         poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, method="class", data=bank_marketing)
summary(decision_tree)

#
#
# Plot the breakdown of subscribers to non-susbcribers
#
#

library(ggplot2)
ggplot(data=bank_marketing, aes(x=y)) + 
  geom_bar(alpha = 0.5, fill = "Green") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Subscribed?", y = "Count")

#
#
#Distribution of Last Contact Duration
#
#

ggplot(bank_marketing, aes(x=duration/60))+ geom_histogram (alpha = 0.5, fill = "blue", breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=12)) +
  labs(x= "Last Contact Duration, mins", y = "Count")

#
# Distribution of previous campaign outcome

ggplot(data=bank_marketing, aes(x=poutcome)) + 
  geom_bar(alpha = 0.5, fill = "Red") + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Previous Campaign Outcome", y = "Count")

#
#
# Subscription status by previous outcome
#
#

ggplot(data=bank_marketing, aes(x=y, fill = poutcome)) + 
  geom_bar(alpha = 0.5) + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Subscribed?", y = "Count")

#
# Subscription status by education
#
#

ggplot(data=bank_marketing, aes(x=y, fill = education)) + 
  geom_bar(alpha = 0.5) + theme_bw() +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(face="bold", vjust=1, size=14),
        axis.text.y  = element_text(size=14)) + 
  labs(x= "Subscribed?", y = "Count")

#
#
#
# Display the performance results
#
#

printcp(decision_tree)

#
#
# Visualize cross-validation results
#
#

plotcp(decision_tree)

#
#
# Output of the decision tree
#
#

plot(decision_tree, uniform=TRUE, main="Classification Tree")
text(decision_tree, use.n=TRUE, all=TRUE, cex=.8)

#
#
# Create plot of tree using the rpart.plot package for a better look
#
#

library(rpart.plot)
rpart.plot(decision_tree)

#
#
#create a training and testing set, 
#which can be used to train the dataset and 
#test those values with the test set
#
#

library(base)
size <- nrow(bank_marketing) * 0.7
validation_index <- sample(1:nrow(bank_marketing), size = size)

validation <- bank_marketing[-validation_index,]
bank <- bank_marketing[validation_index,]

#
#
#

bank.rpart <- rpart(y ~ ., data = bank_marketing)
rpart.plot(bank.rpart)

#
#
#
#Predicitions 
#
#
#

predictions <- predict(bank.rpart, bank_marketing, type = "class")
confusion.matrix <- prop.table(table(predictions, bank_marketing$y))
confusion.matrix

#
#
#
#Accuracy of the model
#
#
#

accuracy_model <- confusion.matrix[1,1] + confusion.matrix[2,2] 
accuracy_model

#
#
#
# ACCURACY = 0.91688 or 91.688%