
library(readr)
dodgers <- read_csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/dodgers.csv")
View(dodgers)

#Structure of Dodgers dataset 

str(dodgers)
#Storing Charater values as factors 
dodgers$day_of_week <- factor(dodgers$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
dodgers$month <- factor(dodgers$month, levels = c("APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT"))

# First 20 rows of the dataset
head(dodgers, 20)
## Box plot for exploring attendance by day of week
plot(dodgers$day_of_week, dodgers$attend / 1000, main = "Dodgers Attendence - Day Of Week", xlab = "Day of Week", ylab = "Attendance in Thousands", col = "red", las = 1)
## Box plot for exploring attendance by Month
plot(dodgers$month, dodgers$attend / 1000, main = "Dodgers Attendence By Month", xlab = "Month", 
     ylab = "Attendance (thousands)", col = "yellow", las = 1)

#
#
#

#Attendance by weather 
library(ggplot2)
ggplot(dodgers, aes(x=temp, y=attend/1000, color=fireworks)) + 
  geom_point() + 
  facet_wrap(day_night~skies) + 
  ggtitle("Dodgers game attendance of occupants in the stadium 
          By Temperature By Time of Game and Skies") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=10)) +
  xlab("Temperature degreeF") +
  ylab("Attendance X 1000")

#
#
#

#Plot of Attendance by opponent teams
ggplot(dodgers, aes(x=attend/1000, y=opponent, color=day_night)) + 
  geom_point() + 
  ggtitle("Dodgers - Opponent Attendance") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=10)) +
  xlab("Attendance X 1000") +
  ylab("Opponent :- Visiting Team")
#
#
#

#
#
#

#Designing Predicition Model 
bobble.model <- {attend ~ month + day_of_week + bobblehead}
# Prepare a Training and Test dataset
#
#
# Reseeding for repeatability
set.seed(12345)
training_test_dodgers <- c(rep(1, trunc((2/3)*nrow(dodgers))), rep(2, trunc((1/3)*nrow(dodgers))))
dodgers$Training_Test <- sample(training_test_dodgers)
dodgers$Training_Test <- factor(dodgers$Training_Test, levels = c(1, 2), labels = c("TRAIN", "TEST"))
dodgers.Train <- subset(dodgers, Training_Test == "TRAIN")
dodgers.Test <- subset(dodgers, Training_Test == "TEST")
# Fit model to training set
train.model.fit <- lm(bobble.model, data = dodgers.Train)
# Predict from Training Set
dodgers.Train$Predict_Attend <- predict(train.model.fit)
# Evaluate The Fitted Model on the Test Set
dodgers.Test$Predict_Attend <- predict(train.model.fit, newdata = dodgers.Test)
#round(cor(DodgersData.Test$attend, DodgersData.Test$Predict_Attend)^2, digits=3)
# compute the proportion of response variance accounted for when predicting Test Data
cat("\n","Proportion of Test Set Variance Accounted for: ", round(cor(dodgers.Test$attend, dodgers.Test$Predict_Attend)^2, digits=3), "\n", sep="")
# Plot of the fitting data
DodgersData.Training_Test <- rbind(dodgers.Train, dodgers.Test)
ggplot(dodgers.Test, aes(x=attend/1000, y=Predict_Attend/1000, color=bobblehead)) + 
  geom_point() + 
  geom_line(data = data.frame(x = c(25,60), y = c(25,60)), aes(x = x, y = y), colour = "red") +
  facet_wrap(~Training_Test) +
  ggtitle("Regression Model Performance : Bobblehead and Attendance") +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=10)) +
  xlab("Actual Attendance (Thousands)") +
  ylab("Predicted Attendance (Thousands)")
# Estimate increase in bobblehead sales
bobblehead.model.fit <- lm(bobble.model, data = dodgers)
print(summary(bobblehead.model.fit))
cat("\n","The effect of Bobblehead Promotion on Attendance at the Dodgers: ", round(bobblehead.model.fit$coefficients[length(bobblehead.model.fit$coefficients)], digits = 0),"\n",sep="")
#Bobblehaed promotions has positive impact on the attendance at the dodgers games with an estimated increase in 10715 fans. 

# Prediction of number of bobbleheads to order

june <- subset(dodgers , month == "JUN" & bobblehead == "NO",
               select=month:attend)
sum(june$attend)

july <- subset(dodgers , month == "JUL" & bobblehead == "NO",
               select=month:attend)
sum(july$attend)

aug <- subset(dodgers , month == "AUG" & bobblehead == "NO",
              select=month:attend)
sum(aug$attend)

total.bobblheads <- sum(june$attend) + sum(july$attend) + sum(aug$attend)
total.bobblheads

