#Q1.Load the dataset day.csv  Download day.csv into memory.
day <- read.csv("Downloads/day.csv")
View(day)

#Q2.Perform the following data preparations using control structures:
  #a. Convert numerical season (1,2,3, 4) to characters (springer, summer, fall and winter)
day$season <- as.character(day$season)
day$season[day$season == "1"] <- "springer"
day$season[day$season == "2"] <- "summer"
day$season[day$season == "3"] <- "fall"
day$season[day$season == "4"] <- "winter"
print(unique(day$season))
  #b. Convert numerical weathersit (1,2,3,4) to characters (Good, Mist, Bad, Severe)
day$weathersit <- as.character(day$weathersit)
day$weathersit[day$weathersit == "1"] <- "Good"
day$weathersit[day$weathersit == "2"] <- "Mist"
day$weathersit[day$weathersit == "3"] <- "Bad"
day$weathersit[day$weathersit == "4"] <- "Severe"
print(unique(day$weathersit))

#Q3.Consider the following predictors, season, holiday, workingday, weathersit, atemp, hum, windspeed, casual and List all categorical variables from this list and convert them to factors.

#ANS:In this case, season, holiday, workingday, and weathersit are categorical variables. The rest (atemp, hum, windspeed, casual) are continuous/numerical variables.
day$season <- as.factor(day$season)
day$holiday <- as.factor(day$holiday)
day$workingday <- as.factor(day$workingday)
day$weathersit <- as.factor(day$weathersit)
str(day)
#Q4.Calculate the minimum, maximum, mean, median, standard deviation and three quartiles (25th, 50th and 75th percentiles) of cnt.
min(day$cnt)
max(day$cnt)
mean(day$cnt)
median(day$cnt)
sd(day$cnt)
quantile(day$cnt,probs = c(0.25, 0.5, 0.75))

#Q5.Calculate the minimum, maximum, mean, median, standard deviation and three quartiles (25th, 50th and 75th percentiles) of registered.

min(day$registered)
max(day$registered)
mean(day$registered)
median(day$registered)
sd(day$registered)
quantile(day$registered,probs = c(0.25, 0.5, 0.75))

#Q6.Calculate the correlation coefficient of the two variables: registered and cnt. Do they have a strong relationship?
cor(day$registered,day$cnt)
#ANS: Both does have strong relationship between as the correlation coefficient is closer to one ( 0.9455169)

#Q7.Calculate the frequency table of season? What’s the mode of season variable?

#To check whether a variable is factor or not
is.factor(day$season)
#To convert variable to factor
day$season <- as.factor(day$season)
#To check whether a variable is factor or not
is.factor(day$season)
#To find frequency table
table(day$season)
#To find the mode
names(sort(table(day$season), decreasing = TRUE))[1]

#Q8.Calculate the cross table of season and weathersit, then produce proportions by rows and columns respectively.

#To convert variable to factor
day$weathersit <- as.factor(day$weathersit)
#To Find cross table
xtab.season.weathersit <- xtabs(~ season + weathersit, data = day)
#To produce propotions by rows (season)
prop.table(xtab.season.weathersit, margin = 1) 
#To produce propotions by columns(weathersit)
prop.table(xtab.season.weathersit, margin = 2)

#Q9.Please plot the histogram and density of the cnt and add the vertical line denoting the mean using ggplot2.

library(ggplot2)
ggplot(data=day,aes(x=cnt))+
geom_histogram( aes(y=..density..),color="black",fill="white",binwidth = 60)+
geom_density(alpha=.2,fill="blue")+geom_vline(aes(xintercept=mean(cnt)),color="orange",linetype="dashed",linewidth=1)

#Q10.Please scatter plot of cnt (y-axis) against registered (x-axis) and add the trend line using ggplot2.

ggplot(data = day, aes(x = registered, y=cnt))+geom_point()+geom_smooth() +
  labs(title = "Scatter plot of count againist registered", x = "registered", y = "count")

#Q11.Please plot the barplot of season and weathersit on the same barplot using ggplot2

#To convert variable to factor
day$weathersit <- as.factor(day$weathersit)
day$season <- as.factor(day$season)
#To check whether a variable is factor or not
is.factor(day$weathersit)
is.factor(day$season)

ggplot(data = day, aes(x=weathersit)) + geom_bar(aes(fill = season),position = "dodge") +
  labs(title = "Barplot of Weather Situation by Season", x = "Weather Situation", y = "Count", fill = "Season")

#Q12.Please boxplot cnt (y-axis) against weathersit (x-axis) and save the graph in a file, cntweather.jpg, using ggplot2. Are there any differences in cnt with respect to weathersit?

#To convert variable to factor
day$weathersit <- as.factor(day$weathersit)

#To check whether a variable is factor or not
is.factor(day$weathersit)

ggplot(data=day, aes(x=weathersit, y=cnt)) + 
geom_boxplot(notch = TRUE)+
labs(title = "Boxplot of Count by Weather Situation", x = "Weather Situation", y = "count")
ggsave("Downloads/cntweather.jpg", width = 20, height = 15, units = "cm")

#ANS: Yes. There are visibile difference in cnt with respect to weathersit as the median counts are different for each weather situation

#Q13.Build the following multiple linear regression models:
  #a.Perform multiple linear regression with cnt as the response and the predictors are: season, weathersit, atemp, and registered.
model_1 <- lm(cnt ~ season + weathersit + atemp + registered, data = day)
summary(model_1)
  #Write down the math formula with numerical coefficients for predictors  atemp and registered and skip the coefficients for season and  weathersit.
  # cnt = B0 + B1*atemp +B2*registered
  #b.Preform multiple linear regression with cnt as the response and the predictors are: season, workingday, weathersit, atemp, and registered. 
model_2 <- lm(cnt ~ season + workingday + weathersit + atemp + registered, data = day)
summary(model_2)
  #Write down the math formula with numerical coefficients for predictors  atemp and registered and skip the coefficients for season, workingday and weathersit.
  # cnt = B0 + B1*atemp +B2*registered
  #c.Preform multiple linear regression with cnt as the response and the predictors are: season, holiday, workingday, weathersit, atemp, hum, windspeed, and registered. 
model_3 <- lm(cnt ~ season + holiday+ workingday + weathersit + atemp + hum+ windspeed + registered, data = day)
summary(model_3)
  #Write down the math formula with numerical coefficients for predictors  atemp, hum, windspeed, and  registered  and skip the coefficients for season, holiday, workingday and  weathersit.
# cnt = B0 + B1*atemp +B2*hum + B3*windspeed +B4*registered
  #d.Which model do you recommend to the management based on adjusted R squared? Justify your answer.
  #Third model is the best in terms of adjusted R-sqaured value(0.9653) because it is closer to one and greater than other two models

#Q15.Build the following logistic models:

  #a.forecast holiday using cnt, season, and registered.
is.factor(day$holiday)
day$holiday <- as.factor(day$holiday)
library(pscl)
model_1 <- glm(holiday ~ cnt +season+registered, data = day, family = binomial)
pR2(model_1)
  #b.forecast the holiday using cnt, season, weathersit , and registered
model_2 <- glm(holiday ~ cnt +season+weathersit+registered, data = day, family = binomial)
pR2(model_2)
 #c.forecast the holiday using cnt, season, weathersit , workingday, and registered
model_3 <- glm(holiday ~ cnt +season+weathersit+workingday+registered, data = day, family = binomial)
pR2(model_3)
  #d.Which model do you recommend to the management based on McFadden/pseudo R squared to? Justify your answer
#ANS: I would recommend the third model as it has the highest McFadden/pseudo R squared value among the above three models (0.2883)

