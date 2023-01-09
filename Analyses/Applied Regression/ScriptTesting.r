ggplot(airquality, aes(x=Temp))+
  geom_histogram(binwidth=5, fill="skyblue", color="lightblue")+
  labs(title= "title", x="temp", y="count")
library(ggplot2)
mean(airquality$Temp)
library(mosaic)
favstats(airquality$Temp)
library(car)

ggplot(airquality, aes(x=factor(Month), y=Temp))+
  geom_boxplot(fill="skyblue", color="lightblue")+
  labs(title= "title", x="temp", y="count")
favstats(airquality$Temp~airquality$Month)



testlm <- lm(Temp ~ Wind, airquality)
summary(testlm)
b <- coefficients(testlm)
ggplot(airquality, aes(x=Wind, y=Temp))+
  geom_point(fill="skyblue", color="lightblue")+
  labs(title= "title", x="temp", y="count")+
  theme_bw()+
  stat_function(fun = function(x) (b[1]) + (b[2])*x, color="#A58AFF")
  #  OR  #
  #geom_smooth(method=lm, se=F, formula=y~x)
favstats(airquality$Temp~airquality$Month)
b[1]+ (b[2]*19)


set.seed(102) #Allows us to always get the same "random" sample
#Change to a new number to get a new sample

n <- 153 #set the sample size
X_i <- runif(n, 0, 20) #Gives n random values from a uniform distribution between 15 to 45.
beta0 <- 89 #Our choice for the y-intercept. 
beta1 <- -1.25 #Our choice for the slope. 
sigma <- 8 #Our choice for the std. deviation of the error terms.
epsilon_i <- rnorm(n, 0, sigma) #Gives n random values from a normal distribution with mean = 0, st. dev. = sigma.
Y_i <- beta0 + beta1*X_i + epsilon_i #Create Y using the normal error regression model
fabData <- data.frame(y=Y_i, x=X_i) #Store the data as data
#View(fabData) 
#In the real world, we begin with data (like fabData) and try to recover the model that (we assume) was used to created it.
fab.lm <- lm(y ~ x, data=fabData) #Fit an estimated regression model to the fabData.
summary(fab.lm) #Summarize your model. 



plot(y ~ x, data=fabData) #Plot the data.
abline(fab.lm) #Add the estimated regression line to your plot.

# Now for something you can't do in real life... but since we created the data...

abline(beta0, beta1, lty=2) #Add the true regression line to your plot using a dashed line (lty=2). 

legend("topleft", legend=c("True Line", "Estimated Line"), lty=c(2,1), bty="n") #Add a legend to your plot specifying which line is which.














mylm <- lm(Wind ~ Temp, airquality)
summary(mylm)
b <- coefficients(mylm)
ggplot(airquality, aes(x=Temp, y=Wind))+
  geom_point(fill="skyblue", color="lightblue")+
  labs(title= "title", x="temp", y="count")+
  theme_bw()+
  stat_function(fun = function(x) (b[1]) + (b[2])*x, color="#A58AFF")
favstats(airquality$Temp~airquality$Month)
b[1]+ (b[2]*72)

par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)

t1 = (b[1] -0)/2.11239
t1
pt(-abs(t1), 151)*2

t2 = (b[2] -0)/0.02693
t2
pt(-abs(t2), 151)*2




mylm2 <- lm(mpg ~ wt, mtcars)
summary(mylm2)
b <- coefficients(mylm2)
ggplot(mtcars, aes(x=wt, y=mpg))+
  geom_point(fill="skyblue", color="lightblue")+
  labs(title= "title", x="temp", y="count")+
  theme_bw()+
  stat_function(fun = function(x) (b[1]) + (b[2])*x, color="#A58AFF")
favstats(airquality$Temp~airquality$Month)
b[1]+ (b[2]*3)


par(mfrow=c(1,3))
plot(mylm2,which=1:2)
plot(mylm2$residuals)

t1 = (b[1] -0)/1.8776
t1
pt(-abs(t1), 30)*2

t2 = (b[2] -0)/0.5591
t2
pt(-abs(t2), 30)*2




library(car)
View(Davis)
?Davis

Davis2 <- Davis %>% filter(sex == "M")
library(tidyverse)

testlm <- lm(weight ~ height, Davis2)
summary(testlm)
180*0.9956 + -101.3301


View(USArrests)
?USArrests

testlm <- lm(Murder ~ Assault, USArrests)
summary(testlm)


