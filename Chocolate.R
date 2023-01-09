library(readr)
Chocolate <- read_csv("Data/Chocolate.csv")
#list <- c(unique(Chocolate$BroadOrig)) #get list of all counttries
list <- c( "Peru", "Brazil", "Ecuador", "Venezuala", "Colombia", "Chile", "Bolivia", "Uruguay", "Argentina", "Suriname", "Guyana", "Paraguay")
`%!in%` <- Negate(`%in%`)
library(tidyverse)
library(pander)
Chocolate$CountryOrig <- case_when(Chocolate$BroadOrig %in% list ~ 1 , Chocolate$BroadOrig %!in% list ~ 0)

ChocolateSA <- filter(Chocolate, Chocolate$CountryOrig == 1 )
ChocolateO <- filter(Chocolate, Chocolate$CountryOrig == 0)

hist(ChocolateSA$Rating, main= "South American Chocolate Ratings", col="blue3", xlab="Rating (1=poor)",)
hist(ChocolateO$Rating, main= "Other Chocolate Ratings", col="orange3", xlab="Rating (1=poor)",)

Choc.glm <- glm(Chocolate$CountryOrig ~ Chocolate$Rating, data = Chocolate,
                  family=binomial)
summary(Choc.glm)%>%
  pander(caption= "stuff")

ggplot(data=Chocolate, aes(x=Rating,y=CountryOrig)) +
  geom_point(color=rgb(.8,.4,.1,.025)) +
  geom_smooth(method="glm",method.args = list(family="binomial"), se=FALSE) +
  theme_bw()

pchisq(2148, 1792, lower.tail=FALSE)
\

plot(Temp ~ Month, data=airquality, col="skyblue", pch=21, bg="gray83", main="Quadratic Model using airquality data set", cex.main=1)

lm.quad <- lm(Temp ~ Month + I(Month^2), data=airquality)

#get the "Estimates" automatically:
b <- coef(lm.quad)
# Then b will have 3 numbers stored inside:
# b[1] is the estimate of beta_0: -95.73
# b[2] is the estimate of beta_1: 48.72
# b[3] is the estimate of beta_2: -3.28
curve(, col="skyblue", lwd=2, add=TRUE)

library(ggplot2)
b <- coef(lm.quad)
ggplot(airquality, aes(y=Temp, x=Month)) +
  geom_point(pch=21, bg="gray98") +
  stat_function(fun = function(x) b[1] + b[2]*x + b[3]*x^2, color="lightblue4")
summary(lm.quad)
  pander(caption= "stuff")
