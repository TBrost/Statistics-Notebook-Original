library(car)
library(dplyr)
library(mosaic)
?KidsFeet

mylm <- lm(`length` ~ width, data = KidsFeet)

summary(mylm)
  pander(caption = "Simple Regression Model")

  
  
  RT <- RailTrail
  
  RT$Season <- as.factor(with(RT, 1*spring + summer + 0*fall))
favstats(precip ~ Season, data= RT)  


mylm <- lm(`hightemp` ~ dayType + Season + dayType:Season, data = RT)
summary(mylm)

mylm <- lm(hightemp ~ lowtemp, data = RT)
summary(mylm)

RT.aov <- aov(hightemp ~ dayType + Season + dayType:Season, data = RT)
summary(RT.aov) 

RT.glm <- glm(Season ~ avgtemp, data=RT, family=binomial)
summary.glm(RT.glm)

RT <- RailTrail
t.test(hightemp~ as.factor(dayType),data= RT %>% filter(Season == 2), alternative = "two.sided", conf.level = 0.95)

RT$Season <- as.factor(with(RT, spring + 2*summer + 3*fall))
kruskal.test(volume ~ Season, data = RT)



View(KidsFeet)
favstats(length ~ birthyear, data= KidsFeet)
mylm <- lm(length ~ width, data = KidsFeet)
summary(mylm)
par(mfrow=c(1,3))
plot(mylm,which=1:2)
plot(mylm$residuals)


View(Galton)

KF2 <- KidsFeet
KF2$sex <- case_when(KF2$sex == "B" ~ 1, KF2$sex == "G" ~ 0)
KF.glm <- glm( sex ~ width, data=KidsFeet, family=binomial)
summary.glm(KF.glm)

library(ResourceSelection)
hoslem.test(Gal.glm$y, Gal.glm$fitted, g=10)

predict(KF.glm, newdata = data.frame(width = 9.5), type = "response")

x <- table(KidsFeet$biggerfoot, KidsFeet$domhand)
chisq.test(x)
results <- chisq.test(x)
results$expected


wilcoxon
wilcox.test(KidsFeet$width, mu = 8, alternative = 'two.sided', paired=FALSE, conf.level = 0.95) 




Log graph
their.glm <- glm( (volume>350) ~ hightemp + weekday, data=RailTrail, family=binomial)
summary(their.glm)
plot( ( (volume>350) + as.numeric(as.character(weekday))/100 ) ~ hightemp, data=RailTrail, col=weekday, ylab="Probability there are Over 200 Trail Users", xlab="Daily High Temperature") 
#Don't spend too much time reading the plotting code. It's just a fancy way of showing the data more clearly in the plot
curve( exp(-10.73710 + 0.16714*x)/(1 + exp(-10.73710 + 0.16714*x)), add=TRUE)
curve( exp(-10.73710 -0.28298 + 0.16714*x)/(1 + exp(-10.73710 -0.28298 + 0.16714*x)), add=TRUE)
