library(car)
library(tidyverse)
library(mosaic)
library(DT)
library(pander)
library(plotly)
library(ggplot2)
library(readr)


View(starwars)

starwars1 <- filter(starwars, species== "Wookiee" |  species=="Gungan" |  species== "Kaminoan")

p2 <- ggplot(starwars1, aes(x=species, y=height, fill = species))+
  geom_boxplot()+
  labs(title="How many hours a week do students spend \n with family based on Family size?", x="Size of Family", y="Hours/Week with Family")+
  stat_summary(fun=mean, geom="point", shape=4, size=4, color="red", fill="red") 
ggplotly(p2)
  p2

  starwars.aov <- aov(height ~ species, data = starwars1)
  summary(starwars.aov)    

?Highway1

View(Highway1)
Highway1.1 <- filter(Highway1, lane== 2 )
hist(Highway1.1$rate)

wilcox.test(Highway1$rate[Highway1$lane == 2],
            Highway1$rate[Highway1$lane == 4], mu = 0, alternative = "two.sided", conf.level = 0.95, conf.int = TRUE) %>%
  pander(caption="Wilcoxon test for Meshed vs. SFR")

plot(rate ~ slim, data=Highway1, pch=16, xlab="", ylab="", main="")
favstats(rate ~ htype, data =Highway1)


plot(height ~ age, data=Loblolly)

lm.2lineslob <-lm(height ~ age , data=Loblolly)
summary(lm.2lineslob)

par(mfrow=c(1,3))
plot(lm.2lineslob,which=1:2)
plot(lm.2lineslob$residuals)





?KidsFeet

wilcox.test(KF$length[KF$sex == "G"],
            KF$length[KF$sex == "B"], mu = 0, alternative = "two.sided", conf.level = 0.95, conf.int = TRUE) %>%
  pander(caption="Wilcoxon test for Meshed vs. SFR")


xyplot(length ~ sex, data=KidsFeet, group=domhand, type=c("p","a"), auto.key=TRUE)
KF.aov <- aov(length ~ sex + domhand + sex:domhand, data = KF)
summary(KF.aov)






?iris
View(iris)
lm.2linesiris <-lm(Sepal.Length ~ Sepal.Width + Species, data=iris)
summary(lm.2linesiris) %>%
  pander()


?cars
plot(speed ~ dist, data= cars)
lm.2linescar <-lm(speed ~ dist, data=cars)
summary(lm.2linescar) %>%
  pander()

plot(speed > 15 ~ dist, data=cars, ylab="Probability Speed > 15 mph", xlab="Stopping Distance (feet)")

cars$speed15 <- case_when(cars$speed >15 ~ 1, cars$speed <= 15 ~0)
cars.glm <- glm(cars$`speed15` ~ cars$`dist`, data = cars,
                  family=binomial)
summary(cars.glm)%>%
  pander(caption= "Time in Math lab vs Course topic")


View(Galton)

#Step 1
myTest <-t.test(Galton$height[Galton$sex == "F"], Galton$height[Galton$sex == "M"], paired = FALSE, mu = 0, alternative = "two.sided", conf.level = 0.95)
observedTestStat <- 30.662

#Step 2
N <- 2000      
permutedTestStats <- rep(NA, N)
for (i in  1:N){
  permuteData <- sample(x=c(-1,1), size=10, replace=TRUE) 
  permutedTest <- with(Galton, t.test(permuteData*(height[sex=="F"] - height[sex=="M"]), mu = 0))
  permutedTestStats[i] <- permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat)

#Step 3
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N



?singer
View(singer)
ggplot(singer[singer$voice.part="Soprano 1"], aes(x=singer$height))+
  geom_histogram()
ggplot(singer, aes(x=height))+ 
  geom_histogram()+
  facet_wrap(singer$voice.part)

favstats(height ~ voice.part, data =singer)
singer.aov <- aov(height ~ voice.part, data = singer)
summary(singer.aov)

hist(islands, xlab="Area in Thousands of Square Miles", main="Areas of the World's Major Landmasses")
summary(islands)






view(mtcars)
ggplot(mtcars, aes(x=cyl))+ 
  geom_histogram()+
  facet_wrap(mtcars$am)




View(ToothGrowth)
#Is the average length of tooth growth in guinea pigs different for guinea pigs receiving an 
#Orange Juice supplement than it is for guinea pigs receiving a Vitamin C supplement?

t.test(len ~ supp, data=ToothGrowth, mu = 0, alternative = "two.sided", conf.level = 0.95)
