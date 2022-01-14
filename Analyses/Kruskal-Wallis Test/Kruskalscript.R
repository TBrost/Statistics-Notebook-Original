KruskalWallis
library(mosaic)
?SaratogaHouses
View(SaratogaHouses)
table(SaratogaHouses$fuel)
kruskal.test(price ~ fuel, data = SaratogaHouses)    
#stripchart(price ~ fuel, data=SaratogaHouses)
#barchart(price ~ fuel, data=SaratogaHouses)
boxplot(price ~ fuel, data=SaratogaHouses)
favstats(price~ fuel, data=SaratogaHouses)



?ToothGrowth

 xyplot(len ~ as.factor(dose), groups=supp, data=ToothGrowth, type=c("p","a"), auto.key=TRUE)

 myaov <- aov(len ~ supp + as.factor(dose) + as.factor(dose):supp, data=ToothGrowth) 
 
 summary(myaov) 
 
 plot(myaov, which=1:2)
 
 
library(mosaic)
?RailTrail
boxplot(cloudcover ~ weekday, data=RailTrail, names=c("Weekend/Holiday", "Weekday"), ylab="Cloud Cover Measurement (in oktas)") 

wilcox.test(RailTrail$cloudcover[RailTrail$dayType == "weekday"], RailTrail$cloudcover[RailTrail$dayType == "weekend"], mu = 0, alternative = "two.sided", conf.level = 0.95) 
