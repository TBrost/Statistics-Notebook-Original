library(ggplot2)
air.glm <- glm(Wind > 10 ~ Temp + as.factor(Month), data=airquality, family=binomial)
ggplot(data=airquality, aes(y=(Wind >10),x=Temp)) +
  geom_smooth(method="glm",method.args = list(family="binomial"), se=FALSE) +
  facet_wrap(airquality$Month)

plot((Wind > 10) ~ Temp, data=airquality, xlab="Daily Average Temperature", ylab="Probability Daily Average Wind Speed (mph) > 10", main="La Guardia Airport Measurements in 1973")+
  

air.glm <- glm(Wind > 10 ~ Temp + as.factor(Month), data=airquality, family=binomial)
?airquality

plot((Wind > 10) ~ Temp, data=airquality, xlab="Daily Average Temperature", ylab="Probability Daily Average Wind Speed (mph) > 10", main="La Guardia Airport Measurements in 1973")

air.glm <- glm(Wind > 10 ~ Temp + as.factor(Month), data=airquality, family=binomial)

summary(air.glm)


















WW3 <- cbind(`yes` = c("DONT BELIEVE" = 87.8, NOWAYTOFINDOUT = 176.1, SOMEHIGHERPOWER = 358.8, BELIEVESOMETIMES = 173.8, BELIEVEBUTDOUBTS = 730.1, KNOWGODEXISTS = 3131.8), `no` = c(DONTBELIEVE = 163.0, NOWAYTOFINDOUT = 257.1, SOMEHIGHERPOWER = 469.2, BELIEVESOMETIMES = 215.8, BELIEVEBUTDOUBTS = 805, KNOWGODEXISTS = 2769.7))

barplot(WW3, beside=TRUE, col=c("green","blue","purple", "yellow", "orange", "tomato3"),legend.text=TRUE,args.legend = list(x="topleft",bty="n",title="Strength of Beliefs"),xlab="World War 3?",main="Are Religious People Worried about World War 3?")
