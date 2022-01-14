pval <- pchisq(904.9, 755, lower.tail=FALSE)

pander(str_glue("The p-value from the test is: {pval}."))











palette(c("skyblue","firebrick"))

plot(mpg ~ qsec, data=mtcars, col=as.factor(am), xlim=c(0,30), ylim=c(-30,40), main="1974 Motor Trend Cars", pch=16)

lm2 <- lm(mpg ~ qsec + am + qsec:am, data=mtcars)

summary(lm2)



abline( -9.0099, 1.4385, col=palette()[1])

abline( -23.5197, 2.7599, col=palette()[2])

legend("topleft", legend=c("automatic","manual"), pch=1, col=palette(), title="Transmission (am)", bty="n")



palette(c("skyblue","firebrick"))

plot(mpg ~ qsec, data=mtcars, col=as.factor(am), xlim=c(0,30), ylim=c(-30,40), main="1974 Motor Trend Cars", pch=16)

lm2 <- lm(mpg ~ qsec + am , data=mtcars)

summary(lm2)



abline( -18.8893, 1.98, col=palette()[1])

abline( -10.01, 1.98, col=palette()[2])

legend("topleft", legend=c("automatic","manual"), pch=1, col=palette(), title="Transmission (am)", bty="n")





plot(mpg ~ qsec, data=mtcars, col=as.factor(am), xlim=c(0,30), ylim=c(-30,40), main="1974 Motor Trend Cars", pch=16)

lm2 <- lm(mpg ~ qsec + qsec:am , data=mtcars)

summary(lm2)



abline( -15.3005, 1.78149, col=palette()[1])

abline( -15.3005, 2.29107, col=palette()[2])

legend("topleft", legend=c("automatic","manual"), pch=1, col=palette(), title="Transmission (am)", bty="n")





























This regression model does not include an interaction term, so it requires the slopes of the two lines to be the same.

The ??0 term is the y-intercept of the baseline (year = 87), and is significantly different from zero as shown by its p-value.

The ??1 term is the slope of the baseline (year = 87) and is significantly different from zero as show by its p-value.

The ??2 term is the change in the y-intercept of the year = 88 line over the baseline. Since the p-value for this term is not significant, the line isn't really different from the baseline, and thus we would continue to believe the null hypothesis that ??2=0 , which would cause the lines to be the same line. In other words, the average foot length of kids doesn't change depending on the year they were born in, it only depends on the width of their foot.





kids.lm <- lm(length ~ width + as.factor(birthyear), data=KidsFeet)
summary(kids.lm)



plot(circumference ~ age, data=Orange) #notice the fanning out of the points on the right

orange.lm <- lm(circumference ~ age, data=Orange)

plot(orange.lm, which=1) #looks very linear, but variance is increasing (so not constant)

plot(orange.lm, which=2) #looks very normal.





















KF <- KidsFeet
KF$GB <- case_when(KF$sex == "B" ~ 1, KF$sex == "G" ~ 0 )
KFGL <- glm(KF$GB ~ KF$length, data = KF,
                    family=binomial)
summary(KFGL)


ggplot(data=KF, aes(x=KF$length,y=KF$GB, color=factor(KF$GB))) +
  geom_point()+
  geom_smooth(method="glm",method.args = list(family="binomial"), se=FALSE) +
  theme_bw()


predict(KFGL, newdata = data.frame(length = 25), type = "response")
