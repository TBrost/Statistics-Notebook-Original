View(infert)
?infert
infert.glm <- glm( (spontaneous > 0) ~ age, data=infert, family=binomial)

summary(infert.glm)
plot( (spontaneous > 0) ~ age, data=infert)

curve( exp(1.48706 + -0.05616*x)/(1 + exp(1.48706 + -0.05616*x)), add=TRUE)
table(infert$age)

pchisq(334.01, 246, lower.tail=FALSE)




library(mosaic)

View(Galton)

?Galton

Gal.glm <- glm( sex == "M" ~ Galton$height, data=Galton, family=binomial)
summary.glm(Gal.glm)

library(ResourceSelection)
hoslem.test(Gal.glm$y, Gal.glm$fitted, g=10)

predict(Gal.glm, newdata = data.frame(height = 65.0), type = "response")


myglm <- glm(am ~ disp, data = mtcars, family = binomial)
predict(myglm, newdata = data.frame(disp = 200), type = "response")














library(mosaic)

View(Gestation)

?Gestation

Ges.glm <- glm( smoke == "never" ~ wt, data=Gestation, family=binomial)
summary.glm(Ges.glm)

smoke!="never"