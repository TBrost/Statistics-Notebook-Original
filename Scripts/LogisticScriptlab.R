#tutor stuff
library(car)
library(tidyverse)
library(mosaic)
library(DT)
library(pander)
library(ggplot2)
library(readr)

lab <- read_csv("C:/Users/user/OneDrive/Desktop/Stats stuff/325 book/Statistics-Notebook-master/tutoringcenter.csv")

labmin <- filter(lab, `Visit duration (in minutes)` >= 1)

labmin$`60` <- case_when(labmin$`Visit duration (in minutes)` <=60 ~ 0, labmin$`Visit duration (in minutes)` >60 ~ 1 )

clist <- list("MATH 221A", "MATH 221B", "MATH 221C", "MATH 325", "MATH 425")
clist <- list("MATH 100a", "MATH 100b", "MATH 101", "MATH 108X", "MATH 109", "MATH 110X", "MATH 111", "MATH 112X", "MATH 113", "MATH 119", "MATH 191", "")

labmindi$`60` <- case_when(labmindi$`Course` == "MATH 221A" | labmindi$`Course` == "MATH 221B" | labmindi$`Course` == "MATH 221C" | labmindi$`Course` == "MATH 325" | labmindi$`Course` == "MATH 425" ~ 0,
                           labmindi$`Course` != "MATH 221A" | labmindi$`Course` != "MATH 221B" | labmindi$`Course` != "MATH 221C" | labmindi$`Course` != "MATH 325"| labmindi$`Course` != "MATH 425"  ~ 1 )
labmindiIS <- filter(labmindi, labmindi$`Course` %in% clist == TRUE & labmindi$`Visit duration (in minutes)` !=60)
labmindiNS <- filter(labmindi, labmindi$`Course` %in% clist == FALSE & labmindi$`Visit duration (in minutes)` !=60)
labmindiISD <- filter(labmindi, labmindi$`Course` %in% clist == TRUE)
labmindiNSD <- filter(labmindi, labmindi$`Course` %in% clist == FALSE)                   



hist(labmindiNSD$`Visit duration (in minutes)`)
#BOTH are very right skewed
favstats(labmindiIS$`Visit duration (in minutes)`)
#median in stats 65.27
favstats(labmindiNS$`Visit duration (in minutes)`)
#median not in stats 64.27

labmindiAVG <- labmindi
labmindiAVG$`Visit duration (in minutes)` <- case_when(labmindi$`Course` %in% clist == TRUE & labmindi$`Visit duration (in minutes)` ==60 ~ 65.27,
                         labmindi$`Course` %in% clist == FALSE & labmindi$`Visit duration (in minutes)` ==60 ~ 64.27, labmindi$`Visit duration (in minutes)` !=60 ~ labmindi$`Visit duration (in minutes)`)
YourGlmName4 <- glm(labmindiAVG$`60` ~ labmindiAVG$`Visit duration (in minutes)`, data = labmindiAVG,
                   family=binomial)
summary(YourGlmName4)



YourGlmName <- glm(labmindi$`60` ~ labmindi$`Visit duration (in minutes)`, data = labmindi,
                   family=binomial)
summary(YourGlmName)



labmindi <- filter(labmin, labmin$`Slot type`== "DropIn")
ggplot(data=labmindi, aes(x=labmindi$`Visit duration (in minutes)`,y=labmindi$`60`)) +
  geom_point() +
  geom_smooth(method="glm",method.args = list(family="binomial"), se=FALSE) +
  theme_bw()


library(tidyr)
labmindp <-labmindi


#find Q1, Q3, and interquartile range for values in column family
Q1 <- quantile(labmindp$`Visit duration (in minutes)`, .25, na.rm = TRUE)
Q3 <- quantile(labmindp$`Visit duration (in minutes)`, .75, na.rm = TRUE)
IQR <- IQR(labmindp$`Visit duration (in minutes)`, na.rm = TRUE)


#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
LCD <- subset(labmindp, labmindp$`Visit duration (in minutes)`> (Q1 - 1.5*IQR) & labmindp$`Visit duration (in minutes)`< (Q3 + 1.5*IQR))

#LCD <- LCD[-c(3980:4084),]


YourGlmName2 <- glm(LCD$`60` ~ LCD$`Visit duration (in minutes)`, data = LCD,
                   family=binomial)
summary(YourGlmName2)

pchisq(3312.5, df=3879, lower.tail=FALSE)

ggplot(data=LCD, aes(x=LCD$`Visit duration (in minutes)`,y=LCD$`60`)) +
  geom_point() +
  geom_smooth(method="glm",method.args = list(family="binomial"), se=FALSE) +
  theme_bw()+
  coord_cartesian(xlim = c(0, 500))




LCDs <- LCD[sample(nrow(LCD), 1000), ]

YourGlmName3 <- glm(LCDs$`60` ~ LCDs$`Visit duration (in minutes)`, data = LCDs,
                    family=binomial)
summary(YourGlmName3)

pchisq(896, df=976, lower.tail=FALSE)

ggplot(data=LCDs, aes(x=LCDs$`Visit duration (in minutes)`,y=LCDs$`60`)) +
  geom_point() +
  geom_smooth(method="glm",method.args = list(family="binomial"), se=FALSE) +
  theme_bw()+
  coord_cartesian(xlim = c(0, 150))



plot(LCD$`60` ~ LCD$`Visit duration (in minutes)`, data=LCD)
curve(exp((1.925436) + (-0.0035)*x)/(1 + exp((1.925436) + (-0.0035)*x)), add = TRUE)







LCDIS <- filter(LCD, LCD$`Course` %in% clist == TRUE & LCD$`Visit duration (in minutes)` !=60)
LCDNS <- filter(LCD, LCD$`Course` %in% clist == FALSE & LCD$`Visit duration (in minutes)` !=60)
LCDISD <- filter(LCD, LCD$`Course` %in% clist == TRUE)
LCDNSD <- filter(LCD, LCD$`Course` %in% clist == FALSE)                   



hist(LCDISD$`Visit duration (in minutes)`)
#BOTH are very right skewed
favstats(LCDIS$`Visit duration (in minutes)`)
#mean in stats 63.68339
favstats(LCDNS$`Visit duration (in minutes)`)
#mean not in stats 58.642468

LCDAVG <- LCD
LCDAVG$`Visit duration (in minutes)` <- case_when(LCD$`Course` %in% clist == TRUE & LCD$`Visit duration (in minutes)` ==60 ~ 65.27,
                                                       LCD$`Course` %in% clist == FALSE & LCD$`Visit duration (in minutes)` ==60 ~ 64.27, LCD$`Visit duration (in minutes)` !=60 ~ LCD$`Visit duration (in minutes)`)
YourGlmName4 <- glm(LCDAVG$`60` ~ LCDAVG$`Visit duration (in minutes)`, data = LCDAVG,
                    family=binomial)
summary(YourGlmName4)
#P = 0.00673
pchisq(3311.2, df=3879, lower.tail=FALSE)
#1
ggplot(data=LCDAVG, aes(x=LCDAVG$`Visit duration (in minutes)`,y=LCDAVG$`60`)) +
  geom_point(color=rgb(.2,.4,.1,.025)) +
  geom_smooth(method="glm",method.args = list(family="binomial"), se=FALSE) +
  theme_bw()+
  coord_cartesian(xlim = c(0, 150))



LCDAVGs <- LCDAVG[sample(nrow(LCDAVG), 1000), ]

YourGlmName5 <- glm(LCDAVGs$`60` ~ LCDAVGs$`Visit duration (in minutes)`, data = LCDAVGs,
                    family=binomial)
summary(YourGlmName5)
#0.0396
pchisq(787.9, df=975, lower.tail=FALSE)
#.999997
ggplot(data=LCDAVGs, aes(x=LCDAVGs$`Visit duration (in minutes)`,y=LCDAVGs$`60`)) +
  geom_point(color=rgb(.2,.4,.1,.1)) +
  geom_smooth(method="glm",method.args = list(family="binomial"), se=FALSE) +
  theme_bw()+
  coord_cartesian(xlim = c(0, 150))


