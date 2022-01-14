
set.seed(1140411)
sample1 <- rnorm(30, 69, 2.5)
sample2 <- rnorm(30, 69, 2.5)
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30))
View(theData)
boxplot(values ~ group, data = theData)



# Run the permutation test:

myTest <-  t.test(values ~ group, data=theData, mu=0)
observedTestStat <- myTest$statistic



N <- 2000      
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedData <- sample(x=theData$group)
  permutedTest <- t.test(values ~ permutedData, data=theData, mu=0)
  permutedTestStats[i]  <-  permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat)
sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N
sum(permutedTestStats != observedTestStat)/N






















Paired test
set.seed(121)
sample1 <- rnorm(30, 185, 8)
sample2 <- sample1 - rnorm(30, 0, 3.5)
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30), id = rep(c(1:30),times=2))
with(theData, hist(values[group==1] - values[group==2]))
myTest <- t.test(values ~ group, data= theData, mu=0, paired= TRUE)
  # Get the test statistic from the test:
observedTestStat <- myTest$statistic


# Obtain the permutation sampling distribution 
N <- 2000
permutedTestStats <- rep(NA, N)
for (i in 1:N){
  permuteData <- sample(x=c(1,-1), size=30, replace=TRUE) 
  permutedTest <- with(theData, t.test(permuteData*(values[group==1] - values[group==2]), mu = 0))
  #Note, t.test(group1 - group2) is the same as t.test(group1, group2, paired=TRUE).
  permutedTestStats[i] <- permutedTest$statistic
}
hist(permutedTestStats)
abline(v=observedTestStat, col='skyblue', lwd=3)

sum(permutedTestStats >= observedTestStat)/N
sum(permutedTestStats <= observedTestStat)/N
2*sum(permutedTestStats <= observedTestStat)/N
