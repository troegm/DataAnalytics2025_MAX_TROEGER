# Lab 1 - Data Analytics
# Friday January 24, 2025
# Max Troeger (troegm@rpi.edu)

EPI_data <- read.csv("Documents/School/data_analytics/repo/lab1/epi2024results06022024.csv")
attach(EPI_data)
NAs <- is.na(EPI.new)
EPI.new.noNAs <- EPI.new[!NAs]

## Exercise 1
summary(EPI.new)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 24.50   38.25   45.50   46.84   53.10   75.30

fivenum(EPI.new,na.rm=TRUE)
# 24.5 38.2 45.5 53.1 75.3

stem(EPI.new)
#The decimal point is 1 digit(s) to the right of the |
#  2 | 
#  2 | 5667889
#  3 | 001122233334444
#  3 | 5555666666777777788888889999999
#  4 | 0000000111122222222333333334444
#  4 | 5555556666667777777778888889999999
#  5 | 000011122222233333444
#  5 | 5666677788
#  6 | 0011223334444
#  6 | 56677777889
#  7 | 0134
#  7 | 555

hist(EPI.new)
hist(EPI.new,seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))
rug(EPI.new)

boxplot(EPI.new,APO.new)

hist(EPI.new,seq(20., 80., 1.0), prob=TRUE)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)

## Exercise 2
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 

qqnorm(EPI.new)
qqline(EPI.new) 

qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new) 

qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)

## Exercise 2a
### MPE.new
#View(EPI_data)
NAs <- is.na(MPE.new)
MPE.new.noNAs <- MPE.new[!NAs]
summary(MPE.new)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.70   50.00   70.20   69.85  100.00  100.00      49 

fivenum(MPE.new,na.rm=TRUE)
# 1.7  50.0  70.2 100.0 100.0

stem(MPE.new)
# The decimal point is 1 digit(s) to the right of the |
# 0 | 2
# 0 | 
# 1 | 11
# 1 | 
# 2 | 11
# 2 | 6677
# 3 | 13333
# 3 | 6777
# 4 | 01344
# 4 | 55557789
# 5 | 000000000133444
# 5 | 55557789
# 6 | 0111224
# 6 | 6799
# 7 | 00033
# 7 | 566788
# 8 | 4
# 8 | 566888999
# 9 | 1
# 9 | 67788
# 10 | 000000000000000000000000000000000000000

hist(MPE.new)
hist(MPE.new,seq(0., 100., 1.0), prob=TRUE)
lines(density(MPE.new,na.rm=TRUE,bw=1.))
lines(density(MPE.new,na.rm=TRUE,bw="SJ"))
rug(MPE.new)

boxplot(MPE.new,APO.new)

hist(MPE.new,seq(0., 100., 1.0), prob=TRUE)
x<-seq(0,100,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)

plot(ecdf(MPE.new), do.points=FALSE, verticals=TRUE) 

qqnorm(MPE.new)
qqline(MPE.new) 

qqplot(rnorm(250), MPE.new, xlab = "Q-Q plot for norm dsn")
qqline(MPE.new) 

qqplot(rt(250, df = 5), MPE.new, xlab = "Q-Q plot for t dsn")
qqline(MPE.new)

## Exercise 2a
### BDH.new
NAs <- is.na(BDH.new)
MPE.new.noNAs <- BDH.new[!NAs]
summary(BDH.new)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00   34.67   49.90   47.71   60.55   85.80 

fivenum(BDH.new,na.rm=TRUE)
# 5.00 34.35 49.90 60.70 85.80

stem(BDH.new)
# The decimal point is 1 digit(s) to the right of the |
# 0 | 5
# 1 | 012333
# 1 | 556788
# 2 | 0000134
# 2 | 56666778999
# 3 | 00011122333344
# 3 | 5666777788999
# 4 | 0001122333444
# 4 | 55556666677777889
# 5 | 0000001111122222233334444
# 5 | 55566667777788999
# 6 | 000001112233344444
# 6 | 55667778889
# 7 | 000011122234
# 7 | 599
# 8 | 1234
# 8 | 56

hist(BDH.new)
hist(BDH.new,seq(5., 86., 1.0), prob=TRUE)
lines(density(BDH.new,na.rm=TRUE,bw=1.))
lines(density(BDH.new,na.rm=TRUE,bw="SJ"))
rug(BDH.new)

boxplot(BDH.new,APO.new)

hist(BDH.new,seq(5., 86., 1.0), prob=TRUE)
x<-seq(5,86,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)

plot(ecdf(BDH.new), do.points=FALSE, verticals=TRUE) 

qqnorm(BDH.new)
qqline(BDH.new) 

qqplot(rnorm(250), BDH.new, xlab = "Q-Q plot for norm dsn")
qqline(BDH.new) 

qqplot(rt(250, df = 5), BDH.new, xlab = "Q-Q plot for t dsn")
qqline(BDH.new)