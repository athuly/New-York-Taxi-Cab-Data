setwd("C:/Users/jvijayaraghavan/Desktop/Kaggle/NYCab")
# data loading 
df <- read.csv("Fdata.csv",header = T)

#Library needed
library("dplyr", lib.loc="~/R/win-library/3.5")
library("lubridate", lib.loc="~/R/win-library/3.5")
library("geosphere", lib.loc="~/R/win-library/3.5")
library("leaflet", lib.loc="~/R/win-library/3.5")
library(ggplot2)
library("MASS", lib.loc="C:/Program Files/R/R-3.5.1/library")

#Converting data in to factors
df$title <- as.factor(df$title)
df$weekdays <- as.factor(df$weekdays)



#ploting the points on the geomap
set.seed(1234)
foo <- sample_n(df, 8e3)
leaflet(data = foo) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  
  addCircleMarkers(~ pickup_longitude, ~pickup_latitude, radius = 1,
                   
                   color = "blue", fillOpacity = 0.3) %>%
  
  addCircleMarkers(~ dropoff_longitude, ~dropoff_latitude, radius = 1,
                   
                   color = "red", fillOpacity = 0.3)
# all most all trip were fact in manhattan city only, another hotable spot is JFK
# airport towards the south east city

#ploting the data differently
#Histogram
# histogram for trip duration
ggp <- ggplot(df, aes(x=trip_duration))
ggp + geom_histogram(fill="blue")+ggtitle("Histogram of Trip duration")+xlab("Trip duration")+ylab("Count")

# histogram for passenger count
ggp <- ggplot(df, aes(x=passenger_count))
ggp + geom_histogram(fill="lightgreen")+ggtitle(" Histogram of Passenger count")+xlab("Passenger count")+ylab("Count")

# majority rides are single passengers,2 passenger is the second popular count

# bar plot of title(holidays)
barplot(table(df$title),col=c("blue","red","green","yellow","orange","black","pink"))

boxplot(df$trip_duration~df$title,xlab="Holiday",ylab="Travel time",main="Travel time vs holiday",col =c("red","sienna","royalblue2", "palevioletred1"))

#bar plot of holidays without nonholidays
unique(df$title)
k <- df[which(df$title!='nonholiday'),]
k$Holidays <- k$title
k1 <- k%>%group_by(Holidays)%>%summarise(n=n())
ggplot(data=k1, aes(x=Holidays,y=k1$n))+geom_bar(stat = 'identity',aes(fill=Holidays))+
  xlab("Holidays")+ylab(" ")

L <- length(which(df$title!='nonholiday'))
N <- nrow(df)
q <- mdy(df$date)
df$month <- month(q)
df$month <-as.factor(df$month)
(N-L)/(181-7)

# bar plot of weekdays
ggplot(df, aes(x=weekdays)) + geom_bar(fill=c("dark green","blue","red","pink","yellow","orange","light  blue"))+ggtitle("Bar plot of weekdays")
# friday and thursday have more passengers compared to other day

#histogram of dates

# 5. bar plolt of time
p <- hm(df$pickup_time)
df$time_day <- hour(p)
ggplot(df,aes(x=time_day))+geom_bar(fill="green",color=("violet"))+xlab("Time of the day")+ylab("Count of the trip")+ggtitle("Bar plot of time of the day")

# bar plot of month
q <- mdy(df$date)
df$month <- month(q)
df$month <- as.factor(df$month)
ggplot(df,aes(x=month))+geom_bar(fill=c("pink","orange","yellow","red","dark red","brown"),color=("violet"))+xlab("Month")+ylab("Count of the trip")+ggtitle("Bar plot of month")

# Bar plot month of the day
q <- mdy(df$date)
df$day_month <- day(q)
df$day_month <- as.factor(df$day_month)
ggplot(df,aes(x=day_month))+geom_bar(fill="pink",color=("violet"))+xlab(" Day of the Month")+ylab("Count of the trip")+ggtitle("Bar plot ofday of the month")




#5. histogram for pickup longitude
ggplot(df, aes(x=pickup_longitude)) + geom_histogram(fill="light pink")+xlab("Pick up longitude")+ylab("Count")+ggtitle("Histogram of pickup longitude")

#6. histogram for pickup lattitude


ggplot(df, aes(x=pickup_latitude)) + geom_histogram(fill="orange")+xlab("Pick up latitude")+ylab("Count")+ggtitle("Histogram of pickup latitude")

#7 histogram for dropoff longitude

ggplot(df, aes(x=dropoff_longitude)) + geom_histogram(fill="blue")+xlab("Drop off longitude")+ylab("Count")+ggtitle("Histogram of Drop off longitude")

#8 histogram for dropoff latitude

ggplot(df, aes(x=dropoff_latitude)) + geom_histogram(fill="green")+xlab("Drop off latitude")+ylab("Count")+ggtitle("Histogram of Drop off latitude")

#bar plot of month
df$date <- mdy(df$date)
df$pickup_time <- hm(df$pickup_time)

p1 <- df %>%
  
  mutate(hpick = hour(pickup_time),
         
         Month = factor(month(date, label = TRUE))) %>%
  
  group_by(hpick, Month) %>%
  
  count() %>%
  
  ggplot(aes(hpick, n, color = Month)) +
  
  geom_line(size = 1.5) +
  
  labs(x = "Hour of the day", y = "count")
plot(p1)

p2 <- df %>%
  
  mutate(hpick = hour(pickup_time),
         
         wday = factor(wday(date, label = TRUE))) %>%
  
  group_by(hpick, wday) %>%
  
  count() %>%
  
  ggplot(aes(hpick, n, color = wday)) +
  
  geom_line(size = 1.5) +
  
  labs(x = "Hour of the day", y = "count")

plot(p2)

# finding distance using harvesine formula


add_dist <- function(Data){
  p1 <- as.matrix(Data[c('pickup_longitude', 'pickup_latitude')])
  p2 <- as.matrix(Data[c('dropoff_longitude', 'dropoff_latitude')])
  Data$dist <- distHaversine(p1, p2)
  Data
}

df <- add_dist(df)
# scatter plot of duration and distance

plot(df$dist,df$trip_duration, xlab = "Distance",ylab = "Trip duration",main = "Distance vs Trip duration")

ggplot(df,aes(dist, trip_duration)) +
  
  geom_point() +
  
  scale_x_log10() +
  
  scale_y_log10() +
  
  labs(x = " log(Direct distance [m])", y = "log(Trip duration [s])")+ggtitle("Distance vs Trip duration")
#The distance generally increases with increasing trip_duration
#there are number of trips with very short distances, down to 1 metre, but with a large range of apparent trip_durations

#############filtering of extreme points 
df %>%
  
  filter(trip_duration < 3600 & trip_duration > 120) %>%
  
  filter(dist > 100 & dist < 100e3) %>%
  
  ggplot(aes(dist, trip_duration)) +
  
  geom_bin2d(bins = c(500,500)) +
  
  scale_x_log10() +
  
  scale_y_log10() +
  
  labs(x = "Direct distance [m]", y = "Trip duration [s]")
####################
#Spliting of Data into test data and train data
require(caTools)
set.seed(105) 
sample = sample.split(df[,1], SplitRatio = 0.75)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)
write.csv(train,file = "train.csv")
write.csv(test,file = "test.csv")

#Loading of train data
train <- read.csv("train.csv")
str(train)

# Simple linear regression between distance and duration

simple_fit <- lm(trip_duration~dist,data = train)
summary(simple_fit)

##Assumptions check
par(mfrow=c(1,2))
plot(df$dist,df$trip_duration,main="Plot of duration vs distance",cex.main=1,col.main="blue",col=" dark green",ylab = "Trip duration",xlab = "Distance")
qqnorm(rstudent(simple_fit),main="QQ plot of simple_fit",cex.main=1,col.main="blue",col="red")
abline(0,1)
plot(fitted.values(simple_fit),rstudent(simple_fit),main="Residual plot of simple_fit",cex.main=1,col.main="blue")
## Const variance assumption of the residual satisfied but normality assumption looks  violated
##Hence we transform dist to log(dist) and trip_duration to log(trip_duration)
# while doing log transformation some zero distance values are there so we need to do fix that first
# by adding small constant value 5 to all dist values
train$dist <- train$dist+5
simple_fit2 <- lm(trip_duration~log(dist),data = train)
summary(simple_fit2)
#assumption check of simple_fit 2
qqnorm(rstudent(simple_fit2))
abline(0,1)
plot(fitted.values(simple_fit2),rstudent(simple_fit2),main="Residual plot of simple_fit2")

# for finding the transformation for predictor we can do boxcox transformation

library("MASS", lib.loc="C:/Program Files/R/R-3.5.1/library")
boxcox(simple_fit2,data=df)

# form the box cox model we can see that lambda=0.5 or zero we can do log transfomration or sqaure root tarnsfomation
# log transformation
logy<-log(train$trip_duration)
logx<-log(train$dist)
simple_fit3<-lm(logy~logx)
summary(simple_fit3)

##Assumptions check simple_fit3
plot(logy,logx)
qqnorm(rstudent(simple_fit3))
abline(0,1)
plot(fitted.values(simple_fit3),rstudent(simple_fit3))



##Indicator variable treatment
train$weekdays <-as.factor(train$weekdays)
contrasts(train$weekdays)<-contr.treatment(7,base = 1)
train$title <- as.factor(train$title)
contrasts(train$title)<-contr.treatment(8,base = 1)
train$time_day <- as.factor(train$time_day)
contrasts(train$time_day)<-contr.treatment(24,base = 1)

#linear regression with distance and weekdays as predictor
fit_weekday<-lm(log(trip_duration)~log(dist)+weekdays,data = train)
summary(fit_weekday)
#assumption check
qqnorm(rstudent(fit_weekday))
abline(0,1)
plot(fitted.values(fit_weekday),rstudent(fit_weekday),main="Residual plot with weekdays")

##Regression using holiday and non holiday

fit.holiday <- lm(log(trip_duration)~log(dist)+title,data = train)
summary(fit.holiday)

#Assumption check 
qqnorm(rstudent(fit.holiday))
abline(0,1)
plot(fitted.values(fit.holiday),rstudent(fit.holiday),main = "Residual plot with holiday")

# regression with passenger count, holidays and weekdays,time
#Since month and day of the month has no siginificant effect on traffic we are not considering this variable as predictor
fit.all <- lm(log(trip_duration)~log(dist)+title+time_day+weekdays+passenger_count,data = train)
summary(fit.all)
# assumption check
qqnorm(rstudent(fit.all))
abline(0,1)
plot(fitted.values(fit.all),rstudent(fit.all))

####### Prediction of trip duration in test data using fit.all model
#Structure  of test data
test <- read.csv("test.csv")
str(test)
test$weekdays <-as.factor(test$weekdays)
test$title <- as.factor(test$title)
test$time_day <- as.factor(test$time_day)
yhatlog <- predict(fit.all,test[,c(1:17)])
for(i in 1:length(yhatlog)){
if(yhatlog[i] == -Inf){
  yhatlog[i] <-0
}
}
yhattest <- log(test$trip_duration)
pred_error <- (yhattest-yhatlog)
RMSE <- sqrt(sum(pred_error^2)/nrow(test))
RMSE

#Treatment of outliers



#varaible selection 
train$weekdays <-as.factor(train$weekdays)
contrasts(train$weekdays)<-contr.treatment(7,base = 1)
train$title <- as.factor(train$title)
contrasts(train$title)<-contr.treatment(8,base = 1)
train$time_day <- as.factor(train$time_day)
contrasts(train$time_day)<-contr.treatment(24,base = 1)
#forward varaible selection
fit.0 <- lm(log(trip_duration)~1,data = train)
add1(fit.0,log(trip_duration)~log(dist)+title+time_day+weekdays+passenger_count,data = train,test = "F")

#adding logx
fit.1 <- lm(log(trip_duration)~log(dist),data = df)
add1(fit.1,log(trip_duration)~log(dist)+title+time_day+weekdays+passenger_count,data = train,test = "F")
#adding holiday
fit.2  <- lm(log(trip_duration)~log(dist)+title,data = df)
add1(fit.2,log(trip_duration)~log(dist)+title+time_day+weekdays+passenger_count,data = train,test = "F")
#adding time
fit.3  <- lm(log(trip_duration)~log(dist)+title+time_day,data = df)
add1(fit.3,log(trip_duration)~log(dist)+title+time_day+weekdays+passenger_count,data = train,test = "F")
#Adding weekdays
fit.4  <- lm(log(trip_duration)~log(dist)+title+time_day+weekdays,data = df)
add1(fit.4,log(trip_duration)~log(dist)+title+time_day+weekdays+passenger_count,data = train,test = "F")
#Adding passenger count also getting the model same as fit.all

# backward variable selection
fit.4  <- lm(log(trip_duration)~log(dist)+title+time_day+weekdays+passenger_count,data = df)
drop1(fit.4,log(trip_duration)~log(dist)+title+time_day+weekdays+passenger_count,data = train,test = "F")


#Exhaustive varaible selection
y <- log(train$trip_duration)
x1 <- log(train$dist)
x2 <- train$title
x3 <- train$weekdays
x4 <- train$passenger_count
x5 <- train$time_day
library(leaps)
allexhustive <- regsubsets(data=train,x=cbind(x1,x2,x3,x4,x5), y=y,  method = "exhaustive", all.best = T, nbest = 3)
summary(allexhustive)
Cp <- summary(allexhustive)$cp
AdjR2 <- summary(allexhustive)$adjr2
SSRes <- summary(allexhustive)$rss
R2 <- summary(allexhustive)$rsq
Matrix <- summary(allexhustive)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(25786-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)
colnames(output)[3:7] <- c("x1", "x2", "x3", "x4","x5") 
output
#preiction of trip duration using exhustive model
fit.exhaustive <- lm(log(trip_duration)~log(dist)+title+time_day+weekdays,data = train)
summary(fit.exhaustive)

yhatlog <- predict(fit.exhaustive,test[,c(1:17)])
for(i in 1:length(yhatlog)){
  if(yhatlog[i] == -Inf){
    yhatlog[i] <-0
  }
}
yhattest <- log(test$trip_duration)
pred_error <- (yhattest-yhatlog)
RMSE <- sqrt(sum(pred_error^2)/nrow(test))
RMSE

#residual analyisis
#Checking of leverage point
X <- cbind(1,train$dist)
H <- X%*%(solve(t(X)%*%X))%*%t(X)
p <- (12/nrow(train))
which(diag(H)>p)
diag(H)[diag(H)>p]
order(diag(H)[diag(H)>p])
head(order(diag(H)[diag(H)>p]))

#removal of outlier

newtrain <- subset(train,train$trip_duration <3600 & train$trip_duration>120)
fit.allnew <- lm(log(trip_duration)~log(dist)+title+time_day+weekdays,data =newtrain)
summary(fit.allnew)
qqnorm(rstudent(fit.allnew))
abline(0,1)
plot(fitted.values(fit.allnew),rstudent(fit.allnew))

# filtering based on distance
newtrain2 <-subset(train,train$dist > 100 & train$dist < 100e3)

fit.allnew2 <- lm(log(trip_duration)~log(dist)+title+time_day+weekdays,data =newtrain2)
summary(fit.allnew2)
qqnorm(rstudent(fit.allnew2))
abline(0,1)
plot(fitted.values(fit.allnew2),rstudent(fit.allnew2))


newtest <-subset(test,test$dist > 100 & test$dist < 100e3)
yhatlog <- predict(fit.all,newtest[,c(1:17)])
for(i in 1:length(yhatlog)){
  if(yhatlog[i] == -Inf){
    yhatlog[i] <-0
  }
}
yhattest <- log(newtest$trip_duration)
pred_error <- (yhattest-yhatlog)
RMSE <- sqrt(sum(pred_error^2)/nrow(test))
RMSE













# in weekday one of the weekday has high p value how to deal with that



#confidence interval for estimates
# test for significance anova test or f test
# prediction interval
#do time as chategorical by changing base line and do time as factors and find the difference
# need to make a story on prediction on travel time how itb benifical to cab driver and others normal people
# finding cook distance and mark the outliersor the influential point in different colors
# change the size of dot in the graph
# finding multicolinearity based on dummy varaibles
# making graphs for different categorical predictors and analysis is there any interaction of preditors
# since we are predicting the duration normalization has not that sginificance influence
# by doing sample consist of all varity of data can do cross validation again
# inclusion of manhattan distnce or not







# distance calculation
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}
theta1 <- deg2rad(-28.904)
theta2 <-  deg2rad(28.904)
R1 <- matrix(c(cos(theta1),sin(theta1),-sin(theta1),cos(theta1)),2,2,byrow = T)
R2 <- matrix(c(cos(theta2),sin(theta2),-sin(theta2),cos(theta2)),2,2,byrow = T)
pickup <- cbind(train$pickup_longitude,train$pickup_latitude)
pT <-  R1 %*%t(pickup)
dropoff <- cbind(train$dropoff_longitude,train$dropoff_latitude)
dT = R1 %*% t(dropoff)
vT <- cbind(pT[1,],dT[2,])
v = R2 %*% t(vT)
mid <- t(v)

dist1 <- distHaversine(pickup,mid)
dist2 <- distHaversine(dropoff,mid)
train$newdist <- dist1+dist2
















