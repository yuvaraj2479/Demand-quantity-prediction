library(tidyverse)
library(readr)

data11 = read.csv("S:/colleage/archive/demand.csv")
data11
view(data11)

set.seed(1)
is.na(data11)
!is.na(data11)
view(data11)


colSums(is.na(data11))



colSums(is.na(data11))
data11=data11%>%mutate(lat=ifelse(is.na(lat),"not_available",data11$lat))
data11=data11%>%mutate(long=ifelse(is.na(long),"not_available",data11$long))

colSums(is.na(data11))


str(data11)
boxplot(data11$pop)


boxplot(data11$price)

filter(data11,data11$price>3)
data11=data.frame(data11)
a <- filter(data11,data11$price>2)
a
view(a)
summary(data11)
1.555+1.5*IQR(data11$price)

data111<-subset(data11,data11$price<2)
data111
view(data111)
boxplot(data111$price)
##data111=subset(data11,data11$id<7559,data11$date,data11$city,data11$lat,data$long,data11$pop<664046,data11$shop,data11$brand,data11$container,data$capacity,data11$price<3,)
boxplot(data11$quantity)
table(data11$quantity)
table(data111$capacity)

##EDA
library(ggplot2)
names(data111)
str(data111)
ggplot(data = data111)+geom_point(mapping = aes(x=quantity,y=price,color=container))
ggplot(data = data111)+geom_bar(mapping = aes(brand,fill=brand))
ggplot(data = data111)+geom_point(mapping=aes(x=quantity,y=price,color=capacity>1.5))
ggplot(data = data111)+geom_histogram(mapping = aes(fill=city,price),position = "dodge")

##model
set.seed(1)


train1=sample(nrow(data111),nrow(data111)*0.7)
train=data111[train1,]
test=data111[-train1,]


library(tree)
model1=tree(quantity~.,data = train)

summary(model1)

plot(model1)
text(model1,pretty = 0)


cv=cv.tree(model1)
plot(cv$size,cv$dev,type = "l")
cv$size

minimum<- which.min(cv$dev)
minimum

points(cv$size[minimum],cv$dev[minimum],col="red")

pred=predict(model1,newdata = test)
pred

range(pred)

err=mean((pred-test$quantity)^2)
tss=mean((test$quantity-mean(pred))^2)
tss
rss=1-err/tss
rss
1-0.3803092


##random forest
library(randomForest)
dim(data11)
set.seed(1)
rfm=randomForest(train$quantity~.,data = train,mtry=6,ntree=500,importance=TRUE)
summary(rfm)



plot(rfm)


predrfm=predict(rfm,newdata = test)
predrfm

range(predrfm)

plot(predrfm,test$quantity)
abline(0,1)


err=mean((predrfm-test$quantity)^2)
tss=mean((test$quantity-mean(predrfm))^2)
tss
rss=1-err/tss
rss
1-0.768681






