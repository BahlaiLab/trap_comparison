###########Simulate Data
x <- seq(from=80, to=101,by=0.1)  ##make temperature
temp <- sample(x,size=30,replace=TRUE)
a <- 0.5
b <- 2.1
e <- rnorm(30,0,1.5)
y1  <- rep(NA, 30)
for(i in 1:30){
  y1[i] <- (a+b*temp[i])+e[i]
}
a2 <- 0.1
b2 <- 0.2
e2 <- rnorm(30,1,3)
y2  <- rep(NA, 30)
for(i in 1:30){
  y2[i] <- (a2+b2*temp[i])+e2[i]
}



CombineData <- cbind(temp,y1,y2)
colnames(CombineData) <- c("Temperature","Species1","Species2")
setwd("C:/Users/tfranzem/Desktop/KentStateResearch/CerambycidaeTrapping")
write.csv(CombineData,file="FakeData.csv")

CombineData <- as.data.frame(CombineData)

##############below is the analysis we want the students to do
hist(CombineData$Species1)
hist(CombineData$Species2)
t.test(y1,y2,data=CombineData)

model1 <- lm(log(CombineData$Species1)~log(CombineData$Temperature))
summary(model1)
plot(model1)

model2 <- lm(log(CombineData$Species2)~log(CombineData$Temperature))
summary(model2)
plot(model2)


plot(x=log(CombineData$Temperature),y=log(CombineData$Species1),ylab="Species 1 Abundance", xlab="Temperature")
abline(model1)
plot(x=CombineData$Temperature,y=CombineData$Species2,ylab="Species 2 Abundance", xlab="Temperature")
abline(model2)

model3 <- lm(Species1~Species2,CombineData)
summary(model3)
plot(model3)
plot(x=CombineData$Species2,y=CombineData$Species1,ylab="Species 1 Abundance", xlab="Species 2 Abundance")
abline(model3)

model4 <- lm(Species2~Species1,CombineData)
summary(model4)
plot(model4)

plot(x=CombineData$Species1,y=CombineData$Species2,ylab="Species 2 Abundance", xlab="Species 1 Abundance")
abline(model4)
