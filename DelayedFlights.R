
setwd("/Users/abigailatchison/Documents/StatModels/FinalProject/")
dat <- read.csv("DelayedFlights.csv")

sum <- dat$ArrDelay + dat$DepDelay
histinfo <- hist(sum)
#hist

histDen <- hist(sum, breaks = c(-61,0,15,45,120,240,5000), plot=TRUE)

histDen

#histDen <- hist(sum, breaks = c(-61,-5,6,26,61,121,5000), plot=TRUE)
#-61--5
# -5-6  
# 6-26
# 26-61
# 61-121
# 121+

DELAY <- cut(sum, c(0,15,45,120,Inf), labels = c(0,1,2,3))
names(dat)
datFormatted.all <- cbind(DELAY,dat[,c(0,2:14,18:25,31,32)])
names(datFormatted.all.nona)
# freq1 <- table(datFormatted.all$DELAY)
# freq1

datFormatted.all.nona <- na.omit(datFormatted.all)

datFormatted.Final <- datFormatted.all.nona[!datFormatted.all.nona$DELAY == 3,]
datFormatted.Final[,1]

datFormatted.all.nona <- datFormatted.Final

# FACTOR OUT QUALITATIVE VARIABLES
# Factored out Airline carrier variables
UniqueCarrier. <- factor(datFormatted.all.nona$UniqueCarrier)
UniqueCarrier.frame <- data.frame(model.matrix(~ UniqueCarrier. - 1))

# Factored out region variables 
OriginRegion. <- factor(datFormatted.all.nona$OriginRegion)
OriginRegion.frame <- data.frame(model.matrix(~ OriginRegion. - 1))

DestRegion. <- factor(datFormatted.all.nona$DestRegion)
DestRegion.frame <- data.frame(model.matrix(~ DestRegion. - 1))

# Origin. <- factor(datFormatted.all.nona$Origin)
# Origin.frame <- data.frame(model.matrix(~ Origin. - 1))
# 
# Dest. <- factor(datFormatted.all.nona$Dest)
# Dest.frame <- data.frame(model.matrix(~ Dest. - 1))

# Bind factored variables with dataset containing no NA values
datFormatted.bind <- cbind(datFormatted.all.nona, UniqueCarrier.frame, OriginRegion.frame, DestRegion.frame)

names(datFormatted.bind)

# Select columns needed for analysis 
datFormatted <- datFormatted.bind[,c(1:8,17:20,22,25:58)] # dataset with UniqueCarrier factored out. Other delays (WeatherDelay, SecurityDelay etc.)

names(datFormatted)

freq <- table(datFormatted$DELAY)
freq

# 0      1      2      3      4      5 
# 0 141174 327276 321942      0      0 

sapply(datFormatted,class)
delay <- as.numeric(datFormatted$DELAY)-1
delay

datFormatted <- cbind(delay,datFormatted[,2:47])

names(datFormatted)

freq2 <- table(datFormatted$delay)
freq2

# Collect Sample & Validation datasets

group.0 <- subset(datFormatted, datFormatted[,1] == 0)
group.1 <- subset(datFormatted, datFormatted[,1] == 1)
group.2 <- subset(datFormatted, datFormatted[,1] == 2)
#group.3 <- subset(datFormatted, datFormatted[,1] == 3)
#group.4 <- subset(datFormatted, datFormatted[,1] == 4)
#group.5 <- subset(datFormatted, datFormatted[,1] == 5)
# group.6 <- subset(datFormatted, datFormatted[,1] == 6)
# group.7 <- subset(datFormatted, datFormatted[,1] == 7)
# group.8 <- subset(datFormatted, datFormatted[,1] == 8)
# group.9 <- subset(datFormatted, datFormatted[,1] == 9)

train.0 <- sample(nrow(group.0), 50000)
train.1 <- sample(nrow(group.1), 50000)
train.2 <- sample(nrow(group.2), 50000)
#train.3 <- sample(nrow(group.3), 50000)
#train.4 <- sample(nrow(group.4), 5000)
#train.5 <- sample(nrow(group.5), 5000)
# train.6 <- sample(nrow(group.6), 5000)
# train.7 <- sample(nrow(group.7), 5000)
# train.8 <- sample(nrow(group.8), 5000)
# train.9 <- sample(nrow(group.9), 5000)

train.data <- rbind(group.0[train.0,], group.1[train.1,], group.2[train.2,])
               # group.6[train.6,], group.7[train.7,], group.8[train.8,], group.9[train.9,],group.4[train.4,], group.5[train.5,], , group.3[train.3,]

validation <- rbind(group.0[-train.0,], group.1[-train.1,], group.2[-train.2,])
               #group.6[-train.6,], group.7[-train.7,], group.8[-train.8,], group.3[-train.3,], group.9[-train.9,]), , group.4[-train.4,], group.5[-train.5,]


sub.0 <- subset(validation, validation[,1] == 0)
sample.0 <- sample(nrow(sub.0), 50000)
val.0 <- sub.0[sample.0,]

sub.1 <- subset(validation, validation[,1] == 1)
sample.1 <- sample(nrow(sub.1), 50000)
val.1 <- sub.1[sample.1,]

sub.2 <- subset(validation, validation[,1] == 2)
sample.2 <- sample(nrow(sub.2), 50000)
val.2 <- sub.2[sample.2,]

# sub.3 <- subset(validation, validation[,1] == 3)
# sample.3 <- sample(nrow(sub.3), 50000)
# val.3 <- sub.3[sample.3,]

# sub.4 <- subset(validation, validation[,1] == 4)
# sample.4 <- sample(nrow(sub.4), 5000)
# val.4 <- sub.4[sample.4,]
# 
# sub.5 <- subset(validation, validation[,1] == 5)
# sample.5 <- sample(nrow(sub.5), 5000)
# val.5 <- sub.5[sample.5,]

# sub.6 <- subset(validation, validation[,1] == 6)
# sample.6 <- sample(nrow(sub.6), 5000)
# val.6 <- sub.6[sample.6,]
# 
# sub.7 <- subset(validation, validation[,1] == 7)
# sample.7 <- sample(nrow(sub.7), 5000)
# val.7 <- sub.7[sample.7,]
# 
# sub.8 <- subset(validation, validation[,1] == 8)
# sample.8 <- sample(nrow(sub.8), 5000)
# val.8 <- sub.8[sample.8,]
# 
# sub.9 <- subset(validation, validation[,1] == 9)
# sample.9 <- sample(nrow(sub.9), 5000)
# val.9 <- sub.9[sample.9,]

validation.data <- rbind(val.0, val.1, val.2) #, val.4, val.5, val.6, val.7, val.8, val.9)

table(train.data$delay)
table(validation.data$delay)


mean(as.numeric(validation.data[,1]))


# K N N

train.x <- train.data[,c(2:22)]
train.y <- train.data[,1]
validation.x <- validation.data[,c(2:22)]
validation.y <- validation.data[,1]

train.xx <- na.omit(train.x)
train.yy <- na.omit(train.y)
validation.xx <- na.omit(validation.x)
validation.yy <- na.omit(validation.y)

library(class)
for (i in seq(1,75,2)){
  print(i)
  temp <- table(validation.yy, knn(train.xx, validation.xx, train.yy, i))
  print((temp[1,2] + temp[2,1]) / sum(temp))
}


# L I N E A R  R E G R E S S I O N
names(train.data)
reg1 <- lm(delay ~., data = train.data)
summary(reg1)


#Tree
library(tree)
tree.train <- tree(delay~., train.data)
summary(tree.train)

library(rpart)
classTreeAll <- rpart(delay~., data = train.data, method = "class")

summary(classTreeAll)
classTreeAll
plot(classTreeAll)
text(classTreeAll, use.n=TRUE, all=TRUE, cex=.8)

printcp(classTreeAll)

pred <- predict(classTreeAll, newdata = validation.data, type = 'class')
pred

install.packages('mda')
library(mda)
CM = confusion(pred,validation.data$delay)
CM

