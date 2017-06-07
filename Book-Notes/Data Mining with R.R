
## Example from Chap 2 using a LM to predict algae levels
library(DMwR)
head(algae)

## According to the book, we'll need to clean up the data when using a LM
algae[manyNAs(algae),]
algae.clean <-  algae[-manyNAs(algae),]
nrow(algae.clean)

lm.a1 <- lm(a1 ~ ., data=algae.clean[,c(1:11,12)])
#lm.a1
summary(lm.a1)


## Creating a tree using the rpart package
library(rpart)
tree <- rpart(a1 ~ ., data=algae.clean[,1:12])
tree

## Visualizing the tree
plot(tree)
text(tree)
prettyTree(tree)

summary(tree)  # outputs a crazy amount of information on the tree


## From the stock market simulation
p.avg <- with(GSPC, (High+Low+Close)/3)
head(p.avg)

##  Based on figure 3.5
## Where i is the current date and h is the number of days (so i-h is h days ago)
## i is a lubridate date represented as an integer in the following format 19700105
library(lubridate)

## todo:  should be memoized at some point
Close <- function(i, h=0) as.numeric(GSPC$Close[ymd(i)-ddays(h)])
ret <- function(i, h) (Close(i)-Close(i,h))/Close(i)
ret.log <- function(i, h) log(Close(i)/Close(i,h))


ndays <- 10
maxh <- 10
m <- matrix(NA, nrow=ndays, ncol=maxh)  
# add rownames = to the date later
# add colnames = to the h values
head(m)

h = 2
start = 19700102
for (i in (start+h+1):(start+ndays)) m[i,h] = ret(i,h)

##Running into issues with indexing into the dates... not every day is a trading day
for (i in (start+h+1):(start+ndays)) print(sprintf("i=%d, h=%d, ret=%f", i, h, ret(i,h)))



###################################################################
## From the chapter that shows you have to create a NNet and SVM ##
###################################################################

pts=300
bottom <- data.frame(x=rnorm(n=pts, mean=0, sd=1), y=rnorm(n=pts, mean=0, sd=2), col=4, type=20)
top <- data.frame(x=rnorm(n=pts, mean=5, sd=2), y=rnorm(n=pts, mean=5, sd=1), col=2, type=10)
train <- rbind(top,bottom)

tmp <- sample(nrow(train))
test <- train[tmp[1:pts],]
train <- train[tmp[(pts+1):nrow(train)],]

p <- function(df) plot(df[,1:2], col=df$col, pch=df$type)
p(train)
p(test)

## Using a neural network
nn <- nnet(type~x+y, train, size=20)
predict(nn, test$x, test$y)

## Using a SVM
sv <- svm(type~x+y, train)
tmp <- test
tmp$type <- predict(sv, test)
tmp$col <- sapply(tmp$type, color.pts, eps=3)
tmp$type = 8
tmp <- rbind(test,tmp)
p(tmp)

color.pts <- function(n, eps=2) {
  color <- 'yellow'
  if (abs(n-20) <= eps) color = 4
  if (abs(n-10) <= eps) color = 2
  color
}

range_per_kwh <- function(range, kwh) range / kwh

cost_per_mile <- function(wh_per_mi, kwh_cost=.12)  wh_per_mi * (kwh_cost/1000)

fusion_wh_per_mile <- 7000 / 21
  
  
  
