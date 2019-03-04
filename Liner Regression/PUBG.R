#####################################
##배틀그라운드 유저 데이터 회귀분석##
#####################################
rm(list=ls())

#게임유저들의 승률을 예측해보자!

#1. 작업공간 지정해주기
setwd("C:/Users/laep9/Desktop/school/PUBG")

#2. 데이터 불러오기
install.packages("car")
library(car)
data = read.csv("df.csv")

#3. 데이터 확인해보기
head(data)
str(data)

colSums(is.na(data)) #결측치가 없습니다 ~^.^~
#결측치가 있으면, 분석자의 주관에 따라 버리거나/평균/중앙값/최빈값을 넣기도 합니다.

#4. 필요없는 열 제거: 2가지 방법
data2 = subset(data, select = -c(X, Id, groupId, matchId)) #저는 이걸 더 선호합니다.

data3 = data[,-c(1:4)]
rm(data3)

str(data2)
#5. eda 실시 - 여기서는 넘어갈게용
#hist(data2$변수)
#plot(data2)
#boxplot(data2$변수)

#일단 회귀분석 돌려보겠습니다.
lm = lm(winPlacePerc ~ ., data = data2)

#회귀분석의 결과를 보려면 summary함수를 써야합니다.
summary(lm)
#Multiple R-squared:  0.8381,	Adjusted R-squared:  0.8379 
# p-value: < 2.2e-16
#모델 유의! 설명력도 나쁘지 않음!

#데이터 train, test 분리 7:3
#install.packages("caret")
library(caret)
idx <- createDataPartition(y = data2$winPlacePerc, p = 0.7, list =FALSE)

#7:3으로 나눠라
train<- data2[idx,]
test <- data2[-idx,]


#전진선택법, 후진제거법, 단계적회귀
#설명변수를 넣지않은 모델
fit.con <- lm(winPlacePerc ~ 1, train)
#다 적합한 모델
fit.full <- lm(winPlacePerc~., train)

# 전진선택법
fit.forward <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "forward")
# 후진제거법
fit.backward <- step(fit.full, list(lower=fit.con, upper = fit.full), direction = "backward")
# 단계적회귀방법(stepwise)
fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "both")

summary(fit.forward) # Multiple R-squared:  0.8356,	Adjusted R-squared:  0.8354  
summary(fit.backward) # Multiple R-squared:  0.8365,	Adjusted R-squared:  0.8363 
summary(fit.both) # Multiple R-squared:  0.8356,	Adjusted R-squared:  0.8354 

#이상치 확인
library(car)
#outlierTest(model, ...)
outlierTest(fit.backward, order=TRUE)


#################################test에 대입
predicted <- predict(fit.backward, test[ ,-26])
y <- test[,26]
RMSE(predicted,y) #0.1238298
#낮을수록 좋습니다:)

#r^2값
1 - sum((y-predicted)^2)/sum((y-mean(y))^2) #0.8372568

######### cross validation
library(caret)
cv <- trainControl(method = "cv", number = 5, verbose = T)

######### lm
train.lm <- train(winPlacePerc~.,train, method = "lm", trControl =cv)
predict.lm <- predict(train.lm,test[,-26])
y <- test[,26]
RMSE(predict.lm,y) #0.1235363

