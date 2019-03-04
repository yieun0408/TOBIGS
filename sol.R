###################
##주택경매 데이터##
###################
rm(list=ls())

setwd("C:/Users/laep9/Desktop")

#불러오기aq2@qaaa2q
df = read.csv("Auction_master_train.csv", stringsAsFactors = T, fileEncoding="utf-8")
str(df)

#regist = read.csv("Auction_regist.csv", stringsAsFactors = T, fileEncoding="utf-8")
#rent = read.csv("Auction_rent.csv", stringsAsFactors = T, fileEncoding="utf-8")
#result = read.csv("Auction_result.csv", stringsAsFactors = T, fileEncoding="utf-8")


#결측치 확인
colSums(is.na(df)) #road_bunji2에서 결측치 1778
#변수명을 확인했더니, 도로명 주소.

#필요 없는 변수 제거
df2 = subset(df, select = -c(road_bunji1, road_bunji2, Auction_key, addr_bunji2,
                             addr_li, addr_san, addr_bunji1, addr_etc, road_name,
                             point.y, point.x))
str(df2)

#날짜형으로 바꿔주기
df2$Appraisal_date = as.Date(df2$Appraisal_date)
df2$First_auction_date = as.Date(df2$First_auction_date)
df2$Final_auction_date = as.Date(df2$Final_auction_date)
df2$Close_date = as.Date(df2$Close_date)
df2$Preserve_regist_date = as.Date(df2$Preserve_regist_date)
str(df2)

colSums(is.na(df2))
unique(df2$Close_result) #여백과 배당으로만 되어있네


#시각화
#1. 회사와 집값 사이에 관계가 있을까?적
library(ggplot2)
plot(df2$Appraisal_company, df2$Hammer_price)
#딱히 없어보인다.

#2. 서울과 부산의 집값차이가 있을까?
plot(df2$addr_do, df2$Hammer_price)
#서울이 부산보다는 높다 

#3. Apartment_usage와 집값과의 관계
plot(df2$Apartment_usage, df2$Hammer_price)

#4. 연속변수들만 뽑아내서 종속변수와의 상관성을 보자
#cor()
land.building = subset(df2, select = c(Total_land_gross_area, Total_land_real_area, 
                                       Total_land_auction_area,
                                       Total_building_area, 
                                       Total_building_auction_area, 
                                       Total_appraisal_price,
                                       Minimum_sales_price))

cor(land.building)
plot(land.building)
#총토지경매면적, 총건물면적, 총건물경매면적, 총감정가, 최저매각가격끼리의 
#상관성이 높게 나타남 = 경매에 있어서 중요한 것은 건물과 관련

#5. 최종입찰가격 및 최소 가격과 입찰가격간의 관계


##############파생변수
#파생변수 1: Final_auction_date - First_auction_date = 경매 진행일수
df2$during = df2$Final_auction_date - df2$First_auction_date

#파생변수 2: per_height (현재 층수 /건물 층수)
df2$per_height = df2$Current_floor/df2$Total_floor

#파생변수 3: k-means 클러스터링 -> 위도 변수에 가격을 추가해를 그 안에서 클러스터 만들기 
#이걸 새로운 변수로 할당! 
colSums(is.na(df2))
add.do = subset(df, select = c(point.x, point.y, Hammer_price))
add.do.scaled = scale(add.do)

#적당한 k를 구해볼까~
wss = 0 #반드시 설정해주어야한다
for(i in 1:20) wss[i] = sum(kmeans(add.do.scaled, centers = i)$withinss)

plot(1:20, wss, type = "b", 
     xlab = "Number of Clusters", ylab = "Within group sum of squares")

#k를 6로 정함
add.kmeans <- kmeans(add.do.scaled, centers = 6, nstart = 1)
add.kmeans$cluster
plot(add.do.scaled, col = add.kmeans$cluster)
points(add.kmeans$centers,  col = 1:6, pch = 8, cex = 2)

df2$cluster <- add.kmeans$cluster

#다시한번 필요없는 변수 제거
colnames(df2)
df3 = subset(df2, select = -c(Specific, Appraisal_company, Creditor,
                              First_auction_date, Final_auction_date,
                              Appraisal_date, addr_si, addr_do, addr_dong,
                              Apartment_usage,Close_date, Preserve_regist_date,
                              Total_land_gross_area, Total_land_real_area,
                              Total_land_auction_area,Final_result))
str(df3)
colnames(df3)


#라벨 붙이기
median(df3$Hammer_price)
df3$label = ifelse(df3$Hammer_price >= median(df3$Hammer_price),
                   2,1)

df3$label = as.factor(df3$label)
df4 = subset(df3, select = -c(Hammer_price))

df4$cluster = as.factor(df4$cluster)

#################데이터 train, test 분리 7:3
#install.packages("caret")
library(caret)
idx <- createDataPartition(y = df4$label, p = 0.7, list =FALSE)

#7:3으로 나눠라
train<- df4[idx,]
test <- df4[-idx,]


###중요 변수 선정
library(randomForest)
?randomForest
rf = randomForest(label~., data = train)
importance(rf)
varImpPlot(rf)

#중요한 얘들만 뽑자
df_rf = subset(df4, select = c(Claim_price, Total_building_area,
                                    Total_building_auction_area, 
                                    Total_appraisal_price,
                                    Minimum_sales_price,
                                    cluster, label))
table(df_rf$cluster)


##############knn
#거리 조정 스케일링
## min-max 스케일링
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

df_rf_n <- as.data.frame(lapply(df_rf[-c(7:8)], normalize))
summary(df_rf_n)

df_rf_n$cluster <- df_rf$cluster

df_rf_n$label = df_rf$label
colnames(train_rf_n)
str(train_rf_n)

set.seed(1)
idx <- createDataPartition(y = df_rf_n$label, p = 0.7, list =FALSE)

#7:3으로 나눠라
train_n<- df_rf_n[idx,]
test_n<- df_rf_n[-idx,]

#최적의 k 찾기
#그리드 서치 cv
cv <- trainControl(method = "cv", number = 5, verbose = T)

knn.grid = expand.grid(
  .k = c(1,3,5,7,9,11,13,15)
)

train.knn <- train(label~.,train_n, method = "knn",trControl = cv,
                   tuneGrid = knn.grid)
train.knn$results
train.knn$bestTune #9
predict.knn <- predict(train.knn, test_n)
confusionMatrix(predict.knn, test_n$label) #Accuracy : 0.9655 

########svm
library(e1071)
?svm
str(train_n)
svm = tune.svm(label~., data = train_n, cost=10:100,gamma=seq(0,3,0.1))
svm$best.parameters #gamma:0.4, cost:34

svm_tune = svm(label~., data =train_n, cost = 34, 
               gamma = 0.4, kernel ="radial") 
summary(svm_tune)

svm_tune$degree
svm_tune$index

#정확도
svm_predict = predict(svm_tune, test_n[,-8])
confusionMatrix(svm_predict, test_n$label) #Accuracy : 0.9724

#####나이브베이즈
nb 

set.seed(1)
idx <- createDataPartition(y = df_rf$label, p = 0.7, list =FALSE)

#7:3으로 나눠라
train<- df_rf[idx,]
test<- df_rf[-idx,]

nb = naiveBayes(label~., data = train, laplace = 1)
nb

nb_predict = predict(nb, test[,-8])
confusionMatrix(nb_predict, test$label) #Accuracy : 0.9309


############로지스틱
#더미변수 만들기
dm = dummyVars('~cluster', df_rf)
dm = data.frame(predict(dm,df_rf))

df_rf_dummy = cbind(df_rf, dm)
colnames(df_rf_dummy)

#원변수 인 trip 삭제
df_rf_dummy2 = subset(df_rf_dummy, select = -cluster)


set.seed(1)
idx <- createDataPartition(y = df_rf_dummy2$label, p = 0.7, list =FALSE)

#7:3으로 나눠라
train_dm<- df_rf_dummy2[idx,]
test_dm<- df_rf_dummy2[-idx,]

str(train_dm)

glm = glm(label~., data = train_dm, family=binomial)
summary(glm)


glm_predict = predict(glm, test_dm[,-6], type = "response")
pred_threshold2<-as.factor(ifelse(glm_predict>=0.5,2,1))

table(pred_threshold2, test_dm$label)

mean(pred_threshold2==test$label) #0.970639


#roc그래프
pr = prediction(glm_predict, test_dm$label)
