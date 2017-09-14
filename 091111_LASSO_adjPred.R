setwd('C:/Users/User/Desktop/bigcontest/data') #디렉토리 설정
data_set <- read.table('Data_set_number.csv', header = T, sep = ',', stringsAsFactors= F) #데이터 불러오기 



####################자료형에 적절한 변수처리######################
data_set = data_set[,-1] #CUST_ID 제거 
data_set$X1 = as.factor(data_set$X1) #Target factor처리
data_set$X16 = as.factor(data_set$X16) #직업 factor처리
table(data_set$X21) #막내자녀나이가 numeric이지만, 숫자로된 범주일 수도 있어 table을 통한 빈도 파악
data_set$X21[data_set$X21 == 'NULL'] = 0 #막내자녀나이 NULL을 0로 처리. 
data_set$X21 = as.numeric(data_set$X21) #막내자녀나이 numberic처리
data_set$X22 = as.factor(data_set$X22) #배우자직업 factor처리
table(data_set$X25) #table을 통해 년과 월단위로 구성되어 있음을 파악
#최초대출날짜 변수를 년도별로 축소
data_set = transform(data_set, X25 = cut(X25, breaks = c(0,199909, 200001, 200101, 200201, 200301, 200401, 200501, 200601, 200701, 200801, 200901, 201001, 201101,201201,201301,201401,201501,201601, 201701),
                                         right = F, labels = c("0","1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")))

data_set$X25 = as.factor(data_set$X25) #년도별로 축소한 최초대출날짜를 facotr처리
data_set$X31 = as.factor(data_set$X31) #최초신용등급 facotr처리 (NULL또한 범주로 처리)
data_set$X32 = as.factor(data_set$X32) #최근신용등급 factor처리 (NULL또한 범주로 처리)
#10단위로 구성된 보험연체율 numeric 처리
data_set$X34[data_set$X34 == '10미만'] = 10; data_set$X34[data_set$X34 == '20미만'] = 20
data_set$X34[data_set$X34 == '30미만'] = 30; data_set$X34[data_set$X34 == '40미만'] = 40
data_set$X34[data_set$X34 == '50미만'] = 50; data_set$X34[data_set$X34 == '60미만'] = 60
data_set$X34[data_set$X34 == '90미만'] = 90; data_set$X34[data_set$X34 == '90이상'] = 100
data_set$X34 = as.numeric(data_set$X34) #보험연체율 numeric 처리
data_set$X52 = as.factor(data_set$X52) #5단위씩 증가하는 나이를 factor처리 (연령대별 정보 도출이 목적이므로, 정확하지 않은 값을 numeric 처리할 이유 없음)
data_set$X53 = as.factor(data_set$X53) #성별 factor (NULL 존재시 범주처리)
data_set$X56 = as.factor(data_set$X56) #멤버쉽등급 factor (NULL 존재시 범주처리)
data_set$X59 = as.factor(data_set$X59) #결합상품가입여부 factor(NULL또한 범주처리)
data_set$X66 = as.factor(data_set$X66) #납부방법 factor (NULL 존재시 범주처리)
data_set$X67 = as.factor(data_set$X67) #회선상태 factor (NULL 존재시 범주처리)

###################DATA분리(oversampling)####################################
#Train data에서 타겟을 같은 비율로 샘플링하는 oversampling 적용. (oversampling 미적용시, 해당 방법으로는 타겟 분류 불가했음)
set.seed(1)
data_set_1 = data_set[data_set$X1 == 1,] #Target 1 추출
data_set_0 = data_set[data_set$X1 == 0,] #Target 2 추출
train_1 = sample(length(data_set_1[,1]), 3001, replace = F) #전체 Target 1중 70% 해당하는 3001개 추출을 위해 관측치 넘버 랜덤 추출
train_0 = sample(length(data_set_0[,1]), 3001, replace = F) #Target 0을 Target 1과 수를 맞추기 위해 3001개 추출을 위해 관측치 넘버 랜덤 추출
data_train_1 = data_set_1[train_1,] #랜덤한 3001개 관측치 번호로 Target 1으로 train set 생성
data_train_0 = data_set_0[train_0,] #위와 동일하게 3001개의 Target 0으로 train set 생성
data_train = rbind(data_train_1, data_train_0) #6002개의 Target 0과 1로 train set 생성

data_test_0 = data_set_0[-train_0,] #Train set을 제외한 Target 0의 관측치 추출
data_test_1 = data_set_1[-train_1,] #Train set을 제외한 Target 1의 관측치 추출

data_test_0 = data_test_0[sample(length(data_test_0[,1]), 28770, replace = F),] #모집단의 Target 비율에 맞춰, Target 0을 28770개 추출

data_test = rbind(data_test_0, data_test_1) #Test set 생성
###################LASSO 적합##################################
#lambda 랜덤 추출 범위와, 모델 평가 척도를 다르게 하여 lasso 실행

#1번 gird(5, -5), mae, alpha(1)
x = model.matrix(X1~., data_train)
y = data_train$X1
grid = 10^seq(5, -5, length = 100)
library(glmnet)
cv.out = cv.glmnet(x, y, alpha = 1, lambda = grid, family = 'binomial', type.measure = 'mae')
plot(cv.out)
bestlambda = cv.out$lambda.min
bestlambda


fit = glmnet(x, y, alpha = 1, lambda = bestlambda, family = 'binomial')
x_test = model.matrix(X1~., data_test)
y_test = data_test$X1
pred = predict(fit, newx = x_test, type = "response", s = bestlambda ) #s는 lambda에 따른 fitting 결과


#2번 grid(10, -10), mae, alpha(1)

x = model.matrix(X1~., data_train)
y = data_train$X1
grid2 = 10^seq(10, -10, length = 100)
library(glmnet)
cv.out2 = cv.glmnet(x, y, alpha = 1, lambda = grid2, family = 'binomial', type.measure = 'mae')
plot(cv.out2)
bestlambda2 = cv.out2$lambda.min
bestlambda2

fit2 = glmnet(x, y, alpha = 1, lambda = bestlambda2, family = 'binomial')

x_test = model.matrix(X1~., data_test)
y_test = data_test$X1
pred2 = predict(fit2, newx = x_test, type = "response", s = bestlambda2 ) #s는 lambda에 따른 fitting 결과


#3번 grid(10,5), mae, alpha(1)
x = model.matrix(X1~., data_train)
y = data_train$X1
grid3 = 10^seq(10, 5, length = 100)
library(glmnet)
cv.out3 = cv.glmnet(x, y, alpha = 1, lambda = grid3, family = 'binomial', type.measure = 'mae')
plot(cv.out3)
bestlambda3 = cv.out3$lambda.min
bestlambda3

fit3 = glmnet(x, y, alpha = 1, lambda = bestlambda3, family = 'binomial')

x_test = model.matrix(X1~., data_test)
y_test = data_test$X1
pred3 = predict(fit3, newx = x_test, type = "response", s = bestlambda3 ) #s는 lambda에 따른 fitting 결과




#4번 grid(-20,-10), mae, alpha(1)
x = model.matrix(X1~., data_train)
y = data_train$X1
grid4 = 10^seq(-10, -20, length = 100)
library(glmnet)
cv.out4 = cv.glmnet(x, y, alpha = 1, lambda = grid4, family = 'binomial', type.measure = 'mae')
plot(cv.out4)
bestlambda4 = cv.out4$lambda.min
bestlambda4

fit4 = glmnet(x, y, alpha = 1, lambda = bestlambda4, family = 'binomial')

x_test = model.matrix(X1~., data_test)
y_test = data_test$X1
pred4 = predict(fit4, newx = x_test, type = "response", s = bestlambda4 ) #s는 lambda에 따른 fitting 결과



#5번 grid(5,-5), auc, alpha(1)
x = model.matrix(X1~., data_train)
y = data_train$X1
grid5 = 10^seq(5, -5, length = 100)
library(glmnet)
cv.out5 = cv.glmnet(x, y, alpha = 1, lambda = grid5, family = 'binomial', type.measure = 'auc')
plot(cv.out5)
bestlambda5 = cv.out5$lambda.min
bestlambda5

fit5 = glmnet(x, y, alpha = 1, lambda = bestlambda5, family = 'binomial')

x_test = model.matrix(X1~., data_test)
y_test = data_test$X1
pred5 = predict(fit5, newx = x_test, type = "response", s = bestlambda5 ) #s는 lambda에 따른 fitting 결과




#평가
target = c()
target[pred>0.818] = 1
target[pred<=0.818] = 0
result <- table(y_test, target)
Precision <- result[4] / (result[3] + result[4])
Recall <- result[4] / (result[2] + result[4])
f.value <- 2 * (Precision * Recall) / (Precision + Recall)


target2 = c()
target2[pred2>0.818] = 1
target2[pred2<=0.818] = 0
result <- table(y_test, target2)
Precision <- result[4] / (result[3] + result[4])
Recall <- result[4] / (result[2] + result[4])
f.value2 <- 2 * (Precision * Recall) / (Precision + Recall)


target3 = c()
target3[pred3>0.4] = 1
target3[pred3<=0.4] = 0
result <- table(y_test, target3)
Precision <- result[2] / (result[1] + result[2])
Recall <- result[2] / (result[2])
f.value3 <- 2 * (Precision * Recall) / (Precision + Recall)

target4 = c()
target4[pred4>0.828] = 1
target4[pred4<=0.825] = 0
result <- table(y_test, target4)
Precision <- result[4] / (result[3] + result[4])
Recall <- result[4] / (result[2] + result[4])
f.value4 <- 2 * (Precision * Recall) / (Precision + Recall)

target5 = c()
target5[pred5>0.83] = 1
target5[pred5<=0.83] = 0
result <- table(target5, y_test)
Precision <- result[4] / (result[3] + result[4])
Recall <- result[4] / (result[2] + result[4])
f.value5 <- 2 * (Precision * Recall) / (Precision + Recall)


#F값 비교
f.value
f.value2
f.value3
f.value4
f.value5

#Beta값 비교
beta_table =  cbind(fit$beta, fit2$beta, fit3$beta, fit4$beta, fit5$beta)


#모집단의 비율만큼 random Selection한 경우의 정확
tar = rep(0, 30056)
n = sample(30056, 14652, replace = F)
tar[n] = 1
result = table(y_test, tar)
result
