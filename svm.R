##########################################################################################
##########################################################################################
########################################## SVM ###########################################
##########################################################################################
##########################################################################################


if(!require(e1071)){install.packages('e1071') ; library(e1071)}


#svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
#      "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
#    coef0 = 0, cost = 1, nu = 0.5,
#    class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
#    shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,
#    ..., subset, na.action = na.omit)

# scale = TRUE가 기본, 따로 x를 scale안해도 됨
# kernel : kernel함수를 결정
# cross	: if a integer value k>0 is specified, a k-fold cross validation 
# on the training data is performed to assess the quality of the model:
# cost : 과적합을 막는 정도, 잘못 분류하면 얼마만큼의 비용을 지불할 것인지 결정


##########################################################################################


# svm기본모델(1)을 추정하고 kernel 함수에 따라 3가지 모델(2, 3, 4)을 추정


##########################################################################################
#################################### 1. svm기본모형 ######################################

# SVM모형 추정
svm1 <- svm(TARGET~., data = data_Train2)

# 추정한 모형으로 data_Test2의 TARGET 예측
svm1.pred <- predict(svm1,data_Test2)

# F measure 계산
table(svm1.pred)
table(data_Test2$TARGET)
result1 <- table(data_Test2$TARGET, svm1.pred)
Precision1 <- result1[4] / (result1[3] + result1[4])
Recall1 <- result1[4] / (result1[2] + result1[4])
f.value1 <- 2 * (Precision1 * Recall1) / (Precision1 + Recall1)
f.value1 # 0.09317001


##########################################################################################
############## 2. (방사형 커널) gamma cost를 변화시키면서 최적의 모형 찾기 ###############
# kernel = 'radial'(방사형 커널)

# cost={0.01, 0.1, 1, 10}, gamma={0.25, 0.5, 1} -> 3X3 = 9가지 경우의 수 비교
svm2 <- tune.svm(TARGET~., data=data_Train2, cost=10^c(-2:1), gamma=2^c(-3:0))
svm2
summary(svm2)

# cost = 1일때 가장 error가 낮고 gamma는 낮아질수록 error가 작아지므로 
# cost = 1로 고정하고 더작은 gamma로 추정해본다.
svm2 <- tune.svm(TARGET~., data=data_Train2, cost=1, gamma=2^c(-8:-5))
summary(svm2)
# 결과 => best parameters: gamma=0.0078125, cost=1

# 최적의 cost, gamma로 SVM모형 추정
svm2 <- svm(TARGET~., data = data_Train2, cost = 1, gamma = 2^(-7))

# 추정한 모형으로 data_Test2의 TARGET 예측
svm2.pred <- predict(svm2,data_Test2)

# F measure 계산
table(svm2.pred)
table(data_Test2$TARGET)
result2 <- table(data_Test2$TARGET, svm2.pred)
Precision2 <- result2[4] / (result2[3] + result2[4])
Recall2 <- result2[4] / (result2[2] + result2[4])
f.value2 <- 2 * (Precision2 * Recall2) / (Precision2 + Recall2)
f.value2 # 0.09614288


##########################################################################################
################# 3. (선형 초평면) cost를 변화시키면서 최적의 모형 찾기 ##################
# kernel = 'linear'(선형 초평면) -> cost값만 조정

# cost={0.01, 0.1, 1, 10}
svm3 <- tune.svm(TARGET~., data=data_Train2, cost=10^c(-3:1), kernel = 'linear')
svm3
summary(svm3)
# 결과 => best parameters: cost = 0.01

# 최적의 cost로 SVM모형 추정
svm3 <- svm(TARGET~., data = data_Train2, cost = 10^(-2), kernel = 'linear')

# 추정한 모형으로 data_Test2의 TARGET 예측
svm3.pred <- predict(svm3,data_Test2)

# F measure 계산
table(svm3.pred)
table(data_Test2$TARGET)
result3 <- table(data_Test2$TARGET, svm3.pred)
Precision3 <- result3[4] / (result3[3] + result3[4])
Recall3 <- result3[4] / (result3[2] + result3[4])
f.value3 <- 2 * (Precision3 * Recall3) / (Precision3 + Recall3)
f.value3 # 0.09877033


##########################################################################################
############## 4. (다항식 커널)cost, degree를 변화시키면서 최적의 모형 찾기 ##############
# kernel = 'polynomial'(다항식 커널) -> cost, degree인자 조정

# cost={0.01, 0.1, 1, 10}, degree = 
svm4 <- tune.svm(TARGET~., data=data_Train2, cost=10^c(-2:1), degree = c(0:3), kernel = 'polynomial')
svm4
summary(svm4)

# degree = 1일때 가장 error가 낮고 cost 커질수록 error가 작아지므로 
# degree = 1로 고정하고 더큰 cost 추정해본다.
svm4 <- tune.svm(TARGET~., data=data_Train2, cost=10^c(1:3), degree = 1, kernel = 'polynomial')
summary(svm4)
# 결과 => best parameters: cost= 10, degree = 1

# 최적의 cost, degree로 SVM모형 추정
svm4 <- svm(TARGET~., data = data_Train2, cost = 10, degree = 1, kernel = 'polynomial')

# 추정한 모형으로 data_Test2의 TARGET 예측
svm4.pred <- predict(svm4,data_Test2)

# F measure 계산
table(svm4.pred)
table(data_Test2$TARGET)
result4 <- table(data_Test2$TARGET, svm4.pred)
Precision4 <- result4[4] / (result4[3] + result4[4])
Recall4 <- result4[4] / (result4[2] + result4[4])
f.value4 <- 2 * (Precision4 * Recall4) / (Precision4 + Recall4)
f.value4 # 0.1006036




##########################################################################################
