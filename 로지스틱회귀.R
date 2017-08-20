############################## data_use : 모델링시 최종적으로 사용할 데이터 ##############################
data_use <- data_factor
summary(data_use)

# data_use TARGET == '1'만족하는 자료는 4243개

#################################################################################

SCI <- c(BNK_LNIF_CNT,CPT_LNIF_CNT,    
         SPART_LNIF_CNT,ECT_LNIF_CNT,TOT_LNIF_AMT,
         TOT_CLIF_AMT,BNK_LNIF_AMT,CPT_LNIF_AMT,
         CRDT_OCCR_MDIF,SPTCT_OCCR_MDIF,CRDT_CARD_CNT,
         CTCD_OCCR_MDIF,CB_GUIF_CNT,CB_GUIF_AMT)

HAN <- c(OCCP_NAME_G,CUST_JOB_INCM,HSHD_INFR_INCM,
         ACTL_FMLY_NUM,CUST_FMLY_NUM,LAST_CHLD_AGE,
         MATE_OCCP_NAME_G,MATE_JOB_INCM,CRDT_LOAN_CNT,
         MIN_CNTT_DATE,TOT_CRLN_AMT,TOT_REPY_AMT,
         CRLN_OVDU_RATE,CRLN_30OVDU_RATE,LT1Y_CLOD_RATE,
         STRT_CRDT_GRAD,LTST_CRDT_GRAD,PREM_OVDU_RATE,
         LT1Y_PEOD_RATE,AVG_STLN_RATE,STLN_REMN_AMT,
         LT1Y_STLN_AMT,LT1Y_SLOD_RATE,GDINS_MON_PREM,
         SVINS_MON_PREM,FMLY_GDINS_MNPREM,FMLY_SVINS_MNPREM,
         MAX_MON_PREM,TOT_PREM,FMLY_TOT_PREM,
         CNTT_LAMT_CNT,LT1Y_CTLT_CNT,AUTR_FAIL_MCNT,
         FYCM_PAID_AMT,FMLY_CLAM_CNT,FMLY_PLPY_CNT)

c(AGE,SEX)

SKT <- c(AVG_CALL_TIME,
         AVG_CALL_FREQ,TEL_MBSP_GRAD,ARPU,     
         MON_TLFE_AMT,CBPT_MBSP_YN,MOBL_FATY_PRC,
         TEL_CNTT_QTR,NUM_DAY_SUSP,CRMM_OVDU_AMT,
         TLFE_UNPD_CNT,LT1Y_MXOD_AMT,PAYM_METD,    
         LINE_STUS,MOBL_PRIN)

SCI <- c("BNK_LNIF_CNT",      "CPT_LNIF_CNT"   ,
         "SPART_LNIF_CNT",    "ECT_LNIF_CNT",      "TOT_LNIF_AMT"     ,
         "TOT_CLIF_AMT",      "BNK_LNIF_AMT",      "CPT_LNIF_AMT"     ,
         "CRDT_OCCR_MDIF",    "SPTCT_OCCR_MDIF",   "CRDT_CARD_CNT"    ,
         "CTCD_OCCR_MDIF",    "CB_GUIF_CNT",       "CB_GUIF_AMT"      )

HAN <- c("OCCP_NAME_G"    ,   "CUST_JOB_INCM",     "HSHD_INFR_INCM"   ,
         "ACTL_FMLY_NUM"  ,   "CUST_FMLY_NUM",     "LAST_CHLD_AGE"    ,
         "MATE_OCCP_NAME_G",  "MATE_JOB_INCM",     "CRDT_LOAN_CNT"    ,
         "MIN_CNTT_DATE" ,    "TOT_CRLN_AMT" ,     "TOT_REPY_AMT"     ,
         "CRLN_OVDU_RATE"  ,  "CRLN_30OVDU_RATE" , "LT1Y_CLOD_RATE"   ,
         "STRT_CRDT_GRAD" ,   "LTST_CRDT_GRAD"  ,  "PREM_OVDU_RATE"   ,
         "LT1Y_PEOD_RATE" ,   "AVG_STLN_RATE"   ,  "STLN_REMN_AMT"    ,
         "LT1Y_STLN_AMT"  ,   "LT1Y_SLOD_RATE"  ,  "GDINS_MON_PREM"   ,
         "SVINS_MON_PREM" ,   "FMLY_GDINS_MNPREM", "FMLY_SVINS_MNPREM",
         "MAX_MON_PREM"   ,   "TOT_PREM"       ,   "FMLY_TOT_PREM"    ,
         "CNTT_LAMT_CNT"  ,   "LT1Y_CTLT_CNT"  ,   "AUTR_FAIL_MCNT"   ,
         "FYCM_PAID_AMT"  ,   "FMLY_CLAM_CNT"  ,   "FMLY_PLPY_CNT"    )

c("AGE"           ,    "SEX" )              

SKT <- c("AVG_CALL_TIME"    ,
         "AVG_CALL_FREQ"  ,   "TEL_MBSP_GRAD"  ,   "ARPU"             ,
         "MON_TLFE_AMT"   ,   "CBPT_MBSP_YN"   ,   "MOBL_FATY_PRC"    ,
         "TEL_CNTT_QTR"    ,  "NUM_DAY_SUSP"   ,   "CRMM_OVDU_AMT"    ,
         "TLFE_UNPD_CNT"   ,  "LT1Y_MXOD_AMT"   ,  "PAYM_METD"        ,
         "LINE_STUS"     ,    "MOBL_PRIN"  )

#################################################################################


############################## data_use 데이터로 로지스틱 회귀모형 추정 ##############################
############################## 데이터가 너무 크고 변수가 많아 추정 못함 ##############################
logis.fit <- glm(TARGET ~ ., data = data_use, family = binomial)
summary(logis.fit)

logis.probs <- predict(logis.fit, data_use, type = 'response')
logis.pred <- rep(0, nrow(data_use))
logis.pred[logis.probs > 0.5] <- 1

result <- table(data_use$TARGET, logis.pred)
Precision <- result[1] / (result[1] + result[2])
Recall <- result[1] / (result[1] + result[3])
f.measure <- 2 * (Precision * Recall) / (Precision + Recall)

######F measure 계산
#true <- c(1,1,1,0,1,0,1,1,0,0)
#prediction <- c(1,1,0,1,1,0,0,1,0,0)

#a <- table(true,prediction)[1] #a
#c <- table(true,prediction)[2] #c
#b <- table(true,prediction)[3] #b
#d <- table(true,prediction)[4] #d

#Precision(정확률) = a / (a + c)
#- 컴퓨터가 True라 한것 중에 실제 True의 비율
#Recall(재현률) = a / (a + b)
#- 실제 True중 컴퓨터가 True라 한것의 비율
#F1 score =  2 * (Precision * Recall) / (Precision + Recall)




############################ train 데이터의 모든 변수로 로지스틱 모형 추정 ##############################
############################ 수렴안함, 변수가 너무 많은 듯 하다.. ##############################
data_target0 <- data_use[data_use$TARGET == '0',]
data_target1 <- data_use[data_use$TARGET == '1',]

myind_train_target0 <- sample(nrow(data_target0), nrow(data_target1) * 0.7) # data_target0의 인덱스 샘플링
myind_test_target0 <- sample(nrow(data_target0), nrow(data_target1) * 0.3) # data_target0의 인덱스 샘플링
myind_train_target1 <- sample(nrow(data_target1), nrow(data_target1) * 0.7) # data_target1의 인덱스 샘플링

data_train <- rbind(data_target0[myind_train_target0,], data_target1[myind_train_target1,]) # 분석할 데이터 합치기
data_test <- rbind(data_target0[myind_test_target0,], data_target1[-myind_train_target1,])

logis.fit <- glm(TARGET ~ ., data = data_train, family = binomial)
summary(logis.fit)

############################ train 데이터의 변수를 선택해 로지스틱 모형 추정 ##############################
############################ 37개 변수, 6000개의 데이터, 3분 정도 소요 ##############################
data_target0 <- data_use[data_use$TARGET == '0',]
data_target1 <- data_use[data_use$TARGET == '1',]

myind_train_target0 <- sample(nrow(data_target0), nrow(data_target1) * 0.7) # data_target0의 인덱스 샘플링
myind_test_target0 <- sample(nrow(data_target0), nrow(data_target1) * 0.3) # data_target0의 인덱스 샘플링
myind_train_target1 <- sample(nrow(data_target1), nrow(data_target1) * 0.7) # data_target1의 인덱스 샘플링

data_train <- rbind(data_target0[myind_train_target0,], data_target1[myind_train_target1,]) # 분석할 train 데이터 합치기
data_test <- rbind(data_target0[myind_test_target0,], data_target1[-myind_train_target1,])

logis.fit <- glm(TARGET ~ BNK_LNIF_CNT+CPT_LNIF_CNT+SPART_LNIF_CNT+ECT_LNIF_CNT+TOT_LNIF_AMT
                 +TOT_CLIF_AMT+BNK_LNIF_AMT+CPT_LNIF_AMT+CRDT_OCCR_MDIF+SPTCT_OCCR_MDIF
                 +CRDT_CARD_CNT+CTCD_OCCR_MDIF+CB_GUIF_CNT+CB_GUIF_AMT
                 +HSHD_INFR_INCM+CRDT_LOAN_CNT+TOT_CRLN_AMT+TOT_REPY_AMT+CRLN_OVDU_RATE
                 +LT1Y_CLOD_RATE+LTST_CRDT_GRAD+PREM_OVDU_RATE+LT1Y_PEOD_RATE+LT1Y_STLN_AMT
                 +LT1Y_SLOD_RATE+GDINS_MON_PREM+SVINS_MON_PREM+F MLY_GDINS_MNPREM+FMLY_SVINS_MNPREM
                 +LT1Y_CTLT_CNT+FYCM_PAID_AMT+FMLY_PLPY_CNT+MON_TLFE_AMT
                 +CRMM_OVDU_AMT+TLFE_UNPD_CNT+LT1Y_MXOD_AMT+LINE_STUS, data = data_train, family = binomial)
summary(logis.fit)

logis.probs <- predict(logis.fit, data_test, type = 'response')
logis.pred <- rep(0, nrow(data_use))
logis.pred[logis.probs > 0.5] <- 1

result <- table(data_use$TARGET, logis.pred)
Precision <- result[1] / (result[1] + result[2])
Recall <- result[1] / (result[1] + result[3])
f.measure <- 2 * (Precision * Recall) / (Precision + Recall)



########################## train 데이터의 변수를 선택해 로지스틱 모형 추정 ############################
#################### SCI, HAN, SKT 변수별로 logis.sci, logis.han, logis.skt로 분석 #####################
data_target0 <- data_use[data_use$TARGET == '0',]
data_target1 <- data_use[data_use$TARGET == '1',]

myind_train_target0 <- sample(nrow(data_target0), nrow(data_target1) * 0.7) # data_target0의 인덱스 샘플링
myind_test_target0 <- sample(nrow(data_target0), nrow(data_target1) * 0.3) # data_target0의 인덱스 샘플링
myind_train_target1 <- sample(nrow(data_target1), nrow(data_target1) * 0.7) # data_target1의 인덱스 샘플링

data_train <- rbind(data_target0[myind_train_target0,], data_target1[myind_train_target1,]) # 분석할 train 데이터 합치기
data_test <- rbind(data_target0[myind_test_target0,], data_target1[-myind_train_target1,])

# 세 종류의 logistic 모델 세우기
SCI.var = paste('TARGET ~ ', SCI[1], sep ='')
HAN.var = paste('TARGET ~ ', HAN[1], sep ='')
SKT.var = paste('TARGET ~ ', SKT[1], sep ='')
for (i in 2:length(SCI)){SCI.var = paste(SCI.var, SCI[i], sep=" + ")}
for (i in 2:length(HAN)){HAN.var = paste(HAN.var, HAN[i], sep=" + ")}
for (i in 2:length(SKT)){SKT.var = paste(SKT.var, SKT[i], sep=" + ")}

# 모델 추정
logis.sci <- glm(SCI.var, data = data_train, family = binomial)
summary(logis.fit)

logis.han <- glm(HAN.var, data = data_train, family = binomial)
summary(logis.han)

logis.skt <- glm(SKT.var, data = data_train, family = binomial)
summary(logis.skt)

# F measure 계산, 비교
f.measure <- matrix(c(0, 0, 0), nrow = 3)
logis <- c(logis.sci, logis.han, logis.skt)

{
  logis.probs <- predict(logis.sci, data_test, type = 'response')
  logis.pred <- rep(0, nrow(data_test))
  logis.pred[logis.probs > 0.5] <- 1
  
  result <- table(data_test$TARGET, logis.pred)
  Precision <- result[1] / (result[1] + result[2])
  Recall <- result[1] / (result[1] + result[3])
}
# 요인(factor) TOT_LNIF_AMT는 300001, 408001, 444001, 471001, 634001, 704001, 724001, 804001, 874001개의 
# 새로운 수준(levels)들을 가지고 있습니다.
# 라는 ERROR가 나옴...

f.measure[1] <- 2 * (Precision * Recall) / (Precision + Recall)

f.measure






# 교차검증 : 검정오차를 추정하여 성능평가(모델평가), 적절한 수준의 유연성을 선택하는데 사용(모델선택)
# 붓스트랩 : 파라미터 추정의 정확도  또는 주어진 통계학습 방법의 정확도 측정




############################## 20 fold ##############################
### TARGET == '0'인 데이터를 20개로 나눠서 각각 TARGET == '1'인 데이터와 합친후
### logistic모형을 추정해 20개의 F measure를 계산해 평균낸다.

length(which(data_use$TARGET == '1')) # data_use TARGET == '1'만족하는 자료는 4243개

# TARGET == '0'을 만족하는 데이터 셋을 data_target0으로 저장
data_target0 <- data_use[data_use$TARGET == '0',]
nrow(data_target0)

# TARGET == '1'을 만족하는 데이터 셋을 data_target1으로 저장
data_target1 <- data_use[data_use$TARGET == '1',]
nrow(data_target1)

# 20-fold 약 4740개
K<-20


myind <- sample(nrow(data_target0)) #자료의 순서 랜덤화하
len1 <- floor(length(myind)/K) #처음 9개의 파티션의 길이 설정

f.value <- rep(0,K)
for (i in 1:K){
  startind <- (i - 1) * len1 + 1   
  if (i == K){
    endind <- nrow(data_target0)} else
    {endind <- i * len1}
  
  testind <- myind[startind:endind]
  traindata <- rbind(data_target0[-testind,], data_target1)
  testdata <-  rbind(data_target0[testind,], data_target1)
  
  logis.fit <- glm(TARGET ~ ., data = traindata, family = binomial) # logistic 모형
  logis.probs <- predict(logis.fit, testdata, type = 'response')
  logis.pred <- rep(0, nrow(testdata))
  logis.pred[logis.probs > 0.5] <- 1
  
  # F measure 계산
  result <- table(testdata$TARGET, logis.pred)
  Precision <- result[1] / (result[1] + result[2])
  Recall <- result[1] / (result[1] + result[3])
  f.value[i] <- 2 * (Precision * Recall) / (Precision + Recall)
  
}

f.measure = mean(f.value)
f.measure



############################## Roughly Balanced Bagging ##############################
### 최적의 변수가 정해졌을때 최적의 b를 Roughly Balanced Bagging을 이용해 구한다.
# data_train을 만들어 logistic모델을 추정한 후 추정한 theta를 b에 저장한다.
# k개의 부스트랩 샘플에 대해 b를 계산한 후에 평균 b를 구한다.
# test data를 이용해 logistic 모델의 성능을 F measure을 이용해 평가한다. 

train_p = 0.7 # train data의 비율
Nmin <- train_p * length(which(data_use$TARGET == '1')) 
p = 1 / 2 # 사용할 데이터에서 Nmaj의 비율?
Nmaj <- Nmin * (1 - p) / p # negativebinomial(Nmin, p)의 평균
k <- 10 # 부스트랩 샘플의 수
q = 15 # 추정할 theta의 수(b0, b1, b2, ...)

# TARGET == '0'을 만족하는 데이터 셋을 data_target0으로 저장
data_target0 <- data_use[data_use$TARGET == '0',]

# TARGET == '1'을 만족하는 데이터 셋을 data_target1으로 저장
data_target1 <- data_use[data_use$TARGET == '1',]
myind_train_target1 <- sample(nrow(data_target1), Nmin) # data_target1의 인덱스 샘플링

b <- matrix(rep(0, k * q), nrow = k)

for (i in 1:k){
  myind_train_target0 <- sample(nrow(data_target0), Nmaj) # data_target0의 인덱스 샘플링
  data_train <- rbind(data_target0[myind_train_target0,], data_target1[myind_train_target1,]) # 분석할 데이터 합치기
  
  logis.fit <- glm(TARGET ~ BNK_LNIF_CNT+CPT_LNIF_CNT+SPART_LNIF_CNT+ECT_LNIF_CNT+TOT_LNIF_AMT
                   +TOT_CLIF_AMT+BNK_LNIF_AMT+CPT_LNIF_AMT+CRDT_OCCR_MDIF+SPTCT_OCCR_MDIF
                   +CRDT_CARD_CNT+CTCD_OCCR_MDIF+CB_GUIF_CNT+CB_GUIF_AMT, data = data_train, family = binomial)
  print(logis.fit$coef)
  
  # 문제1)) 범주형 변수의 계수를 b에 넣는 방법? 데이터 전처리 과정에서 범주형 변수를 더미변수로 바꾼다.
  b[i, ] <- logis.fit$coef
}

theta <- apply(b, 2, mean)

b0 <- theta[1]
theta[1] <- b0 + log((0.04233137)/(1 - 0.04233137)) - log((0.5)/(1 - 0.5))
theta

# 문제2)) 추정한 theta를 가지는 logistic 모델을 생성하는 방법? 그냥 직접 수식으로 한다?
# test data 생성
myind_test_target0 <- sample(nrow(data_target0), Nmaj * 0.3)
data_test <- rbind(data_target0[myind_test_target0,], data_target1[-myind_train_target1,]) 

z <- subset(data_test, select = c(BNK_LNIF_CNT,CPT_LNIF_CNT,SPART_LNIF_CNT,ECT_LNIF_CNT,TOT_LNIF_AMT,
                                  TOT_CLIF_AMT,BNK_LNIF_AMT,CPT_LNIF_AMT,CRDT_OCCR_MDIF,SPTCT_OCCR_MDIF,
                                  CRDT_CARD_CNT,CTCD_OCCR_MDIF,CB_GUIF_CNT,CB_GUIF_AMT)) %*% t(theta)
logis.probs <- 1 / (1 + exp(-z))
logis.pred[logis.probs > 0.5] <- 1

result <- table(data_train$TARGET, logis.pred)
Precision <- result[1] / (result[1] + result[2])
Recall <- result[1] / (result[1] + result[3])

f.measure <- 2 * (Precision * Recall) / (Precision + Recall)
f.measure




############################## Roughly Balanced Bagging ##############################
### stepwise를 이용해 변수를 선택한다. Roughly Balanced Bagging을 이용해 구한다.
# data_train을 만들어 logistic모델을 추정한 후 추정한 theta를 b에 저장한다.
# k개의 부스트랩 샘플에 대해 b를 계산한 후에 평균 b를 구한다.
# test data를 이용해 logistic 모델의 성능을 F measure을 이용해 평가한다. 

train_p = 0.7 # train data의 비율
Nmin <- train_p * length(which(data_use$TARGET == '1')) 
p = 1 / 2 # 사용할 데이터에서 Nmaj의 비율?
Nmaj <- Nmin * (1 - p) / p # negativebinomial(Nmin, p)의 평균
k <- 10 # 부스트랩 샘플의 수
q = 68 # 추정할 theta의 수(b0, b1, b2, ...)

# TARGET == '0'을 만족하는 데이터 셋을 data_target0으로 저장
data_target0 <- data_use[data_use$TARGET == '0',]

# TARGET == '1'을 만족하는 데이터 셋을 data_target1으로 저장
data_target1 <- data_use[data_use$TARGET == '1',]
myind_train_target1 <- sample(nrow(data_target1), Nmin) # data_target1의 인덱스 샘플링

# stepwise로 추정한 모형 저장할 공간 만들기
variables <- matrix(rep(0, k), nrow = k)

# Roughly Balanced Bagging을 이용해 k개의 모형 추정
for (i in 1:k){
  myind_train_target0 <- sample(nrow(data_target0), Nmaj) # data_target0의 인덱스 샘플링
  data_train <- rbind(data_target0[myind_train_target0,], data_target1[myind_train_target1,]) # 분석할 데이터 합치기
  
  logis.fit <- glm(TARGET ~ ., data = data_train, family = binomial)
  logis.step <- step(logis.fit, direction = 'both')
  
  # 문제1)) 변수별로 불러와서 저장하는 방법?
  variables[i, ] <- logis.step$formula
}

variables




## http://www.statmethods.net/stats/regression.html
## https://cran.r-project.org/web/packages/bestglm/vignettes/bestglm.pdf

