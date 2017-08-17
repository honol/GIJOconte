############################## data_set 원본 데이터 ##############################
data_set <- read.table('Data_set.csv', header = T, sep = ',', stringsAsFactors= F)

data_factor <- data_set
head(data_set)
summary(data_set)



############################## 범주화 변수 변환 ##############################
# 범주화(순서존재)
data_factor$TOT_LNIF_AMT <- as.factor(data_factor$TOT_LNIF_AMT)
data_factor$TOT_CLIF_AMT <- as.factor(data_factor$TOT_CLIF_AMT)
data_factor$BNK_LNIF_AMT <- as.factor(data_factor$BNK_LNIF_AMT)
data_factor$CPT_LNIF_AMT <- as.factor(data_factor$CPT_LNIF_AMT)
data_factor$CRDT_OCCR_MDIF <- as.factor(data_factor$CRDT_OCCR_MDIF)
data_factor$SPTCT_OCCR_MDIF <- as.factor(data_factor$SPTCT_OCCR_MDIF)
data_factor$CTCD_OCCR_MDIF <- as.factor(data_factor$CTCD_OCCR_MDIF)
data_factor$CB_GUIF_AMT <- as.factor(data_factor$CB_GUIF_AMT)
data_factor$CUST_JOB_INCM <- as.factor(data_factor$CUST_JOB_INCM)
data_factor$HSHD_INFR_INCM <- as.factor(data_factor$HSHD_INFR_INCM)
data_factor$LAST_CHLD_AGE <- as.factor(data_factor$LAST_CHLD_AGE)
data_factor$MATE_JOB_INCM <- as.factor(data_factor$MATE_JOB_INCM)
data_factor$TOT_CRLN_AMT <- as.factor(data_factor$TOT_CRLN_AMT)
data_factor$TOT_REPY_AMT <- as.factor(data_factor$TOT_REPY_AMT)
data_factor$STRT_CRDT_GRAD <- as.factor(data_factor$STRT_CRDT_GRAD)
data_factor$LTST_CRDT_GRAD <- as.factor(data_factor$LTST_CRDT_GRAD)
data_factor$LT1Y_PEOD_RATE <- as.factor(data_factor$LT1Y_PEOD_RATE)
data_factor$STLN_REMN_AMT <- as.factor(data_factor$STLN_REMN_AMT)
data_factor$LT1Y_STLN_AMT <- as.factor(data_factor$LT1Y_STLN_AMT)
data_factor$LT1Y_SLOD_RATE <- as.factor(data_factor$LT1Y_SLOD_RATE)
data_factor$GDINS_MON_PREM <- as.factor(data_factor$GDINS_MON_PREM)
data_factor$SVINS_MON_PREM <- as.factor(data_factor$SVINS_MON_PREM)
data_factor$FMLY_GDINS_MNPREM <- as.factor(data_factor$FMLY_GDINS_MNPREM)
data_factor$FMLY_SVINS_MNPREM <- as.factor(data_factor$FMLY_SVINS_MNPREM)
data_factor$MAX_MON_PREM <- as.factor(data_factor$MAX_MON_PREM)
data_factor$TOT_PREM <- as.factor(data_factor$TOT_PREM)
data_factor$FMLY_TOT_PREM <- as.factor(data_factor$FMLY_TOT_PREM)
data_factor$FYCM_PAID_AMT <- as.factor(data_factor$FYCM_PAID_AMT)
data_factor$ARPU <- as.factor(data_factor$ARPU)
data_factor$MON_TLFE_AMT <- as.factor(data_factor$MON_TLFE_AMT)
data_factor$MOBL_FATY_PRC <- as.factor(data_factor$MOBL_FATY_PRC)
data_factor$CRMM_OVDU_AMT <- as.factor(data_factor$CRMM_OVDU_AMT)
data_factor$LT1Y_MXOD_AMT <- as.factor(data_factor$LT1Y_MXOD_AMT)
data_factor$MOBL_PRIN <- as.factor(data_factor$MOBL_PRIN)

# 범주화(순서존재NO)
data_factor$TARGET <- as.factor(data_factor$TARGET)
data_factor$OCCP_NAME_G <- as.factor(data_factor$OCCP_NAME_G)
data_factor$MATE_OCCP_NAME_G <- as.factor(data_factor$MATE_OCCP_NAME_G)
data_factor$SEX <- as.factor(data_factor$SEX)
data_factor$TEL_MBSP_GRAD <- as.factor(data_factor$TEL_MBSP_GRAD)
data_factor$CBPT_MBSP_YN <- as.factor(data_factor$CBPT_MBSP_YN)
data_factor$PAYM_METD <- as.factor(data_factor$PAYM_METD)
data_factor$LINE_STUS <- as.factor(data_factor$LINE_STUS)

# 범주화(날짜)
data_factor$MIN_CNTT_DATE <- as.factor(data_factor$MIN_CNTT_DATE)
data_factor$TEL_CNTT_QTR <- as.factor(data_factor$TEL_CNTT_QTR)

### data_factor 범주화한 데이터 확인
summary(data_factor)
str(data_factor)



######################### data_fn 범주화(as.factor)후 na|null처리한 데이터 #########################
data_fn = data_factor

### NULL 처리
# OCCP_NAME_G : '*' -> NA
data_fn$OCCP_NAME_G[data_fn$OCCP_NAME_G == '*'] <- NA
summary(data_fn$OCCP_NAME_G)

# LAST_CHLD_AGE : 'NULL' -> NULL ?????????????????????????안됨
idx <- which(data_fn$LAST_CHLD_AGE == 'NULL')
data_fn$LAST_CHLD_AGE[23] <- NULL
which(data_fn$LAST_CHLD_AGE == 'NULL')
is.null(data_fn$LAST_CHLD_AGE[23])
length(data_fn$LAST_CHLD_AGE)
length(NULL)
summary(data_fn$LAST_CHLD_AGE)

# MATE_OCCP_NAME_G : '*' -> NA
data_fn$MATE_OCCP_NAME_G[data_fn$MATE_OCCP_NAME_G == '*'] <- NA
summary(data_fn$MATE_OCCP_NAME_G)

# AGE : '*' -> NA
data_fn$AGE[data_fn$AGE == '*'] <- NA
summary(data_fn$AGE)

# SEX : '*' -> NA
data_fn$SEX[data_fn$SEX == '*'] <- NA
summary(data_fn$SEX)

# TEL_MBSP_GRAD / PAYM_METD : ' ' -> NULL ?????????????????????????

###data_fn 범주화(as.factor) 후 na|null처리한 데이터 확인
summary(data_fn)



############################## data_narm 결측값 있는 자료 삭제 ##############################
# 방법1
data_narm <- data_fn[which(!is.na(data_fn$OCCP_NAME_G) & !is.na(data_fn$MATE_OCCP_NAME_G)
                           & !is.na(data_fn$AGE) & !is.na(data_fn$SEX)), ]
# 방법2
data_narm <- na.omit(data_fn)
summary(data_narm)



############################## data_narm_xy : CUST_ID 삭제 ##############################
data_narm_xy <- data_narm[,-1]
summary(data_narm_xy)



############################## 데이터 write ##############################
write.table(data_narm, "data_narm.csv", sep=",", col.names=TRUE) 




############################## data_narm 데이터로 로지스틱 회귀모형 추정 ##############################

logis.fit <- glm(TARGET ~ ., data = data_narm_xy, family = binomial)
summary(logis.fit)

logis.probs <- predict(logis.fit, data_narm_xy, type = 'response')
logis.pred <- rep(0, nrow(data_narm_xy))
logis.pred[logis.probs > 0.5] <- 1

result <- table(data_narm_xy$TARGET, logis.pred)
Precision <- result[1] / (result[1] + result[2])
Recall <- result[1] / (result[1] + result[3])
f.measure <- 2 * (Precision * Recall) / (Precision + Recall)




# 교차검증 : 검정오차를 추정하여 성능평가(모델평가), 적절한 수준의 유연성을 선택하는데 사용(모델선택)
# 붓스트랩 : 파라미터 추정의 정확도  또는 주어진 통계학습 방법의 정확도 측정
############################## 10 fold ##############################
length(which(data_narm_xy$TARGET == '1')) # data_narm_xy에서 TARGET == '1'만족하는 자료는 4243개

# TARGET == '0'을 만족하는 데이터 셋을 data_target0으로 저장
data_target0 <- data_narm_xy[data_narm_xy$TARGET == '0',]
nrow(data_target0)

# TARGET == '0'을 만족하는 데이터 셋을 data_target1으로 저장
data_target1 <- data_narm_xy[data_narm_xy$TARGET == '1',]
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

######F measure 계산
#x <- c(1,1,1,0,1,0,1,1,0,0) #true
#y <- c(1,1,0,1,1,0,0,1,0,0) #prediction

#table(x,y)[1] #a
#table(x,y)[2] #c
#table(x,y)[3] #b
#table(x,y)[4] #d

#Precision(정확률) = a / (a + c)
#- 컴퓨터가 True라 한것 중에 실제 True의 비율
#Recall(재현률) = a / (a + b)
#- 실제 True중 컴퓨터가 True라 한것의 비율
#F1 score =  2 * (Precision * Recall) / (Precision + Recall)


################Bootstrap sample을 사용
x<-rnorm(100)
median(x)
#median의 분포?
M<-1000
mymed<-rep(0,M)
for (i in 1:M){
  bs<-sample(x,length(x),replace=T)
  mymed[i]<-median(bs)
}
var(mymed) #[1] 0.02054708
sd(mymed) #[1] 0.1433425
mean(ifelse(mymed<0,1,0))# [1] 0.915



