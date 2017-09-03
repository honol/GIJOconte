##########################################################################################
###################################### 데이터 처리 #######################################

data_set <- read.table('Data_set.csv', header = T, sep = ',', stringsAsFactors= F)

data_na = data_set

# OCCP_NAME_G : '*' -> NA
data_na$OCCP_NAME_G[data_na$OCCP_NAME_G == '*'] <- NA

# MATE_OCCP_NAME_G : '*' -> NA
data_na$MATE_OCCP_NAME_G[data_na$MATE_OCCP_NAME_G == '*'] <- NA

# AGE : '*' -> NA
data_na$AGE[data_na$AGE == '*'] <- NA

# SEX : '*' -> NA
data_na$SEX[data_na$SEX == '*'] <- NA

# LAST_CHLD_AGE : 'NULL' -> NA
data_na$LAST_CHLD_AGE[which(data_na$LAST_CHLD_AGE == 'NULL')] <- NA

data_narm = na.omit(data_na)


############################## data_factor : 범주화 변수 변환 ##############################
data_factor <- data_narm


#클러스터링 변수 선택(범주형 중 수준 축소가 필요한 경우)
SCI <- cbind(data_factor$TOT_LNIF_AMT, data_factor$TOT_CLIF_AMT,      
             data_factor$BNK_LNIF_AMT, data_factor$CPT_LNIF_AMT, 
             data_factor$CB_GUIF_AMT)

HAN <- cbind(data_factor$CUST_JOB_INCM, data_factor$HSHD_INFR_INCM,
             data_factor$LAST_CHLD_AGE, data_factor$MATE_JOB_INCM, data_factor$TOT_CRLN_AMT,
             data_factor$TOT_REPY_AMT, data_factor$STLN_REMN_AMT, data_factor$LT1Y_STLN_AMT,
             data_factor$GDINS_MON_PREM, data_factor$SVINS_MON_PREM, data_factor$FMLY_GDINS_MNPREM,
             data_factor$FMLY_SVINS_MNPREM, data_factor$MAX_MON_PREM, data_factor$TOT_PREM,
             data_factor$FMLY_TOT_PREM, data_factor$FYCM_PAID_AMT, data_factor$FMLY_CLAM_CNT, data_factor$AGE)
SKT <- cbind(data_factor$AVG_CALL_TIME,
             data_factor$AVG_CALL_FREQ, data_factor$ARPU,
             data_factor$MON_TLFE_AMT, data_factor$MOBL_FATY_PRC,
             data_factor$NUM_DAY_SUSP, data_factor$CRMM_OVDU_AMT,
             data_factor$LT1Y_MXOD_AMT, data_factor$MOBL_PRIN)

#cluster값으로 변환한 변수들을 data_Train에 삽입
#k$cluster 값을 할당하여 변수를 10개 범주로 변환

#SCI에 속하는 변수 중, cluster가 필요한 변수를 변환함. 변환한 각 변수를 SCI[,i]에 저장
for (i in 1:5)
{
  SCI[,i] = kmeans(SCI[,i], 10)$cluster
}

#HAN에 속하는 변수 중, cluster가 필요한 변수를 변환함. 변환한 각 변수를 HAN[,i]에 저장
for (i in 1:18)
{
  HAN[,i] = kmeans(HAN[,i], 10)$cluster
}

#SKT에 속하는 변수 중, cluster가 필요한 변수를 변환함. 변환한 각 변수를 SKT[,i]에 저장
for (i in 1:9)
{
  SKT[,i] = kmeans(SKT[,i], 10)$cluster
}

#data_Train에서 각 변수의 인덱스 파악
names(data_factor)
SCI_num = c(7, 8, 9, 10, 16)
HAN_num = c(18, 19, 22, 24, 27, 28, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 51, 53)
SKT_num = c(55, 56, 58, 59, 61, 63, 64, 66, 69)

j = 1
for (i in SCI_num)
{
  data_factor[,i] = SCI[,j]
  j = j + 1
  print(j)
}

j = 1
for (i in HAN_num)
{
  data_factor[,i] = HAN[,j]
  j = j + 1
  print(j)
}

j = 1
for (i in SKT_num)
{
  data_factor[,i] = SKT[,j]
  j = j + 1
  print(j)
}


#범주화
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


#동일한 비율로 타겟 변수 추출
data_od = data_factor[which(data_factor$TARGET == 1),]
data_rp = data_factor[which(data_factor$TARGET == 0),]

set.seed(123)
randomNumber_od = sample(1:4243, size = 3395, replace = F)
randomNumber_rp = sample(1:94801, size = 3395, replace = F)


data_od_train = data_od[randomNumber_od,]
data_rp_train = data_rp[randomNumber_rp,]

data_od_test = data_od[-randomNumber_od,]
data_rp_test = data_rp[-randomNumber_rp,]

data_Test = rbind(data_od_test, data_rp_test)
data_Train = rbind(data_od_train, data_rp_train)


data_Train2 = subset(data_Train, select = -c(CUST_ID, MIN_CNTT_DATE, ARPU, TEL_CNTT_QTR))
data_Test2 = subset(data_Test, select = -c(CUST_ID, MIN_CNTT_DATE, ARPU, TEL_CNTT_QTR))



######################################################################################













######################################################################################
######################################################################################
########################################## tree ######################################
######################################################################################
######################################################################################


######################################################################################
###################################  1. tree함수 이용 ################################
# tree함수 불순도 지표 : 지니지수
if(!require(tree)){install.packages('tree') ; library(tree)}

# tree모형 추정
tree1 <- tree(TARGET ~ ., data=data_Train2)
plot(tree1); text(tree1)

# 추정한 모형으로 data_Test2의 TARGET 예측
tree1.pred <- predict(tree1,data_Test2, type = 'class'); tree1.pred

# F measure 계산
table(tree1.pred)
table(data_Test2$TARGET)
result1 <- table(data_Test2$TARGET, tree1.pred)
Precision1 <- result1[4] / (result1[3] + result1[4])
Recall1 <- result1[4] / (result1[2] + result1[4])
f.value1 <- 2 * (Precision1 * Recall1) / (Precision1 + Recall1)
f.value1


######################################################################################
################################## 2. rpart함수 이용 #################################
# rpart함수 불순도 지표 : 지니지수
if(!require(rpart)){install.packages('rpart') ; library(rpart)}

# tree모형 추정
tree2 <- rpart(TARGET ~ ., data=data_Train2, method="class")
plot(tree2); text(tree2)
print(tree2) 

# tree 가지치기
fit <- prune(tree2, cp = 0.02)
par(mar = rep(0.1, 4))
plot(fit, branch = 0.3, compress = TRUE);text(fit)

# 추정한 모형으로 data_Test2의 TARGET 예측
tree2.pred <- predict(fit,data_Test2, type = 'class'); tree2.pred

# F measure 계산
table(tree2.pred)
table(data_Test2$TARGET)
result2 <- table(data_Test2$TARGET, tree2.pred)
Precision2 <- result2[4] / (result2[3] + result2[4])
Recall2 <- result2[4] / (result2[2] + result2[4])
f.value2 <- 2 * (Precision2 * Recall2) / (Precision2 + Recall2)
f.value2


######################################################################################
################################## 3. pltree함수 이용 ################################
if(!require(cluster)){install.packages('cluster') ; library(cluster)}

agn <- agnes(data_Train2)
tree3 <- pltree(agn)

# 추정한 모형으로 data_Test2의 TARGET 예측
tree3.pred <- predict(tree3, data_Test2, type = 'class'); tree3.pred

# F measure 계산
table(tree3.pred)
table(data_Test2$TARGET)
result3 <- table(data_Test2$TARGET, tree3.pred)
Precision3 <- result3[4] / (result3[3] + result3[4])
Recall3 <- result3[4] / (result3[2] + result3[4])
f.value3 <- 2 * (Precision3 * Recall3) / (Precision3 + Recall3)
f.value3


######################################################################################
##################################  4. randomForest ##################################

if(!require(randomForest)){install.packages('randomForest') ; library(randomForest)}

# tree모형 추정
tree4 <- randomForest(TARGET ~ ., data=data_Train2, importance = TRUE, ntree=100) #ntree=1000 : 예측모형의 개수
plot(tree4)
varImpPlot(tree4)

# 추정한 모형으로 data_Test2의 TARGET 예측
tree4.pred <- predict(tree4, data_Test2) #데이터를 설정하지 않으면 OOB prediction을 이용

# F measure 계산
table(tree4.pred)
table(data_Test2$TARGET)
result4 <- table(data_Test2$TARGET, tree4.pred)
Precision4 <- result4[4] / (result4[3] + result4[4])
Recall4 <- result4[4] / (result4[2] + result4[4])
f.value4 <- 2 * (Precision4 * Recall4) / (Precision4 + Recall4)
f.value4





######################################################################################
######################################################################################
# save_y : Bagging, tree함수를 이용해 test data prediction
# use_var : Bagging, tree함수를 이용해 중요 변수 추출

#동일한 비율로 타겟 변수 추출
data_od = data_factor[which(data_factor$TARGET == 1),]
data_rp = data_factor[which(data_factor$TARGET == 0),]

set.seed(123)

# 예측해야하는 TEST SET을 준비
randomNumber_od_test = sample(1:4243, size = 848, replace = F)
randomNumber_rp_test = sample(1:94801, size = 19795, replace = F)

data_od_test = data_od[randomNumber_od_test,]
data_rp_test = data_rp[randomNumber_rp_test,]

data_Test = rbind(data_od_test, data_rp_test)
data_Test2 = subset(data_Test, select = -c(CUST_ID, MIN_CNTT_DATE, ARPU, TEL_CNTT_QTR))

# Bagging하기전에 기본값 설정
B = 10 #반복횟수
save_y = matrix(rep(0, 20643 * B), nrow = B) # B번 반복되는 동안 예측한 y값 저장 10X20643
use_var = matrix(rep(0, 20 * B), nrow = B) # B번 반복되는 동안 사용된 변수 저장 10X20

# B번 train data를 샘플링해 tree 모델을 추정 후 predict값을 저장
for (i in 1:B) {
  # train data를 샘플링
  randomNumber_od = sample(1:4243, size = 3395, replace = T)
  randomNumber_rp = sample(1:94801, size = 3395, replace = T)
  
  data_od_train = data_od[randomNumber_od,]
  data_rp_train = data_rp[randomNumber_rp,]
  
  data_Train = rbind(data_od_train, data_rp_train)
  data_Train2 = subset(data_Train, select = -c(CUST_ID, MIN_CNTT_DATE, ARPU, TEL_CNTT_QTR))
  
  # tree 모델 추정
  tree.fit <- tree(TARGET ~ ., data=data_Train2)
  
  # 사용된 변수 use_var에 저장
  use_var[i, 1:20] <- tree.fit$frame[1:20, 1]
  
  # 추정한 모델로 test data를 predict해서 save_y에 저장
  save_y[i, 1:20643] <- predict(tree.fit, data_Test2, type = 'class')
}

# 가장 많이 나온 값으로 test data에 대한 예측
save_y <- save_y - 1
pred_y <- rep(0, 20643)
for (i in 1:20643){
  temp <- table(as.vector(save_y[1:10, i]))
  pred_y[i] <- names(temp)[temp == max(temp)][1]
}

# F measure 계산
result <- table(data_Test2$TARGET, pred_y)
Precision <- result[4] / (result[3] + result[4])
Recall <- result[4] / (result[2] + result[4])
f.value <- 2 * (Precision * Recall) / (Precision + Recall)
f.value

# B번의 모델 추정에서 가장 많이 사용한 변수 10
use_variable <- matrix(names(data_Test2)[use_var], nrow = 10)




#####################################################################################
#SPTCT_OCCR_MDIF  SCI)대출정보 최근 개설일로부터 현재까지 유지기간[2산업분류-신용대출]
#CRDT_CARD_CNT    SCI)개설정보 현재 신용개설 총 건수[신용카드]
#PAYM_METDO       SKT)PAYM_METD납부방법
#LT1Y_MXOD_AMT    SKT)년간최대연체금액_원
#PAYM_METDK       SKT)PAYM_METD납부방법
#CRDT_OCCR_MDIF   SCI)대출정보 최근 개설일로부터 현재까지 유지기간[신용대출]
#SPART_LNIF_CNT   SCI)대출정보 현재 총 건수[2산업분류]
#CTCD_OCCR_MDIF   SCI)개설정보 최초 개설일로부터 현재까지 유지기간[신용카드]

#BNK_LNIF_AMT     SCI)대출정보 현재 총 금액[은행]
#TOT_LNIF_AMT     SCI)대출정보 현재 총 금액[신용대출]
#####################################################################################

## randomForest : names(sort(tree4$importance[1:64,3],decreasing = TRUE)[1:15])
