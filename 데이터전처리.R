############################## data_set : 원본 데이터 ##############################
data_set <- read.table('D:/빅콘테스트/bigcontest/Data_set.csv', header = T, sep = ',', stringsAsFactors= F)

head(data_set)
summary(data_set)




######################### data_na : na|null처리한 데이터 #########################
data_na <- data_set

### NULL 처리
# OCCP_NAME_G : '*' -> NA
data_na$OCCP_NAME_G[data_na$OCCP_NAME_G == '*'] <- NA
summary(data_na$OCCP_NAME_G)

# MATE_OCCP_NAME_G : '*' -> NA
data_na$MATE_OCCP_NAME_G[data_na$MATE_OCCP_NAME_G == '*'] <- NA
summary(data_na$MATE_OCCP_NAME_G)

# AGE : '*' -> NA
data_na$AGE[data_na$AGE == '*'] <- NA
summary(data_na$AGE)

# SEX : '*' -> NA
data_na$SEX[data_na$SEX == '*'] <- NA
summary(data_na$SEX)

# LAST_CHLD_AGE : 'NULL' -> NULL ?????????????????????????안됨
data_na$LAST_CHLD_AGE[which(data_na$LAST_CHLD_AGE == 'NULL')] <- NULL

idx <- which(data_na$LAST_CHLD_AGE == 'NULL')
data_na$LAST_CHLD_AGE[23] <- NULL
which(data_na$LAST_CHLD_AGE == 'NULL')
is.null(data_na$LAST_CHLD_AGE[23])
length(data_na$LAST_CHLD_AGE)
length(NULL)
summary(data_na$LAST_CHLD_AGE)

# TEL_MBSP_GRAD / PAYM_METD : ' ' -> NULL ?????????????????????????

###data_na 범주화(as.factor) 후 na|null처리한 데이터 확인
summary(data_na)




############################## data_narm : 결측값 있는 자료 삭제 + CUST_ID 삭제 ##############################
# 결측값 있는 자료 삭제-방법1
data_narm <- data_na[which(!is.na(data_na$OCCP_NAME_G) & !is.na(data_na$MATE_OCCP_NAME_G)
                           & !is.na(data_na$AGE) & !is.na(data_na$SEX)), ]
# 결측값 있는 자료 삭제-방법2
data_narm <- na.omit(data_na)
summary(data_narm)
# CUST_ID 삭제
data_narm <- data_narm[,-1]
summary(data_narm)




############################## data_factor : 범주화 변수 변환 ##############################
data_factor <- data_narm

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



############################## 데이터 write ##############################
write.table(data_factor, "data_factor.csv", sep=",", col.names=TRUE) 
write.table(data_na, "data_na.csv", sep=",", col.names=TRUE) 
write.table(data_narm, "data_narm.csv", sep=",", col.names=TRUE) 


