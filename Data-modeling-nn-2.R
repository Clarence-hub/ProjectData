library(nnet)
library(dplyr)
library(summarytools)
library(GGally)
library(DataExplorer)
library(neuralnet)
library(NeuralNetTools)
gc <- read.csv("GermanCredit.csv", sep = ";")
gc$RESPONSE <- recode_factor(gc$RESPONSE, "1" = "Yes",
                             "0" = "No")

str(gc)
gc_1 <- gc %>% select(-1)
gc_1 <- gc_1 %>% select(2,10,13,22,26,28)
gc_2 <- gc %>% select(-1,-3,-11,-14,-23,-27,-29)
str(gc_1)


for (i in 1:ncol(gc_2)){
  if (class(gc_2[,i])=="integer"){
    gc_2[,i] <- factor(gc_2[,i])
  }
}
str(gc_2)
gc_eda <- cbind(gc_1, gc_2)
gc_eda$NUM_DEPENDENTS <- factor(gc_eda$NUM_DEPENDENTS)
gc_eda$INSTALL_RATE <- factor(gc_eda$INSTALL_RATE)
gc_eda$NUM_CREDITS <- factor(gc_eda$NUM_CREDITS)

#Min Max Normalization
gc_eda$DURATION <-(gc_eda$DURATION-min(gc_eda$DURATION)) /
  (max(gc_eda$DURATION)-min(gc_eda$DURATION))
gc_eda$AMOUNT <-(gc_eda$AMOUNT-min(gc_eda$AMOUNT)) /
  (max(gc_eda$AMOUNT)-min(gc_eda$AMOUNT))
gc_eda$AGE <-(gc_eda$AGE-min(gc_eda$AGE)) /
  (max(gc_eda$AGE)-min(gc_eda$AGE))

gc_nn <- gc_eda

#### fitting neural network
# training set
index.tr <- sample(1:nrow(gc_nn), size = 2/3*nrow(gc_nn), replace = FALSE)
gc.tr <- gc_nn[index.tr,]
gc.te <- gc_nn[-index.tr,]
summary(gc.tr)

gc_net <- nnet(RESPONSE~., data = gc.tr, size = 2, range = 0.1, decay = 5e-04,
               Hess = T, maxit = 200)
eigen(gc_net$Hess)$values

par(mar = numeric(4), family = "serif")
plotnet(gc_net, pos_col = "darkgreen", neg_col = "darkblue")


# binarize the categorical variables in training

################# Clark you better do it all 
nnet_gctrain <- gc.tr
nnet_gctest <- gc.te
# binarize the categorical output
# RESPONSE
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$RESPONSE == "Yes")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$RESPONSE == "No")
names(nnet_gctrain)[32] <- "Response_Yes"
names(nnet_gctrain)[33] <- "Response_No"

# FOREIGN
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$FOREIGN == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$FOREIGN == "0")
names(nnet_gctrain)[34] <- "Foreigner_Yes"
names(nnet_gctrain)[35] <- "Foreigner_No"

# TELEPHONE
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$TELEPHONE == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$TELEPHONE == "0")
names(nnet_gctrain)[36] <- "Telephone_Yes"
names(nnet_gctrain)[37] <- "Telephone_No"

# Job
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$JOB == "0")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$JOB == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$JOB == "2")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$JOB == "3")
names(nnet_gctrain)[38] <- "Job_0"
names(nnet_gctrain)[39] <- "Job_1"
names(nnet_gctrain)[40] <- "Job_2"
names(nnet_gctrain)[41] <- "Job_3"

# OWN_RES
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$OWN_RES == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$OWN_RES == "0")
names(nnet_gctrain)[42] <- "OWN_RES_Yes"
names(nnet_gctrain)[43] <- "OWN_RES_No"

# OTHER_INSTALL
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$OTHER_INSTALL == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$OTHER_INSTALL == "0")
names(nnet_gctrain)[44] <- "OTHER_INSTALL_Yes"
names(nnet_gctrain)[45] <- "OTHER_INSTALL_No"

# PROP_UNKN_NONE
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$PROP_UNKN_NONE == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$PROP_UNKN_NONE == "0")
names(nnet_gctrain)[46] <- "PROP_UNKN_NONE_Yes"
names(nnet_gctrain)[47] <- "PROP_UNKN_NONE_No"

# REAL_ESTATE
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$REAL_ESTATE == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$REAL_ESTATE == "0")
names(nnet_gctrain)[48] <- "REAL_ESTATE_Yes"
names(nnet_gctrain)[49] <- "REAL_ESTATE_No"

# PRESENT_RESIDENT
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$PRESENT_RESIDENT == "0")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$PRESENT_RESIDENT == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$PRESENT_RESIDENT == "2")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$PRESENT_RESIDENT == "3")
names(nnet_gctrain)[50] <- "PRESENT_RESIDENT_0"
names(nnet_gctrain)[51] <- "PRESENT_RESIDENT_1"
names(nnet_gctrain)[52] <- "PRESENT_RESIDENT_2"
names(nnet_gctrain)[53] <- "PRESENT_RESIDENT_3"

# GUARANTOR
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$GUARANTOR == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$GUARANTOR == "0")
names(nnet_gctrain)[54] <- "GUARANTOR_Yes"
names(nnet_gctrain)[55] <- "GUARANTOR_No"

# CO.APPLICANT
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$CO.APPLICANT == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$CO.APPLICANT == "0")
names(nnet_gctrain)[56] <- "CO.APPLICANT_Yes"
names(nnet_gctrain)[57] <- "CO.APPLICANT_No"

#MALE_MAR_OR_WID
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$MALE_MAR_or_WID == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$MALE_MAR_or_WID == "0")
names(nnet_gctrain)[58] <- "MALE_MAR_or_WID_Yes"
names(nnet_gctrain)[59] <- "MALE_MAR_or_WID_No"

# MALE_SINGLE
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$MALE_SINGLE == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$MALE_SINGLE == "0")
names(nnet_gctrain)[60] <- "MALE_SINGLE_Yes"
names(nnet_gctrain)[61] <- "MALE_SINGLE_No"

# MALE_DIV
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$MALE_DIV == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$MALE_DIV == "0")
names(nnet_gctrain)[62] <- "MALE_DIV_Yes"
names(nnet_gctrain)[63] <- "MALE_DIV_No"

# EMPLOYMENT
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$EMPLOYMENT == "0")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$EMPLOYMENT == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$EMPLOYMENT == "2")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$EMPLOYMENT == "3")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$EMPLOYMENT == "4")
names(nnet_gctrain)[64] <- "EMPLOYMENT_0"
names(nnet_gctrain)[65] <- "EMPLOYMENT_1"
names(nnet_gctrain)[66] <- "EMPLOYMENT_2"
names(nnet_gctrain)[67] <- "EMPLOYMENT_3"
names(nnet_gctrain)[68] <- "EMPLOYMENT_4"

# SAV_ACCT
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$SAV_ACCT == "0")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$SAV_ACCT == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$SAV_ACCT == "2")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$SAV_ACCT == "3")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$SAV_ACCT == "4")
names(nnet_gctrain)[69] <- "SAV_ACCT_0"
names(nnet_gctrain)[70] <- "SAV_ACCT_1"
names(nnet_gctrain)[71] <- "SAV_ACCT_2"
names(nnet_gctrain)[72] <- "SAV_ACCT_3"
names(nnet_gctrain)[73] <- "SAV_ACCT_4"

# RETRAINING
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$RETRAINING == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$RETRAINING == "0")
names(nnet_gctrain)[74] <- "RETRAINING_Yes"
names(nnet_gctrain)[75] <- "RETRAINING_No"

# EDUCATION
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$EDUCATION == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$EDUCATION == "0")
names(nnet_gctrain)[76] <- "EDUCATION_Yes"
names(nnet_gctrain)[77] <- "EDUCATION_No"

# RADIO
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$RADIO.TV == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$RADIO.TV == "0")
names(nnet_gctrain)[78] <- "RADIO.TV_Yes"
names(nnet_gctrain)[79] <- "RADIO.TV_No"

# FURNITURE
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$FURNITURE == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$FURNITURE == "0")
names(nnet_gctrain)[80] <- "FURNITURE_Yes"
names(nnet_gctrain)[81] <- "FURNITURE_No"

# USED_CAR
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$USED_CAR == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$USED_CAR == "0")
names(nnet_gctrain)[82] <- "USED_CAR_Yes"
names(nnet_gctrain)[83] <- "USED_CAR_No"

# NEW_CAR
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$NEW_CAR == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$NEW_CAR == "0")
names(nnet_gctrain)[84] <- "NEW_CAR_Yes"
names(nnet_gctrain)[85] <- "NEW_CAR_No"

# CHK_ACCT
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$CHK_ACCT == "0")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$CHK_ACCT == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$CHK_ACCT == "2")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$CHK_ACCT == "3")
names(nnet_gctrain)[86] <- "CHK_ACCT_0"
names(nnet_gctrain)[87] <- "CHK_ACCT_1"
names(nnet_gctrain)[88] <- "CHK_ACCT_2"
names(nnet_gctrain)[89] <- "CHK_ACCT_3"

# NUM_DEPENDENTS
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$NUM_DEPENDENTS == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$NUM_DEPENDENTS == "2")
names(nnet_gctrain)[90] <- "NUM_DEPENDENTS_1"
names(nnet_gctrain)[91] <- "NUM_DEPENDENTS_2"

# NUM_CREDIT
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$NUM_CREDITS == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$NUM_CREDITS == "2")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$NUM_CREDITS == "3")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$NUM_CREDITS == "4")
names(nnet_gctrain)[92] <- "NUM_CREDITS_1"
names(nnet_gctrain)[93] <- "NUM_CREDITS_2"
names(nnet_gctrain)[94] <- "NUM_CREDITS_3"
names(nnet_gctrain)[95] <- "NUM_CREDITS_4"

#
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$INSTALL_RATE == "1")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$INSTALL_RATE == "2")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$INSTALL_RATE == "3")
nnet_gctrain <- cbind(nnet_gctrain, gc.tr$INSTALL_RATE == "4")
names(nnet_gctrain)[96] <- "INSTALL_RATE_1"
names(nnet_gctrain)[97] <- "INSTALL_RATE_2"
names(nnet_gctrain)[98] <- "INSTALL_RATE_3"
names(nnet_gctrain)[99] <- "INSTALL_RATE_4"

summary(nnet_gctrain)

##########  binarize in test 

# RESPONSE
nnet_gctest <- cbind(nnet_gctest, gc.te$RESPONSE == "Yes")
nnet_gctest <- cbind(nnet_gctest, gc.te$RESPONSE == "No")
names(nnet_gctest)[32] <- "Response_Yes"
names(nnet_gctest)[33] <- "Response_No"

# FOREIGN
nnet_gctest <- cbind(nnet_gctest, gc.te$FOREIGN == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$FOREIGN == "0")
names(nnet_gctest)[34] <- "Foreigner_Yes"
names(nnet_gctest)[35] <- "Foreigner_No"

# TELEPHONE
nnet_gctest <- cbind(nnet_gctest, gc.te$TELEPHONE == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$TELEPHONE == "0")
names(nnet_gctest)[36] <- "Telephone_Yes"
names(nnet_gctest)[37] <- "Telephone_No"

# Job
nnet_gctest <- cbind(nnet_gctest, gc.te$JOB == "0")
nnet_gctest <- cbind(nnet_gctest, gc.te$JOB == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$JOB == "2")
nnet_gctest <- cbind(nnet_gctest, gc.te$JOB == "3")
names(nnet_gctest)[38] <- "Job_0"
names(nnet_gctest)[39] <- "Job_1"
names(nnet_gctest)[40] <- "Job_2"
names(nnet_gctest)[41] <- "Job_3"

# OWN_RES
nnet_gctest <- cbind(nnet_gctest, gc.te$OWN_RES == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$OWN_RES == "0")
names(nnet_gctest)[42] <- "OWN_RES_Yes"
names(nnet_gctest)[43] <- "OWN_RES_No"

# OTHER_INSTALL
nnet_gctest <- cbind(nnet_gctest, gc.te$OTHER_INSTALL == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$OTHER_INSTALL == "0")
names(nnet_gctest)[44] <- "OTHER_INSTALL_Yes"
names(nnet_gctest)[45] <- "OTHER_INSTALL_No"

# PROP_UNKN_NONE
nnet_gctest <- cbind(nnet_gctest, gc.te$PROP_UNKN_NONE == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$PROP_UNKN_NONE == "0")
names(nnet_gctest)[46] <- "PROP_UNKN_NONE_Yes"
names(nnet_gctest)[47] <- "PROP_UNKN_NONE_No"

# REAL_ESTATE
nnet_gctest <- cbind(nnet_gctest, gc.te$REAL_ESTATE == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$REAL_ESTATE == "0")
names(nnet_gctest)[48] <- "REAL_ESTATE_Yes"
names(nnet_gctest)[49] <- "REAL_ESTATE_No"

# PRESENT_RESIDENT
nnet_gctest <- cbind(nnet_gctest, gc.te$PRESENT_RESIDENT == "0")
nnet_gctest <- cbind(nnet_gctest, gc.te$PRESENT_RESIDENT == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$PRESENT_RESIDENT == "2")
nnet_gctest <- cbind(nnet_gctest, gc.te$PRESENT_RESIDENT == "3")
names(nnet_gctest)[50] <- "PRESENT_RESIDENT_0"
names(nnet_gctest)[51] <- "PRESENT_RESIDENT_1"
names(nnet_gctest)[52] <- "PRESENT_RESIDENT_2"
names(nnet_gctest)[53] <- "PRESENT_RESIDENT_3"

# GUARANTOR
nnet_gctest <- cbind(nnet_gctest, gc.te$GUARANTOR == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$GUARANTOR == "0")
names(nnet_gctest)[54] <- "GUARANTOR_Yes"
names(nnet_gctest)[55] <- "GUARANTOR_No"

# CO.APPLICANT
nnet_gctest <- cbind(nnet_gctest, gc.te$CO.APPLICANT == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$CO.APPLICANT == "0")
names(nnet_gctest)[56] <- "CO.APPLICANT_Yes"
names(nnet_gctest)[57] <- "CO.APPLICANT_No"

#MALE_MAR_OR_WID
nnet_gctest <- cbind(nnet_gctest, gc.te$MALE_MAR_or_WID == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$MALE_MAR_or_WID == "0")
names(nnet_gctest)[58] <- "MALE_MAR_or_WID_Yes"
names(nnet_gctest)[59] <- "MALE_MAR_or_WID_No"

# MALE_SINGLE
nnet_gctest <- cbind(nnet_gctest, gc.te$MALE_SINGLE == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$MALE_SINGLE == "0")
names(nnet_gctest)[60] <- "MALE_SINGLE_Yes"
names(nnet_gctest)[61] <- "MALE_SINGLE_No"

# MALE_DIV
nnet_gctest <- cbind(nnet_gctest, gc.te$MALE_DIV == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$MALE_DIV == "0")
names(nnet_gctest)[62] <- "MALE_DIV_Yes"
names(nnet_gctest)[63] <- "MALE_DIV_No"

# EMPLOYMENT
nnet_gctest <- cbind(nnet_gctest, gc.te$EMPLOYMENT == "0")
nnet_gctest <- cbind(nnet_gctest, gc.te$EMPLOYMENT == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$EMPLOYMENT == "2")
nnet_gctest <- cbind(nnet_gctest, gc.te$EMPLOYMENT == "3")
nnet_gctest <- cbind(nnet_gctest, gc.te$EMPLOYMENT == "4")
names(nnet_gctest)[64] <- "EMPLOYMENT_0"
names(nnet_gctest)[65] <- "EMPLOYMENT_1"
names(nnet_gctest)[66] <- "EMPLOYMENT_2"
names(nnet_gctest)[67] <- "EMPLOYMENT_3"
names(nnet_gctest)[68] <- "EMPLOYMENT_4"

# SAV_ACCT
nnet_gctest <- cbind(nnet_gctest, gc.te$SAV_ACCT == "0")
nnet_gctest <- cbind(nnet_gctest, gc.te$SAV_ACCT == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$SAV_ACCT == "2")
nnet_gctest <- cbind(nnet_gctest, gc.te$SAV_ACCT == "3")
nnet_gctest <- cbind(nnet_gctest, gc.te$SAV_ACCT == "4")
names(nnet_gctest)[69] <- "SAV_ACCT_0"
names(nnet_gctest)[70] <- "SAV_ACCT_1"
names(nnet_gctest)[71] <- "SAV_ACCT_2"
names(nnet_gctest)[72] <- "SAV_ACCT_3"
names(nnet_gctest)[73] <- "SAV_ACCT_4"

# RETRAINING
nnet_gctest <- cbind(nnet_gctest, gc.te$RETRAINING == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$RETRAINING == "0")
names(nnet_gctest)[74] <- "RETRAINING_Yes"
names(nnet_gctest)[75] <- "RETRAINING_No"

# EDUCATION
nnet_gctest <- cbind(nnet_gctest, gc.te$EDUCATION == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$EDUCATION == "0")
names(nnet_gctest)[76] <- "EDUCATION_Yes"
names(nnet_gctest)[77] <- "EDUCATION_No"

# RADIO
nnet_gctest <- cbind(nnet_gctest, gc.te$RADIO.TV == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$RADIO.TV == "0")
names(nnet_gctest)[78] <- "RADIO.TV_Yes"
names(nnet_gctest)[79] <- "RADIO.TV_No"

# FURNITURE
nnet_gctest <- cbind(nnet_gctest, gc.te$FURNITURE == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$FURNITURE == "0")
names(nnet_gctest)[80] <- "FURNITURE_Yes"
names(nnet_gctest)[81] <- "FURNITURE_No"

# USED_CAR
nnet_gctest <- cbind(nnet_gctest, gc.te$USED_CAR == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$USED_CAR == "0")
names(nnet_gctest)[82] <- "USED_CAR_Yes"
names(nnet_gctest)[83] <- "USED_CAR_No"

# NEW_CAR
nnet_gctest <- cbind(nnet_gctest, gc.te$NEW_CAR == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$NEW_CAR == "0")
names(nnet_gctest)[84] <- "NEW_CAR_Yes"
names(nnet_gctest)[85] <- "NEW_CAR_No"

# CHK_ACCT
nnet_gctest <- cbind(nnet_gctest, gc.te$CHK_ACCT == "0")
nnet_gctest <- cbind(nnet_gctest, gc.te$CHK_ACCT == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$CHK_ACCT == "2")
nnet_gctest <- cbind(nnet_gctest, gc.te$CHK_ACCT == "3")
names(nnet_gctest)[86] <- "CHK_ACCT_0"
names(nnet_gctest)[87] <- "CHK_ACCT_1"
names(nnet_gctest)[88] <- "CHK_ACCT_2"
names(nnet_gctest)[89] <- "CHK_ACCT_3"

# NUM_DEPENDENTS
nnet_gctest <- cbind(nnet_gctest, gc.te$NUM_DEPENDENTS == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$NUM_DEPENDENTS == "2")
names(nnet_gctest)[90] <- "NUM_DEPENDENTS_1"
names(nnet_gctest)[91] <- "NUM_DEPENDENTS_2"

# NUM_CREDIT
nnet_gctest <- cbind(nnet_gctest, gc.te$NUM_CREDITS == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$NUM_CREDITS == "2")
nnet_gctest <- cbind(nnet_gctest, gc.te$NUM_CREDITS == "3")
nnet_gctest <- cbind(nnet_gctest, gc.te$NUM_CREDITS == "4")
names(nnet_gctest)[92] <- "NUM_CREDITS_1"
names(nnet_gctest)[93] <- "NUM_CREDITS_2"
names(nnet_gctest)[94] <- "NUM_CREDITS_3"
names(nnet_gctest)[95] <- "NUM_CREDITS_4"

#
nnet_gctest <- cbind(nnet_gctest, gc.te$INSTALL_RATE == "1")
nnet_gctest <- cbind(nnet_gctest, gc.te$INSTALL_RATE == "2")
nnet_gctest <- cbind(nnet_gctest, gc.te$INSTALL_RATE == "3")
nnet_gctest <- cbind(nnet_gctest, gc.te$INSTALL_RATE == "4")
names(nnet_gctest)[96] <- "INSTALL_RATE_1"
names(nnet_gctest)[97] <- "INSTALL_RATE_2"
names(nnet_gctest)[98] <- "INSTALL_RATE_3"
names(nnet_gctest)[99] <- "INSTALL_RATE_4"

#######
nn <- neuralnet(Response_Yes + Response_No ~ DURATION + AMOUNT + AGE + 
                  Foreigner_Yes + Foreigner_No + Telephone_Yes + Telephone_No +
                  Job_0 + Job_1 + Job_2 + Job_3 + OWN_RES_Yes + OWN_RES_No +
                  OTHER_INSTALL_Yes + OTHER_INSTALL_No + PROP_UNKN_NONE_Yes +
                  PROP_UNKN_NONE_No + REAL_ESTATE_Yes + REAL_ESTATE_No +
                  PRESENT_RESIDENT_0 + PRESENT_RESIDENT_1 + PRESENT_RESIDENT_2 +
                  PRESENT_RESIDENT_3 +GUARANTOR_Yes + GUARANTOR_No +
                  CO.APPLICANT_Yes + CO.APPLICANT_No + MALE_MAR_or_WID_Yes + MALE_MAR_or_WID_No +
                  MALE_SINGLE_Yes + MALE_SINGLE_No+ MALE_DIV_Yes + MALE_DIV_No +
                  EMPLOYMENT_0 + EMPLOYMENT_1 + EMPLOYMENT_2 + EMPLOYMENT_3 +
                  EMPLOYMENT_4 + SAV_ACCT_0 + SAV_ACCT_1 + SAV_ACCT_2 + SAV_ACCT_3+
                  SAV_ACCT_4 + RETRAINING_Yes + RETRAINING_No + EDUCATION_Yes + EDUCATION_No +
                  RADIO.TV_Yes + RADIO.TV_No + FURNITURE_Yes + FURNITURE_No +USED_CAR_Yes + USED_CAR_No +
                  NEW_CAR_Yes + NEW_CAR_No + CHK_ACCT_0 + CHK_ACCT_1 + CHK_ACCT_2 + CHK_ACCT_3  +
                  NUM_DEPENDENTS_1 + NUM_DEPENDENTS_2 +NUM_CREDITS_1 + NUM_CREDITS_2 +NUM_CREDITS_3 + NUM_CREDITS_4 +
                  INSTALL_RATE_1 + INSTALL_RATE_2 + INSTALL_RATE_3 + INSTALL_RATE_4, data = nnet_gctrain, hidden = c(2))

plot(nn, rep = "best", cex = 0.8)
str(nnet_gctrain)


?plot
#############







###########


###### using caret
library(caret)
set.seed(10666)
val_index <- createDataPartition(gc_nn$RESPONSE, p = 0.5, list = FALSE)
TrainData <- gc_nn[val_index, ]
TrainClasses <- gc_nn[val_index, 5]
head(TrainData)

# test
TestData <- gc_nn[-val_index, ]
TestClasses <- gc_nn[-val_index, 5]
head(TestData)

# neural network

nnetFit <- caret::train(RESPONSE ~ ., data = nnet_gctrain, method = "nnet", preProcess = "range",
                        trace = FALSE, trControl = trainControl(method = "cv"))

nnetFit
plotnet(nnetFit)

confusionMatrix(predict(nnetFit, nnet_gctest[, -5]), TestClasses)