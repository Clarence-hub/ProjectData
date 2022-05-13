library(nnet)
library(dplyr)
library(summarytools)
library(GGally)
library(DataExplorer)
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


## NN model
samp <- c(sample(1:300, 25), sample(301:600, 25), sample(601:1000, 25))
gc.training <- gc_nn[samp, ]
summary(gc.training)


iris.test <- iris[-samp, ]
summary(iris.test)

## fitting nn 
gc.net <- nnet(RESPONSE ~ ., data = gc.training, size = 2, rang = 0.1, decay = 5e-04,
                 Hess = T, maxit = 200)

eigen(gc.net$Hess, T)$values


library(NeuralNetTools)
par(mar = numeric(4), family = "serif")
plotnet(gc.net, pos_col = "darkgreen", neg_col = "darkblue")

str(gc_nn)


#### doing the same but with a training set
library(caret)
set.seed(1)
fitControl <- trainControl(method = "cv", 
                           #`cv` stands for `cross-validation` which we will 
                           #see during one of the upcoming courses
                           number = 10)

nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))

nnetFit <- train(RESPONSE~., 
                 data = gc_nn,
                 method = "nnet",
                 metric = "Accuracy",
                 tuneGrid = nnetGrid,
                 trControl = fitControl)

plot(nnetFit)

# visualizing the neural network
library(neuralnet)


#########

for (i in 1:ncol(gc_nn)){
  if(class(gc_nn[,i]) == "Factor"){
    gc_nn[,i] <- class.ind(gc_nn[,i])
  }
}
str(gc_nn)

####### changing the format of categorical variable for neural network
RESPONSE.hot <- class.ind(gc_nn$RESPONSE) 
FOREIGN.hot <- class.ind(gc_nn$FOREIGN) 
TELEPHONE.hot <- class.ind(gc_nn$TELEPHONE) 
JOB.hot <- class.ind(gc_nn$JOB) 
OWN_RES.hot <- class.ind(gc_nn$OWN_RES) 
OTHER_INSTALL.hot <- class.ind(gc_nn$OTHER_INSTALL) 
PROP_UNKN_NONE.hot <- class.ind(gc_nn$PROP_UNKN_NONE) 
REAL_ESTATE.hot <- class.ind(gc_nn$REAL_ESTATE) 
PRESENT_RESIDENT.hot <- class.ind(gc_nn$PRESENT_RESIDENT) 
GUARANTOR.hot <- class.ind(gc_nn$GUARANTOR) 
CO.APPLICANT.hot <- class.ind(gc_nn$CO.APPLICANT) 
MALE_MAR_OR_WID.hot <- class.ind(gc_nn$MALE_MAR_or_WID) 
MALE_SINGLE.hot <- class.ind(gc_nn$MALE_SINGLE) 
MALE_DIV.hot <- class.ind(gc_nn$MALE_DIV) 
EMPLOYMENT.hot <- class.ind(gc_nn$EMPLOYMENT) 
SAV_ACCT.hot <- class.ind(gc_nn$SAV_ACCT) 
RETRAINING.hot <- class.ind(gc_nn$RETRAINING) 
EDUCATION.hot <- class.ind(gc_nn$EDUCATION) 
RADIO.TV.hot <- class.ind(gc_nn$RADIO.TV) 
FURNITURE.hot <- class.ind(gc_nn$FURNITURE) 
USED_CAR.hot <- class.ind(gc_nn$USED_CAR) 
NEW_CAR.hot <- class.ind(gc_nn$NEW_CAR) 
CHK_ACCT.hot <- class.ind(gc_nn$CHK_ACCT) 
NUM_DEPENDENTS.hot <- class.ind(gc_nn$NUM_DEPENDENTS) 
NUM_CREDITS.hot <- class.ind(gc_nn$NUM_CREDITS) 
INSTALL_RATE.hot <- class.ind(gc_nn$INSTALL_RATE) 

gc_hot <- cbind(gc_nn[,c(1,2,4)], RESPONSE.hot, FOREIGN.hot, TELEPHONE.hot, JOB.hot,
                OWN_RES.hot, OTHER_INSTALL.hot, PROP_UNKN_NONE.hot, REAL_ESTATE.hot,
                PRESENT_RESIDENT.hot, GUARANTOR.hot, CO.APPLICANT.hot, MALE_MAR_OR_WID.hot,
                MALE_DIV.hot, MALE_SINGLE.hot, EMPLOYMENT.hot, SAV_ACCT.hot, RETRAINING.hot,
                EDUCATION.hot, RADIO.TV.hot, FURNITURE.hot, USED_CAR.hot, NEW_CAR.hot,
                CHK_ACCT.hot, NUM_DEPENDENTS.hot, NUM_CREDITS.hot, INSTALL_RATE.hot)
head(gc_hot)
f <- as.formula(paste("Yes+No ~", paste(names(gc_hot)[1:30], collapse = " + ")))
f
nn4 <- neuralnet(f, data=gc_hot, hidden=2)
nn5 <- neuralnet(f, data=gc_hot, hidden=c(2,3))
plot(nn4, rep="best")
plot(nn5, rep="best")

str(gc_nn)

gc_nn

?neuralnet
