library(dplyr)
library(caret)
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
gc_tree <- gc_eda

####
gc_eda_over <- ovun.sample(RESPONSE~., data = gc_eda, method = "over", N = 1400)$data
table(gc_eda_over$RESPONSE)

# training set
set.seed(12345)
index.tr <- sample(1:nrow(gc_eda_over), size = 2/3*nrow(gc_eda_over), replace = FALSE)
df.tr <- gc_eda_over[index.tr,]
df.te <- gc_eda_over[-index.tr,]

set.seed(123456)
tree.over <- rpart(RESPONSE~., data = df.tr)
rpart.plot(tree.over)

pred.tree.over <- predict(tree.over, newdata = df.te)
roc.curve(df.te$RESPONSE, pred.tree.over[,2])

cp <- tree.over$cptable
opt <- which.min(tree.over$cptable[, "xerror"])
r <- cp[, 4][opt] + cp[, 5][opt]
rmin <- min(seq(1:dim(cp)[1])[cp[, 4] < r])
cp0 <- cp[rmin, 1]
cat("size chosen was", cp[rmin, 2] + 1, "\n")
plotcp(tree.over)
tree.over.prune <- prune(tree.over, cp = 0.012)

rpart.plot(tree.over.prune)

pred <- predict(tree.over.prune, newdata = df.te, type = "class")
confusionMatrix(data = pred, reference = df.te$RESPONSE)

# while we slighty reduced the accuracy and the sensitivity with the over-sampling, we can notice that the specificity more than double 
# we can also notice that the balanced accuracy has increased, suggesting a better overall model.                