library(rpart)
library(rpart.plot)

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

set.seed(1234)
gc_ct <- rpart(RESPONSE~., method = "class", data = gc_tree, 
               control = rpart.control(minsplit = 4, cp = 1e-05, model = TRUE))
summary(gc_ct)

par(pty = "s", mar = c(1, 1, 1, 1))
plot(gc_ct, cex = 1)
text(gc_ct, cex = 0.6)

# as of now, the tree is not readable
# we're going to prune it as much as possible

# first let's take a look at the complexity table
par(mar = c(2, 1, 2, 1))
options(digits = 5)
printcp(gc_ct)
plotcp(gc_ct)


par(pty = "s")
plotcp(gc_ct)
with(gc_ct, {
  lines(cptable[, 2] + 1, cptable[, 3], type = "b", col = "red")
  legend(2.7, 1, c("Resub. Error", "CV Error", "min(CV Error)+1SE"), lty = c(1,
                                                                             1, 2), col = c("red", "black", "black"), bty = "n", cex = 0.8)
})

# we can assume that a tree with 5 split should be enough 
# the cp value should be between (0.01833 + 0.01667)/2 and 0.01833.
# therefore we're going to use : 0.018165 which correspond to the average between 5 split and 4

# looking at it through formula
cp <- gc_ct$cptable
opt <- which.min(gc_ct$cptable[, "xerror"])
r <- cp[, 4][opt] + cp[, 5][opt]
rmin <- min(seq(1:dim(cp)[1])[cp[, 4] < r])
cp0 <- cp[rmin, 1]
cat("size chosen was", cp[rmin, 2] + 1, "\n")

# indeed the size chosen is still 5

# pruning the tree
gc_prune <- prune(gc_ct, cp = 0.018165)
summary(gc_prune)
plot(gc_prune)
text(gc_prune)


# prediction

library(knitr)
gc_pred <- predict(gc_prune, type = "class")
table(gc_pred, gc_tree$RESPONSE)
# the prediction are pretty bad, especially for the false positive
kable(table(gc_pred, gc_tree$RESPONSE), caption = "Prediction table.")

library(gmodels)
CrossTable(x = gc_tree$RESPONSE, y = gc_pred, prop.chisq = FALSE)


############ doing the same but with training set
library(dplyr)

set.seed(12345)
index.tr <- sample(1:nrow(gc_tree), size = 2/3*nrow(gc_tree), replace = FALSE)
df.tr <- gc_tree[index.tr,]
df.te<- gc_tree[-index.tr,]

## tree
set.seed(123456)
gc_tree_1 <- rpart(RESPONSE~., data = df.tr)
rpart.plot(gc_tree_1)

# pruning the tree
plotcp(gc_tree_1)
# the value has to be between cp 0.034 and cp 0.016 
# therefore we will use cp = 0.025
gc_tree_1_prune <- prune(gc_tree_1, cp = 0.025)
rpart.plot(gc_tree_1_prune)

# now let's take a look at the prediction
pred <- predict(gc_tree_1_prune, newdata = df.tee, type = "class")
confusionMatrix(data = pred, reference = df.tee$RESPONSE)
table(Pred = pred, Obs = df.te$RESPONSE)

######### using the sampling method &  checking which model offer the best accuracy.
gc_eda_over <- ovun.sample(RESPONSE~., data = df.tr, method = "over", N = 926)$data
table(gc_eda_over$RESPONSE)

# under-sampling
gc_eda_under <- ovun.sample(RESPONSE~., data = df.tr, method = "under", N = 406, seed = 1)$data
table(gc_eda_under$RESPONSE)

# both
gc_eda_balanced_both <- ovun.sample(RESPONSE~., data = df.tr, method = "both", p = 0.5, N = 666, seed = 1)$data
table(gc_eda_balanced_both$RESPONSE)

# ROSE
gc_eda_ROSE <- ROSE(RESPONSE~., data = df.tr, seed = 1)$data
table(gc_eda_ROSE$RESPONSE)


########
tree.rose <- rpart(RESPONSE~., data = gc_eda_ROSE)
tree.both <- rpart(RESPONSE~., data = gc_eda_balanced_both)
tree.over <- rpart(RESPONSE~., data = gc_eda_over)
tree.under <- rpart(RESPONSE~., data = gc_eda_under)

# prediction
pred.tree.rose <- predict(tree.rose, newdata = df.te)
pred.tree.both <- predict(tree.both, newdata = df.te)
pred.tree.over <- predict(tree.over, newdata = df.te)
pred.tree.under <- predict(tree.under, newdata = df.te)

# AUC
# rose
roc.curve(df.te$RESPONSE, pred.tree.rose[,2])
# both
roc.curve(df.te$RESPONSE, pred.tree.both[,2])
# over
roc.curve(df.te$RESPONSE, pred.tree.over[,2])
# under
roc.curve(df.te$RESPONSE, pred.tree.under[,2])
# we can conclude that for the CART, we will use the under-sampling method

# CART model
rpart.plot(tree.under)
# we want to check for pruning
plotcp(tree.under)
# let's take a look through a formula
# looking at it through formula
cp <- tree.under$cptable
opt <- which.min(tree.under$cptable[, "xerror"])
r <- cp[, 4][opt] + cp[, 5][opt]
rmin <- min(seq(1:dim(cp)[1])[cp[, 4] < r])
cp0 <- cp[rmin, 1]
cat("size chosen was", cp[rmin, 2] + 1, "\n")
# we will therefore use a tree of size 11
tree.under.prune <- prune(tree.under, cp = 0.0555)
rpart.plot(tree.under.prune)

# now let's take a glance at the final accuracy mesusre
tree.under.prune.predict <- predict(tree.under.prune, newdata = df.te, type = "class")
table(Pred = tree.under.prune.predict, Obs = df.te$RESPONSE)

confusionMatrix(data = tree.under.prune.predict, df.te$RESPONSE)



#### comparing 
set.seed(123)

test.tr <- sample(1:nrow(gc_tree), size = 2/3*nrow(gc_tree), replace = FALSE)
da.tr <- gc_tree[test.tr,]
da.te<- gc_tree[-test.tr,]

set.seed(123)
tree.test <- rpart(RESPONSE~., data = da.tr)
rpart.plot(tree.test)

# doing the same but for over
rpart.plot(tree.over)
# we want to check for pruning
plotcp(tree.over)
# let's take a look through a formula
# looking at it through formula
cp <- tree.over$cptable
opt <- which.min(tree.over$cptable[, "xerror"])
r <- cp[, 4][opt] + cp[, 5][opt]
rmin <- min(seq(1:dim(cp)[1])[cp[, 4] < r])
cp0 <- cp[rmin, 1]
cat("size chosen was", cp[rmin, 2] + 1, "\n")
# we will therefore use a tree of size 11
tree.over.prune <- prune(tree.over, cp = 0,0165)
rpart.plot(tree.over.prune)

# now let's take a glance at the final accuracy mesure
tree.over.prune.predict <- predict(tree.over.prune, newdata = df.te, type = "class")
table(Pred = tree.over.prune.predict, Obs = df.te$RESPONSE)

### doing for both


rpart.plot(tree.both)
# we want to check for pruning
plotcp(tree.both)
# let's take a look through a formula
# looking at it through formula
cp <- tree.both$cptable
opt <- which.min(tree.both$cptable[, "xerror"])
r <- cp[, 4][opt] + cp[, 5][opt]
rmin <- min(seq(1:dim(cp)[1])[cp[, 4] < r])
cp0 <- cp[rmin, 1]
cat("size chosen was", cp[rmin, 2] + 1, "\n")
# we will therefore use a tree of size 11
tree.both.prune <- prune(tree.both, cp = 0,010)
rpart.plot(tree.both.prune)

# now let's take a glance at the final accuracy mesure
tree.both.prune.predict <- predict(tree.both.prune, newdata = df.te, type = "class")
table(Pred = tree.both.prune.predict, Obs = df.te$RESPONSE)


# rose

rpart.plot(tree.rose)
# we want to check for pruning
plotcp(tree.rose)
# let's take a look through a formula
# looking at it through formula
cp <- tree.rose$cptable
opt <- which.min(tree.rose$cptable[, "xerror"])
r <- cp[, 4][opt] + cp[, 5][opt]
rmin <- min(seq(1:dim(cp)[1])[cp[, 4] < r])
cp0 <- cp[rmin, 1]
cat("size chosen was", cp[rmin, 2] + 1, "\n")
# we will therefore use a tree of size 11
tree.rose.prune <- prune(tree.rose, cp = 0,014)
rpart.plot(tree.rose.prune)

tree.rose.prune.predict <- predict(tree.rose.prune, newdata = df.te, type = "class")
table(Pred = tree.rose.prune.predict, Obs = df.te$RESPONSE)

# now let's take a glance at the final accuracy mesure

confusionMatrix(data = tree.over.prune.predict, df.te$RESPONSE)
confusionMatrix(data = tree.under.prune.predict, df.te$RESPONSE)
confusionMatrix(data = tree.both.prune.predict, df.te$RESPONSE)
confusionMatrix(data = tree.rose.prune.predict, df.te$RESPONSE)

