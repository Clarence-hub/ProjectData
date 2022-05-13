library(dplyr)
library(summarytools)
library(GGally)
library(DataExplorer)
library(ROSE)
gc <- read.csv("GermanCredit.csv", sep = ";")

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


gc_eda %>%
  explore::describe(INSTALL_RATE)
gc_eda %>%
  explore::describe(NUM_CREDITS)

# we can see that for both INSTALL_RATE & NUM_CREDITS that the number of unique valu is only 4, thereby letting them on an intengers format would not make sense in our point of view
# Therefore, for more clarity, we're goint to switch their format to factor.
gc_eda$INSTALL_RATE <- factor(gc_eda$INSTALL_RATE)
gc_eda$NUM_CREDITS <- factor(gc_eda$NUM_CREDITS)

view(dfSummary(gc_eda, style = "grid",plain.ascii = FALSE, tmp.img.dir = "/tmp"))

# we now want to check if we should scale the numerical value
gc_eda <- tibble(gc_eda)
plot_histogram(gc_eda)


# we can see that none of the numerical value follows a Gaussian distribution
# to overcome this, we're should  use a Min-Max scaling
# However, because we're going to model our data with first a decision tree, the scaling is not our priority


## Now that we have tried to model our data set through a tree,
## we are going to scale our data because we want to split our data into a training and a test set


#Min Max Normalization
gc_eda$DURATION <-(gc_eda$DURATION-min(gc_eda$DURATION)) /
  (max(gc_eda$DURATION)-min(gc_eda$DURATION))
gc_eda$AMOUNT <-(gc_eda$AMOUNT-min(gc_eda$AMOUNT)) /
  (max(gc_eda$AMOUNT)-min(gc_eda$AMOUNT))
gc_eda$AGE <-(gc_eda$AGE-min(gc_eda$AGE)) /
  (max(gc_eda$AGE)-min(gc_eda$AGE))

# the target variable is unbalanced
table(gc_eda$RESPONSE)

#### we are going to try different method to see which one suits us the best
# over-sampling
gc_eda_over <- ovun.sample(RESPONSE~., data = gc_eda, method = "over", N = 1400)$data

table(gc_eda_over$RESPONSE)

# under-sampling
gc_eda_under <- ovun.sample(RESPONSE~., data = gc_eda, method = "under", N = 600, seed = 1)$data
table(gc_eda_under$RESPONSE)

# both
gc_eda_balanced_both <- ovun.sample(RESPONSE~., data = gc_eda, method = "both", p = 0.5, N = 1000, seed = 1)$data
table(gc_eda_balanced_both$RESPONSE)

# ROSE
gc_eda_ROSE <- ROSE(RESPONSE~., data = gc_eda, seed = 1)$data
table(gc_eda_ROSE$RESPONSE)

# for the CART model, we are going to check which one of these sampling works the best.

