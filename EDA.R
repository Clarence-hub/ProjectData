library(dplyr)
library(summarytools)
library(GGally)
library(DataExplorer)
library(inspectdf)
gc <- read.csv("GermanCredit.csv", sep = ";")


############ EDA
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

str(gc_eda)

view(dfSummary(gc_eda, style="grid",plain.ascii = FALSE, tmp.img.dir = "/tmp"))


# looking at boxplot for int value
gc_eda %>%
  select(1:6) %>%
  head()

lblue <- "#6699CC"
par(mfrow = c(2, 3))
boxplot(DURATION ~ RESPONSE, data = gc_eda, xlab = "Duration", notch = T,
        varwidth = T, col = lblue)
boxplot(AMOUNT ~ RESPONSE, data = gc_eda, xlab = "Amount", varwidth = T, col = lblue)
boxplot(INSTALL_RATE ~ RESPONSE, data = gc_eda, xlab = "Install_rate", varwidth = T, col = lblue)
boxplot(AGE ~ RESPONSE, data = gc_eda, xlab = "Age", varwidth = T, col = lblue)
boxplot(NUM_CREDITS ~ RESPONSE, data = gc_eda, xlab = "Num_credit", varwidth = T, col = lblue)
boxplot(NUM_DEPENDENTS ~ RESPONSE, data = gc_eda, xlab = "Num_dependents", varwidth = T, col = lblue)
# we can notice that the boxplot made for num_dependents is not useful as of now, therefore we're goint to scale it with a log transformation
# however after doing so nothing change.
# After looking at the distribution of num_dependents, we can come to the conclusion that only few possibilities exist. 
# Thereby we're goint to put it into factor
gc_eda$NUM_DEPENDENTS <- factor(gc_eda$NUM_DEPENDENTS)



### co-matrix

## categorical
# marital status
gc_eda %>% select(MALE_DIV, MALE_SINGLE, MALE_MAR_or_WID, RESPONSE) %>%
  ggpairs()
# we can see tht for the marital status
# if a man if he's not divorced or not, there's still a higher chance for him to have a bad credit score
# if a man is not single, the probability that he'll have a bad /good credit score is pretty close
## however, if he's single, then there is a higher chance that he'll have a good credit score
# if a man is neither married nor widower, the resulst would be close to those with a non-divorced man
## however, shall he be married or widower, then there would be small but still higher chance for him to have a good credit score

#purpose of application
gc_eda %>% select(NEW_CAR, USED_CAR, FURNITURE, RADIO.TV, EDUCATION, RETRAINING, RESPONSE) %>%
  ggpairs()
# it is interesting to see that no mather the purpose of a credit taken
# if the binary response is "yes" the chance for a good credit will be higher
# however, it is also interesting to see that the chance of having a applicant with a bad credit score is still higher


# past history 
gc_eda %>% select(CHK_ACCT, NUM_CREDITS, HISTORY, INSTALL_RATE, OTHER_INSTALL, RESPONSE) %>%
  ggpairs()
# relationship with RESPONSE
# 

# guarantee
gc_eda %>% select(CO.APPLICANT, GUARANTOR, PRESENT_RESIDENT,SAV_ACCT, RESPONSE) %>%
  ggpairs()
#real estate
gc_eda %>% select(REAL_ESTATE, OWN_RES, PROP_UNKN_NONE, RENT, RESPONSE) %>%
  ggpairs()
# employment & dependency
gc_eda %>% select(EMPLOYMENT, JOB, NUM_DEPENDENTS, RESPONSE) %>%
  ggpairs()
# it is interesting to see that there seems to be  a higher chance for a good credit score if the applicant has been employed between 4-7 years
# the same goes for applicant who's category under job is "un-skilled - resident" than for the other
# for the dependent category, there's a higher chance for an applicant to have a good credit score if the number of liable individual is close to 1


# foreigners 
gc_eda %>% select(FOREIGN, RESPONSE) %>%
  ggpairs()
# for this categroy we can see that proportion of bad credit score is way more significative if the applicant is foreigner
# however, the probability that he'll have a good credit score is lower if he's not foreigner which is pretty interesting


# Phone 
gc_eda %>% select(TELEPHONE,RESPONSE) %>%
  ggpairs()
# we can see that having a phone would drasticly increase the probability that the applicant would have a good credit score

## numerical
gc_eda %>% select(DURATION, AMOUNT, INSTALL_RATE, AGE, NUM_CREDITS, RESPONSE) %>% 
  ggpairs()
# regarding correaltion
# there is a significant positive correlation between the amount taken and the duration of the credit
# a negative correaltion between the amount taken and the installment rate of the applicant
# and finaly a positive correaltion between the age and the number of credit already taken by the applicant

boxplot(NUM_DEPENDENTS~RESPONSE, data = gc_eda)
# looking through a simple report
create_report(gc_eda, y = "RESPONSE")

gc_eda %>%
  explore::describe(NUM_DEPENDENTS)


x<-inspect_cor(gc_eda[,-31])
show_plot(x, col_palette=4)
