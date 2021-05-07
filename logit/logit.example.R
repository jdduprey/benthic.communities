# ==============================================
# Practice Logistic Regression in R
# 05/06/20201
# tutorial from https://stats.idre.ucla.edu/r/dae/logit-regression/
# ==============================================

library('aod')
library('ggplot2')

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)

summary(mydata)
sapply(mydata, sd)

# two-way contingency table of categorical outcome and predictors we want
# make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

# rank should be treated as a categorical variable
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

# the logistic regression coefficients give the change in the log odds of the outcome for a one unit
# increase in the predicted variable
summary(mylogit)

# use confint to obtain confidence intervals - for logistic models, confidence intervals are based on the p
# the profiled log-likelihood function (like population models) 

confint(mylogit)

# ==============================================
# odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# on interpretting odds ratios in logistic regression 
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

# In the above output we see that the predicted probability of being accepted into a 
# graduate program is 0.52 for students from the highest prestige undergraduate institutions (rank=1), 
# and 0.18 for students from the lowest ranked institutions (rank=4), holding gre and gpa at their means. 
# We can do something very similar to create a table of predicted probabilities varying the value of gre 
# and rank. We are going to plot these, so we will create 100 values of gre between 200 and 800, at each 
# value of rank (i.e., 1, 2, 3, and 4).

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
  4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
  ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                                                                                                                      size = 1)

