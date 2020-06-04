library(tableone)
library(Matching)
library(ipw)
library(survey)
library(MatchIt)

data(lalonde)

xvars<-c("age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75")

#What are the minimum and maximum weights?
psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75, data = lalonde,
               family  = binomial(link ="logit"))
ps <-predict(psmodel, type = "response")
weight<-ifelse(lalonde$treat==1,1/(ps),1/(1-ps))

summary(weight)

#Find the standardized differences for each confounder on the weighted (pseudo) population. What is the standardized difference for nodegree?
weighteddata<-svydesign(ids = ~ 1, data =lalonde, weights = ~ weight)
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treat", 
                                  data = weighteddata, test = FALSE)
print(weightedtable, smd = TRUE)

#Using IPTW, find the estimate and 95% confidence interval for the average causal effect. This can be obtained from svyglm
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~weight,
                                                    data =lalonde)))
coef(msm)
confint(msm)

#Using IPTW with the truncated weights, find the estimate and 95% confidence interval for the average causal effect
# fit propensity score model to get weights, but truncated
weightmodel<-ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~ age + educ + black + hispan + married + nodegree + re74 + re75, 
                      data=lalonde,trunc=.01)

#numeric summary of weights
summary(weightmodel$weights.trun)
#plot of weights
ipwplot(weights = weightmodel$weights.trun, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
lalonde$wt<-weightmodel$weights.trun
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~wt,
                                                data =lalonde)))
coef(msm)
confint(msm)
