library(tableone)
library(Matching)
library(MatchIt)
library(readr)

data(lalonde)

xvars <- c("age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75")

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treat", data=lalonde, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)

#outcome analysis
income_trt<-lalonde$re78[lalonde$treat==1]
income_con<-lalonde$re78[lalonde$treat==0]

mean(income_trt) - mean(income_con)

psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
             family=binomial(),data=lalonde)
summary(psmodel)
pscore<-psmodel$fitted.values

summary(pscore)

set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE)
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]
xvars <- c("age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75")

matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE, caliper=.1)
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]
xvars <- c("age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75")

matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

income_trt<-matched$re78[matched$treat==1]
income_con<-matched$re78[matched$treat==0]

#pairwise difference
diffy<-income_trt-income_con
mean(diffy)

#paired t-test
t.test(diffy)
