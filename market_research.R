setwd("c:/MyProject")
library(agricolae)
library(ca)
library(car)
library(GPArotation)
library(QuantPsyc)
survey=read.csv("Survey_Result.csv")
satis_result=read.csv("satis_result.csv")
attach(survey)

#Two-sample T-test
#성별에 따른 전체만족도 차이
var.test(all_satis ~ gender)
t.test(all_satis ~ gender)
#성별에 따른 유료화서비스 만족도 차이
var.test(pay_service ~ gender)
t.test(pay_service ~ gender, var.equal=TRUE)
#나이 그룹에 따른 전체 만족도 차이
var.test(all_satis ~ age)
t.test(all_satis ~ age, var.equal=TRUE)
#나이 그룹에 따른 지불금액 차이 
var.test(usageprice ~ age)
t.test(usageprice ~ age)

#One-way ANOVA
#주로 이용하는 플랫폼에 따른 전체 만족도 차이
plat_satis=aov(all_satis~platform)
summary(plat_satis)
scheffe.test(plat_satis, "platform", group = FALSE, console = TRUE)
#직업에 따른 가격 만족도 차이
job_psatis=aov(satis_price~job)
summary(job_psatis)
scheffe.test(job_psatis, "job", group = FALSE, console = TRUE)
#소득에 따른 이벤트 및 혜택 중요도 차이
income_esatis=aov(im_event~userincome)
summary(income_esatis)
scheffe.test(income_esatis,"userincome",group = FALSE, console = TRUE)

#Multiple linear regression
#속성만족도-전체만족도
linear_satis=lm(all_satis ~ ., data=satis_result)
summary(linear_satis)
#다중공선성, 서비스 제공사 호감도 제외
vif(linear_satis)

#linear regression
#가격만족도-전체만족도
sprice_out=lm(all_satis~price, data=satis_result)
summary(sprice_out)
#장르만족도-전체만족도
sgenre_out=lm(all_satis~genre, data=satis_result)
summary(sgenre_out)
#UI디자인만족도-전체만족도
sdesign_out=lm(all_satis~design, data=satis_result)
summary(sdesign_out)
#이외서비스만족도-전체만족도
sservice_out=lm(all_satis~service, data=satis_result)
summary(sservice_out)

#Chi-squared test