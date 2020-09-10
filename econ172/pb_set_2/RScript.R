#PB Set 2

data=read.csv("~/Desktop/Econ172_S19_ProblemSet2_data.csv")
library(stargazer)
library(dplyr)

######b######
summary(data$eligible)
baseline_data = filter(data,time==0)
summary_stats = summary(baseline_data)
summary_stats

###Average differences between households eligible and ineligible for treatment### 

##Gender differences 

reg1= lm(female ~ eligible, baseline_data)
reg1
summary(reg1) #coeff = -0.048 , SE = 0.028, t-value = -1.735 

##Age differences 

reg2= lm(age25 ~ eligible, baseline_data)
reg2
summary(reg2) #coeff = -0.126, SE = 0.0190 , t-value = -6.634 (***)

##Education differences 

reg3= lm(schooling ~ eligible, baseline_data)
reg3
summary(reg3) #coeff = 0.062 , SE = 0.0304, t-value = 2.041 (*)

##Result Outputs

stargazer(baseline_data,
          out="Table 1.html",type="html",header=FALSE,
          titles="Baseline Summary Statistics",align=TRUE,no.space=TRUE,stats = c("mean", "sd", "min", "med", "max", "n.valid"))

stargazer(reg1,reg2,reg3,
          out="Table 2.html", type="html",header=FALSE,
          title="Baseline Covariate Results",align=TRUE, omit.stat=c("LL","ser","f","rsq","adj.rsq"),no.space=TRUE)

######c######

##Regression of health on eligbility, female, age25 and shcooling for the baseline data

reg4 = lm(health ~ eligible + female + age25 + schooling, baseline_data)
reg4
summary(reg4)

##Regression of health on eligbility, female, age25 and shcooling for the endline data

endline_data = filter(data,time==1)
reg5 = lm(health ~ eligible + female + age25 + schooling, endline_data)
reg5
summary(reg5)

##Result Outputs 

stargazer(reg4,reg5,
          out="Table 3.html", type="html",header=FALSE,
          title="Baseline and Endline Regressions of Health Outcomes",align=TRUE, omit.stat=c("LL","ser","f","rsq","adj.rsq"),no.space=TRUE)

######d######

#Creation of the interaction variable "treat" and inclusion in data

data = mutate(data, treat = data$eligible*data$time)

##DD regression of health on eligbility, time, the intereaction term, and controls

reg6 = lm(health ~ eligible + time + treat + female + age25 + schooling, data)
reg6
summary(reg6)

##Result Outputs 

stargazer(reg6,
          out="Table 4.html", type="html",header=FALSE,
          title="DD Regression of Health on Eligibility, Time, the Intersection Term and Controls",align=TRUE, omit.stat=c("LL","ser","f","rsq","adj.rsq"),no.space=TRUE)

timestamp(stamp=date())
savehistory(file="PB_Set_2_Oscar_CHAIX.Rhistory")


