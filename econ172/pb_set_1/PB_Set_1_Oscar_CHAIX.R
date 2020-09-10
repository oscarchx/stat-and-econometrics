#PB Set 1

data=read.csv("~/Desktop/Econ172_S19_ProblemSet1_data.csv")
install.packages("stargazer")
library(stargazer)
library(dplyr)

######b######

###Average differences between treatment and control households### 

##Gender differences 

reg1= lm(female ~ cash, data)
reg1
summary(reg1) #coeff = 0.022, SE = 0.029, t-value = 0.75

##Age differences 

reg2= lm(age25 ~ cash, data)
reg2
summary(reg2) #coeff = 0.002, SE = 0.025, t-value = 0.081

##Education differences 

reg3= lm(schooling ~ cash, data)
reg3
summary(reg3) #coeff = -0.012, SE = 0.031, t-value = -0.392

###Differences in gender and age, with schooling restricted at 1### 

schooled = filter(data,schooling==1)

reg4 = lm(female ~ cash, schooled)
reg4
summary(reg4) #coeff = 0.073, SE = 0.051, t-value = 1.417

reg5
summary(reg5) #coeff = -0.029, SE = 0.045, t-value = -0.659

###Regression outputs###

#reg1

stargazer(reg1,out="Table 1.html",type="html",
          title="Gender Differences")

#reg2

stargazer(reg2,out="Table 2.html",type="html",
          title="Age Differences")

#reg3 

stargazer(reg3,out="Table 3.html",type="html",
          title="Education Differences")

#reg4

stargazer(reg4,out="Table 4.html",type="html",
          title="Gender differences when schooled")

#reg 5

stargazer(reg5,out="Table 5.html",type="html",
          title="Age differences when shcooled")


######c######

##Health on cash

reg6= lm(health ~ cash, data)
reg6
summary(reg6) #coeff = 0.024, SE = 0.06, t-value = 0.377

##Health on cash (education restricted at 1)

schooled = filter(data,schooling==1)

reg7=lm(health ~ cash, schooled)
reg7
summary(reg7) #coeff = 0.049, SE = 0.10, t-value = 0.490

###Regression outputs###

#reg6

stargazer(reg6,out="Table 6.html",type="html",
          title="Health on Cash Transfer")

#reg7

stargazer(reg7,out="Table 7.html",type="html",
          title="Health on Cash Transfer (Education Restricted)")


######d######

##Health on cash, gender and age

reg8 = lm(health ~ cash + female + age25, data)
reg8 
summary(reg8) 

stargazer(reg8,out="Table 8.html",type="html",
          title="Health on Cash Transfer, Gender and Age")

###Health on cash, gender and age, with schooling restricted to 1### 

schooled = filter(data,schooling==1)

reg9 = lm(health ~ cash + female + age25, schooled)
reg9
summary(reg9)

stargazer(reg9,out="Table 9.html",type="html",
          title="Health on Cash Transfer, Gender and Age (Education Restricted)")

timestamp(stamp=date())
savehistory(file="PB_Set_1_Oscar_CHAIX.Rhistory")






