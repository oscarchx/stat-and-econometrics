#PB Set 3

data=read.csv("~/Desktop/Econ172_S19_ProblemSet3_data.csv")
install.packages("stargazer")
install.packages("ivpack")
library(stargazer)
library(dplyr)
library(ivpack)

summary(data)

#c

reg1 = lm(armed_conflict ~ gdp_growth + year, data)
reg1
summary(reg1)

stargazer(reg1,out="Table 1.html",type="html",
          title="Second Stage Regression")

#d


reg2 = lm(gdp_growth ~ green_index_growth + year, data)
summary(reg2)

stargazer(reg2,out="Table 2.html",type="html",
          title="First Stage Regression")

#e

reg3 = lm(armed_conflict ~ green_index_growth + year, data)
reg3
summary(reg3)

stargazer(reg3,out="Table 3.html",type="html",
          title="Reduced Form Regression")

#f

reg4 = ivreg(armed_conflict ~ gdp_growth + year | green_index_growth + year, x=TRUE, data=data)
reg4
summary(reg4)

stargazer(reg4,out="Table 4.html",type="html",
          title="IV Regression")

timestamp(stamp=date())
savehistory(file="PB_Set_3_Oscar_CHAIX.Rhistory")


