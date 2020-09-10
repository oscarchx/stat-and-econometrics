data=read.csv(whr)
data2016=filter(data,year==2016)
data2017=filter(data,year==2017)
data1=data2016
data2=data2017

											##########INTRODUCTION###########

#Study of life ladder#
select(data2016[order(data2016$Life.Ladder),],"Life.Ladder","country") #order of countries by Life Ladder score in 2016
slice(select(data2016[order(data2016$Life.Ladder),],"Life.Ladder","country")[2],1:5) #5 countries with worst scores in 2016
slice(select(data2017[order(data2017$Life.Ladder),],"Life.Ladder","country")[2],1:5) #5 countries with worst scores in 2017
tail(select(data2016[order(data2016$Life.Ladder),],"Life.Ladder","country")[2], 5) #5 countries with best scores in 2016
tail(select(data2017[order(data2017$Life.Ladder),],"Life.Ladder","country")[2], 5) #5 best in 2017

#Hypothesis testing
lifeladderorderedbyGDP2017=na.omit(select(data2017[order(data2017$Log.GDP.per.capita),],"Life.Ladder")[1]) # Life Ladder scores of countries ordered by GDP (2017)
lifeladderhighGDP2017=slice(lifeladderorderedbyGDP2017,112:141) #Life Ladder scores of the 30 countries with the highest GDP (2017)
lifeladderotherGDP2017=slice(lifeladderorderedbyGDP2017,1:111) #Life Ladder scores of all the other countries 
vectorlifeladderhighGDP2017=c(lifeladderhighGDP2017$Life.Ladder) # 30 samples from Box A
avg1=mean(vectorlifeladderhighGDP2017) #observed average from Box A
# 6.399141
sd1=sd(vectorlifeladderhighGDP2017)*((30-1)/30) #SD of Box A (bootstrap method)
# 1.247951
se1=sd1/sqrt(31)
# 0.2241386
vectorlifeladderotherGDP2017=c(lifeladderotherGDP2017$Life.Ladder) # 111 samples from Box B
avg2=mean(vectorlifeladderotherGDP2017) #observed avergae from Box B
# 5.228073
sd2=sd(vectorlifeladderotherGDP2017)*((111-1)/111) #SD of Box B (bootstrap method)
# 0.9135201
se2=sd2/sqrt(111) 
# 0.08670749
sediff=sqrt(se2*se2+se1*se1)
# 0.2403254
z=(avg1-avg2)/sediff #test-statistic:z=(observed difference in averages - expected difference in averages)/sqrt(se1*se1+se2*se2)
# 4.872842
1-pnorm(z) #exact p-value
# 5.50021e-07 

										#########EXPLORATRORY STATISTICS##########

####Log GDP per Capita in PPP (Hugo Savoyat)####
summary(data2016$Log.GDP.per.capita)
#mean 9.274, min 6.474, max 11.459
summary(data2017$Log.GDP.per.capita)
#mean 9.341, min 6.625, max 11.465
sd(data2016$Log.GDP.per.capita, na.rm=T)
#1.208074
sd(data2017$Log.GDP.per.capita, na.rm=T)
#1.181755

ggplot(data2016, aes(x=Log.GDP.per.capita, y=..density..))+geom_histogram(bins=10, color="blue")+ggtitle(2016)
ggplot(data2017, aes(Log.GDP.per.capita, y=..density..))+geom_histogram(bins=10, color="blue")+ggtitle(2017)

####Democratic quality and confidence in national government (Thomas Carrie)####
cor(data2016$Confidence.in.national.government, data2016$Democratic.Quality, use = "pair")
# -0.159
cor(data2016$Confidence.in.national.government, data2016$Life.Ladder, use = "pair")
# -0.194
cor(data2016$Life.Ladder, data2016$Democratic.Quality, use = "pair")
# 0.626
ggplot(data2016, aes(x=data2016$Life.Ladder, y=data2016$Democratic.Quality)) + geom_point() + geom_smooth(method = lm) + ggtitle(" Democratic Quality & Life Ladder's Regression") + xlab("Life Ladder") + ylab("Democratic Quality")

####Gini world bank (Alba Nouet)####
cor(data2016$Life.Ladder,data2016$GINI.index..World.Bank.estimate...average.2000.15,use="pair") #correlation betweeen GINI WB and Life.Ladder in 2016 
#-0.2805007
lifeladder2016=na.omit(filter(data,year=="2016"))
ggplot(data2016,aes(x=data2016$GINI.index..World.Bank.estimate...average.2000.15,y=lifeladder2016))+geom_point(alpha=0.3)+geom_smooth(method=lm,color="blue",se=F)+labs(title="Life Ladder score in terms of the World Bank Estimate of the GINI index (2016)")+xlab("World Bank Estimate of the GINI Index")+ylab("Life Ladder Score") #Life Ladder score in terms of the World Bank Estimate of the GINI index (2016)
cor(data2017$Life.Ladder,data2017$GINI.index..World.Bank.estimate...average.2000.15,use="pair") #correlation betweeen GINI WB and Life.Ladder in 2017
#-0.2662538
lifeladder2017=na.omit(filter(data,year=="2017"))
ggplot(data2017,aes(x=data2017$GINI.index..World.Bank.estimate...average.2000.15,y=lifeladder2017))+geom_point(alpha=0.3)+geom_smooth(method=lm,color="red",se=F)+labs(title="Life Ladder score in terms of the World Bank Estimate of the GINI index (2017)")+xlab("World Bank Estimate of the GINI Index")+ylab("Life Ladder Score") #Life Ladder score in terms of the World Bank Estimate of the GINI index (2017)
data2016lowGINI=filter(data2016,data2016$GINI.index..World.Bank.estimate...average.2000.15<=mean(data2016$GINI.index..World.Bank.estimate...average.2000.15,na.rm=TRUE)) #new data frame for countries in which's World Bank GINI estimate was smaller than average in 2016
ggplot(data2016lowGINI,aes(x=data2016lowGINI$GINI.index..World.Bank.estimate...average.2000.15,y=data2016lowGINI$Life.Ladder))+geom_point(alpha=0.3)+geom_smooth(method=lm,color="blue",se=F) #Plot of LiFe Ladder Scores in terms of the GINI estimate for countries in which GINI is smaller than average (2016)
cor(data2016lowGINI$Life.Ladder,data2016lowGINI$GINI.index..World.Bank.estimate...average.2000.15,use="pair") #correlation between Life Ladder Scores and GINI index in countries in which the GINI index is smaller than average (2016)
# -0.3946803

####Gini household (Martin Lepercq)####
# Correlation GINI and Life Ladder

x= c(data2016$gini.of.household.income.reported.in.Gallup..by.wp5.year)
y= c(data2016$Life.Ladder)
cor(x, y, use= "pair")
# Correlation = -0.4102329

x= c(data2017$gini.of.household.income.reported.in.Gallup..by.wp5.year)
y= c(data2017$Life.Ladder)
cor(x, y, use= "pair")
# Correlation = -0.4266287

# mean and SD  of GINI index for households
mysd=function(x){sd(x)*sqrt((length(x)-1)/length(x))}

GiniH2016 = data2016$gini.of.household.income.reported.in.Gallup..by.wp5.year
GiniH2017 = data2017$gini.of.household.income.reported.in.Gallup..by.wp5.year

summarize(data2016, mean(Life.Ladder))
summarize(data2017, mean(Life.Ladder))
# mean 2016 = 5.399414     # mean 2017 = 5.485542

summarize(data2016, mean(GiniH2016))
summarize(data2017, mean(GiniH2017))
# mean 2016 = 0.4503161      # mean 2017 = 0.4584814

summarize(data2016, mysd(GiniH2016))
summarize(data2017, mysd(GiniH2017))
# sd  2016 =  0.1019892       # sd 2017 = 0.1093008

# Regression impact of household GINI on Life Ladder: plot and formula 
ggplot (data2017, aes(x=GiniH2017, y= Life.Ladder)) + geom_point(alpha=1, color="red") + geom_smooth(method="lm", color="blue",se=F) + geom_abline(slope=0, intercept = mean(data2017$Life.Ladder)) + labs(title="Impact of Household Income Gini Index on Happiness")

lm(Life.Ladder ~ GiniH2017, data2017)
#Intercept = 7.484     #slope = -4.358

####Healthy life expectancy at birth (Oscar Chaix)####

summary(data2016$Healthy.life.expectancy.at.birth)
#mean healthy life expectancy at birth 63.15 (median 64.96)

summary(data2017$Healthy.life.expectancy.at.birth)
#mean healthy life expectancy at birth 63.40 (median 65.13)

sd(data2016$Healthy.life.expectancy.at.birth,na.rm=T) 
#7.84
sd(data2017$Healthy.life.expectancy.at.birth,na.rm=T)
#7.58

ggplot(data2016,aes(Healthy.life.expectancy.at.birth,y=..density..)) + geom_histogram(fill="yellow",col = "blue", breaks =c(40,45,50,55,60,65,70,75,80))+labs(title="Distribution 2016",x="Healthy life expectancy at birth",y="Density") 
ggplot(data2017,aes(Healthy.life.expectancy.at.birth,y=..density..)) + geom_histogram(fill="yellow",col = "blue", breaks =c(40,45,50,55,60,65,70,75,80))+labs(title="Distribution 2017",x="Healthy life expectancy at birth",y="Density") 

									##########VARIABLES THAT INFLUENCE LIFE LADDER########

###Healthy life expectancy at birth####

cor(data2016$Healthy.life.expectancy.at.birth,data2016$Life.Ladder,use="pair")
#0.7797613
cor(data2017$Healthy.life.expectancy.at.birth,data2017$Life.Ladder,use="pair")
#0.7368059

ggplot(data2017,aes(x=Healthy.life.expectancy.at.birth,y=Life.Ladder)) +
  geom_point(aes(x=Healthy.life.expectancy.at.birth,y=Life.Ladder),alpha=1,color="blue",size=0.5) + 
  geom_smooth(method=lm,color="red",se=F) +
  geom_smooth(aes(y=mean(data2016$Life.Ladder)),color="orange") +
  labs(title="Hapiness & Health Regression 2017",x="Healthy life expectancy at birth",y="Happiness (life ladder)")

lm.17=lm(Life.Ladder ~ Healthy.life.expectancy.at.birth, data2017)
lm.17$coefficients
#intercept -1.42, slope 0.109

select(filter(data2017, Healthy.life.expectancy.at.birth>=70), Life.Ladder)
#the countries above the 70 years of healthy life expectancy treshold seem to be the "happiest"

group = group_by(data2017, Healthy.life.expectancy.at.birth>=70)
summarise(group, avg_happyness = mean(Life.Ladder)) 
#in fact, for countries above this treshold, mean life ladder = 6.75, 
#against a mean of 5.14 for countries below the 70 years of healthy life expectancy

#top 6 2017#
dataclasshappiness2017=select(arrange(data2017, Healthy.life.expectancy.at.birth),Life.Ladder) 
dataclasshappiness2017
top6.1=slice(dataclasshappiness2017,136:141)
top6.1
top6hapinness=top6.1
dataclasshealth2017=select(arrange(data2017, Healthy.life.expectancy.at.birth),Healthy.life.expectancy.at.birth) 
dataclasshealth2017
top6.2=slice(dataclasshealth2017,136:141)
top6.2
top6health=top6.2
cor(top6hapinness,top6health)
#0.473

#bottom 10 2017#
dataclasshappiness2017=select(arrange(data2017, Healthy.life.expectancy.at.birth),Life.Ladder) 
dataclasshappiness2017
bottom10.1=slice(dataclasshappiness2017,1:10)
bottom10.1
bottom10hapinness=bottom10.1
dataclasshealth2017=select(arrange(data2017, Healthy.life.expectancy.at.birth),Healthy.life.expectancy.at.birth) 
dataclasshealth2017
bottom10.2=slice(dataclasshealth2017,1:10)
bottom10.2
bottom10health=bottom10.2
cor(bottom10hapinness,bottom10health)
#-0.035

#correlation is strong, but health expectancy itself strongly correlated to GDP!
cor(data2016$Healthy.life.expectancy.at.birth,data2016$Log.GDP.per.capita,use="pair")
#0.8666038
cor(data2017$Healthy.life.expectancy.at.birth,data2017$Log.GDP.per.capita,use="pair")
#0.8566417

####Democratic quality#####

Test1 = arrange(data2016, desc(data2016$Democratic.Quality))
Test1_top = Test1[c(1:20), c(2,4,14)]
Test1.1 = arrange(data2016, data2016$Democratic.Quality)
Test1_bottom = Test1.1[c(1:20), c(2,4,14)]
Test1Final = rbind(Test1_top, Test1_bottom)
Test1Final
cor(Test1Final$Life.Ladder, Test1Final$Democratic.Quality)
# 0.8415406

Test2 = arrange(data2016, desc(data2016$Democratic.Quality))
Test2_top = Test2[c(21:40), c(2,4,14)]
Test2.1 = arrange(data2016, data2016$Democratic.Quality)
Test2_bottom = Test2.1[c(21:40), c(2,4,14)]
Test2Final = rbind(Test2_top, Test2_bottom)
cor(Test2Final$Life.Ladder, Test2Final$Democratic.Quality)
# 0.636916

Test3 = arrange(data2016, desc(data2016$Democratic.Quality))
Test3_top = Test3[c(41:60), c(2,4,14)]
Test3.1 = arrange(data2016, data2016$Democratic.Quality)
Test3_bottom = Test3.1[c(41:60), c(2,4,14)]
Test3Final = rbind(Test3_top, Test3_bottom)
cor(Test3Final$Life.Ladder, Test3Final$Democratic.Quality)
# 0.2872139

cor(x = data2016$Log.GDP.per.capita, data2016$Democratic.Quality, use = "pair")
# 0.6162388
Test1_top["Ranking"]=c("Top")
Test1_bottom["Ranking"]=c("Bottom")
TestFinal = rbind(Test1_top, Test1_bottom)
ggplot(TestFinal, aes(x = TestFinal$Life.Ladder, y = TestFinal$Democratic.Quality, color = Ranking)) + geom_point() + geom_smooth(method = lm, color = "black") + ggtitle("Life Ladder and 20 highest & lowest Democratic indexes") + xlab("Life Ladder") + ylab("Democratic Quality")


####GDP per Capita####

#correlations between Log GDP per capita and Life Ladder (2016 and 2017)
x=c(data2016$Log.GDP.per.capita)
y=c(data2016$Life.Ladder)
cor(x,y, use="pair") 
#use "pair" to avoid NAs

x=c(data2017$Log.GDP.per.capita)
y=c(data2017$Life.Ladder)
cor(x,y, use="pair")

#scatter plot and draw the regression line
#add a line to show Life Ladder World average
ggplot(data2016, aes(x=data2016$Log.GDP.per.capita, y=data2016$Life.Ladder))+geom_point(alpha=0.3)+geom_smooth(method=lm, color="red", SE=F)+ggtitle("Regression 2016")+xlab("Log GDP per capita in PPP")+ylab("Hapiness")
ggplot(data2017, aes(x=data2017$Log.GDP.per.capita, y=data2017$Life.Ladder))+geom_point(alpha=0.3)+geom_smooth(method=lm, color="red", SE=F)+ggtitle("Regression 2017")+xlab("Log GDP per capita in PPP")+ylab("Hapiness")

#Top10 2016
dataclass2016=select(arrange(data2016, Log.GDP.per.capita),Log.GDP.per.capita, country, Life.Ladder) 
dataclass2016 
#countries are ranked from the lowest GDP per capita to the highest
#only "countries", "Life Ladder" and "Log GDP per capita" appear

#select the 10 countries with the highest "Log GDP per capita"
top10=slice(dataclass2016, 130:139)
top10

#correlation between "Log GDP per capita" and "Life Ladder" for the 10 countries with the highest "Log GDP per capita" in 2016
top10GDP=c(top10$Log.GDP.per.capita)
top10GDP
top10hapiness=c(top10$Life.Ladder)
top10hapiness
cor(top10GDP, top10hapiness)
ggplot(top10, aes(x=top10$Log.GDP.per.capita, y=top10$Life.Ladder))+geom_point(alpha=0.3)+geom_smooth(method=lm, color="red", SE=F)+ geom_abline(slope=0, intercept=5.399, color="green")+ggtitle("Regression 2016 Top 10")+xlab("Log GDP per capita in PPP")+ylab("Hapiness")

#Top10 2017
dataclass2017=select(arrange(data2017, Log.GDP.per.capita),Log.GDP.per.capita, country, Life.Ladder) 
dataclass2017 
#countries are ranked from the lowest GDP per capita to the highest
#only "countries", "Life Ladder" and "Log GDP per capita" appear

#select the 10 countries with the highest "Log GDP per capita"
top102017=slice(dataclass2017, 125:134)
top102017

#correlation between "Log GDP per capita" and "Life Ladder" for the 10 countries with the highest "Log GDP per capita" in 2017
top10GDP2017=c(top102017$Log.GDP.per.capita)
top10GDP2017
top10hapiness2017=c(top102017$Life.Ladder)
top10hapiness2017
cor(top10GDP2017, top10hapiness2017)
ggplot(top102017, aes(x=top102017$Log.GDP.per.capita, y=top102017$Life.Ladder))+geom_point(alpha=0.3)+geom_smooth(method=lm, color="red", SE=F)+geom_abline(slope=0, intercept=5.486, color="green")+ggtitle("Regression 2017 Top 10")+xlab("Log GDP per capita in PPP")+ylab("Hapiness")

#Bottom10 2016
dataclass2016=select(arrange(data2016, Log.GDP.per.capita),Log.GDP.per.capita, country, Life.Ladder) 
dataclass2016 
#countries are ranked from the lowest GDP per capita to the highest
#only "countries", "Life Ladder" and "Log GDP per capita" appear

#select the 10 countries with the lowest "Log GDP per capita"
bottom10=slice(dataclass2016, 1:10)
bottom10

#correlation between "Log GDP per capita" and "Life Ladder" for the 10 countries with the lowest "Log GDP per capita" in 2016
bottom10GDP=c(bottom10$Log.GDP.per.capita)
bottom10GDP
bottom10hapiness=c(bottom10$Life.Ladder)
bottom10hapiness
cor(bottom10GDP, bottom10hapiness)
ggplot(bottom10, aes(x=bottom10$Log.GDP.per.capita, y=bottom10$Life.Ladder))+geom_point(alpha=0.3)+geom_smooth(method=lm, color="red", SE=F)+geom_abline(slope=0, intercept=5.399, color="green")+ggtitle("Regression 2016 Bottom10")+xlab("Log GDP per capita in PPP")+ylab("Hapiness")


#Bottom10 2017
dataclass2017=select(arrange(data2016, Log.GDP.per.capita),Log.GDP.per.capita, country, Life.Ladder) 
dataclass2017 
#countries are ranked from the lowest GDP per capita to the highest
#only "countries", "Life Ladder" and "Log GDP per capita" appear

#select the 10 countries with the lowest "Log GDP per capita"
bottom102017=slice(dataclass2017, 1:10)
bottom102017

#correlation between "Log GDP per capita" and "Life Ladder" for the 10 countries with the lowest "Log GDP per capita" in 2017
bottom10GDP2017=c(bottom102017$Log.GDP.per.capita)
bottom10GDP2017
bottom10hapiness2017=c(bottom102017$Life.Ladder)
bottom10hapiness2017
cor(bottom10GDP2017,bottom10hapiness2017)
ggplot(bottom102017, aes(x=bottom102017$Log.GDP.per.capita, y=bottom102017$Life.Ladder))+geom_point(alpha=0.3)+geom_smooth(method=lm, color="red", SE=F)+geom_abline(slope=0, intercept=5.486, color="green")+ggtitle("Regression 2017 Bottom10")+xlab("Log GDP per capita in PPP")+ylab("Hapiness")


							########COUNTRIES THAT HAVE AN EVOLUTION IN THEIR LIFE LADDER#########
							
#### Comparison of evolution in Life Ladder between countries #######

data_bis = data[-c(43,124,139,144,177,180,193,202,215,230,239,254,275),]
data2016_bis = filter(data_bis, year==2016)
data2017_bis = filter(data_bis, year==2017)

## Largest decrease in Life Ladder score from 2016 to 2017

x = select(data2016_bis, Life.Ladder)
y = select(data2017_bis, Life.Ladder)
Happy_evolution=x-y
order(x-y)
dataAfghanistan = filter(data, country=="Afghanistan")

# Determinant Happiness Afghanistan 
x = filter(dataAfghanistan, year==2016)
y = filter(dataAfghanistan, year==2017)
SadAfgha = x-y

# Regression impact of Generosity on Life Ladder

lm(Life.Ladder ~ Generosity , data2016)
#Intercept = 5.3977     #slope = 0.8995
lm(Life.Ladder ~ Generosity , data2017)
#Intercept =  5.546     #slope = 1.114

#Correlation btw Generosity and Life Ladder 
x= c(data2016$Generosity)
y= c(data2016$Life.Ladder)
cor(x, y, use= "pair")
# correlation = 0.1208286

x= c(data2017$Generosity)
y= c(data2017$Life.Ladder)
cor(x, y, use= "pair")
# correlation = 0.1639421

#Regression plot impact of Generosity on Life Ladder
ggplot(data2016, aes(x= Generosity, y=Life.Ladder)) +  geom_point(alpha=1, color="pink") + geom_smooth(method="lm", color="purple",se=T) + labs(title = "impact Generosity on ???Life Ladder??? World 2016" )
ggplot(data2017, aes(x= Generosity, y=Life.Ladder)) +  geom_point(alpha=1, color="pink") + geom_smooth(method="lm", color="purple",se=T) + labs(title = "impact Generosity on ???Life Ladder??? World 2017" )

## Largest increase in Life Ladder score from 2016 to 2017
data_bis = whr[-c(43,124,139,144,177,180,193,202,215,230,239,254,275),]
data2016_bis = filter(data_bis, year==2016)
data2017_bis = filter(data_bis, year==2017)
x = select(data2016_bis, Life.Ladder)
y = select(data2017_bis, Life.Ladder)
Happy_evolution=x-y
order(x-y)

Guinea=filter(data,country=="Guinea")
Guinea2016=filter(Guinea,year=="2016") #data for 2016
Guinea2017=filter(Guinea,year=="2017") #data for 2017
Guinea2016=c(Guinea2016) #vectorized data for Guinea (2016)
Guinea2017=c(Guinea2017) #vectorized data for Guinea (2017)

