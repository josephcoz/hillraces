#read in data
hills <- read.table(text = 'Race	Distance	Climb	Time
Greenmantle	2.5	650	16.083
Carnethy	6	2500	48.35
CraigDunain	6	900	33.65
BenRha	7.5	800	45.6
BenLomond	8	3070	62.267
Goatfell	8	2866	73.217
BensofJura	16	7500	204.617
Cairnpapple	6	800	36.367
Scolty	5	800	29.75
Traprain	6	650	39.75
LairigGhru	28	2100	192.667
Dollar	5	2000	43.05
Lomonds	9.5	2200	65
CairnTable	6	500	44.133
EildonTwo	4.5	1500	26.933
Cairngorm	10	3000	72.25
SevenHills	14	2200	98.417
KnockHill	3	350	78.65
BlackHill	4.5	1000	17.417
CreagBeag	5.5	600	32.567
KildconHill	3	300	15.95
MeallAnt-Suidhe	3.5	1500	27.9
HalfBenNevis	6	2200	47.633
CowHill	2	900	17.933
NBerwickLaw	3	600	18.683
CreagDubh	4	2000	26.217
Burnswark	6	800	34.433
LargoLaw	5	950	28.567
Criffel	6.5	1750	50.5
Acmony	5	500	20.95
BenNevis	10	4400	85.583
Knockfarrel	6	600	32.383
TwoBreweries	18	5200	170.25
Cockleroi	4.5	850	28.1
MoffatChase	20	5000	159.833', 
header = TRUE, stringsAsFactors = FALSE)

#check data
str(hills)

plot(hills$Climb, hills$Time)
identify(hills$Climb, hills$Time, hills$Race)
hills[c(7, 11, 33, 35),]
#row numbers for outilers were 7, 11, 33, 35
# Races: BensofJura, LairigGhru, TwoBreweries, MoffatChase
#Dr. Grimshaw also named KnockHill as an outlier for this plot - 18
#He also said BensofJura isn't necessarily an outlier because 
# it seems to follow the trend - called an influential obs

plot(hills$Distance, hills$Time)
identify(hills$Distance, hills$Time, hills$Race)
#row numbers for outliers were 7, 11, 33, 35
#same as previous outliers
# Races: BensofJura, LairigGhru, TwoBreweries, MoffatChase
#Dr. Grimshaw also named KnockHill as an outlier for this plot - 18
#He also said BensofJura isn't necessarily an outlier because 
# it seems to follow the trend - called an influential obs

#WE LATER FIND OUT MORE ABOUT BENSOFJURA, HOLD THE PHONE

#more EDA on my own
subset(hills, Distance > 15)
subset(hills, (Climb > 2000) & (Distance > 10))

#Analysis

#model: Time = betao + beta1Distance + beta2Climb + epsilon, epsilon ~ N(0, sigma^2)

hills_out <- lm(Time ~ Distance + Climb, data = hills)

#don't look at any inference in model UNTIL
# diagnostics are FINISHED
#(seeing if data is 'healthy')

#NORMALITY assumption first
# look for shape of distribution, and for any outliers
# shape will help us know which distribution to use,
# outliers will help us know what to take out

hills_Rstud <- rstudent(hills_out)

hist(hills_Rstud)
#for more informative plot, includes density
hist(hills_Rstud, freq = FALSE)
my_z <- seq(-3, 3, length = 50)
lines(my_z, dnorm(my_z, 0, 1), col = 'purple', lty = 2)

#plot of data density instead of histogram
plot(density(hills_Rstud))
my_z <- seq(-3, 3, length = 50)
lines(my_z, dnorm(my_z, 0, 1), col = 'purple', lty = 2)

#there appear to be a few outlers..what are they?

#is it CairnTable?
#subset(hills_Rstud, hills$Race == 'CairnTable')

#we took a few other guesses in class, but
# this is better than guessing 
subset(hills, hills_Rstud > 2)

#Shapiro-Wilk test of normality
shapiro.test(hills_Rstud)
#p-value < 0.0001, therefore not normal

#trust your eyeball analysis of graph
# to investigate further if p-value says it's normal,
# but graph appears non-normal

#The actual time for Kildcon Hill is 15.95,
# but our model predicts 12.97. 
#Is this an outlier?

#Ho: Kildcon is not an outlier
#Ha: Kildcon is an outlier

subset(hills_Rstud, hills$Race == 'KildconHill')
#we already have a pretty good idea that this is not
# an outlier, because the RstudResid is only .205
#Outliers are usually 2 or 3 away

#2 * is because we want both tails
#1 - is because we want left hand, not right hand
#df = 31 is calculated in notebook
2 * (1 - pt(0.2054782, 31))
#since pval=0.8385 > alpha=0.01 we conclude
# Kildcon Hill is NOT an outlier

#now check if Knock Hill is an outlier

subset(hills_Rstud, hills$Race == 'KnockHill')
#probably an outlier because of the huge residual

2 * (1 - (pt(7.610845, 31)))
#since pval < 0.0001,
# Knock Hill IS an outlier

#If we were to run the above test for every observation,
# we would want to use a p-value of .05/35 because there 
# are 35 observations

#Regression Data Diagnostics

#leverage: the weight an obs has in predicting itself
hills_leverage <- lm.influence(hills_out)$hat
#check we did it correctly
subset(hills_leverage, hills$Race=='BenNevis')

#Leverage *rule of thumb*: 2*(p+1)/n
#n=num rows, p=num parameters
subset(hills, hills_leverage > 2 * 3 / 35)
#this gives us four possible influential observations

#Cook's distance: change in parameter estimates with and without obs
hills_cooks <- cooks.distance(hills_out)
#check we did it correctly
subset(hills_cooks, hills$Race == 'MoffatChase')

#Cook's distance *rule of thumb*: 4/(n-(p+1))
#n = num rows, p=num parameters
subset(hills, hills_cooks > 4 / (35 - (3)))

#NOW we must investigate whether the 5 total obs
# from both rules of thumb are GOOD or BAD influential

#look at MoffatChase, was only detected by leverage rule of thumb
par(mfrow = c(1, 2))
plot(hills$Distance, hills$Time)
points(hills$Distance[35], hills$Time[35], col = 'red', pch = 19)

#same thing for climb now

par(mfrow = c(1, 1))
plot(hills$Climb, hills$Time)
points(hills$Climb[35], hills$Time[35], col = 'red', pch = 19)
#would probably say this is good infuential

#look at LairigGru
par(mfrow = c(1, 2))
plot(hills$Distance, hills$Time)
points(hills$Distance[11], hills$Time[11], col = 'red', pch = 19)

#same thing for climb now

par(mfrow = c(1, 1))
plot(hills$Climb, hills$Time)
points(hills$Climb[11], hills$Time[11], col = 'red', pch = 19)
#would probably say this is bad influential

#look at TwoBreweries
#par(mfrow = c(1, 2))
plot(hills$Distance, hills$Time)
points(hills$Distance[33], hills$Time[33], col = 'red', pch = 19)

#same thing for climb now

#par(mfrow = c(1, 1))
plot(hills$Climb, hills$Time)
points(hills$Climb[33], hills$Time[33], col = 'red', pch = 19)
#would say that this is good influential

#if you're truly lazy
plot(hills_out)

#ANALYSIS

# filter rows and columns

#remove Knock Hill (18) since was recorded wrong

#remove LairigGru (11) since long flat race 
# where other races have length and climb correlated

#remove BensofJura (7) since very difficult terrain

hills1 <- hills[-c(18, 11, 7)]

#model: time = beta0 + beta1dist + beta2climb + epsilon, epsilon~N(0, sigma^2)

hills1_out <- lm(Time ~ Distance + Climb, data = hills1)

summary(hills1_out)

