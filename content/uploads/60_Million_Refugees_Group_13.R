rm(list=ls())

load("~/hatton2016.rdata")
head(data)
attach(data)
datanona<-data[complete.cases(data),]
bdeaths<-data$bdbest/1000

#descriptive stats table
summary<-summary(data)
deathsummary<-summary(bdeaths)
sd(datanona$lnapps)
sd(pt)
sd(datanona$fhcl)
sd(datanona$fhpr)
sd(bdeaths)
sd(lngdpsource)
sd(lngdpdest)
sd(unp)
sd(poltot)
sd(polacc)
sd(polpro)
sd(polwel)

#create 3 subset for three time periods and calculate mean
subset9701<-data[data$year<2002,]
detach(data)
attach(subset9701)
head(subset9701)
bdeaths9701<-subset9701$bdbest/1000
deathcivilwar9701mean<- mean(bdeaths9701)
freedomhousecivil9701<-mean(fhcl )
freedomhousepolitical9701<-mean(fhpr )

#transform the variables which were in log form
apps9701<- exp(lnapps)
applicationssource9701<-mean(apps9701, trim = 0, na.rm = TRUE,)

gdpdest9701<-exp(lngdpdest)
gdpdestination9701<-mean(gdpdest9701)

gdpsource9701<-exp(lngdpsource)
gdporigin9701<-mean(gdpsource9701)

policytotal9701<-mean(poltot)
policyprocess9701<-mean(polpro)
policywelfare9701<-mean(polwel)
policyaccess9701<-mean(polacc)

politicalterror9701<-mean(pt)
unemployment9701<-mean(unp)

detach(subset9701)

#second subset
subset0206<-data[data$year<2007 & data$year>2001,]
attach(subset0206)

bdeaths0206<-subset0206$bdbest/1000
deathcivilwar0206mean<- mean(bdeaths0206)
summary(bdbest)
freedomhousecivil0206<-mean(fhcl )
freedomhousepolitical0206<-mean(fhpr )

#non log
apps0206<- exp(lnapps)
applicationssource0206<-mean(apps0206, trim = 0, na.rm = TRUE,)
gdpdest0206<-exp(lngdpdest)
gdpdestination0206<-mean(gdpdest0206)
gdpsource0206<-exp(lngdpsource)
gdporigin0206<-mean(gdpsource0206)

policytotal0206<-mean(poltot)
policyprocess0206<-mean(polpro)
policywelfare0206<-mean(polwel)
policyaccess0206<-mean(polacc)

politicalterror0206<-mean(pt)
unemployment0206<-mean(unp)

detach(subset0206)

#third dataset
subset0712<-data[data$year<=2012 & data$year>2006,]
attach(subset0712)

bdeaths0712<-subset0712$bdbest/1000
deathcivilwar0712<- mean(bdeaths0712)
freedomhousecivil0712<-mean(fhcl, trim = 0, na.rm = TRUE,)
freedomhousepolitical0712<-mean(fhpr, trim = 0, na.rm = TRUE,)

#non log
apps0712<- exp(lnapps)
applicationssource0712<-mean(apps0712, trim = 0, na.rm = TRUE,)
gdpdest0712<-exp(lngdpdest)
gdpdestination0712<-mean(gdpdest0712)
gdpsource0712<-exp(lngdpsource)
gdporigin0712<-mean(gdpsource0712)

policytotal0712<-mean(poltot)
summary(poltot)
policyprocess0712<-mean(polpro)
policywelfare0712<-mean(polwel)
policyaccess0712<-mean(polacc)

politicalterror0712<-mean(pt)
unemployment0712<-mean(unp)


detach(subset0712)
attach(data)

#Plot the average applications over the years
#use CI to check whether the averages were different over the years
#i do it using the mean calculated above all years
install.packages("gplots")
library(gplots)
plotmeans(lnapps~year, data=data, connect=FALSE)

boxplot(subset9701$lnapps, subset0206$lnapps, subset0712$lnapps, main= "Log of applications per capita of source population", names = c("1997-2001", "2002-2006", "2007-2012"))
abline( h = 5.29977)
abline( h = 4.84452)

#Check country wise difference in applications
dcode1<-data[data$dcode==1, ]
apps<-exp(dcode1$lnapps)
meand1<-mean(apps, na.rm=TRUE)

dcode2<-data[data$dcode==2, ]
apps<-exp(dcode2$lnapps)
meand2<-mean(apps, na.rm=TRUE)

dcode3<-data[data$dcode==3, ]
apps<-exp(dcode3$lnapps)
meand3<-mean(apps, na.rm=TRUE)

dcode4<-data[data$dcode==4, ]
apps<-exp(dcode4$lnapps)
meand4<-mean(apps, na.rm=TRUE)

dcode5<-data[data$dcode==5, ]
apps<-exp(dcode5$lnapps)
meand5<-mean(apps, na.rm=TRUE)

dcode6<-data[data$dcode==6, ]
apps<-exp(dcode6$lnapps)
meand6<-mean(apps, na.rm=TRUE)

dcode7<-data[data$dcode==7, ]
apps<-exp(dcode7$lnapps)
meand7<-mean(apps, na.rm=TRUE)

dcode8<-data[data$dcode==8, ]
apps<-exp(dcode8$lnapps)
meand8<-mean(apps, na.rm=TRUE)

dcode9<-data[data$dcode==9, ]
apps<-exp(dcode9$lnapps)
meand9<-mean(apps, na.rm=TRUE)

dcode10<-data[data$dcode==10, ]
apps<-exp(dcode10$lnapps)
meand10<-mean(apps, na.rm=TRUE)

dcode11<-data[data$dcode==11, ]
apps<-exp(dcode11$lnapps)
meand11<-mean(apps, na.rm=TRUE)

dcode12<-data[data$dcode==12, ]
apps<-exp(dcode12$lnapps)
meand12<-mean(apps, na.rm=TRUE)

dcode13<-data[data$dcode==13, ]
apps<-exp(dcode13$lnapps)
meand13<-mean(apps, na.rm=TRUE)

dcode14<-data[data$dcode==14, ]
apps<-exp(dcode14$lnapps)
meand14<-mean(apps, na.rm=TRUE)

dcode15<-data[data$dcode==15, ]
apps<-exp(dcode15$lnapps)
meand15<-mean(apps, na.rm=TRUE)

dcode16<-data[data$dcode==16, ]
apps<-exp(dcode16$lnapps)
meand16<-mean(apps, na.rm=TRUE)

dcode17<-data[data$dcode==17, ]
apps<-exp(dcode17$lnapps)
meand17<-mean(apps, na.rm=TRUE)

dcode18<-data[data$dcode==18, ]
apps<-exp(dcode18$lnapps)
meand18<-mean(apps, na.rm=TRUE)

dcode19<-data[data$dcode==19, ]
apps<-exp(dcode19$lnapps)
meand19<-mean(apps, na.rm=TRUE)


vectorapps<-c(meand1, meand2, meand3, meand4, meand5, meand6,meand7,meand8,meand9,meand10,meand11,meand12,meand13,meand14,meand15,meand16,meand17,meand18,meand19)

countrywise<-barplot(vectorapps,main = "Applications in different countries", xlab="Countries", ylab="Average number of applications per capita of source population", names.arg=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))


#and its correlation with the per capita GDP for origin and destination regions
#i keep destination fixed and i vary source, done for specific years

#country 1
subset.destination.country1.2001<-data[data$dcode==1 & data$year==2001, ]
detach(data)
attach(subset.destination.country1.2001)
plot(lngdpsource,lnapps, main = "Country 1, year 2001", xlab="Log real GDP per capita in source country", ylab="Log of applications per capita of source population", pch=1)
abline(lm(lnapps ~lngdpsource, data = subset.destination.country1.2001), col = "blue")
detach(subset.destination.country1.2001)

#country 2
subset.destination.country2.2001<-data[data$dcode==2& data$year==2001,]
attach(subset.destination.country2.2001)
plot(lngdpsource,lnapps, main = "Country 2, year 2001", xlab="Log real GDP per capita in source country", ylab="Log of applications per capita of source population", pch=1)
abline(lm(lnapps ~ lngdpsource, data = subset.destination.country2.2001), col = "blue")
detach(subset.destination.country2.2001)


#country 19
subset.destination.country19.2001<-data[data$dcode==19& data$year==2001,]
attach(subset.destination.country19.2001)
plot(lngdpsource,lnapps, main = "Destination country 19, year 2001", xlab="Log real GDP per capita in source country", ylab="Log of applications per capita of source population", pch=1)
abline(lm(lnapps ~ lngdpsource, data = subset.destination.country19.2001), col = "blue")
detach(subset.destination.country19.2001)

#country 7
subset.destination.country7.2001<-data[data$dcode==7& data$year==2001,]
attach(subset.destination.country7.2001)
plot(lngdpsource,lnapps, main = "Destination country 7, year 2001", xlab="Log real GDP per capita in source country", ylab="Log of applications per capita of source population", pch=1)
abline(lm(lnapps ~ lngdpsource, data = subset.destination.country7.2001), col = "blue")
detach(subset.destination.country7.2001)

#change year, 2012
#country 1
subset.destination.country1.2012<-data[data$dcode==1 & data$year==2012, ]
attach(subset.destination.country1.2012)
country1.2012<-plot(lngdpsource,lnapps, main = "Destination country 1, year 2012", xlab="Log real GDP per capita in source country", ylab="Log of applications per capita of source population", pch=1)
country1.2012line<-abline(lm(lnapps ~lngdpsource, data = subset.destination.country1.2012), col = "blue")
detach(subset.destination.country1.2012)
attach(data)


#country 2
subset.destination.country2.2012<-data[data$dcode==2& data$year==2012,]
detach(data)
attach(subset.destination.country2.2012)
country2.2012<-plot(lngdpsource,lnapps, main = "Destination country 2, year 2012", xlab="Log real GDP per capita in source country", ylab="Log of applications per capita of source population", pch=1)
country2.2012line<-abline(lm(lnapps ~ lngdpsource, data = subset.destination.country2.2012), col = "blue")
detach(subset.destination.country2.2012)
attach(data)


#country 19
subset.destination.country19.2012<-data[data$dcode==19& data$year==2012,]
detach(data)
attach(subset.destination.country19.2012)
plot(lngdpsource,lnapps, main = "Destination country 19, year 2012", xlab="Log real GDP per capita in source country", ylab="Log of applications per capita of source population", pch=1)
abline(lm(lnapps ~ lngdpsource, data = subset.destination.country19.2012), col = "blue")
detach(subset.destination.country19.2012)

#Plot scatter plots between the key determinants and 
#log applications to see the correlation between the two,
#let's do it for 2001
subset.2001<-data[data$year==2001,]
attach(subset.2001)
bdeaths<-subset.2001$bdbest/1000


plot(bdeaths,lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Uppsala index of battle deaths in civil war (s×y)", pch=1)
abline(lm(lnapps ~ bdeaths, data = subset.2001), col = "blue")
plot(fhcl, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Freedom House index of civil liberties (s×y)", pch=1)
abline(lm(lnapps ~ fhcl, data = subset.2001), col = "blue")
plot(fhpr, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Freedom house index of political rights (s×y)", pch=1)
abline(lm(lnapps ~ fhpr, data = subset.2001), col = "blue")
plot(lngdpdest, lnapps, main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Log GDP per capita in destination country (s×y)", pch=1)
abline(lm(lnapps ~ lngdpdest, data = subset.2001), col = "blue")
plot(lngdpsource, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Log real GDP per capita in source country (s×y) ", pch=1)
abline(lm(lnapps ~ lngdpsource, data = subset.2001), col = "blue")
plot(polacc, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Policy index on access to territory (d×y)", pch=1)
abline(lm(lnapps ~ polacc, data = subset.2001), col = "blue")
plot(polpro, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Policy index on asylum processing (d×y)", pch=1)
abline(lm(lnapps ~ polpro, data = subset.2001), col = "blue")
plot(poltot, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Total policy index (d×y)", pch=1)
abline(lm(lnapps ~ poltot, data = subset.2001), col = "blue")
plot(polwel, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Policy index on welfare for asylum seekers (d×y)", pch=1)
abline(lm(lnapps ~ polwel, data = subset.2001), col = "blue")
plot(pt, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Political terror scale (s×y)", pch=1)
abline(lm(lnapps ~ pt, data = subset.2001), col = "blue")
plot(unp, lnapps,  main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Unemployment rate at destination (d×y)", pch=1)
abline(lm(lnapps ~unp , data = subset.2001), col = "blue")
plot(lndist, lnapps, main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Log distance from source to destination (sxd)", pch=1)
abline(lm(lnapps ~lndist , data = subset.2001), col = "blue")
plot(lnsttot, lnapps, main = "Year 2001", ylab="Log of applications per capita of source population", xlab="Log migrant stock from source at destination in 2000/1 (d×s)", pch=1)
abline(lm(lnapps ~lnsttot , data = subset.2001), col = "blue")

detach(subset.2001)

#this can be done for specific years, i do it for 2000
subset2000<-data[data$year==2000,]
attach(subset2000)
plot(bdbest, lnapps, main = "Year 2000", ylab="Log of applications per capita of source population", xlab="Uppsala index of battle deaths in civil war (s×y)", pch=1)
abline(lm(lnapps ~ bdbest, data = subset2000), col = "blue")
fhcl2000<-plot( fhcl,lnapps, main = "Year 2000", ylab="Log of applications per capita of source population", xlab="Freedom House index of civil liberties (s×y)", pch=1)
abline(lm(lnapps ~ fhcl, data = subset2000), col = "blue")
plot( fhpr,lnapps, main = "Year 2000",  ylab="Log of applications per capita of source population", xlab="Freedom house index of political rights (s×y)", pch=1)
abline(lm(lnapps ~ fhpr, data = subset2000), col = "blue")
plot(lngdpdest,lnapps, main = "Year 2000",  ylab="Log of applications per capita of source population", xlab="Log GDP per capita in destination country (s×y)", pch=1)
abline(lm(lnapps ~ lngdpdest, data = subset2000), col = "blue")
plot( lngdpsource, lnapps,main = "Year 2000",  ylab="Log of applications per capita of source population", xlab="Log real GDP per capita in source country (s×y) ", pch=1)
abline(lm(lnapps ~ lngdpsource, data = subset2000), col = "blue")

plot(polacc,lnapps,  main = "Year 2000",  ylab="Log of applications per capita of source population", xlab="Policy index on access to territory (d×y)", pch=1)
abline(lm(lnapps ~ polacc, data = subset2000), col = "blue")
plot( polpro,lnapps, main = "Year 2000",  ylab="Log of applications per capita of source population", xlab="Policy index on asylum processing (d×y)", pch=1)
abline(lm(lnapps ~ polpro, data = subset2000), col = "blue")
plot( poltot, lnapps,main = "Year 2000", ylab="Log of applications per capita of source population", xlab="Total policy index (d×y)", pch=1)
abline(lm(lnapps ~ poltot, data = subset2000), col = "blue")
plot( polwel, lnapps, main = "Year 2000", ylab="Log of applications per capita of source population", xlab="Policy index on welfare for asylum seekers (d×y)", pch=1)
abline(lm(lnapps ~ polwel, data = subset2000), col = "blue")
plot( pt, lnapps, main = "Year 2000", ylab="Log of applications per capita of source population", xlab="Political terror scale (s×y)", pch=1)
abline(lm(lnapps ~ pt, data = subset2000), col = "blue")
plot( unp,lnapps, main = "Year 2000",  ylab="Log of applications per capita of source population", xlab="Unemployment rate at destination (d×y)", pch=1)
abline(lm(lnapps ~ unp, data = subset2000), col = "blue")
plot(lndist, lnapps, main = "Year 2000", ylab="Log of applications per capita of source population", xlab="Log distance from source to destination (sxd)", pch=1)
abline(lm(lnapps ~ lndist, data = subset2000), col = "blue")
plot(lnsttot, lnapps, main = "Year 2000", ylab="Log of applications per capita of source population", xlab="Log migrant stock from source at destination in 2000/1 (d×s)", pch=1)
abline(lm(lnapps ~lnsttot , data = subset2000), col = "blue")


detach(subset2000)


#this can be done for specific years, i'll do it in 2012
subset2012<-data[data$year==2012,]
attach(subset2012)
plot(bdbest, lnapps, main = "Year 2012", ylab="Log of applications per capita of source population", xlab="Uppsala index of battle deaths in civil war (s×y)", pch=1)
abline(lm(lnapps ~ bdbest, data = subset2012), col = "blue")
plot( fhcl,lnapps, main = "Year 2012", ylab="Log of applications per capita of source population", xlab="Freedom House index of civil liberties (s×y)", pch=1)
abline(lm(lnapps ~ fhcl, data = subset2012), col = "blue")
plot( fhpr,lnapps, main = "Year 2012",  ylab="Log of applications per capita of source population", xlab="Freedom house index of political rights (s×y)", pch=1)
abline(lm(lnapps ~ fhpr, data = subset2012), col = "blue")
plot(lngdpdest,lnapps, main = "Year 2012",  ylab="Log of applications per capita of source population", xlab="Log GDP per capita in destination country (s×y)", pch=1)
abline(lm(lnapps ~ lngdpdest, data = subset2012), col = "blue")
plot( lngdpsource, lnapps,main = "Year 2012",  ylab="Log of applications per capita of source population", xlab="Log real GDP per capita in source country (s×y) ", pch=1)
abline(lm(lnapps ~ lngdpsource, data = subset2012), col = "blue")

plot(polacc,lnapps,  main = "Year 2012",  ylab="Log of applications per capita of source population", xlab="Policy index on access to territory (d×y)", pch=1)
abline(lm(lnapps ~ polacc, data = subset2012), col = "blue")
plot( polpro,lnapps, main = "Year 2012",  ylab="Log of applications per capita of source population", xlab="Policy index on asylum processing (d×y)", pch=1)
abline(lm(lnapps ~ polpro, data = subset2012), col = "blue")
plot( poltot, lnapps,main = "Year 2012", ylab="Log of applications per capita of source population", xlab="Total policy index (d×y)", pch=1)
abline(lm(lnapps ~ poltot, data = subset2012), col = "blue")
plot( polwel, lnapps, main = "Year 2012", ylab="Log of applications per capita of source population", xlab="Policy index on welfare for asylum seekers (d×y)", pch=1)
abline(lm(lnapps ~ polwel, data = subset2012), col = "blue")
plot( pt, lnapps, main = "Year 2012", ylab="Log of applications per capita of source population", xlab="Political terror scale (s×y)", pch=1)
abline(lm(lnapps ~ pt, data = subset2012), col = "blue")
plot( unp,lnapps, main = "Year 2012",  ylab="Log of applications per capita of source population", xlab="Unemployment rate at destination (d×y)", pch=1)
abline(lm(lnapps ~ unp, data = subset2012), col = "blue")
plot(lndist, lnapps, main = "Year 2012", ylab="Log of applications per capita of source population", xlab="Log distance from source to destination (sxd)", pch=1)
abline(lm(lnapps ~ lndist, data = subset2012), col = "blue")
plot(lnsttot, lnapps, main = "Year 2012", ylab="Log of applications per capita of source population", xlab="Log migrant stock from source at destination in 2000/1 (d×s)", pch=1)
abline(lm(lnapps ~lnsttot , data = subset2012), col = "blue")
detach(subset2012)



##########table 2#########
attach(data)
bdeaths<-data$bdbest/1000
m1<-lm(lnapps~pt+bdeaths+fhcl+fhpr+lndist+lngdpdest+lngdpsource+lnsttot+unp)
summary(m1)
#first column equal to ols_results.pdf

#let's do a breusch pagan test
install.packages("lmtest")
library(lmtest)
bptest(m1)

#let's test for autocorrelation
install.packages("plm")
library(plm)
dwtest(m1)

#I compute the HAc std errors
library(lmtest)
m <- floor(0.75 * 10128^(1/3))
NW_VCOV <- NeweyWest(m1, lag = m-1,)
coeftest(m1, vcov = NW_VCOV)

#second column
m2<-lm(lnapps~pt+bdeaths+fhcl+fhpr+lngdpdest+lngdpsource+lnsttot+unp+poltot, data=data)
summary(m2)
bptest(m2)
dwtest(m2)

library(lmtest)
NW_VCOV <- NeweyWest(m2, lag = m-1,)
coeftest(m2, vcov = NW_VCOV)

#third column
m3<-lm(lnapps~pt+bdeaths+fhcl+fhpr+lngdpdest+lngdpsource+lnsttot+unp+polacc+polpro+polwel, data=data)
summary(m3)

bptest(m3)
dwtest(m3)

library(sandwich)
NW_VCOV <- NeweyWest(m3, lag = m-1,)
coeftest(m3, vcov = NW_VCOV)

#fourth column
m4<-lm(lnapps~pt+bdeaths+fhcl+fhpr+lngdpdest+lngdpsource+lnsttot+lndist+unp+polacc+polpro+polwel, data=data)
summary(m4)

bptest(m4)
dwtest(m4)

library(sandwich)
NW_VCOV <- NeweyWest(m4, lag = m-1,)
coeftest(m4, vcov = NW_VCOV)
#############table 3############

#IN REGRESSION 1 include dummies for year, destination
#fixed effect are origin, scode
install.packages("fastDummies")
library(fastDummies)
df_dummy<-dummy_cols(data, select_columns = c("dcode", "year"), ignore_na = TRUE,)

install.packages("lfe")
library(lfe)
reg1felm <- felm(lnapps ~ pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lndist + lngdpdest + unp +
                dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 +
                year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | scode | 0 | scode, data=df_dummy)
summary(reg1felm)

#test for heteroskedasticity
library(lmtest)
bptest(reg1felm)

#let's test for serial correlation
dwtest(reg1felm)

#let's test joint significance
install.packages("car")
library(car)
reg1felmjoint<-felm(lnapps~pt+bdeaths+fhcl+fhpr+
                 lndist+lngdpdest+lngdpsource+lnsttot+unp+
                 factor(year)+factor(dcode) | scode | 0 | scode)

linearHypothesis(reg1felmjoint, c("bdeaths=0", "pt=0"), white.adjust = "hc1")
linearHypothesis(reg1felmjoint, c("lndist=0", "lnsttot=0"), white.adjust = "hc1")

#IN REGRESSION 2 includo year, origin by destination
#fixed effect sono orignxdestination, dscode


reg2felm <- felm(lnapps ~ pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lngdpdest + unp + poltot +
                year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg2felm)

bptest(reg2felm)
dwtest(reg2felm)

reg2felmjoint<-felm(lnapps~pt+fhcl+fhpr+bdeaths+lngdpsource+lngdpdest+unp+poltot+factor(year) | dscode | 0 | dscode)

linearHypothesis(reg2felmjoint, c("bdeaths=0", "pt=0"), white.adjust = "hc1")

#IN REGRESSION 3 include year, origin by destination
#fixed effect are originxdestination, dscode

reg3felm <- felm(lnapps ~ pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lngdpdest + unp + polacc + polpro + polwel +
                year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg3felm)

bptest(reg3felm)
dwtest(reg3felm)

reg3felmjoint<-felm(lnapps~pt+fhcl+fhpr+bdeaths+lngdpsource+lngdpdest+unp+polacc+polpro+polwel+factor(year) | dscode | 0 | dscode)


linearHypothesis(reg3felmjoint, c("polwel=0", "lngdpdest=0"), white.adjust = "hc1")
linearHypothesis(reg3felmjoint, c("bdeaths=0", "pt=0"), white.adjust = "hc1")

#IN REGRESSION 4 includo destination, origin by year
#fixed effect sono originxyear, scodeyear

reg4felm<- felm(lnapps ~ pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lndist + lngdpdest + unp + polacc + polpro + polwel +
                  dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                  dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 | scodeyear | 0 | scodeyear, data=df_dummy)
summary(reg4felm)

bptest(reg4felm)
dwtest(reg4felm)

reg4felmjoint<-felm(lnapps~lnsttot+lndist+lngdpdest+unp+polacc+polpro+polwel+factor(dcode) | scodeyear | 0 | scodeyear)

linearHypothesis(reg4felmjoint, c("polwel=0", "lngdpdest=0"), white.adjust = "hc1")
linearHypothesis(reg4felmjoint, c("lndist=0", "lnsttot=0"), white.adjust = "hc1")

##### sensitivity analysis #####
#let's exclude the two outliers, country 7 and 8
detach(data)
nooutliers<- subset(df_dummy, dcode > 8 | dcode < 7,)
attach(nooutliers)  
bdeathsnoout<-nooutliers$bdbest/1000


reg1felmnoout<-felm(lnapps ~ pt + fhcl + fhpr + bdeathsnoout + lngdpsource + lnsttot + lndist + lngdpdest + unp +
                      dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                      dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 +
                      year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                      year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | scode | 0 | scode, data = nooutliers)

summary(reg1felmnoout)

reg2felmnoout<-felm(lnapps ~ pt + fhcl + fhpr + bdeathsnoout + lngdpsource + lnsttot + lngdpdest + unp + poltot +
                      year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                      year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data = nooutliers)

  summary(reg2felmnoout)

reg3felmnoout<-felm(lnapps ~ pt + fhcl + fhpr + bdeathsnoout + lngdpsource + lnsttot + lngdpdest + unp + polacc + polpro + polwel +
                      year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                      year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data = nooutliers)
summary(reg3felmnoout)

reg4felmnoout<-felm(lnapps ~ pt + fhcl + fhpr + bdeathsnoout + lngdpsource + lnsttot + lndist + lngdpdest + unp + polacc + polpro + polwel +
                      dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                      dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 | scodeyear | 0 | scodeyear, data = nooutliers)
summary(reg4felmnoout)

#only outliers
detach(nooutliers)
outliers<- subset(df_dummy, dcode == 8 | dcode == 7,)
attach(outliers)
bdeathsout<-outliers$bdbest/1000

reg1felmout<-felm(lnapps ~ pt + fhcl + fhpr + bdeathsout + lngdpsource + lnsttot + lndist + lngdpdest + unp +
                    dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                    dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 +
                    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                    year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | scode | 0 | scode, data=outliers)
summary(reg1felmout)

reg2felmout<-felm(lnapps ~ pt + fhcl + fhpr + bdeathsout + lngdpsource + lnsttot + lngdpdest + unp + poltot +
                    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                    year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=outliers)
summary(reg2felmout)

reg3felmout<-felm(lnapps ~ pt + fhcl + fhpr + bdeathsout + lngdpsource + lnsttot + lngdpdest + unp + polacc + polpro + polwel +
                    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                    year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=outliers)
summary(reg3felmout)

reg4felmout<-felm(lnapps ~ pt + fhcl + fhpr + bdeathsout + lngdpsource + lnsttot + lndist + lngdpdest + unp + polacc + polpro + polwel +
                    dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                    dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 | scodeyear | 0 | scodeyear, data=outliers)
summary(reg4felmout)

detach(outliers)
attach(data)

#let's try to put gdp source squared.
lngdporsq<-(data$lngdpsource)*(data$lngdpsource)

reg1felmsq<-felm(lnapps ~ pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lndist + lngdpdest + lngdporsq + unp +
                   dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                   dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 +
                   year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                   year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | scode | 0 | scode, data=df_dummy)
summary(reg1felmsq)

reg2felmsq<-felm(lnapps ~ pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lngdpdest + lngdporsq + unp + poltot +
                   year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                   year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg2felmsq)
reg3felmsq<-felm(lnapps ~ pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lngdpdest + lngdporsq + unp + polacc + polpro + polwel +
                   year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                   year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
  summary(reg3felmsq)
reg4felmsq<-felm(lnapps ~ pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lndist + lngdpdest + lngdporsq + unp + polacc + polpro + polwel +
                   dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                   dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 | scodeyear | 0 | scodeyear, data=df_dummy)

  summary(reg4felmsq)

#let's try to put a trend 
trend<-seq_along(lnapps)

reg1felmtrend <- felm(lnapps ~ trend + pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lndist + lngdpdest + unp +
                dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 +
                year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | scode | 0 | scode, data=df_dummy)
summary(reg1felmtrend)
reg2felmtrend <- felm(lnapps ~ trend + pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lngdpdest + unp + poltot +
                year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg2felmtrend)
reg3felmtrend <- felm(lnapps ~ trend + pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lngdpdest + unp + polacc + polpro + polwel +
                year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg3felmtrend)
reg4felmtrend <- felm(lnapps ~ trend + pt + fhcl + fhpr + bdeaths + lngdpsource + lnsttot + lndist + lngdpdest + unp + polacc + polpro + polwel +
                dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 | scodeyear | 0 | scodeyear, data=df_dummy)
summary(reg4felmtrend)

gdptrend<-lm(lngdpdest~trend)
summary(gdptrend)

#lagged model
install.packages("Hmisc")
library(Hmisc)
gdporigin_1 <- Lag(lngdpsource, -1)
pt_1<- Lag(pt, -1)
fhcl_1<- Lag(fhcl, -1)
fhpr_1<- Lag(fhpr, -1)
bdeaths_1<- Lag(bdeaths, -1)

reg1felmlag<-felm(lnapps~pt+pt_1+fhcl+fhcl_1+fhpr+fhpr_1+lndist+lngdpdest+lngdpsource+gdporigin_1+lnsttot+unp+bdeaths+bdeaths_1+dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                    dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 +
                    year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                    year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | scode | 0 | scode, data=df_dummy)
summary(reg1felmlag)
reg2felmlag<-felm(lnapps~+pt+pt_1+fhcl+fhcl_1+fhpr_1+fhpr+bdeaths+bdeaths_1+lngdpsource+gdporigin_1+lngdpdest+unp+poltot+year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                    year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg2felmlag)
reg3felmlag<-felm(lnapps~pt+pt_1+fhcl+fhcl_1+fhpr+fhpr_1+bdeaths+bdeaths_1+lngdpsource+gdporigin_1+lngdpdest+unp+polacc+polpro+polwel+
  year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
  year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg3felmlag)


#lag 2
gdporigin_2 <- Lag(lngdpsource, -2)
pt_2<- Lag(pt, -2)
fhcl_2<- Lag(fhcl, -2)
fhpr_2<- Lag(fhpr, -2)
bdeaths_2<- Lag(bdeaths, -2)

reg1felmlag2<-felm(lnapps~pt+pt_2+fhcl+fhcl_2+fhpr+fhpr_2+lndist+lngdpdest+lngdpsource+gdporigin_2+lnsttot+unp+bdeaths+bdeaths_2+dcode_1 + dcode_2 + dcode_3 + dcode_4 + dcode_5 + dcode_6 + dcode_7 + dcode_8 + dcode_9 + dcode_10 +
                     dcode_11 + dcode_12 + dcode_13 + dcode_14 + dcode_15 + dcode_16 + dcode_17 + dcode_18 + dcode_19 +
                     year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                     year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | scode | 0 | scode, data=df_dummy)
summary(reg1felmlag2)
reg2felmlag2<-felm(lnapps~+pt+pt_2+fhcl+fhcl_2+fhpr_2+fhpr+bdeaths+bdeaths_2+lngdpsource+gdporigin_2+lngdpdest+unp+poltot+year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                     year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg2felmlag2)
reg3felmlag2<-felm(lnapps~pt+pt_2+fhcl+fhcl_2+fhpr+fhpr_2+bdeaths+bdeaths_2+lngdpsource+gdporigin_2+lngdpdest+unp+polacc+polpro+polwel+ year_1997 + year_1998 + year_1999 + year_2000 + year_2001 + year_2002 + year_2003 + year_2004 +
                     year_2005 + year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 | dscode | 0 | dscode, data=df_dummy)
summary(reg3felmlag2)

detach(data)