library( plm )
library( car )
library( dplyr )
library( extrafont )
library( gplots )
library( ggplot2 )
library( reshape2 )
library(lmtest)
library(sandwich)
require(foreign)

setwd("~/Documents/Egyetem/únkp/data")
data <- readxl:: read_excel( "fomc panel.xlsx", sheet = "fomc_panel" )
moredata <-readxl:: read_excel( "fomc panel.xlsx", sheet = "fomc_panel_b" )


#scaling by countrycode
data <- data %>% group_by(ccode) %>% 
  mutate(yield10y_2day = scale(yield10y_2day,center=FALSE)) %>% 
  mutate(yield3m_2day = scale(yield3m_2day,center=FALSE)) %>%
  mutate(yield2y_2day = scale(yield2y_2day,center=FALSE)) %>%
  mutate(yield5y_2day = scale(yield5y_2day,center=FALSE)) %>%
  mutate(exp2y_2day = scale(exp2y_2day,center=FALSE)) %>%
  mutate(exp10y_2day = scale(exp10y_2day,center=FALSE)) %>%
  mutate(tp2y_2day  = scale(tp2y_2day,center=FALSE)) %>%
  mutate(tp10y_2day  = scale(tp10y_2day,center=FALSE)) %>%
  mutate(fx_2day = scale(fx_2day,center=FALSE)) %>%
  mutate(stk_2day = scale(stk_2day,center=FALSE)) %>%
  mutate(mpu_level = scale(mpu_level,center=TRUE,scale=FALSE)) %>%
  ungroup

data$date<-as.Date(data$date)

data2<-merge(data,moredata, by=c("ccode","date"))

#* Drop Venezuela, US, Brazil, Mexico, Taiwan, Greece
#keep if ccode < 49 
#drop if ccode == 5 | ccode == 28 | ccode == 45 | ccode == 15 

data <- subset(data2, !(Country_Name.x %in% c("United States", "Brazil", "Mexico", "Taiwan", "Greece")))

pdata <- pdata.frame(data, index = c("ccode","date"))




#most elevant variables from the database: 
#mps2_2day - shocks obtained from Lakdawala et al. (2021) and normalized to have a unit standard deviation of the changes in 1-year ahead 3-month eurodollar future instrument
#cbi2_2day - information shocks obtained from the decomposition technique used by Jarocinski and Karádi (2020), see Matlab code 
#mp2_2day - information shocks obtained from the decomposition technique used by Jarocinski and Karádi (2020)
#mpu_2day - change in uncertainty around the announcement obtained from Bauer et al.(2022)
#mpu_level - uncertainty level preceeding the announcement day obtained from Bauer et al.(2022)

#poor man's shocks:
pdata$cbi_pm<-as.numeric(pdata$cbi_pm)
pdata$mp_pm<-as.numeric(pdata$mp_pm)


#Table 3 - mps_total effect on local currency bond yields

pooled<-plm(yield3m_2day.x~mps2_2day+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

pooled<-plm(yield3m_2day.x~mps2_2day+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

pooled<-plm(yield3m_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

pooled<-plm(yield2y_2day.x~mps2_2day+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

pooled<-plm(yield2y_2day.x~mps2_2day+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

pooled<-plm(yield2y_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

pooled<-plm(yield10y_2day.x~mps2_2day+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

pooled<-plm(yield10y_2day.x~mps2_2day+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

pooled<-plm(yield10y_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovBK,cluster="time")

#Table 4 

pooled<-plm(yield10y_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(yield10y_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)

#Table 5

pdata$date<-as.Date(pdata$date)
pre<-subset(pdata,date<as.Date("2011-01-01"))
post<-subset(pdata,date>as.Date("2011-01-01"))
pre <- pre %>% 
  mutate(mpu_level.x = scale(mpu_level.x,center=TRUE, scale=TRUE))
post <- post %>% 
  mutate(mpu_level.x = scale(mpu_level.x,center=TRUE, scale=TRUE))

pooled<-plm(yield10y_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pre,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(yield10y_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(post,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)

pooled<-plm(yield10y_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pre,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(yield10y_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(post,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)

#Table 6

pooled<-plm(stk_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(stk_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)

pooled<-plm(fx_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(fx_2day.x~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)

GRA<-lm(vxo~mps2_2day+mpu_2day.x+mpu_level.x+mps2_2day*mpu_level.x+us_yield10y+VIX_level,data=subset(pdata,ccode==4))
summary(GRA)

#Table 7

pooled<-plm(stk_2day.x~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(stk_2day.x~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)

pooled<-plm(fx_2day.x~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(fx_2day.x~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)

GRA<-lm(vxo~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,data=subset(pdata,ccode==4))
summary(GRA)

#Table 8

pooled<-plm(yield2y_2day.x~mp2_2day+cbi2_2day+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(yield2y_2day.x~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(yield10y_2day.x~mp2_2day+cbi2_2day+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(yield10y_2day.x~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=pdata,model="pooling")
coeftest(pooled,vcovDC)

#Table 9
pooled<-plm(yield10y_2day.x~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(exp10y_2day~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(tp10y_2day~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==0),model="pooling")
coeftest(pooled,vcovDC)

pooled<-plm(yield10y_2day.x~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(exp10y_2day~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)
pooled<-plm(tp10y_2day~mp2_2day+cbi2_2day+mpu_2day.x+mpu_level.x+mp2_2day*mpu_level.x+cbi2_2day*mpu_level.x+us_yield10y+VIX_level,index=c("ccode","date"),data=subset(pdata,advanced.x==1),model="pooling")
coeftest(pooled,vcovDC)


#Tests to be run on any model residual


bptest(pooled) #heteroskedasticity, if p<0.05

#cross-sectional dependency - the error for one unit may be correlated with the errors for other unit in the same time period 
pcdtest(pooled, test = c("cd")) #cross-sectional dependence, if p<0.05

#serial correlation (autocorrelation) - the errors for a given unit are correlated with previous errors for that unit
pbgtest(pooled,order=1) #serial correlation, if p<0.05
pbsytest(pooled)










