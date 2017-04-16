#rm(list=ls())
library(CASdatasets)
library(sqldf)
library(tcltk)
library(fitdistrplus)
library(gdata)


source("function.R")

# data #
data(fremotor1freq)
data(fremotor1sev)
data(fremotor1prem)

freq=fremotor1freq;
sev=fremotor1sev;
prem=fremotor1prem;

####### generation for the estimation of loss #######

### data ###
sev03=subset(sev, format(sev$OccurDate,'%Y')==2003)
prem03=subset(prem, prem$Year==2003)

# Calibration - Frequency (By Guarantee)
Nb_Guarantee=Nb_sinistre(sev03)
Freq_Windscreen=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Windscreen")
Freq_Damage=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Damage")
Freq_Fire=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Fire")
Freq_TPL=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "TPL")
Freq_Theft=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Theft")
Freq_Other=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Other")

# Calibration - Severity (By Guarantee)
Est_Windscreen_Sev=calib_Sev(sev03,"Windscreen",point_ajust=4.5)
Est_Damage_Sev=calib_Sev(sev03,"Damage",point_ajust=5)
Est_Fire_Sev=calib_Sev(sev03,"Fire",point_ajust=6,size=10)
Est_TPL_Sev=calib_Sev(sev03,"TPL",point_ajust=5)
Est_Theft_Sev=calib_Sev(sev03,"Theft",point_ajust=5)
Est_Other_Sev=calib_Sev(sev03,"Other",point_ajust=5,ysup=1)


### genarator ###
# 1 generation of frequency+severity for all the policies with a certain type of Guarantee
# distribution frequency - 
# distribution severity - log Gamma
Generation_sev_freq=function(nb_scenario, param_freq, param_sev){
  rgamma(nb_scenario, shape=param_sev[1], rate = 1, scale = 1/rate)
  
  PnL_QS_Ass=retention*(Premium-Severity) + Commission*(1-retention)*Premium
  PnL_QS_ReAss=(1-retention)*(Premium-Severity) - Commission*(1-retention)*Premium
}




####### Implementation quota-share reinsurance #######
retention=0.8
commision=0.25

premium03=subset(prem, prem$Year==2003)
severite03=subset(sev, format(sev$OccurDate,'%Y')==2003)

Reins_QS=function(Premium, Severity, Retention, Commission){
  PnL_QS_Ass=retention*(Premium-Severity) + Commission*(1-retention)*Premium
  PnL_QS_ReAss=(1-retention)*(Premium-Severity) - Commission*(1-retention)*Premium
}

### test
premium03=subset(prem, prem$Year==2003)
severite03=subset(sev, format(sev$OccurDate,'%Y')==2003)
PnL_QS_03=(1-commission)*premium03 - (1-retention)*sevrite03

####### Implementation excess of loss reinsurance #######
priorite=200
plafond=300
commision=0.25

# a modifier
Reins_XL=function(Premium, Severity, Priorite, Plafond, Commission){
  PnL_QS_Ass=retention*(Premium-Severity) + Commission*(1-retention)*Premium
  PnL_QS_ReAss=(1-retention)*(Premium-Severity) - Commission*(1-retention)*Premium
}

### test
premium03=subset(prem, prem$Year==2003)
severite03=subset(sev, format(sev$OccurDate,'%Y')==2003)
PnL_QS_03=(1-commission)*premium03 - (1-retention)*sevrite03



####### pricing quota-share reinsurance MC #######
Pricing_QS(Premium, Severity, Priorite, Plafond, Commission, factor){
  sub_severity=subset(severity, Guarantee==factor)
  sub_premium=subset(frequency, )
  
  return(p);
}



####### xxxxx #######

# library(lubridate)

library(fitdistrplus)

nrow(sev) # 18057
length(unique(sev$IDpol)) # 10917

sev03=subset(sev, format(sev$OccurDate,'%Y')==2003)
summary(sev03$Guarantee)
sev03_TPL=subset(sev03, sev03$Guarantee=="TPL")
hist(yday(sev03_TPL$OccurDate)/365)

sev03_TPL=subset(sev03, sev03$Guarantee=="TPL")

summary(yday(sev03_TPL$OccurDate))
f_sev03=fitdist(x, "gamma", method="mle")

plot(subset(fremotor1prem$PremTot, fremotor1prem$Year=="2003"))

hist(subset(fremotor1sev$Payment, fremotor1sev$OccurDate%Y=="2003"))

summary(subset(fremotor1sev$Payment,
               format(fremotor1sev$OccurDate,'%Y')==2004,
               fremotor1sev$Guarantee=="Fire"))



# Freq : multinomiale
Calib_Freq=function(Severity, Premium, Factor){
  num_risk=length(Factor)
  p=rep(0,num)
  for(i in 1:num){
    sub_severity=subset(Severity, Guarantee==Factor[i])
    num_pol=length(unique(subset(Premium, Factor[i]>0)[,"IDpol"]))
    p[i]=sum(sign(Severity[]Factor))/num_pol
  }
  
  return(p);
}

# rmultinom(10, size = 12, prob = c(0.1,0.2,0.8))

# num_pol=length(unique(subset(Premium, Premium[,Factor>0)[,"IDpol"]))


