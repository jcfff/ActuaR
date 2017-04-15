#rm(list=ls())
library(CASdatasets)
library(sqldf)
library(tcltk)
library(fitdistrplus)
library(gdata)

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

### calibration ###
Prep_Calib=function(premium){
  prem_agg=aggregate(premium[,c("PremTot","PremWindscreen","PremDamAll","PremFire","PremAcc1","PremAcc2",
                                "PremLegal","PremTPLM","PremTPLV","PremServ","PremTheft")], 
                     by=list(IDpol=premium$IDpol), FUN=sum)
  prem_agg$PremDamage=prem_agg$PremDamAll+prem_agg$PremAcc1+prem_agg$PremAcc2
  prem_agg$PremTPL=prem_agg$PremTPLM+prem_agg$PremTPLV
  prem_agg$PremOther=prem_agg$PremLegal+prem_agg$PremServ
  tmp=cbind(prem_agg[,c("IDpol","PremTot","PremWindscreen","PremDamage","PremFire",
                            "PremTPL","PremTheft","PremOther")], 
             aggregate(prem_agg[,c("PremWindscreen","PremDamage","PremFire",
                                   "PremTPL","PremTheft","PremOther")], 
                       by=list(Category=prem_agg$IDpol), FUN=sign)[,-1])
  colnames(tmp)[9:14]=c("Windscreen","Damage","Fire","TPL","Theft","Other")
  tmp
}

Nb_sinistre=function(Severity){
  Nb_sev=sqldf("select distinct IDpol as IDpol, count(IDpol) as nb_sinistre, 
               Guarantee as Guarantee from Severity group by Idpol,Guarantee")
  Nb_sev
}

# for a certain guarantee
Prep_Calib_Freq=function(Nb_sinistre, Premium, Factor){
  sub_severity=subset(Nb_sinistre, Guarantee==Factor)[,-3]
  prem=Premium[,startsWith(colnames(Premium), Factor)]
  ind = prem > 0
  prem=Premium[ind,]
  tmp=merge(prem, sub_severity, by="IDpol", all.x=TRUE)
  tmp$nb_sinistre=ifelse(is.na(tmp$nb_sinistre),0,tmp$nb_sinistre)
  tmp
}

# Calibration Frequency: By Guarantee
Nb_Guarantee=Nb_sinistre(sev03)
Freq_Windscreen=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Windscreen")
Freq_Damage=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Damage")
Freq_Fire=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Fire")
Freq_TPL=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "TPL")
Freq_Theft=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Theft")
Freq_Other=Prep_Calib_Freq(Nb_Guarantee, Prep_Calib(prem03), "Other")

# Calibration Severity: By Guarantee
Est_Windscreen=calib_Sev(sev03,"Windscreen",point_ajust=4.5)
Est_Damage=calib_Sev(sev03,"Damage",point_ajust=5)
Est_Fire=calib_Sev(sev03,"Fire",point_ajust=6,size=10)
Est_TPL=calib_Sev(sev03,"TPL",point_ajust=5)
Est_Theft=calib_Sev(sev03,"Theft",point_ajust=5)
Est_Other=calib_Sev(sev03,"Other",point_ajust=5,ysup=1)

# function for calibration of the distribution of payments by guarantee
calib_Sev=function(Severity,Factor,point_ajust=0,size=30,ysup=0.8){
  x=Severity[which(Severity$Guarantee==Factor),"Payment"]
  x_log=log(x)
  f1 <- fitdist(x_log, "gamma", method="mle")
  f2 <- fitdist(x_log[which(x_log>=point_ajust)], "gamma", method="mle")
  pdf (file=paste("D:/ENSAE2016-2017/projet_R_images/",Factor,".pdf"), width = 6, height = 4, paper = "special") 
  hist(x_log,size,ylim=c(0,ysup),freq=FALSE,main=paste("Fitting of log_severity for guarantee",Factor))
  curve(dgamma(x, shape=f1$estimate[1], rate = f1$estimate[2]), add=TRUE,col = "blue")
  curve(dgamma(x, shape=f2$estimate[1], rate = f2$estimate[2]), add=TRUE,col = "red")
  dev.off()
  return(f2$estimate)
}



### genarator ###
Generation_sev_freq=function(nb_scenario, param_freq, param_sev, ){
  rgamma(n, shape, rate = 1, scale = 1/rate)
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


