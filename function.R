# all functions

####### Calibration & Generation Frequency Severity #######

# general data preparation for calibration and generation
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

# nb of payments by police for each Guarantee
Nb_sinistre=function(Severity){
  Nb_sev=sqldf("select distinct IDpol as IDpol, count(IDpol) as nb_sinistre, 
               Guarantee as Guarantee from Severity group by Idpol,Guarantee")
  Nb_sev
}

# data preparation for frequency calibration (for a certain type of Guarantee)
Prep_Calib_Freq=function(Nb_sinistre, Premium, Factor){
  sub_severity=subset(Nb_sinistre, Guarantee==Factor)[,-3]
  prem=Premium[,startsWith(colnames(Premium), Factor)]
  ind = prem > 0
  prem=Premium[ind,]
  tmp=merge(prem, sub_severity, by="IDpol", all.x=TRUE)
  tmp$nb_sinistre=ifelse(is.na(tmp$nb_sinistre),0,tmp$nb_sinistre)
  tmp
}

# calibration - distribution of payments by Guarantee
calib_Sev=function(Severity,Factor,point_ajust=0,size=30,ysup=0.8){
  x=Severity[which(Severity$Guarantee==Factor),"Payment"]
  x_log=log(x)
  f1 <- fitdist(x_log, "gamma", method="mle")
  f2 <- fitdist(x_log[which(x_log>=point_ajust)], "gamma", method="mle")
  pdf (file=paste(getwd(),"/",Factor,".pdf",sep = ""), width = 6, height = 4, paper = "special") 
  hist(x_log,size,ylim=c(0,ysup),freq=FALSE,main=paste("Fitting of log_severity for guarantee",Factor))
  curve(dgamma(x, shape=f1$estimate[1], rate = f1$estimate[2]), add=TRUE,col = "blue")
  curve(dgamma(x, shape=f2$estimate[1], rate = f2$estimate[2]), add=TRUE,col = "red")
  dev.off()
  return(f2$estimate)
}




