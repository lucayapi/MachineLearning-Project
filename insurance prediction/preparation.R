library(readr)
library(dplyr)

#importation data 2003
path=getwd() # repository path
filename <- "cost2003.csv"
filename1 <- "freq2003.csv"
filename2 <- "pol2003.csv"
cost2003=read_csv2(file.path(path,"data",filename))
freq2003=read_csv2(file.path(path,"data",filename1))
pol2003=read_csv2(file.path(path,"data",filename2))

filename <- "cost2004.csv"
filename1 <- "freq2004.csv"
filename2 <- "pol2004.csv"
cost2004=read_csv2(file.path(path,"data",filename))
freq2004=read_csv2(file.path(path,"data",filename1))
pol2004=read_csv2(file.path(path,"data",filename2))


#fusion des classes (2003)
datapol= pol2003%>% select(!starts_with("pre"))
datafreq = freq2003 %>% select('IDpol','Freq_total')
datacost = cost2003 %>% select("IDpol","Payment")
data=merge(datapol,datacost,by='IDpol',all.x = TRUE)
data=merge(data,datafreq,by='IDpol',all.x = TRUE)
data=data %>% distinct(IDpol,.keep_all = TRUE)
data =data %>% select(!c('IDpol','DrivGender','Year','MaritalStatus','JobCode','Garage','LicenceNb'))
data$Payment[is.na(data$Payment)]=0
data=data %>% mutate(Freq_total_bin=if_else(Freq_total>=1,1,0))



dataseg2004=pol2004 %>% select(!c('IDpol','DrivGender','Year','MaritalStatus','JobCode','Garage','LicenceNb'))

par(mfrow=c(2,2))
#decoupe DrivAge en 2classes 

ages=sort(unique(data$DrivAge))
Distance=rep(NA,length(ages))
names(Distance)=ages
for(k in 2:(length(ages)-1)){
  classe0 <- data$DrivAge<=ages[k]
  classe1 <- data $DrivAge> ages[k]
  M=matrix(
    rbind(c(sum(data$Freq_total_bin[classe0]==FALSE),
               sum(data$Freq_total_bin[classe0]==TRUE)),
            c(sum(data$Freq_total_bin[classe1]==FALSE),
                sum(data$Freq_total_bin[classe1]==TRUE))),2,2)
            Distance[k] <- (-chisq.test(M)$statistic)}

plot(ages,Distance,type="b",ylab="distance du chi-deux",pch=3,main = 'DrivAge')
data$DrivAge=cut(data$DrivAge,breaks = c(min(data$DrivAge),50,max(data$DrivAge)),include.lowest=T)
dataseg2004$DrivAge=cut(dataseg2004$DrivAge,breaks = c(min(dataseg2004$DrivAge),50,max(dataseg2004$DrivAge)),include.lowest=T)

#decoupe VehAge en 2 classes 
ages=sort(unique(data$VehAge))
Distance=rep(NA,length(ages))
names(Distance)=ages
for(k in 2:(length(ages)-1)){
  classe0 <- data$VehAge<=ages[k]
  classe1 <- data$VehAge> ages[k]
  M=matrix(
    rbind(c(sum(data$Freq_total_bin[classe0]==FALSE),
            sum(data$Freq_total_bin[classe0]==TRUE)),
          c(sum(data$Freq_total_bin[classe1]==FALSE),
            sum(data$Freq_total_bin[classe1]==TRUE))),2,2)
  Distance[k] <- (-chisq.test(M)$statistic)}

plot(ages,Distance,type="b",ylab="distance du chi-deux",pch=3,main='VehAge')
data$VehAge=cut(data$VehAge,breaks =c(min(data$VehAge),11,max(data$VehAge)),include.lowest=T)
dataseg2004$VehAge=cut(dataseg2004$VehAge,breaks = c(min(dataseg2004$VehAge),50,max(dataseg2004$VehAge)),include.lowest=T)

#decoupe BonusMalus en 2 classes 
bm=sort(unique(data$BonusMalus))
Distance=rep(NA,length(bm))
names(Distance)=bm
for(k in 2:(length(ages)-1)){
  classe0 <- data$BonusMalus<=bm[k]
  classe1 <- data$BonusMalus> bm[k]
  M=matrix(
    rbind(c(sum(data$Freq_total_bin[classe0]==FALSE),
            sum(data$Freq_total_bin[classe0]==TRUE)),
          c(sum(data$Freq_total_bin[classe1]==FALSE),
            sum(data$Freq_total_bin[classe1]==TRUE))),2,2)
  Distance[k] <- (-chisq.test(M)$statistic)}

plot(bm,Distance,type="b",ylab="distance du chi-deux",pch=3,main="BonusMalus")
data$BonusMalus=cut(data$BonusMalus,breaks =c(min(data$BonusMalus),60,80,max(data$BonusMalus)),include.lowest=T)
dataseg2004$BonusMalus=cut(dataseg2004$BonusMalus,breaks =unique(c(min(dataseg2004$BonusMalus),50,max(dataseg2004$BonusMalus))),include.lowest=T)


#exportation des données pour la segmentation (2003 et 2004)
dataseg2003=data %>% select(!c('Payment','Freq_total','Freq_total_bin'))
filename <- "dataseg2003.csv"
filename1 <- "dataseg2004.csv"
write_csv2(dataseg2003,file.path(path,"data",filename))
write_csv2(dataseg2004,file.path(path,"data",filename1))

#exportattion des données pour la constrcution des modeles
filename <- "data_mod.csv"
write_csv2(data,file.path(path,"data",filename))

