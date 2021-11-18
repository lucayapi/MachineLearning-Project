library(readr)
library(dplyr)
library(visdat)
library(funModeling)
library(corrplot)
library(moments)
library(gridExtra)
#importation data 2003
path=getwd() # repository path
filename <- "cost2003.csv"
filename1 <- "freq2003.csv"
filename2 <- "pol2003.csv"
cost2003=read_csv2(file.path(path,"data",filename))
freq2003=read_csv2(file.path(path,"data",filename1))
pol2003=read_csv2(file.path(path,"data",filename2))
pol2004=read_csv2(file.path(path,"data",'pol2004.csv'))

#number of contract in 2003 
paste("nombre de contrat dans le portefeuille 2003 :", dim(pol2003)[1])
paste("nombre de contrat dans le portefeuille 2004 :", dim(pol2004)[1])

#missing values
vis_miss(cost2003)
vis_miss(freq2003)
vis_miss(pol2003,warn_large_data = FALSE)

Pol_na=df_status(pol2003)[,c(1,5)]
Pol_na


#analyse cout moyen
a=summary(cost2003$Payment)
tab_stat=cbind(min=a[1],first_quart=a[2],Median=a[3],mean=a[4],third_quart=a[5],max=a[6],std=sd(cost2003$Payment),kurt=kurtosis(cost2003$Payment),skew=skewness(cost2003$Payment),total=sum(cost2003$Payment))
rownames(tab_stat)='cost'


#data visualization
p1=cost2003 %>% ggplot( aes(x=Payment)) +geom_boxplot() + ggtitle('Boite à moustache')
p2=cost2003 %>% ggplot( aes(x=Payment)) + geom_histogram(bins=60) + ggtitle('Histogramme')
grid.arrange(p1,p2)
#max eloigné des autres ce qui explique boxplot aplati et histogramme difficilement interpretable
#75% des couts de sinistres sont inferieurs a 897 et 50% a 412
#la moyenne des couts est 1212.38
# sans surprise la distribution est asymetrique a gauche ( skewness>1)


#zoom sur histogramme
df=cost2003 %>% filter(Payment<summary(cost2003$Payment)[5])

p1=df %>% ggplot( aes(x=Payment)) +geom_boxplot() + ggtitle('Boite à moustache < 3ieme quartile') 
p2=df %>% ggplot( aes(x=Payment)) + geom_histogram(bins=60) + ggtitle('Histogramme < 3ieme quartile')
grid.arrange(p1,p2)



#analyse des frequences
tab_freq=freq2003 %>% count(Freq_total)%>% mutate(prct=(n/nrow(freq2003))*100) 
freq2003 %>% ggplot(aes(x=Freq_total)) +geom_bar() #titre : repartition  de la frequence des sinistres


#Analyse par garantie (2003)
#cout moyen
cost2003 %>% group_by(Guarantee) %>% summarise(moy=mean(Payment),med=median(Payment),max=max(Payment),min=min(Payment),var=var(Payment),,kurt=kurtosis(Payment),skew=skewness(Payment),percent=sum(Payment)*100/tab_stat[10])    
r=cost2003 %>% group_by(Guarantee) %>% summarise(cost=sum(Payment))

bp<- ggplot(r, aes(x="", y=cost, fill=Guarantee))+geom_bar(width = 1, stat = "identity")
bp

pie=bp+coord_polar("y", start=0)
pie

#distribution des couts par garantie
df=cost2003 %>% filter(Payment<summary(cost2003$Payment)[5])
p1=df %>% filter(Guarantee=='Damage') %>% ggplot( aes(x=Payment)) + geom_histogram(bins=60) + ggtitle('Histogramme Damage')
p2=df %>% filter(Guarantee=='Fire') %>% ggplot( aes(x=Payment)) + geom_histogram(bins=60) + ggtitle('Histogramme Fire')
p3=df %>% filter(Guarantee=='Other') %>% ggplot( aes(x=Payment)) + geom_histogram(bins=60) + ggtitle('Histogramme Other')
p4=df %>% filter(Guarantee=='Theft') %>% ggplot( aes(x=Payment)) + geom_histogram(bins=60) + ggtitle('Histogramme Theft')
p5=df %>% filter(Guarantee=='TPL') %>% ggplot( aes(x=Payment)) + geom_histogram(bins=60) + ggtitle('Histogramme TPL')
p6=df %>% filter(Guarantee=='Windscreen') %>% ggplot( aes(x=Payment)) + geom_histogram(bins=60) + ggtitle('Histogramme Windscreen')

grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3,ncol=2)

#frequence par garantie
tab_freq_gar=freq2003 %>% select(!c('IDpol','Year','Freq_total')) %>% apply(2,sum)
data=data.frame(name=names(tab_freq_gar),value=tab_freq_gar)
data %>% ggplot(aes(x=name,y=value)) +geom_bar(stat='identity') # titre : frequence par garantie
#la garantie TPL ( responsabilite civile) plus activé

eff=apply(freq2003[,3:8],2,sum)
prop=(apply(freq2003[,3:8],2,sum)/sum(freq2003$Freq_total))*100 
freq_garantie=data.frame(var=names(eff),eff,prop)
freq_garantie

###interaction (test de chi2)
new=freq2003 %>% select(IDpol,Freq_total)
data=merge(pol2003,new,all.x = T)
data=data %>% mutate(Freq_total=if_else(Freq_total>=1,1,0)) 
data=data %>% select(-IDpol,-Year)

for(i in c(1,18:28)){
  data[,i]=cut(data[,i],breaks = unique(quantile(data[,i])),include.lowest=T)}

var=colnames(data)
p_value=rep(0,ncol(data))
result=rep(0,ncol(data))

for( i in 1:ncol(data)){
  a=chisq.test(data[,i],data$Freq_total)
  p_value[i]=a$p.value
  if (a$p.value <0.05) 
  {result[i]="Accept"}
  else{result[i]="Refuse"}
  result[i]
}
dataresult=cbind(var,p_value,result)
dataresult
