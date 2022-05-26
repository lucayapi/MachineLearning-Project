#install.packages("ggfortify")
library(ggfortify)
library(MASS)
path=getwd() # repository path
filename <- "base.csv"
filename1="test.csv"

train=read.csv(file.path(path,"data",filename))
test=read.csv(file.path(path,"data",filename1))

#retrait variable custormerid
train=train[,-1]
train$good_bad_flag=as.factor(train$good_bad_flag)
train$termdays=as.factor(train$termdays)
#Separation en train et test set 
set.seed(1)
donn=train[,-2] # données sans variable à expliquer
y=train$good_bad_flag #variable à expliquée
n=dim(donn)[1]
samp <- c(sample(1:n,ceiling(0.6*n)))
na=length(samp)
nt=n-na

#**********************************
#*Diagnostique du modèle
#********************************
weigths=ifelse(y[samp]==1,0.34,0.66)
rg.glm.w <- glm(y[samp]~bank_account_type+temp_pay,data=donn[samp,],family=binomial,weights = weigths)
autoplot(rg.glm.w,which = 1:6)

#Zoom sur les points influents
inf_point=c(1940,4237,4275)
train[inf_point,]

#Modèles sans ces points
train=read.csv(file.path(path,"data",filename))
train=train[-inf_point,-1]
train$good_bad_flag=as.factor(train$good_bad_flag)
train$termdays=as.factor(train$termdays)
#Separation en train et test set 
set.seed(1)
donn=train[,-2] # données sans variable à expliquer
y=train$good_bad_flag 


#Ajustement des poids 
grille=seq(0.0,0.99,0.01)
erreurtrain=rep(0,length(grille))
erreurtest=rep(0,length(grille))
i=1
for (w in grille){
  #modèle complet
  weigths=ifelse(y[samp]==1,w,1-w)
  rg.glm.w <- glm(y[samp]~bank_account_type+temp_pay,data=donn[samp,],family=binomial,weights = weigths)
  summary(rg.glm.w)
  #train
  y_obs=y[samp]
  y_pred=ifelse(predict(rg.glm.w ,donn[samp,],type="response")>0.5,1,0)
  erreur_train=taux_erreur(y_obs,y_pred)
  erreurtrain[i]=erreur_train[2]
  #test
  y_obs=y[-samp]
  y_pred=ifelse(predict(rg.glm.w ,donn[-samp,],type="response")>0.5,1,0)
  erreur_test=taux_erreur(y_obs,y_pred)
  erreurtest[i]=erreur_test[2]
  i=i+1
}

plot(grille,erreurtrain,col="red",type="l",main="poids classe majoritaire vs erreur")
lines(grille,erreurtest,col="blue")
legend(0.6,0.7, legend=c("erreur train", "erreur test"),
       col=c("red", "blue"), lty=c(1,1), cex=0.8,
       title="Line types", text.font=4, bg='lightblue')
min(erreurtest)
grille[which.min(erreurtest)] #w1=0.32



weigths=ifelse(y[samp]==1,0.32,0.68)
rg.glm.w <- glm(y[samp]~bank_account_type+temp_pay,data=donn[samp,],family=binomial,weights = weigths)


#train
y_obs=y[samp]
y_pred=ifelse(predict(rg.glm.w,donn[samp,],type="response")>0.5,1,0)
erreur_train=taux_erreur(y_obs,y_pred)
#test
y_obs=y[-samp]
y_pred=ifelse(predict(rg.glm.w,donn[-samp,],type="response")>0.5,1,0)
erreur_test=taux_erreur(y_obs,y_pred)
rbind(train=erreur_train,test=erreur_test)

pseudo_R2 <- 1 - rg.glm.w$deviance/rg.glm.w$null.deviance
pseudo_R2

