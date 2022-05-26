####### Importation des données #################
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

#train : 2622  obs
#test : 1748   obs

#################################################" Approche construction / Feature selection Regression logistique #############################################################
#fonction retournant taux d'erreur et autres métriques classification ( confusionMatrix( caret))

#library(caret)
#Attentio à l'ordre
#confusionMatrix(factor(y_pred),factor(y_obs),mode="everything")
#Sensitivity : parmis tous les prêts qui sont pas bon, combien le modèle à pu trouver ( modèle arrive à trouver 10 prêt sur 100 pas bon) ( atux vrais positif)
#Specififty: parmis tout ceux qui sont bon combien le modèle arrive à trouver (98%)  ( 98/100) ( taux vrais négaitifs)
#prevalence : proportion de ceux qui sont pas bon en réalité
#F mesure  : moyenne harmonique entre recall et Precision
# Comment choisir, Quelles erreurs le modèle peut-il faire ? Et lesquelles sont interdites ?
#Metrics :  taux d'erreur
taux_erreur=function(y_obs,y_pred){
  mc <- table(y_pred,y_obs)
  #mal classes
  wrong <- sum(mc) - sum(diag(mc))
  #taux d'erreur
  err <- wrong/sum(mc)
  #intervalle de confiance
  #ex. https://webapps.fundp.ac.be/umdb/biostats2017/biostat/modules/module105/page4.html
  #effectif
  n <- sum(mc)
  #?cart-type
  et <- sqrt(err*(1-err)/n)
  #quantile loi normale 95%
  z <- qnorm(0.95)
  #borne basse
  bb <- err - z * et
  bh <- err + z * et
  vec=c(nb_mal_class=wrong,tx_err=err,tx_inf=bb,tx_sup=bh)
  return(vec)
}

#*********************************
#modèle de base avec prédiction classe majoritaire 
#********************************
#train
y_pred=factor(rep(names(table(y))[which.max(y)],na),levels = c("0","1"))
y_obs=y[samp]
erreur_train=taux_erreur(y_obs,y_pred)
#test
y_pred=factor(rep(names(table(y))[which.max(y)],nt),levels = c("0","1"))
y_obs=y[-samp]
erreur_test=taux_erreur(y_obs,y_pred)
rbind(train=erreur_train,test=erreur_test)

#*********************************
#modèle complet avec toutes les variables
#*********************************
rg.glm <- glm(y~.,data=donn,subset=samp,family = binomial)
#train
y_obs=y[samp]
y_pred=ifelse(predict(rg.glm,donn[samp,],type="response")>0.5,1,0)
erreur_train=taux_erreur(y_obs,y_pred)
#test
y_obs=y[-samp]
y_pred=ifelse(predict(rg.glm,donn[-samp,],type="response")>0.5,1,0)
erreur_test=taux_erreur(y_obs,y_pred)
rbind(train=erreur_train,test=erreur_test)


pseudo_R2 <- 1 - rg.glm$deviance/rg.glm$null.deviance
pseudo_R2

mod_null <- update(rg.glm, ~1)

pseudo_R2 =1- logLik(rg.glm)/logLik(mod_null)
pseudo_R2

#*********************************
#selection backward/forward/ Stepwise ( AIC et BIC)
#*********************************

#Selction en lien avec la méthode utilisé + pas trop d'impact sur le taux d'erreur
#Selection par optimisation ( sous-ensemble de variable permetant de maximiser un critère)
#procédure pas à pas qui compare des modèles emboitée
#type de selection avec des modèles avec optimisation vraisemblance ( paramétrique)
#BIC a tendance à retenir moins de variable que le AIC 
#On peut aboutir à des sous-ensembles différents/ des variables pas significatifs avec le test de Wald ou le test de Rapport de vraisemblance
#ces techniques numériques nous proposent des scénarios de solutions. Il ne faut surtout pas prendre pour argent comptant les sous-ensembles de variables expli- catives proposées. D'autant qu'ils peuvent varier d'une stratégie à une autre, et même d'un échantillon 
#d'apprentissage à un autre. Il faut plutôt les considérer comme des alternatives que l'on peut soumettre et faire valider par un expert du domaine. 

#selection forward BIC/AIC
#remplacer k=log(na) pour BIC
library(MASS)
mBack <- stepAIC(rg.glm,k=log(na),direction="forward",trace=FALSE)
mBack$aic
summary(mBack)
#nombre de variables selectionnées
print(length(mBack$coefficients)-1)
#noms des variables retenues
names_mBack=tail(names(mBack$coefficients),length(mBack$coefficients)-1)

# plot(mBack$anova[,'AIC'],type="b",axes=FALSE,ylab="BIC",xlab="",main="Evolution BIC")
# axis(1,at=1:length(mBack$anova[,'Step']),labels=mBack$anova[,'Step'],las=2,cex.axis=0.75)
# axis(2)


#selection backward BIC/AIC
library(MASS)
mBack <- stepAIC(rg.glm,direction="backward",trace=FALSE)
mBack$aic
summary(mBack)
#nombre de variables selectionnées
print(length(mBack$coefficients)-1)
#noms des variables retenues
names_mBack=tail(names(mBack$coefficients),length(mBack$coefficients)-1)



#selection stepwise BIC/AIC
library(MASS)
mBack <- stepAIC(rg.glm,direction="both",trace=FALSE)
mBack$aic
summary(mBack)
#nombre de variables selectionnées
print(length(mBack$coefficients)-1)
#noms des variables retenues
names_mBack=tail(names(mBack$coefficients),length(mBack$coefficients)-1)

# on a un scénario de solution à 2 variables ( type de compte, temp_pay) ==> BIC variable type de compte n'est pas singicatif par rapport au test de Wald
# on a un scénario de solution à 3 variables ( type de compte, temp_pay,age) ==> AIC variable type de compte n'est pas sigicatif par rapport au test de Wald


#evaluation du taxu d'erreur 
#Scénarion avec 5 variables
rg.glm.sc<- glm(y~bank_account_type+temp_pay+age+temp_cloture+termdays,data=donn,subset=samp,family = binomial)
#train
y_obs=y[samp]
y_pred=ifelse(predict(rg.glm.sc,donn[samp,],type="response")>0.5,1,0)
erreur_train=taux_erreur(y_obs,y_pred)
#test
y_obs=y[-samp]
y_pred=ifelse(predict(rg.glm.sc,donn[-samp,],type="response")>0.5,1,0)
erreur_test=taux_erreur(y_obs,y_pred)
rbind(train=erreur_train,test=erreur_test)
pseudo_R2 <- 1 - rg.glm.sc$deviance/rg.glm.sc$null.deviance
pseudo_R2

#Scénarion avec 2 variables
rg.glm.sc<- glm(y~bank_account_type+temp_pay,data=donn,subset=samp,family = binomial)
#train
y_obs=y[samp]
y_pred=ifelse(predict(rg.glm.sc,donn[samp,],type="response")>0.5,1,0)
erreur_train=taux_erreur(y_obs,y_pred)
#test
y_obs=y[-samp]
y_pred=ifelse(predict(rg.glm.sc,donn[-samp,],type="response")>0.5,1,0)
erreur_test=taux_erreur(y_obs,y_pred)
rbind(train=erreur_train,test=erreur_test)

pseudo_R2 <- 1 - rg.glm.sc$deviance/rg.glm.sc$null.deviance
pseudo_R2

# le taux d'erreur ne change pas beaucoup rapport avec les deux scénario et donne à peu près les mêmes valeurs que celle du modèle complet 
#conclusion : avec un modèle avec 2 ou 3 variables on obtient un modèle de même complexité et aux résultatx avec le modèle complet


#*********************************
#selection par les méthodes " Filtre "
#*********************************

# Optimalité au sens du critère de performance
#But : Trouver des variables fortement corrélées avec la variable cible / faible corrélées entre ( pas de redondance d'information)

#*********************************
#selection par ranking
#*********************************

#package FSelector
library(FSelector)
importance <- FSelector::symmetrical.uncertainty(good_bad_flag ~ ., data =train[samp,])
print(importance)

#afficher dans l'ordre
idx <- order(importance$attr_importance,decreasing = TRUE)
sortedImp <- data.frame(var=rownames(importance)[idx],importance=importance$attr_importance[idx])
print(sortedImp)

#repr?sentation graphique
barplot(rev(sortedImp$importance),horiz=TRUE,names.arg = rev(sortedImp$var),cex.names=0.6,las=2)


#r?cup?rons les max(foward,backward) meilleures variables 
selImportance <- FSelector::cutoff.k(importance,k=4)
print(selImportance)


#Evaluation modèles avec ces variables
rg.glm.f<- glm(y~bank_account_type+temp_pay+ interet_es + bank_name_clients ,data=donn,subset=samp,family = binomial)
#train
y_obs=y[samp]
y_pred=ifelse(predict(rg.glm.f,donn[samp,],type="response")>0.5,1,0)
erreur_train=taux_erreur(y_obs,y_pred)
#test
y_obs=y[-samp]
y_pred=ifelse(predict(rg.glm.f,donn[-samp,],type="response")>0.5,1,0)
erreur_test=taux_erreur(y_obs,y_pred)
rbind(train=erreur_train,test=erreur_test)

pseudo_R2 <- 1 - rg.glm.f$deviance/rg.glm.f$null.deviance
pseudo_R2


#*********************************
#selection CFS
#*********************************

selCFS <- FSelector::cfs(good_bad_flag ~ ., data =train[samp,])

#nombre de variables retenus
#variables retenus
print(selCFS)

#Evaluation modèles avec ces variables
rg.glm.f<- glm(y~temp_pay,data=donn,subset=samp,family = binomial)
#train
y_obs=y[samp]
y_pred=ifelse(predict(rg.glm.f,donn[samp,],type="response")>0.5,1,0)
erreur_train=taux_erreur(y_obs,y_pred)
#test
y_obs=y[-samp]
y_pred=ifelse(predict(rg.glm.f,donn[-samp,],type="response")>0.5,1,0)
erreur_test=taux_erreur(y_obs,y_pred)
rbind(train=erreur_train,test=erreur_test)

pseudo_R2 <- 1 - rg.glm.f$deviance/rg.glm.f$null.deviance
pseudo_R2


#Conclusion :  Les méthodes de Filtrages nous frounissent des variables proche de celle trouvées avec les méthodes AIC et BIC
#La variable temp_pay semble joué un rôle très important dans la modélisation
#le tau d'erreur n'evolue pas significativement



#*************************************************************
# Appproche pour prendre en compte le déséquilibre des classes 
#****************************************************************************

#*********************************
#Régression pondérée 
#*********************************

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
min(erreurtrain)
grille[which.min(erreurtest)] #w1=0.34



weigths=ifelse(y[samp]==1,0.34,0.66)
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

#*********************************
#Rééchantillonage
#*********************************
library(unbalanced)
library(UBL)
# ls("package:unbalanced")
# ls("package:UBL")


#Separation en train et test set 
set.seed(1)
donn=train_new[,-2] # données sans variable à expliquer
y=train$good_bad_flag #variable à expliquée
n=dim(donn)[1]
samp <- c(sample(1:n,ceiling(0.6*n)))
train=train[samp,]
test=train[-samp,]
na=length(samp)
nt=n-na

#rééchantillonage base d'apprentissage
#Algorithme SMOTE  :  Synthetic  Minority Over-sampling  TEchnique (plus utilisée)
X=as.data.frame(model.matrix(good_bad_flag ~bank_account_type+temp_pay,data=train))
X=cbind.data.frame(X[,-1],good_bad_flag=train$good_bad_flag)
train_new=SmoteClassif(good_bad_flag ~.,dat=X,k=4) #rééchantillonnage

#verification rééchantillonage
table(train_new$good_bad_flag)

rg.glm <- glm(good_bad_flag~bank_account_type+temp_pay,data=train_new,family = binomial)

#performances en test
#train
y_obs=train_new$good_bad_flag
y_pred=ifelse(predict(rg.glm,train_new,type="response")>0.5,1,0)
erreur_train=taux_erreur(y_obs,y_pred)
#test
test_new=as.data.frame(model.matrix(good_bad_flag ~.,data=test))
y_obs=test$good_bad_flag
y_pred=ifelse(predict(rg.glm,test_new[,-1],type="response")>0.5,1,0)
erreur_test=taux_erreur(y_obs,y_pred)
rbind(train=erreur_train,test=erreur_test)
pseudo_R2

#Conclusion : la méthode rééchantillonnage n'a pas apporté de compténces significatices aux taux d'erreur 

# Modèle retenu :  good_bad_flag + temps_pay + account_type

# go diagnostique du modèle


