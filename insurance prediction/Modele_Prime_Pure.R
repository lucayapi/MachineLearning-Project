  library(randomForest)
  library(xgboost)
  library(readr)
  library(dplyr)
  library(h2o)
  library(moments)
  library(gridExtra)
  
  path=getwd() #repertoire du projet
  
  #importation data pour la construction des modeles
  filename <- "data_mod_2003.csv"
  filename1="pol2003.csv"
  data1=read_csv2(file.path(path,"data",filename))
  pol2003=read_csv2(file.path(path,"data",filename1))
  data1$classe=as.factor(data1$classe)
  
  
  
  #Construction des modeles pour chqque classe de sinistralité
  
  h2o.init()
  
  #modelisation cout moyen
  resultat_cout=list()
  train_score=rep(0,4)
  valid_score=rep(0,4)
  
  for (i in 1:5){
    if (i==5) {data=data1 %>% filter(classe==i) %>% select(!c('classe','Region'))}
    else{data=data1 %>% filter(classe==i) %>% select(!classe)}
    
    set.seed(123)
    ind=sample(2,nrow(data),replace=T,prob = c(0.8,0.2))
    train=data[ind==1,]
    valid=data[ind==2,]
    
  
    cible_train=train$Payment
    X_train=as.data.frame(model.matrix(Payment~., train))
    train=cbind(X_train[,-1],Payment=cible_train)
    
    cible_valid=valid$Payment
    X_valid=model.matrix(Payment~., valid)
    X_valid=as.data.frame(model.matrix(Payment~., valid))
    valid=cbind(X_valid[,-1],Payment=cible_valid)
    
    variables=colnames(valid)[colnames(valid) %in% colnames(train)]
    
    train=train[,variables]
    valid=valid[,variables]
    
    d_train=xgb.DMatrix(data = as.matrix(train), label = cible_train, missing = NA)
    d_valid=xgb.DMatrix(data = as.matrix(valid), label = cible_valid, missing = NA)
    
    predictors=colnames(train %>% select(!c('Freq_total','Freq_total_bin','Payment')))
    response='Payment'
    
    train=as.h2o(train)
    valid=as.h2o(valid)
    
    #Gradient Boostin Machine (gamma)
    cost_gbm=h2o.gbm(x=predictors,
                     y=response,
                     nfolds=10,
                     seed=1111,
                     distribution = 'gamma',
                     keep_cross_validation_models = TRUE,
                     training_frame = train,
                     validation_frame = valid)
    
    train_score[1]=h2o.rmse(cost_gbm,train =TRUE )
    valid_score[1]=h2o.rmse(cost_gbm,valid =TRUE )
    
    #Gradient Boostin Machine (tweedie)
    cost_gbm=h2o.gbm(x=predictors,
                     y=response,
                     nfolds=10,
                     seed=1111,
                     distribution = 'tweedie',
                     keep_cross_validation_models = TRUE,
                     training_frame = train,
                     validation_frame = valid)
    
    train_score[2]=h2o.rmse(cost_gbm,train =TRUE )
    valid_score[2]=h2o.rmse(cost_gbm,valid=TRUE )
    
    #randomforest
    cost_rf=h2o.randomForest(x = predictors, y = response,
                             training_frame=train,
                             validation_frame = valid ,
                             nfolds =10,
                             seed = 1234)
    
    train_score[3]=h2o.rmse(cost_rf,train =TRUE )
    valid_score[3]=h2o.rmse(cost_rf,valid =TRUE )
     
     
    #Xgb tweedie
    params <- list(
      objective = 'reg:tweedie',
      eval_metric = 'rmse',
      tweedie_variance_power = 1.4,
      max_depth = 6,
      eta = 1)
    
    bst <- xgb.train(
      data = d_train,
      params = params,
      maximize = FALSE,
      watchlist = list(train = d_train),
      nrounds = 2)
    
    train_score[4]=sqrt(sum(mean((cible_train - predict(bst, d_train)) ^ 2)))
    valid_score[4]=sqrt(sum(mean((cible_valid - predict(bst, d_valid)) ^ 2)))
    
    
    resultat_cout[[i]]=rbind(train_score,valid_score)
    row.names(resultat_cout[[i]])=c('RMSE(train)','RMSE(valid)')
    colnames(resultat_cout[[i]])=c('gbm_gamma','gbm_tweedie','randomforest','xgb_tweedie')
  }
  
h2o.init()
  
#modelisation de la frequence
  resultat_freq=list()
  train_score=rep(0,3)
  valid_score=rep(0,3)
  for (i in 1:5){
    if (i==5) {data=data1 %>% filter(classe==i) %>% select(!c('classe','Region'))}
    else{data=data1 %>% filter(classe==i) %>% select(!classe)}
    
    #division en train set et valid set
    set.seed(123)
    ind=sample(2,nrow(data),replace=T,prob = c(0.8,0.2))
    train=data[ind==1,]
    valid=data[ind==2,]
    
    
    cible_train=train$Freq_total
    X_train=as.data.frame(model.matrix(Freq_total~., train))
    train=cbind(X_train[,-1],Freq_total=cible_train)
    
    cible_valid=valid$Freq_total
    X_valid=model.matrix(Payment~., valid)
    X_valid=as.data.frame(model.matrix(Freq_total~., valid))
    valid=cbind(X_valid[,-1],Freq_total=cible_valid)
    
    variables=colnames(valid)[colnames(valid) %in% colnames(train)]
    
    train=train[,variables]
    valid=valid[,variables]
    
    d_train=xgb.DMatrix(data = as.matrix(train), label = cible_train, missing = NA)
    d_valid=xgb.DMatrix(data = as.matrix(valid), label = cible_valid, missing = NA)
    
    predictors=colnames(train %>% select(!c('Freq_total','Freq_total_bin','Payment')))
    response='Freq_total'
    
    train=as.h2o(train)
    valid=as.h2o(valid)
    
    #Gradient Boostin Machine poisson
    cost_gbm=h2o.gbm(x=predictors,
                     y=response,
                     nfolds=10,
                     seed=1111,
                     distribution = 'poisson',
                     keep_cross_validation_models = TRUE,
                     training_frame = train,
                     validation_frame = valid)
    
    train_score[1]=h2o.rmse(cost_gbm,train =TRUE )
    valid_score[1]=h2o.rmse(cost_gbm,valid =TRUE )
    
    
    #randomforest
    cost_rf=h2o.randomForest(x = predictors, y = response,
                             training_frame=train,
                             validation_frame = valid ,
                             nfolds =10,
                             seed = 1234)
    
    train_score[2]=h2o.rmse(cost_rf,train =TRUE )
    valid_score[2]=h2o.rmse(cost_rf,valid =TRUE )
    
    #Xgb poisson
    params <- list(
      objective = 'count:poisson',
      eval_metric = 'rmse',
      tweedie_variance_power = 1.4,
      max_depth = 6,
      eta = 1)
    
    bst <- xgb.train(
      data = d_train,
      params = params,
      maximize = FALSE,
      watchlist = list(train = d_train),
      nrounds = 2)
    
    train_score[3]=sqrt(sum(mean((cible_train - predict(bst, d_train)) ^ 2)))
    valid_score[3]=sqrt(sum(mean((cible_valid - predict(bst, d_valid)) ^ 2)))
    
    resultat_freq[[i]]=rbind(train_score,valid_score)
    row.names(resultat_freq[[i]])=c('RMSE(train)','RMSE(valid)')
    colnames(resultat_freq[[i]])=c('gbm_poisson','randomforest','xgb_poisson')
}
  
      
#resultats des modelisations 
resultat_cout
resultat_freq
  

#estimation de la prime pure 
data2=data1 %>% mutate(frequence=rep(0,nrow(data1)),cout=rep(0,nrow(data1)))

#estimation de la frequence
for (i in 1:5){
  if (i==5) {data=data1 %>% filter(classe==i) %>% select(!c('classe','Region'))}
  else{data=data1 %>% filter(classe==i) %>% select(!classe)}
  
  
  cible_data=data$Freq_total
  X_data=as.data.frame(model.matrix(Freq_total~., data))
  new=cbind(X_data[,-1],Freq_total=cible_data)
  
  variables=colnames(new %>% select(!c('Freq_total','Freq_total_bin','Payment')))
  
  new=new[,variables]
  
  d_data=xgb.DMatrix(data = as.matrix(new), label = cible_data, missing = NA)
  
  #Xgb poisson
  params <- list(
    objective = 'count:poisson',
    eval_metric = 'rmse',
    tweedie_variance_power = 1.4,
    max_depth = 6,
    eta = 1)
  
  bst <- xgb.train(
    data = d_data,
    params = params,
    maximize = FALSE,
    watchlist = list(train = d_data),
    nrounds = 2)
  
  data2[data2$classe==i,'frequence']=predict(bst,d_data)
}
          
#estimation du cout moyen
for (i in 1:5){
  if (i==5) {data=data1 %>% filter(classe==i) %>% select(!c('classe','Region'))}
  else{data=data1 %>% filter(classe==i) %>% select(!classe)}
  
  
  cible_data=data$Payment
  X_data=as.data.frame(model.matrix(Payment~., data))
  new=cbind(X_data[,-1],Payment=cible_data)
  
  variables=colnames(new %>% select(!c('Freq_total','Freq_total_bin','Payment')))
  
  new=new[,variables]
  
  d_data=xgb.DMatrix(data = as.matrix(new), label = cible_data, missing = NA)
  
  #Xgb tweedie
  params <- list(
    objective = 'reg:tweedie',
    eval_metric = 'rmse',
    tweedie_variance_power = 1.4,
    max_depth = 6,
    eta = 1)
  
  bst <- xgb.train(
    data = d_data,
    params = params,
    maximize = FALSE,
    watchlist = list(train = d_data),
    nrounds = 2)
  
  data2[data2$classe==i,'cout']=predict(bst,d_data)
}

data2=data2 %>% mutate(prime_pur=frequence*cout)

#prime pure
data2$prime_pur


#comparaison de la prime pur et prime commerciale

p1=data2 %>% ggplot( aes(x=prime_pur)) + geom_histogram(bins=30) + ggtitle('Histogramme  prime_pure')
a=summary(data2$prime_pur)
tab_stat_prime_pure=cbind(min=a[1],first_quart=a[2],Median=a[3],mean=a[4],third_quart=a[5],max=a[6],std=sd(data2$prime_pur),kurt=kurtosis(data2$prime_pur),skew=skewness(data2$prime_pur),total=sum(data2$prime_pur))
rownames(tab_stat_prime_pure)='prime pure'
tab_stat_prime_pure

tab_stat_prime_commerciale=cbind(min=a[1],first_quart=a[2],Median=a[3],mean=a[4],third_quart=a[5],max=a[6],std=sd(pol2003$PremTot),kurt=kurtosis(pol2003$PremTot),skew=skewness(pol2003$PremTot),total=sum(pol2003$PremTot))
rownames(tab_stat_prime_commerciale)='prime commerciale'
p2=pol2003 %>% ggplot( aes(x=PremTot)) + geom_histogram(bins=30) + ggtitle('Histogramme prime commerciale')
tab_stat_prime_commerciale

grid.arrange(p1,p2,nrow=1,ncol=2)



