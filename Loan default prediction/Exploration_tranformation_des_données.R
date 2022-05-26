#chargement des libraries
library(readr)
library(dplyr)

library(visdat)
library(ggplot2)
library(funModeling)
library(ggrepel)
library(corrplot)
library(moments)
library(gridExtra)



### Importation des données #####
path=getwd() # repository path
filename <- "traindemographics.csv"
filename1 <- "testdemographics.csv"
filename2 <- "trainperf.csv"
filename3 <- "testperf.csv"
filename4 <- "trainprevloans.csv"
filename5 <- "testprevloans.csv"
filename6 <- "SampleSubmission.csv"

traindemographics=read.csv(file.path(path,"data",filename),na.strings="")
testdemographics=read.csv(file.path(path,"data",filename1),na.strings="")
trainperf=read.csv(file.path(path,"data",filename2),na.strings="")
testperf=read.csv(file.path(path,"data",filename3),na.strings="")
trainprevloans=read.csv(file.path(path,"data",filename4),na.strings="")
testprevloans=read.csv(file.path(path,"data",filename5),na.strings="")
SampleSubmission=read.csv(file.path(path,"data",filename6),na.strings="")

#Shape des données
paste("traindemographics",paste(c("lignes","colonnes"), c(nrow(traindemographics),ncol(traindemographics)),collapse =" "),sep=": ")
paste("testdemographics",paste(c("lignes","colonnes"), c(nrow(testdemographics),ncol(testdemographics)),collapse =" "),sep=": ")
paste("trainperf",paste(c("lignes","colonnes"), c(nrow(trainperf),ncol(trainperf)),collapse =" "),sep=": ")
paste("testperf",paste(c("lignes","colonnes"), c(nrow(testperf),ncol(testperf)),collapse =" "),sep=": ")
paste("trainprevloans",paste(c("lignes","colonnes"), c(nrow(trainprevloans),ncol(trainprevloans)),collapse =" "),sep=": ")
paste("testprevloans",paste(c("lignes","colonnes"), c(nrow(testprevloans),ncol(testprevloans)),collapse =" "),sep=": ")
paste("SampleSubmission",paste(c("lignes","colonnes"), c(nrow(SampleSubmission),ncol(SampleSubmission)),collapse =" "),sep=": ")


###### fusion des bases demographics et prevloans ######
demographics=rbind(traindemographics,testdemographics)
prevloans=rbind(trainprevloans,testprevloans)

####### conversion des variables dans le bon type ########
data_format = function(data,var_quali=NULL,var_quanti=NULL){
  names_col=colnames(data)
  if (!is.null(var_quali)){
    ind_quali=which(names_col %in% var_quali)
    print(ind_quali)
    for (i in ind_quali) {data[,i]=as.factor(data[,i])}}
  
  if (!is.null(var_quanti)){
    ind_quanti=which(names_col %in% var_quanti)
    for (i in ind_quanti){data[,i]=as.numeric(data[,i])}}
  
  return(data)
}

#demographics
var=colnames(demographics)[sapply(demographics,class)=="character"]
demographics=data_format(demographics,var_quali=var)

#prevloans
var=colnames(prevloans)[sapply(prevloans,class)=="character"]
prevloans=data_format(prevloans,var_quali=var)

#Fonction pour graphique de l'exploration
#Grpahiques pour variable qualitative
plot_quali = function(data,var){
  data%>% count(.data[[var]]) %>% 
    mutate(Pct = round(n / nrow(data),5)) %>% 
    ggplot(aes_string(x=var, y="Pct",fill=var)) +
    geom_bar(stat="identity")+
    geom_label_repel(aes(label = paste("%",Pct*100)), color = 'white',size=6)+
    theme_minimal()+
    labs(y = "Frequence",
         title = paste("Diagramme en barre et Fréquence"),
         subtitle = paste0("Variable : ",var))+
    theme(
      legend.position="none",
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15))
  
}


########### Analyse de la base Demographics ##############

#Analyse des valeurs manquantes
vis_miss(demographics)

##### variable bank_account_type
paste("nombre de modalitées :",nlevels(demographics$bank_account_type))
tab=cbind(eff=table(demographics$bank_account_type),freq=100*prop.table(table(demographics$bank_account_type)))
tab  

#Diagramme en barre
plot_quali(demographics,var="bank_account_type")

#### Variable bank_name_clients
paste("nombre de modalitées :",nlevels(demographics$bank_name_clients))
tab=cbind(eff=table(demographics$bank_name_clients),freq=100*prop.table(table(demographics$bank_name_clients)))
tab  

#Diagramme en barre
plot_quali(demographics,var="bank_name_clients")

#Variable bank_branch_clients
paste("nombre de modalitées :",nlevels(demographics$bank_branch_clients))
tab=cbind(eff=table(demographics$bank_branch_clients),freq=100*prop.table(table(demographics$bank_branch_clients)))
tab  

#Diagramme en barre (illisible)
plot_quali(demographics,var="bank_branch_clients")


#### Variable employment_status_clients
paste("nombre de modalitées :",nlevels(demographics$employment_status_clients))
tab=cbind(eff=table(demographics$employment_status_clients),freq=100*prop.table(table(demographics$employment_status_clients)))
tab  

#Diagramme en barre (illisible)
plot_quali(demographics,var="employment_status_clients")

#### Variable level_of_education_clients

paste("nombre de modalitées :",nlevels(demographics$level_of_education_clients
))
tab=cbind(eff=table(demographics$level_of_education_clients
),freq=100*prop.table(table(demographics$level_of_education_clients
)))
tab  

#Diagramme en barre (illisible)
plot_quali(demographics,var="level_of_education_clients")


# Transformation variable Birthdate in age
today=as.Date("2022-01-01")
age=as.numeric(difftime(today,demographics$birthdate, units = "days"))/365
demographics=cbind(demographics,age)

#resumés statistique
a=summary(demographics$age)
tab_stat=cbind(min=a[1],first_quart=a[2],
               Median=a[3],mean=a[4],
               third_quart=a[5],max=a[6],
               std=sd(demographics$age),
               kurt=kurtosis(demographics$age),
               skew=skewness(demographics$age))
rownames(tab_stat)='age'
tab_stat

#graphique
var="age"
grid.arrange(
  demographics %>% ggplot(aes_string(x = var)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "#E69F00") +
    geom_density(lwd = 1, colour = 4,
                 fill = 4, alpha = 0.25)+
    labs(
      title = paste("Histogramme & Densité"),
      subtitle = paste0("Variable : ",var))+
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),
  demographics %>% ggplot(aes_string(y = var)) +
    geom_boxplot(fill = "#E69F00") +
    labs(
      title = paste("Boîte à moustache"),
      subtitle = paste0("Variable : ",var))+
    theme(
      legend.position="none",
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),nrow=1,ncol=2
)

#Suppression des variables avec +80% de valeurs manquantes + données gps + birthdate
var_supp=c("bank_branch_clients","level_of_education_clients","longitude_gps","latitude_gps","birthdate")
ind_supp=which(colnames(demographics) %in% var_supp)
demographics_new=demographics[,-ind_supp]

# Regrouper la variable Bank_name client en deux modalités
demographics_new$bank_name_clients=ifelse(demographics_new$bank_name_clients!="GT Bank","autres","GT Bank")

#Imputer valeurs manquantes par le mode ( employment_status_clients)
mode=names(sort(table(demographics_new$employment_status_clients),decreasing = TRUE))[1]
demographics_new$employment_status_clients[is.na(demographics_new$employment_status_clients)]=mode

#vérification
vis_miss(demographics_new)

names(sort(table(demographics_new$level_of_education_clients),decreasing = TRUE))[1]
######### Analyse de la base Prevloan ##################
#valeurs manquantes
vis_miss(prevloans)

# Création de nouvelles variables
# 1.  temp_cloture=closeddate-firtrepaiddate ( le temps qu'il a pris pour le prêt)
temps_cloture=as.numeric(difftime(prevloans$closeddate,prevloans$firstrepaiddate, units = "days"))
prevloans=cbind(prevloans,temps_cloture)
temp_cloture=aggregate(temps_cloture ~ customerid,data=prevloans,mean)

# 2. temps_premier_pay='firstduedate'-'firstrepaiddate - ( temps pour le premier paiement)
temps_first_pay=as.numeric(difftime(prevloans$firstduedate,prevloans$firstrepaiddate, units = "days"))
prevloans=cbind(prevloans,temps_first_pay)
temp_pay=aggregate(temps_first_pay ~ customerid,data=prevloans,mean)

# #3. interet=totaldue-Loanmount ( interet du pret)
interet=prevloans$totaldue-prevloans$loanamount
prevloans=cbind(prevloans,interet)
interet=aggregate(interet ~ customerid,data=prevloans,mean)

prevloans_new = cbind(interet,temp_cloture=temp_cloture[,2],temp_pay=temp_pay[,2])

##### Variable interet

#resumés statistique
a=summary(prevloans_new$interet)
tab_stat=cbind(min=a[1],first_quart=a[2],
               Median=a[3],mean=a[4],
               third_quart=a[5],max=a[6],
               std=sd(prevloans_new$interet),
               kurt=kurtosis(prevloans_new$interet),
               skew=skewness(prevloans_new$interet))
rownames(tab_stat)='interet'
tab_stat

#graphique
var="interet"
grid.arrange(
  prevloans_new %>% ggplot(aes_string(x = var)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "#E69F00") +
    geom_density(lwd = 1, colour = 4,
                 fill = 4, alpha = 0.25)+
    labs(
      title = paste("Histogramme & Densité"),
      subtitle = paste0("Variable : ",var))+
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),
  prevloans_new %>% ggplot(aes_string(y = var)) +
    geom_boxplot(fill = "#E69F00") +
    labs(
      title = paste("Boîte à moustache"),
      subtitle = paste0("Variable : ",var))+
    theme(
      legend.position="none",
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),nrow=1,ncol=2
)

##### Variable temp_pay

#resumés statistique
a=summary(prevloans_new$temp_pay)
tab_stat=cbind(min=a[1],first_quart=a[2],
               Median=a[3],mean=a[4],
               third_quart=a[5],max=a[6],
               std=sd(prevloans_new$temp_pay),
               kurt=kurtosis(prevloans_new$temp_pay),
               skew=skewness(prevloans_new$temp_pay))
rownames(tab_stat)='temp_pay'
tab_stat

#graphique
var="temp_pay"
grid.arrange(
  prevloans_new %>% ggplot(aes_string(x = var)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "#E69F00") +
    geom_density(lwd = 1, colour = 4,
                 fill = 4, alpha = 0.25)+
    labs(
      title = paste("Histogramme & Densité"),
      subtitle = paste0("Variable : ",var))+
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),
  prevloans_new %>% ggplot(aes_string(y = var)) +
    geom_boxplot(fill = "#E69F00") +
    labs(
      title = paste("Boîte à moustache"),
      subtitle = paste0("Variable : ",var))+
    theme(
      legend.position="none",
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),nrow=1,ncol=2
)


##### Variable temp_cloture

#resumés statistique
a=summary(prevloans_new$temp_cloture)
tab_stat=cbind(min=a[1],first_quart=a[2],
               Median=a[3],mean=a[4],
               third_quart=a[5],max=a[6],
               std=sd(prevloans_new$temp_cloture),
               kurt=kurtosis(prevloans_new$temp_cloture),
               skew=skewness(prevloans_new$temp_cloture))
rownames(tab_stat)='temp_cloture'
tab_stat

#graphique
var="temp_cloture"
grid.arrange(
  prevloans_new %>% ggplot(aes_string(x = var)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "#E69F00") +
    geom_density(lwd = 1, colour = 4,
                 fill = 4, alpha = 0.25)+
    labs(
      title = paste("Histogramme & Densité"),
      subtitle = paste0("Variable : ",var))+
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),
  prevloans_new %>% ggplot(aes_string(y = var)) +
    geom_boxplot(fill = "#E69F00") +
    labs(
      title = paste("Boîte à moustache"),
      subtitle = paste0("Variable : ",var))+
    theme(
      legend.position="none",
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),nrow=1,ncol=2
)

######## Analyse Data pref ############

#valeur manquante 
vis_miss(trainperf)
vis_miss(testperf)

# Etude de la variable cible (train) : good_bad_flag
paste("nombre de modalitées :",nlevels(trainperf$good_bad_flag))
tab=cbind(eff=table(trainperf$good_bad_flag),freq=100*prop.table(table(trainperf$good_bad_flag)))
tab  

#Diagramme en barre
plot_quali(trainperf,var="good_bad_flag")


# Création de nouvelle variable et recodage variable cible

#interet_es=totaldue-Loanmount (espérée)
#train
interet_es=trainperf$totaldue-trainperf$loanamount
trainperf=cbind(trainperf,interet_es)


col_supp=c("systemloanid",'approveddate','loannumber','creationdate', 'loanamount', 'totaldue',
           'referredby')
ind_col_sup=which(colnames(trainperf) %in% col_supp)
trainperf_new=trainperf[,-ind_col_sup]

#recode variable cible
trainperf_new$good_bad_flag=as.factor(ifelse(trainperf_new$good_bad_flag=="Good",1,0))

#test
interet_es=testperf$totaldue-testperf$loanamount
testperf=cbind(testperf,interet_es)
col_supp=c("systemloanid",'approveddate','loannumber','creationdate', 'loanamount', 'totaldue',
           'referredby')
ind_col_sup=which(colnames(testperf) %in% col_supp)
testperf_new=testperf[,-ind_col_sup]


#graphique
var="interet_es"
grid.arrange(
  trainperf_new %>% ggplot(aes_string(x = var)) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "#E69F00",bins=30) +
    geom_density(lwd = 1, colour = 4,
                 fill = 4, alpha = 0.25)+
    labs(
      title = paste("Histogramme & Densité"),
      subtitle = paste0("Variable : ",var))+
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),
  trainperf_new %>% ggplot(aes_string(y = var)) +
    geom_boxplot(fill = "#E69F00") +
    labs(
      title = paste("Boîte à moustache"),
      subtitle = paste0("Variable : ",var))+
    theme(
      legend.position="none",
      plot.title = element_text(color = "black", size = 18, face = "bold"),
      plot.subtitle = element_text(color = "red",size=15)),nrow=1,ncol=2
)




############# Fusion des bases demographics,prevloans et trainperf ##################

#train
data=merge(trainperf_new,demographics_new,all.x = TRUE)
data=merge(data,prevloans_new,all.x = TRUE)
data=na.omit(data)

#test
test=merge(testperf_new,demographics_new,all.x = TRUE)
test=merge(test,prevloans_new,all.x = TRUE)
test=na.omit(test)
############### Analyse de base fusionnée ( data) ######################

#matrice de corrélation
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

col_quanti=c('interet_es','age','temp_cloture', 'temp_pay', 'interet')
data_quanti=data[,col_quanti]
#matrice de corrélation
M=cor(data_quanti)
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(data_quanti)
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         
)

## Etude de la liaison entre bank_account_type vs good_bad_flag

#tableau de conteingence
tab_cont=table(data$bank_account_type,data$good_bad_flag)
#graphique
ggplot(data, aes(x = bank_account_type, fill = factor(good_bad_flag))) +
  geom_bar(stat='count', position='dodge') 

#test de chi2
chisq.test(tab_cont)

## bank_name_clients vs good_bad_flag

#tableau de conteingence
tab_cont=table(data$bank_name_clients,data$good_bad_flag)
#graphique
ggplot(data, aes(x = bank_name_clients, fill = factor(good_bad_flag))) +
  geom_bar(stat='count', position='dodge') 

#test de chi2
chisq.test(tab_cont)


## termsday vs good_bad_flag

#tableau de conteingence
tab_cont=table(data$termdays,data$good_bad_flag)
#graphique
ggplot(data, aes(x =termdays, fill = factor(good_bad_flag))) +
  geom_bar(stat='count', position='dodge') 

#test de chi2
chisq.test(tab_cont)


# Liaison entre interetes et good_bad_flag

#graphique
ggplot(data, aes(x =good_bad_flag, y =interet_es))+
  geom_boxplot() +
  geom_hline(aes(yintercept=180921.2), 
             colour='red', linetype='dashed', lwd=2) 

# #test anova
# summary(aov(interet_es~good_bad_flag,data=data))




#exportation des données pour les modèles
filename="train.csv"
filename1="test.csv"
write.csv(data,file.path(path,"data",filename),row.names = FALSE)
write.csv(test,file.path(path,"data",filename1),row.names = FALSE)



