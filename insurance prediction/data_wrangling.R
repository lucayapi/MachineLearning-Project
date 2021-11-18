library(readr)
library(dplyr)
library(lubridate)
library(forcats)


path=getwd() # chemin du projet

#data loading
filename <- "claims_cost.csv"
filename1 <- "claims_freq.csv"
filename2 <- "claims_pol.csv"

cost=read_csv2(file.path(path,"Rawdata",filename))
freq=read_csv2(file.path(path,"Rawdata",filename1))
pol=read_csv2(file.path(path,"Rawdata",filename2))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         2(file.path(path,"Rawdata",filename2))

#Check data type
sapply(cost,class)
sapply(freq,class)
sapply(pol,class)
#look_for((cost))
#glimpse(cost)

#formatting some variables 
cost=cost %>% mutate(IDpol=as.factor(IDpol),
                OccurDate=dmy(OccurDate),
                IDclaim=as.factor(IDclaim))

freq=freq %>% mutate(across(where(is.character),as.factor),
                     Year=as.factor(Year))

pol=pol %>% mutate(across(where(is.character),as.factor),Year=as.factor(Year))

# extraction data 2003
cost2003=cost %>% filter(year(OccurDate)=="2003")
freq2003=freq %>%
  filter(Year=="2003") %>%
  mutate(Freq_total= rowSums(.[3:8]))
pol2003=pol %>% filter(Year=="2003")

#extraction data 2004
cost2004=cost %>% filter(year(OccurDate)=="2004")
freq2004=freq %>%
  filter(Year=="2004") %>%
  mutate(Freq_total= rowSums(.[3:8]))
pol2004=pol %>% filter(Year=="2004")


#exportation data 2003
filename <- "cost2003.csv"
filename1 <- "freq2003.csv"
filename2 <- "pol2003.csv"
write_csv2(cost2003,file.path(path,"data",filename))
write_csv2(freq2003,file.path(path,"data",filename1))
write_csv2(pol2003,file.path(path,"data",filename2))

#exportation data 2004
filename <- "cost2004.csv"
filename1 <- "freq2004.csv"
filename2 <- "pol2004.csv"
write_csv2(cost2004,file.path(path,"data",filename))
write_csv2(freq2004,file.path(path,"data",filename1))
write_csv2(pol2004,file.path(path,"data",filename2))


