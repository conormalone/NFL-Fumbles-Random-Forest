library(tidyverse)
library(reticulate)
library(deldir)
library(sp)
library(raster)
library(lubridate)

IPWorking<-read.csv("SackFumbleQuery.csv")


playupornot<-read.csv("ballqblocation.csv")
playupornot$gameId <-as.factor(playupornot$gameId)
playupornot$playId <-as.factor(playupornot$playId)
IPWorking$gameId <-as.factor(IPWorking$gameId)
IPWorking$playId <-as.factor(IPWorking$playId)
IPWorking<-IPWorking %>%
  left_join(playupornot, by = c("gameId","playId"))

IPWorking<-IPWorking %>% dplyr::select(-c("football","qb"))
IPWorking$ToLeft<- ifelse(IPWorking$Posession == 1, IPWorking$playupornot, abs(IPWorking$playupornot - 1))

cleaningfunction <- function(dataframe3){
  dataframe3$WeatherStd<- ifelse(dataframe3$Weather == 'Controlled Climate' | dataframe3$Weather == 'Indoor' | dataframe3$Weather == 'Indoors',"Indoor", ifelse(dataframe3$Weather =="Clear and warm"| dataframe3$Weather == 'Sun & clouds' | dataframe3$Weather == 'Sunny' | dataframe3$Weather == 'Mostly Sunny' | dataframe3$Weather == 'Sunny, highs to upper 80s',"Warm", ifelse(dataframe3$Weather =="Light Rain"| dataframe3$Weather == "Showers","Wet","Mild")))

  dataframe <- dataframe3 %>% 
    
    mutate(X_std = ifelse(ToLeft, 120-x, x) - 10, ## Standardizes X
           Y_std = ifelse(ToLeft, 160/3-y, y), ## Standardized Y
           Dir_std_1 = ifelse(ToLeft & dir < 90, dir + 360, dir), 
           Dir_std_1 = ifelse(!ToLeft & dir > 270, dir - 360, Dir_std_1), 
           Dir_std_2 = ifelse(ToLeft, Dir_std_1 - 180, Dir_std_1),
           Dir_std_1 = ifelse(!ToLeft & dir > 270, dir - 360, Dir_std_1), )   ## Standardized Dir
  
  dataframe$gameId<-as.factor(dataframe$gameId)
  dataframe$playId<-as.factor(dataframe$playId)
  dataframe$WeatherStd<-as.factor(dataframe$WeatherStd)
  return(dataframe)
}
FumblesData<-cleaningfunction(IPWorking)


qblocation<-FumblesData %>% filter(positionAbbr == "QB") %>% dplyr::select(c(gameId, playId, X_std, Y_std, s, dis, dir,  HeightInches, Weight))
#join balllocation x,y as new variables

IPWorking<-FumblesData %>%
  left_join(qblocation, by = c("gameId","playId"))
library(raster)
linesstore<-NULL
#for each row calculate distance from ball                     
for(i in 1:nrow(IPWorking)){
  linesstore[i]<-pointDistance(c(IPWorking$X_std.x[i], IPWorking$Y_std.x[i]), c(IPWorking$X_std.y[i], IPWorking$Y_std.y[i]),lonlat=F )
}
#store distance from ball as new variable
IPWorking$linelength<-linesstore 
IPWorking1<-IPWorking%>% unique %>%filter(Posession == 1)
print(nrow(IPWorking1))
IPWorking1$lineorder <-NULL
IPWorking1<- IPWorking1 %>%
  group_by(gameId, playId) %>%
  mutate(lineorder = order(order(linelength, decreasing=F)))
#subset for defense and order by distance from rusher
IPWorking2<-IPWorking%>% unique %>%filter(Posession == 0)
print(nrow(IPWorking2))
IPWorking2$lineorder <-NULL
IPWorking2<- IPWorking2 %>%
  group_by(gameId, playId) %>%
  mutate(lineorder = order(order(linelength, decreasing=F)))
#just defenders 1 and 2
Offense12<-IPWorking1%>%filter(lineorder==1)
Defenders12<-IPWorking2%>%filter(lineorder==1)
Defenders12$HeightDiff <-Defenders12$HeightInches.x - Defenders12$HeightInches.y
Defenders12$WeightDiff <-Defenders12$Weight.x - Defenders12$Weight.y
Defenders12$SpeedDiff <- Defenders12$s.x - Defenders12$s.y
Defenders12$AngleDiff <- Defenders12$dir.x - Defenders12$dir.y
library(reshape2)
#widen offense data so the distances of the 11 off players are variables                    
#wideIPworkingoff<-dcast(Offense12, gameId + playId + WeatherStd + Temperature + Fumble + Dir_std_1.x + s.x + team~ lineorder, value.var = "linelength")
#widen defense data so the distances of the 11 def players are variables   
wideIPworkingdef1<-dcast(Defenders12, gameId + playId ~ lineorder, value.var = "dis.x")
colnames(wideIPworkingdef1) <- c("gameId","playId","DefDist")
wideIPworkingdef2<-dcast(Defenders12, gameId + playId ~ lineorder, value.var = "s.x")
colnames(wideIPworkingdef2) <- c("gameId","playId","DefSpeed")
wideIPworkingdef3<-dcast(Defenders12, gameId + playId ~ lineorder, value.var = "HeightDiff")
colnames(wideIPworkingdef3) <- c("gameId","playId","HeightDiff")
wideIPworkingdef4<-dcast(Defenders12, gameId + playId ~ lineorder, value.var = "WeightDiff")
colnames(wideIPworkingdef4) <- c("gameId","playId","WeightDiff")
wideIPworkingdef5<-dcast(Defenders12, gameId + playId ~ lineorder, value.var = "SpeedDiff")
colnames(wideIPworkingdef5) <- c("gameId","playId","SpeedDiff")
wideIPworkingdef6<-dcast(Defenders12, gameId + playId ~ lineorder, value.var = "positionAbbr")
colnames(wideIPworkingdef6) <- c("gameId","playId","DefPosition")
wideIPworkingdef7<-dcast(Defenders12, gameId + playId ~ lineorder, value.var = "AngleDiff")
colnames(wideIPworkingdef7) <- c("gameId","playId","AngleDiff")

merge1<-merge(Offense12, wideIPworkingdef1 , by.x=c("gameId","playId"), by.y=c("gameId","playId"))
merge2<-merge(merge1, wideIPworkingdef2 , by.x=c("gameId","playId"), by.y=c("gameId","playId"))
merge3<-merge(merge2, wideIPworkingdef3 , by.x=c("gameId","playId"), by.y=c("gameId","playId"))
merge4<-merge(merge3, wideIPworkingdef4 , by.x=c("gameId","playId"), by.y=c("gameId","playId"))
merge5<-merge(merge4, wideIPworkingdef5 , by.x=c("gameId","playId"), by.y=c("gameId","playId"))
merge6<-merge(merge5, wideIPworkingdef6 , by.x=c("gameId","playId"), by.y=c("gameId","playId"))
merge7<-merge(merge6, wideIPworkingdef7 , by.x=c("gameId","playId"), by.y=c("gameId","playId"))

BMISubset<-Defenders12 %>% dplyr::select(c(gameId, playId, HeightInches.x, Weight.x))
colnames(BMISubset) <- c("gameId","playId","DefHeight", "DefWeight")
merge8<-merge(merge7, BMISubset , by.x=c("gameId","playId"), by.y=c("gameId","playId"))

#fumbles<-merge8 %>% dplyr::select(-c(gameId,playId,ï..nflID,Name,event,x,y,PassResult,Weather,Posession, playupornot, ToLeft, X_std.x, Y_std.x,X_std.y, Y_std.y, dir.x, dir.y, Dir_std_2, linelength, lineorder, positionAbbr   ))
fumbles<-merge8 %>% dplyr::select(-c(gameId,playId,nflID,Name,event,x,y,PassResult,Weather,Posession, playupornot, ToLeft, X_std.x, Y_std.x,X_std.y, Y_std.y, dir.x, dir.y, Dir_std_2, linelength, lineorder, positionAbbr   ))
fumbles$BMI <- (fumbles$Weight.x/(fumbles$HeightInches.x^2))*703
fumbles$DefBMI <- (fumbles$DefWeight/(fumbles$DefHeight^2))*703
fumbles$BMIDiff <- fumbles$BMI - fumbles$DefBMI 
fumbles[is.na(fumbles)]<-0
