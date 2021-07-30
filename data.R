library(plyr)
library(tidyverse)
library(ggplot2)
options(stringsAsFactors = FALSE)


# define workspace environment 
Path2Dir<-"/set/your/path/"
setwd(Path2Dir)

# define your country
country<-"Austria"

#download data
URL = "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/csv/data.csv"
destfile = paste(Path2Dir,"data_",Sys.Date(),".csv",sep="")
download.file(URL,destfile)

# open pandemic data from : european center for disease prevention and control
datas = read.csv(paste("data_",Sys.Date(),".csv",sep=""))
#datas = read.csv("data_historical.csv")


#treatment and extraction of data
nbr_ligne = by(datas,datas$countriesAndTerritories,nrow)
liste_donnee = matrix(NA,nrow = nbr_ligne[country],ncol = 5)
count = 0
datesliste = c()
for (i in 1:length(datas$dateRep)) {
  if (datas$countriesAndTerritories[i]==country) {
    count = count +1
    month = NA
    if (nchar(toString(datas$month[i]))==1) {month = paste('0',datas$month[i],sep='')} else {month = datas$month[i]}
    day = NA
    if (nchar(toString(datas$day[i]))==1) {day = paste('0',datas$day[i],sep='')} else {day = datas$day[i]}
    dates = strtoi(paste(datas$year[i],month,day,sep = ''))
    dates=as.Date(datas$dateRep[i],"%d/%m/%Y")
    sousliste = c(strtoi(datas$day[i]),strtoi(datas$month[i]),strtoi(datas$year[i]), strtoi(datas$cases[i]),strtoi(datas$deaths[i]))
    print(i)
    #print(data[i,])
    liste_donnee[count,] = c(sousliste)
    datesliste = c(datesliste,format(dates, format="%m/%d/%Y"))
  }
}
#creates dataset that store numbers to plot
plot_data = matrix(NA, ncol=4, nrow = length(liste_donnee[,1]))
plot_data[,1]=datesliste
plot_data[,2]=liste_donnee[,4]
plot_data[,3]=liste_donnee[,5]

# change format from character to date for column 1 and to int for column 2
plot_data = data.frame(plot_data,stringsAsFactors = FALSE)
plot_data$X1 = as.Date(plot_data$X1,format = "%m/%d/%Y")
plot_data$X2=strtoi(plot_data$X2)
plot_data$X3=strtoi(plot_data$X3)



# define week number thanks to date
for (i in 1:length(plot_data$X1)){
  semaine = strftime(plot_data$X1[i],format="%V")
  plot_data[i,4]=semaine
}

# plot data
plot_data_sum = ddply(plot_data, c('X4'),summarise,cases=sum(X2),death=sum(X3))
semaineEnCours = strftime(Sys.Date(),format="%V")
p1=ggplot(data=plot_data, aes())+geom_line(aes(x=X1, y=X2,group=1,color='cases'))+geom_line(aes(x=X1, y=X3,group=1,color='deaths'))+labs(x='date',y='number of cases/deaths')+ggtitle('number of cases/deaths per day')+theme(plot.title = element_text(hjust = 0.5))
ggsave(paste("data_",Sys.Date(),'.png',sep=""), plot = p1)
p2=ggplot(plot_data_sum,aes())+geom_line(aes(X4,cases,group=1,color='cases'))+scale_y_continuous(trans = 'log10')+geom_line(aes(X4,death,group=1,color='deaths'))+labs(x='week',y='number of cases/deaths')+ ggtitle(paste('number of cases/deaths in function of the week (current week: ',semaineEnCours,')',sep = ""))+theme(plot.title = element_text(hjust = 0.5))
ggsave(paste("data_",Sys.Date(),'perweek.png',sep=""), plot = p2)

