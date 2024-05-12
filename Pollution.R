# file name: Pollution
#
# Rome, April 7th, 2024
#
library(car) 
library(runner) # for moving mean
library(lubridate) # for make_date()
library(corrplot) # for corrplot
source("Pollution_func.R") # a set of subroutines
#
# acquire paths
#
pollutants<-c("BENZENE","CO","NO","NO2","NOX","O3","PM2.5","PM10","SO2")
years<-as.character(seq(2013,2023))
url.0<-"https://www.arpalazio.net/main/aria/sci/basedati/chimici/BDchimici/RM/MedieGiornaliere/"
path.0<-getwd( )
setwd(path.0)
#
nd<-7 # number of days for the moving mean
#
# load, moving mean
#
e.list<-load.mm(url.0,path.0,nd,pollutants,years)
#
# data normalization
#
for (i in 1:length(years)) {
  for (j in 1:length(pollutants)) {
    e.list[[i]][j]<-as.vector(scale(e.list[[i]][j]))
  }
}
#
# plot for each year
#
for (i in 1:length(years)) {
  fun.plot(e.list[[i]],years[i],pollutants)
}
#
# mean over all the available years and plot
#
e.data<-e.list[[1]]
for (p in 1:length(pollutants)) {
  for (d in 1:length(e.list[[i]][,1])) {
    vec<-c()
    for (y in 1:length(years)) {
      vec[y]<-e.list[[y]][d,p]
      e.data[d,p]<-mean(vec[y],na.rm=T)
    }
  }
}
fun.plot(e.data,paste(years[1],"-",years[length(years)]),pollutants)
