# file name: Pollution_func
#
# Rome, April 9th, 2024
#
#-------------------------------------------------------------------------------
# Data loading and moving mean
#-------------------------------------------------------------------------------
#
load.mm<-function(url.0,path.0,nd,pollutants,years) {
  mymatrix<-matrix(nrow=365,ncol=length(pollutants))
  e.data<-as.data.frame(mymatrix)
  e.list<-list()
  for (y in 1:length(years)) {
    for (j in 1:length(pollutants)) {
      name.file<-gsub(" ","",paste("RM_",pollutants[j],"_",years[y],"_gg.txt"))
      url<-gsub(" / ","/",paste(url.0,"/",name.file))
      path<-gsub(" / ","/",paste(path.0,"/",name.file))
      download.file(url,path,method="auto") # download file from website
      mydata<-read.csv(file=name.file,header=T,sep="")
      file.remove(path)
      colnames(mydata)<-gsub("X","",colnames(mydata))
      c.col<-2
      for (k in 3:length(mydata[1,])) {
        if (as.numeric(colnames(mydata)[k])<60) c.col<-c.col+1
      }
      mydata<-mydata[,1:c.col] # stations within Rome
      mydata<-replace(mydata,mydata==-999,NA)
      for (k in 1:length(mydata[,1])) {
        mydata[k,3]<-mean(as.numeric(mydata[k,3:c.col]),na.rm=T) # mean over all the stations
      }
      days<-length(mydata[,1])
      for (d in 1:days) {
        e.data[d,j]<-mydata[d,3]
      }
      e.data[,j]<-runner(e.data[,j],k=nd,f=mean,simplify=T) # moving mean
      colnames(e.data)[j]<-pollutants[j]
    }
    e.list[[y]]<-e.data
  }
  return(e.list)
}
#
#-------------------------------------------------------------------------------
# Plot of normalized diagrams for pollutants and table of correlations
#-------------------------------------------------------------------------------
#
fun.plot<-function(df,str,pollutants) {
  len<-length(pollutants)
  col.names<-c("orange","black","blue","cyan","magenta","red3","azure2","gray","yellow")
  file.str<-gsub(" ","",paste("RM_",str,".jpeg")) 
  jpeg(file.str,quality=100,res=150,width=1500,height=1500)
  par(mfrow=c(2,1))
  massimo<-max(df,na.rm=T)
  minimo<-min(df,na.rm=T)
  days<-length(df[,1])
  for (j in 1:length(pollutants)) {
    vec<-as.data.frame(df[j])
    xlab<-paste("days - moving mean on",nd,"days")
    plot(seq(1:days),vec[,1],xlim=c(1,days+100),ylim=c(minimo,massimo),type="l",lty=1,
         col=col.names[j],lwd=2.5,xlab=xlab,ylab="normalized measures")
    if (j<length(pollutants)) par(new=T)
  }
  #
  months<-c()
  months[1]<-1
  vector<-c()
  vector[1]<-1
  for (k in 2:days) {
    months[k]<-month(make_date(2023,1,1)+days(k-1))
    if (months[k]>months[k-1]) vector[months[k]]<-k
  }
  #
  title(paste("Rome - year:",str))
  abline(v=vector,lty=2)
  abline(h=0,lty=2)
  legend("right",legend=pollutants,col=col.names,lwd=rep(2.5,len),lty=rep(1,len),
         bg ="white")
  vector<-vector+10
  text(x=vector,y=c(rep(minimo,12)), 
       labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dic"))
  corrplot(cor(df),method="color",addCoef.col="black",number.digits=2,
           number.cex=0.8)
  dev.off()
}
