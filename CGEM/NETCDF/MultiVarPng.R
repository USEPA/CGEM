#ENTER FILE PATH TO netCDF File
#ncfile<-"1D.nc"
#Pick Variable
#var <- c("A1","A2","A3","A4","A5","A6")
#Plot log?
#ilog <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)


#Function, input is two strings and a boolean
#Just for CGEM, because of time variable
MultiVarPng <- function(ncfile,var,ilog){

library(ncdf4)

#Open netCDF file and get dimensions
nc<-nc_open(ncfile)

#Load Model Variables
nvars<-dim(as.array(var))

Var1 <- ncvar_get(nc,var[1])
dims <- dim(Var1)

Var <- array(NA,dim=c(nvars,dims[1],dims[2]))
Var[1,,] <- Var1
unit <- ncatt_get(nc,var[1],attname="units")
units <- unit$value

if(nvars>=2){
 for (i in 2:nvars) {
   Var[i,,]<-ncvar_get(nc,var[i])
   unit <- ncatt_get(nc,var[i],attname="units")
   units <- cbind(units,unit$value)
 }
}

for(i in 1:nvars){
 if(ilog[i]){ 
  Var[i,,] <- log10(Var[i,,]) #Might have 'log zero' error, need to add check
  logtxt <- "log of"
 }else {
  logtxt <- ""
 }
}

ymin1<-min(Var[,1,])
if(ymin1<=-9998.) ymin1<-min(Var[,1,2:dims[2]])

ymax1<-max(Var[,1,])

lon<-ncvar_get(nc,"longitude")
lat <- ncvar_get(nc,"latitude")

#Get times and parse units to get time variables
time_sec<-ncvar_get(nc,"time")
time_sec<- as.POSIXct(time_sec, origin="2002-01-01", tz="GMT")

#Title, use lat/lon
if(nvars==1) {
 VarTitle<-file.path(var[1],"_lon=",round(lon,1),"_lat=",round(lat,1),".png", fsep="")
}else{
 VarTitle<-file.path(var[1],"-",var[nvars],"_lon=",round(lon,1),"_lat=",round(lat,1),".png", fsep="")
}

png(VarTitle)

colorlist <- c("black","red","orange","green","blue","purple")


par(oma=c(0,0,2,0));              #page with 2x2 plots
plot(time_sec,Var[1,1,],main="k=1",xlab="",ylab=paste(logtxt[1],units[1]),ylim=c(ymin1,ymax1),type="l",col=colorlist[1])
if(nvars>=2){
 for(i in 2:nvars){
  lines(time_sec,Var[i,1,],col=colorlist[round(i%%6.1)])
 }
}

if(nvars==1) {
 title(paste(var[1],"vs time starting",time_sec[1],sep=" "),outer=TRUE)
}else{
title(paste(var[1],"-",var[nvars],"vs time starting",time_sec[1],sep=" "),outer=TRUE)
}
dev.off()

}
