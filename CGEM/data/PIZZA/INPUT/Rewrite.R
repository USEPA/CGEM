rdata<-read.table("ReformatWindPar.dat")

#year<-rdata[,1]
#month<-rdata[,2]
#day<-rdata[,3]
#hour<-rdata[,4]
#minute<-rdata[,5]
dates<-rdata[,1:5]
Wind<-rdata[,6]
PAR<-rdata[,7]

tw<-cbind(dates,Wind)
ts<-cbind(dates,PAR)

write.table(tw,file="Wind.dat",quote=FALSE,row.names=FALSE,col.names=FALSE)
write.table(ts,file="Solar.dat",quote=FALSE,row.names=FALSE,col.names=FALSE)

