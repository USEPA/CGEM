## To use:
## source ("R3D_timeseries.R")

R3D_timeseries <- function(time,rdata,unit,ii,jj,k,Var,ilog){

if(ilog){
   rdata[k,] <- log(rdata[k,]) #Might have 'log zero' error, need to add check
   logtxt<-"log of"
}else{
   logtxt<-""
}

plot(time,rdata[k,],yaxt="n",type="l",xlab="",ylab=paste(logtxt,unit),main=paste(Var,"k=",k),ylim=ylim_k(rdata[k,],rdata[k,]));
axis(2, at=get_lab(rdata[k,],rdata[k,]), labels=get_lab(rdata[k,],rdata[k,]), las=2);

}
