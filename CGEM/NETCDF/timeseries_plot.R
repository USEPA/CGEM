## To use:
## source ("timeseries_plot.R")

#Starts a new plot of a 1D time series variable
timeseries_plot <- function(Var,time,rdata,unit,ilog=FALSE,label=NULL,range=NULL){

if(ilog){
   rdata <- log(rdata) #Might have 'log zero' error, need to add check
   logtxt<-"log of"
}else{
   logtxt<-""
}

ylimit <- get_ylim(rdata,range)

#yaxt="n"
plot(time,rdata,yaxt="n",type="l",ylab="",xlab="",main=paste(Var,label),ylim=ylimit);
mtext(paste(logtxt,unit),side=3,line=.2,cex=0.8)
axis(2, at=get_lab(ylimit), labels=get_lab(ylimit), las=2)
}


#Adds lines to an existing plot, can change color but not labels or ranges (for comparisons)
timeseries_addlines <- function(Var,time,rdata,color="red"){
lines(time,rdata,col=color)
}


get_ylim <- function(indata,range) {
 ymin <- min(indata,na.rm=TRUE) 
 ymax <- max(indata,na.rm=TRUE)
 if(!is.null(range)){
  ymin <- range[1]
  ymax <- range[2]
 }
 c(ymin,ymax)
}

get_ylim_for2 <- function(indata,indata2,range) {
 ymin <- (min( min(indata), min(indata2) ));
 ymax <- (max( max(indata), max(indata2) ));
 if(!is.null(range)){
  ymin <- range[1]
  ymax <- range[2]
 }
 c(ymin,ymax)
}

get_lab <- function(ylimit){
 ymin<-ylimit[1] 
 ymax<-ylimit[2]
 ymid <- ymin + (ymax-ymin)/2.
 ymin <- signif(ymin,digits=4)
 ymax <- signif(ymax,digits=4)
 ymid <- signif(ymid,digits=4)
 c(ymin,ymid,ymax)
}
