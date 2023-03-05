#Set output directory
#setwd("./FishTank_GEM/NETCDF/")
library(ncdf4)

source("timeseries_plot.R")

#enter defaults- if user does not use a run script with these choices
if(!exists("which_eqs")){
cat("You need to specify defaults:\n")
cat("which_eqns, pdf_layout, pdfname,\n")
cat("ncfileX, X=1-7\n")
stop("Please use a header file with defaults.")
}

nc1<-nc_open(ncfile1)
nc2<-nc_open(ncfile2)
nc3<-nc_open(ncfile3)
nc4<-nc_open(ncfile4)
nc5<-nc_open(ncfile5)
nc6<-nc_open(ncfile6)
nc7<-nc_open(ncfile7)

#later, let user define an array of variables, for now we do all of them.
Var <- names(nc1$var) 
nvars <- length(Var)

#for CGEM, the first 5 variables are not state variables (put those in later...for now, cut out)
#for WQEM, the first 6 variables
#for odd files, let user specify:
if(!exists("firsts")){
if(which_eqs=="cgem") firsts <- 6
if(which_eqs=="wqem") firsts <- 7
}

Var <- Var[firsts:nvars]
nvars <- length(Var) 

time <- ncvar_get(nc1,"time")
iYr0 <- ncatt_get(nc1,0,attname="iYr0")$value
time<- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz="GMT")
tt <- length(time) 

depth <- ncvar_get(nc1,"h")
nz <- length(depth)

if(!exists("pdfname")){
if(which_eqs=="cgem") pdfname="cgem_1D.pdf"
if(which_eqs=="wqem") pdfname="wqem_1D.pdf"
}

pdf(file=pdfname,paper="a4r")

which_times <- c(1,60,121,152,182,213,274,335)

n_times <- length(which_times)

which_mod <- pdf_layout[1]*pdf_layout[2] 
par(mfrow=pdf_layout)

 for(i in 1:nvars){
  rdata1 <- ncvar_get(nc1,Var[i])
  rdata2 <- ncvar_get(nc2,Var[i])
  rdata3 <- ncvar_get(nc3,Var[i])
  rdata4 <- ncvar_get(nc4,Var[i])
  rdata5 <- ncvar_get(nc5,Var[i])
  rdata6 <- ncvar_get(nc6,Var[i])
  rdata7 <- ncvar_get(nc7,Var[i])

  unit <- ncatt_get(nc1,Var[i],attname="units")$value

  for(jj in 1:n_times){
          j <- which_times[jj]   
          t_label <- paste(months(time[j]),paste(format(time[j],"%d"),format(time[j],"%Y"),sep=","))

#   if(rdata1[1,j]>1.e30){  #wqem has missing data for first output, shows as 1e30+
#    xmin <- min(rdata1[,j+1],rdata7[,j+1],na.rm=TRUE) 
#    xmax <- max(rdata1[,j+1],rdata7[,j+1],na.rm=TRUE)
#    xrange <- c(xmin,xmax) 
#    timeseries_plot(Var[i],rdata1[,j+1],depth,time[j+1],range=rev(range(depth)),uselim=FALSE,xrange=xrange,color="red")
#    timeseries_addlines(Var[i],rdata2[,j+1],depth,color="blue") #blue, X*0.5
#    timeseries_addlines(Var[i],rdata3[,j+1],depth,color="black",linewidth=3) #black, X*1
#    timeseries_addlines(Var[i],rdata4[,j+1],depth,color="red",linetype="dashed") #red, X*1.25
#    timeseries_addlines(Var[i],rdata5[,j+1],depth,color="blue",linetype="dashed") #blue, X*1.5
#    timeseries_addlines(Var[i],rdata6[,j+1],depth,color="purple") #purple, X*2
#    timeseries_addlines(Var[i],rdata7[,j+1],depth,color="green") #green, X*10
#    }else{
    xmin <- min(rdata1[,j],rdata2[,j],rdata3[,j],rdata4[,j],rdata5[,j],rdata6[,j],rdata7[,j],na.rm=TRUE)
    xmax <- max(rdata1[,j],rdata2[,j],rdata3[,j],rdata4[,j],rdata5[,j],rdata6[,j],rdata7[,j],na.rm=TRUE)
    xrange <- c(xmin,xmax)
    timeseries_plot(Var[i],rdata1[,j],depth,t_label,range=rev(range(depth)),uselim=FALSE,xrange=xrange,color="red",linetype="dashed")#red, X*0.25
    timeseries_addlines(Var[i],rdata2[,j],depth,color="blue",linetype="dashed") #blue, X*0.5
    timeseries_addlines(Var[i],rdata3[,j],depth,color="black",linewidth=2) #black, X*1	
    timeseries_addlines(Var[i],rdata4[,j],depth,color="red") #red, X*1.25
    timeseries_addlines(Var[i],rdata5[,j],depth,color="blue") #blue, X*1.5
    timeseries_addlines(Var[i],rdata6[,j],depth,color="purple") #purple, X*2
    timeseries_addlines(Var[i],rdata7[,j],depth,color="green") #green, X*10
#    }
   }


 if(i%%which_mod == 0) {
  par(mfrow=pdf_layout)
 }

}

dev.off()
