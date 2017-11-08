#Set output directory
#setwd("./FishTank_GEM/NETCDF/")
library(ncdf4)

source("timeseries_plot.R")

#enter defaults- if user does not use a run script with these choices
if(!exists("which_eqs")){
which_eqs <- "cgem"
}
if(!exists("ncfile")){
ncfile<-"output.000000.nc"
}

nc<-nc_open(ncfile)
nc2<-nc_open(ncfile2)

#later, let user define an array of variables, for now we do all of them.
Var <- names(nc$var) 
nvars <- length(Var)

#for CGEM, the first 5 variables are not state variables (put those in later...for now, cut out)
#for GoMDOM, the first 6 variables
#for odd files, let user specify:
if(!exists("firsts")){
if(which_eqs=="cgem") firsts <- 6
if(which_eqs=="gomdom") firsts <- 7
}

Var <- Var[firsts:nvars]
nvars <- length(Var) 

time <- ncvar_get(nc,"time")
iYr0 <- ncatt_get(nc,0,attname="iYr0")$value
time<- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz="GMT")
tt <- length(time) #64
#length(time)

depth <- ncvar_get(nc,"h")
nz <- length(depth)

if(!exists("pdfname")){
if(which_eqs=="cgem") pdfname="cgem_1D.pdf"
if(which_eqs=="gomdom") pdfname="gomdom_1D.pdf"
}

pdf(file=pdfname,paper="USr")

which_times <- c(1,121,152,182,213,335)
n_times <- length(which_times)

if(!exists("pdf_layout")){
pdf_layout <- c(1,6)
}

which_mod <- pdf_layout[1]*pdf_layout[2] 

par(mfrow=pdf_layout)

 for(i in 1:nvars){
  rdata <- ncvar_get(nc,Var[i])
  rdata2 <- ncvar_get(nc2,Var[i])

  unit <- ncatt_get(nc,Var[i],attname="units")$value
  #ymax <- max(rdata,na.rm=TRUE)
  #ymax <- ymax + 0.1*ymax

  for(jj in 1:n_times){
          j <- which_times[jj]   
          t_label <- paste(months(time[j]),format(time[j],"%d"))
   if(rdata[1,j]>1.e30){
    xmin <- min(rdata[,j+1],rdata2[,j+1],na.rm=TRUE) 
    xmax <- max(rdata[,j+1],rdata2[,j+1],na.rm=TRUE)
    xrange <- c(xmin,xmax) 
    timeseries_plot(Var[i],rdata[,j+1],depth,time[j+1],range=rev(range(depth)),uselim=FALSE,xrange=xrange)
    timeseries_addlines(Var[i],rdata2[,j+1],depth,color="red")
    }else{
    xmin <- min(rdata[,j],rdata2[,j],na.rm=TRUE)
    xmax <- max(rdata[,j],rdata2[,j],na.rm=TRUE)
    xrange <- c(xmin,xmax)
    timeseries_plot(Var[i],rdata[,j],depth,time[j],range=rev(range(depth)),uselim=FALSE,xrange=xrange)
    timeseries_addlines(Var[i],rdata2[,j],depth,color="red")
    }
   }


 if(i%%which_mod == 0) {
  par(mfrow=pdf_layout)
 }

}

dev.off()
