#Set output directory
#setwd("./FishTank_GEM/NETCDF/")
library(ncdf4)

source("timeseries_plot.R")

#enter defaults- if user does not use a run script with these choices
if(!exists("which_eqs")){
which_eqs <- "cgem"
}
if(!exists("ncfile")){
ncfile <- "output.000000.nc"
}

if(!exists("ncfile2")){
ncfile2 <- "output.000000.nc"
}

nc<-nc_open(ncfile)
nc2<-nc_open(ncfile2)

#later, let user define an array of variables, for now we do all of them.
Var <- names(nc$var) 
nvars <- length(Var)

#for CGEM, the first 5 variables are not state variables (put those in later...for now, cut out)
#for WQEM, the first 6 variables
#for odd files, let user specify:
if(!exists("firsts")){
if(which_eqs=="cgem") firsts <- 6
if(which_eqs=="wqem") firsts <- 7
}
#firsts <- 6

if(which_eqs=="wqem"){
#Var <- c("DOC","DIA","GRE","ZOO","LOC","ROC","SRP","DOP","LOP","ROP","NH4","NO3","DON","LON","RON","SA","SU","DO2","TR","DIAN","DIAP","GREN","GREP")
Var <- Var[firsts:(nvars)]
nvars <- length(Var)
}else{
Var <- names(nc$var)
Var <- Var[Var != "Tr"]
nvars <- length(Var)
Var <- Var[firsts:(nvars)]
nvars <- length(Var) 
}


time <- ncvar_get(nc,"time")
iYr0 <- ncatt_get(nc,0,attname="iYr0")$value
time<- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz="GMT")
tt <- length(time) #64
#length(time)

if(!exists("pdfname")){
if(which_eqs=="cgem") pdfname="cgem_1D.pdf"
if(which_eqs=="wqem") pdfname="wqem_1D.pdf"
}

pdf(file=pdfname)

k_layers <- c(1,6,12,20)
n_layers <- length(k_layers)
#label <- paste("k=1,6,12,20")

if(!exists("pdf_layout")){
pdf_layout <- c(4,4)
}

which_mod <- pdf_layout[1]*pdf_layout[2] 

par(mfrow=pdf_layout)

colorlist <- c("black","red","blue","green","purple","orange","yellow","pink","brown")

 for(i in 1:nvars){

 rdata <- ncvar_get(nc,Var[i])
 rdata2 <- ncvar_get(nc2,Var[i])
 unit <- ncatt_get(nc,Var[i],attname="units")$value

 for(j in 1:n_layers){

 label <- paste("k=",k_layers[j])
 divide <- mean(rdata[k_layers[j],])
 if(divide==0) divide <- 1e-18
 pdiff <- (rdata[k_layers[j],] - rdata2[k_layers[j],])/divide * 100.
 ymin <- min(pdiff,na.rm=TRUE) 
 ymax <- max(pdiff,na.rm=TRUE)
 zeros <- c(1:tt)
 zeros <- 0*zeros
 pdiff_mean <- c(1:tt)
 pdiff_mean <- pdiff_mean*0
 pdiff_mean <- pdiff_mean + mean(pdiff)
 #label <- paste(round(100.*(rdata[k_layers[j],1] - rdata2[k_layers[j],1])/rdata[k_layers[j],1],5))
 timeseries_plot(Var[i],time[1:tt],pdiff,paste("m%d=",round(pdiff_mean[1],3)),label=label,range=c(ymin,ymax))
 timeseries_addlines(Var[i],time[1:tt],zeros,color="red")
 timeseries_addlines(Var[i],time[1:tt],pdiff_mean,color="blue")

}

 if((i*j)%%which_mod == 0) {
  par(mfrow=pdf_layout)
 }
}

dev.off()
