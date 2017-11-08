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

if(!exists("ncfile2")){
ncfile<-"output.000000.nc"
}

nc1<-nc_open(ncfile1)
nc2<-nc_open(ncfile2)
nc3<- nc_open(ncfile3)
nc4<- nc_open(ncfile4)

if(which_eqs=="gomdom"){
Var <- c("DOC","DIA","GRE","ZOO","LOC","ROC","SRP","DOP","LOP","ROP","NH4","NO3","DON","LON","RON","SA","SU","DO2","TR","DIAN","DIAP","GREN","GREP")
}else{
Var <- names(nc1$var)
Var <- Var[Var != "Tr"]
}

nvars <- length(Var)

#for CGEM, the first 5 variables are not state variables (put those in later...for now, cut out)
#for GoMDOM, the first 6 variables
#for odd files, let user specify:
if(!exists("firsts")){
if(which_eqs=="gomdom"){
firsts <- 1
}else{
firsts <- 6
}
}

if(which_eqs=="gomdom"){
Var <- Var[firsts:(firsts+19)]
}else{
Var <- Var[firsts:(nvars-2)]
}

nvars <- length(Var) 

Var <- c("A1","O2","Chla_mg_tot","irradiance_fraction")
nvars <- length(Var)

time <- ncvar_get(nc1,"time")
iYr0 <- ncatt_get(nc1,0,attname="iYr0")$value
time<- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz="GMT")
tt <- length(time) #64
#length(time)

if(!exists("pdfname")){
if(which_eqs=="cgem") pdfname="cgem_1D.pdf"
if(which_eqs=="gomdom") pdfname="gomdom_1D.pdf"
}

pdf(file=pdfname)

#k_layers <- c(1,6,12,20)
k_layers <- c(20)
n_layers <- length(k_layers)
#label <- paste("k=1,6,12,20")
label <- "k=20, black=old,fCCL blue=new,fCCL red=old,cloern green=new,cloern"

if(!exists("pdf_layout")){
pdf_layout <- c(4,4)
}

pdf_layout <- c(1,1)

which_mod <- pdf_layout[1]*pdf_layout[2] 

par(mfrow=pdf_layout)

colorlist <- c("black","red","blue","green","purple","orange","yellow","pink","brown")

 for(i in 1:nvars){
 rdata1 <- ncvar_get(nc1,Var[i])
 rdata2 <- ncvar_get(nc2,Var[i])
 rdata3 <- ncvar_get(nc3,Var[i])
 rdata4 <- ncvar_get(nc4,Var[i])

 unit <- ncatt_get(nc1,Var[i],attname="units")$value

 for(j in 1:n_layers){

# label <- paste("k=",k_layers[j])
 ymin <- min(rdata1[k_layers[j],],rdata4[k_layers[j],])
 ymax <- max(rdata1[k_layers[j],],rdata4[k_layers[j],])
 timeseries_plot(Var[i],time[1:tt],rdata1[k_layers[j],],unit,label=label,range=c(ymin,ymax))
 timeseries_addlines(Var[i],time[1:tt],rdata2[k_layers[j],],color="blue")
 timeseries_addlines(Var[i],time[1:tt],rdata3[k_layers[j],],color="red")
 timeseries_addlines(Var[i],time[1:tt],rdata4[k_layers[j],],color="green")
 }

 if((i*j)%%which_mod == 0) {
  par(mfrow=pdf_layout)
 }
}


dev.off()
