# Set output directory
# setwd("./testrun/NETCDF/")

library(ncdf4)

source("timeseries_plot.R")

# Enter defaults- if user does not use a run script with these choices
if (!exists("which_eqs"))
{
   which_eqs <- "cgem"
}
if (!exists("ncfile"))
{
   ncfile <- "output.000000.nc"
}

nc <- nc_open(ncfile)

#later, let user define an array of variables, for now we do all of them.
Var <- names(nc$var) 
nvars <- length(Var)

#for CGEM, the first 5 variables are not state variables (put those in later...for now, cut out)
#for WQEM, the first 6 variables are not state variables
#for odd files, let user specify:
if (!exists("firsts"))
{
    if(which_eqs == "cgem") firsts <- 6
    if(which_eqs == "wqem") firsts <- 7
}

Var <- Var[firsts:nvars]
nvars <- length(Var) 

time <- ncvar_get(nc,"time")
iYr0 <- ncatt_get(nc,0,attname="iYr0")$value
time <- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz="GMT")
tt <- length(time)

if(!exists("pdfname"))
{
   if(which_eqs == "cgem") pdfname = "cgem_1D.pdf"
   if(which_eqs == "wqem") pdfname = "wqem_1D.pdf"
}

pdf(file=pdfname)

# Specify indices of desired location.
iloc <- 4
jloc <- 8

k_layers <- nc$dim$k$vals
n_layers <- length(k_layers)
label <- paste("k = ", toString(k_layers))

if(!exists("pdf_layout"))
{
   pdf_layout <- c(4,4)
}

which_mod <- pdf_layout[1] * pdf_layout[2] 

par(mfrow = pdf_layout)

colorlist <- c("black","red","blue","green","purple","orange","yellow","pink","brown")

for(i in 1:nvars)
{

    rdata <- ncvar_get(nc, Var[i], start=c(iloc,jloc,k_layers[1],1), count=c(1,1,1,tt))
    unit <- ncatt_get(nc, Var[i], attname="units")$value
    rdata[is.infinite(rdata)] <- NA
    ymax <- max(rdata, na.rm=TRUE)
    ymax <- ymax + 0.1*ymax
    timeseries_plot(Var[i],time[1:tt],rdata,unit,label=label,range=c(0,ymax))
    if(n_layers >= 2)
    {
        for(j in 2:n_layers)
        {
             rdata <- ncvar_get(nc,Var[i],start=c(22,8,k_layers[j],1),count=c(1,1,1,tt))
             timeseries_addlines(Var[i],time[1:tt],rdata,color=colorlist[j])
        }
     }

     if(i%%which_mod == 0) 
     {
          par(mfrow=pdf_layout)
     }

}

dev.off()
