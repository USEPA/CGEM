#Set output directory
#setwd("./FishTank_GEM/NETCDF/")
library(ncdf4)

source("timeseries_plot.R")

#enter defaults- if user does not use a run script with these choices
if (!exists("which_eqs")) {
  which_eqs <- "cgem"
}

if (!exists("ncfile")) {
  ncfile <- "output.000000.nc"
}

nc <- nc_open(ncfile)

#later, let user define an array of variables, for now we do all of them.
Var <- names(nc$var) 
nvars <- length(Var)

#for CGEM, the first 5 variables are not state variables (put those in later...for now, cut out)
#for WQEM, the first 6 variables
#for odd files, let user specify:
if (!exists("firsts")){
  if (which_eqs == "cgem") firsts <- 6
  if (which_eqs == "wqem") firsts <- 7
}

Var <- Var[firsts:nvars]
nvars <- length(Var) 

time <- ncvar_get(nc,"time")
iYr0 <- ncatt_get(nc,0,attname="iYr0")$value
time <- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz="GMT")
tt <- dim(time)

if (!exists("pdfname")) {
  if (which_eqs=="cgem") pdfname = "cgem_0D.pdf"
  if (which_eqs=="wqem") pdfname = "wqem_0D.pdf"
}

pdf(file=pdfname)

if (!exists("pdf_layout")) {
  pdf_layout <- c(4,4)
}

which_mod <- pdf_layout[1] * pdf_layout[2]

par(mfrow=pdf_layout)

for (i in 1:nvars) {
  rdata <- ncvar_get(nc,Var[i])
  unit <- ncatt_get(nc,Var[i],attname="units")$value
  
  if (!is.na(rdata[1])){
    timeseries_plot(Var[i],time,rdata,unit)
  }
  
  if (i%%which_mod == 0) {
    par(mfrow=pdf_layout)
  }

}

dev.off()
