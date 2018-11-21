#Set output directory
#setwd("./FishTank_GEM/NETCDF/")
library(ncdf4)

source("timeseries_plot.R")

#enter defaults- if user does not use a run script with these choices
if (!exists("which_eqs"))
{
   which_eqs <- "cgem"
}

if (!exists("ncfile"))
{
    ncfile <- "output.000000.nc"
}

## Open NetCDF file
nc <- nc_open(ncfile)

## Later, let user define an array of variables, for now we do all of them.
Var <- names(nc$var) 
nvars <- length(Var)

## Phosphorus to Carbon ratio
PC_RatioAtt <- ncatt_get(nc, varid = 0, attname = "APCP")
PC_ratio <- PC_RatioAtt$value

## Load Model Variables
srp <- ncvar_get(nc, "SRP")
dop <- ncvar_get(nc, "DOP")
rop <- ncvar_get(nc, "ROP")
lop <- ncvar_get(nc, "LOP")
dia <- ncvar_get(nc, "DIA")
gre <- ncvar_get(nc, "GRE")
zoo <- ncvar_get(nc, "ZOO")

## Total phosphorus
TP_all <- srp + dop + rop + lop + PC_ratio * (dia + gre + zoo)

## Add TP to the variable vector Var
Var[nvars + 1] <- "TP"
nvars <- length(Var)

## Nitrogen to Carbon ratio
fracNCAtt <- ncatt_get(nc, varid = 0, attname = "ANCP")
fracNC <- fracNCAtt$value

## Carbon:chlorophyll ratio for diatoms
chlrdAtt <- ncatt_get(nc, varid = 0, attname = "CCHLD")  
chlrd <- chlrdAtt$value

# Carbon:chlorophyll ratio for greens
chlrgAtt <- ncatt_get(nc, varid = 0, attname = "CCHLG")  
chlrg <- chlrgAtt$value

# For CGEM, the first 5 variables are not state variables (put those in later...for now, cut out)
# For GoMDOM, the first 6 variables
# For odd files, let user specify:
if (!exists("firsts"))
{
    if(which_eqs == "cgem") firsts <- 6
    if(which_eqs == "gomdom") firsts <- 7
}

Var <- Var[firsts:nvars]
nvars <- length(Var) 

time <- ncvar_get(nc,"time")
iYr0 <- ncatt_get(nc, 0, attname = "iYr0")$value
time <- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz = "GMT")
tt <- dim(time)

if (!exists("pdfname"))
{
    if(which_eqs == "cgem") pdfname = "cgem_0D.pdf"
    if(which_eqs == "gomdom") pdfname = "gomdom_0D.pdf"
}

pdf(file = pdfname)

if (!exists("pdf_layout"))
{
    pdf_layout <- c(4,4)
}

which_mod <- pdf_layout[1] * pdf_layout[2]

par(mfrow = pdf_layout)

for(i in 1:nvars)
{  
  if (Var[i] == "TP")
  {
     rdata <- TP_all
     unit <- "P kg/m3"
  } else
    {
       rdata <- ncvar_get(nc, Var[i])
       unit <- ncatt_get(nc, Var[i], attname = "units")$value
    }

  if(rdata[1] > 1.e30)
  {
     timeseries_plot(Var[i], time[2:tt], rdata[2:tt], unit, chlrd, chlrg)

  } else
    {
       timeseries_plot(Var[i], time, rdata, unit, chlrd, chlrg)
    }


  if(i%%which_mod == 0) 
  {
     par(mfrow = pdf_layout)
  }

}

dev.off()
