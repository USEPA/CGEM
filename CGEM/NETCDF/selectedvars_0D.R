#Set output directory
#setwd("./FishTank_GEM/NETCDF/")
library(ncdf4)

source("timeseries_selectedplots.R")

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
Var <- c("GRE", "SRP", "LOP", "PFG", "IFG", "TFG", "PAR")
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
pfg <- ncvar_get(nc, "PFG")
ifg <- ncvar_get(nc, "IFG")
tfg <- ncvar_get(nc, "TFG")
ti  <- ncvar_get(nc, varid = "time")

nt <- length(ti)

## Maximum production rate
pmgAtt <- ncatt_get(nc, varid = 0, attname = "PMG")
pmg <- pmgAtt$value

## Greens production 
PG <- array(0.0, dim = c(nt))
for (i in 1:nt)
{
  PG[i] <- pmg * pfg[i] * ifg[i] * tfg[i] 
}

## Add PG to the variable vector Var
Var[nvars + 1] <- "PG"
nvars <- length(Var)

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

# Var <- Var[firsts:nvars]
# nvars <- length(Var) 

time <- ncvar_get(nc, "time")
iYr0 <- ncatt_get(nc, 0, attname = "iYr0")$value
time <- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz = "GMT")
tt <- dim(time)

# Define array that will be used to hold greens production values.
PG_table <- array(0, dim = c(tt,2))
PG_table[,2] <- as.character(time)
PG_table[,1] <- PG

# Define array that will hold nutrient, light, and temperature limitations
Limitations_table <- array(0, dim = c(tt,4))
Limitations_table[,4] <- as.character(time)
Limitations_table[,1] <- pfg
Limitations_table[,2] <- ifg
Limitations_table[,3] <- tfg

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
  }else if (Var[i] == "PG")
    {
       rdata <- PG
       unit <- "1/s"
    }else
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

# Write PG values to a CSV file.
output_folder <- getwd()
output_title <- file.path("Greens_Production", "csv", fsep = ".")
output_text <- file.path(output_folder, output_title, fsep = "/")

write.table(PG_table, output_text, col.names = F, sep = ",")

# Write limitation values to a CSV file.
output_folder1 <- getwd()
output_title1 <- file.path("Nutrient_Light_Temp_Limitations", "csv", fsep = ".")
output_text1 <- file.path(output_folder1, output_title1, fsep = "/")

write.table(Limitations_table, output_text1, col.names = F, sep = ",")
