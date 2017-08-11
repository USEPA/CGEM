#ENTER FILE PATH TO netCDF File
ncfile<-"output.000000.nc"

source("PlotLimits.R")
source("MultiVarPng.R") #function 
source("TXPng.R")

var <- c("DOC") #Pick Variable
ilog <- c(FALSE) #Plot log?
MultiVarPng(ncfile,var,ilog,lDOC) #Create png plots

var <- c("DIA")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lDIA)

var <- c("GRE")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lGRE)

var <- c("ZOO")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lZOO)

var <- c("LOC")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lLOC)

var <- c("ROC")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lROC)

var <- c("SRP")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lSRP)

var <- c("DOP")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lDOP)

var <- c("LOP")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lLOP)

var <- c("ROP")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lROP)

var <- c("NH4")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lNH4)

var <- c("NO3")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lNO3)

var <- c("DON")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lDON)

var <- c("LON")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lLON)

var <- c("RON")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lRON)

var <- c("SA")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lSA)

var <- c("SU")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lSU)

var <- c("DO2")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lDO2)

var <- c("TR")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lTR)

var <- c("DIAN")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lDIAN)

var <- c("DIAP")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lDIAP)

var <- c("GREN")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lGREN)

var <- c("GREP")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lGREP)

var <- c("PAR")
ilog <- c(FALSE)
MultiVarPng(ncfile,var,ilog,lPAR)

TXPng(ncfile,"TN",FALSE,lTN)
TXPng(ncfile,"TP",FALSE,lTP)

source("ProdRespPng.R")
source("LimitationFactors.R")
