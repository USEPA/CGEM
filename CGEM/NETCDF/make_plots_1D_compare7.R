#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
stop("arguments are 1 to 6")
}


which_eqs <- "cgem"

if(args[1]=="1"){
 ncfile1 <- "nc_output/astar490_0.009375.nc"
 ncfile2 <- "nc_output/astar490_0.01875.nc"
 ncfile3 <- "nc_output/astar490_0.0375.nc"
 ncfile4 <- "nc_output/astar490_0.046875.nc"
 ncfile5 <- "nc_output/astar490_0.05625.nc"
 ncfile6 <- "nc_output/astar490_0.075.nc"
 ncfile7 <- "nc_output/astar490_0.375.nc"
 pdfname <- "astar490.compare7.pdf"
}else if(args[1]=="2"){
 ncfile1 <- "nc_output/astarOMA_0.0025.nc"
 ncfile2 <- "nc_output/astarOMA_0.005.nc"
 ncfile3 <- "nc_output/astarOMA_0.01.nc"
 ncfile4 <- "nc_output/astarOMA_0.0125.nc"
 ncfile5 <- "nc_output/astarOMA_0.015.nc"
 ncfile6 <- "nc_output/astarOMA_0.02.nc"
 ncfile7 <- "nc_output/astarOMA_0.1.nc"
 pdfname <- "astarOMA.compare7.pdf"
}else if(args[1]=="3"){
 ncfile1 <- "nc_output/astarOMZ_0.0025.nc"
 ncfile2 <- "nc_output/astarOMZ_0.005.nc"
 ncfile3 <- "nc_output/astarOMZ_0.01.nc"
 ncfile4 <- "nc_output/astarOMZ_0.0125.nc"
 ncfile5 <- "nc_output/astarOMZ_0.015.nc"
 ncfile6 <- "nc_output/astarOMZ_0.02.nc"
 ncfile7 <- "nc_output/astarOMZ_0.1.nc"
 pdfname <- "astarOMZ.compare7.pdf"
}else if(args[1]=="4"){
 ncfile1 <- "nc_output/aw490_0.00375.nc"
 ncfile2 <- "nc_output/aw490_0.0075.nc"
 ncfile3 <- "nc_output/aw490_0.015.nc"
 ncfile4 <- "nc_output/aw490_0.01875.nc"
 ncfile5 <- "nc_output/aw490_0.0225.nc"
 ncfile6 <- "nc_output/aw490_0.03.nc"
 ncfile7 <- "nc_output/aw490_0.15.nc"
 pdfname <- "aw490.compare7.pdf"
}else if(args[1]=="5"){
 ncfile1 <- "nc_output/PARfac_0.25.nc"
 ncfile2 <- "nc_output/PARfac_0.5.nc"
 ncfile3 <- "nc_output/PARfac_1.0.nc"
 ncfile4 <- "nc_output/PARfac_1.25.nc"
 ncfile5 <- "nc_output/PARfac_1.5.nc"
 ncfile6 <- "nc_output/PARfac_2.0.nc"
 ncfile7 <- "nc_output/PARfac_10.0.nc"
 pdfname <- "PARfac.compare7.pdf"
}else if(args[1]=="6"){
 ncfile1 <- "nc_output/astarOMBC_0.0025.nc"
 ncfile2 <- "nc_output/astarOMBC_0.005.nc"
 ncfile3 <- "nc_output/astarOMBC_0.01.nc"
 ncfile4 <- "nc_output/astarOMBC_0.0125.nc"
 ncfile5 <- "nc_output/astarOMBC_0.015.nc"
 ncfile6 <- "nc_output/astarOMBC_0.02.nc"
 ncfile7 <- "nc_output/astarOMBC_0.1.nc"
 pdfname <- "astarOMBC.compare7.pdf"
}else{
stop("Arguments are 1-6")
}


pdf_layout <- c(2,4)
source("compare_vars_depth7.R")
