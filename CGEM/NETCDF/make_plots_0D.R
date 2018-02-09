#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){

cat("R script that makes plots for CGEM-0D.\n")
cat("Usage: Rscript make_plots_0D.R [cgem or gomdom]\n")
cat("You have not specified a command line argument.\n")
cat("Default equation is cgem, default netCDF file is cgem.000000.nc, and default pdf is cgem_0D.pdf.\n")
 which_eqs <- "cgem"
 ncfile <- "cgem.000000.nc"
 pdfname <- "cgem_0D.pdf"
 pdf_layout <- c(4,4)

} else if (length(args)==1){

if(args[1]=="gomdom"){
 which_eqs <- "gomdom"
 ncfile <- "gomdom.000000.nc"
 pdfname <- "gomdom_0D.pdf"
 pdf_layout <- c(4,4) 
} else if(args[1]=="cgem") {
 which_eqs <- "cgem"
 ncfile <- "cgem.000000.nc"
 pdfname <- "cgem_0D.pdf"
 pdf_layout <- c(4,4)
} else if(args[1]=="lt") {
 which_eqs <- "cgem"
 ncfile <- "cgem_lt.nc"
 pdfname <- "cgem_lt.pdf"
 pdf_layout <- c(4,4)
} else if(
args[1]=="dk") {
 which_eqs <- "cgem"
 ncfile <- "cgem_dk.nc"
 pdfname <- "cgem_dk.pdf"
 pdf_layout <- c(4,4)
} else if(
args[1]=="ltnt") {
 which_eqs <- "cgem"
 ncfile <- "cgem_ltnt.nc"
 pdfname <- "cgem_ltnt.pdf"
 pdf_layout <- c(4,4)
} 

#To set equations, netCDF, pdf name, and/or layout to something
#other than default, put it here:

# which_eqs <- [ "cgem" or "gomdom" ]
# ncfile <- [ "cgem.000000.nc" or "gomdom.000000.nc" or "whatever.nc" ]
# pdfname <- [ "pdfname.pdf"]
# pdf_layout <- [ c(X,Y) ]


}

source("allvars_0D.R")
