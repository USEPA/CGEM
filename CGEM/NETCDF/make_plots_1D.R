#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
cat("If it doesn't work, specify cgem or gomdom at the command line.\n")
} else if (length(args)==1){

if(args[1]=="gomdom"){
 which_eqs <- "gomdom"
 ncfile <- "gomdom_1D.nc"
 pdfname <- "gomdom_1D.pdf"
 pdf_layout <- c(4,4) 
} else {
 which_eqs <- "cgem"
 ncfile <- "cgem_1D.nc"
 pdfname <- "cgem_1D.pdf"
 pdf_layout <- c(4,4)
}

}

source("allvars_1D.R")

#Which run:
#which_eqs <- "cgem"
#which_eqs <- "gomdom"

#netcdf file:
#ncfile <- cgem_0D.nc
#ncfile <- gomdom_0D.nc

#firsts:
#don't use unless you know what this is...

#name of output pdf:
#pdfname <- "cgem_0D.pdf"
#pdfname <- "gomdom_0D.pdf"

#Layout of pdf, like 4x4 plots per page:
#pdf_layout <- c(4,4)
#pdf_layout <- c(2,2)

