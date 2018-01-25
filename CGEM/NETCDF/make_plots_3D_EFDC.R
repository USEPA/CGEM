#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
cat("If it doesn't work, specify cgem or gomdom at the command line.\n")
} else if (length(args)==1){

if(args[1]=="gomdom"){
 which_eqs <- "gomdom"
 ncfile <- "gomdom.000000.nc"
 pdfname <- "gomdom_3D.pdf"
 pdf_layout <- c(4,4) 
} else {
 which_eqs <- "cgem"
 ncfile <- "cgem.000000.nc"
 pdfname <- "cgem_3D.pdf"
 pdf_layout <- c(4,4)
}

}

#pdf_layout <- c(2,1)

source("allvars_3D_EFDC.R")
#source("twovars_3D_EFDC.R")
