#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
cat("If it doesn't work, specify cgem or gomdom at the command line.\n")
} else if (length(args)==1){

if(args[1]=="gomdom"){
 which_eqs <- "gomdom"
 ncfile <- "gomdom.000000.nc"
 ncfile2 <- "gd_wind=5.nc"
 pdfname <- "gd_doc.pdf"
 pdf_layout <- c(4,4) 
} else {
 which_eqs <- "cgem"
 ncfile <- "cgem.000000.nc"
 ncfile2 <- "G07_wind=5.000000.nc"
 pdfname <- "cgem.doc.pdf"
 pdf_layout <- c(4,4)
}

}

source("compare_vars_1D.R")
if(args[1]=="gomdom"){
pdfname <- "gomdom_pdiff_doc.pdf"
}else{
pdfname <- "cgem_pdiff_doc.pdf"
}
source("compare_vars_sub_1D.R")
