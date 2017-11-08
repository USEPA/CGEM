#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
cat("If it doesn't work, specify cgem or gomdom at the command line.\n")
} else if (length(args)==1){

if(args[1]=="gomdom"){
 which_eqs <- "gomdom"
 ncfile <- "monod.nc"
 ncfile2 <- "newpars_droop.nc"
 pdfname <- "monod_vs_newparsdroop.pdf"
 pdf_layout <- c(4,4) 
} else {
 which_eqs <- "cgem"
 ncfile <-  "original.nc"
 ncfile2 <- "cgem.000000.nc"
 pdfname <- "cgem.compare.pdf"
 pdf_layout <- c(4,4)
}

}

source("compare_vars_1D.R")
if(args[1]=="gomdom"){
pdfname <- "monod_vs_newparmsdroop_pdiff.pdf"
pdfname1 <- "gd_depth.pdf"
}else{
pdfname <- "cgem_pdiff.pdf"
pdfname2 <- "cgem_depth.pdf"
}
source("compare_vars_sub_1D.R")

pdfname <- pdfname2
pdf_layout <- c(2,3)
source("compare_vars_depth.R")
