#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
cat("If it doesn't work, specify cgem or gomdom at the command line.\n")
} else if (length(args)==1){

if(args[1]=="gomdom"){
 which_eqs <- "gomdom"
 ncfile <- "gomdom.000000.nc"
 ncfile2 <- "gomdom.noflux.nc"
 pdfname <- "gomdom.w_woflux.pdf"
 pdf_layout <- c(4,4) 
} else {
 which_eqs <- "cgem"
 ncfile <-  "cgem.000000.nc"
 ncfile2 <- "cgem.noflux.nc"
 pdfname <- "cgem.w_woflux.pdf"
 pdf_layout <- c(4,4)
}

}

source("compare_vars_1D.R")
if(args[1]=="gomdom"){
pdfname <- "gomdom_pdiff.pdf"
pdfname2 <- "gd.w_woflux_depth.pdf"
}else{
pdfname <- "cgem_pdiff.pdf"
pdfname2 <- "cgem.w_woflux_depth.pdf"
}
source("compare_vars_sub_1D.R")

pdfname <- pdfname2
pdf_layout <- c(2,3)
source("compare_vars_depth.R")
