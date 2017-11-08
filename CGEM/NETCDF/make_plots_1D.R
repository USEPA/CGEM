#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
 which_eqs <- "cgem"
 ncfile <- "cgem.000000.nc"
 pdfname <- "cgem_1D.pdf"
 pdfname2 <- "cgem_depth.pdf"
 pdf_layout <- c(4,4)

} else if (length(args)==1){

if(args[1]=="gomdom"){
 which_eqs <- "gomdom"
 ncfile <- "gomdom.000000.nc"
 pdfname <- "gomdom_1D.pdf"
 pdfname2 <- "gomdom_depth.pdf"
 pdf_layout <- c(4,4) 
} else {
 which_eqs <- "cgem"
 ncfile <- "cgem.000000.nc"
 pdfname <- "cgem_1D.pdf"
 pdfname2 <- "cgem_depth.pdf"
 pdf_layout <- c(4,4)
}

}

source("allvars_1D.R")

#pdfname <- pdfname2
#pdf_layout <- c(2,3) 
#source("allvars_1D_depth.R")
