#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){

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


}

source("selectedvars_0D.R")
