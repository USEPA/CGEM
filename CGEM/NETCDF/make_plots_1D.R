#type "Rscript make_plots_1D.R cgem" or "Rscript make_plots_1D.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

#Usage:  Rscript make_plots_1D.R [wqm]

if(length(args)==0){
#Default, no arguments
 which_eqs <- "cgem"
 ncfile <- "cgem.000000.nc"
 pdfname <- "cgem_1D.pdf"
 pdfname2 <- "cgem_depth.pdf"
 pdf_layout <- c(4,4)

} else {

if(args[1]=="gomdom"){
#Default, argument=gomdom
 which_eqs <- "gomdom"
 ncfile <- "gomdom.000000.nc"
 pdfname <- "gomdom_1D.pdf"
 pdfname2 <- "gomdom_depth.pdf"
 pdf_layout <- c(4,4) 
} else {
#Default, arument=something other than gomdom
 which_eqs <- "cgem"
 ncfile <- "cgem.000000.nc"
 pdfname <- "cgem_1D.pdf"
 pdfname2 <- "cgem_depth.pdf"
 pdf_layout <- c(4,4)
}

}

#Can change values here:
# which_eqs <- "[cgem or gomdom]"
# ncfile <- "[name of output file.nc]"
# pdfname <- "[name of pdf].pdf"
# pdfname2 <- "[name for depth pdf].pdf"
# pdf_layout <- c(4,4)


#Plots a time series of every variable
source("allvars_1D.R")

#Plots depth profiles at various times
pdfname <- pdfname2
pdf_layout <- c(2,3) 
source("allvars_1D_depth.R")
