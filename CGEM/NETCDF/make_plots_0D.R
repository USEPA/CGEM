#type "Rscript make_plots.R cgem" or "Rscript make_plots.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){

} else if (length(args)==1){

if(args[1]=="gomdom"){
 which_eqs <- "gomdom"
 ncfile <- "gomdom_0D.nc"
 pdfname <- "gomdom_0D.pdf"
 pdf_layout <- c(4,4) 
} else {
 which_eqs <- "cgem"
# ncfile <- "cgem_0D.nc"
 ncfile <- "output.000000.nc"
 pdfname <- "cgem_0D.pdf"
 pdf_layout <- c(4,4)
}

}

source("allvars_0D.R")

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

