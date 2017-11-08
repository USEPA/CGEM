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
 ncfile1<- "After_20min_tstep/cgem.000000.nc" #old code
 ncfile2 <- "After_divide_by_k/cgem.000000.nc" #new code
 ncfile3<-  "Compare_Cloern/cloern.nc"     #old code
 ncfile4 <- "Cloern_Run3/cgem.000000.nc"  #new code
 pdfname <- "compare4_all.pdf"
 pdf_layout <- c(4,4)
}

}

source("compare_vars_1D_4_all.R")
