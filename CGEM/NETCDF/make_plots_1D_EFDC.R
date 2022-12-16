#type "Rscript make_plots_1D_EFDC.R cgem" or "Rscript make_plots_1D_EFDC.R gomdom"
#(or else it does cgem)

args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
        cat("If it doesn't work, specify cgem or gomdom at the command line.\n")
} else if (length(args) == 1) {
        if(args[1] == "gomdom"){
                which_eqs <- "gomdom"
                ncfile <- "gomdom.000000.nc"
                pdfname <- "gomdom_1D_EFDC.pdf"
                pdf_layout <- c(4,4) 
        } else {
                which_eqs <- "cgem"
                ncfile <- "cgem.000000.nc"
                pdfname <- "cgem_1D_EFDC.pdf"
                pdf_layout <- c(4,4)
        }
}

source("allvars_1D_EFDC.R")
