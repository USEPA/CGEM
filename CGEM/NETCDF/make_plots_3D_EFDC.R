#type "Rscript make_plots_3D_EFDC.R cgem" or "Rscript make_plots_3D_EFDC.R wqem"
#(or else it does cgem)

args = commandArgs(trailingOnly = TRUE)

if(length(args) == 0)
{
   cat("If it doesn't work, specify cgem or wqem at the command line.\n")

} else if (length(args) == 1)
{
    if(args[1] == "wqem")
    {
         which_eqs <- "wqem"
         ncfile <- "wqem.000000.nc"
         pdfname <- "wqem_3D.efdc.pdf"
         pdf_layout <- c(4,4) 
    } else 
    {
         which_eqs <- "cgem"
         ncfile <- "cgem.000000.nc"
         pdfname <- "cgem_3D.efdc.pdf"
         pdf_layout <- c(4,4)
     }

}

source("allvars_3D_EFDC.R")
