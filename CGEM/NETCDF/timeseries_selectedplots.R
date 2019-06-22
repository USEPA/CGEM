## To use:
## source ("timeseries_plot.R")

# Starts a new plot of a 1D time series variable
timeseries_plot <- function(Var, time, rdata, unit, chlrd = 1.0, chlrg = 1.0, ilog = FALSE, 
                            label = NULL, range = NULL, uselim = TRUE, xrange = NULL, 
                            color = "black", linetype = "solid")
{

    if(ilog)
    {
       rdata <- log(rdata) # Might have 'log zero' error, need to add check
       logtxt <- "log of"

    } else
      {
	 logtxt <- ""
      }

    switch(Var,
	   "LOP" = {rdata <- rdata * 1.0e+06
		    unit = "P ug/L"
		    ylimit <- c(0.0, 10.0)},
	   "SRP" = {rdata <- rdata * 1.0e+06
		    unit = "P ug/L"
		    ylimit <- c(0.0, 20.0)},
           "TP"  = {rdata <- rdata * 1.0e+06
		    unit = "P ug/L"
		    ylimit <- c(0.0, 20.0)},
	   "GRE" = {rdata <- rdata * 1.0e+06
		    chlgdata <- rdata / chlrg
		    unit = "C ug/L"
		    ylimit <- c(0.0, 100.0)
		    unitchl = "Chl ug/L"
		    ylimitchl <- c(0.0, 5.0)},
           "ZOO" = {rdata <- rdata * 1.0e+06
		    unit = "C ug/L"
		    ylimit <- c(0.0, 20.0)},
           "PG"  = {unit = "1/s"
		    ylimit <- c(0.0, 1.0e-06)},
	   "IFD" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   "IFG" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   "TFD" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   "TFG" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   "PFD" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   "PFG" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   "NFD" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   "NFG" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   "SFD" = {unit = "Unitless"
		    ylimit <- c(0.0, 1.0)},
	   ylimit <- get_ylim(rdata, range)
    )

    # ylimit <- get_ylim(rdata, range)
    xlimit <- get_ylim(time, xrange)

    plot(time, rdata, yaxt = "n", type = "l", ylab = "", xlab = "", main = paste(Var,label), 
	 ylim = ylimit, xlim = xlimit, col = color, lty = linetype);

    mtext(paste(logtxt, unit), side = 3, line = 0.2, cex = 0.8)

    if (uselim)
    {
       if (Var == "LOP" || Var == "SRP" || Var == "GRE")
       {
	   axis(2, at = get_lab(ylimit), labels = get_lab2(ylimit), las=2)

       } else 
	 {
	   axis(2, at = get_lab(ylimit), labels = get_lab(ylimit), las=2)
	 }
    } else 
      {
	 axis(2, las=2)
      }

    if (Var == "GRE")
    {
       ## Allow a second plot on the same graph
       par(new = TRUE)

       ## Plot the second plot and put axis scale on right
       plot(time, chlgdata, yaxt = "n", type = "l", ylab = "", xlab = "", ylim = ylimitchl, 
	    axes = FALSE, col = "red", lty = linetype)
       mtext(paste(logtxt, unitchl), side = 4, line = 2, cex = 0.6, col = "red")
       axis(4, at = get_lab(ylimitchl), labels = get_lab2(ylimitchl), las=2, col = "red", col.axis = "red")
    }

}


# Adds lines to an existing plot, can change color but not labels or ranges (for comparisons)
timeseries_addlines <- function(Var,time,rdata,color="red",linewidth=1,linetype="solid")
{
   lines(time,rdata,col=color,lwd=linewidth,lty=linetype)
}


get_ylim <- function(indata,range) 
{
  ymin <- min(indata,na.rm=TRUE) 
  ymax <- max(indata,na.rm=TRUE)
  if (!is.null(range))
  {
     ymin <- range[1]
     ymax <- range[2]
  }
  c(ymin,ymax)
}

get_ylim_for2 <- function(indata,indata2,range) 
{
  ymin <- (min( min(indata), min(indata2) ));
  ymax <- (max( max(indata), max(indata2) ));
  if(!is.null(range))
  {
     ymin <- range[1]
     ymax <- range[2]
  }
  c(ymin,ymax)
}

get_lab <- function(ylimit)
{
  ymin <- ylimit[1] 
  ymax <- ylimit[2]
  ymid <- ymin + (ymax - ymin) / 2.0
  ymin <- signif(ymin, digits=4)
  ymax <- signif(ymax, digits=4)
  ymid <- signif(ymid, digits=4)
  c(ymin,ymid,ymax)
}

get_lab2 <- function(ylimit)
{
  ymin <- ylimit[1] 
  ymax <- ylimit[2]
  ymid <- ymin + (ymax - ymin) / 2.0
  ymin <- signif(ymin, digits=2)
  ymax <- signif(ymax, digits=2)
  ymid <- signif(ymid, digits=2)
  c(ymin,ymid,ymax)
}