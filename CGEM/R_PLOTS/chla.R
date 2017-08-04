## To use:
## source ("avgrad.R");

# Open a plot device
pdf("chla_rad.pdf");
#X11();
# default mgp = c(3, 1, 0)
# default mar = c( 5.1 4.1 4.1 2.1)
par(pch=".",cex.lab=1.5, mgp=c(2, 0.25, 0), tck=0.05);
par(mar=c(5.1, 4.2, 5, 4.5))
par(mfrow=c(2,2)); # Display plots in a grid, row first
# change placement of axis tics
par(xaxs="i", yaxs="i");
# Common settings used in plot statements
cex_y2_lab <- 1.1 # 2nd y axis label size
cex_y2_axis <- 1.1 # 2nd y axis value size

rdata1 <- read.table("Chla.txt");
rdata2 <- read.table("DailyRad.txt");

plot(rdata1[,1]/288.,rdata1[,2], main="k = 1", cex=2, col="blue", yaxt="n", xlab="Days", ylab="",
	ylim=c(0,35));
abline(h=c(10,20,30), col="lightgray");
axis(2, col="blue", col.axis="blue", las=1);
mtext("Chla", side=2, col="blue", line=2, cex=1.5);
par(new=T);
#plot(rdata2[,1]/288.,log10(rdata2[,2]), cex=2, axes=F, xlab="", ylab="", col="green");
plot(rdata2[,1]/288.,rdata2[,2], cex=2, axes=F, xlab="", ylab="", col="green");
axis(4, col="green", col.axis="green", las=1, cex.axis=cex_y2_axis);
#mtext(bquote(Log[10] ~ "Daily Rad"), side=4, col="green",line=2.75, cex=1.25);
mtext("Daily Rad", side=4, col="green",line=2.75, cex=cex_y2_lab);

plot(rdata1[,1]/288.,rdata1[,3], main="k = 6", cex=2, col="blue", yaxt="n", xlab="Days", ylab="",
	ylim=c(0,35));
abline(h=c(10,20,30), col="lightgray");
axis(2, col="blue", col.axis="blue", las=1);
mtext("Chla", side=2, col="blue", line=2, cex=1.5);
par(new=T);
#plot(rdata2[,1]/288.,log10(rdata2[,3]), cex=2, axes=F, xlab="", ylab="", col="green");
plot(rdata2[,1]/288.,rdata2[,3], cex=2, axes=F, xlab="", ylab="", col="green");
axis(4, col="green", col.axis="green", las=1, cex.axis=cex_y2_axis);
#mtext(bquote(Log[10] ~ "Daily Rad"), side=4, col="green",line=2.75, cex=1.25);
mtext("Daily Rad", side=4, col="green",line=2.75, cex=cex_y2_lab);

plot(rdata1[,1]/288.,rdata1[,4], main="k = 12", cex=2, col="blue", yaxt="n", xlab="Days", ylab="",
	ylim=c(0,35));
abline(h=c(10,20,30), col="lightgray");
axis(2, col="blue", col.axis="blue", las=1);
mtext("Chla", side=2, col="blue", line=2, cex=1.5);
par(new=T);
#plot(rdata2[,1]/288.,log10(rdata2[,4]), cex=2, axes=F, xlab="", ylab="", col="green");
plot(rdata2[,1]/288.,rdata2[,4], cex=2, axes=F, xlab="", ylab="", col="green");
axis(4, col="green", col.axis="green", las=1, cex.axis=cex_y2_axis);
#mtext(bquote(Log[10] ~ "Daily Rad"), side=4, col="green",line=2.75, cex=1.25);
mtext("Daily Rad", side=4, col="green",line=2.75, cex=cex_y2_lab);

plot(rdata1[,1]/288.,rdata1[,5], main="k = 20", cex=2, col="blue", yaxt="n", xlab="Days", ylab="",
	ylim=c(0,35));
abline(h=c(10,20,30), col="lightgray");
axis(2, col="blue", col.axis="blue", las=1);
mtext("Chla", side=2, col="blue", line=2, cex=1.5);
par(new=T);
#plot(rdata2[,1]/288.,log10(rdata2[,5]), cex=2, axes=F, xlab="", ylab="", col="green");
plot(rdata2[,1]/288.,rdata2[,5], cex=2, axes=F, xlab="", ylab="", col="green");
axis(4, col="green", col.axis="green", las=1, cex.axis=cex_y2_axis);
#mtext(bquote(Log[10] ~ "Daily Rad"), side=4, col="green",line=2.75, cex=1.25);
mtext("Daily Rad", side=4, col="green",line=2.75, cex=cex_y2_lab);


#-------------

# Close all plot devices
dev.off();
