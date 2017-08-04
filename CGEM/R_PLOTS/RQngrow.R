## To use:
## source ("RQngrow.R")

pdf("Qngrow.pdf");

par(pch=".",cex=3);


par(mfrow=c(4,4));              #page with 4x4 plots

rdata <- read.table("Qngrowk1.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Ntotal");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="PO4");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="pn");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="pp");

plot(rdata[,1]/288.,(rdata[,6]),xlab="days",ylab="Qnn_a");

plot(rdata[,1]/288.,(rdata[,7]),xlab="days",ylab="Qpp_a");

plot(rdata[,1]/288.,(rdata[,8]),xlab="days",ylab="uA");

plot(rdata[,1]/288.,(rdata[,9]),xlab="days",ylab="pn - Qnn_a(isp)*uA");

plot(rdata[,1]/288.,(rdata[,10]),xlab="days",ylab="pp - Qpp_a(isp)*uA");

plot(rdata[,1]/288.,(rdata[,11]),xlab="days",ylab="monodNA(isp)");

plot(rdata[,1]/288.,(rdata[,12]),xlab="days",ylab="monodPA(isp)");

dev.off();

