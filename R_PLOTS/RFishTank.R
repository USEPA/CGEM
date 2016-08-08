## This R script assumes a 5 minute time step
## To use:
## Open R: on iris type "R"
## On sol, type:
## module add intel
## module add R
## R
## Then, in R, type:
## source ("RFishTank.R")

pdf("FishTank.pdf"); #This will produce a pdf called "FishTank.pdf"

par(pch=".",cex=3);

par(mfrow=c(4,4));              #page with 4x4 plots

rdata <- read.table("A1.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="Log10 A1, k1");

rdata <- read.table("A2.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="Log10 A2, k1");

rdata <- read.table("A3.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="Log10 A3, k1");

rdata <- read.table("A4.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="Log10 A4, k1");

rdata <- read.table("A5.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="Log10 A5, k1");

rdata <- read.table("A6.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="Log10 A6, k1");

rdata <- read.table("Qn1.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qn1, k1");

rdata <- read.table("Qn2.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qn2, k1");

rdata <- read.table("Qn3.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qn3, k1");

rdata <- read.table("Qn4.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qn4, k1");

rdata <- read.table("Qn5.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qn5, k1");

rdata <- read.table("Qn6.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qn6, k1");

rdata <- read.table("Qp1.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qp1, k1");

rdata <- read.table("Qp2.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qp2, k1");

rdata <- read.table("Qp3.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qp3, k1");

rdata <- read.table("Qp4.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qp4, k1");

par(mfrow=c(4,4));              #page with 4x4 plots

rdata <- read.table("Qp5.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qp5, k1");

rdata <- read.table("Qp6.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Qp6, k1");

rdata <- read.table("G1.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="Log10 G1, k1");

rdata <- read.table("G2.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="Log10 G2, k1");

rdata <- read.table("NO3.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="NO3, k1");

rdata <- read.table("NH4.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="NH4, k1");

rdata <- read.table("PO4.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="PO4, k1");

rdata <- read.table("DIC.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="DIC, k1");

rdata <- read.table("O2.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="O2, k1");

rdata <- read.table("OM1_A.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="OM1_A, k1");

rdata <- read.table("OM2_A.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="OM2_A, k1");

rdata <- read.table("OM1_fp.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="OM1_fp, k1");

rdata <- read.table("OM2_fp.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="OM2_fp, k1");

rdata <- read.table("OM1_rp.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="OM1_rp, k1");

rdata <- read.table("OM2_rp.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="OM2_rp, k1");

rdata <- read.table("CDOM.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="CDOM, k1");

par(mfrow=c(4,4));              #page with 4x4 plots

rdata <- read.table("Si.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="Si, k1");

rdata <- read.table("OM1_bc.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="OM1_bc, k1");

rdata <- read.table("OM2_bc.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="OM2_bc, k1");

rdata <- read.table("Chla.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="Chla, k1");

rdata <- read.table("DailyRad.txt");
plot(rdata[,1]/288.,rdata[,2],xlab="days",ylab="DailyRad, k1");


dev.off();

