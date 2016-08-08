## This R script assumes a 5 minute time step
## To use:
## Open R: on iris type "R"
## On sol, type:
## module add intel
## module add R
## R
## Then, in R, type:
## source ("RFishTank.R")

pdf("FishTank_compare.pdf"); #This will produce a pdf called "FishTank.pdf"

par(pch=".",cex=3);


rdata1 <- read.table("./Athresh=1.67e7/A1.txt");
rdata2 <- read.table("./Athresh=7e7/A1.txt");
rdata3 <- read.table("./Athresh=9e7/A1.txt");
rdata4 <- read.table("./Athresh=1.67e8/A1.txt");
rdata5 <- read.table("./Athresh=1.67e9/A1.txt");
plot(rdata1[,1]/288.,log10(rdata1[,2]),xlab="days",ylab="Log10 A1",ylim=c(6,10));
lines(rdata2[,1]/288.,log10(rdata2[,2]),col="red");
lines(rdata3[,1]/288.,log10(rdata3[,2]),col="blue");
lines(rdata4[,1]/288.,log10(rdata4[,2]),col="green");
lines(rdata5[,1]/288.,log10(rdata5[,2]),col="purple");

dev.off();

