## To use:
## source ("RFishTank_short.R")

log_ylim_k <- function(indata) {
 ymin <- log10(min(indata));
 ymax <- log10(max(indata));
 ymid <- ymin + (ymax-ymin)/2.;
 c(ymin-ymax*.001,ymax+ymax*.001)
}

log_get_lab <- function(indata){
 ymin <- log10(min(indata));
 ymax <- log10(max(indata));
 ymid <- ymin + (ymax-ymin)/2.
 ymin <- signif(ymin,digits=3)
 ymax <- signif(ymax,digits=3)
 ymid <- signif(ymid,digits=3)
 c(ymin,ymid,ymax)
}

ylim_k <- function(indata) {
 ymin <- (min(indata));
 ymax <- (max(indata));
 ymid <- ymin + (ymax-ymin)/2.
 c(ymin-ymax*.001,ymax+ymax*.001)
}

get_lab <- function(indata){
 ymin <- min(indata);
 ymax <- max(indata);
 ymid <- ymin + (ymax-ymin)/2.
 ymin <- signif(ymin,digits=3)
 ymax <- signif(ymax,digits=3)
 ymid <- signif(ymid,digits=3)
 c(ymin,ymid,ymax)
}

get_lab_4 <- function(indata){
 ymin <- min(indata);
 ymax <- max(indata);
 ymid <- ymin + (ymax-ymin)/2.
 ymin <- signif(ymin,digits=4)
 ymax <- signif(ymax,digits=4)
 ymid <- signif(ymid,digits=4)
 c(ymin,ymid,ymax)
}




pdf("FishTank_short.pdf");

par(pch=".",cex=3,oma=c(0,0,3,0));#,cex.axis=.75);


par(mfrow=c(4,4));              #page with 4x4 plots

rdata  <- read.table("A1.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 A1, k1",ylim=log_ylim_k(rdata[,2]));
axis(2, at=log_get_lab(rdata[,2]), labels=log_get_lab(rdata[,2]), las=2);


#-------------
rdata  <- read.table("Qn1.txt");

plot(rdata[,1]/288.,(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Qn1, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#---------------------------------------------
rdata  <- read.table("Qp1.txt");

plot(rdata[,1]/288.,(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Qp1, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#----------------------------------------
rdata  <- read.table("G1.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 G1, k1",ylim=log_ylim_k(rdata[,2]));
axis(2,at=log_get_lab(rdata[,2]), labels=log_get_lab(rdata[,2]), las=2);

#---------------------------------------
rdata  <- read.table("G2.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 G2, k1",ylim=log_ylim_k(rdata[,2]));
axis(2,at=log_get_lab(rdata[,2]), labels=log_get_lab(rdata[,2]), las=2);

#-----------------------------------
rdata  <- read.table("NO3.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="NO3, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#-------------------------------
rdata  <- read.table("NH4.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="NH4, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#------------------------------------------
rdata  <- read.table("PO4.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="PO4, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#----------------------------
rdata  <- read.table("DIC.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="DIC, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab_4(rdata[,2]), labels=get_lab_4(rdata[,2]), las=2);

#--------------------------------
rdata  <- read.table("O2.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="O2, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#--------------------
rdata  <- read.table("OM1_A.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM1_A, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#------------------------
rdata  <- read.table("OM2_A.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM2_A, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#--------------------------
rdata  <- read.table("OM1_fp.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM1_fp, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#-----------------------------------
rdata  <- read.table("OM2_fp.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM2_fp, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#----------------------------------
rdata  <- read.table("CDOM.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="CDOM, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#----------------------------------
rdata  <- read.table("Si.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="Si, k1",ylim=ylim_k(rdata[,2]));
axis(2,at=get_lab(rdata[,2]), labels=get_lab(rdata[,2]), las=2);

#----------------------------------
label <- scan("label.txt",what=character());
mtext(label,outer=TRUE);

dev.off();
