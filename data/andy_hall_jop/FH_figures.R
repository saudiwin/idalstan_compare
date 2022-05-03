###### Graphics code for Feigenbaum and Hall
###### This code produces all of the non-map figures in the body of the paper

setwd("~/Dropbox/Trade-Elections/Replication")
library(foreign)


#######################
#### Figure 1 #########
#######################

data <- read.dta("fh_final_analysis.dta")
data <- data[data$decade==1,]
data <- na.omit(data)
attach(data)

cols <- vector(length=length(dem_share))
for (i in 1:length(dem_share)) {
	cols[i] <- rgb((255-(230*dem_share[i])), 0, (0+230*dem_share[i]), maxColorValue=256, alpha=120)
}

pdf(file="Figure_3.pdf", height=5.5, width=11)
par(mfrow=c(1,2), mar=c(5,1,3,0), oma=c(3,3,3,3))

plot(x=nontradescore, y=tradescore, col=cols, pch=16, main="Legislator Voting Behavior, 1990s", xlab="Non-Trade Score", ylab="Trade Score", yaxt="n", xlim=c(-18, 13))
axis(side=2, las=1)
mtext(side=2, "Trade Score", line=2.5)

data <- read.dta("fh_final_analysis.dta")
data <- data[data$decade==2,]
data <- na.omit(data)
attach(data)

cols <- vector(length=length(dem_share))
for (i in 1:length(dem_share)) {
	cols[i] <- rgb((255-(230*dem_share[i])), 0, (0+230*dem_share[i]), maxColorValue=256, alpha=120)
}
plot(x=nontradescore, y=tradescore, col=cols, pch=16, main="Legislator Voting Behavior, 2000s", yaxt="n", ylab="", xlab="Non-Trade Score")
dev.off()

#######################
#### Figure 2 #########
#######################

pdf(file="Figure_2.pdf", height=5, width=11)
par(mfrow=c(1,2), mar=c(5,3,3,0), oma=c(3,3,3,3))
data <- read.dta("fh_final_analysis.dta")
data <- data[data$decade==1,]
data <- na.omit(data)
attach(data)
reg1 <- lm(tradescore ~ pro_ft_barrier)
plot(x=pro_ft_barrier, y=tradescore, yaxt="n", cex=.7, main="1990s", xlab="Cato Score", pch=16, col="gray40", ylim=c(-60, 50))
abline(reg1, col="red")
axis(side=2, las=1)
mtext(side=2, "Trade Score", line=2.5)

data <- read.dta("fh_final_analysis.dta")
data <- data[data$decade==2,]
data <- na.omit(data)
attach(data)
reg1 <- lm(tradescore ~ pro_ft_barrier)
plot(x=pro_ft_barrier, y=tradescore, yaxt="n", cex=.7, ylab="", main="2000s", xlab="Cato Score", pch=16, col="gray40", ylim=c(-60, 50))
abline(reg1, col="red")
dev.off()

#######################
#### Figure 5 #########
#######################

data <- read.dta("fh_final_analysis.dta")
data.one <- data[data$decade==1,]
data.one <- na.omit(data.one)
data.two <- data[data$decade==2,]
data.two <- na.omit(data.two)

pdf(file="Figure_5.pdf", width=11, height=6)
par(mfrow=c(1,2), mar=c(5,3,5,.5))
# first graph, needs axis label
cols <- vector(length=length(data.one$dem_share))
for (i in 1:length(data.one$dem_share)) {
	cols[i] <- rgb((255-(230*data.one$dem_share[i])), 0, 	(0+230*data.one$dem_share[i]), maxColorValue=256, alpha=120)
}
names <- paste(data.one$state, "-", data.one$dist, sep="")
plot(x=data.one$z, y=data.one$x, cex=.4, col="white", main="First Stage, 1990s", xlab="Import Exposure Per Worker, Other Countries", ylab="", xaxt="n", xlim=c(0,4))
text(x=data.one$z, y=data.one$x, labels=names, cex=.9, col="gray20")
my.lm <- lm(data.one$x~data.one$z)
lines(x=data.one$z, y=my.lm$fitted.values, lwd=2)
axis(side=1)
mtext(side=2, "Import Exposure Per Worker, USA", line=2)

cols <- vector(length=length(data.two$dem_share))
for (i in 1:length(data.two$dem_share)) {
	cols[i] <- rgb((255-(230*data.two$dem_share[i])), 0, 	(0+230*data.two$dem_share[i]), maxColorValue=256, alpha=120)
}
names <- paste(data.two$state, "-", data.two$dist, sep="")
plot(x=data.two$z, y=data.two$x, cex=.4, col="white", main="First Stage, 2000s", xlab="Import Exposure Per Worker, Other Countries", ylab="", xaxt="n", ylim=c(0,14))
text(x=data.two$z, y=data.two$x, labels=names, cex=.9, col="gray20")
my.lm <- lm(data.two$x~data.two$z)
lines(x=data.two$z, y=my.lm$fitted.values, lwd=2)
axis(side=1)
dev.off()


#######################
#### Figure 6 #########
#######################

data <- read.csv("tradescores_for_r.csv")

pdf(file="Figure_6.pdf", height=5, width=9)
par(mfrow=c(1,2))
lm.trade <- lm(data$tradescore ~ data$fits)

plot(x=data$fits, y=data$tradescore, pch=16, ylim=c(-11, 0), xlab="Instrumented Import Exposure", yaxt="n", ylab="", col="darkred")
abline(lm.trade)
axis(side=2, las=1)
mtext(side=2, line=2.5, "Trade Score")

data <- read.csv("cato_for_r.csv")
lm.cato <- lm(data$pro_ft_barrier ~ data$fits)

plot(x=data$fits, y=data$pro_ft_barrier, pch=16, ylim=c(-12, 5), xlab="Instrumented Import Exposure", yaxt="n", ylab="", col="darkred")
axis(side=2, las=1)
mtext(side=2, line=2.5, "Cato Score")
abline(lm.cato)

dev.off()


#######################
#### Figure 7 #########
#######################

pdf(file="Figure_7.pdf", height=5, width=9)
par(mfrow=c(1,2))
data <- read.csv("tradeint.csv")
lm.trade1 <- lm(data$tradescore_by1 ~ data$fits_by1)
lm.trade2 <- lm(data$tradescore_by2 ~ data$fits_by2)
plot(x=data$fits_by1, y=data$tradescore_by1, col="darkgreen", xlab="Instrumented Import Exposure", ylab="", yaxt="n", pch=1)
abline(lm.trade1, col="darkgreen", lty=2)
points(x=data$fits_by2, y=data$tradescore_by2, col="darkred", pch=16)
abline(lm.trade2, col="darkred")
text(x=3.7, y=-10.3, "Competitive \n Districts", col="darkred")
text(x=4, y=-6.2, "Safe \n Districts", col="darkgreen")
axis(side=2, las=1)
mtext(side=2, line=2.5, "Trade Score")

data <- read.csv("catoint.csv")
lm.trade1 <- lm(data$pro_ft_barrier_by1 ~ data$fits_by1)
lm.trade2 <- lm(data$pro_ft_barrier_by2 ~ data$fits_by2)
plot(x=data$fits_by1, y=data$pro_ft_barrier_by1, col="darkgreen", xlab="Instrumented Import Exposure", ylab="", yaxt="n", pch=1, ylim=c(-12,4))
abline(lm.trade1, col="darkgreen", lty=2)
points(x=data$fits_by2, y=data$pro_ft_barrier_by2, col="darkred", pch=16)
abline(lm.trade2, col="darkred")
mtext(side=2, line=2.5, "Cato Score")
axis(side=2, las=1)
legend("topright", pch=c(1,16), col=c("darkgreen", "darkred"), c("Safe Districts", "Competitive Districts"), cex=.8, lty=c(2,1))

dev.off()