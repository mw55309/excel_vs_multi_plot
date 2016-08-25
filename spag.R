# set up the data

df <- data.frame(Circulatory=c(32,26,19,16,14,13,11,11),
		 Mental=c(11,11,18,24,23,24,26,23),
		 Musculoskeletal=c(17,18,13,16,12,18,20,26),
		 Cancer=c(10,15,15,14,16,16,14,14))

rownames(df) <- seq(1975,2010,by=5)


# excel style plot
png("excel_plot.png", width=1000, height=600)
cols <- c("darkolivegreen3","darkcyan","mediumpurple2","coral3")
pch <- c(17,18,8,15)

xmax <- nrow(df) + 2.5

par(mar=c(3,3,0,0))
plot(1:nrow(df), 1:nrow(df), pch="", xlab=NA, ylab=NA, xaxt="n", yaxt="n", ylim=c(0,35), bty="n", xlim=c(1,xmax))

for (i in seq(0,35,by=5)) {
	lines(1:nrow(df), rep(i,nrow(df)), col="grey")
}

for (i in 1:ncol(df)) {

	points(1:nrow(df), df[,i], pch=pch[i], col=cols[i], cex=1.5)
	lines(1:nrow(df), df[,i], col=cols[i], lwd=4)


}

axis(side=1, at=1:nrow(df), tick=FALSE, labels=rownames(df))
axis(side=1, at=seq(-0.5,8.5,by=1), tick=TRUE, labels=NA)

axis(side=2, at=seq(0,35,by=5), tick=TRUE, las=TRUE, labels=paste(seq(0,35,by=5),"%",sep=""))

legend(8.5,25,legend=colnames(df), pch=pch, col=cols, cex=1.5, bty="n",  lwd=3, lty=1)
dev.off()

# split plot
png("multi_plot.png", width=1000, height=600)
split.screen(c(2,2))

scr <- 1
for (i in 1:ncol(df)) {
	screen(scr)

	# empty plot
	par(mar=c(3,2,1,1))
	plot(1:nrow(df), 1:nrow(df), pch="", xlab=NA, ylab=NA, xaxt="n", yaxt="n", ylim=c(0,35), bty="n")

	# plot all in grey
	for (j in 1:ncol(df)) {
		lines(1:nrow(df), df[,j], col="grey", lwd=3)

	}	

	# plot selected in blue
	lines(1:nrow(df), df[,i], col="blue4", lwd=4)

	# add blobs
	points(c(1,nrow(df)), c(df[1,i], df[nrow(df),i]), pch=16, cex=2, col="blue4")

	# add numbers
	mtext(df[1,i], side=2, at=df[1,i], las=2)
	mtext(df[nrow(df),i], side=4, at=df[nrow(df),i], las=2)	

	# add title
	title(colnames(df)[i])

	if (scr >= 3) {
		axis(side=1, at=1:nrow(df), tick=FALSE, labels=rownames(df))
	}

	scr <- scr + 1
}


close.screen(all=TRUE)
dev.off()

