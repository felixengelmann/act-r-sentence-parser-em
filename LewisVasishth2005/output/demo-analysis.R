library(reshape)
library(ggplot2)


## Read simulation data
f <- read.table("fixations.txt", header=T)
colnames(f) <- c("exp","iteration","cond","roi","word","dur")
# f <- subset(f,roi!=1)
head(f)
summary(f)

retr <- read.table("attachments.txt", header=TRUE)
colnames(retr) <- c("exp","iteration","cond","roi","word","retr")
# retr <- subset(retr,roi!=1)
head(retr)

## MEANS ##
## fix
f.roi <- cast(f, cond+roi+word~., mean, value="dur")
colnames(f.roi)[4] <- "RT"
## retr
retr.roi <- cast(retr, cond+roi+word~., mean, value="retr")
colnames(retr.roi)[4] <- "retr"
head(retr.roi)
## merge
f.roi <- merge(f.roi[,-3], retr.roi, by=c("cond","roi"), all.x=TRUE, all.y=TRUE)
f.roi$RT[is.na(f.roi$RT)] <- 0
# sort rows of dataframe
f.roi <- f.roi[with(f.roi, order(cond,roi)), ]
head(f.roi)
summary(f.roi)

## PLOT ATTACHMENT TIME ##
(p1 <- ggplot(f.roi, aes(factor(roi), retr, fill=cond)) + geom_bar(stat="identity", position="dodge") + scale_x_discrete(labels=f.roi$word))
ggsave(p1, file="plot-attachment-times.pdf")

## PLOT READING TIME ##
(p2 <- ggplot(f.roi, aes(factor(roi), RT, group=cond, fill=cond)) + geom_bar(stat="identity", position="dodge") + scale_x_discrete(labels=f.roi$word))
ggsave(p2, file="plot-reading-times.pdf")


## PLOT SCANPATH OF RANDOM TRIAL ##
f$index <- NA
for(i in unique(f$iteration)){
	for(c in f$cond){
		s <- (f$iteration==i & f$cond==c)
		f$index[s] <- 1:length(f$roi[s])
	}
}
# dev.new(width=10, height=2)

if(length(unique(f$iteration))>1) i <- sample(unique(f$iteration),1) else i <- unique(f$iteration)
f1 <- subset(f, iteration%in%i)
(p3 <- ggplot(f1, aes(index, roi, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() + scale_y_discrete(labels=f.roi$word[f.roi$cond==levels(f.roi$cond)[1]]) + ggtitle(i))
ggsave(p3, file=paste("plot-scanpath-",i,".pdf",sep=""))
# (p3 <- ggplot(f1, aes(index, roi, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() + scale_y_discrete(labels=as.vector(abbreviate(f.roi$word[f.roi$cond==levels(f.roi$cond)[1]]))) + coord_flip() + ggtitle(i))
# ggsave(p3, file=paste("plot-scanpath-",i,".pdf",sep=""), width=10, height=2)


## PLOT SCANPATH OF 7 RANDOM TRIALS ##
if(length(unique(f$iteration))>1){
	# dev.new(width=13, height=6)

	i <- sample(unique(f$iteration),7)
	f1 <- subset(f, iteration%in%i)
	(p4 <- ggplot(f1, aes(index, roi, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() + facet_grid(.~iteration) + scale_y_discrete(labels=f.roi$word[f.roi$cond==levels(f.roi$cond)[1]]) + theme(legend.position="bottom"))
	ggsave(p4, file="plot-scanpaths.pdf", width=13, height=6)
}

