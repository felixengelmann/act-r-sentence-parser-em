rm(list=ls())
# library(em2)
library(reshape)
library(ggplot2)

## Function for computing confidence intervals
ci <- function (x) 
{
    m <- mean(x, na.rm = TRUE)
    n <- length(x[!is.na(x)])
    s <- sd(x, na.rm = TRUE)
    upper <- m + qt(0.975, df = n - 1) * (s/sqrt(n))
    lower <- m + qt(0.025, df = n - 1) * (s/sqrt(n))
    return(data.frame(lower = lower, upper = upper))
}

##------------------------------------------------------------
## READ SIMULATION DATA
##------------------------------------------------------------
f <- read.table("fixations.txt", header=FALSE)
colnames(f) <- c("exp","iteration","cond","pos","word","dur")
f$cond <- factor(f$cond)
head(f)
summary(f)


##------------------------------------------------------------
## READ ADDITIONAL INFO
##------------------------------------------------------------
m <- f

## ATTACHMENT TIMES:
att <- read.table("attachments.txt", header=FALSE)
colnames(att) <- c("exp","iteration","cond","pos","word","AT")
att$cond <- factor(att$cond)
dim(m)
m <- merge(m[-5], att, by=c("exp","iteration","cond","pos"), all.x=TRUE, all.y=TRUE)
dim(m)


## ENCODING TIMES:
enc <- read.table("enctimes.txt", header=FALSE)
colnames(enc) <- c("exp","iteration","cond","pos","word","ET","ecc","freq")
enc$cond <- factor(enc$cond)
enc$iteration <- factor(enc$iteration)
dim(m)
m <- merge(m, enc[-5], by=c("exp","iteration","cond","pos"), all.x=TRUE)
dim(m)

## TRIAL MESSAGES
msg <- read.table("trialmessages.txt", header=F)
colnames(msg) <- c("exp","iteration","cond","pos","word","variable","value")

trialinfo <- subset(msg[,-c(4,5)], variable!="timeout")
trialinfo <- reshape(trialinfo, idvar = c("exp","iteration","cond"), timevar="variable", direction="wide")
colnames(trialinfo) <- gsub("value.","", colnames(trialinfo))
trialinfo <- droplevels(trialinfo)

## Merge with EM results
dim(m)
m <- merge(m, trialinfo, by=c("exp","iteration","cond"), all.x=TRUE)
m$fail <- ifelse(is.na(m$fail), 0, 1)
dim(m)

## TIMEOUTS:
tmo <- droplevels(subset(msg, variable=="timeout"))
colnames(tmo)[6:7] <- c("timeout", "tmo-eyloc")
tmo$timeout <- 1
dim(m)
m <- merge(m, tmo[,-5], by=c("exp","iteration","cond","pos"), all.x=TRUE)
dim(m)
m$timeout[is.na(m$timeout)] <- 0

## SKIPPINGS ##
m$dur[is.na(m$dur)] <- 0
m$skip <- 0
m$skip[m$dur==0] <- 1




##------------------------------------------------------------
## MEANS
##------------------------------------------------------------


## FAILURES
fail <- cast(m, cond~., function(x) c(M=mean(x), N=length(x)), value="fail")
fail$SE <- sqrt(fail$M*(1-fail$M))/sqrt(fail$N)
fail$CI.lower <- fail$M + qt(.025, df=fail$N-1) * fail$SE
fail$CI.upper <- fail$M + qt(.975, df=fail$N-1) * fail$SE
colnames(fail)[2] <- "fail"

m <- subset(m, pos!=1 & pos!=6 & !is.na(word))

## ATT
at <- cast(att, cond+pos+word~., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x))), value="AT")
colnames(at)[4] <- "AT"
## ENC
et <- cast(enc, cond+pos+word~., mean, value="ET")
colnames(et)[4] <- "ET"
## RT
rt <- cast(subset(m, dur!=0 & !is.na(word)), cond+pos+word~., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x))), value="dur")
colnames(rt)[4] <- "RT"
# merge
rt <- merge(rt[-3], at, by=c("cond","pos"), all.x=TRUE, all.y=TRUE)
rt$RT[is.na(rt$RT)] <- 0
# sort rows of dataframe
rt <- rt[with(rt, order(cond,pos)), ]
## SKIP
skip <- cast(m, cond+pos+word~., function(x) c(M=mean(x), N=length(x)), value="skip")
skip$SE <- sqrt(skip$M*(1-skip$M))/sqrt(skip$N)
skip$CI.lower <- skip$M + qt(.025, df=skip$N-1) * skip$SE
skip$CI.upper <- skip$M + qt(.975, df=skip$N-1) * skip$SE
colnames(skip)[4] <- "skip"

## Timeout
to <- cast(m, cond+pos+word~., function(x) c(M=mean(x), N=length(x)), value="timeout")
to$SE <- sqrt(to$M*(1-to$M))/sqrt(to$N)
to$CI.lower <- to$M + qt(.025, df=to$N-1) * to$SE
to$CI.upper <- to$M + qt(.975, df=to$N-1) * to$SE
colnames(to)[4] <- "timeout"



##------------------------------------------------------------
## PLOTS
##------------------------------------------------------------
dodge <- position_dodge(width=.9)

pdf("quick-analysis.pdf")

## PLOT ATTACHMENT TIMES ##
(pa <- ggplot(at, aes(factor(pos), AT, fill=cond)) + geom_bar(stat="identity", position=dodge) + scale_x_discrete(labels=at$word) + geom_errorbar(aes(max=AT+2*SE, min=AT-2*SE, width=0), position=dodge) + ggtitle("Attachment time"))
# ggsave(pa, file="quick-plot-attachment-times.pdf")

## PLOT ENCODING TIMES ##
(pe <- ggplot(et, aes(factor(pos), ET, fill=cond)) + geom_bar(stat="identity", position=dodge) + scale_x_discrete(labels=et$word) + ggtitle("Encoding time"))
# ggsave(pe, file="quick-plot-encoding-times.pdf")

## PLOT READING TIMES ##
(pr <- ggplot(rt, aes(factor(pos), RT, group=cond, fill=cond)) + geom_bar(stat="identity", position=dodge) + scale_x_discrete(labels=rt$word) + geom_errorbar(aes(max=RT+2*SE.x, min=RT-2*SE.x, width=0), position=dodge) + ggtitle("Reading time"))
# ggsave(pr, file="quick-plot-reading-times.pdf")

## PLOT SKIPPING RATES ##
(ps <- ggplot(skip, aes(factor(pos), skip, group=cond, fill=cond)) + geom_bar(stat="identity", position=dodge) + scale_x_discrete(labels=skip$word) + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0), position=dodge) + coord_cartesian(ylim=c(-0.01,1)) + ggtitle("Skipping rate"))
# ggsave(ps, file="quick-plot-skipping-rate.pdf")

## PLOT TIMEOUTS ##
(ps <- ggplot(to, aes(factor(pos), timeout, group=cond, fill=cond)) + geom_bar(stat="identity", position=dodge) 
	# + ylim(0,1) 
	+ scale_x_discrete(labels=to$word) + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0), position=dodge) + coord_cartesian(ylim=c(-0.01,1)) + ggtitle("Time-out rate"))
# ggsave(ps, file="quick-plot-timeout-rate.pdf")

## PLOT FAILURE RATES ##
(pf <- ggplot(fail, aes(cond, fail, group=cond, fill=cond)) + geom_bar(stat="identity", position=dodge) + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0), position=dodge) + coord_cartesian(ylim=c(-0.01,1)) + ggtitle("Failure rate"))
# ggsave(pf, file="quick-plot-failure-rate.pdf")



##------------------------------------------------------------
## SCANPATHS
##------------------------------------------------------------
## PLOT SCANPATH OF RANDOM TRIAL ##
i <- sample(unique(f$iteration),1)
t1 <- subset(f, iteration%in%i)
t1$index <- NA
for(i in unique(t1$iteration)){
	for(c in t1$cond){
		s <- (t1$iteration==i & t1$cond==c)
		t1$index[s] <- 1:length(t1$pos[s])
	}
}
(psc <- ggplot(t1, aes(index, pos, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() 
	+ scale_y_discrete(labels=min(t1$pos):max(t1$pos)) 
	+ ggtitle(i))
# ggsave(psc, file="quick-plot-scanpath.pdf")


## PLOT SCANPATH OF 7 RANDOM TRIALS ##
i <- sample(unique(f$iteration),7)
t1 <- subset(f, iteration%in%i)
t1$index <- NA
for(i in unique(t1$iteration)){
	for(c in t1$cond){
		s <- (t1$iteration==i & t1$cond==c)
		t1$index[s] <- 1:length(t1$pos[s])
	}
}
(psc7 <- ggplot(t1, aes(index, pos, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() + facet_grid(.~iteration) + scale_y_discrete(labels=min(t1$pos):max(t1$pos)) + theme(legend.position="bottom"))
# ggsave(psc7, file="quick-plot-scanpaths7.pdf", width=13, height=6)

dev.off()
