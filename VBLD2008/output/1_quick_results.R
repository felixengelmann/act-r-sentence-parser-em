# rm(list=ls())
library(ggplot2)

myTheme <- theme(axis.text.x = element_text(angle = 45, hjust=1))


# READ SIMULATION DATA
# ---------------------------
f <- read.table("fixations.txt", header=FALSE)
colnames(f) <- c("exp","iteration","cond","pos","word","dur")
f$cond <- factor(f$cond)
head(f)
summary(f)


# READ ADDITIONAL INFO
# ---------------------------
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




# ============================
# PLOTS
# ============================

words <- unique(with(m[!is.na(m$word),], paste(pos,word)))
m <- subset(m, !is.na(word))

pdf("quick_results.pdf")


# SCANPATHS
# ---------------------------

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
t1$pos <- factor(t1$pos)
t1$index <- factor(t1$index)
(psc <- ggplot(t1, aes(index, pos, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() 
	+ scale_y_discrete(labels=words) 
	+ ggtitle(i))


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
t1$pos <- factor(t1$pos)
# t1$index <- factor(t1$index)
(ggplot(t1, aes(index, pos, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() + facet_grid(.~iteration) +  theme(legend.position="bottom"))



# OTHER PLOTS
# ---------------------------
dodge <- position_dodge(width=.3)

## PLOT ATTACHMENT TIMES ##
(ggplot(m, aes(factor(pos), AT, col=cond, group=cond)) 
	+ stat_summary(fun.y="mean", geom="line", position=dodge) 
	+ stat_summary(fun.data="mean_cl_boot", position=dodge, size=.2)
	+ myTheme
	+ scale_x_discrete(labels=words) 
	+ ggtitle("Attachment time")
	)

## PLOT ENCODING TIMES ##
(ggplot(m, aes(factor(pos), ET, col=cond, group=cond)) 
	+ stat_summary(fun.y="mean", geom="line", position=dodge) 
	+ stat_summary(fun.data="mean_cl_boot", position=dodge, size=.2)
	+ myTheme
	+ scale_x_discrete(labels=words) 
	+ ggtitle("Encoding time"))

## PLOT READING TIMES ##
(ggplot(subset(m, dur!=0), aes(factor(pos), dur, group=cond, col=cond))
	+ stat_summary(fun.y="mean", geom="line", position=dodge) 
	+ stat_summary(fun.data="mean_cl_boot", position=dodge, size=.2)
	+ myTheme
	+ scale_x_discrete(labels=words) 
	+ ggtitle("Reading time"))

## PLOT SKIPPING RATES ##
(ggplot(m, aes(factor(pos), skip, group=cond, col=cond))
	+ stat_summary(fun.y="mean", geom="line", position=dodge) 
	+ stat_summary(fun.data="mean_cl_boot", position=dodge, size=.2)
	+ myTheme
	+ scale_x_discrete(labels=words) 
	# + coord_cartesian(ylim=c(-0.01,1)) 
	+ ggtitle("Skipping rate"))

## PLOT TIMEOUTS ##
(ggplot(m, aes(factor(pos), timeout, group=cond, col=cond))
	+ stat_summary(fun.y="mean", geom="line", position=dodge) 
	+ stat_summary(fun.data="mean_cl_boot", position=dodge, size=.2)
	+ myTheme
	+ scale_x_discrete(labels=words) 
	+ ggtitle("Time-out rate"))


## PLOT FAILURE RATES ##
(ggplot(m, aes(cond, fail, group=cond, fill=cond))
	+ stat_summary(fun.y="mean", geom="bar", position=dodge) 
	+ stat_summary(fun.data="mean_cl_boot", position=dodge, geom="errorbar", width=.2)
 	# + coord_cartesian(ylim=c(-0.01,1)) 
	+ ggtitle("Failure rate"))




dev.off()
