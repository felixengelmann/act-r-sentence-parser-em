
require(em2)
require(ggplot2)
require(tidyr)
require(dplyr)
require(xtable)


# ===========================
# 00 Helper functions
# ===========================
rmsd <- function (obs, pred){
	sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

## DATA CLEAN-UP
data.cleanup <- function(m){
	m <- subset(m, pos!=1 & word!="*")
	m$trial <- paste(m$exp, m$iteration, m$cond)
	(n.trials <- length(unique(m$trial)))
	m$trialpos <- paste(m$exp, m$iteration, m$cond, m$pos)
	(length(unique(m$trialpos)))
  # head(m)
	m$dupl <- duplicated(m$trialpos)
	dim(subset(m,dupl))[1]
	m <- subset(m, !dupl)
	return(m)
}

myTheme <- theme(axis.text.x = element_text(angle = 45, hjust=1))

quickreport <- function(sets){
  p_1 <- subset(p, set%in%sets)
  p_2 <- cbind(p_1$set, p_1$rmsd, p_1$cor, p_1$score, p_1$effect1, p_1$effect2)
  for(i in 1:(length(params)-1)){
    p_2 <- cbind(p_2,params[[i]][p_1$set])
  }
  colnames(p_2) <- c("set","rmsd","cor","score","effect1","effect2",params$names)
  data.frame(p_2)
}

myxtable<-function(res,cap="Model fit.",lab="tab:modelfit"){
  print(xtable(res,caption=cap,label=lab),
        include.rownames=F)}

min.value <- function(d,measure, f=2, limit=12, cut=F){ ## FIND MIN VALUE
  print(bestrange <- c(min(d[,measure],na.rm=T), min(d[,measure],na.rm=T)+f*sd(d[,measure],na.rm=T)))
  bestvals <- d[d[,measure] >= bestrange[1] & d[,measure] <= bestrange[2],]
  bestvals <- bestvals[order(bestvals[,measure], decreasing=F),]
  bestvals <- bestvals[which(bestvals[,measure]!=F)<=limit,]
  if(cut) subset(bestvals, bestvals[,measure]<=cut) else bestvals
}

max.value <- function(d,measure, f=2, limit=12, cut=F){ ## FIND MAX VALUE
  print(bestrange <- c(max(d[,measure],na.rm=T), max(d[,measure],na.rm=T) - f*sd(d[,measure],na.rm=T)))
  bestvals <- d[d[,measure] <= bestrange[1] & d[,measure] >= bestrange[2],]
  bestvals <- bestvals[order(bestvals[,measure], decreasing=T),]
  bestvals <- bestvals[which(bestvals[,measure]!=F)<=limit,]
  if(cut) subset(bestvals, bestvals[,measure]>=cut) else bestvals
}





# ===========================
# 01 analyse_experiment()
# ===========================

analyse_experiment <- function(prefix=""){
	# Read experimental data
	# ------------------------------
	d <- read.table("experiment-data.txt", header=T)
	expname <- d$exp[1]
	fullname <- d$fullname[1]

	if(prefix==""){
		write.table(d, paste(expname,"-data.txt",sep=""))
		resultsfile <- paste(expname,"-results",sep="") 
	} else resultsfile <- paste(prefix,"results",sep="")


	# Read simulation data
	# ------------------------------
	f <- read.table(paste(prefix,"fixations.txt",sep=""), header=F)
	colnames(f) <- c("exp","iteration","cond","pos","word","dur")
	f$cond <- factor(f$cond)


	# Compute eye movement measures
	# ------------------------------
	f$iteration <- as.factor(as.character(f$iteration))
	etm <- with(f, em2(pos, dur, data.frame(exp, iteration, cond)))
	colnames(etm)[4] <- "pos"

	##
	## ADDITIONAL MEASURES
	##
	## first-pass regression probability:
	etm$fp_reg <- ifelse(etm$RBRC>0,1,0) 
	## skipping prob.:
	etm$skip<-ifelse(etm$FPRT==0,1,0)
	## re-reading prob:
	etm$reread<-ifelse(etm$RRT>0,1,0)
	## re-fixation prob.:
	etm$refix <- ifelse(etm$FPRT>etm$FFD,1,0)
	# head(etm)

	## re-add words:
	# dim(etm)
	etm <- merge(etm, unique(f[,3:5]), by=c("cond","pos"))
	# dim(etm)



	# Read additional info
	# ------------------------------
	m <- etm

	## ATTACHMENT TIMES:
	if(file.exists(paste(prefix,"attachments.txt",sep=""))){
		att <- read.table(paste(prefix,"attachments.txt",sep=""), header=F)
		colnames(att) <- c("exp","iteration","cond","pos","word","AT")
		att$cond <- factor(att$cond)
	# dim(m)
		m <- merge(m, att[,-5], by=c("exp","iteration","cond","pos"), all.x=TRUE, all.y=TRUE)
	# dim(m)
	}

	## ENCODING TIMES:
	if(file.exists(paste(prefix,"enctimes.txt",sep=""))){
		enc <- read.table(paste(prefix,"enctimes.txt",sep=""), header=F)
		colnames(enc) <- c("exp","iteration","cond","pos","word","ET","ecc","freq")
		enc$cond <- factor(enc$cond)
		enc$iteration <- factor(enc$iteration)
	# dim(m)
		m <- merge(m, enc[-5], by=c("exp","iteration","cond","pos"), all.x=TRUE)
	# dim(m)
	}

	## TRIAL MESSAGES
	msg <- read.table(paste(prefix,"trialmessages.txt",sep=""), header=F)
	colnames(msg) <- c("exp","iteration","cond","pos","word","variable","value")

	trialinfo <- subset(msg[,-c(4,5)], variable!="timeout" & variable!="regression")
	trialinfo <- reshape(trialinfo, idvar = c("exp","iteration","cond"), timevar="variable", direction="wide")
	colnames(trialinfo) <- gsub("value.","", colnames(trialinfo))
	trialinfo <- droplevels(trialinfo)

	## Merge with EM results
	if(dim(trialinfo)[1]>0){
		dim(m)
		m <- merge(m, trialinfo, by=c("exp","iteration","cond"), all.x=TRUE)
		m$fail <- ifelse(is.na(m$fail), 0, 1)
		dim(m)
	}else{m$fail<-0}

	## TIMEOUTS:
	tmo <- droplevels(subset(msg, variable=="timeout"))
	colnames(tmo)[6:7] <- c("timeout", "tmo-eyloc")
	tmo$timeout <- 1
	dim(m)
	m <- merge(m, tmo[,-5], by=c("exp","iteration","cond","pos"), all.x=TRUE)
	# dim(m)
	m$timeout[is.na(m$timeout)] <- 0



	# Subject information
	# ------------------------------
	if(file.exists(paste(prefix,"subjects.txt",sep=""))){
		subjects <- read.table(paste(prefix,"subjects.txt",sep=""), header=F)
		colnames(subjects) <- c("exp","subj","ga")
		smed <- median(subjects$ga)
		subjects$wmc <- as.factor(ifelse(subjects$ga<smed,"lowWMC","highWMC"))
		# dim(m)
		m <- merge(m, subjects, by=c("exp"), all.x=T)
		# dim(m)
		print(summary(subjects$wmc))
		print(summary(subjects$ga))
	}else{
		m$subj <- 1
		m$ga <- 1
		m$wmc <- 1
		d$wmc <- factor(1)
		m$wmc <- factor(m$wmc)
	}





	# Regions of interest
	# ------------------------------
	# summary(factor(m$cond))
	# dim(m)
	# m <- merge(m, d[,c(2,4,5,7)], by=c("cond","pos"), all.x=TRUE)
	m <- merge(m, d[,c("cond","pos","roi","data")], by=c("cond","pos"), all.x=TRUE)

	# summary(m$roi)
	m$roi <- as.character(m$roi)
	for(c in unique(d$cond)){
		s <- subset(d, cond==c)
		minp <- min(s$pos)
		maxp <- max(s$pos)
		m$roi[m$cond==c & m$pos<minp] <- "n-"
		m$roi[m$cond==c & m$pos>maxp] <- "n+"
	}

	m$roi[is.na(m$roi)] <- "other"
	newLev <- unique(m$roi, na.rm=TRUE)	
	m$roi <- factor(m$roi, levels=newLev)

	words <- unique(with(m[!is.na(m$word),], paste(pos,word)))


	# Cleanup
	# ------------------------------
	m <- data.cleanup(m)
	d[d=="NIL"] <- NA


	# Plots
	# ------------------------------
	m <- m %>% gather(Measure, Value, FFD:timeout, -word, -fail)
	m$Set <- "Model"
	m$Value <- as.numeric(m$Value)

	d <- d %>% gather(Measure, Value, 8:ACC)
	d$Value <- as.numeric(d$Value)
	d$Set <- "Data"

	both <- bind_rows(m,d)
	both$roi <- factor(both$roi, levels=as.character(newLev))

	dodge <- position_dodge(width=.2)
	dodge2 <- position_dodge(width=.6)
	dodge3 <- position_dodge(width=.1)



	pdf(paste(resultsfile,".pdf",sep=""), onefile=T)

	## READING TIME
	suppressWarnings(print(
		ggplot(subset(both, Value!=0 & Measure%in%c("data","FFD","FPRT","RPD","TFT","RRT")), aes(roi, Value, col=cond, group=interaction(Set,cond,wmc), shape=wmc, linetype=Set)) 
		+ geom_point(stat="summary",fun.y="mean", position=dodge)
		+ stat_summary(fun.y="mean", geom="line", position=dodge) 
		+ stat_summary(fun.data="mean_cl_normal", position=dodge, size=.2)
		+	scale_linetype_manual(values=c(2,1))
		+	facet_wrap(~Measure)
		+	ggtitle("Reading times")
		+	myTheme
		))

	## Attachment and encoding
	suppressWarnings(print(
		ggplot(subset(both, Value!=0 & Measure%in%c("data","AT","ET")), aes(roi, Value, col=cond, group=interaction(Set,cond,wmc), shape=wmc, linetype=Set))
		+ geom_point(stat="summary",fun.y="mean", position=dodge)
		+ stat_summary(fun.y="mean", geom="line", position=dodge) 
		+ stat_summary(fun.data="mean_cl_normal", position=dodge, size=.2)
		+ scale_linetype_manual(values=c(2,1))
		+ facet_wrap(~Measure, scales="free", nrow=2)
		+ ggtitle("Attachment and encoding")
		+ myTheme
		))

	## PROBABILITIES
	suppressWarnings(print(
		ggplot(subset(both, Measure%in%c("reread","fp_reg","refix", "skip","timeout")), aes(roi, Value, col=cond, group=interaction(Set,cond,wmc), shape=wmc, linetype=Set))
		+ geom_point(stat="summary",fun.y="mean", position=dodge)
		+ stat_summary(fun.y="mean", geom="line", position=dodge) 
		+ stat_summary(fun.data="mean_cl_boot", position=dodge, size=.2)
		+ scale_linetype_manual(values=c(2,1))
		+ facet_wrap(~Measure, scales="free")
		+ ggtitle("Probabilities")
		+ myTheme
		))


	## FAILURES
	print(
		ggplot(m, aes(cond, fail, fill=wmc)) 
		+ stat_summary(fun.y="mean", geom="bar", position=dodge) 
		+ stat_summary(fun.data="mean_cl_boot", position=dodge, width=.2, geom="errorbar")
		+ coord_cartesian(ylim=c(0,1))
		+ xlab("")
		+ ylab("Probability")
		+ ggtitle("Model failure")
		)


	## WMC
	boxplot(as.numeric(m$ga), main="WMC")


	if(prefix==""){

	## OVERVIEW RT
		suppressWarnings(
			print(ggplot(subset(both, Value!=0 & Measure%in%c("data","FPRT","TFT","AT","ET")), aes(factor(pos), Value, col=cond, group=interaction(Set,cond,wmc), shape=wmc, linetype=Set))
				+ geom_point(stat="summary",fun.y="mean", position=dodge)
				+ stat_summary(fun.y="mean", geom="line", position=dodge) 
				+ stat_summary(fun.data="mean_cl_normal", position=dodge, size=.2)
				+	scale_linetype_manual(values=c(2,1))
				+	facet_wrap(~Measure, ncol=1, scales="free")
				+ coord_cartesian()
			# + theme(legend.position="bottom")
				+	ggtitle("Reading times")
				+ ylab("Mean duration in ms")
				+ ggtitle("Model overview RT")
				))

	## OVERVIEW PROB
		suppressWarnings(
			print(ggplot(subset(both, Measure%in%c("reread","fp_reg","refix", "skip","timeout")), aes(factor(pos), Value, col=cond, group=interaction(Set,cond,wmc), shape=wmc, linetype=Set))
				+ geom_point(stat="summary",fun.y="mean", position=dodge)
				+ stat_summary(fun.y="mean", geom="line", position=dodge) 
				+ stat_summary(fun.data="mean_cl_boot", position=dodge, size=.2)
				+ scale_linetype_manual(values=c(2,1))
				+ facet_wrap(~Measure, ncol=1, scales="free")
				+ ggtitle("Model overview Prob")
				))

	}



	# Scanpaths
	# ------------------------------
	if(prefix==""){

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
		print(ggplot(t1, aes(index, pos, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() 
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
		print(ggplot(t1, aes(index, pos, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() + facet_grid(.~iteration) 
			# + scale_y_discrete(labels=words)
			+ theme(legend.position="bottom"))
	}


	# Fit with the data
	# ------------------------------
	result <- subset(m,Measure=="TFT" & !roi%in%c("n-","n+","other"))
	result <- group_by(result, Measure, roi, cond, data) %>% summarise(model=mean(Value, na.rm=TRUE)) %>% ungroup() %>% filter(!is.na(data))
	(error <- rmsd(result$data,result$model))
	(r <- cor(result$data,result$model))
	(score <- r*100 - error/25)

	effect1 <- 0
	effect2 <- 0
	effscore <- 0

	par(mfrow=c(1,3))
	barplot(error,main="Root-mean-squared error (TFT)",ylim=c(0,error*1.5))
	barplot(r,main="Correlation (TFT)", ylim=c(-1,1))
	barplot(score,main="Score", ylim=c(0,100))

	dev.off()



	# returnResult <- function(){
	return(list(m=m, both, error=error,r=r,score=score, effect1=effect1, effect2=effect2, effscore=effscore))
}





# ===========================
# 02 fit_parameters()
# ===========================
fit_parameters <- function(){

	# Read files
	# ---------------------------

	p <- read.table("paramsearch.txt")
	colnames(p) <- c("experiment","set","file","params")
	p$file <- as.character(p$file)
	p$params <- as.character(p$params)
	p$p.short <- NA
	p$rmsd <- p$cor <- p$score <- p$effect1 <- p$effect2 <- NA 
	p$effscore  <- NA 
	# head(p)

	dirname <- sub("^.*/([^/]*)/[^/]+.txt", "\\1", p$file[1])

	#### BEGIN LOOP ####
	#i <- 1
	for(i in 1:dim(p)[1]){
		print(p$file[i])
		params <- sub("^\\(", "", p$params[i])
		params <- sub("\\)", "", params)
		params <- strsplit(params," ")[[1]]
		p$p.short[i] <- paste(params[seq(2,length(params),2)], collapse=" ")
	  #  
		prefix <- paste(i,"-",sep="")
		results <- analyse_experiment(prefix)
	  #
		p$rmsd[i] <- round(results$error, digits=3)
		p$cor[i] <- round(results$r, digits=3)
		p$score[i] <- round(results$score, digits=3)
		p$effect1[i] <- round(results$effect1, digits=3)
		p$effect2[i] <- round(results$effect2, digits=3)
		p$effscore[i] <- round(results$effscore, digits=3)
	}
	#### END LOOP ####



	# Write
	# ---------------------------
	# colnames(p)[param_col] <- param_names
	write.table(p, "results.txt")

	# p <- read.table("results.txt",header=T)


	# Parameters
	# ---------------------------

## Param columns
	p$p.short <- as.character(p$p.short)
	parmatr <- matrix(unlist(strsplit(p$p.short, split=" ")),nrow=dim(p)[1],byrow=T)
# colnames(parmatr) <- param_names
	x <- NULL
	for(i in 1:dim(parmatr)[2]) x[i] <- paste("p",i,sep="")
		colnames(parmatr) <- x
	p <- cbind(p,parmatr)
#
## Paramlist
	params <- sub("^\\(", "", p$params[1])
	params <- sub("\\)", "", params)
	params <- strsplit(params," ")[[1]]
	param_names <- params[seq(1,length(params),2)]#[-c(1,2)]
#
	params <- list()
	for(i in 1:length(param_names)){
		params[[i]] <- as.double(unlist(lapply(strsplit(p$p.short, split=" "), "[", i)))
	}
	params$names <- param_names

	# params


#
	fit <- quickreport(p$set)
	write.table(fit, "fit.txt")
#



	# Find best fit
	# ---------------------------
	summary(p$cor)
	summary(p$rmsd)
	summary(p$score)

## general
	bestcor <- max.value(p,"cor",limit=3)
	bestrmsd <- min.value(p,"rmsd",limit=3)
	bestscore <- min.value(p,"score", limit=3)

## effects
	besteff <- max.value(p,"effscore")
	besteffect1 <- max.value(p,"effect1", cut=1.5)
	besteffect2 <- max.value(p,"effect2", cut=1.2)
	besteff2 <- subset(besteffect2, effect1>1.5)

	bestfit1 <- unique(rbind(bestscore,bestrmsd,bestcor))
	bestfit <- quickreport(bestfit1$set)

#
	print(bestfit)
	length(bestfit)
	write.table(bestfit, "bestfit.txt")
#
## Latex table:
	write(myxtable(bestfit,cap="Model fit."), file="bestfit.tex")
#



	# Plots
	# ---------------------------

	fit2 <- reshape(fit, idvar = "set", v.names="val",
		times = names(fit)[2:6], timevar = "var",
		varying = list(names(fit)[2:6]), direction = "long")


	pdf(file = paste("../",dirname,"-fit.pdf",sep=""), onefile=TRUE)

## fit by set
	print(ggplot(fit2, aes(factor(set), val, group=var))
		+ geom_line()
		+ geom_point()
		+ facet_wrap(~ var, scales="free")
		+ ggtitle("Fit by parameter set")
		+ ylab("")
		)



## fit by parameters
	param_names <- gsub("-",".",param_names)

	for(i in 1:(length(params)-1)){
		npoly <- min(c(4,length(unique(params[[i]]))-1))
		print(ggplot(fit2, aes(get(param_names[i]), val))
			+ geom_smooth(method="lm", formula= y~poly(x, npoly))
			+ geom_point()
			+ facet_wrap(~ var, scales="free")
			+ ggtitle(param_names[i])
			+ xlab(param_names[i])
			+ ylab("")
			)
	}

	dev.off()

	print(paste("Summary PDF in: paramsearch/",dirname,"-fit.pdf",sep=""))

}





# ===========================
# 02.1 f()
# ===========================

f <- function(ix,iy,z1,ip3,ivp3, persp=1, best="min"){
	require(grDevices)

	theta <- 30
	if(persp==2) theta <- 120
	x1 <- params[[ix]]
	y1 <- params[[iy]]
	x <- sort(unique(x1))
	y <- sort(unique(y1))
	p3 <- params[[ip3]]
	vp3 <- sort(unique(params[[ip3]]))[ivp3]
	z <- tapply(z1[p3==vp3],INDEX=list(x1[p3==vp3],y1[p3==vp3]),mean)

#   x<-1:10
#   y<-1:10
#   z<-matrix(nrow=10,ncol=10,outer(x,y, function(x,y) x*y))

  ## colors
	nrz <- nrow(z)
	ncz <- ncol(z)
  # Create a function interpolating colors in the range of specified colors
	jet.colors <- colorRampPalette( c("blue", "green") )
  # Generate the desired number of colors from this palette
	nbcol <- 1000
	color <- jet.colors(nbcol)
  # Compute the z-value at the facet centres
	zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  #zfacet <- max(z)
  # Recode facet z-values into color indices
	facetcol <- cut(zfacet, nbcol)

	persp(x, y, z, theta = theta, phi = 30, expand = 0.5, 
		col = color[facetcol], ltheta = 120, shade = 0.75, 
		ticktype = "detailed", xlab = params$names[ix], ylab = params$names[iy], zlab = "Score",
		main=paste("Parameter space with",params$names[ip3],"=", vp3)) ->> res

	zbest <- ifelse(best=="min", min(z), max(z))
	xbest <- x1[z1==zbest]
	ybest <- y1[z1==zbest]
	points(trans3d(xbest, ybest, zbest, pmat = res), col = 2, pch = 16)
	return(z)
}








# ===========================
# 03 quick_results()
# ===========================

quick_results <- function(){

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



# PLOTS
# ---------------------------

	words <- unique(with(m[!is.na(m$word),], paste(pos,word)))
	m <- subset(m, !is.na(word))

	dodge <- position_dodge(width=.3)

	pdf("quick-results.pdf")

## PLOT ATTACHMENT TIMES ##
	(ggplot(m, aes(factor(pos), AT, col=cond, group=cond)) 
		+ stat_summary(fun.y="mean", geom="line", position=dodge) 
		+ stat_summary(fun.data="mean_cl_normal", position=dodge, size=.2)
		+ myTheme
		+ scale_x_discrete(labels=words) 
		+ ggtitle("Attachment time")
		)

## PLOT ENCODING TIMES ##
	(ggplot(m, aes(factor(pos), ET, col=cond, group=cond)) 
		+ stat_summary(fun.y="mean", geom="line", position=dodge) 
		+ stat_summary(fun.data="mean_cl_normal", position=dodge, size=.2)
		+ myTheme
		+ scale_x_discrete(labels=words) 
		+ ggtitle("Encoding time"))

## PLOT READING TIMES ##
	(ggplot(subset(m, dur!=0), aes(factor(pos), dur, group=cond, col=cond))
		+ stat_summary(fun.y="mean", geom="line", position=dodge) 
		+ stat_summary(fun.data="mean_cl_normal", position=dodge, size=.2)
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

	dev.off()

}




