remove(list=ls())

args <- commandArgs(trailingOnly = TRUE)
if(length(args>0)) {
  path <- args[1]
  setwd(path)
}


library(reshape)
library(ggplot2)
library(xtable)


####################################################################
## FUNCTIONS
####################################################################
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


require(grDevices)
f <- function(ix,iy,z1,ip3,ivp3, persp=1, best="min"){
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




####################################################################
## READ FILES
####################################################################

p <- read.table("paramsearch.txt")
colnames(p) <- c("experiment","set","file","params")
p$file <- as.character(p$file)
p$params <- as.character(p$params)
p$p.short <- NA
p$rmsd <- p$cor <- p$score <- p$effect1 <- p$effect2 <- NA 
p$effscore  <- NA 
# head(p)
#
dirname <- sub("^.*/([^/]*)/[^/]+.txt", "\\1", p$file[1])
#
#### BEGIN LOOP ####
#i <- 1
for(i in 1:dim(p)[1]){
  print(p$file[i])
  params <- sub("^\\(", "", p$params[i])
  params <- sub("\\)", "", params)
  params <- strsplit(params," ")[[1]]
  p$p.short[i] <- paste(params[seq(2,length(params),2)], collapse=" ")
  #  
  prefix <<- paste(i,"-",sep="")
  source("results.R")
  # pdf(paste(prefix,"results.pdf",sep=""),onefile=T)
  result <- returnResult()
  # dev.off()
  #
  p$rmsd[i] <- round(result$error, digits=3)
  p$cor[i] <- round(result$r, digits=3)
  p$score[i] <- round(result$score, digits=3)
  p$effect1[i] <- round(result$effect1, digits=3)
  p$effect2[i] <- round(result$effect2, digits=3)
  p$effscore[i] <- round(result$effscore, digits=3)
}
#### END LOOP ####

#head(p)




##################################################################### 
## Write and read
##
# colnames(p)[param_col] <- param_names
write.table(p, "results.txt")

#####################################################################
p <- read.table("results.txt",header=T)
#####################################################################
##
## PARAMETERS
##
#
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

params


#
fit <- quickreport(p$set)
write.table(fit, "fit.txt")
#




####################################################################
## FIND BEST FIT
####################################################################
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







####################################################################
## PLOTS
####################################################################

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

# ## 
# ## 3D Parameter space plots
# ##
# params

# #par(mfrow=c(2,1))
# pdf(file = "p-landscape1.pdf")
# z <- f(3, 4, p$score, 2, 1, persp=2)
# dev.off()
# pdf(file = "p-landscape2.pdf")
# z <- f(1, 2, p$score, 3, 2, persp=2)
# dev.off()
# pdf(file = "p-landscape3.pdf")
# z <- f(1, 2, p$score, 3, 3, persp=2)
# dev.off()


# ## effect1
# pdf(file = "p-landscape-effect1.pdf")
# z <- f(3, 4, p$effect1, 2, 2, persp=1, "max")
# dev.off()

# ## effect2
# pdf(file = "p-landscape-effect2.pdf")
# z <- f(3, 4, p$effect2, 1,1, persp=1, "max")
# dev.off()
# pdf(file = "p-landscape-effect2_2.pdf")
# z <- f(1, 2, p$effect2, 3,3, persp=2, "max")
# dev.off()



