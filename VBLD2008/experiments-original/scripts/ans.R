#######

superpose.eb <- function (x, y, ebl, ebu = ebl, length = 0.08, ...) 
    arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3, 
    length = length, ...)

#NOMAT = matrix(c(99.80, 97.15, 90.83, 61.52, 53.22, 56.04, 95.57, 80.70, 73.90), 3, 3) # data 
#NOMAT = matrix(c(100.00, 94.08, 88.53, 64.92, 61.25, 58.50, 96.58, 80.82, 76.64), 3, 3) # pirat data 

#     A      B     C
#0.15 100.00 64.92 96.58 
#0.30 94.08  61.2580.82 
#0:45 88.53 58.50,76.64 

NOMAT = matrix(c(100.00, 94.08, 88.53, 85.00, 64.92, 61.25, 58.50, 70.00, 96.58, 80.82, 76.64, 83.00), 4, 3)

colnames(NOMAT) = c("A", "B", "C") 
rownames(NOMAT) = c("0.15", "0.30", "0.45", "-") 

#eblb = matrix(c(14,21,12,18,12,18,13,19),2,4) # 1.96 * s.d. of data 
#x.abscis <- barplot(RT, beside=TRUE, col=0:1, ylim=c(0,1200), 
#    main="RT as a function of Age with 95%-confidance bars", 
#    xlab="Age (yrs)") 
#superpose.eb(x.abscis, RT, eblb, col="orange", lwd=2)

noisecol <- c(grey(.1), grey(.5), grey(.9), 0.0)

barplot(NOMAT, beside=TRUE, col=noisecol, ylim=c(0,100), 
    main="Licensor:  Matrix nominative DP vs. Data", 
    xlab="Conditions",
    ylab="Percentage correct")

legend(9,95,
               legend=c("ans 0.15", "ans 0.30", "ans 0.45", "Data"),
               fill=noisecol,
               xjust=1)


