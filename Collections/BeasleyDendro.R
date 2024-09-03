## Dendro WORK##

library(dplR)

#Remove
remove(ChildsCI)

#Data

UB <- read.tucson('UBCol1.rwl')
LB <- read.tucson('LBCol1.rwl')
plot(UB, plot.type = "seg")

rwl.report(Childs)
summary(Childs)

#Detrend
UB.rwi <- detrend(rwl= UB, method = "AgeDepSpline")
LB.rwi <- detrend(rwl= LB, method = "AgeDepSpline")

rwi.stats()

#Mean-Value Chronology
UB.crn <- chron(UB.rwi)
LB.crn <- chron(LB.rwi)
plot(UB.crn, add.spline = TRUE, nyrs = 5)
plot(LB.crn, add.spline = TRUE, nyrs = 5)

#BAI
UBBAI <- bai.in(UB, d2pith= NULL)
LBBAI <- bai.in(LB, d2pith = NULL)

LBBAI.rwi <- detrend(rwl = LBBAI, method = "AgeDepSpline")
UBBAI.rwi <- detrend(rwl= UBBAI, method = "AgeDepSpline")

#Detrend? Do I need to for BAI???
##BAI.rwi <- detrend(rwl= ##BAI, method = "AgeDepSpline")

#Plot
UBBAIChron <- chron(UBBAI.rwi)
LBBAIChron <- chron(LBBAI.rwi)

plot.crn(UBBAIChron, add.spline=TRUE, nyrs = 5, window(x=1980))
plot.crn(LBBAIChron, add.spline = TRUE, nyrs = 5)



