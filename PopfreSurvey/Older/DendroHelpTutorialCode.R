#This is a series of functions for dendrochronological analysis
#By Dr. Stockton Maxwell
#Radford University
#North American Dendroecological Fieldweek


##############################################################################

#GETTING STARTED

#Set working directory or use Session menu
setwd("G:/My Drive/Teaching/GEOS 480 - Tree Ring Analysis/Dendro Class Share/Analysis")#make sure those slashes face the correct way

#install libraries
install.packages("dplR")
install.packages("treeclim")
install.packages("TRADER")
install.packages("graphics")
install.packages("utils")
install.packages("tidyr")

#load libraries
library(dplR)
library(treeclim)
library(TRADER)
library(graphics)
library(utils)


########################################################################################
#Dendrochronology Program Library in R 
#Cofecha type stuff in dplR - this section of the package helps the user crossdate tree ring series

#read in raw ring widths, change file name to run your own data
grow.rwl <- read.tucson(fname = "Selu_final.rwl") #just change fname to run stuff below or use the appropriate "read" function - see dplR help

#run some stats and graphs
rwl_stats <- rwl.stats(grow.rwl) #summary and stats of raw ring width file
write.csv(rwl_stats, file = "rwl_stats.csv") #write to a file
seg.plot(grow.rwl) #plot of series time spans
ms <- sens1(grow.rwl) #Calculate mean sensitivity
rwl_report <- rwl.report(grow.rwl) #report on your rwl file, could be good for your project to print
print(rwl_report)

#crossdating- you can use this but I highly recommend Andy Bunn's xdater app
#https://andybunn.shinyapps.io/xDateR/ Andy has a tutorial video to guide you
corr.rwl.seg(rwl = grow.rwl, seg.length = 50, bin.floor = 100, n = NULL, prewhiten = TRUE, pcrit = 0.05, 
             biweight = TRUE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL) #cofecha essentially
series.rwl.plot(grow.rwl, series = "SELU01B", series.yrs = as.numeric(names(series)), #look at an individual series
                seg.length = 40, bin.floor = 80, n = NULL,
                prewhiten = TRUE, biweight = TRUE, floor.plus1 = FALSE)
inter <- interseries.cor(grow.rwl, n = NULL, prewhiten = TRUE, biweight = TRUE, method = "spearman")#calculate interseries correlations for each series
write.csv(inter, file = "interseries_corrs.csv") #write to a file

#spaghetti plot of raw ring widths
spag.plot(rwl = grow.rwl, zfac = 1, useRaster = FALSE, res = 300)


###################################################################################
#Arstan stuff in dplR - this section of the package detrends or standardizes series into a site chronology

#interactive detrending - this allows you to explore curve fits for each tree ring series
grow.rwi.int <- i.detrend(rwl = grow.rwl, nyrs = NULL, f = 0.5,pos.slope = FALSE) #allows you to see a variety of fits
spag.plot(rwl = grow.rwi.int, zfac = 1, useRaster = FALSE, res = 300) #again but with the detrended series

#detrend all series at once - after you know which option is best for your data. Just adjust the method.
grow.rwi <- detrend(rwl = grow.rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE) 
rwi_stats <- rwi.stats(grow.rwi) #stats for entire crn
write.csv(rwi_stats, file = "rwi_stats.csv")
rwi_stats_run <- rwi.stats.running(grow.rwi) #running stats - time periods can be adjusted, see help
write.csv(rwi_stats_run, file = "rwi_stats_run.csv")

#building crn with AR model, this produces a residual crn
grow.crn <- chron(x = grow.rwi, prefix = "", biweight = TRUE, prewhiten = TRUE)
#plot crn
crn.plot(crn = grow.crn[,-1], add.spline = TRUE, nyrs = NULL, f = 0.5, crn.line.col='grey50',
         spline.line.col='red', samp.depth.col='grey90', samp.depth.border.col='grey80',
         crn.lwd=1, spline.lwd=2.0, abline.pos=1, abline.col='black', abline.lty=1,abline.lwd=1,
         xlab="Time", ylab="RWI")
write.csv(grow.crn, file = "chronology.csv")

#building crn without AR model, this produces a standardized crn
grow.crn <- chron(x = grow.rwi, prefix = "", biweight = TRUE, prewhiten = FALSE)
#plot crn
crn.plot(crn = grow.crn, add.spline = TRUE, nyrs = NULL, f = 0.5, crn.line.col='grey50',
         spline.line.col='red', samp.depth.col='grey90', samp.depth.border.col='grey80',
         crn.lwd=1, spline.lwd=2.0, abline.pos=1, abline.col='black', abline.lty=1,abline.lwd=1,
         xlab="Time", ylab="RWI")


################################################################################
#code to format stacked or long format monthly climate data to 12 column format

#try with PRISM stacked or long format downloaded from PRISM data viewer for a single point
#you will need to download a csv file of climate data from https://prism.oregonstate.edu/explorer/

setwd("C:/Users/rmaxwell2/Google Drive/Teaching/GEOS 480 - Tree Ring Analysis/Dendro Class Data/Analysis") #set your working directory
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
clim <- read.table(file = "PRISM_ppt_Selu.csv", skip = 10, header = TRUE, sep = ",") #skips reading the header
head(clim)
x <- clim %>%
    mutate(year = substr(clim[,1], 1, 4),
          month = substr(clim[,1], 6, 7)) %>%
    select(2:4) %>%
    pivot_wider(names_from = month, values_from = "ppt..mm.")
write.csv(x,file = "selu_ppt12.csv") #write to a file for later


##############################################################################
#Treeclim - this package allow for the assessment of growth-climate relationships

#load libraries
library(dplR)
library(treeclim)
library(TRADER)
library(graphics)
library(utils)
library(ggplot2)

#set your working directory
setwd("G:/My Drive/Teaching/GEOS 480 - Tree Ring Analysis/Dendro Class Share/Analysis")#make sure those slashes face the correct way

#read in climate data
clim <- read.csv("selu_ppt12.csv")
#plot one month of climate data
a <- ggplot(clim, aes(year, X6))+
  geom_line()+
  labs(x = "Year", y = "Precipitation (mm)",
           title ="June Precipitation for Selu Conservancy")
a

#Response function analysis in treeclim - modeled after Dendroclim2002. 
#Can take dynamic = "static", "moving", "evolving"
resp <- dcc(chrono = grow.crn, climate = clim, selection = -5:10, 
            method = "correlation", dynamic = "evolving", win_size = 35, win_offset = 1, start_last = FALSE,
            timespan = NULL, var_names = NULL, ci = 0.05, boot = "std", sb = FALSE) #this is the main function in treeclim
coef <- coef(resp) #model coefficients 
plot(resp) #plot the model coefficients
#resp #show model results if you'd like
traceplot(resp, variables = c("PCP.prev.aug", "PCP.curr.jun", "PCP.curr.jul"), facet = FALSE) #shows correlations over time if moving or evolutionary selected
#save the output
write.csv(coef, file = ("pcp_coef.csv")) #output coeff
#write plot to file
jpeg("resp_coef.jpg", width = 8, height = 4, units = 'in', res = 300) 
plot.new()
plot(resp)
title(main = "Climate", xlab = "Month")
dev.off()


###############################################################################
#TRADER Code for Growth Release Detection

#load libraries needed
library(dplR)
library(TRADER)

#set your working directory
setwd("G:/My Drive/Teaching/GEOS 480 - Tree Ring Analysis/Dendro Class Share/Analysis")#make sure those slashes face the correct way

#read in crns, change file name to run your own data
thedata <- read.tucson('Selu_final.rwl') #Use your file in your WD

#plot raw data
spag.plot(thedata, zfac = 1, useRaster = FALSE, res = 300)
thedata.raw.crn <- chron(thedata, prefix = "xxx", prewhiten=FALSE)
plot(thedata.raw.crn,abline.pos=NULL,ylab='mm',xlab='Year')

#Abrams and Nowacki technique - this is just one of many techniques
growthAveragingALL(thedata, releases = NULL, m1 = 15, m2 = 15, buffer = 10, drawing = TRUE, criteria = 0.25, criteria2 = 0.50, prefix = "ga", gfun = mean, length = 5, storedev = jpeg)



library(dplR)

UB <-read.tucson( fname = "UpperBeasley.rwl")
LB <- read.tucson( fname = "Lower Beasley.rwl")
ub_plot <- seg.plot(UB)
lbseg <- (seg.plot(LB))
lb_plot <- spag.plot(LB)
rwl.stats(UB)

install.packages("ggpubr")
installed.packages("ggarange")


