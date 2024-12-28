setwd ("C:/Users/dmmerritt/ISF_Program/SnakeR")
library(waterData)
library(lubridate)
library(tidyverse)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DescTools) #For calculation of mode

##################################################################
#####For retrieving annual instantaneous peak flow from, NWIS####
#################################################################
##Verde River Near Camp Verde, AZ - 09506000 1934-04-01 	2024-12-17
library(dataRetrieval)   #https://waterdata.usgs.gov/blog/dataretrieval/
pcode <- readNWISpCode("all")
siteNo <- "09506000" #USGS streamflow gaging station number
pCode <- "00061" #calls for instantaneous peak
start.date <- "1984-05-31" #or period of interest (life span of trees)
end.date <- "2024-06-06"

VerdePeakQ <- readNWISpeak(siteNumbers = siteNo,
                       startDate = start.date,
                       endDate = end.date)
VerdePeakQ <- data.frame(VerdePeakQ)
VerdePeakQ$DayMonth <- format(as.Date(VerdePeakQ$peak_dt), "%m-%d") #pareses out months and years for further subsetting oand summarization f data 
VerdePeakQ$Year <- format(as.Date(VerdePeakQ$peak_dt), "%y")

SnakeVerdePeakQ <- subset (VerdePeakQ, select = c(peak_dt, DayMonth, Year, peak_va))   #tidyverse command for subsetting and cleaning up data
names(SnakeVerdePeakQ)

min (VerdePeakQ$DayMonth) #4-30 
max (VerdePeakQ$DayMonth) #6-24
min(VerdePeakQ$peak_va) #2910
max(VerdePeakQ$peak_va)  #15000
percentilesPKQ <- quantile(VerdePeakQ$peak_va, c(0.50, 0.95, 0.99, 1.00)) #this returns the median, 95th, 99th percentiles and the max
percentilesPKQ               

library(dplyr)
SnakeVerdePks %>%
  mutate(DayMonth = format(as.Date(SnakeVerdePks$PkDate), "%d-%m"))
#50%   95%   99%  100% 
#8550 11650 14805 15000 
max(SnakeVerdePks$USGS13010065_Q)

###############################################################
#######Daily average streamflow time-series import#############
#######Snake River AB Jackson Lake at Verde Ranch, WY##########
###############################################################

q09506000 <-importDVs("09506000", code="00060", stat="00003", sdate = "1983-10-01", edate="2024-12-03") #00060 is daily average flow
Q <- q09506000
list(Q) 
str(Q) 
names (Q)
#Q$dates = ymd(Q$dates) #Lubridate format
#Q$dates <- as.Date(Q$dates, format="%m-%d-%y")
#Create 3 columns from dates: year, month and day for later sub-setting
Q$year <- year(ymd(Q$date))
Q$month <- month(ymd(Q$date))
Q$day <- day(ymd(Q$date))
list(Q)
#Eliminate missing values
Q <- na.omit(Q)
list(Q) 
str(Q) 
names(Q)

min(Q$year)
max(Q$year)
min(Q$val)
max(Q$val)


#This will provide requested percentiles of the entire data set for the date range selected
percentilesQ <- quantile(Q$val, c(0.1, 0.25, 0.50, 0.95, 0.99, 1.00)) 
####Flow components## 1 = Jan, 12 = Dec, etc.
Winter <- subset(Q, month %in% c('10','11', '12', '1','2','3'))
ptilesWinterQ <- quantile(Winter$val, c(0.1, 0.25, 0.50, 0.95, 0.99, 1.00))

#Quantile will provide requested percentiles of the  data set for the month range selected
Spring <- subset(Q, month %in% c('4','5', '6')) #April, May, June
ptilesSpringQ <- quantile(Spring$val, c(0.1, 0.25, 0.50, 0.95, 0.99, 1.00))
Fall <- subset(Q, month %in% c('7','8', '9'))
ptilesFallQ <- quantile(Fall$val, c(0.1, 0.25, 0.50, 0.95, 0.99, 1.00))

############################################
############ FLOW COMPONENTS################
############################################
#Single baseflow for winter and fall
Oct <- subset(Q, month %in% c('10'))
Nov <- subset(Q, month %in% c('11'))
Dec <- subset(Q, month %in% c('12'))
Jan <- subset(Q, month %in% c('1'))
Feb <- subset(Q, month %in% c('2'))
March <- subset(Q, month %in% c('3'))
April <- subset(Q, month %in% c('4'))
May <- subset(Q, month %in% c('5'))
Jun <- subset(Q, month %in% c('6'))
Jul <- subset(Q, month %in% c('7'))
Aug <- subset(Q, month %in% c('8'))
Sept <- subset(Q, month %in% c('9'))
ptilesOct <- quantile(Oct$val, c(0.50, 0.95, 0.99, 1.00))
ptilesNov <- quantile(Nov$val, c(0.50, 0.95, 0.99, 1.00))
ptilesDec <- quantile(Dec$val, c(0.50, 0.95, 0.99, 1.00))
ptilesJan <- quantile(Jan$val, c(0.50, 0.95, 0.99, 1.00))
ptilesFeb <- quantile(Feb$val, c(0.50, 0.95, 0.99, 1.00))
ptilesMar <- quantile(March$val, c(0.50, 0.95, 0.99, 1.00))
ptilesApr <- quantile(April$val, c(0.50, 0.95, 0.99, 1.00))
ptilesMay <- quantile(May$val, c(0.50, 0.95, 0.99, 1.00))
ptilesJun <- quantile(Jun$val, c(0.50, 0.95, 0.99, 1.00))
ptilesJul <- quantile(Jul$val, c(0.50, 0.95, 0.99, 1.00))
ptilesAug <- quantile(Aug$val, c(0.50, 0.95, 0.99, 1.00))
ptilesSept <- quantile(Sept$val, c(0.50, 0.95, 0.99, 1.00))

EarlyMarch <- April[(March$day < 15), ]
March <- subset(Q, month %in% c('3'))
EarlyMarch <- March[(March$day < 15), ]
LateMarch <- March[(March$day >= 15), ]
LateApril <- April[(April$day >= 15), ]
EarlyApril <- April[(April$day < 15), ]
PeakJun <- Jun[(Jun$day <= 24), ]
LateJun <- Jun[(Jun$day > 24), ]
ptilesAug <- quantile(Aug$val, c(0.1, 0.25, 0.50, 0.95, 0.99, 1.00))
ptilesSept <- quantile(Sept$val, c(0.1, 0.25, 0.50, 0.95, 0.99, 1.00))

#####################################
########BASEFLOW#####################
#####################################
#Winter Baseflow Oct 1 - Mar 14
WinterBaseflow <- subset(Q, month %in% c('10','11', '12', '1','2'))
WinterBaseflow2 <- rbind(WinterBaseflow,EarlyMarch )
ptilesWinterBaseflow2 <- quantile(WinterBaseflow2$val, c(0.50, 0.95, 0.99, 1.00))
#Autumn Baseflow Aug-Sept
AutumnBaseflow <- subset(Q, month %in% c('8','9'))
ptilesAutumnBaseflow <- quantile(AutumnBaseflow$val, c(0.50, 0.95, 0.99, 1.00))
#####################################
########RISING & FALLING#############
#####################################
#Rising season Mar 15 - April 14
RisingSeason <- rbind(LateMarch, EarlyApril)
ptilesRisingSeason <- quantile(RisingSeason$val, c(0.50, 0.95, 0.99, 1.00))
#Falling Season July 1 - 31
FallingSeason <- rbind(Jul)
ptileFallingSeason <- quantile(FallingSeason$val, c(0.50, 0.95, 0.99, 1.00))

#########################################################
####PEARK SEASON#########################################
####  This is on daily average flows; 
####  go to instantaneous peaks dataset for inst value
#########################################################
Peakseason_lA_M_J <- rbind(LateApril, May, Jun)
ptilePeakseason_lA_M_J <- quantile(Peakseason_lA_M_J$val, c(0.50, 0.95, 0.99, 1.00))

##########################################################################################
####Percentiles of each day of entire record: continuous hydrograph#######################
##########################################################################################

#calculate percentiles for each day of the year by grouping variable and create dataframe
#define quantiles (percentiles) of interest
q = c(0.5, 0.95, 0.99, 1.0)

#######DailyQuantiles
DailyQuantiles <- Q %>%
  group_by(month,day) %>%
  summarize(quant50 = quantile(val, probs = q[1]),
            quant95 = quantile(val, probs = q[2]),
            quant99 = quantile(val, probs = q[3]),
            quant100 = quantile(val, probs = q[4]))

############################################################
######RE-ARRANGE DAYS AS WATER YEAR: Oct-Sept###############
############################################################
waterday <- data.frame(seq(93,366, by=1))
waterday2 <- data.frame(seq(1,92, by=1))
colnames(waterday) <- c("waterday")
colnames(waterday2) <- c("waterday")
list(waterday)
str(waterday)
library(dplyr)
waterday3 <- rbind(waterday,waterday2)
###########################################
print(DailyQuantiles, n=366)
print(DailyQuantiles[order(DailyQuantiles$watyr, decreasing = FALSE), ])
list(DailyQuantiles)
####Perrcentage of flow each day
DailyQuantiles$dayNum <- seq(1,366, by=1)
DailyQuantiles$quant99perc <- DailyQuantiles$quant100 * 0.99
DailyQuantiles$quant95perc <- DailyQuantiles$quant100 * 0.95
#combine new waterday vector to big file
DailyQuantiles <- cbind (DailyQuantiles,waterday3$waterday)
#rename column 10 to waterday
names(DailyQuantiles)[10] ="waterday"
#names(DailyQuantiles)[11] ="NIXwaterday2"
#names(DailyQuantiles)[12] ="waterday"

OrderedDailyQuantiles <- arrange(DailyQuantiles, waterday)
print(OrderedDailyQuantiles, n=366)
names(DailyQuantiles)

#######################################################################################################################
#######################################################################################################################
######DAYS: Winter Baseflow: 1-166, Rising: 167-197, PeakSeason: 198-274, Falling limb 275-305, Fall Baseflow: 306-366
#######################################################################################################################
#######################################################################################################################
percentile95day <- c(1,166,166,197,197,274,274,305,305,366)
percentile95Qcfs <- c(506,506,740,740,11650,11650,2185,2185,635,635)
NinetyFifthWR <- as.data.frame(cbind(percentile95day,percentile95Qcfs))
str(Ninetyfifth)
#############################################################################################
######days Winterflow: 1-184 & 316-366, EarlyApril: 184-198, PeakSeason: 198-285, Fall: 285-316
#############################################################################################
percentile99day <- c(1,166,166,197,197,274,274,305,305,366)
percentile99Qcfs <- c(645,645, 931,931, 14805,14805, 3351,3351, 834, 834)
NinetyNineWR <- as.data.frame(cbind(percentile99day,percentile99Qcfs))
str(NinetyNinePer)

###Write the stats out to CSV for plotting

write.csv(DailyQuantiles[,c("month", "day","quant95", "quant99", "quant100")], file="VerdeRanchSnakeDailyQuant.csv")

#####################################################################
######Options for statistical summary using "Summarize" command######
#####################################################################
#Center: mean(), median()
#Spread: sd(), IQR(), mad()
#Range: min(), max(), quantile()
#Position: first(), last(), nth(),
#Count: n(), n_distinct()
#Logical: any(), all()

