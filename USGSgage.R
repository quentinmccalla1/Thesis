#install.packages("dataRetrieval")
library(dataRetrieval)
library(ggplot2)
library(dplyr)
library(lubridate)
```


Download USGS
```{r}
CampVerdeGuage <- readNWISdv(    #Daily
  "09506000",                  #Below camp verde guage
  "00060",                      #Discharge
  startDate = "1991-01-01",      #From first complete year
  endDate = "2006-01-01",                 #Till now
  statCd = "00003"
)
CampVerdeGuage <- CampVerdeGuage %>%     #Rename column to discharge
  rename(Discharge = X_00060_00003)

CampVerdeGauge <- readNWISdv(
  siteNumbers = "09506000",        # Camp Verde gauge
  parameterCd = "00060",           # Discharge
  startDate = "1987-01-01",
  endDate = "2024-01-01"
)
CampVerdeGauge <- renameNWISColumns(CampVerdeGauge)

# 3. Add year column and filter for max Flow per year
AnnualPeaks <- CampVerdeGauge %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  filter(Flow == max(Flow, na.rm = TRUE)) %>%
  ungroup()

write.csv(AnnualPeaks, "C:/Users/qm43/Documents/GitHub/Thesis1/CampVerde_AnnualPeakDischarge.csv", row.names = FALSE)
