library(dplR)
library(ggplot2)
library(dplyr)

DendroMerge <- read.csv("DendroMergeforR.csv")
head(DendroMerge)
SalGOO <- DendroMerge %>%filter(Species == "Salgoo")
Popfre <- DendroMerge %>%filter(Species == "Popfre")
Correlated <- DendroMerge %>% filter(Correlated == "Yes")
OLD <- DendroMerge%>% filter(Correlated == "OLD")

DendroMerge <- DendroMerge %>%
  mutate(ID2 = recode(ID2,
                      "C" = "Childs",
                      "L" = "Lower Beasley",
                      "S" = "Sheep",
                      "U" = "Upper Beasley",
                      "W" = "West Clear Creek",
  ))

DendroMerge <- DendroMerge %>%
  rename(`Site ID` = `ID2`) 

#All plot
ggplot(DendroMerge, aes(x = PithHand, y = DendroMerge$Diameter..CM.)) +
  labs( x= "Pith Year", y = "Diameter (cm)", title = " ALL Diameter vs Pith Date") +
  geom_point(shape=18, size= 3)+
  geom_smooth( method = "lm", se = FALSE, size = 1.5)+
  scale_y_continuous(
    limits = c(0,150)
  ) +
  scale_x_continuous(
    limits = c(1900,2020)
  )+ theme_minimal()

#Salgoo plot
ggplot(SalGOO, aes(x = PithHand, y = Diameter..CM.)) +
  geom_point(shape = 18, size = 3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(1985, 2020)) +
  labs(x = "Pith Year", y = "Diameter (cm)", title = " SALGOO Diameter vs Pith Date") +
  theme_minimal()


#Correlated plot
ggplot(Correlated, aes(x = PithHand, y = Diameter..CM.)) +
  geom_point(shape = 18, size = 3) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(1985, 2020)) +
  labs(x = "Pith Year", y = "Diameter (cm)", title = " Correlated Diameter vs Pith Date") +
  theme_minimal()




#Loop for Popfre sites
# Get unique site IDs
site_ids <- unique(Popfre$`Site ID`)

# Loop through each site ID and generate a plot
for (site in site_ids) {
  # Filter data for the current site
  site_data <- Popfre %>% filter(`Site ID` == site)
  
  # Create the plot
  p <- ggplot(site_data, aes(x = PithHand, y = Diameter..CM., color = factor(`Site ID`))) +
    geom_point(shape = 18, size = 3) +
    geom_smooth(method = "lm", se = FALSE, size = 1.5) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_continuous(limits = c(1985, 2020)) +
    labs(x = "Pith Year", y = "Diameter (cm)", title = paste("Diameter vs Pith Date - Site:", site)) +
    theme_minimal()
  
  # Print the plot
  print(p)
}