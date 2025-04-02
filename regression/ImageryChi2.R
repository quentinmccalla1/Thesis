library(tidyverse)
library(tidyr)
library(dplyr)



ImageryAreas <- read.csv("ImageryStatistics150mforR.csv")


# Convert data to long format for analysis
ImageryAreas_long <- ImageryAreas %>%
  pivot_longer(
    cols = starts_with("X"),  # Select columns with year names (e.g., X2023, X2021)
    names_to = "Year",
    values_to = "Area"
  ) %>%
  mutate(Year = as.numeric(str_remove(Year, "X")))  # Convert "X2023" -> 2023 (numeric)



# Filter for Sheep
ImageryAreas_long_Sheep <- ImageryAreas_long %>% 
  filter(Year %% 1 == 0)  # This keeps only the whole years (without decimals)

ImageryAreas_long_Childs <- ImageryAreas_long %>% 
  filter(grepl("\\.1$", as.character(Year)))  # This keeps only the whole years (without decimals)

ImageryAreas_long_BRAP <- ImageryAreas_long %>% 
  filter(grepl("\\.2$", as.character(Year)))


# Get unique years
years <- unique(ImageryAreas_long_BRAP$Year)
years <- sort(as.numeric(years))  # Ensure years are sorted numerically

# Initialize a dataframe to store results
chi_results <- data.frame(Class = character(),
                          Year1 = integer(),
                          Year2 = integer(),
                          Chi_sq = numeric(),
                          p_value = numeric(),
                          stringsAsFactors = FALSE)

# Loop through each class
for (class_name in unique(ImageryAreas_long_BRAP$Class)) {
  
  # Loop through pairs of years (e.g., 2010 vs 2013, 2010 vs 2015, etc.)
  for (i in 1:(length(years) - 1)) {
    for (j in (i + 1):length(years)) {
      year1 <- years[i]
      year2 <- years[j]
      
      # Filter data for the specific class and years
      chi_data <- ImageryAreas_long %>%
        filter(Class == class_name & Year %in% c(year1, year2)) %>%
        group_by(Class, Year) %>%
        summarise(Total_Area = sum(Area), .groups = "drop") %>%
        pivot_wider(names_from = Year, values_from = Total_Area) %>%
        column_to_rownames("Class")
      
      # Ensure data exists for the two years being compared
      if (nrow(chi_data) > 0 && all(!is.na(chi_data))) {
        # Perform chi-squared test
        chi_test <- chisq.test(as.matrix(chi_data))
        
        # Store the results
        chi_results <- rbind(chi_results, data.frame(
          Class = class_name,
          Year1 = year1,
          Year2 = year2,
          Chi_sq = chi_test$statistic,
          p_value = chi_test$p.value
        ))
      }
    }
  }
}

# View the results
print(chi_results)


# Sorting by p_value in ascending order to show the smallest (most significant) p-values first
chi_results_sorted <- chi_results %>%
  arrange(p_value)

# If you want to sort by the adjusted p-values (e.g., after applying Bonferroni correction)
chi_results_sorted_adj <- chi_results %>%
  mutate(adj_p_value = p.adjust(p_value, method = "bonferroni")) %>%
  arrange(adj_p_value)

# Print the sorted results
print(chi_results_sorted_adj)

chi_results_sorted_adj %>%
  filter(Class == "Forest")




# Count the number of significant changes (where adj_p_value < 0.05) for each class
significant_changes_count <- chi_results_sorted_adj %>%
  filter(adj_p_value < 0.05) %>%  # Only consider significant changes
  group_by(Class) %>%             # Group by Class
  summarise(Significant_Changes = n())  # Count the number of significant changes

# View the result
print(significant_changes_count)




# Filter for results that compare 2010 vs 2023 for each class
specific_years_comparison <- chi_results_sorted_adj %>%
  filter((Year1 == "2021" & Year2 == "2023") | (Year1 == "2023" & Year2 == "2021"))

# View the filtered results
print(specific_years_comparison)

