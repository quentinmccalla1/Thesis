---
  title: "Seedling revamp"
output: html_document
date: "2024-12-09"


install.packages("tidyverse",repos = "https://cloud.r-project.org")
install.packages("gridExtra")
install.packages("grid")
install.packages("knitr")
install.packages("kableExtra")
library(grid)
library(kableExtra)
library(tidyverse)
library(knitr)

brap_data <- read_csv("BRAPALL.csv")
childs_data <- read_csv("ChildsAll.csv")



brap_data <- brap_data %>%
  mutate(across(starts_with("HDR"), ~ as.numeric(.)))

childs_data <- childs_data %>%
  mutate(across(starts_with("HDR"), ~ as.numeric(.)))

Childs_custom_order <- c("102023", "62024", "122024", "52025")  # Replace with your actual visit names or IDs
BRAP_custom_order <- c("102023", "62024", "102024", "52025")

#Standard error#
brap_data %>%
  group_by(Visit) %>%
  summarise(
    Height_mean = mean(Height, na.rm = TRUE),
    Height_se = sd(Height, na.rm = TRUE) / sqrt(n()),
    
    Diameter_mean = mean(Diameter, na.rm = TRUE),
    Diameter_se = sd(Diameter, na.rm = TRUE) / sqrt(n()),
    
    HDR_mean = mean(HDR, na.rm = TRUE),
    HDR_se = sd(HDR, na.rm = TRUE) / sqrt(n())
  )

childs_data %>%
  group_by(Visit) %>%
  summarise(
    Height_mean = mean(Height, na.rm = TRUE),
    Height_se = sd(Height, na.rm = TRUE) / sqrt(n()),
    
    Diameter_mean = mean(Diameter, na.rm = TRUE),
    Diameter_se = sd(Diameter, na.rm = TRUE) / sqrt(n()),
    
    HDR_mean = mean(HDR, na.rm = TRUE),
    HDR_se = sd(HDR, na.rm = TRUE) / sqrt(n())
  )



#Childs Means
mean_value_by_visit <- childs_data %>%
  mutate(Visit = factor(Visit, levels = Childs_custom_order)) %>%  # Set custom order
  group_by(Visit) %>%  # Group by Visit
  summarise(
    mean_Height = mean(Height, na.rm = TRUE),      # Calculate mean Height
    mean_Diameter = mean(Diameter, na.rm = TRUE),  # Calculate mean Diameter
    mean_hdr = mean(HDR, na.rm = TRUE),            # Calculate mean HDR
    .groups = "drop"  # Ungroup the data after summarization
  ) %>%
  arrange(Visit)  # Arrange based on custom order

#BRAP means
mean_value_by_visit <- brap_data %>%
  mutate(Visit = factor(Visit, levels = BRAP_custom_order)) %>%  # Set custom order
  group_by(Visit) %>%  # Group by Visit
  summarise(
    mean_Height = mean(Height, na.rm = TRUE),      # Calculate mean Height
    mean_Diameter = mean(Diameter, na.rm = TRUE),  # Calculate mean Diameter
    mean_hdr = mean(HDR, na.rm = TRUE),            # Calculate mean HDR
    .groups = "drop"  # Ungroup the data after summarization
  ) %>%
  arrange(Visit)  # Arrange based on custom order





BRAP plot level means all visits

variables <- c("Diameter", "Height", "HDR")  # List of variables
visits <- unique(brap_data$Visit)  # Get all unique Visit values
results <- data.frame()  # Empty data frame to store the results

for (var in variables) {
  for (visit in visits) {  # Loop through each visit
    # Calculate the mean for each plot and visit
    mean_value <- brap_data %>%
      filter(Visit == visit) %>%  # Filter for the current visit
      group_by(Plot) %>%  # Group by Plot
      summarise(mean_value = mean(.data[[var]], na.rm = TRUE), .groups = "drop")  # Calculate mean, remove NA
    
    # Add the variable name and visit to the result
    mean_value$Variable <- var
    mean_value$Visit <- visit
    
    # Bind to the results
    results <- bind_rows(results, mean_value)
  }
}

# View the final results
print(results)

```


Childs plot level means all visits
```{r}
variables <- c("Diameter", "Height", "HDR")  # List of variables
visits <- unique(childs_data$Visit)  # Get all unique Visit values
Childsresults <- data.frame()  # Empty data frame to store the results

for (var in variables) {
  for (visit in visits) {  # Loop through each visit
    # Calculate the mean for each plot and visit
    mean_value <- childs_data %>%
      filter(Visit == visit) %>%  # Filter for the current visit
      group_by(Plot) %>%  # Group by Plot
      summarise(mean_value = mean(.data[[var]], na.rm = TRUE), .groups = "drop")  # Calculate mean, remove NA
    
    # Add the variable name and visit to the result
    mean_value$Variable <- var
    mean_value$Visit <- visit
    
    # Bind to the results
    Childsresults <- bind_rows(Childsresults, mean_value)
  }
}

# View the final results
print(Childsresults)




BRAP Specific variable plot
```{r}
# Initialize an empty data frame to store the results
Childsresults <- data.frame()

# Choosevariable
variable_of_interest <- "HDR"

for (visit in visits) {  # Loop through each visit
  # Calculate the mean for each plot and visit for the "Diameter" variable
  mean_value <- childs_data %>%
    filter(Visit == visit) %>%  # Filter for the current visit
    group_by(Plot) %>%  # Group by Plot
    summarise(mean_value = mean(.data[[variable_of_interest]], na.rm = TRUE), .groups = "drop")  # Calculate mean, remove NA
  
  # Add the variable name and visit to the result
  mean_value$Variable <- variable_of_interest
  mean_value$Visit <- visit
  
  # Bind to the results
  Childsresults <- bind_rows(Childsresults, mean_value)
}

# Pivot the data so that each visit is a separate column
Childsresults_wide <- Childsresults %>%
  pivot_wider(names_from = Visit, values_from = mean_value)




#Childs plot specific means
# Initialize an empty data frame to store the results
Childsresults <- data.frame()

# Choose the variable of interest
variable_of_interest <- "Height"

# Define the visits explicitly, ensuring that '122024' is included
visits <- c("62024", "102023", "102024", "122024")  # List all the visits you want to include

for (visit in visits) {  # Loop through each visit
  # Calculate the mean for each plot and visit for the chosen variable
  mean_value <- childs_data %>%
    filter(Visit == visit) %>%  # Filter for the current visit
    group_by(Plot) %>%  # Group by Plot
    summarise(mean_value = mean(.data[[variable_of_interest]], na.rm = TRUE), .groups = "drop")  # Calculate mean, remove NA
  
  # Add the variable name and visit to the result
  mean_value$Variable <- variable_of_interest
  mean_value$Visit <- visit
  
  # Bind to the results
  Childsresults <- bind_rows(Childsresults, mean_value)
}

# Pivot the data so that each visit is a separate column
Childsresults_wide <- Childsresults %>%
  pivot_wider(names_from = Visit, values_from = mean_value)

# View the final results in a table format using kable
Childsresults_wide %>%
  kable("html", caption = "Mean Height Values by Visit for Childs") %>%
  kable_styling(full_width = FALSE)

```


Plot graphs
```{r}

# Choose the variable of interest (Height in this case)
variable_of_interest <- "Height"

# Calculate the mean height for each plot and visit
mean_height <- childs_data %>%
  group_by(Plot, Visit) %>%  # Group by Plot and Visit
  summarise(mean_height = mean(.data[[variable_of_interest]], na.rm = TRUE), .groups = "drop")  # Calculate mean height, remove NA

# Create a bar plot with more space between bars
ggplot(mean_height, aes(x = factor(Plot), y = mean_height, fill = Visit)) +
  geom_bar(stat = "identity", position = position_dodge(width = .8)) +  # Increase spacing between bars
  labs(title = "Mean Height by Plot and Visit",
       x = "Plot",
       y = "Mean Height") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen", "orange", "pink")) +  # Customize colors
  scale_x_discrete(expand = expansion(add = c(2,2))) +  # Add more space around bars on x-axis
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

```




Factor Visit
```{r}
brap_data$Visit <- as.factor(brap_data$Visit)
childs_data$Visit <- as.factor(childs_data$Visit)

```
Brap ANovA

```{r}
# List of variables to analyze
variables <- c("Diameter", "Height", "HDR") 
# Initialize an empty list to store results
anova_results <- list()
tukey_results <- list()
# Loop through each variable
for (var in variables) {
  # Run ANOVA
  anova_model <- aov(as.formula(paste(var, "~ Visit")), data = brap_data)
  anova_results[[var]] <- summary(anova_model)
  
  # Check for significance in the ANOVA model
  p_value <- summary(anova_model)[[1]][["Pr(>F)"]][1]
  if (!is.na(p_value) && p_value < 0.05) {
    # If significant, run Tukey's HSD
    tukey_test <- TukeyHSD(anova_model)
    tukey_results[[var]] <- tukey_test
  } else {
    tukey_results[[var]] <- NULL
  }
}
# View Tukey's HSD results
for (var in variables) {
  if (!is.null(tukey_results[[var]])) {
    cat("\nTukey's HSD for", var, ":\n")
    print(tukey_results[[var]])
  } else {
    cat("\nNo significant results for", var, "in ANOVA.\n")
  }
}

```

BRAP Anova Table
```{r}
# Initialize an empty list to store formatted Tukey results
formatted_results <- list()

# Loop through each variable's Tukey results
for (var in variables) {
  if (!is.null(tukey_results[[var]])) {
    # Extract Tukey results for Visit comparisons
    tukey_data <- as.data.frame(tukey_results[[var]]$Visit)
    tukey_data$Comparison <- rownames(tukey_data)
    rownames(tukey_data) <- NULL
    tukey_data$Variable <- var
    # Reorder columns
    tukey_data <- tukey_data %>%
      select(Variable, Comparison, diff, lwr, upr, `p adj`) %>%
      rename(Mean_Diff = diff, Lower_CI = lwr, Upper_CI = upr, P_Value = `p adj`)
    # Append to formatted results
    formatted_results[[var]] <- tukey_data
  }
}

# Combine all results into one data frame
final_results <- bind_rows(formatted_results)



#Childs ANOVA
# List of variables to analyze
Childsvariables <- c("Diameter", "Height", "HDR") 

# Initialize empty lists to store results
Childsanova_results <- list()
Childstukey_results <- list()

# Loop through each variable
for (var in Childsvariables) {
  # Run ANOVA
  Childsanova_model <- aov(as.formula(paste(var, "~ Visit")), data = childs_data)
  Childsanova_results[[var]] <- summary(Childsanova_model)
  
  # Check for significance in the ANOVA model
  p_value <- summary(Childsanova_model)[[1]][["Pr(>F)"]][1]
  if (!is.na(p_value) && p_value < 0.05) {
    # If significant, run Tukey's HSD
    tukey_test <- TukeyHSD(Childsanova_model)
    Childstukey_results[[var]] <- tukey_test
  } else {
    Childstukey_results[[var]] <- NULL
  }
}

# View Tukey's HSD results
for (var in Childsvariables) {
  if (!is.null(Childstukey_results[[var]])) {
    cat("\nTukey's HSD for", var, ":\n")
    print(Childstukey_results[[var]])
  } else {
    cat("\nNo significant results for", var, "in ANOVA.\n")
  }
}

#Childs Anova Table
# Initialize an empty list to store formatted Tukey results for Childs site
Childsformatted_results <- list()

# Loop through each variable's Tukey results
for (var in Childsvariables) {  # Use Childsvariables to ensure proper referencing
  if (!is.null(Childstukey_results[[var]])) {  # Access Childstukey_results for Tukey data
    # Extract Tukey results for Visit comparisons
    tukey_data <- as.data.frame(Childstukey_results[[var]]$Visit)
    tukey_data$Comparison <- rownames(tukey_data)
    rownames(tukey_data) <- NULL
    tukey_data$Variable <- var
    
    # Reorder columns and rename for clarity
    tukey_data <- tukey_data %>%
      select(Variable, Comparison, diff, lwr, upr, `p adj`) %>%
      rename(Mean_Diff = diff, Lower_CI = lwr, Upper_CI = upr, P_Value = `p adj`)
    
    # Append to formatted results
    Childsformatted_results[[var]] <- tukey_data
  }
}

# Combine all formatted results into one data frame
Childsfinal_results <- bind_rows(Childsformatted_results)

# Display the results as a styled table
Childsfinal_results %>%
  kable("html", caption = "Tukey's HSD Results for Childs Site Variables") %>%
  kable_styling(full_width = FALSE) %>%
  #row_spec(which(Childsfinal_results$P_Value < 0.05), background = "lightgreen")
  








