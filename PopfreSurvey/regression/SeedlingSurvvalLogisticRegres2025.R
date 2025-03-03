library(ggplot2)
library(dplyr)
library(aod)
library(glm2)
library(MASS)
library(readxl)
install.packages("pscl", repos = "http://cran.us.r-project.org")
library(pscl)
#Look into cross validation
#Hypothesis for survival, come up with patterns/combos for soil




#Read datafile into r
BeasleyPopSurv <- read.csv("regression.csv")
ChildsPopSurv <- read.csv("ChildsRegresssion.csv")

#Create polynomial terms for possible inclusion into logistic model (for bell-shaped model fit) 
#For Beasley
#Polynomial
BeasleyPopSurv$LightBINOM <- BeasleyPopSurv$Light^2
BeasleyPopSurv$HerbaceausBINOM <- BeasleyPopSurv$Herbaceaus^2
BeasleyPopSurv$WaterSurfaceElevationBINOM <- BeasleyPopSurv$WaterSurfaceElevation^2
BeasleyPopSurv$FinesBinom <- BeasleyPopSurv$Fines^2


#For Childs
ChildsPopSurv$LightBINOM <- ChildsPopSurv$Light^2
ChildsPopSurv$HerbaceausBINOM <- ChildsPopSurv$Herbaceaus^2
ChildsPopSurv$FinesBINOM <- ChildsPopSurv$Fines^2




#Model

#Brap#
BeasLogit1 <- glm(Fall2024 ~ LightBINOM + HerbaceausBINOM + FinesBinom, data = BeasleyPopSurv, family = "binomial")
#BeasLogit <- glm(TF ~ DistChan + DistChan2 + DGW + DGW2 + PercFines + PercFines2, data = BeasleyPopSurv, family = "binomial")
summary(BeasLogit1)

## Confidence Intervals using profiled log-likelihood
confint(BeasLogit1)

## Confidence Intervals using standard errors
confint.default(BeasLogit1)
wald.test(b = coef(BeasLogit1), Sigma = vcov(BeasLogit1), L = 1)


pR2(BeasLogit1)


BeasLogit3 <- glm(Fall2024 ~ poly(LightBINOM, 2) + HerbaceausBINOM + FinesBinom, data = BeasleyPopSurv, family = "binomial")
anova(BeasLogit1, BeasLogit3, test = "Chisq")

pR2(ChildsLogit1)








#Childs
ChildsLogit1 <- glm(Fall2024 ~ LightBINOM + HerbaceausBINOM + FinesBINOM, data = ChildsPopSurv, family = "binomial")
summary(ChildsLogit1)

## Confidence Intervals using profiled log-likelihood
confint(ChildsLogit1)

## Confidence Intervals using standard errors
confint.default(ChildsLogit1)
wald.test(b = coef(ChildsLogit1), Sigma = vcov(ChildsLogit1), L = 1)




###########STEPWISE LOGISTIC REGRESSION####################
#Stepwise Logistic Regression based on AIC using stepAIC() in the MASS package 
#Option called direction, which can have the following values: “both”, “forward”, “backward”

# Fit the model

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(BeasleyPopSurv), size = 0.7 * nrow(BeasleyPopSurv))
train_data <- BeasleyPopSurv[train_indices, ]
test_data <- BeasleyPopSurv[-train_indices, ]
observed <- BeasleyPopSurv$Fall2024

#+ Herbaceaus + HerbaceausBINOM + LightBINOM + Light 
#BRAP
model <- glm(`Fall2024` ~ LightBINOM + HerbaceausBINOM + FinesBinom  , 
               data = BeasleyPopSurv, family = binomial)
# Stepwise model selection (MASS package)
step.model <- stepAIC(model, direction = "both", 
                        trace = FALSE)
coef(model)
coef(step.model)

# Summarize the final selected model
summary(model)
summary(step.model)

# Full model predictions
full_probabilities <- predict(model, test_data, type = "response")
full_predicted <- ifelse(full_probabilities > 0.5, 1, 0)

# Stepwise model predictions
step_probabilities <- predict(step.model, test_data, type = "response")
step_predicted <- ifelse(step_probabilities > 0.5, 1, 0)

# Model accuracy
# Observed outcomes
observed <- test_data$Fall2024

# Accuracy
full_accuracy <- mean(full_predicted == observed)
step_accuracy <- mean(step_predicted == observed)

cat("Full model accuracy:", full_accuracy, "\n")
cat("Stepwise model accuracy:", step_accuracy, "\n")



# if polynomial is significant, include both
#Test it: tell R to give me predicted values


#Childs
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(ChildsPopSurv), size = 0.7 * nrow(BeasleyPopSurv))
train_data <- ChildsPopSurv[train_indices, ]
test_data <- ChildsPopSurv[-train_indices, ]
observed <- ChildsPopSurv$Fall2024


model <- glm(`Fall2024` ~  HerbaceausBINOM + LightBINOM + FinesBINOM, 
             data = ChildsPopSurv, family = binomial)
# Stepwise model selection (MASS package)
step.model <- stepAIC(model, direction = "both", 
                      trace = FALSE)
coef(model)
coef(step.model)

# Summarize the final selected model
summary(model)
summary(step.model)

# Full model predictions
full_probabilities <- predict(model, test_data, type = "response")
full_predicted <- ifelse(full_probabilities > 0.5, 1, 0)

# Stepwise model predictions
step_probabilities <- predict(step.model, test_data, type = "response")
step_predicted <- ifelse(step_probabilities > 0.5, 1, 0)

# Model accuracy
# Observed outcomes
observed <- test_data$Fall2024

# Accuracy
full_accuracy <- mean(full_predicted == observed)
step_accuracy <- mean(step_predicted == observed)

cat("Full model accuracy:", full_accuracy, "\n")
cat("Stepwise model accuracy:", step_accuracy, "\n")



##Plots
#Plot
ggplot(data = ChildsPopSurv, aes(x = FinesBINOM, y = Fall2024)) +
  geom_point() +
  stat_smooth(method = "glm", color = "purple", se = FALSE, 
              method.args = list(family = binomial))



#Plot

ggplot(data = BeasleyPopSurv, aes(x = FinesBinom, y = Fall2024)) +
  geom_point() +
  stat_smooth(method = "glm", color = "purple", se = FALSE, 
              method.args = list(family = binomial))

ggplot(data = BeasleyPopSurv, aes(x = LightBINOM, y = Fall2024)) +
  geom_point() +
  stat_smooth(method = "glm", color = "purple", se = FALSE, 
              method.args = list(family = binomial))


############################################
####Riparian Land Cover ####################
# Enter cover data (a 2x2 contingency table)
############################################
Sheep <- read_xlsx("ImageryStatistics150m.xlsx", sheet = "Sheep")
Childs <- read_xlsx("ImageryStatistics150m.xlsx", sheet = "Childs")
BRAP <- read_xlsx("ImageryStatistics150m.xlsx", sheet = "BRAP")

# Perform the chi-square test
result <- chisq.test(Childs)
# Print results
print(result)
#Bonferoni adjust the p-value for the number of comparisons to equal p = 0.05
n = 7 # number of comparisons 
Padj <- 0.05/n
print (Padj)
