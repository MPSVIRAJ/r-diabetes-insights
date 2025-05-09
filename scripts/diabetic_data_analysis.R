# -----------------------Load necessary packages----------------------------------
library(dplyr)
library(ggplot2)
library(lsr)

#-------------------------loading the dataset------------------------------------
diabetes_data <- read.csv("/Users/sameeraviraj/Documents/stat_project/diabetes_dataset.csv", header = TRUE, stringsAsFactors = TRUE)
# Check the first few rows
head(diabetes_data)

# Check for missing values in each column
missing_values <- colSums(is.na(diabetes_data))

# Check for duplicate rows
duplicate_count <- sum(duplicated(diabetes_data))

# Remove rows where gender is "Other"
diabetes_data <- diabetes_data[diabetes_data$gender != "Other", ]

# Remove rows where smoking history is "No Info"
diabetes_data <- diabetes_data[diabetes_data$smoking_history != "No Info", ]

# Check if the rows are removed
table(diabetes_data$gender)  # Should not contain "Other"
table(diabetes_data$smoking_history)  # Should not contain "No Info"

# Print results
print("Missing Values in Each Column:")
print(missing_values)
print(paste("Number of Duplicate Rows:", duplicate_count))

# Remove duplicate rows if any
diabetes_data <- diabetes_data[!duplicated(diabetes_data), ]
sum(duplicated(diabetes_data))

#----------------------------Distributions of variables--------------------------------
#Distribution of BMI
ggplot(diabetes_data, aes(x = bmi)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  ggtitle("Distribution of BMI") +
  xlab("BMI") +
  ylab("Frequency")

#Age distribution
ggplot(diabetes_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  ggtitle("Age Distribution") +
  xlab("Age") +
  ylab("Frequency")

#Gender distribution
ggplot(diabetes_data, aes(x = gender, fill = gender)) +
  geom_bar() +
  ggtitle("Distribution of Gender") +
  xlab("Gender") +
  ylab("Count")

# Count of each gender
gender_counts <- table(diabetes_data$gender)
# Convert to percentages
gender_percentages <- prop.table(gender_counts) * 100
# Print
round(gender_percentages, 2)

#Smoking history (NOT ENOUGH DATA)
ggplot(diabetes_data, aes(x = smoking_history, fill = smoking_history)) +
  geom_bar() +
  ggtitle("Smoking History Distribution") +
  xlab("Smoking History") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Diabetes cases
ggplot(diabetes_data, aes(x = factor(diabetes), fill = factor(diabetes))) +
  geom_bar() +
  ggtitle("Diabetes Prevalence") +
  xlab("Diabetes (0 = No, 1 = Yes)") +
  ylab("Count")

#--------------------------Relationships between different variables-----------------
#BMI vs AGE colored by diabetes
ggplot(diabetes_data, aes(x = age, y = bmi, color = diabetes)) +
  geom_point(alpha = 0.5) +
  ggtitle("BMI vs Age (Colored by Diabetes)") +
  xlab("Age") +
  ylab("BMI")

#HbA1c level by diabetes status
ggplot(diabetes_data, aes(x = factor(hbA1c_level), fill = factor(diabetes))) +
  geom_density(alpha = 0.5) +
  ggtitle("HbA1c Level Density by Diabetes Status") +
  xlab("HbA1c Level") +
  ylab("Density")

#--------------------------Hypothesis testing---------------------------------------

# 1. Higher BMI is associated with an increased likelihood of diabetes.

# Logistic regression model: Diabetes ~ BMI
model_bmi <- glm(diabetes ~ bmi, data = diabetes_data, family = binomial)

# View the summary of the model
summary(model_bmi)
exp(coef(model_bmi))

# 2. Smoking history affects the risk of diabetes.
#making different smoking categories
table(diabetes_data$smoking_history)
#making contingency table
smoking_table <- table(diabetes_data$smoking_history, diabetes_data$diabetes)
print(smoking_table)
# Reset factor levels for smoking_history after removing "No Info"
diabetes_data$smoking_history <- droplevels(diabetes_data$smoking_history)
# Check if "No Info" is gone
table(diabetes_data$smoking_history)
#chi-square test
chi_sq_result <- chisq.test(smoking_table)
print(chi_sq_result)
#compute the effect size using cramer's value
cramersV(smoking_table)

# 3. Older individuals have a higher risk of diabetes
# Logistic regression model: Diabetes ~ Age
model_age <- glm(diabetes ~ age, data = diabetes_data, family = binomial)
# View summary of the model
summary(model_age)
exp(coef(model_age))

# 4. Diabetes prevalence differs by gender.
gender_table <- table(diabetes_data$gender, diabetes_data$diabetes)
gender_table <- gender_table[rownames(gender_table) != "Other", ]  # remove zero row
print(gender_table)
chi_sq_gender <- chisq.test(gender_table)
print(chi_sq_gender)

cramersV(gender_table)

# Logistic regression model with multiple predictors
full_model <- glm(diabetes ~ age + bmi + smoking_history + gender, data = diabetes_data, family = binomial)
# View the summary
summary(full_model)
exp(coef(full_model))

#model performance
diabetes_data$predicted_prob <- predict(full_model, type = "response")
diabetes_data$predicted_class <- ifelse(diabetes_data$predicted_prob > 0.5, 1, 0)
table(Predicted = diabetes_data$predicted_class, Actual = diabetes_data$diabetes)
mean(diabetes_data$predicted_class == diabetes_data$diabetes)
