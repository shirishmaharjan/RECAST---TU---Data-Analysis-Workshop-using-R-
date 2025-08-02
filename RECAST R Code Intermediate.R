################################################################################
#                                                                              #
#               R FOR RESEARCHERS: A PRACTICAL DATA ANALYSIS SCRIPT            #
#                                                                              #
#         Workshop organized by CENAS, NEGAAS, Women in STEM Club Nepal        #
#                                                                              #
################################################################################


# --- SECTION 1: SETUP AND DATA LOADING ---

# Welcome! This script will guide you through the basics of data analysis in R.
# We will use a sample dataset called "RECAST R Data.csv".

# Step 1.1: Install and Load Necessary Packages
# Packages are collections of functions that extend R's capabilities.
# We only need to install them once. If you've already installed them, you can skip this.
# To run a line of code, place your cursor on it and press Ctrl+Enter (or Cmd+Enter on Mac).

# install.packages("dplyr")     # For data manipulation
# install.packages("ggplot2")   # For beautiful data visualizations
# install.packages("lubridate") # To work with dates easily

# Now, we load the packages into our R session to use their functions.
library(dplyr)
library(ggplot2)
library(lubridate)

# Step 1.2: Load the Dataset
# Make sure the "RECAST R Data.csv" file is in the same folder as your R script,
# or set your working directory to where the file is located.
# You can set your working directory via the menu: Session > Set Working Directory > To Source File Location

data <- read_csv("C:/Task/NEGAAS/R Training RECAST/Data/RECAST R Data.csv")

# --- END OF SECTION 1 ---


# --- SECTION 2: DATA EXPLORATION AND CLEANING ---

# Real-world data is often messy. Before any analysis, we must inspect and clean it.
# This is one of the most important steps in data analysis!

# Step 2.1: Initial Inspection
# Let's get a first look at our data's structure and a summary.

str(data)
# 'str' shows the structure. We can see 'Join_Date' is a character (chr) and not a date.
# 'Performance_Score' is also a character, but it has a natural order (Low < Medium < High).

summary(data)
# 'summary' gives us statistical summaries.
# Notice anything strange? In the 'Salary' column, the minimum value is -461.43. A salary cannot be negative!
# This is a data entry error we need to fix.

# Step 2.2: Cleaning the Data
# We will create a new, cleaned dataframe to work with.

data_cleaned <- data %>%
  # Rule 1: Remove rows with impossible salary values.
  filter(Salary >= 0) %>%
  
  # Rule 2: Convert 'Join_Date' from text to an actual Date format.
  # This allows us to perform calculations with dates.
  mutate(Join_Date = as.Date(Join_Date)) %>%
  
  # Rule 3: Convert 'Performance_Score' and 'Gender' to factors.
  # A factor is R's way of handling categorical variables.
  # We make 'Performance_Score' an ORDERED factor.
  mutate(
    Performance_Score = factor(Performance_Score, levels = c("Low", "Medium", "High"), ordered = TRUE),
    Gender = as.factor(Gender),
    Department = as.factor(Department)
  )

# Step 2.3: Verify the Cleaned Data
# Let's check the summary of our new 'data_cleaned' dataframe.

summary(data_cleaned)
# Now the minimum salary is a reasonable number.
# The 'Join_Date' column is also correctly formatted as a Date.

# --- END OF SECTION 2 ---


# --- SECTION 3: DESCRIPTIVE ANALYSIS AND VISUALIZATION ---

# Before testing hypotheses, let's visualize our data to understand its distribution.

# Visualization 3.1: Histogram of Employee Ages
# A histogram shows the frequency distribution of a single continuous variable.

ggplot(data_cleaned, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#0072B2", color = "white") +
  labs(
    title = "Distribution of Employee Ages",
    x = "Age",
    y = "Number of Employees"
  ) +
  theme_minimal()

# Visualization 3.2: Bar Chart of Employees per Department
# A bar chart is used to show the counts of a categorical variable.

ggplot(data_cleaned, aes(x = Department, fill = Department)) +
  geom_bar() +
  labs(
    title = "Number of Employees in Each Department",
    x = "Department",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


# --- END OF SECTION 3 ---


# --- SECTION 4: INFERENTIAL STATISTICAL ANALYSIS ---

# Now we will use statistical tests to answer specific research questions.

#-------------------------------------------------------------------------------
# Analysis 1: Independent Samples T-Test
#-------------------------------------------------------------------------------

# Research Question: Is there a statistically significant difference in the average salary between male and female employees?

# Variables Needed:
# - One continuous variable (Salary)
# - One categorical variable with exactly two groups (Gender)

# Hypotheses:
# - Null Hypothesis (H0): The mean salary of males is EQUAL to the mean salary of females.
# - Alternative Hypothesis (H1): The mean salary of males is NOT EQUAL to the mean salary of females.

# R Code for T-Test:
t_test_result <- t.test(Salary ~ Gender, data = data_cleaned)
print(t_test_result)

# Interpretation:
# Look at the 'p-value' in the output.
# The standard threshold is 0.05.
# - If p-value < 0.05, we REJECT the null hypothesis. This means there IS a significant difference.
# - If p-value > 0.05, we FAIL TO REJECT the null hypothesis. This means there is NO significant difference.

# Visualization for T-Test: Boxplot
ggplot(data_cleaned, aes(x = Gender, y = Salary, fill = Gender)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Salary Distribution by Gender",
    subtitle = "A visual comparison for the T-Test",
    x = "Gender",
    y = "Salary"
  ) +
  theme_minimal()


#-------------------------------------------------------------------------------
# Analysis 2: ANOVA (Analysis of Variance)
#-------------------------------------------------------------------------------

# Research Question: Do employee salaries differ significantly across different departments (e.g., IT, Sales, HR)?

# Variables Needed:
# - One continuous variable (Salary)
# - One categorical variable with MORE than two groups (Department)

# Hypotheses:
# - Null Hypothesis (H0): The mean salaries of ALL departments are equal.
# - Alternative Hypothesis (H1): AT LEAST ONE department has a different mean salary.

# R Code for ANOVA:
anova_result <- aov(Salary ~ Department, data = data_cleaned)
summary(anova_result) # Use summary() to see the results table

# Interpretation:
# Look at the 'Pr(>F)' value (this is the p-value for the ANOVA test).
# - If p-value < 0.05, we REJECT the null hypothesis. This means there is a significant difference in mean salaries among the departments.
# - If p-value > 0.05, we FAIL TO REJECT the null hypothesis.

# Visualization for ANOVA: Boxplot
ggplot(data_cleaned, aes(x = Department, y = Salary, fill = Department)) +
  geom_boxplot() +
  labs(
    title = "Salary Distribution across Departments",
    subtitle = "A visual comparison for ANOVA",
    x = "Department",
    y = "Salary"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


#-------------------------------------------------------------------------------
# Analysis 3: Chi-Square Test of Independence
#-------------------------------------------------------------------------------

# Research Question: Is there a statistically significant association between an employee's department and their performance score?

# Variables Needed:
# - Two categorical variables (Department, Performance_Score)

# Hypotheses:
# - Null Hypothesis (H0): Department and Performance Score are INDEPENDENT (there is no association).
# - Alternative Hypothesis (H1): Department and Performance Score are DEPENDENT (there is an association).

# R Code for Chi-Square Test:
# First, we create a contingency table (a table of counts).
contingency_table <- table(data_cleaned$Department, data_cleaned$Performance_Score)
print(contingency_table)

# Now, run the test on the table.
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

# Interpretation:
# Look at the 'p-value'.
# - If p-value < 0.05, we REJECT the null hypothesis. There IS a significant association between the variables.
# - If p-value > 0.05, we FAIL TO REJECT the null hypothesis. There is NO significant association.

# Visualization for Chi-Square: Stacked Bar Chart
ggplot(data_cleaned, aes(x = Department, fill = Performance_Score)) +
  geom_bar(position = "fill") + # "fill" shows proportions, making comparison easy
  labs(
    title = "Proportion of Performance Scores by Department",
    subtitle = "A visual check for the Chi-Square Test",
    x = "Department",
    y = "Proportion"
  ) +
  scale_fill_manual(values = c("Low" = "#D55E00", "Medium" = "#F0E442", "High" = "#009E73")) +
  theme_minimal()

#-------------------------------------------------------------------------------
# Analysis 4: Simple Linear Regression
#-------------------------------------------------------------------------------

# Research Question: Can we predict an employee's salary based on their years of experience?

# Variables Needed:
# - One continuous dependent variable (what we want to predict: Salary)
# - One continuous independent variable (our predictor: Experience_Years)

# Hypotheses (for the predictor variable):
# - Null Hypothesis (H0): The relationship between experience and salary is zero (experience is not a significant predictor).
# - Alternative Hypothesis (H1): The relationship is not zero (experience is a significant predictor).

# R Code for Linear Regression:
regression_model <- lm(Salary ~ Experience_Years, data = data_cleaned)
summary(regression_model)

# Interpretation of the 'summary()' output:
# 1. 'Coefficients' -> 'Estimate' for Experience_Years: For every one-year increase in experience, this is the predicted increase in salary.
# 2. 'Coefficients' -> 'Pr(>|t|)' for Experience_Years: This is the p-value. If < 0.05, experience is a statistically significant predictor of salary.
# 3. 'Adjusted R-squared': This tells you the percentage of variation in salary that is explained by experience. A value of 0.10 means 10% is explained.

# Visualization for Linear Regression: Scatter Plot with Regression Line
ggplot(data_cleaned, aes(x = Experience_Years, y = Salary)) +
  geom_point(alpha = 0.6, color = "darkblue") + # The actual data points
  geom_smooth(method = "lm", color = "red", se = FALSE) + # The regression line
  labs(
    title = "Relationship between Salary and Years of Experience",
    subtitle = "A visualization of our linear regression model",
    x = "Years of Experience",
    y = "Salary"
  ) +
  theme_minimal()

# --- END OF SCRIPT ---