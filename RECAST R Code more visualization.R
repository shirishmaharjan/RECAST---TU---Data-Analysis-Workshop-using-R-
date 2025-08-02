# ------------------------------------------------------------------
# R for Researchers Workshop - Employee Data Analysis
# Author: [Your Name]
# Date: [Date of Workshop]
# ------------------------------------------------------------------

# This script contains all the code for the hands-on practical.
# To run a line of code, place your cursor on the line and press:
# Ctrl + Enter (on Windows/Linux)
# Cmd + Enter (on Mac)

# ------------------------------------------------------------------
# MODULE 1: SETUP AND BASICS
# ------------------------------------------------------------------

# Step 1: Install necessary packages (only need to do this ONCE per computer).
# If you have already done this, you can comment out these lines with a #.
install.packages("tidyverse")
install.packages("lubridate") # A great package for working with dates
install.packages("knitr")     # For making nice-looking tables in the console

# Step 2: Load packages into your R session (do this EVERY time you start R).
library(tidyverse)
library(lubridate)
library(knitr)

# ------------------------------------------------------------------
# MODULE 2: DATA IMPORT AND WRANGLING
# ------------------------------------------------------------------

# --- Reading in the Data ---
# Make sure 'detailed_sample_data.csv' is in your R Project directory.
employee_data <- read_csv("C:/Task/NEGAAS/R Training RECAST/Data/RECAST R Data.csv")

# --- Initial Data Inspection (The Detective Work) ---
# Always get to know your data before you do anything else.

# glimpse() is the best way to see column types and a preview of the data.
glimpse(employee_data)
# We can see `Join_Date` is a character (<chr>), not a date. We'll need to fix this.
# `Performance_Score` is also a character.

# summary() provides a statistical overview of each column.
summary(employee_data)
# *** KEY FINDING ***: The minimum salary is -461.43. This is a data error and
# must be removed for our analysis to be valid.

# --- Data Cleaning and Feature Engineering with dplyr ---
# We will create a single, clean, analysis-ready dataset by chaining
# operations together using the pipe `%>%`.

employee_data_clean <- employee_data %>%
  
  # Step 1: Remove rows with impossible data using filter().
  filter(Salary > 0) %>%
  
  # Step 2: Remove columns that are not needed for this analysis using select().
  # The minus sign (-) means "drop this column".
  select(-ID, -Name) %>%
  
  # Step 3: Create new columns and fix data types using mutate(). This is where
  # we add a lot of value to our dataset.
  mutate(
    # Convert the character 'Join_Date' into a real Date object.
    # We tell R the format the date is currently in.
    Join_Date = as.Date(Join_Date, format = "%Y-%m-%d"),
    
    # Engineer a new feature: Years_at_Company.
    # We calculate the difference between today's date and the join date,
    # and divide by 365.25 to get the number of years.
    Years_at_Company = as.numeric(today() - Join_Date) / 365.25,
    
    # Convert character variables into factors (R's way of handling categories).
    Gender = as.factor(Gender),
    Department = as.factor(Department),
    Remote_Work = as.factor(Remote_Work),
    
    # 'Performance_Score' is not just a category, it's an ORDERED category.
    # We create an ordered factor to tell R that Low < Medium < High.
    # This will make our plots order themselves correctly automatically.
    Performance_Score = factor(Performance_Score,
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE)
  )

# --- Verify the Clean Data ---
# Let's check our new, clean data frame.
glimpse(employee_data_clean)
# Note: Join_Date is now <date>, Years_at_Company is <dbl>, and the factors are correct!

summary(employee_data_clean)
# The minimum salary is now a positive number. Our cleaning worked.

# --- Answering Research Questions with dplyr ---

# Question 1: What are the average salary, satisfaction, and number of employees
# for each department?
department_summary <- employee_data_clean %>%
  group_by(Department) %>%
  summarise(
    Num_Employees = n(), # n() counts the number of observations in the group
    Avg_Salary = mean(Salary, na.rm = TRUE),
    Avg_Satisfaction = mean(Satisfaction_Level, na.rm = TRUE),
    Avg_Experience = mean(Experience_Years, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Salary)) # Sort the result by average salary

# View the resulting summary table using kable() for a nice format.
kable(department_summary, digits = 2)


# Question 2: How does performance vary between remote and non-remote workers?
remote_performance_summary <- employee_data_clean %>%
  group_by(Remote_Work, Performance_Score) %>%
  summarise(Count = n()) %>%
  # This calculates the percentage within each group (Remote vs. Not Remote)
  mutate(Percentage = Count / sum(Count) * 100)

kable(remote_performance_summary, digits = 1)


# ------------------------------------------------------------------
# MODULE 3: DATA VISUALIZATION WITH ggplot2
# ------------------------------------------------------------------
# The Grammar of Graphics: ggplot(data, aes(x, y)) + geom_...()

# --- Plot 1: Bar Chart of Department Salaries (Ordered) ---
plot1_dept_salary <- ggplot(
  data = department_summary,
  # Use reorder() to automatically sort the bars for easier interpretation.
  # The minus sign `-` sorts in descending order.
  aes(x = reorder(Department, -Avg_Salary), y = Avg_Salary)
) +
  geom_col(aes(fill = Department), show.legend = FALSE) +
  labs(
    title = "Average Salary by Department",
    subtitle = "IT and Finance departments have the highest average salaries",
    x = "Department",
    y = "Average Annual Salary ($)"
  ) +
  theme_minimal() +
  # Add the salary values on top of the bars for clarity
  geom_text(aes(label = scales::dollar(Avg_Salary)), vjust = -0.5)

plot1_dept_salary


# --- Plot 2: Boxplot of Salary Distributions (Flipped) ---
plot2_salary_dist <- ggplot(
  data = employee_data_clean,
  # We put the categorical variable on the y-axis for coord_flip()
  aes(x = reorder(Department, Salary, FUN = median), y = Salary)
) +
  geom_boxplot(aes(fill = Department), show.legend = FALSE) +
  coord_flip() + # Flip axes to make department names horizontal and easy to read
  labs(
    title = "Salary Distribution by Department",
    subtitle = "Showing median, quartiles, and outliers. Sales has the largest range.",
    x = "Department",
    y = "Annual Salary ($)"
  ) +
  scale_y_continuous(labels = scales::dollar) + # Format y-axis labels as dollars
  theme_light()

plot2_salary_dist


# --- Plot 3: Violin + Boxplot for Satisfaction vs Performance ---
plot3_satisfaction <- ggplot(
  data = employee_data_clean,
  # Because Performance_Score is an ordered factor, the x-axis is automatically correct.
  aes(x = Performance_Score, y = Satisfaction_Level, fill = Performance_Score)
) +
  geom_violin(alpha = 0.6) + # The main violin plot
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) + # A thin boxplot inside
  geom_jitter(width=0.05, alpha=0.2, height=0) + # Show individual data points
  labs(
    title = "Employee Satisfaction by Performance Score",
    subtitle = "Higher performers tend to report higher satisfaction, but with high variance",
    x = "Performance Score",
    y = "Self-Reported Satisfaction Level (0-1)"
  ) +
  theme_bw() +
  guides(fill = "none") # Hide the legend, the axis is enough

plot3_satisfaction


# --- Plot 4: Scatter Plot of Salary vs. Experience (Faceted) ---
plot4_salary_exp_faceted <- ggplot(
  data = employee_data_clean,
  aes(x = Experience_Years, y = Salary, color = Department)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  # facet_wrap() is a ggplot2 superpower. It creates sub-plots for each category.
  facet_wrap(~ Department, scales = "free_y") + # "free_y" lets y-axis scale be different for each plot
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Salary vs. Experience within Each Department",
    subtitle = "A positive correlation exists across all departments",
    x = "Experience (Years)",
    y = "Annual Salary ($)"
  ) +
  theme_dark() + # Let's try a different theme!
  theme(legend.position = "none") # Hide legend as titles are enough

plot4_salary_exp_faceted


# --- Plot 5: Histogram of Employee Tenure ---
plot5_tenure_hist <- ggplot(data = employee_data_clean, aes(x = Years_at_Company)) +
  geom_histogram(
    aes(fill = ..count..), # Color bars by the count in each bin
    binwidth = 1,
    color = "black"
  ) +
  labs(
    title = "Distribution of Employee Tenure",
    subtitle = "Most employees have been with the company for less than 5 years",
    x = "Years at Company",
    y = "Number of Employees"
  ) +
  theme_classic()

plot5_tenure_hist


# --- Saving Your Plots ---
# Use ggsave() to save the LAST plot that was displayed to a file.
# It automatically determines the format from the extension (.png, .pdf, .svg).
# dpi controls the resolution for raster formats like png.

# Save the faceted scatter plot
ggsave("salary_vs_experience_by_dept.png", plot = plot4_salary_exp_faceted, width = 10, height = 7, dpi = 300)

# Save the satisfaction violin plot as a high-quality PDF for a paper
ggsave("satisfaction_by_performance.pdf", plot = plot3_satisfaction, width = 8, height = 6)


# --- END OF PRACTICAL ---