
# R For Researchers (CENAS, RECAST, NEGAAS)
# Author: Shirish Maharjan
# Purpose: Data Analysis and Visualization using R

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load the dataset
data <- read_csv("C:/Task/NEGAAS/R Training RECAST/Data/RECAST R Data.csv")

# View the structure and summary of the dataset
str(data)
summary(data)

# -----------------------------
# Data Cleaning
# -----------------------------

# Check for missing values
colSums(is.na(data))

# Rename columns for clarity
data <- data %>%
  rename(
    ID = ID,
    Name = Name,
    Age = Age,
    Gender = Gender,
    Department = Department,
    Salary = Salary,
    Experience = Experience_Years,
    Performance = Performance_Score,
    JoinDate = Join_Date,
    Remote = Remote_Work,
    Projects = Projects_Completed,
    Satisfaction = Satisfaction_Level
  )

# Convert data types
data$Gender <- as.factor(data$Gender)
data$Department <- as.factor(data$Department)
data$Performance <- as.factor(data$Performance)
data$Remote <- as.factor(data$Remote)
data$JoinDate <- as.Date(data$JoinDate)

# -----------------------------
# Descriptive Statistics
# -----------------------------

mean(data$Age)
median(data$Salary)
sd(data$Experience)
var(data$Projects)
summary(data$Satisfaction)

# Mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
get_mode(data$Performance)

# -----------------------------
# Data Manipulation with dplyr
# -----------------------------

# Select specific columns
data_selected <- data %>% select(ID, Name, Age, Salary)

# Filter rows
data_filtered <- data %>% filter(Age > 30 & Salary > 50000)

# Mutate new columns
data <- data %>%
  mutate(Salary_K = Salary / 1000,
         Experience_Level = ifelse(Experience > 10, "Senior", "Junior"))

# Arrange data
data_sorted <- data %>% arrange(desc(Salary))

# Group and summarise
summary_by_dept <- data %>%
  group_by(Department) %>%
  summarise(
    Avg_Salary = mean(Salary),
    Avg_Experience = mean(Experience),
    Count = n()
  )

# Join example (self join for demonstration)
data_joined <- inner_join(data, data, by = "Department")

# -----------------------------
# Data Visualization with ggplot2
# -----------------------------

# Bar chart: Count by Department
ggplot(data, aes(x = Department)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  ggtitle("Count by Department")

# Boxplot: Salary by Department
ggplot(data, aes(x = Department, y = Salary)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  ggtitle("Salary Distribution by Department")

# Pie chart: Gender distribution
gender_count <- data %>%
  count(Gender) %>%
  mutate(perc = n / sum(n) * 100,
         label = paste0(Gender, " (", round(perc, 1), "%)"))
ggplot(gender_count, aes(x = "", y = perc, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("Gender Distribution")

# Histogram: Age distribution
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "white") +
  theme_minimal() +
  ggtitle("Age Distribution")

# Violin plot: Satisfaction by Performance
ggplot(data, aes(x = Performance, y = Satisfaction)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  ggtitle("Satisfaction by Performance")

# Scatter plot: Salary vs Experience
ggplot(data, aes(x = Experience, y = Salary)) +
  geom_point(color = "darkred") +
  theme_minimal() +
  ggtitle("Salary vs Experience")

# Line plot: Projects over Age
ggplot(data, aes(x = Age, y = Projects)) +
  geom_line(color = "blue") +
  theme_minimal() +
  ggtitle("Projects Completed over Age")

# Density plot: Satisfaction
ggplot(data, aes(x = Satisfaction)) +
  geom_density(fill = "cyan") +
  theme_minimal() +
  ggtitle("Satisfaction Density")

# Facet plot: Salary by Department and Gender
plot_name <- ggplot(data, aes(x = Department, y = Salary)) +
  geom_boxplot() +
  facet_wrap(~Gender) +
  theme_minimal() +
  ggtitle("Salary by Department and Gender")

# Save plots if needed
ggsave("plot_name.png", bg = "white")

# End of Script
