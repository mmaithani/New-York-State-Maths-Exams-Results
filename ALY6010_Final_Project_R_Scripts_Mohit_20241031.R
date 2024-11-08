# Load necessary libraries
library(tidyverse)
library(ggplot2)
#library(dplyr)
library(readr)
#install.packages("reshape2")
library(reshape2)

# our data is in CSV file named "math_test_results.csv"
library(readr)
data <- read_csv("ALY6010 Probability Theory and Introductory Statistics/Final Project/Math_Test_Result_2013-2023_20241002.csv")
View(data)

# data cleaning: removing str 'S'
data <- data %>%
  filter(!grepl("s", `Mean Scale Score`, ignore.case = TRUE))
# data cleaning: removing str 'All grades'
data <- data %>%
  filter(!grepl("All Grades", `Grade`, ignore.case = TRUE))
view(data)


#changing data type to numeric
data <- data %>%
  mutate(`Mean Scale Score` := as.numeric(`Mean Scale Score`))

# Convert more columns to numeric (if necessary)
data <- data %>%
  mutate(
    Grade = as.numeric(Grade),
    Year = as.numeric(Year),
    Number.Tested = as.numeric(`Number Tested`)
  )

# Clean data
#data <- data %>%
 # filter(!is.na(`Mean Scale Score`) & is.numeric(`Mean Scale Score`)) %>%  # Remove rows with NAs or non-numeric values
  #mutate(`Mean Scale Score` = as.numeric(`Mean Scale Score`))

view(data)
glimpse(data)
summary(data)
# write.csv(data,"export clean math test.csv", row.names = FALSE)

# Descriptive statistics: we will be Printing summary statistics
summary_stats <- data %>%
  group_by(Grade, Year) %>%
  summarize(
    Mean_Scale_Score = mean(`Mean Scale Score`),
    Median_Scale_Score = median(`Mean Scale Score`),
    Std_Dev_Scale_Score = sd(`Mean Scale Score`),
    Min_Scale_Score = min(`Mean Scale Score`),
    Max_Scale_Score = max(`Mean Scale Score`),
    IQR_Scale_Score = IQR(`Mean Scale Score`)
  )
print(summary_stats)


#5
# install.packages("moments")
library(moments)
summary_stats <- data %>%
  group_by(Grade, Year) %>%
  summarize(
    # ... existing summary statistics
    Skewness = skewness(`Mean Scale Score`),
    Kurtosis = kurtosis(`Mean Scale Score`)
  )
print(summary_stats)


frequency_grade <- table(data$Grade)
print(frequency_grade)

frequency_category <- table(data$`Student Category`)
print(frequency_category)


mode_grade <- names(frequency_grade)[which.max(frequency_grade)]
print(mode_grade)

mode_category <- names(frequency_category)[which.max(frequency_category)]
print(mode_category)
#55



# Visualize mean scale score over time
ggplot(data, aes(x = Year, y = `Mean Scale Score`, color = Grade)) +
  geom_line() +
  labs(title = "Mean Scale Score Over Time by Grade",
       x = "Year",
       y = "Mean Scale Score")


# Visualize distribution of scale scores
ggplot(data, aes(x = `Mean Scale Score`)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Distribution of Mean Scale Scores",
       x = "Mean Scale Score",
       y = "Frequency")

# Subset data for a specific grade and year
grade_3_2023 <- data %>%
  filter(Grade == 3, Year == 2023)

# Descriptive statistics for the subset
summary_stats_grade_3_2023 <- grade_3_2023 %>%
  summarize(
    Mean_Scale_Score = mean(`Mean Scale Score`),
    Median_Scale_Score = median(`Mean Scale Score`),
    Std_Dev_Scale_Score = sd(`Mean Scale Score`),
    Min_Scale_Score = min(`Mean Scale Score`),
    Max_Scale_Score = max(`Mean Scale Score`),
    IQR_Scale_Score = IQR(`Mean Scale Score`)
  )

# Print summary statistics for the subset
print(summary_stats_grade_3_2023)

# Visualize distribution of scale scores for the subset
ggplot(grade_3_2023, aes(x = `Mean Scale Score`)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Distribution of Mean Scale Scores for Grade 3 in 2023",
       x = "Mean Scale Score",
       y = "Frequency")



# Top 10 schools based on mean scale score
top_10_schools <- data %>%
  group_by(`School Name`) %>%
  summarize(
    Mean_Scale_Score = mean(`Mean Scale Score`),
    Number_Tested = sum(`Number Tested`)
  ) %>%
  arrange(desc(Mean_Scale_Score)) %>%
  head(10)

print(top_10_schools)

ggplot(top_10_schools, aes(x = reorder(`School Name`, -Mean_Scale_Score), y = Mean_Scale_Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Mean_Scale_Score, 2)), vjust = -0.5) +
  labs(title = "Top 10 Schools by Mean Scale Score",
       x = "School Name",
       y = "Mean Scale Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Top 10 schools by student category
top_10_schools_by_category <- data %>%
  group_by(`Student Category`) %>%
  summarize(
    Mean_Scale_Score = mean(`Mean Scale Score`),
    Number_Tested = sum(`Number Tested`)
  ) %>%
  arrange(desc(Mean_Scale_Score)) %>%
  head(10)

print(top_10_schools_by_category)
ggplot(top_10_schools_by_category, aes(x = reorder(`Student Category`, -Mean_Scale_Score), y = Mean_Scale_Score)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = round(Mean_Scale_Score, 2)), vjust = -0.5) +
  labs(title = "Top 10 Student Categories by Mean Scale Score",
       x = "Student Category",
       y = "Mean Scale Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(data, aes(x = Year, y = `Mean Scale Score`, color = `Student Category`)) +
  geom_line() +
  labs(title = "Mean Scale Score Over Time by Student Category",
       x = "Year",
       y = "Mean Scale Score")



# Calculate the correlation matrix
correlation_matrix <- cor(data[, c("Grade", "Year", "Number Tested", "Mean Scale Score")])
# Print the correlation matrix
print(correlation_matrix)
ggplot(melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red") +
  labs(title = "Correlation Matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate mean scale score for each student category
category_means <- data %>%
  group_by(`Student Category`) %>%
  summarize(Mean_Scale_Score = mean(`Mean Scale Score`))

# Top 10 student categories
top_10 <- category_means %>%
  arrange(desc(Mean_Scale_Score)) %>%
  head(10)

# Bottom 10 student categories
bottom_10 <- category_means %>%
  arrange(Mean_Scale_Score) %>%
  head(10)

# Print the results FOR TIOO AND BOTTON
print(top_10)
print(bottom_10)
# Combine top and bottom 10 categories
all_categories <- rbind(top_10, bottom_10)

# Create a bar chart
ggplot(all_categories, aes(x = reorder(`Student Category`, Mean_Scale_Score), y = Mean_Scale_Score)) +
  geom_bar(stat = "identity") +
  labs(title = "Top and Bottom 10 Student Categories by Mean Scale Score",
       x = "Student Category",
       y = "Mean Scale Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(data, aes(x = `Number Tested`, y = `Mean Scale Score`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Number Tested and Mean Scale Score",
       x = "Number Tested",
       y = "Mean Scale Score")

### final project----------------------------------------------------------------------------------

summary(data)
str(data)
view(data)
colnames(data)

# Convert columns to numeric where necessary (removing commas, if any)
data$`Pct Level 1` <- as.numeric(gsub(",", "", data$`Pct Level 1`))
data$`Pct Level 2` <- as.numeric(gsub(",", "", data$`Pct Level 2`))
data$`Pct Level 3` <- as.numeric(gsub(",", "", data$`Pct Level 3`))
data$`Pct Level 4` <- as.numeric(gsub(",", "", data$`Pct Level 4`))

# Convert relevant columns to numeric for analysis
data$`Mean Scale Score` <- as.numeric(data$`Mean Scale Score`)
data$Grade <- as.factor(data$Grade)
data$Year <- as.numeric(data$Year)
data$`Number Tested` <- as.numeric(data$`Number Tested`)

# Check structure of the dataset
str(data)

# Exploratory Data Analysis (EDA)
# Summary statistics
summary(data)

# Visualization 1: Mean Scale Score across Grades
ggplot(data, aes(x = Grade, y = `Mean Scale Score`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Mean Scale Score across Grades", x = "Grade", y = "Mean Scale Score") +
  theme_minimal()
# Question 1: Is there a significant difference in Mean Scale Score across Grades?
# Hypothesis Testing using ANOVA
anova_result <- aov(`Mean Scale Score` ~ Grade, data = data)
summary(anova_result)

# Interpretation:
# If p-value < 0.05, we conclude there is a significant difference in Mean Scale Score by grade.

# Visualization 2: Mean Scale Score over Years
ggplot(data, aes(x = Year, y = `Mean Scale Score`)) +
  geom_line(stat = "summary", fun = "mean", color = "steelblue", size = 1) +
  labs(title = "Trend of Mean Scale Score over Years", x = "Year", y = "Mean Scale Score") +
  theme_minimal()
# Question 2: Is there a trend in Mean Scale Score over the Years?
# Hypothesis Testing using Linear Regression
model_year <- lm(`Mean Scale Score` ~ Year, data = data)
summary(model_year)

# Interpretation:
# If the Year coefficient's p-value < 0.05, we conclude there is a significant trend over the years.


# Visualization 3: Relationship between Number Tested and Mean Scale Score
ggplot(data, aes(x = `Number Tested`, y = `Mean Scale Score`)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship between Number Tested and Mean Scale Score", x = "Number Tested", y = "Mean Scale Score") +
  theme_minimal()
# Question 3: Is there a significant relationship between Number Tested and Mean Scale Score?
# Hypothesis Testing using Linear Regression
model_number_tested <- lm(`Mean Scale Score` ~ `Number Tested`, data = data)
summary(model_number_tested)

# Interpretation:
# If the Number Tested coefficient's p-value < 0.05, we conclude there is a significant relationship.


# Save the results to output text file
sink("hypothesis_testing_results.txt")
cat("Question 1: ANOVA for Mean Scale Score by Grade\n")
print(summary(anova_result))
cat("\nQuestion 2: Linear Regression for Mean Scale Score over Years\n")
print(summary(model_year))
cat("\nQuestion 3: Linear Regression for Mean Scale Score vs. Number Tested\n")
print(summary(model_number_tested))
sink()
