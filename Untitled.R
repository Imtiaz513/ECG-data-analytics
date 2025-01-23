#####data cleaning ########
data<-ECG_Revised_1_
str(data)
# Load the dataset
data <- read.csv("your_dataset.csv")  # Replace with your dataset path

# Check the structure
str(data)

# Convert categorical variables to factors
data$Designation_A <- as.factor(data$Designation_A)
data$Gender <- as.factor(data$Gender)
data$Age <- as.factor(data$Age)
data$Years_Work_exp <- as.factor(data$Years_Work_exp)
data$Hospital_Unit <- as.factor(data$Hospital_Unit)
data$Desire_learn_ECG <- as.factor(data$Desire_learn_ECG)
data$Previous_edcu_ECG <- as.factor(data$Previous_edcu_ECG)
# Gender distribution
gender_table <- table(data$Gender)
gender_percent <- prop.table(gender_table) * 100

# Combine into a data frame
gender_summary <- data.frame(
  Gender = names(gender_table),
  Count = as.numeric(gender_table),
  Percentage = round(gender_percent, 2)
)
print(gender_summary)
# Summary for Pre_Score and Post_score
summary(data[c("Pre_Score", "Post_score")])

# Using psych package for detailed stats
library(psych)
describe(data[c("Pre_Score", "Post_score")])
cross_tab <- table(data$Gender, data$Hospital_Unit)
print(cross_tab)
# Paired t-test
t_test <- t.test(data$Pre_Score, data$Post_score, paired = TRUE)
print(t_test)
# Mean improvement
mean_improvement <- mean(data$Post_score - data$Pre_Score)
print(mean_improvement)
# ANOVA for Pre_Score by Gender
anova_pre <- aov(Pre_Score ~ Gender, data = data)
summary(anova_pre)

# ANOVA for Post_score by Gender
anova_post <- aov(Post_score ~ Gender, data = data)
summary(anova_post)
# Summary table for scores
score_summary <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation"),
  Pre_Score = c(mean(data$Pre_Score), median(data$Pre_Score), sd(data$Pre_Score)),
  Post_Score = c(mean(data$Post_score), median(data$Post_score), sd(data$Post_score))
)
print(score_summary)
# Frequency table for Gender
gender_freq <- table(data$Gender)
gender_freq_df <- as.data.frame(gender_freq)
colnames(gender_freq_df) <- c("Gender", "Count")
print(gender_freq_df)
library(gt)
# Create a nicely formatted table for scores
gt(score_summary)
library(ggplot2)
ggplot(data, aes(x = factor(1), y = Pre_Score)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  labs(title = "Pre-Training Scores", x = "", y = "Score") +
  theme_minimal()

ggplot(data, aes(x = factor(1), y = Post_score)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Post-Training Scores", x = "", y = "Score") +
  theme_minimal()
data$Improvement <- data$Post_score - data$Pre_Score
ggplot(data, aes(x = Improvement)) +
  geom_histogram(binwidth = 5, fill = "steelblue", alpha = 0.7) +
  labs(title = "Improvement in Scores", x = "Improvement", y = "Frequency") +
  theme_minimal()
library(gtsummary)
# Create a summary table for all variables
summary_table <- data %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",  # Mean and standard deviation for numeric variables
      all_categorical() ~ "{n} ({p}%)"    # Count and percentage for categorical variables
    ),
    digits = all_continuous() ~ 1, # Set decimal places
    label = list(
      Designation_A ~ "Designation",
      Gender ~ "Gender",
      Age ~ "Age Group",
      Years_Work_exp ~ "Work Experience",
      Hospital_Unit ~ "Hospital Unit",
      Years_Education ~ "Years of Education",
      Desire_learn_ECG ~ "Desire to Learn ECG",
      Previous_edcu_ECG ~ "Previous Education on ECG",
      Pre_Score ~ "Pre-training Score",
      Post_score ~ "Post-training Score"
    )
  ) %>%
  modify_header(label ~ "**Variable**") %>% # Change column header
  bold_labels()

# Print the summary table
summary_table
# Load necessary libraries
library(dplyr)
library(gtsummary)
# Conduct paired t-test
paired_ttest <- t.test(data$Pre_Score, data$Post_score, paired = TRUE)

# Summarize t-test results
ttest_summary <- tibble(
  Statistic = c("Mean Difference", "t-Statistic", "p-Value"),
  Value = c(
    mean(data$Post_score - data$Pre_Score),  # Mean difference
    paired_ttest$statistic,                 # t-statistic
    paired_ttest$p.value                    # p-value
  )
)

# Convert summary into a gtsummary table
ttest_table <- ttest_summary %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Paired t-Test Results",
    subtitle = "Comparison of Pre-Training and Post-Training Scores"
  )

# Print the table
ttest_table
names(data)
# Remove the first column
data <- data[, -1]
names(data)
# Paired t-test
t.test(data$Pre_Score, data$Post_score, paired = TRUE)
# Wilcoxon test for Pre_Score by Designation_A
wilcox.test(Pre_Score ~ Designation_A, data = data)

# Wilcoxon test for Post_score by Designation_A
wilcox.test(Post_score ~ Designation_A, data = data)
library(ggplot2)

# Reshape data for ggplot
library(tidyr)
long_data <- data %>%
  pivot_longer(cols = c("Pre_Score", "Post_score"), 
               names_to = "Score_Type", values_to = "Score")

# Boxplot comparison
ggplot(long_data, aes(x = Score_Type, y = Score, fill = as.factor(Designation_A))) +
  geom_boxplot() +
  labs(fill = "Designation_A", x = "Score Type", y = "Score") +
  theme_minimal()
# Load required libraries
library(ggplot2)
library(tidyr)
library(ggpubr)

# Reshape data for ggplot
long_data <- data %>%
  pivot_longer(cols = c("Pre_Score", "Post_score"), 
               names_to = "Score_Type", values_to = "Score")

# Boxplot with P-values
ggplot(long_data, aes(x = Score_Type, y = Score, fill = as.factor(Designation_A))) +
  geom_boxplot() +
  stat_compare_means(aes(group = Designation_A), method = "t.test") + # Adds p-values
  labs(
    fill = "Designation_A",
    x = "Score Type",
    y = "Score",
    title = "Comparison of Pre-Score and Post-Score by Designation"
  ) +
  theme_minimal()
