# clears global environment
rm(list = ls())  
# clears packages
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
# clears plots
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) 
# disables scientific notion for entire R session
options(scipen = 100) 
# clears console
cat("\014")  


install.packages('pacman')
library(pacman)

install.packages('tidyverse')
library(tidyverse)
p_load(tidyverse)

filename <- file.choose()
data <- readRDS(filename)

#Reduce my table to feature only relevant columns
data <- data[, c("date", "location", "county_name", "subject_age", "subject_race", "subject_sex", "type", 
                 "arrest_made", "warning_issued", "contraband_found", "contraband_drugs", 
                 "contraband_weapons", "search_conducted", "reason_for_stop")]

# Convert columns to appropriate data types

# Convert date column to Date type
data$date <- as.Date(data$date)

# Convert categorical columns to factor
data$location <- factor(data$location)
data$county_name <- factor(data$county_name)
data$subject_age <- as.numeric(data$subject_age)  # Assuming age is numeric
data$subject_race <- factor(data$subject_race)
data$subject_sex <- factor(data$subject_sex)
data$type <- factor(data$type)
data$reason_for_stop <- factor(data$reason_for_stop)

# Convert logical columns
data$arrest_made <- as.logical(data$arrest_made)
data$warning_issued <- as.logical(data$warning_issued)
data$contraband_found <- as.logical(data$contraband_found)
data$contraband_drugs <- as.logical(data$contraband_drugs)
data$contraband_weapons <- as.logical(data$contraband_weapons)
data$search_conducted <- as.logical(data$search_conducted)


# Load dplyr and lubridate for data manipulation
library(dplyr)
library(lubridate)

# Add new column with day of the week
data <- data |> mutate(dayofweek = wday(date, label = TRUE))
data

# Frequency table of stops by race
table(data$subject_race)
subject_race_table <- as.data.frame(table(data$subject_race))

# Remove rows where subject race is "Other", "Unknown", or NA
subject_race_table_cleaned <- subject_race_table[!(subject_race_table$Var1 %in% c("NA", "unknown", "other")), ]
rownames(subject_race_table_cleaned) <- NULL
View(subject_race_table_cleaned)

# Frequency table of stops by reason for stop
table(data$reason_for_stop)
race <- data |> group_by(subject_race) |> summarize(counts = n())

# Cross-tabulation of subject race and search conducted
table(data$subject_race, data$search_conducted)
# Create and clean the cross-tabulation
clean_cross_tab <- table(
  data$subject_race[!data$subject_race %in% c("NA", "unknown", "other")],
  data$search_conducted[!data$subject_race %in% c("NA", "unknown", "other")]
)

# Display the cleaned cross-tabulation
print(clean_cross_tab)

# Cross-tabulation of contraband found and type of contraband (drugs vs. weapons)
table(data$contraband_found, data$contraband_drugs, data$contraband_weapons)


# Install and load gmodels for advanced cross-tabulation
install.packages('gmodels')
library(gmodels)

# Cross-tabulation of subject race and contraband found
CrossTable(data$subject_race, data$contraband_found, 
           prop.chisq = FALSE)

# Cross-tabulation of reason for stop and search conducted
CrossTable(data$reason_for_stop, data$search_conducted, 
           prop.chisq = FALSE)


# Load necessary libraries for visualization
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(scales) # For percentage scales

# 1. Bar Chart of Stops by Race
# Filter out "NA", "Unknown", and "Other" values more strictly
filtered_data <- data %>%
  filter(!is.na(subject_race) & 
           !(tolower(subject_race) %in% c("unknown", "other", "na")))

# Bar plot of stops by race
ggplot(filtered_data, aes(x = factor(subject_race))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Traffic Stops by Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(drop = TRUE)

# Bar plot of stops by race with value labels
ggplot(filtered_data, aes(x = factor(subject_race))) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Add labels above bars
  labs(title = "Traffic Stops by Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(drop = TRUE)


# 2. Histogram of Stops by Age
ggplot(data, aes(x = subject_age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  geom_text(stat = "bin", binwidth = 5, aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Stops by Age", x = "Age", y = "Frequency") +
  theme_minimal()

# 3. Proportion of Searches by Race
search_proportions <- data %>%
  group_by(subject_race) %>%
  summarize(search_rate = mean(search_conducted, na.rm = TRUE))

ggplot(search_proportions, aes(x = subject_race, y = search_rate, fill = subject_race)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(search_rate)), vjust = -0.3, size = 3.5) +
  labs(title = "Search Rates by Race", x = "Race", y = "Search Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)

# 4. Line Plot: Daily Stop Trends
data <- data %>% mutate(day = wday(date, label = FALSE))
ggplot(data, aes(x = day)) +
  geom_line(stat = "count", color = "blue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, color = "black") +
  labs(title = "Daily Trends in Traffic Stops", x = "Day of Week", y = "Count") +
  scale_y_continuous(labels = scales::comma) +  # Formatting Y-axis labels with commas for better readability
  theme_minimal()




############  MOD 2   ###################

rm(list = ls())  # clears global environment
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
options(scipen = 100) # disables scientific notion for entire R session
options(digits=2) #reduces the output of decimal places in integers
cat("\014")  # clears console

install.packages('tidyverse')
library(tidyverse)
p_load(tidyverse)

filename <- file.choose()
data <- readRDS(filename)

names(data)
#Reduce my table to feature only relevant columns
data <- data[, c("date", "location", "county_name", "subject_age", "subject_race", "subject_sex", "type", 
                 "arrest_made", "warning_issued", "contraband_found", "contraband_drugs", 
                 "contraband_weapons", "search_conducted", "reason_for_stop", "outcome")]

# Load necessary libraries

library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("psych")
library(psych)

# Clean up the age column
age <- data |> group_by(subject_age) |> summarize(counts = n())

data <- data |> filter(subject_age != 'NA') #remove NAs

data <- data |> filter(subject_age > 15) #remove age less than legal driving age (15 in NC)
View(age)

# 1. Descriptive Statistics
## For the entire sample
overall_stats <- t(psych::describe(data$subject_age))

## By group ('subject_race')
group_stats <- data %>%
  group_by(subject_race) %>%
  summarise(
    mean_age = mean(subject_age, na.rm = TRUE),
    sd_age = sd(subject_age, na.rm = TRUE),
    min_age = min(subject_age, na.rm = TRUE),
    max_age = max(subject_age, na.rm = TRUE),
    N = n()
  )


# Print statistics in a three-line table format
print(knitr::kable(group_stats, format = "pipe"))


# 2. Visualizations
## Scatter plot of age vs. outcome (using jitter to handle categorical outcome)
png("scatter_plot.png", width = 800, height = 600)

# Remove rows with NA in 'outcome' or 'subject_age'
data_clean <- data %>%
  filter(!is.na(subject_age), !is.na(outcome)) %>%
  mutate(age_group = cut(subject_age, breaks = c(0, 25, 50, 75, 100),
                         labels = c("Young", "Middle-aged", "Senior", "Elderly")))

ggplot(data_clean, aes(x = subject_age, y = outcome, color = age_group)) +
  geom_jitter(width = 0.3, alpha = 0.6) +
  labs(title = "Age vs. Outcome by Age Group",
       x = "Age", y = "Outcome") +
  scale_color_manual(values = c("skyblue", "orange", "red", "darkgreen"))

data_clean <- data %>%
  filter(!is.na(subject_age), !is.na(outcome)) %>%
  mutate(age_group = cut(subject_age, breaks = c(0, 25, 50, 75, 100),
                         labels = c("Young", "Middle-aged", "Senior", "Elderly")))

ggplot(data_clean, aes(x = subject_age, y = outcome)) +  # Remove color aesthetic
  geom_jitter(width = 0.3, alpha = 0.6, color = "black") +  # Set color to black here
  labs(title = "Age vs. Outcome",
       x = "Age", y = "Outcome")
dev.off()


# Filter out "Other", "Unknown", and "NA" values
filtered_data <- data %>%
  filter(!is.na(subject_race) &
           !(tolower(subject_race) %in% c("other", "unknown", "NA")) &
           subject_race != "")

# Jitter plot with colors by race
png("jitter_plot.png", width = 800, height = 600)
ggplot(filtered_data, aes(x = factor(subject_race), y = search_conducted, color = factor(subject_race))) +
  geom_jitter(width = 0.3, alpha = 0.6) +
  labs(title = "Search Conducted by Race",
       x = "Race", y = "Search Conducted") +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette for clarity
  scale_x_discrete(drop = TRUE) +  # Remove unused factor levels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
dev.off()

# Boxplot with highlighted major values by race
png("boxplot.png", width = 800, height = 600)
ggplot(data, aes(x = subject_race, y = subject_age)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, fill = "lightblue", color = "darkblue") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "orange") +
  labs(title = "Age Distribution by Race",
       x = "Race", y = "Age") +
  theme_minimal()
dev.off()

#Need to convert subject_race to character so we can remove from boxplot chart
data_age <- data[, c("subject_age", "subject_race")]
data_age$subject_race <- as.character(data_age$subject_race)
data_age <- data_age |> filter(subject_race != 'other', subject_race != 'unknown')
table(data_age$subject_race)



#############   MOD 3   ###################

# Load necessary libraries
library(dplyr)

# Conduct a one-sample t-test for mean age
## Null Hypothesis (H₀): The mean age of individuals stopped is equal to 35 years.
## Alternative Hypothesis (H₁): The mean age of individuals stopped is not equal to 35 years.

# Perform one-sample t-test
t_test_age <- t.test(data$subject_age, mu = 35, alternative = "two.sided")
?t.test

# Print the results of the t-test
print(t_test_age)

# Interpretation:
cat("The one-sample t-test compares the sample mean age to a hypothesized mean of 35 years. 
    With a t-statistic of 1679.8, df = 20,284,198, and p-value < 0.000000000000000022, 
    we reject the null hypothesis. 
    The mean age (35.07) is significantly different from 35 years, as supported by the 95% confidence interval 
    (35.06 to 35.07\n").

# Check the p-value
if(t_test_age$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis. 
      There is evidence to suggest that the mean age of individuals stopped is significantly different from 35 years.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis. 
      There is no significant evidence to suggest that the mean age differs from 35 years.\n")
}

"Hypotheses
Null Hypothesis (H₀): There is no association between race (Black vs. White) 
and the likelihood of being searched during a traffic stop. 
In other words, Black drivers are not more likely to be searched than White drivers.

Alternative Hypothesis (H₁): There is an association between race (Black vs. White) 
and the likelihood of being searched during a traffic stop. 
In other words, Black drivers are more likely to be searched than White drivers."


# Create a contingency table for the observed frequencies
# Use frequency from race table:
observed <- matrix(c(277910, 5909944, 264399, 11629274), nrow = 2, byrow = TRUE,
                   dimnames = list(Race = c("Black", "White"),
                                   Search_Conducted = c("Yes", "No")))

# Print the observed contingency table
print(observed)

# Perform the Chi-square test for independence
chi_test <- chisq.test(observed)

# Print the results of the Chi-square test
print(chi_test)

# Interpretation based on p-value
if(chi_test$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis.
      There is evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis.
      There is no significant evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
}


#############   MOD 4   ###################

rm(list = ls())
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
options(scipen = 100)
cat("\014")

# Using various libraries
library(tidyverse)
install.packages("corrplot")
library(corrplot)
library(car)
library(caret)
library(scales)


data <- read.csv("C:/Users/Jp070/OneDrive/Desktop/nc_statewide_2020_04_01.csv")

filename <- file.choose()
data <- readRDS(filename)

names(data)

traffic_data <- data %>%
  mutate(
    search_conducted = as.numeric(search_conducted),
    arrest_made = as.numeric(arrest_made),
    contraband_found = as.numeric(contraband_found),
    warning_issued = as.numeric(warning_issued),
    citation_issued = as.numeric(citation_issued),
    hour = as.numeric(substr(time, 1, 2)),
    subject_race = as.factor(subject_race),
    subject_sex = as.factor(subject_sex),
  )  %>%
  filter(!is.na(subject_age),
         subject_age >= 16,
         subject_age <= 100)

numeric_vars <- traffic_data %>%
  select(subject_age, search_conducted, arrest_made, 
         contraband_found, warning_issued, citation_issued)

# Correlation Matrix
cor_matrix <- cor(numeric_vars)
print(cor_matrix)

# 1. Correlation Heatmap
png('traffic_correlation.png', width = 800, height = 800)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Correlation Matrix of Traffic Stop Variables",
         bg = "white",
         cl.ratio = 0.2,
         mar = c(0,0,2,0))
dev.off()

# MODEL 1: Search Probability Model

search_model <- glm(search_conducted ~ subject_age + subject_race + subject_sex,
                    data = traffic_data,
                    family = binomial)


# Visualizations
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# Search Rate by Race
    p1 <- traffic_data %>%
    filter(!subject_race %in% c("other", "unknown")) %>% # Exclude "other" and "unknown"
    group_by(subject_race) %>%
    summarise(
      search_rate = mean(search_conducted, na.rm = TRUE), # Handle NA's
      total_stops = n()
    ) %>%
ggplot(aes(x = reorder(subject_race, -search_rate), y = search_rate)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = scales::percent(search_rate, accuracy = 0.1)), 
              vjust = -0.5) +
    labs(title = "Search Rates by Race",
         x = "Race",
         y = "Search Rate") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent)
  
# Print the plot
print(p1)


cat("Model Results\n")
cat("=============\n\n")

cat("1. Search Probability Model\n")
print(summary(search_model))









