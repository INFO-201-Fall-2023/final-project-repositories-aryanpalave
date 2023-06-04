library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
tuition_cost <- read.csv("tuition_cost copy.csv")
salaries_by_region <- read.csv("salaries-by-region copy.csv")

#print(colnames(tuition_cost)) 
#print(colnames(salaries_by_region))

colnames(salaries_by_region)[colnames(salaries_by_region) == "School.Name"] <- "name"

tuition_cost$name <- str_trim(tolower(tuition_cost$name))
salaries_by_region$name <- str_trim(tolower(salaries_by_region$name))

df <- inner_join(tuition_cost, salaries_by_region, by = "name")

df$Mid.Career.Median.Salary <- gsub("[$,]", "", df$Mid.Career.Median.Salary)
df$Mid.Career.Median.Salary <- as.numeric(df$Mid.Career.Median.Salary)

df$Mid.Career.10th.Percentile.Salary <- gsub("[$,]", "", df$Mid.Career.10th.Percentile.Salary)
df$Mid.Career.10th.Percentile.Salary <- as.numeric(df$Mid.Career.10th.Percentile.Salary)

df$Mid.Career.25th.Percentile.Salary <- gsub("[$,]", "", df$Mid.Career.25th.Percentile.Salary)
df$Mid.Career.25th.Percentile.Salary <- as.numeric(df$Mid.Career.25th.Percentile.Salary)

df$Mid.Career.75th.Percentile.Salary <- gsub("[$,]", "", df$Mid.Career.75th.Percentile.Salary)
df$Mid.Career.75th.Percentile.Salary <- as.numeric(df$Mid.Career.75th.Percentile.Salary)

df$Mid.Career.90th.Percentile.Salary <- gsub("[$,]", "", df$Mid.Career.90th.Percentile.Salary)
df$Mid.Career.90th.Percentile.Salary <- as.numeric(df$Mid.Career.90th.Percentile.Salary)

df$Starting.Median.Salary <- gsub("[$,]", "", df$Starting.Median.Salary)
df$Starting.Median.Salary <- as.numeric(df$Starting.Median.Salary)

ratio_calculate <- function(mid_career, total) {
  ratio <- round(mid_career/total, 3)
  return(ratio)
}
df$outstate_ratio <- mapply(ratio_calculate, df$Mid.Career.Median.Salary, df$out_of_state_total)
df$instate_ratio <- mapply(ratio_calculate, df$Mid.Career.Median.Salary, df$in_state_total)

outstate_return_cal <- function(ratio) {
  mean_ratio <- mean(df$outstate_ratio)
  ifelse(ratio > mean_ratio, TRUE, FALSE)
}
instate_return_cal <- function(ratio) {
  mean_ratio <- mean(df$instate_ratio)
  ifelse(ratio > mean_ratio, TRUE, FALSE)
}

df$outstate_return <- mapply(outstate_return_cal, df$outstate_ratio)
df$instate_return <- mapply(instate_return_cal, df$instate_ratio)

grouped_df <- group_by(df, type)

summary_df <- summarize(grouped_df, avg_mid_med_sal = mean(Mid.Career.Median.Salary),
avg_outstate_ratio = mean(outstate_ratio), avg_instate_ratio = mean(instate_ratio),
true_outstate_count = sum(outstate_return == 1), false_outstate_count = sum(outstate_return == 0),
true_instate_count = sum(instate_return == 1), false_instate_count = sum(instate_return == 0),
avg_in_state_tuition = mean(in_state_tuition), avg_out_of_state_tuition = mean(out_of_state_tuition))

write.csv(df, "C:\\Users\\arypa\\Downloads\\df_050723.csv", row.names=FALSE)

instate_data <- data.frame(type = df$type, return_value = df$instate_return, category = "In-State")
outstate_data <- data.frame(type = df$type, return_value = df$outstate_return, category = "Out-of-State")
combined_data <- rbind(instate_data, outstate_data)
ggplot(combined_data, aes(x = as.factor(return_value), fill = category)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ type) +
  labs(title = "Number of True and False Returns by College Type",
       x = "Return Value (True or False)",
       y = "Count") +
  scale_x_discrete(labels = c("False", "True")) +
  theme_minimal()

ggplot(df, aes(x = instate_ratio, y = outstate_ratio, color = type)) +
  geom_point(alpha = 0.6) +
  labs(title = "Relationship between In-State and Out-of-State Ratios",
       x = "In-State Ratio",
       y = "Out-of-State Ratio") +
  theme_minimal() +
  theme(legend.title = element_text("College Type"))

plot_data <- data.frame(type = rep(df$type, 2),
                        ratio = c(df$instate_ratio, df$outstate_ratio),
                        category = c(rep("In-State", length(df$type)), rep("Out-of-State", length(df$type))))

# Plot the histogram
ggplot(plot_data, aes(x = ratio, fill = category)) +
  geom_histogram(position = "dodge", bins = 30) +
  facet_wrap(~ type) +
  labs(title = "In-State and Out-of-State Ratios by College Type",
       x = "Ratio",
       y = "Count") +
  theme_minimal()
print(summary_df)

avg_mid_med_sal_private <- summary_df$avg_mid_med_sal[summary_df$type == "Private"]
avg_mid_med_sal_public <- summary_df$avg_mid_med_sal[summary_df$type == "Public"]

data_salaries <- data.frame(
  College_Type = c("Private", "Public"),
  Avg_Mid_Career_Median_Salary = c(avg_mid_med_sal_private, avg_mid_med_sal_public)
)

ggplot(data_salaries, aes(x=College_Type, y=Avg_Mid_Career_Median_Salary, fill=College_Type)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Comparison of Average Mid-Career Median Salaries",
       x="Type of College",
       y="Average Mid-Career Median Salary",
       fill="Type of College")

avg_in_state_tuition_private <- summary_df$avg_in_state_tuition[summary_df$type == "Private"]
avg_out_of_state_tuition_private <- summary_df$avg_out_of_state_tuition[summary_df$type == "Private"]

avg_in_state_tuition_public <- summary_df$avg_in_state_tuition[summary_df$type == "Public"]
avg_out_of_state_tuition_public <- summary_df$avg_out_of_state_tuition[summary_df$type == "Public"]

data_tuition <- data.frame(
  College_Type = c(rep("Private", 2), rep("Public", 2)),
  Tuition_Type = c("In-State", "Out-of-State", "In-State", "Out-of-State"),
  Avg_Tuition = c(avg_in_state_tuition_private, avg_out_of_state_tuition_private, avg_in_state_tuition_public, avg_out_of_state_tuition_public)
)

ggplot(data_tuition, aes(x=College_Type, y=Avg_Tuition, fill=Tuition_Type)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  labs(title="Comparison of Average Tuition Costs",
       x="Type of College",
       y="Average Tuition Cost",
       fill="Tuition Type")

avg_starting_sal_private <- mean(df$Starting.Median.Salary[df$type == "Private"])
avg_mid_career_sal_private <- mean(df$Mid.Career.Median.Salary[df$type == "Private"])

avg_starting_sal_public <- mean(df$Starting.Median.Salary[df$type == "Public"])
avg_mid_career_sal_public <- mean(df$Mid.Career.Median.Salary[df$type == "Public"])

# Create data frame
data_salary_progression <- data.frame(
  College_Type = c(rep("Private", 2), rep("Public", 2)),
  Career_Stage = c("Starting", "Mid-Career", "Starting", "Mid-Career"),
  Avg_Salary = c(avg_starting_sal_private, avg_mid_career_sal_private, avg_starting_sal_public, avg_mid_career_sal_public)
)

# Create plot
ggplot(data_salary_progression, aes(x=Career_Stage, y=Avg_Salary, fill=College_Type)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  labs(title="Comparison of Average Salaries at Different Career Stages",
       x="Career Stage",
       y="Average Salary",
       fill="Type of College")





