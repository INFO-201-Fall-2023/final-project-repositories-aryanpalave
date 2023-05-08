library(dplyr)
library(stringr)
library(ggplot2)

tuition_cost <- read.csv("tuition_cost.csv")
salaries_by_region <- read.csv("salaries-by-region.csv")

#print(colnames(tuition_cost))
#print(colnames(salaries_by_region))

colnames(salaries_by_region)[colnames(salaries_by_region) == "School.Name"] <- "name"

tuition_cost$name <- str_trim(tolower(tuition_cost$name))
salaries_by_region$name <- str_trim(tolower(salaries_by_region$name))

df <- inner_join(tuition_cost, salaries_by_region, by = "name")

#cleaning the data and making it numeric, getting rid of dollar sign, commas, etc. 

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

#creating a function to calculate the ratio

ratio_calculate <- function(mid_career, total) {
  ratio <- round(mid_career/total, 3)
  return(ratio)
}

#creating new columns based on that function
df$outstate_ratio <- mapply(ratio_calculate, df$Mid.Career.Median.Salary, df$out_of_state_total)
df$instate_ratio <- mapply(ratio_calculate, df$Mid.Career.Median.Salary, df$in_state_total)


#creating a function to create the categorical return variable
outstate_return_cal <- function(ratio) {
  mean_ratio <- mean(df$outstate_ratio)
  ifelse(ratio > mean_ratio, TRUE, FALSE)
}
instate_return_cal <- function(ratio) {
  mean_ratio <- mean(df$instate_ratio)
  ifelse(ratio > mean_ratio, TRUE, FALSE)
}

#actually creating the columns

df$outstate_return <- mapply(outstate_return_cal, df$outstate_ratio)
df$instate_return <- mapply(instate_return_cal, df$instate_ratio)

#grouping and summarizing depending on the out of state or in state categories

grouped_df <- group_by(df, type)

summary_df <- summarize(grouped_df, avg_mid_med_sal = mean(Mid.Career.Median.Salary),
avg_outstate_ratio = mean(outstate_ratio), avg_instate_ratio = mean(instate_ratio),
true_outstate_count = sum(outstate_return == 1), false_outstate_count = sum(outstate_return == 0),
true_instate_count = sum(instate_return == 1), false_instate_count = sum(instate_return == 0),
avg_in_state_tuition = mean(in_state_tuition), avg_out_of_state_tuition = mean(out_of_state_tuition))



sum(is.na(tuition_cost))
sum(is.na(salaries_by_region))


write.csv(df, "C:\\Users\\arypa\\Downloads\\df_050723.csv", row.names=FALSE)


