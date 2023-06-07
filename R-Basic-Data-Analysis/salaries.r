library(readxl)
library(ggplot2)

# Load the excel sheet into a data frame
salaries <- read_excel("R-Basic-Data-Analysis/data/national_salaries.xlsx", sheet = 1) # nolint



# DATA CLEANING

# Remove rows with "*", "#", "**"
salaries <- salaries[!apply(salaries, 1, function(row) {
    any(c("*", "#", "**") %in% row)
}), ]

# Remove duplicate rows
salaries <- salaries[!duplicated(salaries), ]

# Sets columns as correct type
salaries$TOT_EMP <- as.numeric(salaries$TOT_EMP)
salaries$H_MEAN <- as.numeric(salaries$H_MEAN)
salaries$A_MEAN <- as.numeric(salaries$A_MEAN)

# Select only the desired columns
salaries <- subset(salaries, select = c("ST", "STATE", "OCC_CODE", "OCC_TITLE", "GROUP", "TOT_EMP", "H_MEAN", "A_MEAN")) #nolint



# EXAMPLE QUERIES
# Filter jobs in Indiana with hourly salary below 15
in_jobs1 <- subset(salaries, ST == "IN" & H_MEAN < 15)

# Salary yearly range in 10 intervals
in_jobs_bin <- cut(salaries$A_MEAN, breaks = 10, dig.lab = 10)
in_jobs_bin_table <- table(in_jobs_bin)
in_jobs_bin_df <- as.data.frame(in_jobs_bin_table)
colnames(in_jobs_bin_df) <- c("Salary Interval", "Job Count")

# Sums of employment by state
state_nums <- aggregate(TOT_EMP ~ STATE, data = salaries, FUN = sum)
colnames(state_nums) <- c("STATE", "EMPLOYMENT_TOTAL")

# Average yearly salary in Indiana and California
all_in_jobs <- subset(salaries, ST == "IN")
in_jobs_a_mean <- mean(all_in_jobs$A_MEAN)
all_ca_jobs <- subset(salaries, ST == "CA")
ca_jobs_a_mean <- mean(all_ca_jobs$A_MEAN)

# Compare Average Salaries of Computer and mathematical occupations
in_comp_job_sal <- subset(salaries, ST %in% c("IN", "CA", "NY") & OCC_TITLE == "Computer and mathematical occupations", select = c("STATE", "A_MEAN")) # nolint

# Creates graph for Average Salary of Computer and Mathematical Occupation
ggplot(data = in_comp_job_sal, aes(x = STATE, y = A_MEAN, fill = STATE)) +
    geom_col() +
    ggtitle("Average Salary of Computer and Mathematical Occupation") +
    xlab("State") +
    ylab("Average Salary ($)") +
    scale_fill_manual(values = c("red", "blue", "green")) +
    geom_bar(stat = "identity") +
    theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 0, hjust = 1))

# Display the results
cat("\nJobs in Indiana with hourly salary below 15:\n")
View(in_jobs1)

cat("\nSalary yearly range intervals and job counts:\n")
print(in_jobs_bin_df)

cat("\nSums of employment by state:\n")
print(state_nums)

cat("\nAverage yearly salaries in Indiana and California:\nIndiana: ", in_jobs_a_mean, "\nCalifornia: ", ca_jobs_a_mean, "\n") # nolint

cat("\nComparison of average salaries of Computer and Mathematical occupations:\n") # nolint
print(in_comp_job_sal)
