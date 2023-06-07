library(ggplot2)
library(dplyr)


# loads excel sheet into data frame
data <- read.csv("R-Basic-Data-Analysis/data/hospital_data.csv") # nolint


# Clean Data
# Changes column names to be uniform
names(data) <- c("Case_Id", "Hospital_Code", "Hospital_Type_Code", "City_Code_Hospital", "Hospital_Region_Code", "Available_Extra_Rooms_In_Hospital", "Department", "Ward_Type", "Ward_Facility_Code", "Bed_Grade", "Patient_Id", "City_Code_Patient", "Type_Of_Admission", "Severity_Of_Illness", "Visitors_With_Patient", "Age", "Admission_Deposit", "Stay") # nolint

# Converts columns to correct data types
data$Available_Extra_Rooms_In_Hospital <- as.numeric(data$Available_Extra_Rooms_In_Hospital) # nolint
data$Bed_Grade <- as.numeric(data$Bed_Grade)
data$Visitors_With_Patient <- as.numeric(data$Visitors_With_Patient)
data$Admission_Deposit <- as.numeric(data$Admission_Deposit)

# Changes outlying data points to make them unifrom (replace 20-Nov with 11-20)
data$Stay[data$Stay == "20-Nov"] <- "11-20"
data$Age[data$Age == "20-Nov"] <- "11-20"

# Removes rows with missing values
data <- na.omit(data)




# Add 2 new columns to have numerical representation of its counterpart
map <- c("0-10" = 0, "11-20" = 1, "21-30" = 2, "31-40" = 3, "41-50" = 4, "51-60" = 5, "61-70" = 6, "71-80" = 7, "81-90" = 8, "91-100" = 9, "More than 100 Days" = 10) # nolint
cols <- c("Stay", "Age")
new_cols <- c("Stay_Number", "Age_Number")

for (i in 1:length(cols)) { # nolint
    data[, new_cols[i]] <- map[as.character(data[, cols[i]])]
}



# Count all the unique hospital codes
hospital_codes_length <- length(unique(data$Hospital_Code))
print(hospital_codes_length)



# Print the number of unique patient IDs and total rows
num_unique_patient_ids <- length(unique(data$Patient_Id))
total_rows <- nrow(data)

cat("Number of unique patient IDs: ", num_unique_patient_ids, "\n")
cat("Total number of rows: ", total_rows, "\n")




# List the number of unique hospital_codes per each Hospital_Region_Code
# Help from:
# https://www.geeksforgeeks.org/group-by-function-in-r-using-dplyr/
# https://dplyr.tidyverse.org/reference/summarise.html
hospitals_per_code <- data %>%
    group_by(Hospital_Region_Code) %>%
    summarize(n_unique_codes = n_distinct(Hospital_Code))
print(hospitals_per_code)




# Determine the most common type of admission for patients
admission_types <- table(data$Type_Of_Admission)
most_common_admission_type <- names(admission_types[which.max(admission_types)])
cat("  Most common admission type:", most_common_admission_type)




# Calculate the mean median and mode for the number of visitors with patients at admission time # nolint
mean_visitors <- mean(data$Visitors_With_Patient)
median_visitors <- median(data$Visitors_With_Patient)
mode_visitors <- names(sort(table(data$Visitors_With_Patient), decreasing = TRUE))[1] # nolint
cat("  Mean:", mean_visitors, "  ~   Median:", median_visitors, "  ~   Mode:", mode_visitors) # nolint




# Create a bar plot of the distribution of patient ages
ggplot(data, aes(x = Age)) +
    geom_bar(fill = "blue") +
    ggtitle("Distribution of Patient Ages") +
    labs(x = "Age", y = "Frequency") +
    theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5))




# Create a subset of Type_Of_Admission for 'Emergency' and create a variable for the distribution of the severity of illness # nolint
data_subset <- subset(data, Type_Of_Admission == "Emergency")
severity_distribution <- as.data.frame(table(data_subset$Severity_Of_Illness))
names(severity_distribution) <- c("Severity_Of_Illness", "Frequency")
print(severity_distribution)




# Calculate the standard deviation and variance of the "Admission_Deposit" column. # nolint
admission_deposit_sd <- sd(data$Admission_Deposit)
admission_deposit_var <- var(data$Admission_Deposit)
cat("  Standard Deviation:", admission_deposit_sd, "  ~   Variance:", admission_deposit_var) # nolint




# Plot the relationship between the age of the patient and the length of their stay in the hospital. # nolint
# help from https://r-graph-gallery.com/79-levelplot-with-ggplot2.html
ggplot(data, aes(x = Age_Number, y = Stay_Number)) +
    geom_hex(binwidth = c(10, 10), na.rm = TRUE) +
    scale_fill_gradient(low = "white", high = "blue") +
    ggtitle("Relationship of Patient Ages and Length of Stay") +
    labs(x = "Age of Patient", y = "Length of Stay (in days)") +
    theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5
        )
    )




# Calculate the correlation between the deposit amount paid by the patient and the length of their stay in the hospital. # nolint
correlation_deposit_and_stay <- cor(data$Admission_Deposit, data$Stay_Number)
print(correlation_deposit_and_stay)




# Show the distribution of bed grades in each department of the hospital in a variable then plot the data in a bar plot. # nolint
# help from https://dplyr.tidyverse.org/reference/mutate.html
bed_grade_distribution <- data %>%
    group_by(Department, Bed_Grade) %>%
    summarize(n = n()) %>%
    mutate(prop = n / sum(n))
print(bed_grade_distribution)

ggplot(bed_grade_distribution, aes(x = Department, y = prop, fill = Bed_Grade)) + # nolint
    geom_bar(stat = "identity") +
    labs(x = "Department", y = "Proportion of Bed Grades") +
    ggtitle("Distribution of Bed Grades per Department") +
    theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 18))




# Plot the most frequent Bed_Grade per each Hospital_Code in a violin graph
bed_grade_frequency <- data %>%
    group_by(Hospital_Code, Bed_Grade) %>%
    summarize(count = n()) %>%
    group_by(Hospital_Code) %>%
    top_n(1, count)

ggplot(bed_grade_frequency, aes(x = Hospital_Code, y = Bed_Grade)) + # nolint
    geom_violin() +
    ggtitle("Distribution of Most Frequent Bed Grade per Hospital Code") +
    labs(x = "Hospital Code", y = "Bed Grade") +
    theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5))
