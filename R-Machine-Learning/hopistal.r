library(data.table)
library(zoo)
library(nnet)
library(dplyr)
library(caret)


# load data
new_data <- fread("R-Machine-Learning/Data/hospital_data.csv") # nolint


# PREPROCESSING OF DATA
# Show snippet of data before changes
View(new_data [1:500, ])

# Changes column names to be uniform
names(new_data) <- c("Case_Id", "Hospital_Code", "Hospital_Type_Code", "City_Code_Hospital", "Hospital_Region_Code", "Available_Extra_Rooms_In_Hospital", "Department", "Ward_Type", "Ward_Facility_Code", "Bed_Grade", "Patient_Id", "City_Code_Patient", "Type_Of_Admission", "Severity_Of_Illness", "Visitors_With_Patient", "Age", "Admission_Deposit", "Stay") # nolint

# Columns to convert to factors
columns_to_convert <- c("Hospital_Code", "City_Code_Hospital", "Bed_Grade", "City_Code_Patient") # nolint
# Convert columns to factors
for (col in columns_to_convert) {
    new_data[[col]] <- as.factor(new_data[[col]])
}

# Remove uneccessary columns
new_data <- subset(new_data, select = -Case_Id, -Patient_Id)

# Changes outlying data points to make them unifrom (replace 20-Nov with 11-20)
new_data$Stay[new_data$Stay == "20-Nov"] <- "11-20"
new_data$Age[new_data$Age == "20-Nov"] <- "11-20"

# Replace any blank value with NA
new_data[new_data == ""] <- NA

# Check for any missing values in data (including Stay variable)
missing_og <- sum(is.na(new_data))
cat("Number of missing values or NA in data before changes: ", missing_og, "\n",  sep = "") # nolint

# Select only the unique rows (remove duplicates)
duplicates <- duplicated(new_data)
new_data <- new_data[!duplicates, ]

# FILL MISSING VALUES
for (col in names(new_data)) {
    if (is.numeric(new_data[[col]])) {
        new_data[[col]][is.na(new_data[[col]])] <- mean(new_data[[col]], na.rm = TRUE) # nolint
    }
}

# Replace all blank values with 'Unknown' for character cols
for (col in names(new_data)) {
    # get the number of missing values in the column
    num_missing <- sum(is.na(new_data[[col]]))
    # Column has missing values
    if (num_missing > 0) {
        # get the column type
        col_type <- class(new_data[[col]])[1]
        # replace missing values with column mean
        if (col_type == "character") {
        # replace missing values with "unknown"
            new_data[[col]][is.na(new_data[[col]])] <- "Unknown"
        }
    }
}

# ENCODING CATEGORICAL DATA
# Encodes then puts as factors
new_data <- new_data %>% mutate_if(is.character, function(x) as.factor(as.integer(as.factor(x)) - 1)) # nolint

# NORMALIZING THE DATA
for (col in names(new_data)) {
    if (is.numeric(new_data[[col]])) {
        new_data[[col]] <- scale(new_data[[col]], center = TRUE, scale = TRUE)
    }
}

# Show snippet of data after preprocessing
View(new_data [1:500, ])

# Check for any missing values in data
missing_new <- sum(is.na(new_data))
cat("Number of missing values or NA in data after changes: ", missing_new, "\n",  sep = "") # nolint



# SPLIT DATA FOR TRAINING AND TESTING
# Set seed for reproducibility
set.seed(123)

# Split new_data
new_index <- createDataPartition(new_data$Stay, p = 0.7, list = FALSE)
train_data <- new_data[new_index, ]
test_data <- new_data[-new_index, ]



# NEURAL NET
# TRAINING
nn_model <- nnet(Stay ~ ., data = train_data, size = 5, maxit = 100, decay = 0.2) # nolint
nn_pred_raw <- predict(nn_model, newdata = train_data, type = "raw")
nn_pred <- apply(nn_pred_raw, 1, function(row) {
    col_index <- which.max(row)
    colnames(nn_pred_raw)[col_index]
})
nn_accuracy <- mean(nn_pred == train_data$Stay)


# TESTING
nn_test_pred_raw <- predict(nn_model, newdata = test_data, type = "raw")
nn_test_pred <- apply(nn_test_pred_raw, 1, function(row) {
    col_index <- which.max(row)
    colnames(nn_test_pred_raw)[col_index]
})
nn_test_accuracy <- mean(nn_test_pred == test_data$Stay)



# Accuracy comparison
cat("Shallow Neural Net Accuracy on Train Data: ", round(nn_accuracy * 100, 4), "%\n",  sep = "") # nolint
cat("Shallow Neural Net Accuracy on Test Data: ", round(nn_test_accuracy * 100, 4), "%\n",  sep = "") # nolint