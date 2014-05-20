# Returns one data set by reading and merging all component files.
# Data set comprises of the X values, Y values and Subject IDs.
# The path_prefix indicates the path where the data files can be found.
# The fname_suffix indicates the file name suffix to be used to create the complete file name.
#
# This also subsets the data to extract only the measurements on the mean and standard deviation for each measurement.
# The required columns in the subset is determined by selecting only those columns that have either "mean()" or "std()" in their names.
# Subsetting is done early on to help reduce memory requirements.
readData <- function(fname_suffix, path_prefix, download_prefix) {
    fpath <- file.path(download_prefix, path_prefix, paste0("y_", fname_suffix, ".txt"))
    y_data <- read.table(fpath, header=F, col.names=c("ActivityID"))
    
    fpath <- file.path(download_prefix, path_prefix, paste0("subject_", fname_suffix, ".txt"))
    subject_data <- read.table(fpath, header=F, col.names=c("SubjectID"))
    
    # read the column names
    fpath <- file.path(download_prefix, "features.txt")
    data_cols <- read.table(fpath, header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
    
    # read the X data file
    fpath <- file.path(download_prefix, path_prefix, paste0("X_", fname_suffix, ".txt"))
    data <- read.table(fpath, header=F, col.names=data_cols$MeasureName)
    
    # names of subset columns required
    subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
    
    # subset the data (done early to save memory)
    data <- data[,subset_data_cols]
    
    # append the activity id and subject id columns
    data$ActivityID <- y_data$ActivityID
    data$SubjectID <- subject_data$SubjectID
    
    # return the data
    data
}

# read test data set, in a folder named "test", and data file names suffixed with "test"
readTestData <- function() {
    readData("test", "test", "UCI HAR Dataset")
}

# read test data set, in a folder named "train", and data file names suffixed with "train"
readTrainData <- function() {
    readData("train", "train", "UCI HAR Dataset")
}

# Merge both train and test data sets
# Also make the column names nicer
mergeData <- function() {
    data <- rbind(readTestData(), readTrainData())
    cnames <- colnames(data)
    cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
    cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
    colnames(data) <- cnames
    write.table(data, 'merged_data.txt')
    data
}

# Add the activity names as another column
applyActivityLabel <- function(download_prefix, data) {
    fpath <- file.path(download_prefix, "activity_labels.txt")
    activity_labels <- read.table(fpath, header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
    activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
    data_labeled <- merge(data, activity_labels)
    data_labeled
}

# Combine training and test data sets and add the activity label as another column
getMergedLabeledData <- function() {
    download_prefix = "UCI HAR Dataset"
    applyActivityLabel(download_prefix,mergeData())
}

# Create a tidy data set that has the average of each variable for each activity and each subject.
getTidyData <- function(merged_labeled_data) {
    library(reshape2)
    
    # melt the dataset
    id_vars = c("ActivityID", "ActivityName", "SubjectID")
    measure_vars = setdiff(colnames(merged_labeled_data), id_vars)
    melted_data <- melt(merged_labeled_data, id=id_vars, measure.vars=measure_vars)
    write.table(melted_data, 'melted_data.txt')
    
    # recast 
    dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

# Create the tidy data set and save it on to the named file
createTidyDataFile <- function(fname) {
    tidy_data <- getTidyData(getMergedLabeledData())
    write.table(tidy_data, fname)
}

print("Assuming data files from the \"UCI HAR Dataset\" are availale in the current directory with the same structure as in the downloaded archive.")
print("    Refer Data:")
print("    archive: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
print("    description: dataset: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones")
print("Creating merged train and test dataset as merged_data.txt...")
print("Creating melted dataset as melted_data.txt...")
print("Creating tidy dataset as tidy.txt...")
createTidyDataFile("tidy.txt")
print("Done.")
