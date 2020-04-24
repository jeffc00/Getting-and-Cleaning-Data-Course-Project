library(tidyverse)

merge_datasets <- function() {
 X_train <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/data/train/X_train.txt")
 y_train <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/data/train/y_train.txt")
 X_test <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/data/test/X_test.txt")
 y_test <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/data/test/y_test.txt")
 return(rbind(cbind(X_train, y_train), cbind(X_test, y_test)))
}

extract_mean_and_sd <- function(dataframe) {
  mean_cols <- colMeans(dataframe[, 1:561], na.rm = TRUE)
  sd_cols <- apply(dataframe[, 1:561], 2, partial(sd, na.rm = TRUE))
  return(cbind(mean = mean_cols, sd = sd_cols))
}

describe_activities <- function(dataframe) {
  activities <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/activity_labels.txt")
  dataframe[, 562] <- factor(dataframe[, 562], levels = activities[, 1], labels = activities[, 2])
  return(dataframe)
}

name_columns <- function(dataframe) {
  features <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/features.txt", stringsAsFactors = FALSE)
  colnames(dataframe) <- rbind(features, c(562, "activities"))[, 2]
  return(dataframe)
}

create_tidy_dataset <- function(dataframe) {
  activities <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/activity_labels.txt", stringsAsFactors = FALSE)
  subject_train <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/data/train/subject_train.txt")
  subject_test <- read.table("coursera/Getting-and-Cleaning-Data-Course-Project/data/test/subject_test.txt")
  dataframe <- cbind(dataframe, rbind(subject_train, subject_test))
  colnames(dataframe) <- c(colnames(dataframe[, 1:562]), "subjects")
  df_subjects <- split(dataframe, dataframe$subjects)
  df_subjects_activities <- sapply(as.array(1:30), function(s) split(df_subjects[[s]], df_subjects[[s]]$activities))
  tidy <- sapply(as.array(1:180), function(x) colMeans(df_subjects_activities[[x]][1:561]))
  colnames(tidy) <- as.vector(t(outer(paste("subject", 1:30, sep = ""), activities[, 2], paste, sep = "_")))
  return(tidy)
}

data <- merge_datasets()
summaries <- extract_mean_and_sd(data)
data <- describe_activities(data)
data <- name_columns(data)
tidy <- create_tidy_dataset(data)
write.table(tidy, "coursera/Getting-and-Cleaning-Data-Course-Project/tidy.txt", row.names = FALSE)